-module(erlanger).
-export([word_count/2]).

-import(dispatcher, [dispatching_to/1, dispatch/2, dispatch_all/2]).
-import(batch, [of_workers/3, on_workers_batch_free/1]).


% How to run:
% 1. Start the erlang interactive interpreter via the "erl" command.
% 2. Compile the 3 modules (erlanger, batch, dispatcher) via the command "c(moduleName).".
% 3. Call the function erlanger:word_count("fileName", numberOfScanners).
% 4. Wait for the output.


% Entry point of the program which will spawn a reader asynchronously.
word_count(FileName, N) ->
    Reader = spawn(fun reader/0),
    Reader ! {initialize, FileName, N},
    io:format("Starting to collect statistics from file ~p with ~p scanners.~n", [FileName, N]).


% The reader is responsible of orchestrating on an higher level the components such as the summarizer, scanners.
reader() ->
    receive
        {initialize, FileName, N} ->
            Summarizer = init_summarizer(),
            WorkersBatch = batch:of_workers(self(), scanner_worker(Summarizer), N),
            Dispatcher = dispatcher:dispatching_to(WorkersBatch),
            each_line(fun(Line) -> dispatcher:dispatch(Dispatcher, Line) end, FileName),
            dispatcher:dispatch_all(Dispatcher, 'FINISH'),
            batch:on_workers_batch_free(fun() -> Summarizer ! show_summary end),
            close_file(FileName)
    end.


% Creates a spawning function which will spawn a scanner linked to a given summarizer, to which it will
% send some messages.
scanner_worker(Summarizer) -> 
    fun() -> spawn(fun() -> scanner(Summarizer) end) end.

% The scanner is responsible of handling incoming lines (jobs) that are going to be parsed.
%
% The termination condition for a scanner is to receive a job containing the value 'FINISH'.
scanner(Summarizer) ->
    receive
        {handle_job, Line} ->
            if
            Line /= 'FINISH' ->
                Summarizer ! {append_to_summary, parse_line(Line)},
                scanner(Summarizer);
            true -> ok
            end
    end.

% Parses a given line by splitting it in tokens.
%
% An example of line could be "Student 22 Male Married $4571".
parse_line(Line) -> 
    lists:map(fun(Token) -> parse_token(Token) end, string:tokens(Line, " ")).

% Parses a single token from a specific line.
%
% Given a line "Student 22 Male Married $4571" the tokens are all the components separated by a whitespace.
parse_token(Token) ->
    Matches = [
        {is_gender(Token), {'Gender', Token}}, 
        {is_marital_status(Token), {'Marital Status', Token}},
        {is_age(Token), {'Age', compute_age_value(Token)}},
        {is_yearly_income(Token), {'Yearly Income', compute_yearly_income_value(Token)}},
        {true, {'Occupation', Token}}
    ],
    [{_, ReturnValue}|_] = lists:filter(fun({HasMatched, _}) -> HasMatched end, Matches),
    ReturnValue.

is_gender(Token) -> 
    matches(Token, "^(Male|Female|Other)$").
    
is_marital_status(Token) -> 
    matches(Token, "^(Married|Single|Divorced|Widowed)$").

is_age(Token) ->
    matches(Token, "^[0-9]+$").

compute_age_value(Age) -> 
    {AgeAsInt, _} = string:to_integer(Age),
    if 
        (AgeAsInt >= 0) and (AgeAsInt =< 6) -> "0-6";
        (AgeAsInt >= 7) and (AgeAsInt =< 12) -> "7-12";
        (AgeAsInt >= 13) and (AgeAsInt =< 18) -> "13-18";
        (AgeAsInt >= 19) and (AgeAsInt =< 24) -> "19-24";
        (AgeAsInt >= 25) and (AgeAsInt =< 30) -> "25-30";
        (AgeAsInt >= 31) and (AgeAsInt =< 45) -> "31-45";
        (AgeAsInt >= 46) and (AgeAsInt =< 60) -> "46-60";
        AgeAsInt >= 61 -> "61+";
        true -> "invalid age"
    end.

is_yearly_income(Token) -> 
    matches(Token, "^\\$[0-9]+,*[0-9]*$").

compute_yearly_income_value(YearlyIncome) -> 
    {YearlyIncomeAsInt, _} = string:to_integer(re:replace(YearlyIncome, "\\$", "", [{return, list}])),
    if
        YearlyIncomeAsInt < 10000 -> "<$10,000";
        (YearlyIncomeAsInt >= 10000) and (YearlyIncomeAsInt =< 25000) -> "$10,000-$25,000";
        (YearlyIncomeAsInt > 25000) and (YearlyIncomeAsInt =< 50000) -> "$25,001-$50,000";  
        (YearlyIncomeAsInt > 50000) and (YearlyIncomeAsInt =< 100000) -> "$50,001-$100,000"; 
        (YearlyIncomeAsInt > 100000) and (YearlyIncomeAsInt =< 250000) -> "$100,001-$250,000";
        YearlyIncomeAsInt > 250000 -> ">$250,000";                 
        true -> "invalid yealy income"
    end.


% Spawns a summarizer which is initialized with an empty summary map.
init_summarizer() -> 
    spawn(fun() -> summarizer(maps:new()) end).

% The summarizer is responsible of handling parsed data from each scanner.
%
% The termination condition of a summarizer is when the show_summary message is received.
summarizer(SummaryMap) -> 
    receive
        {append_to_summary, ParsedLine} -> 
            summarizer(build_summary(SummaryMap, ParsedLine));
        show_summary -> 
            print_summary(SummaryMap)
    end.

build_summary(SummaryMap, ParsedLine) -> 
    lists:foldl(fun({Category, Value}, OldSummaryMap) -> 
        update_summary(OldSummaryMap, Category, Value) % We update the new summary for each token of the parsed line.
    end, SummaryMap, ParsedLine).

update_summary(SummaryMap, Category, Value) -> 
    update_value_counter(create_category_if_absent(SummaryMap, Category), Category, Value).

create_category_if_absent(SummaryMap, Category) -> 
    create_if_absent(fun() -> maps:new() end, Category, SummaryMap).

create_value_if_absent(CategoryMap, Value) -> 
    create_if_absent(fun() -> 0 end, Value, CategoryMap).

update_value_counter(SummaryMap, Category, Value) -> 
    CategoryMap = maps:get(Category, SummaryMap),
    maps:put(Category, increment_value_counter(create_value_if_absent(CategoryMap, Value), Value), SummaryMap). 

increment_value_counter(CategoryMap, Value) -> 
    maps:put(Value, maps:get(Value, CategoryMap) + 1, CategoryMap).


% Utilities functions
each_line(Block, FileName) -> 
    lists:foreach(Block, read_lines(FileName)).

read_lines(FileName) ->
    {ok, Data} = file:read_file(FileName),
    lists:map(fun(Line) -> binary:bin_to_list(Line) end, binary:split(Data, [<<"\n">>], [global])).

close_file(FileName) -> 
    file:close(FileName).

matches(String, Regex) ->
    case re:run(String, Regex) of
        {match, _} -> true;
        _ -> false
    end.

create_if_absent(Initialize, Key, Map) ->
    Exists = maps:is_key(Key, Map),
    if
        Exists -> Map;
        true -> maps:put(Key, Initialize(), Map)
    end.

print_summary(SummaryMap) ->
    lists:foreach(fun({Category, CategoryMap}) -> 
        io:format("~p:~n", [Category]),
        print_category(CategoryMap),
        io:format("~n")
    end, 
    lists:keysort(1, maps:to_list(SummaryMap))).

print_category(CategoryMap) ->
    lists:foreach(fun({Value, Counter}) -> 
        io:format("~p: ~p~n", [Value, Counter])
    end, 
    lists:keysort(2, maps:to_list(CategoryMap))).  