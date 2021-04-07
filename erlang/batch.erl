-module(batch).
-export([of_workers/3, on_workers_batch_free/1]).

% A batch is a collection of elements on which we can perform some operations, in the case of this module
% a batch of workers is implemented.
%
% A worker is a special entity which is responsible of handling a job, which is a generic piece of data that can represent
% either a value to which we apply a function or a function on its own, which we simply call.


% Creates a batch of workers with a given size.
%
% The workers in this batch are going to be spawned via a spawning function which must be implemented by the caller.
%
% This batch of workers is observed by an external observer which will be notified about some state changes in the batch.
of_workers(Observer, SpawnWorker, Size) -> 
    WorkersBatch = spawn(fun() -> workers_batch(Observer, []) end),
    lists:foreach(fun(_) -> add_worker(WorkersBatch, SpawnWorker()) end, lists:seq(1, Size)),
    WorkersBatch.

% A batch of workers that is responsible of managing workers via the basic primitives (e.g. add, send, remove).
%
% Jobs are sent to workers in a round robin fashion, meaning that the work is spreaded in an optimized way,
% supposing that each job takes approximately the same time to be handled.
%
% Each worker added to this batch is supervised and will be deleted from the list of workers when it finishes,
% for this reason is important that the spawned workers have been properly written to support this behavior.
workers_batch(Observer, Workers) ->
    process_flag(trap_exit, true),
    receive
        {add, Worker} ->
            link(Worker),
            NewWorkers = Workers ++ [Worker],
            workers_batch(Observer, NewWorkers);
        {send, _} when Workers == [] -> throw("Cannot send job to batch containing 0 workers.~n");
        {send, Job} when Workers /= [] ->
            [Worker|OtherWorkers] = Workers,
            Worker ! { handle_job, Job },
            workers_batch(Observer, OtherWorkers ++ [Worker]);
        {send_all, Job} ->
            lists:foreach(fun(Worker) -> Worker ! { handle_job, Job } end, Workers),
            workers_batch(Observer, Workers);
        {'EXIT', From, _} ->
            Worker = From,
            NewWorkers = lists:delete(Worker, Workers),
            if
                NewWorkers == [] -> Observer ! workers_batch_free;
                true -> ok
            end,
            workers_batch(Observer, NewWorkers) 
    end.

% Adds a worker to a batch of workers.
add_worker(WorkersBatch, Worker) -> WorkersBatch ! { add, Worker }.

% Calls the given block whenever the workers batch is free, meaning that no worker is available.
on_workers_batch_free(Block) -> receive workers_batch_free -> Block() end.