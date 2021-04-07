-module(dispatcher).
-export([dispatching_to/1, dispatch/2, dispatch_all/2]).

% A dispatcher is a component responsible of dispatching a given job to a batch of workers which are going to
% run it within a given context.
%
% It has the ability to dispatch jobs in an asynchronous and non-blocking fashion, allowing for fire-and-forget
% usage from the sender. Moreover it can be used to perform intermediate operations on the flow of data (e.g. mapping, filtering...).


% It creates a dispatcher which is bound to a specific batch of workers.
dispatching_to(WorkersBatch) -> spawn(fun() -> dispatcher(WorkersBatch) end).

% It sends jobs to the batch of workers, which will be responsible of handling the job.
dispatcher(WorkersBatch) ->
    receive
        {dispatch, Job} -> 
            WorkersBatch ! {send, Job},
            dispatcher(WorkersBatch);
        {dispatch_all, Job} ->
            WorkersBatch ! {send_all, Job},
            dispatcher(WorkersBatch)
    end.

% It dispatches a job to a given dispatcher.
dispatch(Dispatcher, Job) -> Dispatcher ! {dispatch, Job}.

% It dispatches a job to a given dispatcher specifying that this job will need to be send to all
% the available workers in the batch.
dispatch_all(Dispatcher, Job) -> Dispatcher ! {dispatch_all, Job}.