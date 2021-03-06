-module(w_repo).

% A simple repository that stores data in the process dictionary.

-export([set_wx_server/1, get_wx_server/0, 
         create_identity/0, next_id/0, 
         get_control/1, set_control/2, remove_control/1, get_controls_by_owner_pid/1]).

-define(WXSERVER, wx_server).
-define(IDENTITY, identity).

-include_lib("w.hrl").

set_wx_server(Server) -> 
    put(?WXSERVER, Server),
    ok.
get_wx_server() -> get(?WXSERVER).

create_identity() -> 
    put(?IDENTITY, 1),
    ok.
next_id() -> 
    Id = get(?IDENTITY),
    put(?IDENTITY, Id+1).

-define(ENV, environment).
get_env() -> % Must always return an ENV... If it doesn't exist, return a new env.
    case get(?ENV) of
        undefined -> new_env();
        Env -> Env
    end.
set_env(Env) -> put(?ENV, Env).
new_env() -> dict:new().

get_control(ControlId) ->
    Env = get_env(),
    % Should return something like: {ok, {ControlId, Pid, Control}}
    dict:find(ControlId, Env).

set_control(ControlId, Control) when is_record(Control, control) ->
    Env = get_env(),
    NewEnv = dict:store(ControlId, Control, Env),
    set_env(NewEnv),
    ok.

remove_control(ControlId) ->
    Env = get_env(),
    NewEnv = dict:erase(ControlId, Env),
    set_env(NewEnv),
    ok.

get_controls_by_owner_pid(OwnerPid) ->
    Env = get_env(),
    ControlDict = dict:filter(fun(_ControlId, Control) -> 
        OwnerPid == Control#control.owner_pid
    end, Env),
    Controls = dict:fold(fun(_K,V,L) -> [V|L] end, [], ControlDict),
    Controls.