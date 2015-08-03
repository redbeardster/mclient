
-module(mclient).

-behaviour(gen_server).

%% API
-export([start_link/0]).

-define (Addr, {224,0,0,251}).
-define (IAddr, {0,0,0,0}).
-define (Port, 5000).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {socket}).


start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


init([]) ->

    {ok, Socket} = gen_udp:open(5000, [binary, {active, false}, {reuseaddr, true},
                                        {ip, ?Addr}, {add_membership, {?Addr, ?IAddr}}]),
    gen_udp:send(Socket,?Addr,5000,erlang:atom_to_list(node())),

    net_kernel:monitor_nodes(true, []),
    
    {ok, #state{socket=Socket}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({nodedown, Nodename}, State)->
    error_logger:info_msg("Node ~p has gone down", Nodename),
    {noreply, State};

handle_info({nodeup, Nodename}, State)->
    error_logger:info_msg("Node ~p has up", Nodename),
    net_adm:ping(Nodename),

    {noreply, State}.



terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

