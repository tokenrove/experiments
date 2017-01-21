%% Sketch for experiment for "When is an Erlang iolist an iovec?"

-module(iolists_experiment).
-compile(export_all).

write_a_lot(_Sink, _Data, 0) -> ok;
write_a_lot(Sink, Data, N) ->
    %% port_command(Sink, Data),
    %%Sink ! {self(), {command, Data}},
    ok = gen_tcp:send(Sink, Data),
    write_a_lot(Sink, Data, N-1).

n_binaries() ->
    {binary, L} = process_info(self(), binary),
    length(L).

n_reductions() ->
    {reductions, N} = process_info(self(), reductions),
    N.

experiment(Pid, N, F) ->
    garbage_collect(self()),
    {N_GCs_Before, N_Words_Before, 0} = statistics(garbage_collection),
    L1 = F(),
    Pid ! {then, [iolist_size(L1), n_binaries()]},
    %% many failed attempts:
    %%
    %% Sink = open_port({spawn_executable, "/bin/sh"},
    %%                  [{args, ["-c","cat >/dev/null"]}]),
    %% Sink = open_port({spawn, "efile"}, []),
    %% {ok,Sink} = file:open("/dev/null", [append]),
    %%
    %% run ncat -klp 9000 beforehand
    {ok,Sink} = gen_tcp:connect("127.0.0.1", 9000, [binary,{packet,0},{nodelay,false},{delay_send,true}]),
    write_a_lot(Sink, L1, N),
    garbage_collect(self()),
    {N_GCs_After, N_Words_After, 0} = statistics(garbage_collection),
    %% port_close(Sink),
    gen_tcp:close(Sink),
    Pid ! {now, [n_reductions(), n_binaries(), N_GCs_After-N_GCs_Before, N_Words_After-N_Words_Before]}.

run(N, {Case, F}) ->
    Self = self(),
    Then = os:timestamp(),
    spawn(fun () -> iolists_experiment:experiment(Self, N, F) end),
    H = receive {then,Msg} -> Msg end,
    T = receive {now,A} -> A end,
    Elapsed = timer:now_diff(os:timestamp(), Then),
    io:format("~p took ~p~n  then: ~p~n  now: ~p~n", [Case, Elapsed, H, T]).

run(N) ->
    [run(N, L) || L <- [{heap_34_should_pack, fun () -> [<<I:(8*35)>> || I <- lists:seq(1,32)] end},
                        {refc_15_shouldnt_pack, fun () -> [<<I:(8*74)>> || I <- lists:seq(1,15)] end},
                        {large_refc_17_shouldnt_pack, fun () -> [<<I:(8*260)>> || I <- lists:seq(1,17)] end},
                        {refc_17_should_pack, fun () -> [<<I:(8*65)>> || I <- lists:seq(1,17)] end},
                        {refc_1, fun () -> iolist_to_binary([<<I:(8*74)>> || I <- lists:seq(1,15)]) end}]],
    ok.
