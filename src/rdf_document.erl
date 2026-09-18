%% @copyright 2026 Marc Worrell
%% @doc Lossless document formatting shared by RDF serialization applications.
-module(rdf_document).
-export([compact/1, from_triples/1, to_triples/1]).
-include("../include/zotonic_rdf.hrl").

%% Unlike the legacy compact/1 helper, retain explicit datatypes and blank IDs.
-spec compact(Documents) -> Documents when Documents :: [map()].
compact(Documents) ->
    [(compact_node(D))#{<<"@context">> => zotonic_rdf:namespaces()} || D <- Documents].

compact_node(M) when is_map(M) ->
    maps:from_list([compact_pair(K, V) || {K,V} <- maps:to_list(M), K =/= <<"@context">>]);
compact_node(L) when is_list(L) -> [compact_node(V) || V <- L];
compact_node(V) -> V.
compact_pair(<<"@value">> = K, V) -> {K, V};
compact_pair(<<"@type">> = K, V) -> {K, one([zotonic_rdf:ns_compact(T) || T <- many(V)])};
compact_pair(<<"@list">> = K, V) -> {K, [compact_node(X) || X <- many(V)]};
compact_pair(<<"@graph">> = K, V) -> {K, [compact_node(X) || X <- many(V)]};
compact_pair(<<"@included">> = K, V) -> {K, [compact_node(X) || X <- many(V)]};
compact_pair(<<"@", _/binary>> = K, V) -> {K, compact_node(V)};
compact_pair(K, V) -> {zotonic_rdf:ns_compact(K), one([compact_node(X) || X <- many(V)])}.
one([V]) -> V;
one(V) -> V.
many(V) when is_list(V) -> V;
many(V) -> [V].

%% Keep blank-only graphs, shared blank nodes, and cycles instead of inlining
%% or dropping them. These are the same document/value maps as zotonic_rdf.
-spec from_triples(Triples) -> Documents when Triples :: [map()], Documents :: [map()].
from_triples(Triples) ->
    Nodes = lists:foldl(fun add_triple/2, #{}, Triples),
    [D || {_Id, D} <- lists:sort(maps:to_list(Nodes))].
add_triple(#{<<"subject">> := S, <<"predicate">> := P} = T, Nodes) ->
    case maps:is_key(<<"graph">>, T) of true -> throw({rdf, named_graph_not_supported}); false -> ok end,
    D = maps:get(S, Nodes, #{<<"@id">> => S}),
    {Key, Value} = case {P, maps:find(<<"@id">>, T)} of
        {RdfType, {ok, Iri}} when RdfType =:= <<?NS_RDF/binary, "type">> -> {<<"@type">>, Iri};
        _ -> {P, maps:with([<<"@id">>, <<"@value">>, <<"@type">>, <<"@language">>], T)}
    end,
    Nodes#{S => D#{Key => lists:usort([Value | maps:get(Key, D, [])])}}.

%% Input is expanded JSON-LD. Return flat RDF triples; represent lists using
%% rdf:first/rest, and preserve named graphs with an optional graph key.
-spec to_triples(Documents) -> Triples when Documents :: [map()], Triples :: [map()].
to_triples(Documents) ->
    Used = collect_ids(Documents, #{}),
    {_, Triples} = lists:foldl(fun(D, St) -> element(2, node(D, undefined, St)) end,
        {Used, []}, Documents),
    lists:usort(Triples).
collect_ids(#{<<"@id">> := Id} = M, Acc) -> collect_ids(maps:values(M), Acc#{Id => true});
collect_ids(M, Acc) when is_map(M) -> collect_ids(maps:values(M), Acc);
collect_ids(L, Acc) when is_list(L) -> lists:foldl(fun collect_ids/2, Acc, L);
collect_ids(_, Acc) -> Acc.
blank({Used, Ts}) ->
    Id = zotonic_rdf:blank_id(),
    case maps:is_key(Id, Used) of true -> blank({Used,Ts}); false -> {Id,{Used#{Id=>true},Ts}} end.
node(D, Graph, State0) ->
    {S, State1} = case maps:find(<<"@id">>, D) of {ok, Id} -> {Id, State0}; error -> blank(State0) end,
    State = lists:foldl(fun({K,V}, St) -> node_property(S,K,V,Graph,St) end,
        State1, lists:sort(maps:to_list(D))),
    {S, State}.
node_property(_S, <<"@id">>, _, _, St) -> St;
node_property(_S, <<"@index">>, _, _, St) -> St;
node_property(S, <<"@type">>, Types, G, St) ->
    lists:foldl(fun(T,A) -> emit(S, <<?NS_RDF/binary,"type">>, #{<<"@id">>=>T}, G,A) end,St,many(Types));
node_property(S, <<"@graph">>, Ds, _G, St) ->
    lists:foldl(fun(D,A) -> element(2,node(D,S,A)) end,St,many(Ds));
node_property(_S, <<"@included">>, Ds, G, St) ->
    lists:foldl(fun(D,A) -> element(2,node(D,G,A)) end,St,many(Ds));
node_property(S, <<"@reverse">>, Ps, G, St) ->
    lists:foldl(fun({P,Vs},A) -> lists:foldl(fun(V,B) ->
        {O,C} = node(V,G,B), emit(O,P,#{<<"@id">>=>S},G,C)
    end,A,many(Vs)) end,St,maps:to_list(Ps));
node_property(_S, <<"@",_/binary>> = K, _, _, _) -> throw({rdf,{unsupported_keyword,K}});
node_property(S, P, Vs, G, St) ->
    lists:foldl(fun(V,A) -> {O,B} = object(V,G,A), emit(S,P,O,G,B) end,St,many(Vs)).
object(#{<<"@list">> := L}, G, St) -> collection(L,G,St);
object(#{<<"@value">> := _, <<"@type">> := <<"@json">>}, _G, _St) ->
    throw({rdf, json_literal_not_supported});
object(#{<<"@value">> := V} = M, _G, St) ->
    case maps:is_key(<<"@direction">>, M) of true -> throw({rdf,directional_literal_not_supported}); false -> ok end,
    Type = case V of
        _ when is_integer(V) -> <<?NS_XSD/binary,"integer">>;
        _ when is_float(V) -> <<?NS_XSD/binary,"double">>;
        _ when is_boolean(V) -> <<?NS_XSD/binary,"boolean">>;
        _ -> <<?NS_XSD/binary,"string">>
    end,
    O = maps:with([<<"@value">>,<<"@type">>,<<"@language">>],M),
    O1 = case maps:is_key(<<"@language">>,O) orelse maps:is_key(<<"@type">>,O) of
        true -> O; false -> O#{<<"@type">>=>Type}
    end,
    {O1,St};
object(M, G, St) when is_map(M) -> {Id,S} = node(M,G,St), {#{<<"@id">>=>Id},S}.
collection([], _G, St) -> {#{<<"@id">>=><<?NS_RDF/binary,"nil">>}, St};
collection([H|T], G, St0) ->
    {Id,St1}=blank(St0), {First,St2}=object(H,G,St1), {Rest,St3}=collection(T,G,St2),
    St4=emit(Id,<<?NS_RDF/binary,"first">>,First,G,St3),
    {#{<<"@id">>=>Id},emit(Id,<<?NS_RDF/binary,"rest">>,Rest,G,St4)}.
emit(S,P,O,G,{Used,Ts}) ->
    T=O#{<<"subject">>=>S,<<"predicate">>=>P},
    T1=case G of undefined -> T; _ -> T#{<<"graph">>=>G} end,
    {Used,[T1|Ts]}.
