%% @copyright Copyright (c) 2021 Driebit BV
%% @doc Combine a list of triples into a collection of JSON-LD (alike) documents.

%% Copyright 2021 Driebit BV
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(rdf_triples).

-export([
    to_docs/1,
    compact/2,
    ns_expand/2,
    ns_compact/2
]).



%% @doc Compact a doc, add namespaces and simplify values.
-spec compact( Doc, Namespaces ) -> Doc
    when Doc :: zotonic_rdf:rdf_doc(),
         Namespaces :: #{ binary() := binary() }.
compact(Doc, Namespaces) ->
    maps:fold(
        fun
            (<<"@type">>, V, Acc) ->
                Acc#{ <<"@type">> => ns_compact(V, Namespaces)};
            (K, V, Acc) ->
                K1 = ns_compact(K, Namespaces),
                V1 = compact_value(V, Namespaces),
                Acc#{ K1 => V1 }
        end,
        #{},
        Doc).

compact_value([V], Ns) ->
    compact_value(V, Ns);
compact_value(Vs, Ns) when is_list(Vs) ->
    [ compact_value(V, Ns) || V <- Vs ];
compact_value(#{} = V, Ns) ->
    compact_value_type(compact(V, Ns));
compact_value(V, _Ns) ->
    V.

%% @doc Compact some basic types to their value in JSON.
compact_value_type(#{ <<"@value">> := V, <<"@type">> := <<"xsd:integer">> } = N) ->
    case maps:size(N) of
        2 ->
            z_convert:to_integer(V);
        _ ->
            N1 = maps:without([ <<"@type">> ], N),
            N1#{ <<"@value">> => z_convert:to_integer(V) }
    end;
compact_value_type(#{ <<"@value">> := V, <<"@type">> := <<"xsd:string">> } = N) ->
    case maps:size(N) of
        2 ->
            z_convert:to_binary(V);
        _ ->
            N1 = maps:without([ <<"@type">> ], N),
            N1#{ <<"@value">> => z_convert:to_binary(V) }
    end;
compact_value_type(#{ <<"@value">> := V, <<"@type">> := <<"xsd:boolean">> } = N) ->
    case maps:size(N) of
        2 ->
            z_convert:to_bool(V);
        _ ->
            N1 = maps:without([ <<"@type">> ], N),
            N1#{ <<"value">> => z_convert:to_bool(V) }
    end;
compact_value_type(#{ <<"@value">> := V } = N) ->
    % Defaults to xsd:string
    case maps:size(N) of
        1 ->
            z_convert:to_binary(V);
        _ ->
            N#{ <<"@value">> => z_convert:to_binary(V) }
    end;
compact_value_type(N) ->
    N.


%% @doc Expand a namespace in a predicate. For example, replaces:
%% xsd:integer with http://www.w3.org/2001/XMLSchema#integer
-spec ns_expand( Pred, Ns ) -> Pred
    when Pred :: binary(),
         Ns :: #{ binary() := binary() }.
ns_expand(<<"http:", _/binary>> = Pred, _Ns) -> Pred;
ns_expand(<<"https:", _/binary>> = Pred, _Ns) -> Pred;
ns_expand(<<"urn:", _/binary>> = Pred, _Ns) -> Pred;
ns_expand(<<"_:", _/binary>> = Pred, _Ns) -> Pred;
ns_expand(Pred, Ns) ->
    case binary:split(Pred, <<":">>) of
        [ N, Rest ] ->
            case maps:find(N, Ns) of
                {ok, Uri} -> <<Uri/binary, Rest/binary>>;
                error ->
                    logger:error(#{
                        in => zotonic_rdf,
                        what => ns_expand,
                        result => error,
                        reason => unknown_namespace,
                        predicate => Pred
                    }),
                    Pred
            end;
        [_] ->
            Pred
    end.

%% @doc Compact a namespace in a predicate. For example, replaces:
%% http://www.w3.org/2001/XMLSchema#integer with xsd:integer
-spec ns_compact( Pred, Ns ) -> Pred
    when Pred :: binary(),
         Ns :: #{ binary() := binary() }.
ns_compact(Pred, Ns) ->
    Next = maps:iterator(Ns),
    ns_compact_1(Pred, maps:next(Next)).

ns_compact_1(Pred, none) ->
    Pred;
ns_compact_1(Pred, {Ns, Uri, Next}) ->
    case binary:longest_common_prefix([Uri, Pred]) =:= size(Uri) of
        true ->
            binary:replace(Pred, Uri, <<Ns/binary,$:>>);
        false ->
            ns_compact_1(Pred, maps:next(Next))
    end.



%% @doc Combine a list of triples into a collection of JSON-LD (alike) documents.
%% Only non blank-node dcuments are returned. Blank nodes are substituted into the
%% documents.
-spec to_docs( Triples ) -> {ok, Docs}
    when Triples :: [ zotonic_rdf:rdf_triple() ],
         Docs :: #{ zotonic_rdf:uri() := zotonic_rdf:rdf_doc() }.
to_docs(Triples) ->
    Uris = collect(Triples),
    maps:fold(
        fun
            (<<"_:", _/binary>>, _, Acc) ->
                Acc;
            (Uri, Doc, Acc) ->
                [ expand_blank_node(Doc, Uris, [ Uri ]) | Acc ]
        end,
        [],
        Uris).

%% @doc Replace all blank node references in the document with their document.
expand_blank_node(Doc, Uris, Trace) ->
    maps:fold(
        fun
            (<<"@", _/binary>> = K, V, Acc) when is_binary(V) ->
                Acc#{ K => V };
            (Pred, Ns, Acc) ->
                Ns1 = lists:filtermap(
                    fun(N) ->
                        expand_blanks_1(N, Uris, Trace)
                    end,
                    Ns),
                Acc#{
                    Pred => Ns1
                }
        end,
        #{},
        Doc).

expand_blanks_1(#{ <<"@value">> := _ } = N, _Uris, _Trace) ->
    {true, N};
expand_blanks_1(#{ <<"@id">> := <<"_:", _/binary>> = Uri }, Uris, Trace) ->
    case lists:member(Uri, Trace) of
        true ->
            % Recursion
            false;
        false ->
            case maps:find(Uri, Uris) of
                {ok, Doc} ->
                    Doc1 = expand_blank_node(Doc, Uris, [ Uri | Trace ]),
                    Doc2 = maps:remove(<<"@id">>, Doc1),
                    {true, Doc2};
                error ->
                    % Blank node is undefined
                    false
            end
    end;
expand_blanks_1(#{ <<"@id">> := _Uri } = N, _Uris, _Trace) ->
    {true, N};
expand_blanks_1(V, _Uris, _Trace) ->
    {true, V}.

%% @doc Collect all defined documents. Blank and non-blank nodes.
collect(Triples) ->
    lists:foldr(
        fun
            (#{<<"subject">> := Subject, <<"predicate">> := Pred } = T , Acc) ->
                Sub = case maps:find(Subject, Acc) of
                    {ok, S} ->
                        S;
                    error ->
                        #{ <<"@id">> => Subject }
                end,
                Vs = maps:get(Pred, Sub, []),
                Sub1 = Sub#{ Pred => [ v(T) | Vs ] },
                Acc#{ Subject => Sub1 }
        end,
        #{},
        Triples).

v(#{ <<"@id">> := Url }) ->
    #{ <<"@id">> => Url };
v(#{ <<"@value">> := Value} = V) ->
    V1 = maps:with([ <<"@value">>, <<"@type">>, <<"@language">> ], V),
    case maps:size(V1) of
        1 -> Value;
        _ -> V1
    end.
