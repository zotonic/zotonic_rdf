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
    to_docs/1
]).


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

expand_blanks_1(V, _Uris, _Trace) when is_binary(V) ->
    {true, V};
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
    end.


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
    V1 = maps:with([ <<"@value">>, <<"@type">>, <<"@lang">> ], V),
    case maps:size(V1) of
        1 -> Value;
        _ -> V1
    end.
