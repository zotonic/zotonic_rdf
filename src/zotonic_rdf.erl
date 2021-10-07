%% @copyright Copyright (c) 2021 Driebit BV
%% @doc Support routies for handling RDF triples. A typical triple has two
%% variations.
%%
%% One is for a value:
%%
%% <pre>
%% #{
%%     <<"subject">> => <<"_:n1">>,
%%     <<"predicate">> => <<"http://....">>,
%%     <<"@value">> => <<"...">>,
%%     <<"@type">> => <<"http://....">>,
%%     <<"@lang">> => <<"nl">>
%% }.
%% </pre>
%%
%% And, if the object is a uri:
%%
%% <pre>
%% #{
%%     <<"subject">> => <<"_:n1">>,
%%     <<"predicate">> => <<"http://....">>,
%%     <<"@id">> => <<"...">>
%% }.
%% </pre>


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

-module(zotonic_rdf).

-export([
    blank_id/0,
    triples_to_docs/1
    ]).

-type uri() :: binary().
-type rdf_triple() :: map().
-type rdf_doc() :: map().

-export_type([rdf_triple/0, rdf_doc/0, uri/0]).

%% @doc Generate an unique blank node identifier.
-spec blank_id() -> binary().
blank_id() ->
    Nr = integer_to_binary(erlang:unique_integer()),
    <<"_:n", Nr/binary>>.

%% @doc Combine triples to one or more documents. Every document
%% with a non-blank id is returned separately. The documents are mapped
%% using <code>uri => rdf_doc</code>
-spec triples_to_docs( Triples ) -> {ok, Docs}
    when Triples :: [ rdf_triple() ],
         Docs :: #{ uri() := rdf_doc() }.
triples_to_docs(Triples) ->
    rdf_triples:to_docs(Triples). 

