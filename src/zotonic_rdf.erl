%% @copyright Copyright (c) 2021 Driebit BV
%% @doc Support routies for handling RDF triples. A typical triple has two
%% variations.
%%
%% One is for a value:
%%
%% ```
%% #{
%%     <<"subject">> => <<"_:n1">>,
%%     <<"predicate">> => <<"http://....">>,
%%     <<"@value">> => <<"...">>,
%%     <<"@type">> => <<"http://....">>,
%%     <<"@language">> => <<"nl">>
%% }.
%% '''
%%
%% And, if the object is a uri:
%%
%% ```
%% #{
%%     <<"subject">> => <<"_:n1">>,
%%     <<"predicate">> => <<"http://....">>,
%%     <<"@id">> => <<"...">>
%% }.
%% '''

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
    triples_to_docs/1,
    compact/1,
    compact/2,
    namespaces/0,
    ns_expand/1,
    ns_expand/2,
    ns_compact/1,
    ns_compact/2
    ]).

-include("../include/zotonic_rdf.hrl").

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
-spec triples_to_docs( Triples ) -> Docs
    when Triples :: [ zotonic_rdf:rdf_triple() ],
         Docs :: [ zotonic_rdf:rdf_doc() ].
triples_to_docs(Triples) ->
    rdf_triples:to_docs(Triples).


%% @doc Compact a doc using the known namespaces and simplify values.
-spec compact(Doc) -> Doc
    when Doc :: zotonic_rdf:rdf_doc().
compact(Doc) ->
    rdf_triples:compact(Doc, namespaces()).

%% @doc Compact a doc, add namespaces and simplify values.
-spec compact( Doc, Namespaces ) -> Doc
    when Doc :: zotonic_rdf:rdf_doc(),
         Namespaces :: #{ binary() := binary() }.
compact(Doc, Namespaces) ->
    rdf_triples:compact(Doc, Namespaces).


%% @doc Default JSON-LD RDF namespace context.
-spec namespaces() -> #{ binary() := binary() }.
namespaces() ->
    #{
        <<"rdf">> => ?NS_RDF,
        <<"rdfs">> => ?NS_RDF_SCHEMA,
        <<"foaf">> => ?NS_FOAF,
        <<"geo">> => ?NS_GEO,
        <<"dc">> => ?NS_DC,
        <<"dcterms">> => ?NS_DCTERMS,
        <<"dctype">> => ?NS_DCTYPE,
        <<"vcard">> => ?NS_VCARD,
        <<"dbpedia-owl">> => ?NS_DBPEDIA_OWL,
        <<"dbpedia">> => ?NS_DBPEDIA,
        <<"schema">> => ?NS_SCHEMA_ORG,
        <<"acl">> => ?NS_ACL,
        <<"xsd">> => ?NS_XSD,
        <<"vocab">> => ?NS_VOCAB
    }.


%% @doc Expand a namespace in a predicate using the default namespaces.
%% For example, replaces: xsd:integer with http://www.w3.org/2001/XMLSchema#integer
-spec ns_expand( Pred ) -> Pred
    when Pred :: binary().
ns_expand(Pred) ->
    rdf_triples:ns_expand(Pred, namespaces()).

%% @doc Expand a namespace in a predicate. For example, replaces:
%% xsd:integer with http://www.w3.org/2001/XMLSchema#integer
-spec ns_expand( Pred, Ns ) -> Pred
    when Pred :: binary(),
         Ns :: #{ binary() := binary() }.
ns_expand(Pred, Ns) ->
    rdf_triples:ns_expand(Pred, Ns).

%% @doc Compact a namespace in a predicate using the default namespaces.
%% For example, replaces: http://www.w3.org/2001/XMLSchema#integer with xsd:integer
-spec ns_compact( Pred ) -> Pred
    when Pred :: binary().
ns_compact(Pred) ->
    rdf_triples:ns_compact(Pred, namespaces()).

%% @doc Compact a namespace in a predicate. For example, replaces:
%% http://www.w3.org/2001/XMLSchema#integer with xsd:integer
-spec ns_compact( Pred, Ns ) -> Pred
    when Pred :: binary(),
         Ns :: #{ binary() := binary() }.
ns_compact(Pred, Ns) ->
    rdf_triples:ns_compact(Pred, Ns).


