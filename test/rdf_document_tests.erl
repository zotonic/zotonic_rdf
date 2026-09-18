-module(rdf_document_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../include/zotonic_rdf.hrl").

standard_namespace_test() ->
    ?assertEqual(?NS_ZOTONIC, maps:get(?PREFIX_ZOTONIC, zotonic_rdf:namespaces())),
    ?assertEqual(<<"zotonic:id">>, zotonic_rdf:ns_compact(<<?NAMESPACE_ZOTONIC, "id">>)),
    ?assertEqual(<<"schema:name">>, zotonic_rdf:ns_compact(<<"http://schema.org/name">>)).

blank_cycle_and_datatype_test() ->
    P = <<"https://example.test/p">>,
    Ts = lists:sort([
        #{<<"subject">> => <<"_:a">>, <<"predicate">> => P, <<"@id">> => <<"_:b">>},
        #{<<"subject">> => <<"_:b">>, <<"predicate">> => P, <<"@id">> => <<"_:a">>},
        #{<<"subject">> => <<"_:a">>, <<"predicate">> => <<"https://schema.org/count">>,
            <<"@value">> => <<"01">>, <<"@type">> => <<?NS_XSD/binary, "integer">>}
    ]),
    Docs = rdf_document:from_triples(Ts),
    ?assertEqual(2, length(Docs)),
    ?assertEqual(Ts, rdf_document:to_triples(Docs)),
    [A, _] = rdf_document:compact(Docs),
    ?assertEqual(#{<<"@value">> => <<"01">>, <<"@type">> => <<"xsd:integer">>},
        maps:get(<<"schema:count">>, A)).

unicode_iri_test() ->
    Name = unicode:characters_to_binary([16#e9]),
    Base = <<"https://example.test/", Name/binary, "/">>,
    Expected = <<Base/binary, "%C3%A9/",Name/binary>>,
    ?assertEqual(Expected, rdf_iri:resolve(<<"%C3%A9/",Name/binary>>, Base)),
    ?assert(rdf_iri:is_absolute(Expected)),
    ?assertNot(rdf_iri:is_absolute(<<"https://example.test/with space">>)).
