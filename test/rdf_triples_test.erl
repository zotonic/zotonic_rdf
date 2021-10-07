-module(rdf_triples_test).

-include_lib("eunit/include/eunit.hrl").


to_docs1_test() ->
    Doc = [
        #{
            <<"@id">> => <<"http://example.com/#a">>,
            <<"http://xmlns.com/foaf/0.1/name">> => [
                <<"Jan">>
            ]
        }
    ],
    ?assertEqual(Doc, rdf_triples:to_docs(triples1())).

to_docs2_test() ->
    Doc = [
        #{
            <<"@id">> => <<"http://example.com/#a">>,
            <<"http://xmlns.com/foaf/0.1/name">> => [
                <<"Jan">>
            ],
            <<"http://example.com/#foo">> => [
                <<"Foo">>,
                #{
                    <<"http://example.com/#bar">> => [
                        <<"Bar">>
                    ]
                }
            ]
        }
    ],
    ?assertEqual(Doc, rdf_triples:to_docs(triples2())).

to_docs3_test() ->
    Doc = [
        #{
            <<"@id">> => <<"http://example.com/#a">>,
            <<"http://xmlns.com/foaf/0.1/name">> => [
                <<"Jan">>
            ],
            <<"http://example.com/#foo">> => [
                <<"Foo">>,
                #{
                    <<"http://example.com/#bar">> => [
                        #{
                            <<"@value">> => <<"Bar">>,
                            <<"@lang">> => <<"nl">>,
                            <<"@type">> => <<"http://example.com/#string">>
                        }
                    ]
                }
            ]
        }
    ],
    ?assertEqual(Doc, rdf_triples:to_docs(triples3())).



triples1() ->
    [
        #{
            <<"subject">> => <<"http://example.com/#a">>,
            <<"predicate">> => <<"http://xmlns.com/foaf/0.1/name">>,
            <<"@value">> => <<"Jan">>
        }
    ].

triples2() ->
    [
        #{
            <<"subject">> => <<"http://example.com/#a">>,
            <<"predicate">> => <<"http://xmlns.com/foaf/0.1/name">>,
            <<"@value">> => <<"Jan">>
        },
        #{
            <<"subject">> => <<"http://example.com/#a">>,
            <<"predicate">> => <<"http://example.com/#foo">>,
            <<"@value">> => <<"Foo">>
        },
        #{
            <<"subject">> => <<"http://example.com/#a">>,
            <<"predicate">> => <<"http://example.com/#foo">>,
            <<"@id">> => <<"_:n1">>
        },
        #{
            <<"subject">> => <<"_:n1">>,
            <<"predicate">> => <<"http://example.com/#bar">>,
            <<"@value">> => <<"Bar">>
        }
    ].

triples3() ->
    [
        #{
            <<"subject">> => <<"http://example.com/#a">>,
            <<"predicate">> => <<"http://xmlns.com/foaf/0.1/name">>,
            <<"@value">> => <<"Jan">>
        },
        #{
            <<"subject">> => <<"http://example.com/#a">>,
            <<"predicate">> => <<"http://example.com/#foo">>,
            <<"@value">> => <<"Foo">>
        },
        #{
            <<"subject">> => <<"http://example.com/#a">>,
            <<"predicate">> => <<"http://example.com/#foo">>,
            <<"@id">> => <<"_:n1">>
        },
        #{
            <<"subject">> => <<"_:n1">>,
            <<"predicate">> => <<"http://example.com/#bar">>,
            <<"@value">> => <<"Bar">>,
            <<"@lang">> => <<"nl">>,
            <<"@type">> => <<"http://example.com/#string">>
        }
    ].
