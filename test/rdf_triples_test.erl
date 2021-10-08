-module(rdf_triples_test).

-include_lib("eunit/include/eunit.hrl").


ns_expand_test() ->
    ?assertEqual(<<"http://www.w3.org/2001/XMLSchema#integer">>,
        rdf_triples:ns_expand(<<"xsd:integer">>, zotonic_rdf:namespaces())).

ns_compact_test() ->
    ?assertEqual(<<"xsd:integer">>,
        rdf_triples:ns_compact(<<"http://www.w3.org/2001/XMLSchema#integer">>, zotonic_rdf:namespaces())).


compact_test() ->
    Doc = #{
        <<"@id">> => <<"http://example.com/#a">>,
        <<"http://xmlns.com/foaf/0.1/name">> => [
            <<"Jan">>
        ],
        <<"http://example.com/#foo">> => [
            <<"Foo">>,
            #{
                <<"@value">> => <<"123">>,
                <<"@type">> => <<"http://www.w3.org/2001/XMLSchema#string">>
            },
            #{
                <<"@value">> => <<"123">>,
                <<"@type">> => <<"http://www.w3.org/2001/XMLSchema#integer">>
            },
            #{
                <<"@value">> => <<"123">>,
                <<"@type">> => <<"http://www.w3.org/2001/XMLSchema#boolean">>
            },
            #{
                <<"@value">> => <<"123">>,
                <<"@type">> => <<"http://example.com/#sometype">>
            },
            #{
                <<"http://example.com/#bar">> => [
                    #{
                        <<"@value">> => <<"Bar">>,
                        <<"@language">> => <<"nl">>,
                        <<"@type">> => <<"http://www.w3.org/2001/XMLSchema#string">>
                    }
                ]
            }
        ]
    },
    Compact = #{
        <<"@id">> => <<"http://example.com/#a">>,
        <<"foaf:name">> => <<"Jan">>,
        <<"http://example.com/#foo">> => [
            <<"Foo">>,
            <<"123">>,
            123,
            true,
            #{
                <<"@value">> => <<"123">>,
                <<"@type">> => <<"http://example.com/#sometype">>
            },
            #{
                <<"http://example.com/#bar">> => #{
                    <<"@value">> => <<"Bar">>,
                    <<"@language">> => <<"nl">>
                }
            }
        ]
    },
    {ok, Doc1} = rdf_triples:compact(Doc, zotonic_rdf:namespaces()),
    ?assertEqual(Compact, Doc1).


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
                            <<"@language">> => <<"nl">>,
                            <<"@type">> => <<"http://example.com/#string">>
                        }
                    ]
                }
            ]
        }
    ],
    ?assertEqual(Doc, rdf_triples:to_docs(triples3())).

to_docs4_test() ->
    Doc = [
        #{
            <<"@id">> => <<"http://example.com/#a">>,
            <<"http://xmlns.com/foaf/0.1/name">> => [
                <<"Jan">>
            ],
            <<"http://example.com/#foo">> => [
                #{
                    <<"@id">> => <<"http://example.com/#b">>
                }
            ]
        },
        #{
            <<"@id">> => <<"http://example.com/#b">>,
            <<"http://xmlns.com/foaf/0.1/name">> => [
                <<"Piet">>
            ]
        }
    ],
    Res = lists:sort(fun idsort/2, rdf_triples:to_docs(triples4())),
    ?assertEqual(Doc, Res).

idsort(#{ <<"@id">> := A }, #{ <<"@id">> := B }) ->
    A < B.


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
            <<"@language">> => <<"nl">>,
            <<"@type">> => <<"http://example.com/#string">>
        }
    ].

triples4() ->
    [
        #{
            <<"subject">> => <<"http://example.com/#a">>,
            <<"predicate">> => <<"http://xmlns.com/foaf/0.1/name">>,
            <<"@value">> => <<"Jan">>
        },
        #{
            <<"subject">> => <<"http://example.com/#a">>,
            <<"predicate">> => <<"http://example.com/#foo">>,
            <<"@id">> => <<"http://example.com/#b">>
        },
        #{
            <<"subject">> => <<"http://example.com/#b">>,
            <<"predicate">> => <<"http://xmlns.com/foaf/0.1/name">>,
            <<"@value">> => <<"Piet">>
        }
    ].
