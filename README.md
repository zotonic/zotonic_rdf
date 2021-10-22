# RDF triples handling - map triples to resources - RDF support routines.

This library contains helper functions to work with RDF triples and JSON-LD documents.

## Triples

Triples are in essence three tuples: `{subject, predicate, object}`.

In practice there are variations, depending on the type of the object.

We have the following two variations:

```erlang
#{
    <<"subject">> => <<"_:n1">>,
    <<"predicate">> => <<"http://....">>,
    <<"@value">> => <<"...">>,
    <<"@type">> => <<"http://....">>,
    <<"@language">> => <<"nl">>
}.

```

And, if the object is a uri:

```erlang
#{
    <<"subject">> => <<"_:n1">>,
    <<"predicate">> => <<"http://....">>,
    <<"@id">> => <<"...">>
}.
```

## Documents

Triples can be combined into documents. A document is a map with predicates and values.

To make documents from a list of triples call:

```erlang
zotonic_rdf:triples_to_docs(Triples).
```

This returns a list of documents, only documents with a non-blank `@id` are returned.
Blank nodes are inlined in the documents.

Example document:

```erlang
#{
    <<"@id">> => <<"http://example.com/#a">>,
    <<"http://xmlns.com/foaf/0.1/name">> => [
        <<"Jan">>
    ],
    <<"http://example.com/#foo">> => [
        <<"Baz">>,
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
}
```

The documents have full uris for their types and predicates, also the predicate values are lists
and some values like integers can still be represented as strings.
The uris and values can be compacted using the `compact/1` or `compact/2` function:


```erlang
zotonic_rdf:compact(Document, zotonic_rdf:namespaces()).
```

or, using the default namespaces in `zotonic_rdf:namespaces()`:

```erlang
zotonic_rdf:compact(Document).
```

The example document above will then become:

```erlang
#{
    <<"@id">> => <<"http://example.com/#a">>,
    <<"foaf:name">> => <<"Jan">>,
    <<"http://example.com/#foo">> => [
        <<"Baz">>,
        <<"123">>,
        123,
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
}
```

## Namespaces

The library defines a default list of namespaces for vocabularies.
These are:

<table>
    <tr><th>acl</th><td>http://www.w3.org/ns/auth/acl#</td></tr>
    <tr><th>dbpedia</th><td>http://dbpedia.org/property/</td></tr>
    <tr><th>dbpedia-owl</th><td>http://dbpedia.org/ontology/</td></tr>
    <tr><th>dc</th><td>http://purl.org/dc/elements/1.1/</td></tr>
    <tr><th>dcterms</th><td>http://purl.org/dc/terms/</td></tr>
    <tr><th>dctype</th><td>http://purl.org/dc/dcmitype/</td></tr>
    <tr><th>foaf</th><td>http://xmlns.com/foaf/0.1/</td></tr>
    <tr><th>geo</th><td>http://www.w3.org/2003/01/geo/wgs84_pos#</td></tr>
    <tr><th>rdf</th><td>http://www.w3.org/1999/02/22-rdf-syntax-ns#</td></tr>
    <tr><th>rdfs</th><td>http://www.w3.org/2000/01/rdf-schema#</td></tr>
    <tr><th>schema</th><td>http://schema.org/</td></tr>
    <tr><th>vcard</th><td>http://www.w3.org/2006/vcard/ns#</td></tr>
    <tr><th>vocab</th><td>http://rdf.data-vocabulary.org/#</td></tr>
    <tr><th>xsd</th><td>http://www.w3.org/2001/XMLSchema#</td></tr>
</table>

Use the following functions to compact or expand a namespace:

```erlang
2> zotonic_rdf:ns_compact(<<"http://www.w3.org/2001/XMLSchema#string">>).
<<"xsd:string">>
3> zotonic_rdf:ns_expand(<<"xsd:string">>).                              
<<"http://www.w3.org/2001/XMLSchema#string">>
```

Optionally a map of namespaces can be passed as a second argument, this
defaults to `zotonic_rdf:namespaces()`.
