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
    <<"@lang">> => <<"nl">>
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
