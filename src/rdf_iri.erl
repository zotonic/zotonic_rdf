%% @copyright 2026 Marc Worrell
%% @doc RFC 3986 resolution for Unicode RDF IRIs. uri_string handles URI syntax;
%% temporarily mask non-ASCII characters without decoding existing percent escapes.
-module(rdf_iri).
-export([resolve/2, is_absolute/1]).

-spec is_absolute(binary()) -> boolean().
is_absolute(Iri) when is_binary(Iri) ->
    case re:run(Iri, <<"^[A-Za-z][A-Za-z0-9+.-]*:">>, [{capture, none}]) of
        match ->
            try
                {Masked, _} = mask(Iri, marker(<<"zotoniciri">>, Iri), #{}),
                case uri_string:parse(Masked) of #{scheme := _} -> true; _ -> false end
            catch throw:{rdf, _} -> false end;
        nomatch -> false
    end;
is_absolute(_) -> false.

-spec resolve(binary(), binary() | undefined | null) -> binary().
resolve(Iri, undefined) -> Iri;
resolve(Iri, null) -> Iri;
resolve(Iri, Base) ->
    Prefix = marker(<<"zotoniciri">>, <<Iri/binary, Base/binary>>),
    {MaskedIri, Replacements} = mask(Iri, Prefix, #{}),
    {MaskedBase, All} = mask(Base, Prefix, Replacements),
    case uri_string:resolve(MaskedIri, MaskedBase) of
        Result when is_binary(Result) ->
            maps:fold(fun(K, V, Acc) -> binary:replace(Acc, K, V, [global]) end, Result, All);
        Error -> throw({rdf, {invalid_iri, Error}})
    end.
marker(Prefix, Input) ->
    case binary:match(Input, Prefix) of
        nomatch -> Prefix;
        _ -> marker(<<Prefix/binary, "x">>, Input)
    end.
mask(Iri, Prefix, Replacements) ->
    case unicode:characters_to_list(Iri) of
        Chars when is_list(Chars) ->
            {Parts, All} = lists:mapfoldl(fun
                (C, Acc) when C < 128 -> {C, Acc};
                (C, Acc) ->
                    Key = <<Prefix/binary, (integer_to_binary(C))/binary, "q">>,
                    {Key, Acc#{Key => unicode:characters_to_binary([C])}}
            end, Replacements, Chars),
            {iolist_to_binary(Parts), All};
        _ -> throw({rdf, invalid_utf8})
    end.
