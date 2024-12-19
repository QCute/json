# JSON
* [JSON](http://json.org/index.html) encode/decode library for erlang

# quick start
* Add to rebar.config
```erlang
{deps, [
  ...
  {json, {git, "https://github.com/QCute/json.git", {branch, "master"}}}
]}.
```

* Usage 
```erlang
%% decode
json:decode(<<"{\"key\": \"value\"}">>)

%% encode 
json:encode(#{<<"key">> => <<"value">>})
```

Data Mapping (Erlang <=> JSON)
-------------------------------

```
Erlang                  JSON             Erlang
=================================================================================================
undefined              -> null                       -> undefined
true                   -> true                       -> true
false                  -> false                      -> false
<<"abc">>              -> "abc"                      -> <<"abc">>
abc                    -> "abc"                      -> <<"abc">>
123                    -> 123                        -> 123
123.4                  -> 123.4                      -> 123.4
[]                     -> []                         -> []
[1,2,3]                -> [1,2,3]                    -> [1,2,3]
#{}                    -> {}                         -> #{}
#{key => <<"value">>}  -> {"key":"value"}            -> #{<<"key">> => <<"value">>}
#{<<"key">> => value}  -> {"key":"value"}            -> #{<<"key">> => <<"value">>}
```
