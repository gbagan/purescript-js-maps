FFI bindings for Javascript Maps

This is basically a fork of [purescript-foreign-object](https://github.com/purescript/purescript-foreign-object)
and [purescript-object-maps](https://github.com/thought2/purescript-object-maps)
using Javascript Maps instead of Javascript Objects.

This library contains two objects `Map k v` from `JS.Map` and `JS.Map.Primitive`.
The latter has better performances that the former but can only be used with Javascript primitive values (Int, String, BigInt) as keys. The former can be used with every key that is an instance of the class `EncodeKey`.

Both objects have efficient lookups but inefficient insertion performance ( O(n) time complexity ).
However, both objects have a mutable version that can be used via the `ST` monad.

### Documentation

Documentation is [published on Pursuit](https://pursuit.purescript.org/packages/purescript-js-maps)

### Install

```
spago install js-maps
```

### Example

```
import JS.Map.Primitive (Map, runST)
import JS.Map.Primitive.ST as STM

sample :: ObjectMap Int String
sample = runST do
  m <- STM.new
  foreach (1..10000) \n -> do
    void $ STM.poke i (show i) m
  pure m
```

### Benchmark

Js.Map.Primitive is roughly 8 times than Data.ObjectMap on read and writing using Int.


```
** lookup (reading) **
Js.Map.Primitive (Int)
mean   = 1.45 ms
stddev = 466.28 μs
min    = 1.11 ms
max    = 10.36 ms
Js.Map (Int)
mean   = 2.70 ms
stddev = 598.79 μs
min    = 2.16 ms
max    = 16.63 ms
Data.ObjectMap (Int)
mean   = 10.74 ms
stddev = 763.09 μs
min    = 10.07 ms
max    = 22.42 ms

** poke (writing) **
Js.Map.Primitive.ST (Int)
mean   = 2.09 ms
stddev = 584.54 μs
min    = 1.88 ms
max    = 9.08 ms
Js.Map.ST (Int)
mean   = 3.05 ms
stddev = 1.07 ms
min    = 2.58 ms
max    = 15.08 ms
Data.ObjectMap (Int)
mean   = 15.79 ms
stddev = 3.93 ms
min    = 10.95 ms
max    = 49.13 ms
```