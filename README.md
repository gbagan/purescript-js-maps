# purescript-js-maps

FFI bindings for Javascript Maps

This is basically a fork of [purescript-foreign-object](https://github.com/purescript/purescript-foreign-object)
and [purescript-object-maps](https://github.com/thought2/purescript-object-maps)
using Javascript Maps instead of Javascript Objects.

This library contains two objects `Map k v` from `JS.Map` and `JS.Map.Primitive`.
The latter has better performances that the former but can only be used with Javascript primitive values (`Int`, `String`, `BigInt`) as keys. The former can be used with every key that is an instance of the class `EncodeKey`.

Both objects have efficient lookups but inefficient insertion performance ( O(n) time complexity ).
However, both objects have a mutable version that can be used via the `ST` monad. In this case, insertions (via `poke`) are also efficient.

### Documentation

Documentation is [published on Pursuit](https://pursuit.purescript.org/packages/purescript-js-maps)

### Install

```
spago install js-maps
```

### Example

```haskell
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

Js.Map.Primitive is roughly 8 times faster than Data.ObjectMap on read and writing using Int.


```
**  (reading) **
Js.Map.Primitive (Int)
mean   = 1.38 ms
stddev = 402.12 μs
min    = 1.07 ms
max    = 7.94 ms
Js.Map (Int)
mean   = 2.76 ms
stddev = 477.26 μs
min    = 2.22 ms
max    = 11.05 ms
Data.ObjectMap (Int)
mean   = 10.57 ms
stddev = 523.44 μs
min    = 9.98 ms
max    = 22.08 ms
Data.Map (Int)
mean   = 8.84 ms
stddev = 1.22 ms
min    = 7.91 ms
max    = 22.84 ms
Data.HashMap (Int)
mean   = 3.16 ms
stddev = 379.74 μs
min    = 2.60 ms
max    = 10.01 ms

** Poke (writing) **
Js.Map.Primitive.ST (Int)
mean   = 2.13 ms
stddev = 614.32 μs
min    = 1.92 ms
max    = 9.22 ms
Js.Map.ST (Int)
mean   = 3.08 ms
stddev = 700.02 μs
min    = 2.70 ms
max    = 9.49 ms
Data.ObjectMap (Int)
mean   = 17.11 ms
stddev = 4.41 ms
min    = 11.44 ms
max    = 45.29 ms
Data.Map (Int)
mean   = 27.92 ms
stddev = 3.78 ms
min    = 25.16 ms
max    = 67.87 ms
Data.HashMap (Int)
mean   = 8.04 ms
stddev = 1.32 ms
min    = 7.36 ms
max    = 42.20 ms
```
