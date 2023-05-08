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

| Data structure   | Lookup   | Insertion |
| ---------------- | -------- | --------- |
| JS.Map.Primitive | 1.38 ms  | 2.11 ms   |
| JS.Map           | 2.76 ms  | 3.08 ms   |
| Data.ObjectMap   | 10.57 ms | 17.11 ms  |
| Data.Map         | 8.84 ms  | 27.92 ms  |
| Data.HashMap     | 3.16 ms  | 8.04 ms   |
