## Module Integral

#### `Integral`

``` purescript
class (BoundedOrd a) <= Integral a where
  toBigInt :: a -> BigInt
  fromBigInt :: BigInt -> a
```

#### `fromIntegral`

``` purescript
fromIntegral :: forall a b. (Integral a, Integral b) => a -> b
```

#### `clamp`

``` purescript
clamp :: forall b. (Integral b) => BigInt -> b
```


