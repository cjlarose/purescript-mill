## Module Integral

#### `Integral`

``` purescript
class (BoundedOrd a) <= Integral a where
  toBigInt :: a -> BigInt
  fromBigInt :: BigInt -> a
```

`Integral` represents integers.

In addition to satifying the laws of `BoundedOrd`, instances of `Integral` should satisfy the following law:

`fromBigInt (toBigInt x) == x`

#### `fromIntegral`

``` purescript
fromIntegral :: forall a b. (Integral a, Integral b) => a -> b
```

#### `clamp`

``` purescript
clamp :: forall b. (Integral b) => BigInt -> b
```


