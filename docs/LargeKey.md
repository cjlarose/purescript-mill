## Module LargeKey

#### `LargeKey`

``` purescript
data LargeKey a b
  = LargeKey a b
```

##### Instances
``` purescript
instance eqLargeKey :: (Eq a, Eq b) => Eq (LargeKey a b)
instance boundedLargeKey :: (Bounded a, Bounded b) => Bounded (LargeKey a b)
instance showLargeKey :: (Show a, Show b) => Show (LargeKey a b)
instance ordLargeKey :: (Ord a, Ord b) => Ord (LargeKey a b)
instance boundedOrdLargeKey :: (BoundedOrd a, BoundedOrd b) => BoundedOrd (LargeKey a b)
instance booleanAlgebraLargeKey :: (Integral a, Integral b, BooleanAlgebra a, BooleanAlgebra b) => BooleanAlgebra (LargeKey a b)
instance integralLargeKey :: (Integral a, Integral b) => Integral (LargeKey a b)
instance bitsLargeKey :: (Bits a, Bits b, Integral a, Integral b, Integral (LargeKey a b)) => Bits (LargeKey a b)
```


