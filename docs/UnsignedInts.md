## Module UnsignedInts

#### `UInt8`

``` purescript
data UInt8
```

##### Instances
``` purescript
instance showUInt8 :: Show UInt8
instance boundedUInt8 :: Bounded UInt8
instance ordUInt8 :: Ord UInt8
instance eqUInt8 :: Eq UInt8
instance boundedOrdUInt8 :: BoundedOrd UInt8
instance arbitraryUInt8 :: Arbitrary UInt8
instance booleanAlgrebraUInt8 :: BooleanAlgebra UInt8
instance integralUInt8 :: Integral UInt8
instance bitsUInt8 :: Bits UInt8
```

#### `byteToInt`

``` purescript
byteToInt :: UInt8 -> Int
```

#### `intToByte`

``` purescript
intToByte :: Int -> UInt8
```

#### `UInt16`

``` purescript
type UInt16 = LargeKey UInt8 UInt8
```

#### `UInt32`

``` purescript
type UInt32 = LargeKey UInt16 UInt16
```

#### `UInt64`

``` purescript
type UInt64 = LargeKey UInt32 UInt32
```

#### `UInt128`

``` purescript
type UInt128 = LargeKey UInt64 UInt64
```


