## Module ArithmeticContexts

#### `ModularArithmetic`

``` purescript
newtype ModularArithmetic a
  = ModularArithmetic a
```

##### Instances
``` purescript
instance eqModularArithmetic :: (Eq a) => Eq (ModularArithmetic a)
instance showModularArithmetic :: (Show a) => Show (ModularArithmetic a)
instance functorModularArithmetic :: Functor ModularArithmetic
instance applyModularArithmetic :: Apply ModularArithmetic
instance applicativeModularArithmetic :: Applicative ModularArithmetic
instance arbitraryModularArithmetic :: (Arbitrary a) => Arbitrary (ModularArithmetic a)
instance semiringModularArithmetic :: (Integral a) => Semiring (ModularArithmetic a)
```

#### `runMod`

``` purescript
runMod :: forall a. ModularArithmetic a -> a
```

#### `SaturatingArithmetic`

``` purescript
newtype SaturatingArithmetic a
  = SaturatingArithmetic a
```

##### Instances
``` purescript
instance eqSaturatingArithmetic :: (Eq a) => Eq (SaturatingArithmetic a)
instance showSaturatingArithmetic :: (Show a) => Show (SaturatingArithmetic a)
instance functorSaturatingArithmetic :: Functor SaturatingArithmetic
instance applySaturatingArithmetic :: Apply SaturatingArithmetic
instance applicativeSaturatingArithmetic :: Applicative SaturatingArithmetic
instance arbitrarySaturatingArithmetic :: (Arbitrary a) => Arbitrary (SaturatingArithmetic a)
instance semiringSaturatingArithmetic :: (Integral a) => Semiring (SaturatingArithmetic a)
```

#### `runSat`

``` purescript
runSat :: forall a. SaturatingArithmetic a -> a
```


