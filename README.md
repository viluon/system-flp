# system-flp

System F.
```
((/\ t. ((\x -> x) :: t -> t))[Nat -> Nat -> Nat])(plus)(2)(2)
```
should evaluate to `ValNat 4`.
