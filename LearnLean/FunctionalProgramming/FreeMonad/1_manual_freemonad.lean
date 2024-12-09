set_option autoImplicit true

inductive F (α : Type) where
  | one : α → F α
  | two : α → α → F α
deriving Repr, BEq

instance : Functor F where
  map f a := match a with
    | F.one x => F.one (f x)
    | F.two x y => F.two (f x) (f y)

inductive FreeF (α : Type) where
  | pure : α → FreeF α
  | one : FreeF α → FreeF α
  | two : FreeF α → FreeF α → FreeF α
deriving Repr, BEq

def FreeF.map (f : α → β) (a : FreeF α) : FreeF β :=
  match a with
  | FreeF.pure x => FreeF.pure (f x)
  | FreeF.one x => FreeF.one (FreeF.map f x)
  | FreeF.two x y => FreeF.two (FreeF.map f x) (FreeF.map f y)

instance : Functor FreeF where
  map := FreeF.map

def η : α → FreeF α := fun a => FreeF.pure a

def μ : FreeF (FreeF α) → FreeF α
  | FreeF.pure a => a
  | FreeF.one a => μ a
  | FreeF.two a b => FreeF.two (μ a) (μ b)

def freebind : (FreeF α) → (α → FreeF β) → (FreeF β) :=
  fun ma f => (μ ∘ (FreeF.map f)) ma

instance : Monad FreeF where
  pure := η
  bind := freebind


def y := FreeF.two (FreeF.one (FreeF.pure 1)) (FreeF.two (FreeF.pure 2) (FreeF.one (FreeF.pure 10)))
#eval y
#eval (fun x : Nat => 2 * x) <$> y
#eval toString <$> y

def t := FreeF.one (FreeF.pure 10)
#eval η t
#eval μ (η t) == t

def algF : F Nat → Nat
 | F.one x => x
 | F.two x y => x + y

def alg : FreeF Nat → Nat
  | FreeF.pure x => x
  | FreeF.one x => alg x
  | FreeF.two x y => (alg x) + (alg y)

#eval alg y
