set_option autoImplicit true

structure F (α :Type) where
  x : α
deriving Repr


-- inductive FreeF (f : Type -> Type) (a : Type) where
--   | FMap : (i -> a) -> f i -> FreeF f a
  -- | FMap : ∀(i:Type), (i -> a) -> f i -> FreeF f a

-- inductive FreeF (f : Type -> Type) (a : Type) where
--   | FMap : ∀ i, (i -> a) -> f i -> FreeF f a

-- def getF (x : FreeF f a) : Σ i, (i → a) × f i :=
--   match x with
--   | FreeF.FMap i h fi => ⟨i, (h, fi)⟩

-- instance : Functor (FreeF f) where
--   map g
--     | FreeF.FMap β h fi => FreeF.FMap β (g ∘ h) fi

-- def x := F.mk 1
-- def y := FreeF.FMap Nat (fun x : Nat => x + 1) x

inductive FreeF (f : Type -> Type) (a : Type) where
  | FMap {i : Type}: (i -> a) -> f i -> FreeF f a

def getF (x : FreeF f a) : Σ i, (i → a) × f i :=
  match x with
  | FreeF.FMap h fi => ⟨_, (h, fi)⟩

def x := F.mk 1
def y := FreeF.FMap (fun x : Nat => x + 1) x

#check getF y
#eval (fun x : Nat=> x +1) ((getF y).snd.snd).x

-- #check (fun x :Nat => x +1) <$> y
-- def z := (fun x :Nat => x +1) <$> y
-- #check z
#check FreeF F

inductive FFree (f : Type u → Type u) (α : Type u) : Type (u + 1)
  | pure : α → FFree f α
  | impure : ∀ (x : Type u), f x → (x → FFree f α) → FFree f α
  -- | impure {x : Type}  : f x → (x → FFree f α) → FFree f α

#check FFree F

-- inductive FFree
-- data FFree f a where
-- Pure :: a →FFree f a
-- Impure :: f x →(x →FFree f a) →FFree f a
