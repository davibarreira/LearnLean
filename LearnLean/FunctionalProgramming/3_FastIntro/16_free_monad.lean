-- /-! # Defining `Free` -/
set_option autoImplicit true

-- inductive Free (f : Type → Type) [Functor f] (α : Type) where
--   | pure : α → Free f α
--   | free : ∀ x, f x -> (x -> Free f α) → Free f α

-- def unfree [Functor F]: Free F α → F α
--   | Free.pure x => x
--   | Free.free a f => Functor.map a

-- def μ [Functor F] : Free F (F α) → Free F α
--   | Free.pure x => x
--   | Free.free a f => Functor.map μ a

-- μ(x::Pure{<:FreeF}) = x._1
-- μ(x::FreeF{<:FreeF}) = free(fmap(μ, unfree(x)))

inductive F (α : Type) where
  | one : α → F α
  | two : α → α → F α
deriving Repr

instance : Functor F where
  map f a := match a with
    | F.one x => F.one (f x)
    | F.two x y => F.two (f x) (f y)

-- #check (Free F (Free F Int))
-- #check (Free F Int)

-- F is shallow
-- def y := F.two (F.one 1) (F.two 2 (F.one 10))

inductive FreeF (α : Type) where
  | pure : α → FreeF α
  | one : FreeF α → FreeF α
  | two : FreeF α → FreeF α → FreeF α
deriving Repr

def y := FreeF.two (FreeF.one (FreeF.pure 1)) (FreeF.two (FreeF.pure 2) (FreeF.one (FreeF.pure 10)))


#check FreeF.pure 10
#check y
#check FreeF (FreeF Int)
#check FreeF.one (FreeF.pure (FreeF.pure 10))

def unfree : FreeF (F α) → F (FreeF α)
  | FreeF.pure a =>
    match a with
    | F.one x => F.one (FreeF.pure x)
    | F.two x y => F.two (FreeF.pure x) (FreeF.pure y)
  | FreeF.one a =>
    match unfree a with
    | F.one x => F.one (FreeF.one x)
    | F.two x y => F.two (FreeF.one x) (FreeF.one y)
  | FreeF.two a b =>
    match (unfree a, unfree b) with
    | (F.one x, F.one y) => F.two (FreeF.one x) (FreeF.one y)
    | (F.one x, F.two y z) => F.two (FreeF.one x) (FreeF.two y z)
    | (F.two x y, F.one z) => F.two (FreeF.two x y) (FreeF.one z)
    | (F.two x y, F.two z w) => F.two (FreeF.two x y) (FreeF.two z w)

-- def free : F (FreeF α) → FreeF (F α)
--   | F.one a =>
--     match a with
--     | FreeF.one

def free : F (FreeF α) → FreeF α
  | F.one a => FreeF.one a
  | F.two a b => FreeF.two a b

#eval unfree (FreeF.pure (F.one 10))

def t := FreeF.pure (F.one 10)
#eval free (unfree  t)

def FreeF.map (f : α → β) (a : FreeF α) : FreeF β :=
  match a with
  | FreeF.pure x => FreeF.pure (f x)
  | FreeF.one x => FreeF.one (FreeF.map f x)
  | FreeF.two x y => FreeF.two (FreeF.map f x) (FreeF.map f y)

instance : Functor FreeF where
  map := FreeF.map

def liftF {α β : Type} (f: α → β) : (F α → F β) := Functor.map f

def myf (x : Nat) : Nat := x + 1
#check liftF myf

def v := FreeF.one (FreeF.pure 10)
#eval myf <$> v


def η : α → FreeF α := fun a => FreeF.pure a

def μ : FreeF (FreeF α) → FreeF α
  | FreeF.pure a => a
  | FreeF.one a => μ a
  | FreeF.two a b => FreeF.two (μ a) (μ b)

def tt := FreeF.one (FreeF.pure (FreeF.one (FreeF.pure 10)))
#eval μ tt

def tt2 := FreeF.two (FreeF.pure (FreeF.one (FreeF.pure 10))) (FreeF.pure (FreeF.one (FreeF.pure 10)))
#eval μ tt2

-- #check fun x {α} : F α => Functor.map (fun x => x + 1)
-- #check t

  -- | (f, a) =>
  --   match a with
  --     | FreeF.pure x => FreeF.pure (f x)
  --     | _ x => FreeF.pure (f x)

-- fmap(f::Function, x::Pure)    = Pure(f(x._1))
-- fmap(f::Function, x::FreeF) = free(fmap(y->fmap(f,y), unfree(x)))
-- open FreeF in
-- def μ : FreeF (FreeF α) → FreeF α
--   | FreeF.pure a => a
--   | FreeF.one a =>


-- def unfree : FreeF α → F α
--   | FreeF.pure (F.one a) => a
-- instance : Functor FreeF where
--   map f a :=

-- instance : ToString Pos where
--   toString := posToString

-- #check Free F Int
-- #check (Free.pure 10 : Free F Int)
-- #check (Free.free Int (F.one 10) Free.pure : Free F Int)

-- #check (List.cons 10 List.nil : List Nat)
-- -- #check List.cons 10
-- #eval (List.nil : List Int)
