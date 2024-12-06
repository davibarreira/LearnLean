inductive F (α : Type) where
  | one : α → F α
  | two : α → α → F α
deriving Repr

def x : F Nat := F.one 10
#eval x

def y : F Nat := F.two 1 2
#eval y

def getvalue {α : Type} (x : F α) : List α :=
  match x with
  | F.one a =>  [a]
  | F.two a b =>  [a,b]

#eval getvalue x
#eval getvalue y

instance : Functor F where
  map f Fa :=
  match Fa with
    | F.one a => F.one (f a)
    | F.two a b => F.two (f a) (f b)

def double (x : Nat) : Nat := 2*x
#eval double <$> x
#eval Functor.map double x
#eval double <$> y
#eval Functor.map double y

instance {α : Type} [Add α]: Add (F α) where
  add a b :=
    match (a, b) with
      | (F.one a, F.one b) => F.one (a + b)
      | (F.one a, F.two b1 b2) => F.two (a + b1) (a + b2)
      | (F.two a1 a2, F.one b) => F.two (a1 + b) (a2 + b)
      | (F.two a1 a2, F.two b1 b2) => F.two (a1 + b1) (a2 + b2)

instance {α : Type} [Mul α]: Mul (F α) where
  mul a b :=
    match (a, b) with
      | (F.one a, F.one b) => F.one (a * b)
      | (F.one a, F.two b1 b2) => F.two (a * b1) (a * b2)
      | (F.two a1 a2, F.one b) => F.two (a1 * b) (a2 * b)
      | (F.two a1 a2, F.two b1 b2) => F.two (a1 * b1) (a2 * b2)

#eval x + x
#eval x + y
#eval y + y
#eval y * x + x

instance : Mul String where
  mul a b := a ++ b

def z := F.two "a" "b"
#eval z * z



-- inductive G (α : Type) where
--   | one : α → G α
--   | two : α → α → G α
--   | act : Float → α → G α
-- deriving Repr

-- instance : Functor G where
--   map f Ga :=
--   match Ga with
--     | G.one a => G.one (f a)
--     | G.two a b => G.two (f a) (f b)
--     | G.act a b => G.act a (f b)

-- instance {α γ : Type} [HMul γ α α]: HMul γ (G α) (G α) where
--   hMul n g := (n * ·) <$> g
    -- match g with
    --   | G.one a => G.one (n * a)
    --   | G.two a b => G.two (n * a) (n * b)
    --   | G.act fl ga => G.two (n * a) (n * b)

-- def z : G (G Nat) := G.act 1 (G.one 10)
-- def z := G.two (G.two (G.one 1) (G.one 1) ) (G.one 10)
-- #eval 10 * z

-- #eval 10 * z
