-- From Zulip "Church-encoded free monad"
set_option autoImplicit true

class Monoid (m : Type) where
  e : m
  op : m → m → m

infixr:80 " ⊕ " => Monoid.op

instance : Monoid Nat where
  e := 0
  op x y:= x + y

instance : Monoid (List α) where
  e := []
  op x y:= x ++ y

instance : Monoid String where
  e := ""
  op x y:= x ++ y

inductive Free (F : Type → Type) (α : Type u)
| protected pure : α → Free F α
| free : ∀ (β : Type) [Monoid β], F β → (β → Free F α) → Free F α

namespace Free

protected def bind : Free F α → (α → Free F β) → Free F β
| .pure a, f => f a
| free β fc g, f =>
  free β fc (fun c => g c |>.bind f)

instance instMonad : Monad (Free f) where
  pure := Free.pure
  bind := Free.bind


def η {F : Type → Type} {α : Type u} : α → Free F α := Free.pure


-- instance instMonadLift : MonadLift M (Free M) where
--   monadLift m := free _ m .pure


inductive F (α : Type) where
  | one : α → F α
  | two : α → α → F α
deriving Repr

instance : Functor F where
  map f a := match a with
    | F.one x => F.one (f x)
    | F.two x y => F.two (f x) (f y)

#check Free F

def x : Free F Nat := pure 10
def exampleBind : Free F Nat :=
  pure 10 >>= fun n => free Nat (F.one n) pure

def exampleDo : Free F Nat := do
  let n ← pure 10
  let m ← free Nat (F.one n) pure
  let p ← free Nat (F.two n m) pure
  pure (n + m + p)

def exampleDo2 : Free F Nat := do
  let a ← pure 1
  let b ← pure 2
  let c ← pure 3
  let m ← free Nat (F.two a b) pure
  let n ← free Nat (F.two m c) pure
  pure (n)

-- Tentative to make monadLift easier to use
-- def η {α : Type} (x : F α) : Free F α := (monadLift x : Free F α)
-- #check η (F.one 10)
-- def η {α : Type} (x : F α) : Free F α := do
--   let a ← pure x
--   pure (a)

-- #check (Free.pure (F.one 10) : Free F Nat)
-- def η (x : F α) : Free F α := Free.pure x
-- def η (x : F α) : Free F α :=

def algF [Monoid α] : F α → α
  | F.one x => x
  | F.two x y => x ⊕ y

def alg [Monoid α]: Free F α → α
  | Free.pure a => a
  | Free.free β fb m => alg (m (algF fb))

#eval alg exampleDo
#eval alg exampleDo2

def ex : Free F (List Nat) := do
  let a ← pure [1]
  let b ← pure [2]
  let c ← pure [3]
  let m ← free (List Nat) (F.two a b) pure
  let n ← free (List Nat) (F.two m c) pure
  pure (n)

#eval alg ex


def v (n : α) : Free F α := do
  let a ← pure n
  pure a

#check Free F Nat
#check Free F (Free F Nat)


def μ {M : Type 1 → Type 1} [Monad M] {α : Type 1} (mma : M (M α)) : M α :=
  mma >>= id

-- protected def bind : Free F (Free F a) → ( (Free F a)  → Free F β) → Free F β
#eval alg ((fun x : Nat => x-1) <$> exampleDo2)

def mm := v (v 10)
#check mm
-- #check mm >>= id

def ex2 : Free F String := do
  let a ← pure "a"
  let b ← pure "b"
  let c ← pure "c"
  let m ← free String (F.two a b) pure
  let n ← free String (F.two m c) pure
  pure (n)

#eval alg ex2
