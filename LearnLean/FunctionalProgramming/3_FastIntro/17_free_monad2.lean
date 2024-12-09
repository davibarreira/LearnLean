-- From Zulip "Church-encoded free monad"
set_option autoImplicit true

inductive Free (F : Type u → Type u) (α : Type u) : Type (u + 1)
| protected pure : α → Free F α
| free : ∀ (β : Type u), F β → (β → Free F α) → Free F α

-- def getF (x : Free F α) : Option (Σ β, (β → Free F α) × F β) :=
--   match x with
--   | Free.pure a => none
--   | Free.free β h fi => some ⟨β, fi, h⟩

-- def getPure (x : Free F α) : Option α :=
--   match x with
--   | Free.pure a => some a
--   | _ => none
namespace Free

protected def bind : Free F α → (α → Free F β) → Free F β
| .pure a, f => f a
| free β fc g, f =>
  free β fc (fun c => g c |>.bind f)

instance instMonad : Monad (Free f) where
  pure := Free.pure
  bind := Free.bind

instance instMonadLift : MonadLift M (Free M) where
  monadLift m := free _ m .pure


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



def algF : F Nat → Nat
  | F.one x => x
  | F.two x y => x + y

-- Tentative to make monadLift easier to use
def η {α : Type} (x : F α) : Free F α := (monadLift x : Free F α)

-- | free : ∀ (β : Type u), F β → (β → Free F α) → Free F α
-- def alg : Free F Nat → Nat
--   | Free.pure a => a
--   | Free.free Nat fb k => alg (k (algF fb))
--   | Free.free β fb k => 0

def redF [Add α] : F α → α
  | F.one x => x
  | F.two x y => x + y

def alg [Add α]: Free F α → α
  | Free.pure a => a
  | Free.free β fb m => alg (m (redF fb))
  -- | Free.free β Fb m => alg (bind Fb m)

  -- | Free.free β Fa m => alg (m (algF Fa))
  -- | Free.free β Fb m => m <$>

-- #eval alg exampleDo
#eval (fun x => x +1) <$> F.one 10

#check (fun x => x +1) <$> exampleDo

#check exampleDo2

#check exampleBind

#check η (F.one 10)

def exampleLift : Free F Nat :=
  monadLift (F.one 42)

#check (monadLift (F.one 42) : Free F Nat)


-- def x : Free F Nat := pure 10 >>= fun n => free Nat (F.one n) pure
-- def y : Free F Nat := free (pure 10)
-- #check Free (Free F)

/-! ## Now I can write `Pause` -/

inductive Pause.Op (σ : Type u) (α : Type u)
| mutate : (σ → σ) → α → Op σ α
| yield : α → Op σ α

#check Pause.Op Int

abbrev Pause (σ : Type u) :=
  Free (Pause.Op σ)

#check Pause Int

namespace Pause

def mutate (f : σ → σ) (next : α) : Pause σ α :=
  liftM (Pause.Op.mutate f next)

def yield (a : α) : Pause σ α :=
  liftM (Pause.Op.yield (σ := σ) a)

def done (a : α) : Pause σ α :=
  .pure a

def step (code : Pause σ Unit) (state : σ) : σ × Option (Pause σ Unit) :=
  match code with
  | .pure () => (state, none)
  | .free _ (.mutate f next) k =>
    let code := k next
    step code $ f state
  | .free _ (.yield next) k => (state, k next)

def test : Pause Nat Unit := do
  mutate Nat.succ ()
  yield ()
  mutate Nat.succ ()
  yield ()
  mutate Nat.succ ()
  yield ()
  done ()

/-- info:
-> 1
-> 2
-> 3
done 3
-/
#guard_msgs in
  #eval do
    if let (s, some nxt) := test.step 0 then
      println! "-> {s}"
      if let (s, some nxt) := nxt.step s then
        println! "-> {s}"
        if let (s, some nxt) := nxt.step s then
          println! "-> {s}"
          if let (s, none) := nxt.step s then
            println! "done {s}"

end Pause

end Free
