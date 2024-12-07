-- From Zulip "Church-encoded free monad"
set_option autoImplicit true

inductive Free (F : Type u → Type u) (α : Type u) : Type (u + 1)
| protected pure : α → Free F α
| free : ∀ (β : Type u), F β → (β → Free F α) → Free F α

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
