-- We previously created our own Monad class.
-- Let us now instead use Lean's own implementation

#eval [1,2,3]


def myf (a : Nat) : Option Nat :=
  if a > 10 then some a
  else none

#eval Option.bind (some 10) myf
#eval Option.bind (some 11) myf

def liftList {α : Type} (x : α) : List α := [x]
-- def liftList (α : Type) (x : α) : List α := [x]

#eval List.flatMap [1,2] (fun x : Nat => [x + 1])
#eval List.flatMap [1,2] (liftList ∘ (fun x : Nat => x + 1))

#check List.map (fun x : Nat => x + 1)
#check liftList ∘ (fun x : Nat => x + 1)
#eval List.flatten [[1,2],[4,3]]

instance : Monad List where
  pure := liftList
  bind := List.flatMap

#eval bind [1,2] (fun x : Nat => [x + 1])
#eval bind [1,2] (pure ∘ (fun x : Nat => x + 1))

def x : List Nat := do
  let a ← pure 1
  let b ← pure 2
  let c ← pure 3
  pure (a + b - c)

#eval x
