#eval 1 == 2
#eval 2 == 2
#eval true == true
#eval false == true
#eval false == false

#check 1 = 2 -- Prop
#check 1 == 2 -- Bool
#check (fun x : Nat => x + 1) = (Nat.succ ·)
-- #eval (fun x : Nat => x + 1) = (Nat.succ ·)
-- Code above fails! The proposition is not easily verified

structure T (α : Type) where
  x : α
deriving Repr

#eval hash 10
-- #eval hash (T.mk 10) -- Not hashable
