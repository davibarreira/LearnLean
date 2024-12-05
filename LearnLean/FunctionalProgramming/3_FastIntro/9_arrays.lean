set_option autoImplicit true

def x := #[1,2,3.5]
def x' := [1,2,3.5]

#check x
#check x'

structure NonEmptyList (α : Type) where
  head : α
  tail : List α
deriving Repr

def l : NonEmptyList Nat := {head:= 10, tail:=[1,2]}

abbrev NonEmptyList.inBounds {α : Type} (xs : NonEmptyList α) (i : Nat) : Prop :=
  i ≤ xs.tail.length


def NonEmptyList.get (xs : NonEmptyList α) (i : Nat) (ok : xs.inBounds i) : α :=
  match i with
    | 0 => xs.head
    | n + 1 => xs.tail[n]

theorem checkBound : l.inBounds 2 := by simp [l]

#eval NonEmptyList.get l 2 checkBound


-- Making things smoother with class GetElem

-- class GetElem (coll : Type) (idx : Type) (item : outParam Type) (inBounds : outParam (coll → idx → Prop)) where
--   getElem : (c : coll) → (i : idx) → inBounds c i → item

instance : GetElem (NonEmptyList α) Nat α NonEmptyList.inBounds where
  getElem := NonEmptyList.get

#eval l[0]
#eval l[2]
-- #eval l[20]
