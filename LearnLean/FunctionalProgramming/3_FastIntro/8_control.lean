inductive Pos : Type where
  | one : Pos
  | succ (p : Pos)

def addNatPos : Nat → Pos → Pos
  | 0, p => p
  | n + 1, p => Pos.succ (addNatPos n p)

def addPosNat : Pos → Nat → Pos
  | p, 0 => p
  | p, n + 1 => Pos.succ (addPosNat p n)

instance {n : Nat} : OfNat Pos (n + 1) where
  ofNat :=
    let rec natPlusOne : Nat → Pos
      | 0 => Pos.one
      | k + 1 => Pos.succ (natPlusOne k)
    natPlusOne n


class HPlus (α : Type) (β : Type) (γ : Type) where
  hPlus : α → β → γ

instance : HPlus Nat Pos Pos where
  hPlus := addNatPos

instance : HPlus Pos Nat Pos where
  hPlus := addPosNat

#eval (HPlus.hPlus (3 : Pos) (5 : Nat) : Pos)

-- #eval HPlus.hPlus (3 : Pos) (5 : Nat)
-- The commented code above does not work. Why?
-- Because HPlus signature has two input types and an output type
-- Thus, we can declare another behavior where the output is something different

instance : HPlus Pos Nat String where
  hPlus := fun x y => "eyo"

#eval (HPlus.hPlus (3 : Pos) (5 : Nat) : String)


namespace HPlus2
-- We can avoid having to pass the output type using outParam
-- outParam is a special annotation that indicates
-- that a type is an output parameter of a function.
-- Hence, this gives more context for Lean to
-- infer which type this should be.
-- Note that this allows us to write our function
-- without writing the specific type, even when we have
-- ambiguity. Check the example below:
class HPlus (α : Type) (β : Type) (γ : outParam Type) where
  hPlus : α → β → γ

instance : HPlus Pos Nat String where
  hPlus := fun x y => "ok"

instance : HPlus Nat Pos Pos where
  hPlus := addNatPos

instance : HPlus Pos Nat Pos where
  hPlus := addPosNat

#eval HPlus.hPlus (3 : Pos) (5 : Nat)

-- The output above uses the Pos → Nat → Pos, instead of the Pos → Nat → String
-- Why? Because Lean tries to infer what makes more "sense" in the context.
-- Since Pos is already in the input type, then the probable "default"
-- output type is the Pos, instead of String.

instance : HPlus Pos Nat Nat where
  hPlus := fun x y => 0

#eval HPlus.hPlus (3 : Pos) (5 : Nat)
-- If we also declare a Pos → Nat → Nat, the new default becomes the output Nat.
end HPlus2

-- At last, we can control this default type.
-- But one must be careful! As this might lead to confusing type inference.
namespace HPlus3

@[default_instance]
instance {α : Type} [Add α] : HPlus α α α where
  hPlus := Add.add

#check HPlus.hPlus 2
-- It returns a type Nat → Nat, because it assumes that the
-- default output type will be the same as the input.

end HPlus3

namespace HPlus4

class HPlus (α : Type) (β : Type) (γ : outParam Type) where
  hPlus : α → β → γ

@[default_instance]
instance : HPlus Pos Nat String where
  hPlus := fun x y => "ok"

instance : HPlus Pos Nat Pos where
  hPlus := addPosNat

#check HPlus.hPlus (2 : Pos)

#eval HPlus.hPlus (2 : Pos) 2
#eval HPlus.hPlus (2 : Pos) (2 :Nat)

end HPlus4
