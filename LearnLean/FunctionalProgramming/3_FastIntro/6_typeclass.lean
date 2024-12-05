class Plus (α : Type) where
  plus : α → α → α

instance : Plus Nat where
  plus := Nat.add

instance : Plus String where
  plus := String.append

instance {α : Type} : Plus (List α) where
  plus := List.append

open Plus (plus)

#eval plus 5 3
#eval plus "Ok" "my"
#eval plus [1,2] [3,4]


inductive Pos : Type where
  | one : Pos
  | succ (p : Pos)

-- def toNat (p : Pos) : Nat :=
--   match p with
--   | Pos.one => 0
--   | Pos.succ p => Nat.succ (toNat p)

def toNat : Pos → Nat
  | Pos.one => 1
  | Pos.succ p => Nat.succ (toNat p)

def p := Pos.succ (Pos.succ Pos.one)
def q := Pos.succ (Pos.succ (Pos.succ Pos.one))

def Pos.plus : Pos → Pos → Pos
  | Pos.one, q => Pos.succ q
  | Pos.succ p, q =>  Pos.succ (Pos.plus p q)

instance : Plus Pos where
  plus := Pos.plus
#eval p
#eval toNat p
#eval toNat (plus p p)
#eval toNat (plus p q)

instance : Add Pos where
  add := Pos.plus

#eval p + q

def posToString : Pos → String
  | p => toString (toNat p)

instance : ToString Pos where
  toString := posToString

#eval p + q

-- class OfNat (α : Type) (_ : Nat) where
--   ofNat : α

-- instance : OfNat Pos 1 where
--   ofNat := Pos.one
-- instance : OfNat Pos 2 where
--   ofNat := Pos.succ Pos.one

instance {n : Nat} : OfNat Pos (n + 1) where
  ofNat :=
    let rec natPlusOne : Nat → Pos
      | 0 => Pos.one
      | k + 1 => Pos.succ (natPlusOne k)
    natPlusOne n


def x : Pos := 2
