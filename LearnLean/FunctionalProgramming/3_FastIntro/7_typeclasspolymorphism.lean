-- set_option autoImplicit true

structure Point (α : Type) where
  x : α
  y : α
deriving Repr

def Point.P (u:Unit) : Point Float := {x:=0.0, y:=0.0}

instance {α : Type} [Add α] : Add (Point α) where
  add p1 p2 := { x:= p1.x + p2.x, y:= p1.y + p2.y }


def p1 : Point Float := ⟨1,2⟩
def p2 : Point Float := ⟨1,0⟩
#eval p1
#eval p1 + p2

#check ()
#eval {x := 2, y:=2 : Point Float}

-- close the open
namespace Point
#eval P ()
end Point

#check Point

#check @OfNat.ofNat


inductive Even where
  | zero : Even
  | succ : Even → Even

-- instance {n : Nat} : OfNat Even n where
--   ofNat :=
--     let rec natPlusOne : Nat → Even
--       | 0 => Even.zero
--       | k => Even.succ (natPlusOne k) + 2
--     natPlusOne n


instance : OfNat Even 0 where
  ofNat := Even.zero

instance {n : Nat} [OfNat Even n] : OfNat Even (n+2) where
  ofNat := Even.succ (OfNat.ofNat n)

def exampleEven : Even := OfNat.ofNat 4
#eval exampleEven

def z : Even := 0
#eval z

-- def three : Even := 3
-- #eval three

def two : Even := 2
#eval two
def four : Even := 4
#eval four

def many : Even := 254
#eval 254/2

def Even.toNat : Even → Nat
  | Even.zero => 0
  | Even.succ s => 2 + Even.toNat s

def isEven : Nat → Bool
  | 0 => true
  | 1 => false
  | n + 2 => isEven n

#eval Even.toNat two
#eval Even.toNat four


def Even.plus : Even → Even → Even
  | Even.zero, e => e
  | Even.succ s, e => Even.plus s (Even.succ e)

#eval Even.plus two two
#eval Even.toNat (Even.plus two two)
#eval Even.toNat (Even.plus two four)

instance : Add Even where
  add := Even.plus

def Even.mul : Even → Even → Even
  | Even.zero, _ => Even.zero
  | Even.succ s, e => (Even.plus e (Even.mul s e)) + e


#eval Even.toNat (four + two + two)
#eval Even.toNat (four + four)
#eval Even.toNat (Even.mul two four)
#eval Even.toNat (Even.mul four four)
#eval Even.toNat (Even.mul four two)
#eval Even.toNat (Even.mul two (Even.mul four four))
#eval Even.toNat (Even.mul four (Even.mul four four))
#eval Even.toNat (Even.mul Even.zero (Even.mul four four))
#eval Even.toNat (Even.mul four Even.zero)
