inductive Pos
  | one : Pos
  | succ : Pos → Pos
deriving Repr

def Pos.toNat : Pos → Nat
  | one => 1
  | Pos.succ p => Nat.succ (Pos.toNat p)

#eval Pos.toNat Pos.one
#eval Pos.toNat (Pos.succ Pos.one)
#eval Pos.toNat (Pos.succ (Pos.succ Pos.one))

instance : Coe Pos Nat where
  coe := Pos.toNat


#eval (fun x : Nat => 1 + x) 1
#eval (fun x : Nat => 1 + x) Pos.one


class Monoid (m : Type) where
  e : m
  op : m → m → m


instance : Monoid Nat where
  e := 1
  op := (· * ·)
  -- op := fun x y => x + y

infixr:80 " ⊕ " => Monoid.op

#eval 1 ⊕ 2 ⊕ Monoid.e
#eval (Monoid.e : Nat)

instance {α : Type}: Monoid (List α)  where
  e := []
  op := List.append
#eval [1] ⊕ [2] ⊕ ([] : List Nat)
#eval [1] ⊕ [2] ⊕ Monoid.e


structure Adder where
  howMuch : Nat

def add5 : Adder := ⟨5⟩
instance : CoeFun Adder (fun _ => Nat → Nat) where
  coe a := (· + a.howMuch)

#eval add5 3


structure Point (α : Type) where
  x : α
  y : α
deriving Repr

instance {α : Type} [Monoid α] : CoeFun (Point α) (fun _ => Unit → (Point α)) where
  coe _ := fun _ => Point.mk Monoid.e Monoid.e

def p : Point Nat := Point.mk 3 2
#eval p ()
