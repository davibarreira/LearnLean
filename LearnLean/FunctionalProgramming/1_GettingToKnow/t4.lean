inductive MyBool where
  | false : MyBool
  | true : MyBool
deriving Repr

#check MyBool.true
#eval MyBool.true

inductive MyNat where
  | zero : MyNat
  | succ (n : MyNat) : MyNat
deriving Repr

#eval MyNat.zero
#eval MyNat.succ MyNat.zero

def tonum (n : MyNat) : Nat :=
  if n = MyNat.zero then 0 else 1

#eval tonum MyNat.zero

def isMyZero (n : MyNat) : Bool :=
  match n with
    | MyNat.zero => true
    | MyNat.succ k => false

def isZero (n : Nat) : Bool :=
  match n with
    | Nat.zero => true
    | Nat.succ k => false

#eval isZero 0
#eval isMyZero MyNat.zero
#eval isMyZero (MyNat.succ MyNat.zero)


structure Point where
  point ::
  x : Float
  y : Float
deriving Repr

def depth (p:Point) : Float :=
  match p with
    | {x:=u, y:=v} => v

#eval depth (Point.point 2 1)

def plus (n : Nat) (k : Nat) : Nat :=
  match k with
    | Nat.zero => n
    | Nat.succ k' => Nat.succ (plus n k')

#eval plus 1 2


def div (n : Nat) (k : Nat) : Nat :=
  if n < k then
    0
  else Nat.succ (div (n - k) k)

