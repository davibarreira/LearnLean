structure PPoint (α : Type ) where
  x : α
  y : α
deriving Repr

#eval PPoint.mk 1 1

def natOrigin : PPoint Nat :=
  { x := Nat.zero, y := Nat.zero}


def replaceX (α : Type) (point : PPoint α) (newX : α) : PPoint α :=
  { point with x := newX}

#eval replaceX Nat natOrigin 1

inductive Sign where
  | pos
  | neg
deriving Repr

def posOrNegThree (s:Sign) : match s with | Sign.pos => Nat | Sign.neg => Int :=
  match s with
  | Sign.pos => (3 : Nat)
  | Sign.neg => (-3 : Int)

#eval posOrNegThree Sign.pos

inductive MyList (α : Type) where
  | nil : MyList α
  | cons : α → MyList α → MyList α
deriving Repr

#check List.cons 10 List.nil
#check List.cons 2 (List.cons 10 List.nil)
#eval List.cons 2 (List.cons 10 List.nil)

#check MyList.nil
#check MyList.cons 10 MyList.nil

def length (α : Type) (xs:List α) : Nat :=
  match xs with
  | List.nil => 0
  | List.cons y ys => 1 + length α ys

#eval length Nat [1,2,3,4]
#eval length String ["ok","ok"]
#eval (MyList.nil : MyList Nat)
#eval ([] : List String)

def xval : List Nat := []
#eval ([] : List Nat)

def length2 (α : Type) (xs:List α) : Nat :=
  match xs with
  | [] => 0
  | y :: ys => 1 + length α ys

def repleceX {α : Type} (point : PPoint α) (newX : α) : PPoint α :=
  { point with x := newX}

#eval repleceX (PPoint.mk 1 2) 10

def length3 {α : Type} (xs:List α) : Nat :=
  match xs with
  | [] => 0
  | y :: ys => 1 + length3 ys

#eval length3 ["ok","ok"]


def negprimes : List Int := [-1,-2,-3]

#eval negprimes

#eval negprimes.length

inductive Maybe (α:Type) where
  | some (val : α) : Maybe α
  | none : Maybe α
deriving Repr

#eval Maybe.some 10
#eval (Maybe.none : Maybe Int)

def xl : List Int := [1,2]
def empty : List Int := []

#eval List.length xl
def listhead? {α : Type} (x : List α) : Maybe α :=
  match x with
  | [] => Maybe.none
  | List.cons y ys => Maybe.some y


#eval listhead? xl
#eval listhead? empty

/- the standard Lean function -/
#eval xl.head?
/- #eval [].head? -/

#eval [].head? (α := Int)
#eval ([] : List Int).head?
