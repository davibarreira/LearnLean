set_option autoImplicit true


structure PPoint (α : Type) where
  x : α
  y : α
deriving Repr

def p : PPoint Int := {x:= 1, y:=2}

def replacex (α : Type) (p : PPoint α) (newx : α) :=
  {p with x:= newx}

#eval replacex Int p 10

def replaceX {α : Type} (p : PPoint α) (newx : α) :=
  {p with x:= newx}

#eval replaceX p 10

inductive Sign where
  | pos
  | neg

def posOrNegThree (s : Sign) : match s with | Sign.pos => Nat | Sign.neg => Int :=
  match s with
  | Sign.pos => (3 : Nat)
  | Sign.neg => (-3 : Int)

#check posOrNegThree


def lengthlist {α : Type} (l : List α) : Nat :=
  match l with
  | [] => 0
  | x::xs => 1 + lengthlist xs

#eval lengthlist [1,2,3,4,1]
#eval [1,2,3,4,1].length

def List.lengthlist {α : Type} (l : List α) : Nat :=
  match l with
  | [] => 0
  | x::xs => 1 + lengthlist xs

#eval [1,2,3,4,1].lengthlist

inductive Maybe (α : Type) : Type where
  | algo (x : α) : Maybe α
  | nada : Maybe α
deriving Repr

-- We can make the implementation "clear"
-- inductive CleanMaybe (α : Type) where
--   | some (x : α)
--   | none
-- deriving Repr
-- Note that it is inferring many of the types omitted

def maybe1 : Maybe Int := Maybe.algo 10
def maybe2 : Maybe Int := Maybe.nada

#eval (Maybe.nada : Maybe Int)

#eval Maybe.algo 10
#eval maybe1
#check maybe2

#eval Maybe.algo (Maybe.algo 10)
-- #eval Maybe.algo (Maybe.nada)


-- Lean has the Option type which is implemented as the Maybe
#eval some (some 10)

def List.head??? {α : Type} (l : List α) : Option α :=
  match l with
  | [] => none
  | x :: _ => some x

#eval [1,2,3].head???
#eval ([] : List Int).head???
#check ([] : List Int).head???

-- head? is the already present implementation
#eval [].head? (α := Int)

structure ProdType (α : Type) (β : Type) : Type where
  first : α
  second : β

def st : ProdType Int String := ProdType.mk 10 ""
def st2 : ProdType Int String := ⟨10, ""⟩

-- def ml : List (String ⊕ Int) := [10, "ok"]
def ml : String ⊕ Int := Sum.inr 10
#check ml

def PetName : Type := String ⊕ String

inductive SumType (α : Type) (β : Type) : Type where
  | right (x : α)
  | left (x : β)
  deriving Repr

inductive SumType2 (α : Type) (β : Type) : Type where
  | left : α → SumType2 α β
  | right : β → SumType2 α β
  deriving Repr


def var : SumType Int String := SumType.right 10
def listvar : List (SumType Int String) :=
  [SumType.right 10,SumType.left "ok"]

#eval var

def M : Type := SumType2 Int String
#eval (SumType2.left 10 : M)

def PetType : Type := String ⊕ String

def selectFirst {α : Type} {β : Type} (l : List (Sum α β)) : List α :=
  match l with
  | [] => []
  | Sum.inl x :: xs => x :: selectFirst xs
  | Sum.inr _ :: xs => selectFirst xs

def petlist : List PetType :=
  [Sum.inr "bug", Sum.inl "dog1", Sum.inl "dog2", Sum.inr "cat"]

#eval selectFirst petlist

inductive ArithExpr (ann : Type) : Type where
  | int : ann → Int → ArithExpr ann
  | plus : ann → ArithExpr ann → ArithExpr ann → ArithExpr ann
  | minus : ann → ArithExpr ann → ArithExpr ann → ArithExpr ann
  | times : ann → ArithExpr ann → ArithExpr ann → ArithExpr ann
  deriving Repr

def exp  : ArithExpr Int := (ArithExpr.int 1 1)

#check ArithExpr.plus (ArithExpr.int (ArithExpr.int 1 1) 1)

#check (ArithExpr.int (ArithExpr.int 1 1) 1)
#check ()

def myf : Nat → Int → Nat :=
  fun x => (fun y => 1)

#check myf 10

def unzip {α β : Type} : List (α × β) → List α × List β
  | [] => ([], [])
  | (x,y)::xys =>
  let (xs,ys) : List α × List β := unzip xys
  (x :: xs, y :: ys)

#eval unzip [(1,2),(2,3)]


#eval (1,0).fst
#eval 1 :: [1,2]

#eval List.map (fun x => x + 1) [2,3]


inductive Inline : Type where
  | lineBreak
  | string : String → Inline
  | emph : Inline → Inline
  | strong : Inline → Inline

def Inline.string? (inline : Inline) : Option String :=
  if let Inline.string s := inline then
    some s
  else none

def x := 2
#eval if x = 2 then true else false
#check (· + 1) 2

-- A Lean naming convention is to define operations that might fail in
-- groups using the suffixes ? for a version that returns an Option,
-- ! for a version that crashes when provided with invalid input, and D
-- for a version that returns a default value when the operation would otherwise fail.
-- For instance, head requires the caller to provide mathematical evidence
-- that the list is not empty, head? returns an Option, head! crashes the
-- program when passed an empty list, and headD takes a default value
-- to return in case the list is empty. The question mark and exclamation mark
-- are part of the name, not special syntax, as Lean's naming rules are more
-- liberal than many languages.
