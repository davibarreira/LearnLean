def x : Option Int := some 10
def y : Option Int := none
#eval x
#eval y

def lastElement {α : Type} (xs : List α) : Option α :=
  match xs with
  | [] => none
  | List.cons y [] => some y
  | List.cons y ys => lastElement ys

#eval lastElement ([] : List Int)
#eval lastElement [1]
#eval lastElement [1,2]
#eval lastElement [1,2,3]


def findFirst? {α : Type} (xs:List α) (predicate : α → Bool) : Option α :=
  match xs with
  | [] => none
  | List.cons y ys => if predicate y
    then (some y)
    else findFirst? ys predicate


/- #eval findFirst? [1,2,3]  -/
def p (n : Nat) : Bool :=
  match n with
  | 1 => true
  | 2 => true
  | _ => false


#eval p 0
#check p

#eval findFirst? [3,0,2,1] p


def Prod.swap {α β : Type} (pair : α × β) : β × α :=
  {fst:= pair.snd, snd := pair.fst : β × α}


def z : String × Int := ("ok",2)

#eval z.swap


def myzip {α β : Type} (xs : List α) (ys : List β) : List (α × β) :=
  match xs, ys with
  | x :: xs', y :: ys' => (x, y) :: myzip xs' ys'
  | _,_ => []

#eval myzip ([] : List Int) [3,4,4,5]
#eval myzip [1,2,3] [3,4,4,5]

def take {α : Type} (n:Nat) (xs : List α) : (List α) :=
  match xs with
  | [] => []
  | x :: xs' => if n > 0 then x :: (take (n-1) xs') else []

#eval take 0 [1,2,3]
#eval take 1 [1,2,3]
#eval take 2 [1,2,3]
#eval take 3 [1,2,3]
#eval take 4 [1,2,3]
