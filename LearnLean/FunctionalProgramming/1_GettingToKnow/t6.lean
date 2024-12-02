structure Pair (α : Type) (β : Type) where
  _1 : α
  _2 : β
deriving Repr

#eval Pair.mk 1 1
#eval {_1:=1, _2:=2 : Pair Nat Nat}


def fives : String × Int := {fst := "five", snd:= 5}

#eval fives
def newfives : String × Int := ("five", 5)
#eval newfives

inductive DisjointPair (α : Type) (β : Type) : Type where
  | inleft : α → DisjointPair α β
  | inright : β → DisjointPair α β
deriving Repr

def DogName : Type := DisjointPair String String

def dogs : List DogName :=
  [DisjointPair.inleft "Pan", DisjointPair.inright "Ju"]


def PetName : Type := String ⊕ String

def animals : List PetName :=
  [Sum.inl "Rex", Sum.inr "Miau", Sum.inl "Pan", Sum.inl "Ju", Sum.inr "Judite"]

def countDogs (xs : List PetName) : Nat :=
  match xs with
  | [] => 0
  | List.cons (Sum.inl _) ys => 1 + countDogs ys
  | List.cons (Sum.inr _) ys => 0 + countDogs ys

#eval countDogs animals

#eval ()
