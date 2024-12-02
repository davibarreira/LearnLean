#check true
#eval true

def isZero (n : Nat) : Bool :=
  match n with
    | Nat.zero => true
    | _ => false


inductive Natural where
 | zero
 | succ (x : Natural)
 deriving Repr


#eval Natural.zero
#eval Natural.succ Natural.zero
#eval Natural.succ (Natural.succ Natural.zero)


def n := Natural.succ (Natural.succ Natural.zero)

def count_natural (n : Natural) : Int :=
  match n with
  | Natural.zero => 0
  | Natural.succ i => 1 + count_natural i

#eval count_natural n
#eval count_natural Natural.zero
#eval count_natural (Natural.succ Natural.zero)


inductive MyList (α : Type) where
  | nil
  | const (x : α) (l : MyList α)
  deriving Repr

def l1 : MyList Int := MyList.nil
def l2 : MyList Int := MyList.const 10 MyList.nil
def l3 : MyList Int := MyList.const (-1) (MyList.const 10 MyList.nil)

def unpack_mylist {α : Type} (l : MyList α) : List α :=
  match l with
  | MyList.nil => []
  | MyList.const x tail => x :: unpack_mylist tail

#eval l1
#eval unpack_mylist l2
#eval unpack_mylist l3
