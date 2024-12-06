inductive F (α : Type) where
  | one : α → F α
  | two : α → α → F α
deriving Repr

def x : F Nat := F.one 10
#eval x

def y : F Nat := F.two 1 2
#eval y


def getvalue {α : Type} (x : F α) : List α :=
  match x with
  | F.one a =>  [a]
  | F.two a b =>  [a,b]

#eval getvalue x
#eval getvalue y
