inductive BinTree (α : Type) where
  | leaf : BinTree α
  | branch : BinTree α → α → BinTree α → BinTree α
deriving Repr

open BinTree
def exampleTree : BinTree Nat :=
  branch (branch leaf 1 leaf) 2 (branch leaf 3 leaf)

#eval exampleTree

#eval branch leaf 1 leaf
