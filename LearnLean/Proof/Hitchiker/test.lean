import Mathlib

namespace MyNamespace
#eval 2 + 1 * 2

#eval 0

#check 0
#check abs

opaque a : ℤ → ℤ
#check fun x : ℤ ↦ a x

inductive Natural : Type where
| zero : Natural
| succ : Natural → Natural


inductive New (p : ℤ) (q : ℤ) : Type where
| p
| q

def ex : New 3 4 := New.p
def x : Natural := Natural.succ Natural.zero

inductive AExp : Type where
| num : ℤ → AExp
| var : String → AExp
| add : AExp → AExp → AExp
| sub : AExp → AExp → AExp
| mul : AExp → AExp → AExp
| div : AExp → AExp → AExp

def y1 : AExp := AExp.num 1
def y2 : AExp := AExp.var "1"

#check AExp.add y1 y1

def evalAExp : AExp → ℤ
| AExp.num n => n
| AExp.var s => String.length s  -- Assuming variables evaluate to 0 for simplicity
| AExp.add e1 e2 => evalAExp e1 + evalAExp e2
| AExp.sub e1 e2 => evalAExp e1 - evalAExp e2
| AExp.mul e1 e2 => evalAExp e1 * evalAExp e2
| AExp.div e1 e2 => if evalAExp e2 = 0 then 0 else evalAExp e1 / evalAExp e2

#eval  evalAExp y2
#eval  evalAExp y1
#eval evalAExp (AExp.add y1 y1)


inductive List (α : Type) where
| nil : List α
| cons : α → List α → List α

#check List.nil
#check 1 :: [2]

def myList : List ℤ := List.cons 1 (List.cons 2 (List.cons 3 List.nil))

def listToString : List ℤ → String
| List.nil => "[]"
| List.cons h t =>
    let rec aux : List ℤ → String
    | List.nil => ""
    | List.cons h List.nil => toString h
    | List.cons h t => toString h ++ ", " ++ aux t
    "[" ++ aux (List.cons h t) ++ "]"

#eval listToString myList

#check [1,2,3,4]


end MyNamespace


def fib : ℕ → ℕ
| 0 => 0
| 1 => 1
| n + 2 => fib (n + 1) + fib n

#eval fib 20

def add : ℕ → ℕ → ℕ
| m, Nat.zero => m
| m, Nat.succ n => Nat.succ (add m n)

#eval add 1 1

def iter (α : Type) (f : α → α) (z : α) : ℕ → α
| Nat.zero => z
| Nat.succ n => f ((iter α f z) n)

def double (n : ℕ) : ℕ := n * 2

#eval iter ℕ double 1 5  -- This will compute double(double(double(double(double(1)))))

def myfun := fun x : ℕ ↦ 2*x

#check myfun
#eval myfun 10

def appendImplicit {α : Type} : List α → List α → List α
| List.nil, ys => ys
| List.cons x xs, ys => List.cons x (appendImplicit xs ys)

#eval appendImplicit [3, 1] [4, 1, 5]
#eval @appendImplicit ℤ [3, 1] [4, 1, 5]

def MixedList : Type := List (ℕ ⊕ String ⊕ Float)
-- #check [Sum.inl 1, Sum.inr "Ok"]
def mixed : MixedList := [Sum.inl 1, Sum.inr (Sum.inl "Ok"),Sum.inr (Sum.inr 0.0)]
