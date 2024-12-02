import Mathlib

#check Type
#check Type → Type
#check ℕ → Type

def F : Type → Type := List

#check List
#check F


def triple (x : Float) (y : Float) (z : Float):= x + y + z

#eval triple 2 3 1
def triple_curry :=  triple 2 3

#eval triple_curry 3

def x : Float := 2

def compose (α β γ : Type) (g : β → γ) (f : α → β) (x : α) : γ :=
  g (f x)

#check compose

#eval let y := 2; let z := 10
  z+y

def foo := let a := Nat; fun x : a => x + 2
#check foo
#eval foo 2
/-
  def bar := (fun a => fun x : a => x + 2) Nat
-/

variable (a : ℕ)
opaque b : ℕ


#check a
#check b
