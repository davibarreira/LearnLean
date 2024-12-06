-- F-algebra
def Algebra (f : Type → Type) (a : Type) := f a → a

inductive RingF (α : Type) where
  | RZero
  | ROne
  | RAdd (_1:α) (_2:α)
  | RMul (_1:α) (_2:α)
  | RNeg (_1:α)
deriving Repr, BEq

namespace RingF

def o := (ROne : RingF Int)
def x := RAdd 1 2
def y := RMul 3 5
def n := RNeg 1
#eval n == n

def x' := RAdd 2 2
#eval x == x'

def evalZ : Algebra RingF Int :=
  fun r => match r with
  | RZero => 0
  | ROne => 1
  | RAdd x y => x + y
  | RMul x y => x * y
  | RNeg x => -x

end RingF

-- Recursive Version
inductive Ring where
  | RZero
  | ROne
  | RAdd : Ring → Ring → Ring
  | RMul : Ring → Ring → Ring
  | RNeg : Ring → Ring

namespace Ring
def e1 := RZero
def e2 := RAdd ROne RZero

def evalZ : Ring → Int :=
  fun r => match r with
  | RZero => 0
  | ROne => 1
  | RAdd x y => evalZ x + evalZ y
  | RMul x y => evalZ x * evalZ y
  | RNeg x => - (evalZ x)

end Ring

def f : Nat → String → String :=
  fun n s => match (n,s) with
  | (0, s) => ""
  | (_, s) => s

#check f 10
