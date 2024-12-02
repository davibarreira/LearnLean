#eval 1 + 1 -- Infers the type
#check 1+1

#eval (1 + 1 : Int) -- Forces the type
#check (1 + 1 : Int) -- Forces the type

def hello := "ok" -- infers type for variable `hello`
def hello2 : String := "ok" -- fixes type

def multiplyby2 (x : Int) : Int := 2*x
def x : Nat := 10
#eval multiplyby2 x

def maximum (n : Int) (m : Int) : Int :=
  if n < m then
    m
  else
    n


#eval maximum 0 1

#check maximum 3

def m : Int → Int := maximum 10

#eval m 20

def NaturalNumber : Type := Nat

-- This returns an error. For this to work, the proper
-- way is to use abbrav as shown in the example below.
-- def two : NaturalNumber := 2

abbrev N : Type := Nat

def one : N := 1
