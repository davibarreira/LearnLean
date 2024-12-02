def add1 (n : Nat) : Nat := n + 1

#eval (1 + 2 : Int)

#eval 1 + 2
#eval 1 + 2*5

#eval String.append "hello" "lean"
#eval "hello" ++ "lean"
#eval String.append "great " (String.append "oak " "tree")

#eval 1 + (if 1 > 2 then 1 else 0)
#eval String.append (String.append "A" "B") "C"
#eval String.append "A" (String.append "B" "C")

#eval (1 - 2 : Int)

#check -1

def hello := "Hello"

#eval hello

/- In the code below we are saying that variable `lean` has type String -/
/- and it is equal to the string `Lean` -/
def lean : String := "Lean"

#eval String.append hello (String.append " " lean)

/- Defining a function that takes a value `n : Nat` and returns a value o type Nat -/
def add_1 (n : Nat) : Nat := n + 1

#check add_1
#check (add_1)

#eval add_1 1

def maximum (n : Nat) (k : Nat) : Nat :=
  if n < k then
    k
  else n

#check maximum

#eval maximum 10 2

#eval maximum (5+1) (2+2)

#check maximum 1

#eval (maximum 1) (2)

/- Exercises -/

def joinStringsWith (s1:String) (s2:String) (s3:String) : String :=
  String.append s2 (String.append s1 s3)

#eval joinStringsWith "," "one" " and another"

#check joinStringsWith " ok"

def volume (h:Nat) (w:Nat) (d:Nat) : Nat :=
  h * w * d


def Str : Type := String

#eval ("ok" : Str)

def aStr : Str := "This is a string"

def x : Nat := 10


def v : Nat := 2

#eval v
#check v
