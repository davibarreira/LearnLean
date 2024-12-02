import SciLean
namespace SciLean

#check #[1,2,3]

def fib : Array Nat :=
  Id.run do
  let mut x : Array Nat := Array.mkEmpty 2
  x := x.push 0
  for i in [1:5] do
    x := x.push i
  x := x.push 0
  return x

#eval fib
#eval fib[0]
#eval fib.size

-- #eval fib[10] #crashes, because 10 is larger than the size
#eval fib[1]? -- Returns the Option
#eval fib[10]? -- Returns the Option



def fibonacci (n : Nat) : SciLean.DataArray UInt64 := Id.run do
    let mut fib : SciLean.DataArray UInt64 := SciLean.DataArray.mkEmpty n
    fib := fib.push 0
    fib := fib.push 1
    for i in [2:n] do
        fib := fib.push (fib[i-1]! + fib[i-2]!)
    return fib

#eval fibonacci 10

def x:= fibonacci 10

#check x

def fib2 (n : Nat) : DataArray UInt64 := Id.run do
    let mut fib : DataArray UInt64 := DataArray.mkEmpty n
    fib := fib.push 0
    fib := fib.push 1
    for i in [2:n] do
        fib := fib.push (fib[i-1]! + fib[i-2]!)
    return fib



def dot {n : Nat} (x y : Float^[n]) : Float := ∑ i, x[i] * y[i]

-- ⊞[1,1] does not work, because the values need to be floats
#eval dot ⊞[1.0,1.0] ⊞[1.0,1.0]

def u := ⊞[1.0, 2.0, 3.0]

#eval u[0]

#eval ∑ i, 2*u[i]

def A := ⊞[1.0, 2.0; 3.0, 4.0]

#eval A[0,0]
#eval A[(0,0)] = A[0,0]
#eval A[1,1]


-- def f : Fin 10 → Float :=


variable (f : Fin 10 → Float)
#check ⊞ i => f i


def myf := fun x => x + 1.0
#eval ⊞ i => myf u[i]

def outerProduct {n m : Nat} (x : Float^[n]) (y : Float^[m]) : Float^[n,m] :=
  ⊞ i j => x[i]*y[j]
