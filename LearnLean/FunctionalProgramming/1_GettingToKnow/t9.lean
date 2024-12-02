/- Anonymous Functions -/

#check fun x => x+1

#eval (fun x => x+1) 2

def f := fun x => x+1
#check f
#eval f 2

def g := fun (x : Int) : Int => x+1
