#check 1.2

structure Point where
  x : Float
  y : Float
deriving Repr

def origin : Point := {x := 0.0, y := 0.0}

#eval origin

#eval origin.x

def addPoints (p1:Point) (p2:Point) : Point :=
  {x:=p1.x + p2.x, y:=p1.y + p2.y}


def distance (p1:Point) (p2:Point) : Float :=
 Float.sqrt ((p2.x - p1.x)^2.0 + (p2.y - p1.y)^2.0)

#eval distance { x := 1.0, y := 2.0 } { x := 5.0, y := -1.0 }

/- Errors -/
#check {x:=1.0,y:=2.0}

#check ({x:=1.0,y:=2.0} : Point)
#check {x:=1.0,y:=2.0 : Point}

def zeroX (p:Point) : Point :=
  { x:= 0, y:= p.y}


def zeroX (p:Point) : Point :=
  {p with x:= 0}

#eval zeroX {x:=1 , y:=2 : Point}


/- Another syntax for constructing structures -/
#check Point.mk 1.2 0.6


