structure Point where
  point ::
  x : Float
  y : Float
deriving Repr

#eval Point.point 1 2

#check (Point.x)

def mypoint : Point :=
  Point.point 1 2

#eval mypoint
#eval [Point.x mypoint,Point.y mypoint]

#eval "ok".append "x"

#eval {x:=1, y:=2 : Point}

def Point.fmap (f: Float → Float) (p:Point) : Point :=
  Point.point (f p.x) (f p.y)

#eval mypoint.fmap ( . + 1)

#eval Point.fmap ( . + 1) mypoint


#check True
def x : Bool := true
#eval x
