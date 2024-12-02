structure Point where
  x : Float := 0.0
  y : Float := 0.0
deriving Repr
def origin : Point := {}
#eval origin


def x : Point := {x := 1.0}
#eval ({x := 1.0} : Point)

#eval Point.mk 0.0 0.0


def zerox (p : Point) : Point :=
  {x:= 0, y:= p.y}

#eval zerox x

def zeroX (p : Point) : Point :=
  {p with x :=0 }
#eval zerox x


#check ({ x := 0.0, y := 0.0 } : Point)
#check { x := 0.0, y := 0.0 : Point}

def Point.p : Point :=
  {x := 0, y := 0}

def point : Unit → Point :=
  fun _ => {x := 0.0, y := 0.0}

#eval point ()

#check Point.p

structure RectangularPrism  where
  prism::
  h : Float
  w : Float
  d : Float
  deriving Repr

def RectangularPrism.hw : RectangularPrism → (Float × Float) :=
  fun r => (r.h, r.w)

def volume (r : RectangularPrism) : Float :=
  r.h * r.w * r.d

def r : RectangularPrism := {h:=1,w:=1,d:=2}

#eval r.hw
#eval volume r

#check r
#check RectangularPrism.prism
