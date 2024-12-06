class ShapeInterface (s : Type) where
  area : s → Float

structure Circle where
  radius : Float
deriving Repr

structure Rectangle where
  width : Float
  height : Float
deriving Repr

instance : ShapeInterface Circle where
  area c := 3.14 * c.radius * c.radius

instance : ShapeInterface Rectangle where
  area r := r.width * r.height
def c := Circle.mk 1
def r := Rectangle.mk 2 2
#eval c
#eval r

#eval ShapeInterface.area c

structure Shape where
  {T : Type}
  [inst : ShapeInterface T]
  val : T

def Shape.area : Shape → Float := fun s => s.inst.area s.val

structure CircRect where
  circ : Circle
  rect : Rectangle
deriving Repr

-- area c := 3.14 * c.radius * c.radius
instance : ShapeInterface CircRect where
  area cr := Shape.area ⟨cr.circ⟩ + Shape.area ⟨cr.rect⟩

def cr := CircRect.mk (Circle.mk 1) (Rectangle.mk 2 2)
#eval cr
#eval Shape.area ⟨c⟩
#eval Shape.area ⟨cr⟩

def ss : List Shape :=
  [⟨c⟩, ⟨c⟩, ⟨r⟩, ⟨cr⟩]

-- #eval ⟨c⟩

def cs := Shape.mk c
-- def cr := Shape.mk r
#eval cs.val
#check cs.inst
#check cs.T

#eval List.map (λx => Shape.area x) ss


class MInterface (a : Type) where
  fm : a -> Int

instance :  MInterface Nat where
  fm x := x + 1
instance :  MInterface String where
  fm x := String.length (x) + 1


structure M where
  {T : Type}
  [inst : MInterface T]
  [repr : Repr T]
  val : T

instance : Repr M where
  reprPrec m n := @Repr.reprPrec m.T m.repr m.val n

structure Y where
  a : Nat

instance : Repr Y where
  reprPrec y _ := repr y.a

def y : Y := { a := 1 }

-- instance [Repr T] : Repr (M) where
--   reprPrec m _ := repr m.val

def M.fm : M → Int := fun m => m.inst.fm m.val

def foo : List M :=
  [⟨1⟩, ⟨"example"⟩, ⟨3⟩]

#eval List.map (λx => M.fm x) foo
#eval foo



/- #eval ⟨1⟩.val -/

#eval List.map (λx => 2*x) [1,2,3]


def fooo : List M :=
  [{ T := Nat, inst := inferInstance, val := 1 },
   { T := String, inst := inferInstance, val := "example" },
   { T := Nat, inst := inferInstance, val := 3 }]

def myM : M := ⟨42⟩

#eval myM
#eval myM.val
#eval myM.inst.fm myM.val == 43
