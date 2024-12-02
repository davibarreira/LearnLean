import Mathlib

class MInterface (a : Type) where
  fm : a -> Int

instance :  MInterface Nat where
  fm x := x + 1
instance :  MInterface String where
  fm x := String.length (x) + 1

-- instance (priority := 1) (α : Type*) [MInterface α] : Repr α where reprPrec m n := "no-repr"
--
instance (priority := 1) (α : Type _) : Repr α where reprPrec m n := "no-repr"


structure M where
  {T : Type}
  [inst : MInterface T]
  [repr : Repr T]
  val : T
  deriving Repr



-- structure M where
--   {T : Type}
--   [inst : MInterface T]
--   [repr : Repr T]
--   val : T

-- instance : Repr M where
--   reprPrec m n := @Repr.reprPrec m.T m.repr m.val n

-- def x : M := ⟨1⟩
-- def lx : List M := [⟨1⟩, ⟨"ok"⟩]
-- #eval x
-- #eval lx

-- structure MyType where
--   _1 : Int
--   _2 : Int

-- instance :  MInterface MyType where
--   fm m := m._1 + m._2

-- def m1 : MyType := MyType.mk 1 2
-- def m2 : MyType := ⟨1, 2⟩

-- #eval m1

-- def l2 : List M := [⟨1⟩, ⟨"ok"⟩, ⟨m1⟩]
-- #eval l2

-- structure M where
--   {T : Type}
--   [inst : MInterface T]
--   val : T
--   repr : Repr T := by first | infer_instance | exact defaultRepr

-- instance : Repr M where
--   reprPrec m n := @Repr.reprPrec m.T m.repr m.val n

-- def defaultRepr {T : Type} : Repr T where
--   reprPrec _ _ := "no-repr".quote

structure MyType where
  _1 : Int
  _2 : Int
instance :  MInterface MyType where
  fm m := m._1 + m._2

def m1 : MyType := MyType.mk 1 2

def l2 : List M := [M.mk 1, M.mk "ok", M.mk m1] -- [1, "ok", "no-repr"]
#eval l2

def l3 : List M := [⟨1⟩, ⟨"ok"⟩, ⟨m1⟩]
#eval l3
