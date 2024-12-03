import SciLean

open SciLean Scalar RealScalar

set_option autoImplicit true
set_default_scalar Float

structure Transformation (n : ℕ) where
  A : Float^[n,n]
  b : Float^[n]

def Transformation.eval (f : Transformation n) (x : Float^[n]) := f.A * x + f.b
def Transformation.comp (f g : Transformation n) : Transformation n :=
  { A := f.A * g.A, b := f.A * g.b + f.b }

def Transformation.translate (t : Float^[n]) : Transformation n :=
{
  A := 𝐈 n
  b := t
}

def Transformation.rotate (θ : Float) : Transformation 2 :=
{
  A := ⊞[cos θ, -sin θ;sin θ, cos θ]
  b := 0
}

def Transformation.scale (n : ℕ) (s : Float) : Transformation n :=
{
  A := s • 𝐈 n
  b := 0
}

def examplePoint : Float^[2] := ⊞[1.0, 1.0]  -- Point [1, 1]
#eval examplePoint

def T : Transformation 2 :=
  Transformation.translate ⊞[2.0, 3.0]  -- Translation vector [2, 3]

def R : Transformation 2 :=
  Transformation.rotate π  -- Translation vector [2, 3]

#eval T.eval examplePoint
#eval (T.comp T).eval examplePoint
#eval R.eval examplePoint

instance : HMul (Transformation n) (Float^[n]) (Float^[n]) where
  hMul := Transformation.eval

instance : HMul (Transformation n) (Transformation n) (Transformation n) where
  hMul := Transformation.comp

infixr:80 " ∘ " => Transformation.comp

#eval T * examplePoint
#eval (T ∘ T) * examplePoint
#eval (T ∘ T ∘ T) * examplePoint
#eval T ∘ T ∘ T * examplePoint
