inductive G (α : Type) where
  | nil : G α
  | cons : α → (G α) → G α
deriving Repr

def _g := (G.nil : G Int)

structure F (α : Type) where
  x : α
deriving Repr

instance : Functor F where
  map f A := F.mk (f A.x)
  -- map := fun f A => F.mk (f A.x)

def x : F Int := {x := 2}
#eval (fun x => x*2) <$> x
#eval Functor.map (fun x => x*2) x


def fin {α : Type} : G α → G α
  | a => a

instance : Functor G where
  map := fun f =>
    let rec mapAux
      | G.nil => G.nil
      | G.cons x xs => G.cons (f x) (mapAux xs)
    mapAux

def g : G Int := G.cons 1 (G.cons 10 G.nil)
#eval (fun x => x*2) <$> g


structure K (α : Type) where
  x : α
  y : α
deriving Repr

instance : Functor K where
  map f A := K.mk (f A.y) (f A.x)


def k : K Int := K.mk 1 2

#eval id <$> k
