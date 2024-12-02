class Monoid (α : Type u) where
  unit : α
  op   : α → α → α

instance [s : Monoid α] : OfNat α (nat_lit 1) where
  ofNat := s.unit

def getUnit [Monoid α] : α :=
  1

class M (a : Type) where
  fm : a -> Int

instance :  M Nat where
  fm x := x + 1

instance :  M String where
  fm x := String.length (x) + 1

#eval M.fm 10
#eval M.fm "test"

/- #eval [1,2,3,"ok"] -/
def northernTrees : Array (String ⊕ Nat) :=
  #[Sum.inl "sloe", Sum.inl "birch", Sum.inl "elm", Sum.inl "oak"]

#eval northernTrees

structure Drawing where
  x : String
deriving Repr

#eval Drawing.mk "o"

class Render (a : Type) where
  render : a -> Drawing

instance :  Render Nat where
  render x := Drawing.mk (String.append (String.append "<text>" (toString x) ) "</text>")

/- #eval String.append (String.append "<text>" "ok") "</text>" -/

#eval Render.render 10
