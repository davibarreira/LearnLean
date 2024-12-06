class MInterface (a : Type) where
  fm : a -> Int

instance :  MInterface Nat where
  fm x := x + 1

instance :  MInterface String where
  fm x := String.length (x) + 1


structure M where
  {T : Type}
  [inst : MInterface T]
  val : T

def M.fm : M → Int := fun m => m.inst.fm m.val

def foo : List M :=
  [⟨1⟩, ⟨"example"⟩, ⟨3⟩]

-- def m : M := ⟨1⟩
abbrev m : M := ⟨1⟩

#eval (fun x : Nat => 2*x) m.val
#eval m.val + 1
