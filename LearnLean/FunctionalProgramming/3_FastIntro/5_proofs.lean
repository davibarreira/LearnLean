set_option autoImplicit true
#check rfl

def x : 1=1 := rfl

def OnePlusOneIsTwo : Prop := 1 + 1 = 2

#check OnePlusOneIsTwo

theorem onePlusOneIsTwo : OnePlusOneIsTwo := rfl

#check onePlusOneIsTwo

theorem _onePlusOneIsTwo : 1 + 1 = 2 := by
  simp

#check _onePlusOneIsTwo

def third (xs : List α) (ok : xs.length > 2) : α := xs[2]

def woodlandCritters : List String :=
  ["hedgehog", "deer", "snail"]

#eval third woodlandCritters (by decide)

def l : List Nat := [1,2,3]

theorem ok: l.length > 2 := by decide

#eval third l (by decide)


-- #eval l[2]

def thirdOption (xs : List α) : Option α := xs[2]?

#eval thirdOption woodlandCritters
