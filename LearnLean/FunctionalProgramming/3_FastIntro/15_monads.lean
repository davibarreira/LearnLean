namespace Monad

-- This is my Personal Implementation for a Monad
-- Monad is a monoid in the category of endofunctors
-- Thus, it is characterized by an η (pure) and a μ : M M → M operator,
-- which is analogous to the monoidal operator ⊕ : M × M → M.
-- In Functional Programming, it is more
-- Common to define a Monad using an operator called `bind : M α → (α → M β) → M β`
-- The bind can be used to derive the μ and vice-versa
class Monad (m : Type → Type) [Functor m] where
  η {α : Type} :  α → m α
  μ {α} : m (m α) → m α
  -- pure {α : Type} :  α → m α
  -- bind {α β : Type} : m α → (α → m β) → m β

instance : Monad Option where
  η x := some x
  μ x := match x with
    | some a => a
    | none => none

-- Let us define the `bind` operator.
def Monad.bind {α β : Type} {m : Type → Type} [Functor m] [Monad m] : m α → (α → m β) → m β :=
  fun ma f => Monad.μ (Functor.map f ma)

infixl:55 " ⋊ " => Monad.bind

#eval Monad.μ (some (some 10))

def myf (a : Nat) : Option Nat :=
  if a > 10 then some a
  else none

#eval Monad.μ (Functor.map myf (some 20))
#eval some 20 ⋊ myf
#eval (none : Option Nat) ⋊ myf

#eval (fun x => x+1) <$> some 10

#check Option.none
#eval some 10

def x := [1,2,3]
#eval x[10]?

end Monad
