inductive Free (f : Type → Type) (α : Type) where
  | pure : α → Free f α
  | free : ∀ x, f x -> (x -> Free f α) → Free f α

inductive F (α : Type) where
  | one : α → F α
  | two : α → α → F α
deriving Repr, Inhabited

def x : F Nat := F.one 10
#eval x


-- inductive F (α : Type) where
--   | one : α → F α
--   | two : α × α → F α

-- def x : F Nat := F.one 10
-- #eval x

-- structure F (α : Type) where
--   One : α
--   Two : α × α
-- deriving Repr


-- instance : Functor F where
--   map f a :=
--   match a with
--   | one =>
-- namespace Free

-- -- Functor instance for Free f
-- def map {f : Type → Type} {α β : Type} (g : α → β) : Free f α → Free f β
--   | pure a         => pure (g a)
--   | free x fx kx   => free x fx (λ x' => map g (kx x'))

-- instance {f : Type → Type} : Functor (Free f) where
--   map := map

-- -- Monad instance for Free f
-- def bind {f : Type → Type} {α β : Type} : Free f α → (α → Free f β) → Free f β
--   | pure a, k       => k a
--   | free x fx kx, k => free x fx (λ x' => bind (kx x') k)

-- instance {f : Type → Type} : Monad (Free f) where
--   pure := pure
--   bind := bind

-- end Free



-- Define a simple functor
-- inductive Toy (α : Type) where
--   | Output : String → α → Toy α

-- Functor instance for Toy
-- instance : Functor Toy where
--   map := λ α β f x =>
--     match x with
--     | Toy.Output str next => Toy.Output str (f next)

-- -- Helper function for output
-- def output (str : String) : Free Toy Unit :=
--   Free.free Unit (Toy.Output str ()) (λ _ => Free.pure ())

-- -- Example program using the Freer Monad
-- def program : Free Toy Unit := do
--   output "Hello, "
--   output "world!"

-- -- Interpreter for the Free Toy Monad
-- def runToy : ∀ {α}, Free Toy α → String
--   | _, Free.pure _ => ""
--   | _, Free.free x (Toy.Output str next) kx => str ++ runToy (kx next)

-- -- Run the example program
-- #eval runToy program  -- Outputs: "Hello, world!"
