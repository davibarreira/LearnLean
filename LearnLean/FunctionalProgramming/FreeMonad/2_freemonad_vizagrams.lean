set_option autoImplicit true

structure H where
  g : Float
deriving Repr, BEq

inductive F (α : Type) where
  | comp : α → α → F α
  | act : H → α → F α
deriving Repr, BEq

instance : Functor F where
  map f a := match a with
    | F.comp x y => F.comp (f x) (f y)
    | F.act h x => F.act h (f x)
