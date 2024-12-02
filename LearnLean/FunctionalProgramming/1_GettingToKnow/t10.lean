namespace NewNamespace
def triple (x : Nat) := 3*x
end NewNamespace

#eval NewNamespace.triple 3

def timesTwelve (x : Nat) :=
  open NewNamespace in
  4 * (triple x)

#eval timesTwelve 1

open NewNamespace in
#check triple

/- This one does not work -/
/- because it is not anymore in the -/
/- `open NewNamespace` context -/
#check triple

inductive Inline : Type where
  | lineBreak
  | string : String → Inline
deriving Repr

#check Inline.lineBreak
#eval Inline.lineBreak
#check Inline.string

#check Inline.string "ok"
def s := Inline.string "MyInline"
def s2 := Inline.lineBreak

def Inline.getD : Inline → String
  | Inline.string s => s
  | _ => ""

#eval Inline.getD s
#eval Inline.getD s2

def Inline.get? (x:Inline) : Option String :=
  match x with
  | Inline.string s => some s
  | _ => none

#eval Inline.get? s
#eval Inline.get? s2

#eval s!"check string interpolation for {s.getD}"

def h (x : Nat) : Nat :=
  let y : Nat := 2
  y + 1

#eval h 10

#eval ([Sum.inl 10, Sum.inr " ok"] : List (Nat ⊕ String))
