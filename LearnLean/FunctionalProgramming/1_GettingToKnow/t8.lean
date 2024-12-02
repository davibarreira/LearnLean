def length : List α → Nat
  | [] => 0
  | x::xs => 1 + length xs

#eval length [1,2,3]
#eval  (["ok",2,3] : List (String ⊕ Nat))
#check  List Int ⊕ Nat
/- #check  ([Sum.inl "ok", Sum.inr 1] : List String ⊕ Nat) -/
#eval ([Sum.inr "ok",Sum.inl 10] : List (Int ⊕ String))

def drop : Nat → List α → List α
  | 0, xs => xs
  | _, [] => []
  | Nat.succ n, x::xs => drop n xs

#eval drop 1 [1,2]


/- Innefficient! -/
/- def unzip : List (α × β) → List α × List β -/
/-   | [] => ([],[]) -/
/-   | (x,y) :: xys => (x :: (unzip xys).fst, y :: (unzip xys).snd) -/


def unzip : List (α × β) → List α × List β
  | [] => ([],[])
  | (x,y) :: xys =>
    let unzipped : List α × List β := unzip xys
    (x :: unzipped.fst, y :: unzipped.snd)


#eval unzip [(1,"ok"),(2,"nada"),(3,"check")]


/- Note that the `let` requires the `rec` to indicate -/
/- that it is a recursive function. This is not necessary -/
/- with `def` -/
def reverse (xs : List α) : List α :=
  let rec helper : List α → List α  → List α
    | [], soFar => soFar
    | y :: ys, soFar => helper ys (y :: soFar)
  helper xs []

#eval reverse [1,2,3]
