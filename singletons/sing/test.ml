


type zero = Zero
type 'a succ = Succ

type (_, _) vec =
    Nil  : (zero, 'a) vec
  | Cons : 'a * ('n, 'a) vec -> ('n succ, 'a) vec

let v = Cons (1, Cons (2, Nil))
(* val v : (zero succ succ, int) vec *)


(* OK! *)
type ill_formed = (bool, bool) vec




