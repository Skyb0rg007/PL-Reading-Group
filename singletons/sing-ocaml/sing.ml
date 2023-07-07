
module type Sing = sig
  type 'a sing
  type demote

  type somesing = SomeSing : 'a sing -> somesing

  val fromSing : 'a sing -> demote
  val toSing : demote -> somesing
end

module Bool : Sing with type demote = bool = struct
  type ktrue = KTrue
  type kfalse = KFalse

  type 'a sing =
    | STrue : ktrue sing
    | SFalse : kfalse sing

  type somesing = SomeSing : 'a sing -> somesing

  type demote = bool

  let fromSing (type b) : b sing -> demote = function
    | STrue -> true
    | SFalse -> false

  let toSing = function
    | true -> SomeSing STrue
    | false -> SomeSing SFalse
end

module Nat = struct
  type kzero = KZero
  type 'a ksucc = KSucc

  type _ sing =
    | SZero : kzero sing
    | SSucc : 'a sing -> ('a ksucc) sing

  type somesing = SomeSing : 'a sing -> somesing

  type demote = int

  let rec fromSing : type a. a sing -> demote = function
    | SZero -> 0
    | SSucc n -> fromSing n + 1

  let rec toSing = function
    | 0 -> SomeSing SZero
    | n -> match toSing (n - 1) with SomeSing x -> SomeSing (SSucc x)
end

module Vec = struct

  type (_, _) vec =
    | Nil : ('a, Nat.kzero) vec
    | Cons : 'a * ('a, 'n) vec -> ('a, 'n Nat.ksucc) vec

  type 'a somevec = SomeVec : ('a, 'n) vec -> 'a somevec

  let head = function
    | Cons (x, _) -> x

  let tail = function
    | Cons (_, xs) -> xs


  type (_, _, _) add =
    | AddZero : (Nat.kzero, 'b, 'b) add
    | AddSucc : ('a, 'b, 'c) add -> ('a Nat.ksucc, 'b, 'c Nat.ksucc) add

  let rec append : type n m k. ('a, n) vec -> ('a, m) vec -> (n, m, k) add -> ('a, k) vec =
    fun xs ys add ->
      match xs, add with
      | Nil, AddZero -> ys
      | Cons (x, xs), AddSucc add -> Cons (x, append xs ys add)
      | _, _ -> .

  let map f =
    let rec loop : type n. ('a, n) vec -> ('b, n) vec = function
      | Nil -> Nil
      | Cons (x, xs) -> Cons (f x, loop xs)
    in loop

  let filter f =
    let rec loop : type n. ('a, n) vec -> 'a somevec = function
      | Nil -> SomeVec Nil
      | Cons (x, xs) when not (f x) -> loop xs
      | Cons (x, xs) -> let SomeVec ys = loop xs in SomeVec (Cons (x, ys))
    in loop

end

