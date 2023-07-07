
Require Import Coq.Bool.Bool.
Require Import Coq.Lists.List.
Require Import Coq.Program.Tactics.
Require Import Program.

Inductive nat : Type :=
| O : nat
| S : nat -> nat.

Inductive vec' A : nat -> Type :=
| nil : vec' A O
| cons : forall {n}, A -> vec' A n -> vec' A (S n).

Arguments nil {_} : assert.
Arguments cons {_ _} _ _ : assert.

Definition vec A n := vec' n A.

Fail Definition ill_formed := vec bool bool.


Fixpoint plus n m :=
  match n with
  | O => m
  | S n => S (plus n m)
  end.

(* Fixpoint append {A n m} (xs : vec n A) (ys : vec m A) : vec (plus n m) A := *)
(*   match xs with *)
(*   | nil => ys *)
(*   | cons x xs => cons x (append xs ys) *)
(*   end. *)



Fixpoint lt (n : nat) (m : nat) : bool :=
  match n, m with
  | _, O => false
  | O, S _ => true
  | S n, S m => lt n m
  end.

Lemma lt_zero n : lt n O = false.
Proof.
  induction n; easy.
Qed.

Lemma lt_pred n m : lt (S n) (S m) = lt n m.
Proof.
  easy.
Qed.

Definition nth {A n} (xs : vec n A) (i : nat) (H : lt i n = true) : A.
Proof.
  generalize dependent i.
  induction xs; intros i H.
  - exfalso.
    apply diff_true_false.
    destruct i; exact (eq_sym H).
  - destruct i.
    + exact a.
    + exact (IHxs i H).
Defined.

Definition one := S O.
Definition two := S one.
Definition three := S two.
Compute (nth (cons three (cons one (cons two (cons three nil)))) O eq_refl).




Fixpoint nth {A n} (xs : vec n A) (i : nat) (H : lt i n = true) : A :=
  match xs, i with
  | cons x _, O => x
  | cons _ xs, S i => nth xs i _
  end.



