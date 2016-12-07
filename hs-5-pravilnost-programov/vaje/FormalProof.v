(* We demonstrate how to prove the exercises in ekvivalence.pdf in the Coq
   proof assistant. An online version of Coq is available at
     https://x80.org/rhino-coq/
*)


Parameter A : Type.

Inductive list :=
| nil
| cons (a : A) (l : list).

Notation "x :: l" := (cons x l) (at level 60, right associativity).
Notation "[ ]" := nil.
Notation "[ x ; .. ; y ]" := (cons x .. (cons y nil) ..).

Definition ind_list :
  forall (P : list -> Type),
    P [] ->
    (forall (a : A) (l : list), P l -> P (cons a l)) ->
    forall l : list, P l.
  exact list_rect.
Defined.

Fixpoint append (l1 l2 : list) : list :=
  match l1 with
  | nil    => l2
  | h :: t => h :: (append t l2)
  end.

Notation "x ++ y" := (append x y) (right associativity, at level 60).

Fixpoint rev (l : list) : list :=
  match l with
  | nil => nil
  | h :: t => (rev t) ++ [h]
  end.

Fixpoint length (l : list) : nat :=
  match l with nil => 0 | _ :: t => 1 + (length t) end.

Proposition trd_1 : forall (x : A), rev [x] = [x].
  intros x.
  unfold rev.
  unfold append.
  reflexivity.
Qed.

Proposition trd_2 : forall (xs ys : list),
    length (xs ++ ys) = (length xs) + (length ys).
  intros xs. induction xs; intros.
  - cbn. reflexivity.
  - cbn. rewrite IHxs. reflexivity.
Qed.

Proposition trd_3 : forall (xs : list), xs ++ [] = xs.
  intros. induction xs.
  - unfold append. reflexivity.
  - cbn. rewrite IHxs. reflexivity.
Qed.

Proposition trd_4 : forall xs ys zs, xs ++ (ys ++ zs) = (xs ++ ys) ++ zs.
  intros. induction xs.
  - cbn. reflexivity.
  - cbn. rewrite IHxs. reflexivity.
Qed.

Proposition trd_5 : forall xs ys, rev (xs ++ ys) = rev ys ++ rev xs.
  intros. induction xs.
  - cbn. rewrite trd_3.
    reflexivity.
  - cbn.
    rewrite IHxs.
    rewrite trd_4.
    reflexivity.
Qed.

Proposition trd_6 : forall xs, rev (rev xs) = xs.
  intros. induction xs.
  - reflexivity.
  - cbn.
    rewrite trd_5.
    rewrite IHxs.
    reflexivity.
Qed.

Proposition trd_7 : forall xs, length (rev xs) = length xs.
  intros. induction xs.
  - reflexivity.
  - cbn.
    rewrite trd_2.
    rewrite IHxs. cbn.
    rewrite <- plus_n_Sm.
    rewrite <- plus_n_O.
    reflexivity.
Qed.
