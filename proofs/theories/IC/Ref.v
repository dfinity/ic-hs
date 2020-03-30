Require Import Control.Monad.Trans.State.Lazy.
Require Import Control.Monad.Trans.Except.
Require Import Data.Functor.Identity.

Require Import IC.Ref.
Require Import IC.Types.
Require Import Data.Map.Internal.

Import GHC.Base.
Import Data.Either.
Import GHC.Base.Notations.
Import List.Notations.

Set Bullet Behavior "Strict Subproofs".

Ltac expand_pairs :=
  match goal with
    |- context[let (_,_) := ?e in _] =>
    rewrite (surjective_pairing e)
  end.

Ltac destruct_match :=
  match goal with
  | [ H :context[match ?a with _ => _ end] |- _] =>
    let Heq := fresh "Heq" in
    destruct a eqn:Heq
  | [ |- context[match ?a with _ => _ end]] =>
    let Heq := fresh "Heq" in
    destruct a eqn:Heq
  end.

(** This tactic finds a [let x := rhs in body] anywhere in the goal,
    and moves it into the context, without zeta-reducing it. *)
Ltac find_let e k :=
  lazymatch e with 
  (** LY: It seems that there is a bug in Coq about variable binding.
      This is a temporary fix. *)
  | let _go := ?rhs in ?body => k _go rhs body
  | ?e1 ?e2 =>
    find_let e1 ltac:(fun x rhs body => k x rhs uconstr:(body e2)) ||
    find_let e2 ltac:(fun x rhs body => k x rhs uconstr:(e1 body))
  | _ => fail
  end.
Ltac float_let :=
  lazymatch goal with |- ?goal =>
    find_let goal ltac:(fun x rhs body =>
      let goal' := uconstr:(let x := rhs in let _go := x in body) in
      (change goal'; do 2 intro; subst) || fail 1000 "Failed to change to" goal'
    )
  end.

(* NB, this does not work:
Ltac float_let :=
  lazymatch goal with |-  context C [let x := ?rhs in ?body] =>
    let goal' := context C[body] in
    change (let x := rhs in goal'); intro
  end.
*)

(** Common subexpression elimination *)
Ltac cse_let :=
      repeat lazymatch goal with
        [ x := ?rhs, x0 := ?rhs |- _ ] =>
          change x0 with x in *;clear x0
        end.


(* Lemmas about State *)

Set Warnings "-masking-absolute-name".
Require Import Proofs.GHC.Base.
Require Import Proofs.Data.Functor.Identity.
Require Import Coq.Logic.FunctionalExtensionality.


Instance instance_FunctorLaws_StateT {S} {m} `{FunctorLaws m} : FunctorLaws (StateT S m).
Proof.
 constructor.
 * intros.
   destruct x as [x].
   unfold fmap, Functor__StateT, fmap__, Lazy.Functor__StateT_fmap.
   f_equal. apply functional_extensionality. intro s.
   simpl.
   rewrite <- functor_identity.
   f_equal. apply functional_extensionality. destruct x0. reflexivity.
 * intros.
   destruct x as [x]. 
   unfold fmap, Functor__StateT, fmap__, Lazy.Functor__StateT_fmap.
   f_equal. apply functional_extensionality. intro s.
   simpl.
   rewrite functor_composition.
   f_equal. apply functional_extensionality. destruct x0. reflexivity.
Qed.

Instance instance_ApplicativeLaws_StateT
  {S} {m} {H : Functor m} {H2 : @Applicative m H} {H3 : @FunctorLaws m H} {H4 : @ApplicativeLaws m H H2 H3} {H5 : Monad m}:
  @ApplicativeLaws (StateT S m) (@Functor__StateT m S H) (@Applicative__StateT m S H H H2 H5) instance_FunctorLaws_StateT.
Admitted.

Instance instance_MonadLaws_StateT
  {S} {m}
  {H1 : Functor m}
  {H2 : @Applicative m H1}
  {H5 : Monad m}
  {L1 : @FunctorLaws m H1}
  {L2 : @ApplicativeLaws m H1 H2 L1}
  {L3 : @MonadLaws m H1 H2 H5 L1 L2}:
  MonadLaws (StateT S m).
Admitted.

Instance instance_FunctorLaws_ExceptT {E} {m} `{FunctorLaws m} : FunctorLaws (ExceptT E m).
Admitted.

Instance instance_ApplicativeLaws_ExceptT
  {S} {m}
  {H1 : Functor m}
  {H2 : @Applicative m H1}
  {H5 : Monad m}
  {L1 : @FunctorLaws m H1}
  {L2 : @ApplicativeLaws m H1 H2 L1}
  {L3 : @MonadLaws m H1 H2 H5 L1 L2}:
  ApplicativeLaws (ExceptT S m).
Admitted.

Instance instance_MonadLaws_ExceptT
  {E} {m}
  {H1 : Functor m}
  {H2 : @Applicative m H1}
  {H5 : Monad m}
  {L1 : @FunctorLaws m H1}
  {L2 : @ApplicativeLaws m H1 H2 L1}
  {L3 : @MonadLaws m H1 H2 H5 L1 L2}:
  MonadLaws (ExceptT E m).
Admitted.

Lemma pure_then:
  forall M `{MonadLaws M} A B (x : A) (act : M B),
   (pure x >> act) = act.
Proof.
  intros.
  rewrite monad_applicative_pure. 
  rewrite monad_then.
  rewrite monad_left_id.
  reflexivity.
Qed.

Lemma fmap_to_bind:
  forall M `{MonadLaws M} A B (f : A -> B) (act : M A),
   fmap f act = (act >>= (fun x => return_ (f x))).
Proof.
  intros.
  rewrite applicative_fmap.
  rewrite monad_applicative_pure.
  rewrite monad_applicative_ap.
  unfold ap.
  rewrite monad_left_id.
  reflexivity.
Qed.


Lemma fmap_bind:
  forall M `{MonadLaws M} A B C (f : B -> C) (act1 : M A) (act2 : A -> M B),
   fmap f (act1 >>= act2) = (act1 >>= (fun x => fmap f (act2 x))).
Proof.
  intros.
  (* WTF? *)
  erewrite fmap_to_bind; try typeclasses eauto.
  rewrite <- monad_composition.
  f_equal.
  apply functional_extensionality.
  intros.
  erewrite fmap_to_bind; try typeclasses eauto.
  reflexivity.
Qed.

Lemma fmap_bind_RM:
  forall A B C (f : B -> C) (act1 : RM A) (act2 : A -> RM B),
    fmap f (act1 >>= act2) = (act1 >>= (fun x => fmap f (act2 x))).
Proof.
  intros.
  eapply fmap_bind.
  typeclasses eauto.
  (* Why doesn't type class resolution work here without help.? *)
Qed.


Lemma runState_state:
  forall A B (f : A -> (B * A)) s, runState (state f) s = f s.
Proof. intros. reflexivity. Qed.

Lemma runState_return:
  forall S A (x : A) (s : S), runState (return_ x) s = (x,s).
Proof. intros. reflexivity. Qed.


Lemma runState_fmap:
  forall S A B(f : A -> B) (m : State S A) (s : S),
  runState (fmap f m) s = 
    let (x, s') := runState m s in
    (f x, s').
Proof. intros.
  destruct m as [m].
  unfold runState, Base.op_z2218U__, runIdentity, runStateT.
  unfold fmap, Functor__StateT, fmap__, Lazy.Functor__StateT_fmap.
  unfold fmap, Functor__Identity, fmap__, Identity.Functor__Identity_fmap.
  unfold runStateT.
  destruct (m s) as [ms].
  reflexivity.
Qed.

Lemma runState_bind:
  forall S A B (m1 : State S A) (m2 : A -> State S B) (s : S),
  runState (m1 >>= m2) s = 
    let (x, s') := runState m1 s in
    runState (m2 x) s'.
Proof. intros.
  destruct m1 as [m1].
  repeat unfold runState, Base.op_z2218U__, runIdentity, runStateT,
    op_zgzgze__, Monad__StateT, op_zgzgze____, Lazy.Monad__StateT_op_zgzgze__,
    op_zgzgze__, Monad__Identity, op_zgzgze____, Identity.Monad__Identity_op_zgzgze__.
  destruct (m1 s) as [ms].
  destruct ms.
  reflexivity.
Qed.

Lemma runState_then:
  forall S A B (m1 : State S A) (m2 : State S B) (s : S),
  runState (m1 >> m2) s = 
    let s' := snd (runState m1 s) in
    runState m2 s'.
Proof.
 intros.
  destruct m1 as [m1].
  repeat unfold runState, Base.op_z2218U__, runIdentity, runStateT,
    op_zgzg__, Monad__StateT, op_zgzg____, Lazy.Monad__StateT_op_zgzg__,
    Lazy.Monad__StateT_op_zgzgze__,
    op_zgzg__, Monad__Identity, op_zgzg____, Identity.Monad__Identity_op_zgzg__.
  destruct (m1 s) as [ms].
  destruct ms.
  reflexivity.
Qed.


Lemma runState_gets:
  forall A B C (f : A -> B) (m : B -> StateT _ Identity C) (s : A),
  runState (gets f >>= m) s = runState (m (f s)) s.
Proof. intros. reflexivity. Qed.

Lemma state_extensionality:
  forall S A (m1 m2 : StateT S Identity A),
  (forall s, runState m1 s = runState m2 s) -> m1 = m2.
Proof.
  intros.
  destruct m1 as [m1], m2 as [m2]. 
  f_equal. apply functional_extensionality. intro s.
  specialize (H s).
  unfold runState, Base.op_z2218U__, runIdentity, runStateT in H. 
  destruct (m1 s), (m2 s).
  congruence.
Qed.


Definition SP {a} {s} (P : s -> Prop) (Q : a -> s -> Prop) (act : State s a) :=
  forall s, P s -> Q (fst (runState act s)) (snd (runState act s)).


Lemma SP_return:
  forall {a s} (P : s -> Prop) (Q : a -> s -> Prop) (x : a),
  (forall s, P s -> Q x s) -> SP P Q (return_ x).
Proof. intros. intros s0 HP. apply H. apply HP. Qed.


Lemma SP_put:
  forall {s} (P : s -> Prop) (Q : unit -> s -> Prop) (x : s),
  Q tt x ->  SP P Q (put x).
Proof. intros. intros s0 _. apply H. Qed.

Lemma SP_get:
  forall {s} (P : s -> Prop) (Q : s -> s -> Prop),
  (forall s, P s -> Q s s) ->
  SP P Q get.
Proof. intros. intros s0 H2. apply H. apply H2. Qed.

Lemma SP_gets:
  forall {s a} (P : s -> Prop) (Q : a -> s -> Prop) (f : s -> a),
  (forall s, P s -> Q (f s) s) ->
  SP P Q (gets f).
Proof. intros. intros s0 H1. apply H. apply H1. Qed.

Lemma SP_state:
  forall {S A} (P : S -> Prop) (Q : A -> S -> Prop) (f : S -> A * S),
  (forall s, P s -> Q (fst (f s)) (snd (f s))) ->
  SP P Q (state f).
Proof. intros. intros s0 H1. apply H. apply H1. Qed.


Lemma SP_modify:
  forall {S} (P : S -> Prop) (Q : unit -> S -> Prop) (f : S -> S),
  (forall s, P s -> Q tt (f s)) ->
  SP P Q (modify f).
Proof. intros. intros s0 H1. apply H. apply H1. Qed.

Lemma SP_bind:
  forall {a b s} P Q R (act1 : State s a) (act2 : a -> State s b),
  SP P Q act1 ->
  (forall x, SP (Q x) R (act2 x)) ->
  SP P R (act1 >>= act2).
Proof.
  intros.
  intros ??.
  rewrite runState_bind. expand_pairs.
  apply H0.
  apply H.
  apply H1.
Qed.

Lemma SP_fmap:
  forall {a b s} P Q (f : a -> b) (act : State s a),
  SP P (fun x => Q (f x)) act ->
  SP P Q (fmap f act).
Proof.
  intros.
  intros s0 H1.
  rewrite runState_fmap.
  expand_pairs. simpl.
  apply H. assumption.
Qed.

Definition StateInvariant {a} {s} (P : s -> Prop) (act : State s a) :=
  SP P (fun _ => P) act.

Lemma StateInvariant_return:
  forall {a s} (P : s -> Prop) (x : a),
  StateInvariant P (return_ x).
Proof. intros. apply SP_return. trivial. Qed.

Lemma StateInvariant_get:
  forall {s} (P : s -> Prop),
  StateInvariant P get.
Proof. intros. eapply SP_get. intros. assumption. Qed.


Lemma StateInvariant_gets:
  forall {b s} (P : s -> Prop) (f : s -> b),
  StateInvariant P (gets f).
Proof. intros. apply SP_gets. apply StateInvariant_get. Qed.


Lemma StateInvariant_bind:
  forall {a b s} P (act1 : State s a) (act2 : a -> State s b),
  StateInvariant P act1 ->
  (forall x, StateInvariant P (act2 x)) ->
  StateInvariant P (act1 >>= act2).
Proof.
  intros. eapply SP_bind.
  * apply H.
  * intros ?.  apply H0.
Qed.

Lemma StateInvariant_then:
  forall {a b s} P (act1 : State s a) (act2 : State s b),
  StateInvariant P act1 ->
  StateInvariant P act2 ->
  StateInvariant P (act1 >> act2).
Proof.
  intros.
  rewrite monad_then.
  apply StateInvariant_bind; tauto.
Qed.

Lemma StateInvariant_bind_return: (*  acommon pattern *)
  forall {a b s} P (act1 : State s a) (f : a -> b),
  StateInvariant P act1 ->
  StateInvariant P (act1 >>= (fun x => return_ (f x))).
Proof.
  intros.
  apply StateInvariant_bind.
  * apply H.
  * intro. apply StateInvariant_return.
Qed.

Lemma StateInvariant_fmap:
  forall {a b s} P (f : a -> b) (act : State s a),
  StateInvariant P act ->
  StateInvariant P (fmap f act).
Proof.
  intros.
  apply SP_fmap.
  apply H.
Qed.


Lemma StateInvariant_liftA2:
  forall {a b c s} P (f : a -> b -> c) (act1 : State s a) (act2 : State s b),
  StateInvariant P act1 ->
  StateInvariant P act2 ->
  StateInvariant P (liftA2 f act1 act2).
Proof.
  intros.
  rewrite applicative_liftA2.
  rewrite monad_applicative_ap.
  unfold ap.
  apply StateInvariant_bind; [|intros; apply StateInvariant_bind; intros].
  apply StateInvariant_fmap; assumption.
  assumption.
  apply StateInvariant_return.
Qed.

Lemma StateInvariant_mapM:
  forall {a b s} P (act : a -> State s b) (xs : list a),
  (forall x, In x xs -> StateInvariant P (act x)) ->
  StateInvariant P (Traversable.mapM act xs).
Proof.
  intros ?????? Hact.
  unfold Traversable.mapM, Traversable.Traversable__list, Traversable.mapM__,
         Traversable.Traversable__list_mapM, Traversable.Traversable__list_traverse.
  induction xs.
  * apply StateInvariant_return.
  * simpl.
    apply StateInvariant_liftA2.
    - apply Hact. left. reflexivity.
    - apply IHxs. intros x Hin. apply Hact. right. assumption.
Qed.

Lemma StateInvariant_forM:
  forall {a b s} P (act : a -> State s b) (xs : list a),
  (forall x, In x xs -> StateInvariant P (act x)) ->
  StateInvariant P (Traversable.forM xs act).
Proof.
  intros.
  unfold Traversable.forM, flip.
  apply StateInvariant_mapM.
  assumption.
Qed.

Lemma runExceptT_return:
  forall A E (x : A),
  runExceptT (e := E) (m := M) (return_ x) = return_ (Right x).
Proof. intros. reflexivity. Qed.

Lemma runExceptT_liftRM_bind:
  forall A B (g : M A) (h : A -> RM B),
  runExceptT (liftRM g >>= h) = (g >>= (fun x => runExceptT (h x))).
Proof.
  intros.
  unfold liftRM, Class.lift, MonadTrans__ExceptT, Class.lift__,
     Except.MonadTrans__ExceptT_lift, Base.op_z2218U__,
     liftM.
  simpl.
  rewrite <- monad_composition.
  f_equal. apply functional_extensionality. intro.
  rewrite monad_left_id.
  reflexivity.
Qed.


Lemma onReject_return:
  forall A f (x : A),
  onReject f (return_ x) = return_ x.
Proof. intros. apply state_extensionality. intro. reflexivity. Qed. 



Lemma onReject_liftRM_bind:
  forall A B f (g : M A) (h : A -> RM B),
  onReject f (liftRM g >>= h) = (g >>= (fun x => onReject f (h x))).
Proof.
  intros.
  unfold onReject.
  rewrite runExceptT_liftRM_bind.
  rewrite <- monad_composition.
  reflexivity.
Qed.

Lemma onReject_liftRM:
  forall A f (g : M A),
  onReject f (liftRM g) = g.
Proof.
  intros.
  rewrite <- monad_right_id with (m := liftRM g).
  rewrite <- monad_right_id with (m := g) at 2.
  rewrite onReject_liftRM_bind.
  reflexivity.
Qed.


Lemma onReject_liftRM_then:
  forall A B f (g : M A) (h : RM B),
  onReject f (liftRM g >> h) = (g >> onReject f h).
Proof.
  intros.
  rewrite 2 monad_then.
  apply onReject_liftRM_bind.
Qed.

Lemma throwE_bind:
  forall M E A B (e : E) (act : A -> ExceptT E M B) `{MLM : MonadLaws M},
  (throwE e >>= act) = throwE e.
Proof.
  intros.
  unfold throwE, Base.op_z2218U__.
  unfold op_zgzgze__, Monad__ExceptT, op_zgzgze____, Monad__ExceptT_op_zgzgze__.
  simpl.
  rewrite monad_left_id.
  reflexivity.
Qed.

Lemma throwE_bind':
  forall A B e (act : A -> RM B),
  (throwE e >>= act) = throwE e.
Proof.
  intros.
  (* Why doesn't type class resolution work here without help.? *)
  apply throwE_bind with (ApplicativeLaws0 := instance_ApplicativeLaws_StateT).
  typeclasses eauto.
Qed.

Lemma onReject_throwE:
  forall A B f x (h : RM A),
  (onReject f ((throwE x : RM B) >> h)) = f x.
Proof. intros.  apply state_extensionality. intro. reflexivity. Qed.

Definition PRight {A B S} P (s : S) (x : Either A B) := match x with
  | Left e => True
  | Right y => P y s
  end.

Definition ESP {S A E} (P : S -> Prop) (R : A -> S -> Prop) (act : ExceptT E (State S) A) :=
  SP P (fun x s => PRight R s x /\ P s) (runExceptT act).

Lemma ESP_return:
  forall {s e a} (P : s -> Prop) (R : a -> s -> Prop) (x : a),
  (forall s, P s -> R x s) ->
  (ESP (E := e)) P R (return_ x).
Proof. intros. apply SP_return. intros. simpl. auto. Qed.

Lemma ESP_bind:
  forall {s e a b} (P : s -> Prop) Q (R : b -> s -> Prop)
  (act1 : ExceptT e (State s) a) act2,
  ESP P Q act1 ->
  (forall (x : a), ESP (fun s => Q x s /\ P s) R (act2 x)) ->
  ESP P R (act1 >>= act2).
Proof.
  intros.
  unfold ESP in *.
  destruct act1 as [act1].
  unfold runExceptT in *.
  simpl in *.
  eapply SP_bind.
  * apply H.
  * simpl. intros.
    destruct x.
    - apply SP_return. trivial.
    - specialize (H0 a0).
      unfold runExceptT.
      simpl in *.
      intros ??.
      specialize (H0 s0 H1).
      simpl in *.
      tauto.
Qed.

Lemma ESP_bind_any:
  forall {s e a b} (P : s -> Prop) (R : b -> s -> Prop)
  (act1 : ExceptT e (State s) a) act2,
  ESP P (fun _ _ => True) act1 ->
  (forall (x : a), ESP P R (act2 x)) ->
  ESP P R (act1 >>= act2).
Proof. intros. eapply ESP_bind. apply H.
  intros.
  intros ??.
  destruct H1.
  specialize (H0 x s0 H2).
  simpl in *.
  tauto.
Qed.

Lemma ESP_then:
  forall {s e a b} (P : s -> Prop) (R : b -> s -> Prop)
  (act1 : ExceptT e (State s) a) act2,
  ESP P (fun _ _ => True) act1 ->
  ESP P R act2 ->
  ESP P R (act1 >> act2).
Proof.
  intros.
  rewrite monad_then.
  apply ESP_bind_any.
  apply H.
  intros.
  apply H0.
Qed.

Lemma ESP_fmap:
  forall {e a b s} P R (f : a -> b) (act : ExceptT e (State s) a),
  ESP P (fun x => R (f x)) act ->
  ESP P R (fmap f act).
Proof.
  intros.
  erewrite fmap_to_bind; try typeclasses eauto.
  eapply ESP_bind.
  apply H.
  intros.
  apply ESP_return.
  intros.
  apply H0.
Qed.


Lemma ESP_throwE:
  forall {s e a} (P : s -> Prop) (R : a -> s -> Prop) (x : e),
  ESP P R (throwE x).
Proof. intros. apply SP_return. intros. split. constructor. assumption. Qed.


Lemma ESP_liftRM:
  forall {A P R} (act : M A),
  SP P (fun x s => R x s /\ P s) act ->
  ESP P R (liftRM act).
Proof.
  intros.
  unfold ESP.
  unfold liftRM, Class.lift, MonadTrans__ExceptT, Class.lift__, liftM,
    Except.MonadTrans__ExceptT_lift, liftM.
  simpl.
  eapply SP_bind.
  * apply H.
  * intros. apply SP_return. intros. apply H0.
Qed.

Lemma SP_onReject:
  forall {A} P R
  (act : RM A) f,
  (forall e, SP P (fun x s => R x s /\ P s) (f e)) ->
  ESP P R act ->
  SP P (fun x s => R x s /\ P s) (onReject f act).
Proof.
  intros.
  unfold onReject.
  eapply SP_bind.
  * apply H0.
  * intros.
    simpl in *.
    destruct x.
    - intros ??. apply H. apply H1.
    - apply SP_return. intros. assumption.
Qed.


Lemma StateInvariant_onReject:
  forall {A} P
  (act : RM A) f,
  (forall e, StateInvariant P (f e)) ->
  ESP P (fun _ _ => True) act ->
  StateInvariant P (onReject f act).
Proof.
  intros.
  intros.
  unfold onReject.
  eapply SP_bind.
  * apply H0.
  * intros.
    simpl in *.
    destruct x.
    - intros ??. apply H. apply H1.
    - apply SP_return. intros. apply H1.
Qed.

Definition ICInvariant Q P :=
  P initialIC /\
  (forall rid pk r s0, 
    P s0 ->
    Q pk r ->
    evalState (authAsyncRequest pk r) s0 = true ->
    P (execState (submitRequest rid r) s0)) /\
  StateInvariant P runStep.


Definition any_request (pk : Blob) (r : AsyncRequest) := True.

Lemma StateInvariant_getCallContext:
  forall P c, StateInvariant P (getCallContext c).
Proof.
  intros.
  unfold getCallContext.
  apply StateInvariant_gets.
Qed.

Lemma StateInvariant_calleeOfCallID:
  forall P c, StateInvariant P (calleeOfCallID c).
Proof.
   unfold calleeOfCallID, Functor.op_zlzdzg__.
   intros.
   apply StateInvariant_fmap.
   apply StateInvariant_getCallContext.
Qed.


Theorem no_empty_canister_id :
  ICInvariant any_request (fun s =>
    member (Mk_EntityId nil) (canisters s) = false).
Proof.
  set (P := fun s : IC => member (Mk_EntityId nil) (canisters s) = false).
  split; [|split].
  * reflexivity.
  * intros.
    unfold submitRequest.
    unfold execState.
    unfold modify.
    rewrite runState_state.
    unfold snd.
    destruct_match.
    + assumption.
    + destruct s0. assumption.
  * intros.
    unfold runStep.
    repeat (apply StateInvariant_bind; [|intros; destruct x as [[rid ar]|]]).
    + unfold nextReceived.
      apply StateInvariant_gets.
      (* Somehow pass information about ar down? *)
    + apply StateInvariant_bind; [|intros; apply StateInvariant_return].
      unfold processRequest.
      unfold op_zezlzl__.
      apply StateInvariant_bind; [|intros; apply SP_modify; destruct s; simpl; auto].
      destruct_match.
      - (* Create request *)
        apply StateInvariant_onReject.
        intro; apply SP_return; trivial.
        set (R' := fun new_id (s : IC) => not (new_id = Mk_EntityId nil)).
        apply ESP_bind with (Q := R').
        ** destruct_match.
           -- (* System picks *)
              apply ESP_liftRM.
              apply SP_gets.
              intros.
              subst R'; unfold op_z2218U__; simpl.
              admit. (* fresh id is not nil *)
           -- (* Forced choice *)
              (* TODO: Rule out *)
              admit.
           -- unfold Monad.unless. destruct_match.
              ++ rewrite monad_then.
                 rewrite monad_applicative_pure.
                 rewrite monad_left_id.
                 apply ESP_return.
                 subst R'; unfold op_z2218U__; simpl.
                 admit. (* derived id is not null *)
              ++ unfold reject.
                 rewrite monad_then.
                 rewrite throwE_bind'.
                 apply ESP_throwE.
        ** intros.
           apply ESP_bind_any.
           -- apply ESP_liftRM.
              apply SP_gets.
              intros. split; trivial.
           -- intros.
              apply ESP_then; [|apply ESP_then].
              ++ unfold when.
                 destruct_match.
                 *** unfold reject. apply ESP_throwE.
                 *** rewrite monad_applicative_pure. apply ESP_return. trivial.
              ++ apply ESP_liftRM.
                 unfold createEmptyCanister.
                 apply SP_modify. intros.
                 split; trivial.
                 destruct s; subst P R'; simpl in *.
                 split.
                 tauto.
                 (* map proofs *)
                 admit.
              ++ apply ESP_return; trivial.
      - (* install request *)
        apply StateInvariant_onReject.
        intro; apply SP_return; trivial.
        apply ESP_bind_any.
        { unfold onErr.
           apply ESP_bind_any.
           apply ESP_return; trivial.
           intros. destruct x; simpl.
           unfold reject.
           apply ESP_throwE.
           apply ESP_return; trivial.
        }
        intros.
        apply ESP_bind with (Q := fun x s => member c (canisters s) = true).
        { unfold Functor.op_zlzdzg__.
          unfold getCanisterState, orElse.
          rewrite fmap_bind_RM.
          apply ESP_bind with (Q := fun x s => member c (canisters s) = Maybe.isJust x).
          { apply SP_gets. intros. unfold Base.op_z2218U__.
            split; try assumption. simpl.
            admit. (*  stuff about member and lookup *)
          }
          intros.
          apply ESP_fmap.
          destruct x0; simpl.
          * apply ESP_return.
            intros. tauto.
          * unfold reject. unfold Base.op_z2218U__.
            apply ESP_throwE.
        }
        intros.
        apply ESP_then.
        { unfold when. destruct_match.
          * unfold reject. apply ESP_throwE.
          * rewrite monad_applicative_pure. apply ESP_return. trivial.
        }
        apply ESP_then.
        { unfold when. destruct_match.
          * unfold reject. apply ESP_throwE.
          * rewrite monad_applicative_pure. apply ESP_return. trivial.
        }
        apply ESP_bind_any.
        { unfold onTrap.
          apply ESP_bind_any.
          { apply ESP_return; trivial. }
          intros.
          destruct_match.
          * unfold reject. apply ESP_throwE.
          * apply ESP_return; trivial.
        }
        intros.
        apply ESP_then.
        { apply ESP_liftRM.
          unfold insertCanister.
          apply SP_modify.
          intros.
          destruct s; subst P; simpl in *.
          split; try auto.
          destruct H.
          split.
          * (* it's an artifact of how ESP is setup that we have to prove this *)
             admit.
          * (* map stuff *)
            admit.
        }
        apply ESP_return; trivial.
      - (* upgrade request *)
        apply StateInvariant_onReject.
        intro; apply SP_return; trivial.
        apply ESP_bind_any.
        { unfold onErr.
           apply ESP_bind_any.
           apply ESP_return; trivial.
           intros. destruct x; simpl.
           unfold reject.
           apply ESP_throwE.
           apply ESP_return; trivial.
        }
        intros.
        apply ESP_bind with (Q := fun x s => member c (canisters s) = true).
        { unfold Functor.op_zlzdzg__.
          unfold getNonemptyCanisterState, getCanisterState, orElse.
          rewrite <- monad_composition.
          apply ESP_bind with (Q := fun x s => member c (canisters s) = Maybe.isJust x).
          { apply SP_gets. intros. unfold Base.op_z2218U__.
            split; try assumption. simpl.
            admit. (*  stuff about member and lookup *)
          }
          intros.
          destruct x0; simpl.
          * rewrite monad_left_id.
            destruct o; simpl.
            + apply ESP_return. intros. tauto.
            + unfold reject. apply ESP_throwE.
          * unfold reject. apply ESP_throwE.
        }
        intros. destruct x0.
        apply ESP_bind_any.
        { unfold onTrap.
          apply ESP_bind_any.
          { apply ESP_return; trivial. }
          intros.
          destruct_match.
          * unfold reject. apply ESP_throwE.
          * apply ESP_return; trivial.
        }
        intros.
        apply ESP_bind_any.
        { unfold onTrap.
          apply ESP_bind_any.
          { apply ESP_return; trivial. }
          intros.
          destruct_match.
          * unfold reject. apply ESP_throwE.
          * apply ESP_return; trivial.
        }
        intros.
        apply ESP_then.
        { apply ESP_liftRM.
          unfold insertCanister.
          apply SP_modify.
          intros.
          destruct s; subst P; simpl in *.
          split; try auto.
          destruct H.
          split.
          * (* it's an artifact of how ESP is setup that we have to prove this *)
             admit.
          * (* map stuff *)
            admit.
        }
        apply ESP_return; trivial.
     - (* update call request *) 
        rewrite onReject_liftRM_bind.
        apply StateInvariant_bind.
        { unfold newCallContext.
          apply SP_state.
          intros. destruct s; subst P; simpl.
          apply H.
        }
        intros.
        rewrite onReject_liftRM_then.
        apply StateInvariant_then.
        { unfold enqueueMessage.
          apply SP_modify.
          intros. destruct s; subst P; simpl.
          apply H.
        }
        unfold onReject.
        rewrite runExceptT_return.
        rewrite monad_left_id.
        apply StateInvariant_return.
    + (* pop message *)
      apply StateInvariant_bind.
      {
        unfold popMessage.
        apply SP_state.
        intros. destruct s; destruct_match; subst P; simpl; auto.
      }
      intros.
      destruct_match.
      - (* process message *)
        unfold processMessage.
        apply StateInvariant_then; [|apply StateInvariant_return].
        destruct_match.
        ** rewrite onReject_liftRM_bind.
           apply StateInvariant_bind.
           { apply StateInvariant_calleeOfCallID.
           }
           intros.
           unfold getNonemptyCanisterState, getCanisterState, orElse.
           repeat rewrite <- monad_composition.
           rewrite onReject_liftRM_bind.
           apply StateInvariant_bind.
           { apply StateInvariant_gets.
           }
           intros.
           unfold Maybe.maybe.
           destruct_match.
           ++ rewrite monad_left_id.
              destruct_match.
              -- rewrite monad_left_id.
                 rewrite onReject_liftRM_bind.
                 unfold invokeEntry. destruct_match.
                 repeat rewrite <- monad_composition.
                 apply StateInvariant_bind.
                 { unfold respondedCallID.
                   apply StateInvariant_fmap.
                   apply StateInvariant_getCallContext.
                 }
                 intros.
                 destruct_match.
                 *** repeat rewrite <- monad_composition.
                     apply StateInvariant_bind.
                     {
                       unfold callerOfCallID. 
                       apply StateInvariant_bind.
                       { apply StateInvariant_getCallContext. }
                       intros.
                       destruct_match.
                       --- unfold callerOfRequest.
                           apply StateInvariant_bind.
                           { apply StateInvariant_gets. }
                           intros.
                           destruct_match.
                           +++ expand_pairs.
                               apply StateInvariant_return.
                           +++ admit. (* Partiality! *)
                       --- apply StateInvariant_calleeOfCallID.
                     }
                     intros.
                     destruct_match.
                     --- rewrite monad_left_id.
                         destruct_match.
                         +++ rewrite monad_then. rewrite monad_left_id.
                             rewrite onReject_liftRM.
                             unfold rememberTrap.
                             unfold modifyCallContext.
                             apply SP_modify.
                             intros.
                             destruct s0; subst P; simpl.
                             apply H.
                         +++ 
                       

        
        
Admitted.