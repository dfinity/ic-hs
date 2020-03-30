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


Definition SP {a} {s} (P Q : s -> Prop) (R : a -> Prop) (act : State s a) :=
  forall s, P s -> Q (snd (runState act s)) /\ R (fst (runState act s)).

Definition StateInvariant {a} {s} (P : s -> Prop) (act : State s a) :=
  SP P P (fun _ => True) act.


Lemma SP_snd_runState:
  forall {a s} (P P' : s -> Prop) (R : a -> Prop) (act : State s a) (x : s),
  SP P P' R act ->
  P x ->
  P' (snd (runState act x)).
Proof.
  intros.
  unfold SP in H.
  apply H. assumption.
Qed.

Lemma SP_return:
  forall {a s} (P : s -> Prop) (Q : a -> Prop) (x : a),
  Q x -> SP P P Q (return_ x).
Proof. intros. intros s0 HP. split; assumption. Qed.


Lemma SP_put:
  forall {s} (P P' : s -> Prop) x,
  P' x ->
  SP P P' (fun _ => True) (put x).
Proof. intros. intros s0 _. split; [ apply H | trivial ]. Qed.

Lemma SP_get:
  forall {s} (P : s -> Prop) (R : s -> Prop),
  (forall s, P s -> R s) ->
  SP P P R get.
Proof. intros. intros s0 H2. split; auto. Qed.

Lemma SP_gets:
  forall {s a} (P : s -> Prop) P' (f : s -> a),
  SP P P (fun x => P' (f x)) get ->
  SP P P P' (gets f).
Proof. intros. intros s0 H1. apply H. assumption. Qed.

Lemma SP_modify:
  forall {S} (P : S -> Prop) (P' : S -> Prop) (f : S -> S),
  (forall s, P s -> P' (f s)) ->
  SP P P' (fun _ => True) (modify f).
Proof. intros. intros s0 H1. split; auto.
 unfold modify. rewrite runState_state. simpl. auto. Qed.

Lemma SP_bind:
  forall {a b s} P P' P'' R R' (act1 : State s a) (act2 : a -> State s b),
  SP P P' R act1 ->
  (forall x, R x -> SP P' P'' R' (act2 x)) ->
  SP P P'' R' (act1 >>= act2).
Proof.
  intros ?????????? H1 H2.
  intros s0 H.
  simpl.
  rewrite runState_bind.
  expand_pairs.
  eapply H2; apply H1; assumption.
Qed.

Lemma SP_fmap:
  forall {a b s} P P' R (f : a -> b) (act : State s a),
  SP P P' (fun x => R (f x)) act ->
  SP P P' R (fmap f act).
Proof.
  intros.
  intros s0 H1.
  rewrite runState_fmap.
  expand_pairs. simpl.
  apply H. assumption.
Qed.

Lemma StateInvariant_return:
  forall {a s} (P : s -> Prop) (x : a),
  StateInvariant P (return_ x).
Proof. intros. apply SP_return. trivial. Qed.

Lemma StateInvariant_get:
  forall {s} (P : s -> Prop),
  StateInvariant P get.
Proof. intros. eapply SP_get. split. Qed.


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
  * intros ? _.  apply H0.
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

Lemma StateInvarinat_fmap:
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
  apply StateInvarinat_fmap; assumption.
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


Lemma onReject_liftRM_then:
  forall A B f (g : M A) (h : RM B),
  onReject f (liftRM g >> h) = (g >> onReject f h).
Proof.
  intros.
  apply state_extensionality. intro s.
  destruct g as [g].
  repeat unfold onReject, runExceptT, liftRM, Class.lift,
    op_zgzgze__, op_zgzg__, Monad__StateT,
    Lazy.Monad__StateT_op_zgzgze__, op_zgzgze____, op_zgzg____,
    Lazy.Monad__StateT_op_zgzg__, Lazy.Monad__StateT_op_zgzgze__,
    runState, Base.op_z2218U__, runIdentity, runStateT,
    Except.Monad__ExceptT,  Except.Monad__ExceptT_op_zgzg__,
    Except.Monad__ExceptT_op_zgzgze__, Except.MonadTrans__ExceptT,
    Except.runExceptT, Except.MonadTrans__ExceptT_lift,
    Class.lift__, Monad__Identity, Identity.Monad__Identity_op_zgzgze__,
    liftM, return_, return___, Lazy.Monad__StateT_return_, pure, 
    Applicative__StateT, pure__, Lazy.Applicative__StateT_pure,
    Identity.Monad__Identity_return_, Applicative__Identity,
    Identity.Applicative__Identity_pure.
  destruct (g s) as [gs].
  destruct gs.
  reflexivity.
Qed.

Lemma onReject_liftRM_bind:
  forall A B f (g : M A) (h : A -> RM B),
  onReject f (liftRM g >>= h) = (g >>= (fun x => onReject f (h x))).
Proof.
  intros.
  apply state_extensionality. intro s.
  destruct g as [g].
  repeat unfold onReject, runExceptT, liftRM, Class.lift,
    op_zgzgze__, op_zgzg__, Monad__StateT,
    Lazy.Monad__StateT_op_zgzgze__, op_zgzgze____, op_zgzg____,
    Lazy.Monad__StateT_op_zgzg__, Lazy.Monad__StateT_op_zgzgze__,
    runState, Base.op_z2218U__, runIdentity, runStateT,
    Monad__ExceptT,  Except.Monad__ExceptT_op_zgzg__,
    Monad__ExceptT_op_zgzgze__, MonadTrans__ExceptT,
    runExceptT, Except.MonadTrans__ExceptT_lift,
    Class.lift__, Monad__Identity, Identity.Monad__Identity_op_zgzgze__,
    liftM, return_, return___, Lazy.Monad__StateT_return_, pure, 
    Applicative__StateT, pure__, Lazy.Applicative__StateT_pure,
    Identity.Monad__Identity_return_, Applicative__Identity,
    Identity.Applicative__Identity_pure.
  destruct (g s) as [gs].
  destruct gs.
  reflexivity.
Qed.

Lemma onReject_return:
  forall A f (x : A),
  onReject f (return_ x) = return_ x.
Proof. intros. apply state_extensionality. intro. reflexivity. Qed. 


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

Lemma pure_then_RM:
  forall A B f (x : A) (a : RM B) s,
  runState (onReject f (pure x >> a)) s = runState (onReject f a) s.
(* This should follow from the MonadLaws *)
Proof. intros. reflexivity. Qed.

Definition PRight {A B} P (x : Either A B) := match x with
  | Left e => True
  | Right y => P y
  end.

Definition ESP {S A E} (P : S -> Prop) (R : A -> Prop) (act : ExceptT E (State S) A) :=
  SP P P (PRight R) (runExceptT act).

Lemma ESP_return:
  forall {s e a} (P : s -> Prop) (R : a -> Prop) (x : a),
  R x -> (ESP (E := e)) P R (return_ x).
Proof. intros. apply SP_return. trivial. Qed.

Lemma ESP_bind:
  forall {s e a b} (P : s -> Prop) R (R' : b -> Prop)
  (act1 : ExceptT e (State s) a) act2,
  ESP P R act1 ->
  (forall (x : a), R x -> ESP P R' (act2 x)) ->
  ESP P R' (act1 >>= act2).
Proof.
  intros.
  unfold ESP in *.
  destruct act1 as [act1].
  unfold runExceptT in *.
  simpl in *.
  eapply SP_bind.
  * apply H.
  * intros. unfold PRight in H1.
    destruct x.
    - apply SP_return. trivial.
    - apply H0. assumption.
Qed.

Lemma ESP_then:
  forall {s e a b} (P : s -> Prop) (R : b -> Prop)
  (act1 : ExceptT e (State s) a) act2,
  ESP P (fun _ => True) act1 ->
  ESP P R act2 ->
  ESP P R (act1 >> act2).
Proof.
  intros.
  rewrite monad_then.
  eapply ESP_bind.
  apply H.
  intros.
  apply H0.
Qed.


Lemma ESP_throwE:
  forall {s e a} (P : s -> Prop) (R : a -> Prop) (x : e),
  ESP P R (throwE x).
Proof. intros. apply SP_return. constructor. Qed.


Lemma ESP_liftRM:
  forall {A P R} (act : M A),
  SP P P R act ->
  ESP P R (liftRM act).
Proof.
  intros.
  unfold ESP.
  unfold liftRM, Class.lift, MonadTrans__ExceptT, Class.lift__, liftM,
    Except.MonadTrans__ExceptT_lift, liftM.
  simpl.
  eapply SP_bind.
  * apply H.
  * intros. apply SP_return. apply H0.
Qed.

Lemma SP_onReject:
  forall {A} P R
  (act : RM A) f,
  (forall e, SP P P R (f e)) ->
  ESP P R act ->
  SP P P R (onReject f act).
Proof.
  intros.
  unfold onReject.
  eapply SP_bind.
  * apply H0.
  * intros.
    unfold PRight in H1.
    destruct x.
    - apply H.
    - apply SP_return. assumption.
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
        apply SP_onReject.
        intro; apply SP_return; trivial.
        set (R' := fun new_id => not (new_id = Mk_EntityId nil)).
        apply ESP_bind with (R := R').
        ** destruct_match.
           -- (* System picks *)
              apply ESP_liftRM.
              apply SP_gets.
              apply SP_get.
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
           apply ESP_bind with (R := fun _  => True).
           -- apply ESP_liftRM.
              apply SP_gets.
              apply SP_get.
              intros. trivial.
           -- intros ex _.
              apply ESP_then; [|apply ESP_then].
              ++ unfold when.
                 destruct_match.
                 *** unfold reject. apply ESP_throwE.
                 *** rewrite monad_applicative_pure. apply ESP_return. trivial.
              ++ apply ESP_liftRM.
                 admit. (* todo *)
              ++ apply ESP_return; trivial.
      - (* install request *)
        admit.
      - (* upgrade request *)
        admit.
      - (* update call request *) 
        admit.
    + (* pop message *)
      admit.
Admitted.