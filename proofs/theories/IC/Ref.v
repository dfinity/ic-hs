Require Import Control.Monad.Trans.State.Lazy.
Require Import Control.Monad.Trans.Except.
Require Import Data.Functor.Identity.

Require Import IC.Ref.
Require Import IC.Types.
Require Import Data.Map.Internal.

Import GHC.Base.
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


Lemma onReject_throwE:
  forall A B f x (h : RM A),
  (onReject f ((throwE x : RM B) >> h)) = f x.
Proof. intros.  apply state_extensionality. intro. reflexivity. Qed.

Lemma pure_then_RM:
  forall A B f (x : A) (a : RM B) s,
  runState (onReject f (pure x >> a)) s = runState (onReject f a) s.
(* This should follow from the MonadLaws *)
Proof. intros. reflexivity. Qed.


Definition ICInvariant Q P :=
  P initialIC /\
  (forall rid pk r s0, 
    P s0 ->
    Q pk r ->
    evalState (authAsyncRequest pk r) s0 = true ->
    P (execState (submitRequest rid r) s0)) /\
  (forall s0,
    P s0 ->
    P (execState runStep s0)).


Definition any_request (pk : Blob) (r : AsyncRequest) := True.

Theorem no_empty_canister_id :
  ICInvariant any_request (fun s =>
    member (Mk_EntityId nil) (canisters s) = false).
Proof.
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
    unfold nextReceived.
    unfold execState.
    rewrite runState_gets.
    destruct_match.
    + destruct p as [rid ar].
      rewrite runState_then.
      rewrite runState_return.
      simpl.
      unfold processRequest.
      destruct_match.
      - unfold op_zezlzl__.
        (* TODO: This duplicates the continuation. How to common up? *)
        destruct_match.
        ** rewrite runState_bind.
           rewrite onReject_liftRM_bind.
           rewrite runState_gets.
           rewrite onReject_liftRM_bind.
           rewrite runState_gets.
           match goal with | [ |- context[when ?P] ] => destruct P eqn:Hwhen end.
           ++ unfold when.
              unfold reject.
              rewrite onReject_throwE.
              simpl.
              destruct s0. simpl. assumption.
           ++ unfold when. rewrite pure_then_RM.
              rewrite onReject_liftRM_then.
              unfold createEmptyCanister.
              rewrite runState_then.
              unfold modify. rewrite runState_state.
              unfold snd.
              rewrite onReject_return, runState_return.
              unfold setReqStatus.
              unfold modify. rewrite runState_state.
              unfold Base.op_z2218U__ in *.
              destruct s0.
              simpl.
              (* now need theory about member and insert *)
              (* and that freshid is not nil *)
              admit.
        ** admit. (* Cannot prove this, need to rule out ForcedChoice *)
        ** rewrite runState_bind.
           match goal with | [ |- context[Monad.unless ?P] ] => destruct P eqn:Hunless end.
           ++ unfold Monad.unless.
              rewrite monad_then.
              rewrite monad_applicative_pure.
              rewrite monad_left_id.
              rewrite monad_left_id.
              rewrite onReject_liftRM_bind.
              rewrite runState_gets.
              match goal with | [ |- context[when ?P] ] => destruct P eqn:Hwhen end.
              -- unfold when.
                 unfold reject.
                 rewrite onReject_throwE.
                 simpl. 
                 destruct s0. simpl. assumption.
              -- unfold when. rewrite pure_then_RM.
                 rewrite onReject_liftRM_then.
                 unfold createEmptyCanister.
                 rewrite runState_then.
                 unfold modify. rewrite runState_state.
                 unfold snd.
                 rewrite onReject_return, runState_return.
                 unfold setReqStatus.
                 unfold modify. rewrite runState_state.
                 unfold Base.op_z2218U__ in *.
                 destruct s0.
                 simpl.
                 (* now need theory about member and insert *)
                 (* and that derivedId is not nil *)
                 admit.
Admitted.