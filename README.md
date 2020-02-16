The IC reference implementation
===============================

`ic-ref` is a partial implementation of the public interface of the DFINITY
Internet Computer, as specified in the [Public Spec].

[Public Spec]: https://docs.dfinity.systems/spec/public/

Goals
-----

The goals of the reference implemenation are

 * It evolves in lock-step with the Public Spec. At least versioned released of
   the Public Spec come with a complete implemenation of `ic-stub`.

 * Supplement the prose and pseudo-code in the Public Spec for additional and
   concrete clarity.

 * Ideally, relevant code pieces of `ic-stub` are as easy to understand as
   carefully written pseudo-code.

 * Increase weight of and confidence in the Public Spec, by demonstrating
   implementability.

 * Aid in the development of the Public Spec by uncovering omissions,
   inconsistencies or unexpected complexity.

 * Allow testing of external clients (like `dfx`) directly against the
   reference implementation.

 * Provide a simplified mock environment for testing Canisters (e.g. `motoko`
   output) that does not require networking.

   In particular, provide `ic-ref-run`, a binary that allows to script the
   execution of a single canister.

 * Aid in the production implementation of the Internet Computer, by allowing
   to probe the reference implementation to better understand intended
   behaviour and by comparing the behaviour of the two.

 * Using `ic-ref` is simple: Completely self-contained, no on-disk state.

 * Performance is good enough to run small examples.

 * The primary focus is describing the happy path execution, and not
   necessarily the precise error reporting behaviour upon bad usage (e.g. bad
   canisters, resource exhaustion, module validation).

There are also explicit non-goals to keep in mind:

 * `ic-ref` does not need to support canisters that are large or very
   long-running.

 * No persitence across runs of `ic-ref`.

 * It is explicitly not desirable to share code between reference and
   production implementation, to achieve the multi-version cross-checking
   effect. Not using Rust for `ic-ref` helps with that.

 * No guaranteed protection against bad effects from malicous interactions.

 * No duplication of commonly available functionality. In particular, the
   assumption is that the production implementation will use a mature Wasm
   embedder that implements Wasm validation correctly, so `ic-ref` does not
   itself implement validation.

Furthermore there are some stretch goals that would be nice to have, but not if reqiures
compromising the main goals.

 * The reference implementation describes _one_ possible execution, but not
   _all_ possible behaviours of the Public Spec. If this can be changed (e.g.
   using non-deterministic modeling of computation) without compromising
   readability and normal execution, then this would be nice.

 * A deep or type-level embedding of the interfaces (HTTP, System) that can be
   used separately to  generation of production code (“interface stubs”).

 * Debugging/logging/trace features that aid understanding the behaviour and/or
   help debug canisters.

To achieve these goals, the following design decisions are made:

 * `ic-ref` is implemented in Haskell, to optimize for development speed,
   type-checking safety, and readablity of carefully selected portions of the
   code.

 * As far as possible, a module either corresponds closely to the spec, and is
   written with readability as a high priority, avoiding language features that
   obscure meaning (e.g. `IC.Ref`, `IC.Canister.Impl`) or it is a plumbing
   module that handles some technical aspect (e.g. `IC.Wasm.Imports`).
   This is an ongoing refinement process.

Installation of `ic-ref-run`
-----------------------------

To install it into your normal environment, run from the top-level repository
directory.

    nix-env -i -f . -A ic-ref


Developing on ic-ref
---------------------

Running `nix-shell` in the `ic-ref/` directory should give you an environment
that allows you to build the project using `cabal new-build`.
