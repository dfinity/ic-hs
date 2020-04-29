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

 * It could serve as a starting point for applying formal verfication to this
   part of the system, e.g. by converting the (non-plumbing) modules to Coq
   using `hs-to-coq`, or by implementing them in a theorem prover and
   extracting Haskell code from it.

To achieve these goals, the following design decisions are made:

 * `ic-ref` is implemented in Haskell, to optimize for development speed,
   type-checking safety, and readablity of carefully selected portions of the
   code.

 * As far as possible, a module either

   - corresponds closely to the spec, and is written with readability as a high
     priority, avoiding language features that obscure meaning. The rough goal
     is “executable pseudo-code”. The use of advanced langauge features or non-idiomatic
     code that _help_ readability are encouraged.

     Examples: `IC.Ref`, `IC.Canister.Impl`, `IC.HTTP.RequestId`

   - is a plumbing module that handles some technical aspect, and pave the way
     for the simplicity in the previously mentioned modules. It is expected
     that reading such modules may require high level of familiarity with Haskell.

     Examples: `IC.Wasm.Imports`, `IC.HTTP.CBOR`.

   This is an ongoing refinement process, striving for a probably unattainable
   ideal als the goal.

Installation
------------

To install it into your normal environment, run from the top-level repository
directory.

    nix-env -i -f . -A ic-ref

This installs the all binaries (`ic-ref`, `ic-ref-test`, `ic-ref-run`) in your
the PATH.

Using
-----

* The `ic-ref-run` program takes, as an argument, a file with `install`, `call`,
  `query`, `upgrade` commands, just like
  [`drun`](https://github.com/dfinity-lab/dfinity/tree/master/rs/drun/).

* The `ic-ref` program starts a webserver at `http://0.0.0.0:8001/` that implements the public
  Internet Computer interface, and can be used with `dfx --client http://0.0.0.0:8001/`.

  If you point your browser to `http://0.0.0.0:8001/` you get the evolution of
  the IC state as JSON. Recommended to use Firefox, as it provides a nice UI for
  browsing JSON.

* The `ic-request-id` tool takes a CBOR-request (stdin or via a file) and
  calculates its request id.

* The `ic-ref-test` acceptance test.
  Run `ic-ref-test --endpoint http://localhost:8080/` to run against a specific node.

Both `ic-ref` and `ic-ref-run` can report the spec version (`--spec-version`)
and implementation version (`--version`).

Developing on ic-ref
---------------------

Running `nix-shell` in the `ic-ref/impl` directory gives you an environment
that allows you to build the project using `cabal build`. You can also run
`cabal run ic-ref` etc. to run it directly from source.

One possible workflow is to run

    ghcid -c 'cabal repl ic-ref' -rMain.main

which will run `ic-ref` and restart upon file changes.

Developing on ic-ref-test
-------------------------

Before running the test suite, make sure you have built the universal canister.
The symbolic link in `impl/test-data/universal_canister.wasm` points to the
build output produced by

    cd ../universal_canister
    cargo build --target wasm32-unknown-unknown --release

now you can run the test suite with

    cabal build ic-ref-test

The `-p` flag, i.e.

    cabal build ic-ref-test -- -p upgrade

allows you can run tests selectively (i.e. only those whose name include
“upgrade”).

Updating Haskell Packages
-------------------------

When the `.cabal` file of a Haskell package is changed you need to make sure the
corresponding `default.nix` file (stored in `nix/generated/`) is kept in sync
with it.

As mentioned in the `nix/generate.nix` files, these files are automatically
generated. See `nix/generate.nix` for the command to update them.

Don't worry if you forget to update the `default.nix` file, the CI job
`check-generated` checks if these files are in sync and fails with a diff if
they aren't.
