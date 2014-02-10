Rtime — Timelines for OCaml React
-------------------------------------------------------------------------------
Release %%VERSION%%

Rtime is an OCaml module implementing timelines for [React][1].  It
manages time stamp events, delayed events and delayed signals along
timelines. The client chooses the concrete timeline by providing an
absolute notion of time. Running the timeline at the appropriate pace
is left to the client.

Rtime is made of a single module and depends on [React][1]. It is
distributed under the BSD3 license.

[1]: http://erratique.ch/software/react

Home page: http://erratique.ch/software/rtime  
Contact: Daniel Bünzli `<daniel.buenzl i@erratique.ch>`


## Installation

Rtime can be installed with `opam`:

    opam install rtime

If you don't use `opam` consult the [`opam`](opam) file for build
instructions.


## Documentation

The documentation and API reference is automatically generated by
`ocamldoc` from the interfaces. It can be consulted [online][3]
and there is a generated version in the `doc` directory of the 
distribution. 

[3]: http://erratique.ch/software/rtime/doc/Rtime


## Sample programs

If you installed Rtime with `opam` sample programs are located in
the directory `opam config var rtime:doc`. These programs define
the command line of some classic programs.

In the distribution sample programs are located in the `test`
directory of the distribution. They can be built with:

    ocamlbuild -use-ocamlfind test/tests.otarget

The resulting binaries are in `_build/test`:

- `test.native` tests the library, nothing should fail. Note that it
  may fail in a heavily loaded environment or if the system's timers
  are not precise enough. You can adjust the tolerance to precision
  with `-p`, see `test.native -help` for more information.
  