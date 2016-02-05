* An incremental IMP interpreter, built using Adapton. [![Build Status](https://travis-ci.org/plum-umd/inc-imp.svg?branch=master)](https://travis-ci.org/plum-umd/inc-imp)


Requires:
- ocaml 4.02.0+
- opam packages
  - adapton latest (https://github.com/plum-umd/adapton.ocaml)
    - pin the opam package with `make install`
  - ppx_deriving
- R (no known version dependencies, but I'm running 3.2.0)
  - R packages dplyr, reshape2
- python 2

To run a small batch of tests:
> make test

To run the larger set of tests (recreate OOPSLA submission results):
> make test-oopsla
