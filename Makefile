all: conf; cabal build && cabal test
conf: default.nix shell.nix; \
  nix-shell --command "cabal configure --enable-tests"
repl:; cabal repl lib:dappish
default.nix: *.cabal; cabal2nix . > default.nix
