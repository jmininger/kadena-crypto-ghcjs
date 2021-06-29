{ system ? builtins.currentSystem,
  pkgs ? import <nixpkgs> {}
}:
let
  nix-thunk-src = (pkgs.fetchFromGitHub {
    owner = "obsidiansystems";
    repo = "nix-thunk";
    rev = "bab7329163fce579eaa9cfba67a4851ab806b76f";
    sha256 = "0wn96xn6prjzcsh4n8p1n40wi8la53ym5h2frlqbfzas7isxwygg";
  });
  inherit (import nix-thunk-src {}) thunkSource;
  gitignoreSrc = pkgs.fetchFromGitHub {
    owner = "hercules-ci";
    repo = "gitignore";
    rev = "f9e996052b5af4032fe6150bba4a6fe4f7b9d698";
    sha256 = "0jrh5ghisaqdd0vldbywags20m2cxpkbbk5jjjmwaw0gr8nhsafv";
  };
  inherit (import gitignoreSrc { inherit (pkgs) lib; }) gitignoreSource;

  rp = import ./dep/reflex-platform { inherit system; };

in rp.project ({ pkgs, ... }: {
  packages = {
    kadena-crypto-ghcjs = gitignoreSource ./.;
  };

  shells = {
    ghcjs = ["kadena-crypto-ghcjs" ];
    ghc = ["kadena-crypto-ghcjs" ];
  };
})

