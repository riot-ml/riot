{
  description = "An actor-model multi-core scheduler for OCaml 5";

  inputs = {
    nixpkgs.url = "github:nix-ocaml/nix-overlays";

    bytestring = {
      url = "github:riot-ml/bytestring";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.minttea.follows = "minttea";
      inputs.rio.follows = "rio";
    };

    castore = {
      url = "github:suri-framework/castore";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    config = {
      url = "github:ocaml-sys/config.ml";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.minttea.follows = "minttea";
    };

    gluon = {
      url = "github:riot-ml/gluon";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.bytestring.follows = "bytestring";
      inputs.config.follows = "config";
      inputs.minttea.follows = "minttea";
      inputs.rio.follows = "rio";
    };

    minttea = {
      url = "github:leostera/minttea";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    rio = {
      url = "github:riot-ml/rio/e7ee9006d96fd91248599fa26c1982364375dd9e";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    telemetry = {
      url = "github:leostera/telemetry";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs @ {
    self,
    flake-parts,
    ...
  }:
    flake-parts.lib.mkFlake {inherit inputs;} {
      systems = ["x86_64-linux" "aarch64-linux" "aarch64-darwin" "x86_64-darwin"];
      perSystem = {
        config,
        self',
        inputs',
        pkgs,
        system,
        ...
      }: let
        inherit (pkgs) ocamlPackages mkShell;
        inherit (ocamlPackages) buildDunePackage;
        version = "0.0.9+dev";
      in {
        devShells = {
          default = mkShell.override {stdenv = pkgs.clang17Stdenv;} {
            buildInputs = with ocamlPackages; [
              dune_3
              ocaml
              utop
              ocamlformat
            ];
            inputsFrom = [self'.packages.default];
            packages = builtins.attrValues {
              inherit (pkgs) clang_17 clang-tools_17 pkg-config;
              inherit (ocamlPackages) ocaml-lsp ocamlformat-rpc-lib;
            };
          };
        };
        packages = {
          randomconv = buildDunePackage {
            version = "0.2.0";
            pname = "randomconv";
            src = builtins.fetchGit {
              url = "git@github.com:hannesm/randomconv.git";
              rev = "b2ce656d09738d676351f5a1c18aff0ff37a7dcc";
              ref = "refs/tags/v0.2.0";
            };
          };

          default = buildDunePackage {
            inherit version;
            pname = "riot";
            propagatedBuildInputs = with ocamlPackages; [
              inputs'.bytestring.packages.default
              inputs'.castore.packages.default
              inputs'.config.packages.default
              inputs'.gluon.packages.default
              inputs'.rio.packages.default
              (mdx.override {
                inherit logs;
              })
              mirage-crypto
              mirage-crypto-rng
              mtime
              odoc
              ptime
              self'.packages.randomconv
              inputs'.telemetry.packages.default
              tls
              uri
              x509
            ];
            src = ./.;
          };
        };
        formatter = pkgs.alejandra;
      };
    };
}
