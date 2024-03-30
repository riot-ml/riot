{
  description = "An actor-model multi-core scheduler for OCaml 5";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    castore = {
      url = "github:suri-framework/castore";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    config = {
      url = "github:ocaml-sys/config.ml";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.minttea.follows = "minttea";
    };
    libc = {
      url = "github:ocaml-sys/libc.ml";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.config.follows = "config";
    };
    minttea = {
      url = "github:leostera/minttea";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    rio = {
      url = "github:riot-ml/rio";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    telemetry = {
      url = "github:leostera/telemetry";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{ self, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" "x86_64-darwin" ];
      perSystem = { config, self', inputs', pkgs, system, ... }:
        let
          inherit (pkgs) ocamlPackages mkShell lib;
          inherit (ocamlPackages) buildDunePackage;
          version = "0.0.8+dev";
        in
          {
            devShells = {
              default = mkShell.override {stdenv = pkgs.clang17Stdenv;} {
                inputsFrom = [
                  self'.packages.default
                  self'.packages.bytestring
                  self'.packages.gluon
                ];
                buildInputs = [ ocamlPackages.utop ];
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
                  ref = "refs/tags/${version}";
                };
              };

              default = buildDunePackage {
                inherit version;
                pname = "riot";
                propagatedBuildInputs = with ocamlPackages; [
                  self'.packages.bytestring
                  inputs'.castore.packages.default
                  inputs'.config.packages.default
                  self'.packages.gluon
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
              bytestring = buildDunePackage {
                inherit version;
                pname = "bytestring";
                propagatedBuildInputs = with ocamlPackages; [
                  inputs'.rio.packages.default
                  ppxlib
                  qcheck
                  sedlex
                  inputs'.minttea.packages.spices
                ];
                src = ./.;
              };
              gluon = buildDunePackage {
                inherit version;
                pname = "gluon";
                propagatedBuildInputs = with ocamlPackages; [
                  self'.packages.bytestring
                  inputs'.config.packages.default
                  inputs'.libc.packages.default
                  inputs'.rio.packages.default
                  uri
                ]
                ++ lib.optionals pkgs.stdenv.isDarwin [ pkgs.darwin.apple_sdk.frameworks.System ];
                src = ./.;
              };
            };
          };
    };
}
