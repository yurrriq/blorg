{
  description = "My blog, built with Org mode and Hakyll";

  inputs = {
    emacs-overlay = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:nix-community/emacs-overlay";
    };
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.follows = "nixpkgs-stable";
    nixpkgs-stable.url = "github:NixOS/nixpkgs/nixos-23.05";
    pre-commit-hooks-nix = {
      inputs = {
        nixpkgs.follows = "nixpkgs";
        nixpkgs-stable.follows = "nixpkgs-stable";
      };
      url = "github:cachix/pre-commit-hooks.nix";
    };
    treefmt-nix = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:numtide/treefmt-nix";
    };
  };

  outputs = inputs@{ flake-parts, self, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      flake = {
        overlays.emacs = _final: prev: {
          myEmacs = prev.emacsWithPackagesFromUsePackage {
            alwaysEnsure = true;
            config = ./emacs.el;
          };
        };
      };

      imports = [
        inputs.pre-commit-hooks-nix.flakeModule
        inputs.treefmt-nix.flakeModule
      ];

      systems = [ "x86_64-linux" ];

      perSystem = { config, lib, self', pkgs, system, ... }: {
        _module.args.pkgs = import inputs.nixpkgs {
          overlays = [
            inputs.emacs-overlay.overlay
            self.overlays.emacs
          ];
          inherit system;
        };

        devShells = {
          clojure = pkgs.mkShell {
            nativeBuildInputs = with pkgs; [
              clojure
              clojure-lsp
              leiningen
            ];
          };

          default = pkgs.mkShell {
            FONTCONFIG_FILE = pkgs.makeFontsConf {
              fontDirectories = [
                (pkgs.nerdfonts.override { fonts = [ "Iosevka" ]; })
              ];
            };

            inputsFrom = [
              config.pre-commit.devShell
            ] ++
            builtins.attrValues
              (lib.filterAttrs (name: _: name != "default") self'.devShells);

            nativeBuildInputs = with pkgs; [
              myEmacs
            ];
          };

          erlang = pkgs.mkShell {
            nativeBuildInputs = with pkgs; [
              erlfmt
              erlang-ls
              erlang_nox
              rebar3
            ];
          };

          haskell = pkgs.mkShell {
            inputsFrom = [
              self'.packages.blorg.env
            ];

            nativeBuildInputs = with pkgs; [
              cabal-install
              ghc
              ghcid
              haskell-language-server
            ] ++ (with haskellPackages; [
              hakyll
              ormolu
              pointfree
            ]);
          };

          idris = pkgs.mkShell {
            nativeBuildInputs = with pkgs; [
              idris
            ];
          };

          lilypond = pkgs.mkShell {
            nativeBuildInputs = with pkgs; [
              lilypond
            ];
          };

          lfe = pkgs.mkShell {
            nativeBuildInputs = with pkgs; [
            ];
          };
        };

        packages = {
          blorg = pkgs.haskellPackages.developPackage {
            root = ./.;
            modifier = drv:
              pkgs.haskell.lib.addBuildTools drv (with pkgs; [
                zlib.dev
                zlib.out
              ]);
          };

          default = self'.packages.blorg;
        };

        pre-commit.settings.hooks = {
          convco.enable = true;
          # FIXME: this is very angry about some generated files
          editorconfig-checker.enable = false;
          treefmt.enable = true;
        };

        treefmt = {
          projectRootFile = ./flake.nix;
          programs = {
            deadnix.enable = true;
            erlfmt.enable = true;
            hlint.enable = true;
            nixpkgs-fmt.enable = true;
            ormolu.enable = true;
          };
        };
      };
    };
}
