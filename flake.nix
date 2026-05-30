{
  description = "Haskell DSL for generating ESPHome YAML configurations";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
    flake-utils.url = "github:numtide/flake-utils";
    nix-filter.url = "github:numtide/nix-filter";
    feedback.url = "github:NorfairKing/feedback";

    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    pre-commit-hooks.inputs.nixpkgs.follows = "nixpkgs";
    weeder-nix.url = "github:NorfairKing/weeder-nix";
    weeder-nix.inputs = {
      nixpkgs.follows = "nixpkgs";
      pre-commit-hooks.follows = "pre-commit-hooks";
    };
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    feedback,
    nix-filter,
    pre-commit-hooks,
    ...
  } @ inputs: let
    pkgsFor = system:
      import nixpkgs {
        inherit system;
        overlays = [self.overlays.default];
      };
  in
    {
      overlays.default = final: prev: let
        filteredSrc = nix-filter.lib {
          root = ./.;
          include = [
            "app/"
            "src/"
            "test/"
            "package.yaml"
            "LICENSE"
          ];
        };
      in rec {
        espbunker = final.haskell.lib.justStaticExecutables haskellPackages.espbunker;
        haskellPackages =
          prev.haskell.packages.ghc9122.override
          (old: {
            overrides =
              final.lib.composeExtensions
              (old.overrides or (_: _: {}))
              (
                self: super: {
                  espbunker =
                    # self.generateOptparseApplicativeCompletions
                    #   [ "espbunker" ]
                    (
                      (self.callCabal2nix "espbunker" filteredSrc {}).overrideAttrs
                      (
                        oldAttrs: {
                          nativeBuildInputs =
                            oldAttrs.nativeBuildInputs
                            ++ [final.makeWrapper final.esphome];
                          # postInstall =
                          #   (oldAttrs.postInstall or "")
                          #   + ''
                          #     wrapProgram $out/bin/espbunker \
                          #       --suffix PATH : ${final.lib.makeBinPath [final.esphome]}
                          #   '';
                        }
                      )
                    );
                }
              );
          });
      };
    }
    // flake-utils.lib.eachDefaultSystem (
      system: let
        pkgs = pkgsFor system;
        precommitCheck = pre-commit-hooks.lib.${system}.run {
          src = ./.;
          hooks = {
            # actionlint.enable = true;
            alejandra.enable = true;
            hlint.enable = true;
            hpack.enable = true;
            markdownlint.enable = true;
            nil.enable = true;
            ormolu.enable = true;
            ripsecrets.enable = true;
          };
        };
      in rec {
        packages = {
          inherit (pkgs) espbunker;
          default = packages.espbunker;
        };

        apps = {
          espbunker = flake-utils.lib.mkApp {drv = packages.espbunker;};
          default = apps.espbunker;
        };

        checks = {
          pre-commit-check = precommitCheck;
          weeder-check = inputs.weeder-nix.lib.${system}.makeWeederCheck {
            haskellPackages = pkgs.haskellPackages;
            packages = ["espbunker"];
            reportOnly = true;
          };
        };

        devShells.default = pkgs.haskellPackages.shellFor {
          packages = p: [packages.espbunker];
          buildInputs = with pkgs;
          with pkgs.haskellPackages; [
            # actionlint
            alejandra
            cabal-install
            esphome
            feedback.packages.${system}.default
            ghcid
            haskell-language-server
            hlint
            nil
            ormolu
            statix
          ];
          inherit (precommitCheck) shellHook;
        };
      }
    );
  nixConfig = {
    extra-substituters = "https://opensource.cachix.org";
    extra-trusted-public-keys = "opensource.cachix.org-1:6t9YnrHI+t4lUilDKP2sNvmFA9LCKdShfrtwPqj2vKc=";
  };
}
