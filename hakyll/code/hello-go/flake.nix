{
  description = "Impure GitLab release example";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs/release-22.05";
  };

  outputs = { self, flake-utils, nixpkgs }: {
    overlay = final: prev: {
      inherit (self.packages.${prev.system}) hello-go;
    };
  } // flake-utils.lib.eachDefaultSystem
    (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        inherit (pkgs) lib;
      in
      {
        devShells.default = with pkgs; mkShell {
          buildInputs = [
            go
          ];
        };

        packages = {
          default = self.packages.${system}.hello-go;

          hello-go =
            let
              sourceInfo =
                if self.sourceInfo ? rev
                then self.sourceInfo // {
                  tag = builtins.getEnv "CI_COMMIT_TAG";
                }
                else throw "Refusing to build from a dirty Git tree!";

              version =
                if sourceInfo.tag != ""
                then sourceInfo.tag
                else sourceInfo.lastModifiedDate;
            in
            pkgs.buildGoModule {
              pname = "hello-go";
              inherit version;

              src = lib.cleanSourceWith {
                filter = name: type:
                  let baseName = baseNameOf (toString name); in
                  (
                    baseName == "main.go" ||
                      baseName == "go.mod"
                  );
                src = ./.;
              };

              vendorSha256 = null;

              ldflags = [
                "-X main.Version=${version}"
                "-X main.CommitHash=${sourceInfo.rev}"
              ];
            };
        };
      });
}
