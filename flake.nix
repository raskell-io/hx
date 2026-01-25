{
  description = "hx - Fast, opinionated Haskell toolchain CLI";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        version = "0.4.1";

        # Binary releases for each platform
        sources = {
          x86_64-linux = {
            url = "https://github.com/raskell-io/hx/releases/download/v${version}/hx-v${version}-x86_64-unknown-linux-gnu.tar.gz";
            sha256 = "e10f1306737a81bff158ca90f19ceb160f92a7bf2fd3f6391474861c9b8d8a26";
          };
          aarch64-linux = {
            url = "https://github.com/raskell-io/hx/releases/download/v${version}/hx-v${version}-aarch64-unknown-linux-gnu.tar.gz";
            sha256 = "a5fcb4b700fb52c36135a13c3996cf13176820ba919df1d439e06b4e85f453d9";
          };
          x86_64-darwin = {
            url = "https://github.com/raskell-io/hx/releases/download/v${version}/hx-v${version}-x86_64-apple-darwin.tar.gz";
            sha256 = "dd53121f3b447904d13069a248064f579ee66efb344f4e070c58362b7e2e471e";
          };
          aarch64-darwin = {
            url = "https://github.com/raskell-io/hx/releases/download/v${version}/hx-v${version}-aarch64-apple-darwin.tar.gz";
            sha256 = "bbac7322e6e229363c16494aef0dea5e8ec1615b0c5ae58b562a21ae58e030d7";
          };
        };

        src = sources.${system} or (throw "Unsupported system: ${system}");
      in
      {
        packages.default = pkgs.stdenv.mkDerivation {
          pname = "hx";
          inherit version;

          src = pkgs.fetchurl {
            inherit (src) url sha256;
          };

          sourceRoot = ".";

          nativeBuildInputs = pkgs.lib.optionals pkgs.stdenv.isLinux [
            pkgs.autoPatchelfHook
          ];

          buildInputs = pkgs.lib.optionals pkgs.stdenv.isLinux [
            pkgs.stdenv.cc.cc.lib
          ];

          installPhase = ''
            runHook preInstall
            install -Dm755 hx $out/bin/hx
            runHook postInstall
          '';

          meta = with pkgs.lib; {
            description = "Fast, opinionated Haskell toolchain CLI";
            homepage = "https://github.com/raskell-io/hx";
            license = licenses.mit;
            platforms = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
            mainProgram = "hx";
          };
        };

        packages.hx = self.packages.${system}.default;
      }
    );
}
