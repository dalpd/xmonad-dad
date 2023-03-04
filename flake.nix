{
  description = "dad's XMonad flake";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/haskell-updates";

  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" ];
      forAllSystems = with nixpkgs.lib; f: genAttrs supportedSystems (system: f system);

      nixpkgsFor = forAllSystems (system:
        import nixpkgs {
          inherit system;
          overlays = [ self.overlays.default ];
        });
    in {
      checks = self.packages;

      defaultPackage = forAllSystems (system: self.packages.${system}.xmonad-dad);

      devShell = forAllSystems (system:
        let haskellPackages = nixpkgsFor.${system}.haskellPackages;
        in haskellPackages.shellFor {
          buildInputs = with haskellPackages; [
            haskell-language-server
            ghcid
            cabal-install
            nixfmt
          ];

          packages = p: [ self.packages.${system}.xmonad-dad ];

          shellHook = "export PS1='\\e[1;34m[\\u@nix-shell:\\w]$ \\e[0m'";

          withHoogle = false;
          withHaddocks = false;
        });

      overlays.default =
        (final: prev: { xmonad-dad = final.haskellPackages.callCabal2nix "xmonad-dad" ./. { }; });

      packages = forAllSystems (system: { xmonad-dad = nixpkgsFor.${system}.xmonad-dad; });
    };
}
