let
  release = import ./nix/release.nix;
  pkgs = release.pkgs;
in pkgs.haskellPackages.shellFor {
  nativeBuildInputs = with pkgs.haskellPackages; [
    cabal-install
    ghcid
  ];
  buildInputs = [ pkgs.ghcid pkgs.cabal-install ];
  packages = _: pkgs.lib.attrValues release.packages;
}
