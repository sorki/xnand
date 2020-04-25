{ pkgs ? import <nixpkgs> {} }:
let
  inherit (pkgs) lib;
  hlib = pkgs.haskell.lib;

  hpkgs = pkgs.haskellPackages.extend (self: super: {
    nixbot = (self.callCabal2nix "nixbot" (lib.sourceByRegex ./. [
      "^src.*$"
      "^.*\\.cabal$"
      "^LICENSE$"
      "^nix.*$"
    ]) {}).overrideAttrs (drv: {
      nativeBuildInputs = drv.nativeBuildInputs or [] ++ [ pkgs.makeWrapper ];
      postInstall = drv.postInstall or "" + ''
        wrapProgram $out/bin/nixbot \
          --prefix PATH : "${pkgs.lib.makeBinPath [ pkgs.gnutar pkgs.gzip ]}"
      '';
    });
  });
in hpkgs.nixbot // {
  inherit hpkgs pkgs;
}
