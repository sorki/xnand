(import <nixpkgs> {
  overlays = import /home/srk/git/hnix-overlay/overlay.nix;
}).haskellPackages.callCabal2nix "hnixbot" ./. {}
