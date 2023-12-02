let
  pkgs = import ./nix/nixpkgs.nix;
in
pkgs.mkShell {
  LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
  buildInputs = with pkgs; [
    bash
    curl
    jq
  ];
}
