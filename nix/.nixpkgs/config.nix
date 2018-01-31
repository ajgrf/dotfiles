with import <nixpkgs> {};

{
  packageOverrides = pkgs: rec {

    firefoxBranded = wrapFirefox (firefox-unwrapped.override {
      enableOfficialBranding = true;
    }) { };

    all = with pkgs; buildEnv {
      name = "all";
      paths = [
        anki
	firefoxBranded
	goDev
	go-font
	hledger
	lr
        mblaze
        neovim
	newsboat
        nix
        python35Packages.youtube-dl
        restic
	ripgrep
        shfmt
        stack
        trash-cli
      ];
    };

    goDev = with pkgs; buildEnv {
      name = "goDev";
      paths = [
	delve
        go
	gocode
	godef
	#goimports
	gotags
	gotools
      ];
    };
  };
}
