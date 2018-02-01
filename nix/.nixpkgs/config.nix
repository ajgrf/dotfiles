with import <nixpkgs> {};

{
  packageOverrides = pkgs: rec {

    firefoxBranded = wrapFirefox (firefox-unwrapped.override {
      enableOfficialBranding = true;
    }) { };

    common = with pkgs; buildEnv {
      name = "common";
      paths = [
        anki
	goDev
	hledger
	lr
        mblaze
        neovim
	newsboat
        python36Packages.youtube-dl
        restic
	ripgrep
        shfmt
        stack
        trash-cli
      ];
    };

    nixosProfile = with pkgs; buildEnv {
      name = "nixosProfile";
      paths = [
        common
	alacritty
	calibre
	chromium
	feh
	ffmpeg
	file
	firefoxBranded
	gitAndTools.gitFull
	gnome3.gnome-mines
	gnome-mpv
	isync
	jq
	ledger
	libreoffice
	moreutils
	mpv
	pass
	pdfshuffler
	#quodlibet
	rxvt_unicode-with-plugins
	stow
	syncthing
	tmux
	transmission_gtk
	vim
	xclip
      ];
    };

    debianProfile = with pkgs; buildEnv {
      name = "debianProfile";
      paths = [
        common
	go-font
	nix
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
