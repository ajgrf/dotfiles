with import <nixpkgs> {};

{
  packageOverrides = pkgs: rec {

    firefoxBranded = wrapFirefox (firefox-unwrapped.override {
      enableOfficialBranding = true;
    }) { };

    common = with pkgs; buildEnv {
      name = "common";
      extraOutputsToInstall = [ "doc" "man" ];
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
      extraOutputsToInstall = [ "doc" "man" ];
      paths = [
        common
	alacritty
	calibre
	chromium
	entr
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
      extraOutputsToInstall = [ "doc" "man" ];
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
