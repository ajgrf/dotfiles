with import <nixpkgs> {};

{
  firefox.enableBrowserpass = true;

  packageOverrides = pkgs: rec {

    common = with pkgs; buildEnv {
      name = "common";
      extraOutputsToInstall = [ "doc" "man" ];
      paths = [
        anki
        gnuapl
        gopass
        hledger
        hugo
        mblaze
        neovim
        newsboat
        rclone
        restic
        ripgrep
        shfmt
        stack
        trash-cli
        youtube-dl
      ];
    };

    nixosProfile = with pkgs; buildEnv {
      name = "nixosProfile";
      extraOutputsToInstall = [ "doc" "man" ];
      paths = [
        common
        brightnessctl
        chromium
        cron
        dmenu
        entr
        feh
        ffmpeg
        file
        firefox-esr-60
        go-font
        gitAndTools.gitFull
        gnome3.gnome-mines
        gnome-mpv
        gnupg
        go
        isync
        jq
        ledger
        libreoffice
        moreutils
        mpc_cli
        mpd
        mpdris2
        mpv
        msmtp
        mutt
        pamix
        pamixer
        pandoc
        (pass.withExtensions (ext: with ext; [ pass-otp pass-import ]))
        poppler_utils
        pinentry_gnome
        qtpass
        #quodlibet
        rxvt_unicode-with-plugins
        stow
        syncthing
        tmux
        transmission_gtk
        vim
        w3m
        xclip
        xdotool
      ];
    };

    debianProfile = with pkgs; buildEnv {
      name = "debianProfile";
      extraOutputsToInstall = [ "doc" "man" ];
      paths = [
        common
        nix
      ];
    };
  };
}
