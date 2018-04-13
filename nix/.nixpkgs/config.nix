with import <nixpkgs> {};

{
  packageOverrides = pkgs: rec {

    firefoxBranded = wrapFirefox (firefox-unwrapped.override {
      enableOfficialBranding = true;
    }) { };

    dwmCustom = dwm.override {
      patches = [ ./dwm-ajgrf-6.1.diff ];
    };

    stCustom = st.override {
      conf = builtins.readFile ./st-config.h;
      patches = [ 
        (fetchurl {
          url = "https://st.suckless.org/patches/solarized/st-no_bold_colors-0.7.diff";
          sha256 = "2e8cdbeaaa79ed067ffcfdcf4c5f09fb5c8c984906cde97226d4dd219dda39dc";
        })
        (fetchurl {
          url = "https://st.suckless.org/patches/scrollback/st-scrollback-0.7.diff";
          sha256 = "f721b15a5aa8d77a4b6b44713131c5f55e7fca04006bc0a3cb140ed51c14cfb6";
        })
      ];
    };

    common = with pkgs; buildEnv {
      name = "common";
      extraOutputsToInstall = [ "doc" "man" ];
      paths = [
        anki
        gnuapl
        goDev
        hledger
        iosevka-bin
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
        dmenu
        dwmCustom
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
        mpc_cli
        mpd
        mpdris2
        mpv
        ncmpc
        pass
        poppler_utils
        #quodlibet
        redshift
        rxvt_unicode-with-plugins
        stCustom
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
