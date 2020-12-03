let
  unstable = import (fetchTarball https://nixos.org/channels/nixos-unstable/nixexprs.tar.xz) {};
  stable = import (fetchTarball https://nixos.org/channels/nixos-20.09/nixexprs.tar.xz) {};
in
{
  allowUnfree = true;
  firefox.enableTridactylNative = true;

  packageOverrides = _: with stable.pkgs; rec {

    all-env = buildEnv {
      name = "all-env";
      paths = [
        apps-env
        emacs-env
        fonts-env
        games-env
        tools-env
      ];
    };

    minimal-env = buildEnv {
      name = "minimal-env";
      paths = [
        emacs-env
        tools-env
      ];
    };

    apps-env = buildEnv {
      name = "apps-env";
      extraOutputsToInstall = [ "doc" "man" ];
      paths = [
        anki
        bitwarden
        ungoogled-chromium
        firefox
        gnome3.gnome-boxes
        gnome3.gnome-tweaks
        keybase
        keybase-gui
        libreoffice
        lollypop
        unstable.protonmail-bridge
        # quodlibet
        unstable.pkgs.signal-desktop
        spotify
        thunderbird
        tor-browser-bundle-bin
        transmission-gtk
        virt-manager
        vlc
        xterm
      ];
    };

    emacs-env = buildEnv {
      name = "emacs-env";
      extraOutputsToInstall = [ "doc" "man" ];
      paths = [
        aspell
        aspellDicts.en
        ((emacsPackagesGen emacs).emacsWithPackages (epkgs: [
          epkgs.vterm
          epkgs.pdf-tools
        ]))
        fd
        git
        ripgrep
      ];
    };

    fonts-env = buildEnv {
      name = "fonts-env";
      paths = [
        go-font
        iosevka
        (iosevka.override { set = "aile"; })
        (iosevka.override { set = "slab"; })
      ];
    };

    games-env = buildEnv {
      name = "games-env";
      extraOutputsToInstall = [ "doc" "man" ];
      paths = [
        crispyDoom
        dosbox
        lutris
        openmw
        scummvm
        steam
      ];
    };

    tools-env = buildEnv {
      name = "tools-env";
      extraOutputsToInstall = [ "doc" "man" ];
      paths = [
        aria2
        bitwarden-cli
        brightnessctl
        direnv
        feh
        ffmpeg
        file
        git
        gitAndTools.git-annex
        gitAndTools.git-annex-remote-rclone
        gnupg
        imagemagick
        unstable.pkgs.kopia
        ledger
        libnotify
        moreutils
        mr
        pass
        pinentry-gtk2
        poppler_utils
        unstable.pkgs.rbw
        rclone
        reptyr
        rsync
        shellcheck
        shfmt
        stow
        syncthing
        tmux
        trash-cli
        xclip
        xdotool
        xorg.xrdb
        youtube-dl
      ];
    };
  };
}
