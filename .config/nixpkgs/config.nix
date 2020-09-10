let
  unstable = import (fetchTarball https://nixos.org/channels/nixos-unstable/nixexprs.tar.xz) {};
  stable = import (fetchTarball https://nixos.org/channels/nixos-20.03/nixexprs.tar.xz) {};
in
{
  allowUnfree = true;
  firefox.enableBrowserpass = true;
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

    apps-env = buildEnv {
      name = "apps-env";
      extraOutputsToInstall = [ "doc" "man" ];
      paths = [
        anki
        celluloid
        unstable.pkgs.ungoogled-chromium
        firefox
        gnome3.gnome-boxes
        gnome3.gnome-tweaks
        libreoffice
        protonmail-bridge
        quodlibet
        riot-desktop
        unstable.pkgs.spotify
        tor-browser-bundle-bin
        transmission-gtk
        virt-manager
        xterm
        zathura
      ];
    };

    emacs-env = buildEnv {
      name = "emacs-env";
      extraOutputsToInstall = [ "doc" "man" ];
      paths = [
        aspell
        aspellDicts.en
        ((emacsPackagesGen emacs).emacsWithPackages (epkgs: [
          epkgs.emacs-libvterm
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
        unstable.iosevka
        (unstable.iosevka.override { set = "aile"; })
        (unstable.iosevka.override { set = "slab"; })
      ];
    };

    games-env = buildEnv {
      name = "games-env";
      extraOutputsToInstall = [ "doc" "man" ];
      paths = [
        crispyDoom
        dosbox
        unstable.pkgs.lutris
        unstable.pkgs.openmw
        scummvm
        steam
      ];
    };

    tools-env = buildEnv {
      name = "tools-env";
      extraOutputsToInstall = [ "doc" "man" ];
      paths = [
        abcde
        aria2
        beets
        brightnessctl
        direnv
        feh
        ffmpeg
        file
        git
        unstable.pkgs.gitAndTools.git-annex
        gitAndTools.git-annex-remote-rclone
        gitAndTools.gitRemoteGcrypt
        gnupg
        imagemagick
        isync
        ledger
        libnotify
        moreutils
        mr
        mpv
        msmtp
        mu
        neovim
        (pass.withExtensions (ext: with ext; [ pass-otp ]))
        pinentry-gtk2
        poppler_utils
        rclone
        reptyr
        restic
        restic
        rsync
        shellcheck
        shfmt
        stow
        syncthing
        tmux
        trash-cli
        vcsh
        vim
        xclip
        xdotool
        xorg.xrdb
        youtube-dl
      ];
    };
  };
}
