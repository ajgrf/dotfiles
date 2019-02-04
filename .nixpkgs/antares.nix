# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      /etc/nixos/hardware-configuration.nix
    ];

  boot.initrd.luks.devices."cryptroot".keyFile = "/crypto_keyfile.bin";
  boot.loader = {
    timeout = 1;
    efi = {
      efiSysMountPoint = "/boot/efi";
      canTouchEfiVariables = true;
    };
    grub = {
      device = "nodev";
      efiSupport = true;
      enableCryptodisk = true;
      enable = true;
      extraInitrd = "/crypto_keyfile.bin.cpio.gz";
      gfxmodeEfi = "1024x768";
      splashImage = null;
    };
  };

  networking = {
    hostName = "antares";
    networkmanager.enable = true;
  };

  # Select internationalisation properties.
  i18n = {
    consoleFont = "ter-132n";
    consoleColors = [
      "161616" "a65353" "909653" "bd9c5a" "5f788c" "816b87" "688c88" "c5c5c5"
      "4a4a4a" "cc6666" "b5bd68" "f0c674" "81a2be" "b294bb" "8abeb7" "f7f7f7"
    ];
    consoleKeyMap = "us";
    consolePackages = with pkgs; [ terminus_font ];
    defaultLocale = "en_US.UTF-8";
  };

  # Set your time zone.
  time.timeZone = "America/Chicago";

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    vim
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.bash.enableCompletion = true;
  # programs.mtr.enable = true;
  # programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

  # Enable browserpass host application for Chromium.
  programs.browserpass.enable = true;

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable cron daemon.
  services.cron.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;
    layout = "us";
    xkbOptions = "shift:both_capslock_cancel,caps:ctrl_modifier,lv3:ralt_alt,compose:102";
    xkbVariant = "workman";
    libinput.enable = true;
    desktopManager.gnome3.enable = true;
    displayManager.gdm = {
      enable = true;
      autoLogin.enable = true;
      autoLogin.user = "ajgrf";
    };
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraUsers.ajgrf = {
    description = "Alex Griffin";
    extraGroups = [ "wheel" "networkmanager" ];
    isNormalUser = true;
    uid = 1000;
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "18.09"; # Did you read the comment?

}
