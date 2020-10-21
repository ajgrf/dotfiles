# This file contains common options I want on all my desktop machines.
{ config, pkgs, ... }:

{
  networking = {
    networkmanager.enable = true;

    # Open ports in the firewall.
    firewall = {
      # GSConnect
      allowedTCPPortRanges = [ { from = 1716; to = 1764; } ];
      allowedUDPPortRanges = [ { from = 1716; to = 1764; } ];
    };
  };

  # Set your time zone.
  time.timeZone = "US/Central";

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    gnomeExtensions.gsconnect
    vim
  ];

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # 32-bit graphics & sound support for games.
  hardware.opengl.driSupport32Bit = true;
  hardware.opengl.extraPackages32 = with pkgs.pkgsi686Linux; [ libva ];
  hardware.pulseaudio.support32Bit = true;

  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;
    libinput.enable = true;

    layout = "us";
    xkbOptions = builtins.concatStringsSep "," [
      "caps:ctrl_modifier"
      "lv3:ralt_alt"
      "compose:menu"
      "shift:both_capslock_cancel"
    ];

    desktopManager.gnome3.enable = true;
    displayManager = {
      gdm.enable = true;
      autoLogin.enable = true;
      autoLogin.user = "ajgrf";
    };
  };

  # Disable GNOME Keyring.
  services.gnome3.gnome-keyring.enable = pkgs.lib.mkForce false;

  # Enable GnuPG agent with socket-activation for every user session.
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
    pinentryFlavor = "gtk2";
  };

  # Enable libvirt daemon.
  virtualisation.libvirtd.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.ajgrf = {
    description = "Alex Griffin";
    isNormalUser = true;
    extraGroups = [ "wheel" "libvirtd" "networkmanager" ];
    uid = 1000;
  };

}
