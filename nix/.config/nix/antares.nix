# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      /etc/nixos/hardware-configuration.nix
    ];

  boot.initrd.luks.devices."sda2_crypt".keyFile = "/crypto_keyfile.bin";
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

  networking.hostName = "antares"; # Define your hostname.
  networking.networkmanager.enable = true;

  # Select internationalisation properties.
  i18n = {
    consoleFont = "ter-132n";
    consoleKeyMap = "us";
    consolePackages = with pkgs; [ terminus_font ];
    defaultLocale = "en_US.UTF-8";
  };

  # Set your time zone.
  time.timeZone = "America/Chicago";

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    file
    vim
  ];

  fonts.fonts = with pkgs; [
    go-font
    wqy_microhei
    wqy_zenhei
  ];

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;
    # layout = "us";
    # xkbOptions = "eurosign:e";
    desktopManager.gnome3.enable = true;
    displayManager.gdm = {
      enable = true;
      autoLogin = {
        enable = true;
        user = "ajgrf";
      };
    };
  };

  environment.gnome3.excludePackages = with pkgs; [
    epiphany
    gnome3.accerciser
    gnome3.evolution
    gnome3.gnome-packagekit
    gnome3.gnome-software
    gnome3.totem
  ];

  services.redshift = {
    enable = true;
    latitude = "44.78";
    longitude = "-93.27";
    brightness.day = "1.0";
    brightness.night = "0.7";
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraUsers.ajgrf = {
    description = "Alex Griffin";
    extraGroups = [ "wheel" "networkmanager" ];
    isNormalUser = true;
    shell = pkgs.mksh;
    uid = 1000;
  };

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "17.09";

}
