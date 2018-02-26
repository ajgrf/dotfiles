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

  networking = {
    hostName = "antares";
    networkmanager.enable = true;
    firewall = {
      allowedTCPPortRanges = [ { from = 1714; to = 1764; } ];
      allowedUDPPortRanges = [ { from = 1714; to = 1764; } ];
    };
  };

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
    kdeconnect
    kdeApplications.okular
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

  hardware.bluetooth.enable = true;
  hardware.pulseaudio.enable = true;

  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;
    # layout = "us";
    xkbOptions = "shift:both_capslock_cancel,caps:ctrl_modifier,lv3:ralt_alt,compose:102";
    synaptics.enable = true;
    desktopManager.plasma5.enable = true;
    displayManager.sddm = {
      enable = true;
      autoLogin = {
        enable = true;
        user = "ajgrf";
      };
    };
  };

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
