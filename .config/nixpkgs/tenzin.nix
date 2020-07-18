{ config, lib, pkgs, ... }:

{
  imports =
    [ <nixpkgs/nixos/modules/installer/scan/not-detected.nix>
    ];

  boot.initrd.availableKernelModules = [ "xhci_pci" "dwc3_pci" "ahci" "usb_storage" "usbhid" "sd_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/096a18ff-de0c-4e25-b7a5-32902863574b";
      fsType = "ext4";
    };

  fileSystems."/home" =
    { device = "/dev/disk/by-uuid/648bd2d2-dba4-48d9-acbf-9b069662ee27";
      fsType = "ext4";
    };

  boot.initrd.luks.devices."home".device = "/dev/disk/by-uuid/5abba48a-e3e2-4114-8dfc-d97f2a5ba9ac";

  swapDevices = [
    {
      device = "/dev/disk/by-partuuid/ec0b576e-b8d4-4599-bd1b-6d4b74ed5d73";
      randomEncryption.enable = true;
    }
  ];

  nix.maxJobs = lib.mkDefault 4;
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";

  boot.loader = {
    timeout = 1;
    grub = {
      enable = true;
      device = "/dev/sda";
      gfxmodeBios = "1280x720";
      extraEntries = ''
        menuentry "Debian GNU/Linux" {
          insmod part_gpt
          insmod ext2
          set root='hd0,gpt3'
          configfile /grub/grub.cfg
        }
      '';
    };
  };

  networking = {
    hostName = "tenzin";
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

  # Enable browserpass host application for Chromium.
  programs.browserpass.enable = true;

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
    layout = "us";
    libinput.enable = true;
    desktopManager.gnome3.enable = true;
    displayManager.gdm = {
      enable = true;
      autoLogin.enable = true;
      autoLogin.user = "ajgrf";
    };
  };

  # Disable GNOME Keyring.
  services.gnome3.gnome-keyring.enable = pkgs.lib.mkForce false;

  # Enable libvirt daemon.
  virtualisation.libvirtd.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.ajgrf = {
    description = "Alex Griffin";
    isNormalUser = true;
    extraGroups = [ "wheel" "libvirtd" "networkmanager" ];
    uid = 1000;
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.03"; # Did you read the comment?

}
