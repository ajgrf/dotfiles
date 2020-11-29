# This file contains options specific to my Purism Librem 13v4 installation.
{ config, lib, pkgs, ... }:

{
  imports =
    [ # Include common options I want on all my desktop machines.
      ./desktop.nix
      <nixpkgs/nixos/modules/installer/scan/not-detected.nix>
    ];

  # Bootloader settings.
  boot.loader = {
    timeout = 2;
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

  # Named after the character in The Legend of Korra.
  networking.hostName = "tenzin";

  # Edited results of the hardware scan (nixos-generate-config):

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

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.03"; # Did you read the comment?

}
