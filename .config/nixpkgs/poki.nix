# This file contains options specific to my System76 Lemur Pro installation.
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
    systemd-boot.enable = true;
    efi = {
      efiSysMountPoint = "/boot/efi";
      canTouchEfiVariables = true;
    };
  };

  # Named after Meelo's lemur in The Legend of Korra.
  networking.hostName = "poki";

  # Enable System76 kernel modules and firmware daemon.
  hardware.system76.enableAll = true;

  # Edited results of the hardware scan (nixos-generate-config):

  boot.initrd.availableKernelModules = [ "xhci_pci" "nvme" "usbhid" "usb_storage" "sd_mod" "rtsx_pci_sdmmc" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/c8b62c49-5b6a-454f-aa63-74144ce2b43a";
      fsType = "ext4";
    };

  boot.initrd.luks.devices."cryptroot".device = "/dev/disk/by-uuid/881c583f-599c-4185-970d-e81875c400ce";

  fileSystems."/boot/efi" =
    { device = "/dev/disk/by-uuid/D282-B1CE";
      fsType = "vfat";
    };

  fileSystems."/home" =
    { device = "/dev/disk/by-uuid/d1db40a9-3560-449f-b806-70efa77cb782";
      fsType = "xfs";
    };

  boot.initrd.luks.devices."crypthome".device = "/dev/disk/by-uuid/062ee8be-0112-4885-b61e-d458e2dfebed";

  # Encrypted swap partition with a random key.
  swapDevices = [
    {
      device = "/dev/disk/by-partuuid/2cf3777c-0a29-4a22-848e-264c060eba4c";
      randomEncryption.enable = true;
    }
  ];

  nix.maxJobs = lib.mkDefault 8;
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.09"; # Did you read the comment?

}
