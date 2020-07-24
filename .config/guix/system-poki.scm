(use-modules (gnu)
             (gnu packages cups)
             (gnu packages gnome)
             (gnu packages linux)
             (gnu packages xorg)
             (guix packages)
             (guix utils)
             (nongnu packages linux)
             (nongnu system linux-initrd)
             (ice-9 match)
             (srfi srfi-1))             ; iota
(use-service-modules cups desktop linux networking pm security-token ssh
                     virtualization xorg)
(load "simple-firewall.scm")

;; Disable SSH Agent functionality of GNOME Keyring in favor of GPG Agent
;; https://guix.gnu.org/blog/2018/customize-guixsd-use-stock-ssh-agent-everywhere/

(define gnome-keyring-sans-ssh-agent
  (package
   (inherit gnome-keyring)
   (name "gnome-keyring-sans-ssh-agent")
   (arguments
    (substitute-keyword-arguments
     (package-arguments gnome-keyring)
     ((#:configure-flags flags)
      `(cons "--disable-ssh-agent" ,flags))))))

(define gnome-sans-ssh-agent
  (package
   (inherit gnome)
   (name "gnome-sans-ssh-agent")
   (propagated-inputs
    (map (match-lambda
           ((name package)
            (if (equal? name "gnome-keyring")
                (list name gnome-keyring-sans-ssh-agent)
                (list name package))))
         (package-propagated-inputs gnome)))))

(operating-system
 (kernel linux)
 (kernel-loadable-modules (list ddcci-driver-linux))
 (initrd microcode-initrd)
 (firmware (cons* iwlwifi-firmware
                  %base-firmware))
 (locale "en_US.utf8")
 (timezone "America/Chicago")
 (keyboard-layout
  (keyboard-layout "us"
                   #:options '("caps:ctrl_modifier" "lv3:ralt_alt"
                               "compose:menu" "shift:both_capslock_cancel")))
 ;; Use the UEFI variant of GRUB with the EFI System
 ;; Partition mounted on /boot/efi.
 (bootloader (bootloader-configuration
              (bootloader grub-efi-bootloader)
              (target "/boot/efi")
              (timeout 2)
              (keyboard-layout keyboard-layout)))
 ;; Specify a mapped device for the encrypted root partition.
 ;; The UUID is that returned by 'cryptsetup luksUUID'.
 (mapped-devices
  (list (mapped-device
         (source (uuid "881c583f-599c-4185-970d-e81875c400ce"))
         (target "cryptroot")
         (type luks-device-mapping))
        (mapped-device
         (source (uuid "062ee8be-0112-4885-b61e-d458e2dfebed"))
         (target "crypthome")
         (type luks-device-mapping))))

 (file-systems (append
                (list (file-system
                       (device "/dev/mapper/cryptroot")
                       (mount-point "/")
                       (type "ext4")
                       (dependencies mapped-devices))
                      (file-system
                       (device "/dev/mapper/crypthome")
                       (mount-point "/home")
                       (type "xfs")
                       (dependencies mapped-devices))
                      (file-system
                       (device (uuid "D282-B1CE" 'fat))
                       (mount-point "/boot/efi")
                       (type "vfat")))
                %base-file-systems))
 (host-name "poki")
 (users (cons* (user-account
                (name "ajgrf")
                (comment "Alex Griffin")
                (uid 1000)
                (group "ajgrf")
                (home-directory "/home/ajgrf")
                (supplementary-groups
                 '("wheel" "netdev" "audio" "video" "lp" "libvirt")))
               %base-user-accounts))
 (groups (cons* (user-group
                 (name "ajgrf")
                 (id 1000))
                %base-groups))
 (packages
  (append (map specification->package
               '("nss-certs"
                 "flatpak"
                 "font-dejavu"
                 "gnome-shell-extension-gsconnect"
                 "gnome-shell-extension-paperwm"
                 "gvfs"))
          %base-packages))
 (services
  (cons* (service kernel-module-loader-service-type
                  '("ddcci" "ddcci_backlight"))
         (service gnome-desktop-service-type
                  (gnome-desktop-configuration
                   (gnome gnome-sans-ssh-agent)))
         (set-xorg-configuration
          (xorg-configuration
           (keyboard-layout keyboard-layout)
           (modules (filter (lambda (mod)
                              (not (eq? mod xf86-input-synaptics)))
                            %default-xorg-modules))))
         (service cups-service-type
                  (cups-configuration
                   (web-interface? #t)
                   (extensions (list hplip-minimal))))
         (service pcscd-service-type)
         (service tlp-service-type
                  (tlp-configuration
                   (cpu-scaling-governor-on-ac '("performance"))
                   (cpu-scaling-governor-on-bat '("powersave"))
                   (usb-autosuspend? #f)))
         (bluetooth-service)
         (service iptables-service-type
                  (simple-firewall #:open-tcp-ports (cons* 8376 29254
                                                           (iota 51 1714))
                                   #:open-udp-ports (cons* 1900
                                                           (iota 51 1714))))
         (service libvirt-service-type
                  (libvirt-configuration
                   (unix-sock-group "libvirt")))
         (service virtlog-service-type)
         (service qemu-binfmt-service-type
                  (qemu-binfmt-configuration
                   (guix-support? #t)
                   (platforms (lookup-qemu-platforms "arm" "aarch64"))))
         ;; (modify-services %desktop-services
         ;;   (gdm-service-type
         ;;    config =>
         ;;    (gdm-configuration
         ;;     (inherit config)
         ;;     (auto-login? #t)
         ;;     (default-user "ajgrf")
         ;;     (debug? #t))))
         %desktop-services))

 ;; Allow resolution of '.local' host names with mDNS.
 (name-service-switch %mdns-host-lookup-nss))
