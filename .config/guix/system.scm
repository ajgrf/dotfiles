(use-modules (gnu)
             (gnu packages cups)
             (gnu packages linux)
             (gnu packages xorg)
             (nongnu packages linux))
(use-service-modules cups desktop networking pm security-token shepherd ssh xorg)
(load "simple-firewall.scm")

(operating-system
  (kernel linux)
  (firmware (cons* ath3k-firmware
                   %base-firmware))
  (locale "en_US.utf8")
  (timezone "America/Chicago")
  (keyboard-layout
   (keyboard-layout "us" "workman"
                    #:options '("caps:ctrl_modifier" "lv3:ralt_alt"
                                "compose:menu" "shift:both_capslock_cancel")))
  (bootloader
   (bootloader-configuration
    (bootloader grub-bootloader)
    (target "/dev/sda")
    (timeout 2)
    (keyboard-layout keyboard-layout)
    (menu-entries
     (list (menu-entry
            (label "Debian 10 (buster)")
            (device "debboot")
            (linux "(hd0,gpt3)/vmlinuz-4.19.0-5-amd64")
            (linux-arguments
             '("root=UUID=227c5e05-6dff-4802-9537-688e20892cf6"
               "ro" "quiet" "splash"))
            (initrd "(hd0,gpt3)/initrd.img-4.19.0-5-amd64"))
           (menu-entry
            (label "PureOS")
            (device "pureosboot")
            (linux "(hd0,gpt5)/vmlinuz-4.19.0-5-amd64")
            (linux-arguments
             '("root=UUID=b315dea0-efc1-48ea-9bb4-f1c3aa7e2ce5"
               "ro" "quiet" "splash"))
            (initrd "(hd0,gpt5)/initrd.img-4.19.0-5-amd64"))))))
  (mapped-devices
   (list (mapped-device
          (source (uuid "5abba48a-e3e2-4114-8dfc-d97f2a5ba9ac"))
          (target "home")
          (type luks-device-mapping))))
  (file-systems
   (cons* (file-system
            (mount-point "/")
            (device
             (uuid "1f1bdd00-3aa2-476f-8b5d-4a8200737eb9"
                   'ext4))
            (type "ext4"))
          (file-system
            (mount-point "/home")
            (device "/dev/mapper/home")
            (type "ext4"))
          %base-file-systems))
  (host-name "tenzin")
  (users (cons* (user-account
                 (name "ajgrf")
                 (comment "Alex Griffin")
                 (group "ajgrf")
                 (home-directory "/home/ajgrf")
                 (supplementary-groups
                  '("wheel" "netdev" "audio" "video" "lp")))
                %base-user-accounts))
  (groups (cons* (user-group
                  (name "ajgrf")
                  (id 1000))
                 %base-groups))
  (packages
   (cons* (specification->package "nss-certs")
          %base-packages))
  (services
   (cons* (service gnome-desktop-service-type)
          (service mate-desktop-service-type)
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
                   (simple-firewall #:open-tcp-ports '(8376 29254)
                                    #:open-udp-ports '(1900)))
          (service
           (shepherd-service-type
            'fix-librem-kbd
            (lambda _
              (shepherd-service
               (documentation "Fix backslash/pipe key on Librem laptops.")
               (provision '(fix-librem-kbd))
               (respawn? #f)
               (start #~(lambda _
                          (zero? (system* #$(file-append kbd "/bin/setkeycodes")
                                          "56" "43"))))))
            #f))
          %desktop-services)))
