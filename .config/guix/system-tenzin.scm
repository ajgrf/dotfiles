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
(use-service-modules cups desktop linux networking nix pm security-token ssh
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
  (firmware (cons* atheros-firmware
                   %base-firmware))
  (locale "en_US.utf8")
  (timezone "America/Chicago")
  (keyboard-layout
   (keyboard-layout "us"
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
            (linux "(hd0,gpt3)/vmlinuz-5.5.0-0.bpo.2-amd64")
            (linux-arguments
             '("root=UUID=e0856f22-b7e9-40e7-abd9-75f2f3c11337"
               "ro" "quiet" "splash"))
            (initrd "(hd0,gpt3)/initrd.img-5.5.0-0.bpo.2-amd64"))))
    (theme (grub-theme
            (inherit %default-theme)
            (gfxmode '("848x480x32"))))))
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
                  '("wheel" "netdev" "audio" "video" "lp" "libvirt")))
                %base-user-accounts))
  (groups (cons* (user-group
                  (name "ajgrf")
                  (id 1000))
                 %base-groups))
  (packages
   (append (map specification->package
                '("nss-certs"
                  "font-dejavu"
                  "gnome-shell-extension-gsconnect"
                  "gnome-shell-extension-paperwm"
                  "nix"))
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
          (service nix-service-type)
          ;; (modify-services %desktop-services
          ;;   (gdm-service-type
          ;;    config =>
          ;;    (gdm-configuration
          ;;     (inherit config)
          ;;     (auto-login? #t)
          ;;     (default-user "ajgrf")
          ;;     (debug? #t))))
          %desktop-services)))
