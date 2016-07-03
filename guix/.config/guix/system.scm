(use-modules (gnu)
             (gnu services)
             (gnu system nss))
(use-service-modules desktop xorg)
(use-package-modules certs gnome linux)

(operating-system
  (host-name "polaris")
  (timezone "America/Chicago")
  (locale "en_US.UTF-8")

  (kernel linux-libre-4.1)
  (kernel-arguments '("modprobe.blacklist=kvm,kvm_intel"))

  (bootloader (grub-configuration (device #f)
                                  (timeout 2)))
  (mapped-devices (list (mapped-device
                         (source "/dev/sda1")
                         (target "root")
                         (type luks-device-mapping))))
  (file-systems (cons (file-system
                        (device "/dev/mapper/root")
                        (title 'device)
                        (mount-point "/")
                        (type "ext4"))
                      %base-file-systems))

  (users (cons (user-account
                (name "ajgrf")
                (comment "Alex Griffin")
                (group "users")
                (supplementary-groups '("wheel" "netdev"
                                        "audio" "video" "lp"))
                (home-directory "/home/ajgrf"))
               %base-user-accounts))

  (packages (cons* nss-certs         ;for HTTPS access
                   %base-packages))

  ;; Use the "desktop" services, which include the X11
  ;; log-in service, networking with Wicd, and more.
  (services (cons* (gnome-desktop-service)
                   (console-keymap-service "en-latin9")
                   (bluetooth-service)
                   (modify-services %desktop-services
                     (slim-service-type config =>
                                        (slim-configuration
                                         (inherit config)
                                         (auto-login? #t)
                                         (default-user "ajgrf")
                                         (auto-login-session
                                          #~(string-append #$gnome-session
                                                           "/bin/gnome-session "
                                                           "--disable-acceleration-check")))))))

  ;; Allow resolution of '.local' host names with mDNS.
  (name-service-switch %mdns-host-lookup-nss))
