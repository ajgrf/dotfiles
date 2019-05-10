;; Based on the simple stateful firewall from the Arch Linux wiki:
;; https://wiki.archlinux.org/index.php/Simple_stateful_firewall
(define* (simple-firewall #:key (open-tcp-ports '()) (open-udp-ports '()))
  "Return an iptables-configuration for a simple stateful firewall, suitable
for a laptop, with just the specified ports open."

  (define (allow-tcp-port-rule port)
    (string-append "-A TCP -p tcp --dport "
                   (number->string port)
                   " -j ACCEPT\n"))

  (define (allow-udp-port-rule port)
    (string-append "-A UDP -p udp --dport "
                   (number->string port)
                   " -j ACCEPT\n"))

  (define simple-ipv4-rules
    (plain-file "simple-firewall-ipv4.rules"
                (string-concatenate
                 (append '("*filter
:INPUT DROP
:FORWARD DROP
:OUTPUT ACCEPT
:TCP -
:UDP -
# Accept already-established connections.
-A INPUT -m conntrack --ctstate RELATED,ESTABLISHED -j ACCEPT
# Accept everything from the loopback device.
-A INPUT -i lo -j ACCEPT
# Drop invalid packets.
-A INPUT -m conntrack --ctstate INVALID -j DROP
# Accept ICMP echo replies.
-A INPUT -p icmp -m icmp --icmp-type 8 -m conntrack --ctstate NEW -j ACCEPT
# Send UDP traffic through the UDP chain.
-A INPUT -p udp -m conntrack --ctstate NEW -j UDP
# Send TCP traffic through the TCP chain.
-A INPUT -p tcp --syn -m conntrack --ctstate NEW -j TCP
# Reject UDP traffic that wasn't accepted in the UDP chain.
-A INPUT -p udp -j REJECT --reject-with icmp-port-unreachable
# Reject TCP traffic that wasn't accepted in the TCP chain.
-A INPUT -p tcp -j REJECT --reject-with tcp-reset
# Reject any traffic that was overlooked.
-A INPUT -j REJECT --reject-with icmp-port-unreachable\n")
                         (map allow-udp-port-rule open-udp-ports)
                         (map allow-tcp-port-rule open-tcp-ports)
                         '("COMMIT\n")))))

  (define simple-ipv6-rules
    (plain-file "simple-firewall-ipv6.rules"
                (string-concatenate
                 (append '("*filter
:INPUT DROP
:FORWARD DROP
:OUTPUT ACCEPT
:TCP -
:UDP -
# Accept already-established connections.
-A INPUT -m conntrack --ctstate RELATED,ESTABLISHED -j ACCEPT
# Accept everything from the loopback device.
-A INPUT -i lo -j ACCEPT
# Drop invalid packets.
-A INPUT -m conntrack --ctstate INVALID -j DROP
# Accept NDP traffic.
-A INPUT -p ipv6-icmp -s fe80::/10 -j ACCEPT
# Accept ICMPv6 echo replies.
-A INPUT -p ipv6-icmp --icmpv6-type 128 -m conntrack --ctstate NEW -j ACCEPT
# Send UDP traffic through the UDP chain.
-A INPUT -p udp -m conntrack --ctstate NEW -j UDP
# Send TCP traffic through the TCP chain.
-A INPUT -p tcp --syn -m conntrack --ctstate NEW -j TCP
# Reject UDP traffic that wasn't accepted in the UDP chain.
-A INPUT -p udp -j REJECT --reject-with icmp6-adm-prohibited
# Reject TCP traffic that wasn't accepted in the TCP chain.
-A INPUT -p tcp -j REJECT --reject-with tcp-reset
# Reject any traffic that was overlooked.
-A INPUT -j REJECT --reject-with icmp6-adm-prohibited\n")
                         (map allow-udp-port-rule open-udp-ports)
                         (map allow-tcp-port-rule open-tcp-ports)
                         '("COMMIT\n")))))

  (iptables-configuration
   (ipv4-rules simple-ipv4-rules)
   (ipv6-rules simple-ipv6-rules)))
