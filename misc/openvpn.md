nmcli connection import type openvpn file foo.ovpn
nmcli connection modify packet-ewr1 ipv4.never-default yes
nmcli connection modify packet-ewr1 +vpn.data username=me@mail.com
