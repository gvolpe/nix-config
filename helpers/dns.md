## DNS workaround

```
sudo ln -sf /run/systemd/resolve/resolv.conf /etc/resolv.conf
```

Add the following DNS entries to `/etc/systemd/resolved.conf`:

```
[Resolve]
DNS=8.8.8.8 2001:4860:4860::8888
FallbackDNS=8.8.4.4 2001:4860:4860::8844
```
