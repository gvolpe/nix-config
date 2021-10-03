# Scala Microsites & SSH KEYS on CI

Using [ssh-key-action](https://github.com/shimataro/ssh-key-action) we need to set up a new SSH KEY so we can automate the deployment of any GH site. E.g.

```shell
ssh-keygen -t ed25519 -C "neutron-site-bot@profunktor.dev"
```

Next we need to set the public key at `https://github.com/{author}/{repo}/settings/keys`. E.g.

```
ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICtPs7gWYRemDB3FBBKrIZqXj7Rq+MsR6ji50BCnYuUU neutron-site-bot@profunktor.dev
```

Followed by setting the `SSH_KEY` secret on your Github repo with the value of the private key, and the `KNOWN_HOSTS` secret as well (it should be enough to add only Github).

```
github.com,140.82.121.3 ssh-rsa AAAAB3NzaC1yc2EAAAABIwAAAQEAq2A7hRGmdnm9tUDbO9IDSwBK6TbQa+PXYPCPy6rbTrTtw7PHkccKrpp0yVhp5HdEIcKr6pLlVDBfOLX9QUsyCOV0wzfjIJNlGEYsdlLJizHhbn2mUjvSAHQqZETYP81eFzLQNnPHt4EVVUh7VfDESU84KezmD5QlWpXLmvU31/yMf+Se8xhHTvKSCZIFImWwoG6mbUoWf9nzpIoaSjB+weqqUUmpaaasXVal72J+UX2B+2RPW3RcT0eOzQgqlJL3RKrTJvdsjE3JEAvGq3lGHSZXy28G3skua2SmVi/w4yCE6gbODqnTWlg7+wC604ydGXA8VJiS5ap43JXiUFFAaQ==
```
