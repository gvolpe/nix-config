# Secrets management with rage 

Encrypt a secret.

```console 
echo "my-secret" | rage -R ~/.ssh/id_ed25519.pub > secrets/my-secret.age
```

Decrypt it.

```console 
rage -d -i ~/.ssh/id_ed25519 secrets/my-secret.age
```
