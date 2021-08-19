git-crypt
=========

All files under `home/secrets/**` are to be encrypted, as defined by the `.gitattributes` file.

```
*.key filter=git-crypt diff=git-crypt
home/secrets/** filter=git-crypt diff=git-crypt
```

On a fresh `git clone`, we need to run `git-crypt unlock` to decrypt the secret files, for which we need to have the private GPG key used for encryption. To verify it works, run the following command.

```shell
$ git-crypt status -e
    encrypted: home/secrets/github-notifications-token
    encrypted: home/secrets/other-stuff
```

The local (unlocked) copy will show the secrets but any new file (or update) added to the `secrets/` directory, committed and pushed to the remote repository will automatically be encrypted.

Learn more about it in this article: https://buddy.works/guides/git-crypt
