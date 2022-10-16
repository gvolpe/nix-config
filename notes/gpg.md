# Renew expired key 

List the available keys.

```console
$ gpg --list-keys
-------------------------------
pub   rsa3072/0x121D4302A64B2261 2020-10-12 [SC] [expired: 2022-10-12]
      Key fingerprint = 2DE4 E864 E515 16D7 9121  CF11 121D 4302 A64B 2261
uid                   [ expired] Gabriel Volpe <volpegabriel@gmail.com>
```

Edit the key.

```console
$ gpg --edit-key 2DE4E864E51516D79121CF11121D4302A64B2261
Secret key is available.

sec  rsa3072/0x121D4302A64B2261
     created: 2020-10-12  expired: 2022-10-12  usage: SC
     trust: unknown       validity: expired
ssb  rsa3072/0xC2C299648D89952E
     created: 2020-10-12  expired: 2022-10-12  usage: E
[ expired] (1). Gabriel Volpe <volpegabriel@gmail.com>
```

Update its expiration (type `key 1` to also update the subkey `ssb`).

```console
gpg> expire
Changing expiration time for a subkey.
Please specify how long the key should be valid.
         0 = key does not expire
      <n>  = key expires in n days
      <n>w = key expires in n weeks
      <n>m = key expires in n months
      <n>y = key expires in n years
Key is valid for? (0) 8y
Key expires at Mon 14 Oct 2030 12:16:17 PM CEST
Is this correct? (y/N) y

gpg> key 1
gpg> expire # same procedure here with ssb
gpg> save
```

Source: https://unix.stackexchange.com/questions/552707/how-to-renew-an-expired-encryption-subkey-with-gpg
