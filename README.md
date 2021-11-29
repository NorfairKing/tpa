# TPA

Minimalist declarative command-line Third Party Authenticator 

## Setting it up

1. Put your keys in files:

```
- name: test key
  key: AAAAAAAAAAAAAAAA # Base32
```

2. Refer to the key files in a config file:

```
key-paths:
- /path/to/my/key/file
```

3. Run `tpa` or `tpa <keyname>`.

## Setting it up with NixOS Home Manager

1. Import `./nix/home-manager-module`.
2. Configure in you home manager configuration:
   
   ``` plain
   tpa = {
     enable = true;
     paths = [
       /path/to/key/files
     ];
   };
   ```
