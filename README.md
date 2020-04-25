# hnixbot

hnixbot is an IRC bot supporting Nix-related functionality.
It's using a RabbitMQ broker maintained by @grahamc for
sending messages via the `{^_^}` nick on freenode
(and therefore can't run on its own currently).

hnixbot is based on [nixbot](https://github.com/Infinisil/nixbot)
by @Infinisil.

## Current functionality

### Cache queries

```
<sorki> #cached /nix/store/x5m45fcnky99r0k41kmdwmjb7zw5k4z4-binutils-2.31.1
<{^_^}> Cached, size is 4.90 MB
```

```
<sorki> #nar /nix/store/x5m45fcnky99r0k41kmdwmjb7zw5k4z4-binutils-2.31.1
<{^_^}> URL: https://cache.nixos.org/nar/1a6lzf28ld2g1d4rizbazppd9w9xj04yjqjfli4gs0dbp0mymjjg.nar.xz
```

