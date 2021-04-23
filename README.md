# xnand

my toy IRC bot.

xnand is based on [nixbot](https://github.com/Infinisil/nixbot)
by @Infinisil.

Uses [ircbridge](https://github.com/sorki/ircbridge)

## Current functionality

### Decisions..

```
#decide a | b | c
```

### Factoids

```
Set - ?somefact = something
Get - ?somefact or somefact?
Forget - ?somefact-forget
```

### Nix Cache queries

```
<sorki> #cached /nix/store/xdii8qvch5h8chyp0z2is2qzky565w68-binutils-2.35.1
<{^_^}> Cached, size is 4.63 MB decompressed from xz has 30.75 MB references 4 paths
```

```
<sorki> #nar /nix/store/x5m45fcnky99r0k41kmdwmjb7zw5k4z4-binutils-2.31.1
<{^_^}> URL: https://cache.nixos.org/nar/1a6lzf28ld2g1d4rizbazppd9w9xj04yjqjfli4gs0dbp0mymjjg.nar.xz
```

