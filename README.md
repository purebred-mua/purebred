# purebred

[![Build Status](https://travis-ci.org/purebred-mua/purebred.svg?branch=master)](https://travis-ci.org/purebred-mua/purebred)

An MUA built around [*notmuch*](https://notmuchmail.org/).

## requirements

- GHC >= 8.4
- notmuch
- a local mailer (e.g. ``sendmail``)

## Status

This project is in its infancy, but please join us and help.  All
kinds of contributions (bug reports, testing, documentation, code)
are welcome.  See [HACKING] for more info.

![](screenshot.png)

## Roadmap

### 1.0 - The basics

The first release will deliver a primitive MUA with a primitive UX.
Primitive meaning that most features can be used in anger but are not
recommended for every-day productive use. This release will form the
base upon which we want to build an application which separates itself
from current terminal mail clients.

### Future releases

These releases will focus in more depth on specific issues: e.g. better
UX, better plug-in support, etc.

## Goals

- [ ] re-use strong UX patterns from existing console mailers like mutt and sup
- [ ] extensible with plug-ins
- [x] enforce sane configuration by using Haskell's type system
- [ ] enforce security against malicious emails by using more expressive types
- [x] provide a reliable application using a large suite of user acceptance tests

## Try it

Try out purebred with the following choices. Keep in mind that *none of these* are
*currently tested* in our CI and may be broken.

### Fedora

We operate a
[Fedora Copr](https://copr.fedorainfracloud.org/coprs/romanofski/purebred/)
repository which provides easily installable RPM packages.

### Nix

If you're using the nix package manager - whether on NixOS or any other Linux distribution - you can build purebred too.
You can use `nix-build` to try out purebred without installing it:

```
$ nix-build default.nix
```

and `nix-env` to install the application:

```
$ nix-env --file default.nix --install
```
