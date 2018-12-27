# purebred

[![Build Status](https://travis-ci.org/purebred-mua/purebred.svg?branch=master)](https://travis-ci.org/purebred-mua/purebred)

An MUA built around [*notmuch*](https://notmuchmail.org/).

## requirements

- GHC >= 8.0
- notmuch
- a local mailer (e.g. ``sendmail``)

## Status

This project is in its infancy, but please join us and help.  All
kinds of contributions (bug reports, testing, documentation, code)
are welcome.  See [HACKING] for more info.

![](screenshot.png)

## Goals

- [ ] re-use strong UX patterns from existing console mailers like mutt and sup
- [ ] extensible with plug-ins
- [x] enforce sane configuration by using Haskell's type system

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
