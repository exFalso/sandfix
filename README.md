sandfix
=======

sandfix fixes a cabal sandbox created on another machine by adjusting the global paths and the package dependencies in the sandbox's package database. It has only very basic dependencies, and assumes a ghc installation.

We'll call the machine the sandbox was created on the *host*, the machine you want to port it to the *target*.

There are a couple of requirements your target machine needs to satisfy for the sandbox to work:

* It should have the same version of ghc and the same architecture as the host. (check with ```ghc --info | grep 'Target platform'```)

* The set of global packages on the host should be a subset of the target's global packages. (check with ```ghc-pkg list --global```)

The second requirement is pretty weird, it is caused by the fact that all sandboxes depend transitively on packages outside of the sandbox, in the global DB. An example is **base** or **array**.

This can be a problem for example if the sandbox is created on a box with the Haskell Platform which preinstalls all kinds of junk to the global package DB, and later you try to use it on a machine that only has a basic ghc installation. This will break, as the sandbox will assume that all those packages are in your global DB. The solution is to create sandboxes with a minimal ghc installation. Alternatively you can also install those packages in your global DB, sandfix will tell you about these.

Usage
=====

For _cabal sandboxes_:
```
$ runhaskell src/SandFix.hs PATH_TO_SANDBOX [--package-db=...]
```

or with custom package databases:
```
$ runhaskell src/SandFix.hs PATH_TO_SANDBOX PKGDIR_NAME [--package-db=...]
```
where `PKGDIR_NAME` is the relative path of the package database under `PATH_SANDBOX`.

Package database stack can be specified in the same way as for `cabal`. The default is equivalent to `--package-db=global`.

To turn on more verbose output, use `-v` or `--verbose`.

Troubleshooting
=====

* If you ```cabal add-source``` a package to the sandbox on the host watch out for this message when you use cabal functions:
  ```
  Warning: The package list for the local repo 'PATH_TO_SANDBOX' is missing.
  The repo is invalid.
  ```
  Solution: Delete ```PATH_TO_SANDBOX/packages/00-index.tar``` and reinitialize the sandbox with ```cabal sandbox init```.
  
  Reason: For some reason cabal "encodes" the global path of the add-source'd package into the tar, which it later tries to open.
  
  If you want to dig in:
  
  The global path writing: https://github.com/haskell/cabal/blob/5c70361b362e41b3c13d48b58b46224d42f401dc/cabal-install/Distribution/Client/Sandbox/Index.hs#L92
  
  The "encoding" function: https://github.com/haskell/cabal/blob/5c70361b362e41b3c13d48b58b46224d42f401dc/cabal-install/Distribution/Client/Utils.hs#L130 Yeah who needs UTF when we can invent our own, *better* encoding.
  
  The code that actually breaks: https://github.com/haskell/cabal/blob/5c70361b362e41b3c13d48b58b46224d42f401dc/cabal-install/Distribution/Client/IndexUtils.hs#L458
