sandfix
=======

sandfix fixes a sandbox created on another machine by adjusting the global paths and the package dependencies in the sandbox's package database. It has only very basic dependencies.

There are a couple of requirements your machine still needs to satisfy for the sandbox to work:

* You should have the same version of ghc and the same platform as the machine the sandbox was created on.

* The set of global packages on the machine the sandbox was created on should be a subset of the set of your global packages.

The second requirement is pretty weird, it is caused by the fact that all sandboxes depend transitively on packages outside of the sandbox, in the global DB. An example is base or array.

This can be a problem for example if the sandbox is created on a box with the Haskell Platform which preinstalls all kinds of junk to the global package DB, and later you try to use it on a machine that only has a basic ghc installation. This will break, as the sandbox will assume that all those packages are in your global DB. The solution is to create sandboxes with a minimal ghc installation. Alternatively you can also install those packages in your global DB, sandfix will tell you about these.

Usage
=====

```
$ runhaskell src/SandFix.hs PATH_TO_SANDBOX
```
