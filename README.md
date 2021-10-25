<p align="center">
  <img align="middle" src=
  "https://github.com/Horrocubes/horrocubes-signature-validator/blob/main/assets/horrologo_black.png"
  height="250" /></br>
  <sup><sup><sup><sup>The Horrocubes logo is licensed under
  <a href="https://creativecommons.org/licenses/by/3.0/">Creative
  Commons 3.0 Attributions license</a></sup></sup></sup></sup>
</p>
 
<p align="center"><b>Horrocubes Plutus Smart-Contracts</b></p>

This repository contains the Plutus code for the on chain and off chain parts of the Horrocubes game.


### Setting up the environment

If you already have a Haskell development environment set up, feel free to skip this section, otherwise follow along, we will set up a suitable environment for compiling plutus scripts using Nix.

  

We will use Nix to provide both Haskell and Cabal, but if you desire, you could also rely on ghcup to manage these dependencies. However, we won't cover this. You can refer to the official [ghcup](https://gitlab.haskell.org/haskell/ghcup-hs) site for instructions on that.

  

Nix is an amazing tool that, among other things, allows us to create isolated environments in which we can embed all dependencies needed for an application. These dependencies can even be system-level dependencies. Thus, we can create an isolated environment to ensure the application will work since all required dependencies are available.

  
  

Install Nix on any **Linux distribution**, **MacOS** or **Windows** (via WSL) via the recommended [multi-user installation](https://nixos.org/manual/nix/stable/#chap-installation). In short, you need to run this at your terminal:

```
$ sh <(curl -L https://nixos.org/nix/install) --daemon
```

Add IOHK Binary Cache. To improve build speed, it is possible to set up a binary cache maintained by IOHK.

  
```
$ sudo mkdir -p /etc/nix

$ cat <<EOF | sudo tee /etc/nix/nix.conf

substituters = https://cache.nixos.org https://hydra.iohk.io

trusted-public-keys = iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=

EOF
```
  

Before Nix works in your existing shells, you need to close them and open them again. Other than that, you should be ready to go.

Once Nix is installed, log out and then log back in, so it is activated properly in your shell.

Load a shell with Nix using this file with the following command:
```
$ nix-shell ./nix/default.nix
```  

This will take approximately five or ten minutes, then, you should see something similar to this:
```
these paths will be fetched (445.08 MiB download, 5870.53 MiB unpacked):

/nix/store/04jc7s1006vhg3qj4fszg6bcljlyap1a-conduit-parse-0.2.1.0-doc

/nix/store/052kzx9p5fl52pk436i2jcsqkz3ni0r2-reflection-2.1.6-doc
.
.
.
/nix/store/7jq1vjy58nj8rjwa688l5x7dyzr55d9f-monad-memo-0.5.3... (34 KB left)

```

This creates an environment with all dependencies listed in the “buildInputs” section, with GHC 8.10.4 and Cabal among those.


When you have recent versions of GHC and Cabal, make sure to use GHC 8.10.2 or later:

```
[nix-shell:~]$ ghc --version
The Glorious Glasgow Haskell Compilation System, version 8.10.4

[nix-shell:~]$ cabal --version
cabal-install version 3.4.0.0
compiled using version 3.4.0.0 of the Cabal library

```

### Plutus tx: Compiling the script

1. **Compile the project**. 
 
 To compile the script simply run:
 
```
$ cabal update

$ cabal build
```


License
-------

This contents of this repository is released under the terms of the Apache-2.0 license. See [LICENSE](LICENSE) for more information or see https://www.apache.org/licenses/LICENSE-2.0.html.  
