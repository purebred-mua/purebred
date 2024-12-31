let
  haskellCompilerVersion = "ghc967";
  haskellPackagesOverlay = self: super: with super.haskell.lib; {
    haskellPackages = super.haskell.packages.${haskellCompilerVersion}.override {
      overrides = hself: hsuper: {
        purebred = hsuper.callPackage ./purebred.nix { };
        purebred-icu = hsuper.callPackage ./purebred-icu.nix { };
      };
    };
    make-purebred-with-packages = with-icu:
    let
      envPackages = self: if with-icu then [ self.purebred-icu ] else [ self.purebred ];
      env = self.haskellPackages.ghcWithPackages envPackages;
    in self.stdenv.mkDerivation {
      name = "purebred-with-packages-${env.version}";
      nativeBuildInputs = [ self.makeWrapper ];
      # This creates a Bash script, which sets the GHC in order for dyre to be
      # able to build the config file.
      buildCommand = ''
        mkdir -p $out/bin
        makeWrapper ${env}/bin/purebred $out/bin/purebred \
        --set NIX_GHC "${env}/bin/ghc"
      '';
      preferLocalBuild = true;
      allowSubstitutes = false;

      meta.mainProgram = "purebred";
    };
    purebred-with-packages-icu = self.make-purebred-with-packages true;
    purebred-with-packages = self.make-purebred-with-packages false;
    make-purebred-shell = with-icu: let
      nativeBuildTools = with self.haskellPackages; [
        cabal-install
        cabal2nix
        ghcid
        hlint
        haskell-language-server
        ormolu
        hie-bios
        self.notmuch
        self.tmux
        self.gnumake
        self.asciidoctor
        self.python3Packages.pygments
      ];
    in self.haskellPackages.shellFor {
      withHoogle = true;
      packages = hp: [ hp.purebred ] ++ (if with-icu then [hp.purebred-icu] else []);
      nativeBuildInputs = self.haskellPackages.purebred.env.nativeBuildInputs ++ nativeBuildTools;
    };
  };

in [haskellPackagesOverlay]
