{
  description = "Purebred development environment";

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    # haskell.nix is picky about nixpkgs; let it pin a compatible one.
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };

    haskeline = {
      url = "github:judah/haskeline";
      flake = false;
    };

    purebred-icu = {
      url = "github:purebred-mua/purebred-icu";
      flake = false;
    };
  };

  nixConfig = {
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
  };

  outputs = { self, nixpkgs, utils, flake-compat, haskellNix, haskeline, purebred-icu }:
  utils.lib.eachSystem ["x86_64-linux"] (system:
  let
    overlays = [
      haskellNix.overlay
      (final: _prev: {
        purebredProject = final.haskell-nix.cabalProject' {
          src = ./.;
          compiler-nix-name = "ghc967";

          # Add the external purebred-icu package to the cabal plan.
          cabalProjectLocal = ''
            packages: ${purebred-icu}
            packages: ${haskeline}
          '';
        };
      })
    ];

    pkgs = import nixpkgs {
      inherit system overlays;
      inherit (haskellNix) config;
    };

    project = pkgs.purebredProject;
    flake = project.flake { };

    purebred-exe = project.hsPkgs.purebred.components.exes.purebred;

    makePurebredWithPackages = withIcu:
    let
      ghc = project.ghcWithPackages
      (ps: [ ps.purebred ] ++ pkgs.lib.optional withIcu ps.purebred-icu);
    in
    pkgs.runCommandLocal
    "purebred-with-packages${pkgs.lib.optionalString withIcu "-icu"}"
    {
      nativeBuildInputs = [ pkgs.makeWrapper ];
      meta.mainProgram = "purebred";
    } ''
      mkdir -p $out/bin
      makeWrapper ${purebred-exe}/bin/purebred $out/bin/purebred \
      --set NIX_GHC "${ghc}/bin/ghc"
    '';

    uatRunner = pkgs.writeShellApplication {
      name = "purebred-uat";
      runtimeInputs = [
        (makePurebredWithPackages false)   # wrapped `purebred` on PATH (NIX_GHC set for dyre)
        pkgs.tmux
        pkgs.notmuch
        pkgs.elinks
      ];
      text = ''
        export TERM=''${TERM:-xterm-256color}
        exec ${project.hsPkgs.purebred.components.tests.uat}/bin/uat "$@"
      '';
    };

    mkShell = packages: project.shellFor {
      inherit packages;
      withHoogle = true;
      # These are built with the project's compiler/index-state.
      tools = {
        cabal = "latest";
        ghcid = "latest";
        hlint = "latest";
        haskell-ci = {
          src = pkgs.fetchgit {
            url = "https://github.com/haskell-CI/haskell-ci";
            rev = "355b9a90e0ecdaba31c72ddc1b0e5de7a59bb7a4";
            sha256 = "sha256-P+jDHIaIX6n3mVfvY1SzTQsLYy3eJH0Hq1f5tKfEnV8=";
          };
          version = "0.19.20250821";
        };
        haskell-language-server = "latest";
        ormolu = "latest";
      };
      buildInputs = with pkgs; [
        notmuch
        tmux
        gnumake
        asciidoctor
        python3Packages.pygments
      ];
    };
  in
  flake // {
    packages = flake.packages // {
      default = self.packages.${system}.purebred-with-packages-icu;
      purebred-with-packages = makePurebredWithPackages false;
      purebred-with-packages-icu = makePurebredWithPackages true;

      purebred = purebred-exe;
      purebred-icu = project.hsPkgs.purebred-icu.components.library;
      purebred-email = project.hsPkgs.purebred-email.components.library;
      dyre = project.hsPkgs.dyre.components.library;
      brick = project.hsPkgs.brick.components.library;
    };
    # nix run use the wrapped binary
    apps = (flake.apps or { }) // {
      default = {
        type = "app";
        program = "${self.packages.${system}.purebred-with-packages-icu}/bin/purebred";
      };
      uat = { type = "app"; program = "${uatRunner}/bin/purebred-uat"; };
    };

    devShells = {
      default = mkShell (ps: [ ps.purebred ]);
      with-icu = mkShell (ps: [ ps.purebred ps.purebred-icu ]);
    };
  });
}
