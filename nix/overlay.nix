final: prev:
with final.lib;
with final.haskell.lib;

let
  enableStatic = pkg:
    if final.stdenv.hostPlatform.isMusl
    then
      overrideCabal pkg
        (old:
          let
            # Until https://github.com/NixOS/nixpkgs/pull/311411
            terminfoDirs = final.lib.concatStringsSep ":" [
              "/etc/terminfo" # Debian, Fedora, Gentoo
              "/lib/terminfo" # Debian
              "/usr/share/terminfo" # upstream default, probably all FHS-based distros
              "/run/current-system/sw/share/terminfo" # NixOS
            ];
            staticNcurses = (
              (final.ncurses.override {
                enableStatic = true;
              })
            ).overrideAttrs
              (old: {
                configureFlags = (old.configureFlags or [ ]) ++ [
                  "--with-terminfo-dirs=${terminfoDirs}"
                ];
              });
          in
          {
            configureFlags = (old.configureFlags or [ ]) ++ [
              "--ghc-option=-optl=-static"
              # Static
              "--extra-lib-dirs=${final.gmp6.override { withStatic = true;}}/lib"
              "--extra-lib-dirs=${final.zlib.static}/lib"
              "--extra-lib-dirs=${staticNcurses}/lib"
              "--extra-lib-dirs=${final.libffi.overrideAttrs (old: { dontDisableStatic = true;})}/lib"
            ];
            enableSharedExecutables = false;
            enableSharedLibraries = false;

            postInstall = (old.postInstall or "") + ''
              for b in $out/bin/*
              do
                if ldd "$b"
                then
                  echo "ldd succeeded on $b, which may mean that it is not statically linked"
                  exit 1
                fi
              done
            '';
          })
    else pkg;
  fixGHC = pkg:
    if final.stdenv.hostPlatform.isMusl
    then
      pkg.override
        {
          # To make sure that executables that need template
          # haskell can be linked statically.
          enableRelocatedStaticLibs = true;
          enableShared = false;
          enableDwarf = false;
          # Until https://github.com/NixOS/nixpkgs/pull/317175
          srcOnly = args: final.srcOnly (args // {
            patches = (args.patches or [ ]) ++ [
              (final.fetchpatch {
                url = "https://gitlab.haskell.org/ghc/ghc/-/commit/1bb24432ff77e11a0340a7d8586e151e15bba2a1.diff";
                hash = "sha256-MpvTmFFsNiPDoOp9BhZyWeapeibQ77zgEV+xzZ1UAXs=";
              })
            ];
          });
        }
    else pkg;
in
{
  tpa = justStaticExecutables (enableStatic final.haskellPackages.tpa);
  haskellPackages = prev.haskellPackages.override (old: {
    overrides = composeExtensions (old.overrides or (_: _: { })) (
      self: super: {
        ghc = fixGHC super.ghc;
        buildHaskellPackages = old.buildHaskellPackages.override (oldBuildHaskellPackages: {
          ghc = fixGHC oldBuildHaskellPackages.ghc;
        });
        "tpa" = self.opt-env-conf.installManpagesAndCompletions [ "tpa" ] (buildStrictly (self.callPackage ../tpa { }));
      }
    );
  });
}
