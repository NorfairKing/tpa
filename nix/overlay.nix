final: prev:
with final.lib;
with final.haskell.lib;
{
  tpa = justStaticExecutables final.haskellPackages.tpa;
  haskellPackages = prev.haskellPackages.override (old: {
    overrides = composeExtensions (old.overrides or (_: _: { })) (
      self: super: {
        "tpa" = self.generateOptparseApplicativeCompletions [ "tpa" ] (buildStrictly (overrideCabal (self.callPackage ../tpa { }) (old: {
          configureFlags = (old.configureFlags or [ ]) ++ optionals final.stdenv.hostPlatform.isMusl [
            "--ghc-option=-static"
            "--ghc-option=-optl=-static"
            # Static
            "--extra-lib-dirs=${final.gmp6.override { withStatic = true; }}/lib"
            "--extra-lib-dirs=${final.zlib.static}/lib"
            "--extra-lib-dirs=${final.libffi.overrideAttrs (old: { dontDisableStatic = true; })}/lib"
            # for -ltinfo
            "--extra-lib-dirs=${(final.ncurses.override { enableStatic = true; })}/lib"
          ];
          enableSharedExecutables = !final.stdenv.hostPlatform.isMusl;
          enableSharedLibraries = !final.stdenv.hostPlatform.isMusl;
        })));
      }
    );
  });
}
