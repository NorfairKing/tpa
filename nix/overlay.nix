final: prev:
with final.lib;
with final.haskell.lib;
{
  tpa = justStaticExecutables final.haskellPackages.tpa;
  haskellPackages = prev.haskellPackages.override (old: {
    overrides = composeExtensions (old.overrides or (_: _: { })) (
      self: super:
        {
          "tpa" = generateOptparseApplicativeCompletion "tpa" (buildStrictly (self.callPackage ../tpa { }));
        }
    );
  }
  );
}
