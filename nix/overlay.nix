final: previous:
with final.haskell.lib;

let
  sources = import ./sources.nix;

  tpaPkg =
    name:
    addBuildDepend
      (
        buildStrictly (
          justStaticExecutables (disableLibraryProfiling (final.haskellPackages.callCabal2nixWithOptions name (final.gitignoreSource (../. + "/${name}")) "--no-hpack" { }))
        )
      )
      final.haskellPackages.autoexporter;
  tpaPkgWithComp =
    exeName: name:
    generateOptparseApplicativeCompletion exeName (tpaPkg name);
  tpaPkgWithOwnComp = name: tpaPkgWithComp name name;
in
{
  tpa = tpaPkgWithOwnComp "tpa";
  haskellPackages =
    previous.haskellPackages.override (
      old:
      {
        overrides =
          final.lib.composeExtensions (old.overrides or (_: _: { })) (
            self: super: {
              "tpa" = final.tpa;
            }
          );
      }
    );
}
