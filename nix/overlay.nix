final: previous:
with final.haskell.lib;

let
  sources = import ./sources.nix;
in
{
  tpaPackages =
    let
      pathFor = name: final.gitignoreSource (../. + "/${name}");
      tpaPkg =
        name:
          addBuildDepend
            (
              failOnAllWarnings (
                disableLibraryProfiling (final.haskellPackages.callCabal2nixWithOptions name (pathFor name) "--no-hpack" {})
              )
            )
            final.haskellPackages.autoexporter;
      tpaPkgWithComp =
        exeName: name:
          generateOptparseApplicativeCompletion exeName (tpaPkg name);
      tpaPkgWithOwnComp = name: tpaPkgWithComp name name;
    in
      {
        "tpa" = tpaPkg "tpa";
      };

  haskellPackages =
    previous.haskellPackages.override (
      old:
        {
          overrides =
            final.lib.composeExtensions (old.overrides or (_: _: {})) (
              self: super:
                final.tpaPackages
            );
        }
    );
}
