import Lake
open Lake DSL

package blockfall where
  precompileModules := true

require terminus from git "https://github.com/nathanial/terminus" @ "v0.0.1"
require crucible from git "https://github.com/nathanial/crucible" @ "v0.0.1"

@[default_target]
lean_lib Blockfall where
  roots := #[`Blockfall]

lean_exe blockfall where
  root := `Main

lean_lib Tests where
  roots := #[`Tests]

@[test_driver]
lean_exe tests where
  root := `Tests.Main
