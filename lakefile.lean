import Lake
open Lake DSL

package blockfall where
  precompileModules := true

-- Local workspace dependencies
require terminus from ".." / "terminus"
require crucible from ".." / "crucible"

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
