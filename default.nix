{ mkDerivation, base, openblasCompat, primitive, stdenv }:
mkDerivation {
  pname = "refined-linear";
  version = "0.0.0";
  src = ./.;
  libraryHaskellDepends = [ base primitive ];
  librarySystemDepends = [ openblasCompat ];
  homepage = "https://github.com/ttuegel/refined-linear#readme";
  description = "Linear algebra with static dimensions using refinement types";
  license = stdenv.lib.licenses.unfree;
}
