{ mkDerivation, aviation-units, base, lens, stdenv }:
mkDerivation {
  pname = "aviation-weight-balance";
  version = "0.0.1";
  src = ./.;
  libraryHaskellDepends = [ aviation-units base lens ];
  homepage = "https://github.com/data61/aviation-weight-balance";
  description = "Weight and Balance structures used in aviation";
  license = "unknown";
}
