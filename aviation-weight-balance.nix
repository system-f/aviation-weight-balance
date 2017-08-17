{ mkDerivation, aviation-units, base, directory, doctest, filepath
, lens, parsec, QuickCheck, quickcheck-text, stdenv
, template-haskell
}:
mkDerivation {
  pname = "aviation-weight-balance";
  version = "0.0.1";
  src = ./.;
  libraryHaskellDepends = [ aviation-units base lens ];
  testHaskellDepends = [
    base directory doctest filepath parsec QuickCheck quickcheck-text
    template-haskell
  ];
  homepage = "https://github.com/data61/aviation-weight-balance";
  description = "Weight and Balance structures used in aviation";
  license = "unknown";
}
