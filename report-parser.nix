{ mkDerivation, attoparsec, base, bytestring, stdenv }:
mkDerivation {
  pname = "report-parser";
  version = "0.1.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ attoparsec base bytestring ];
  executableHaskellDepends = [ attoparsec base bytestring ];
  description = "Parser combinator library for parsing log files and text reports";
  license = lib.licenses.bsd3;
}
