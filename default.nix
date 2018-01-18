{ mkDerivation, base, bytestring, containers, ghci-pretty, lens
, mtl, parsers, pretty, stdenv, tasty, text, trifecta
}:
mkDerivation {
  pname = "dappish";
  version = "0.5";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring containers ghci-pretty lens mtl parsers pretty text
    trifecta
  ];
  executableHaskellDepends = [ base bytestring containers text ];
  testHaskellDepends = [ base bytestring tasty text ];
  homepage = "https://github.com/mbrock/dappish";
  description = "A description language for Ethereum contract systems";
  license = stdenv.lib.licenses.gpl3;
}
