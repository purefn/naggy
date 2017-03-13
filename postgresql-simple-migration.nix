{ mkDerivation, base, base64-bytestring, bytestring, cryptohash
, directory, hspec, postgresql-simple, stdenv, text, time
}:
mkDerivation {
  pname = "postgresql-simple-migration";
  version = "0.1.9.0";
  sha256 = "0skjc5ivcrhi0f49p0j2f0k69qfv4argvkz5mvd8kn5q381jyp80";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base base64-bytestring bytestring cryptohash directory
    postgresql-simple time
  ];
  executableHaskellDepends = [
    base base64-bytestring bytestring cryptohash directory
    postgresql-simple text time
  ];
  testHaskellDepends = [ base bytestring hspec postgresql-simple ];
  doCheck = false;
  homepage = "https://github.com/ameingast/postgresql-simple-migration";
  description = "PostgreSQL Schema Migrations";
  license = stdenv.lib.licenses.bsd3;
}
