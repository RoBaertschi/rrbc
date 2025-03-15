# ml2 ts=2 sts=2 sw=2
{rustPlatform, lib}: rustPlatform.buildRustPackage rec {
  pname = "rrbc";
  version = "0.1.0";
  src = ./..;
  useFetchCargoVendor = true;
  cargoLock = {
    lockFile = ../Cargo.lock;
  };
  cargoTestFlags = [ "--workspace" ];
  meta = {
    description = "rrbc C Compiler";
    homepage = "https://github.com/RoBaertschi/rrbc";
    maintainers = [ ];
  };
}
