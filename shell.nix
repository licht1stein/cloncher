{
  pkgs ? import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/f83fc3c307e74bc5fd5adb7eb6b8b13ffd2a36e1.tar.gz";
    sha256 = "09yfs84g2595gmibn4fr4izvjz6hb155l1ig5dyrrcxc79lvq8vh";
  }) { },
}:
let
  version = "0.8.27";
  jolt = pkgs.stdenv.mkDerivation {
    pname = "jolt";
    inherit version;
    src = pkgs.fetchurl {
      url = "https://github.com/jolt-lang/jolt/releases/download/v${version}/jolt-v${version}-x86_64-linux.tar.gz";
      hash = "sha256-eu3OZFpYQHcqdOA0Hac+bgcvQRB+4rME/HpJRwIaZLQ=";
    };
    nativeBuildInputs = [ pkgs.autoPatchelfHook ];
    buildInputs = [
      pkgs.stdenv.cc.cc.lib
      pkgs.zlib
      pkgs.openssl
      pkgs.util-linux.lib
    ];
    dontConfigure = true;
    dontBuild = true;
    installPhase = ''
      runHook preInstall
      install -Dm755 jolt "$out/bin/jolt"
      runHook postInstall
    '';
  };
in
assert pkgs.stdenv.hostPlatform.system == "x86_64-linux";
pkgs.mkShell {
  name = "cloncher";
  packages = [
    jolt
    pkgs.git
    pkgs.pkg-config
    pkgs.fd
    pkgs.fontconfig
    pkgs.libqalculate
    pkgs.wl-clipboard
  ];
  # Jolt runs on Nix's loader, which cannot load the system's GL driver: without one
  # its windows never draw. `jolt dev` and `jolt prod` hand Jolt this pinned Mesa.
  CLONCHER_EGL_VENDOR = "${pkgs.mesa}/share/glvnd/egl_vendor.d/50_mesa.json";
}
