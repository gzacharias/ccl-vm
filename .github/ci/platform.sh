# Sourced by the other scripts.  Sets, for the machine this runs on:
#   CCL_PLATFORM  the name in the ccl 1.14 release tarballs (darwinx86, darwinarm64, linuxx86, linuxarm64)
#   CCL_KERNEL    the native kernel's name (dx86cl64, darm64cl, lx86cl64, arm64cl)
# and defines
#   fetch_release FILE        downloads FILE from the ccl 1.14 release into the current directory
#                             unless it's already there, and prints its path
#   target_info TARGET        sets T_RELEASE (release tarball), T_KERNEL, T_BOOT_IMAGE, T_HEADERS
#                             (interface headers directory), T_FASL (fasl type), T_XLOAD (the
#                             cross-compile setup file) for a cross-compile TARGET
#   github_env NAME=VALUE     records a variable for later workflow steps (no-op outside GitHub)
CCL_RELEASE_URL=https://github.com/Clozure/ccl/releases/download/v1.14

case $(uname -s)-$(uname -m) in
  Darwin-arm64)  CCL_PLATFORM=darwinarm64 CCL_KERNEL=darm64cl ;;
  Darwin-x86_64) CCL_PLATFORM=darwinx86   CCL_KERNEL=dx86cl64 ;;
  Linux-aarch64) CCL_PLATFORM=linuxarm64  CCL_KERNEL=arm64cl ;;
  Linux-x86_64)  CCL_PLATFORM=linuxx86    CCL_KERNEL=lx86cl64 ;;
  *) echo "::error::unknown platform $(uname -s)-$(uname -m)" >&2; exit 1 ;;
esac

fetch_release() {
  [ -f "$1" ] || curl -sSfL -O "$CCL_RELEASE_URL/$1" || { echo "::error::can't download $1" >&2; return 1; }
  echo "$PWD/$1"
}

target_info() {
  case $1 in
    darwinx8664)  T_RELEASE=darwinx86.tar.gz   T_KERNEL=dx86cl64 T_BOOT_IMAGE=x86-boot64.image
                  T_HEADERS=darwin-x86-headers64 T_FASL=dx64fsl T_XLOAD=xdarwinx8664 ;;
    darwinarm64)  T_RELEASE=darwinarm64.tar.gz T_KERNEL=darm64cl T_BOOT_IMAGE=arm64-boot.image
                  T_HEADERS=darwin-arm64-headers T_FASL=da64fsl T_XLOAD=xdarwinarm64 ;;
    *) echo "::error::unknown cross-compile target $1" >&2; return 1 ;;
  esac
}

github_env() {
  [ -n "${GITHUB_ENV:-}" ] && echo "$1" >> "$GITHUB_ENV"
  return 0
}
