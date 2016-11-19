# Xcode support script. Intended to be added to and called from "Run Script" build phase.
#
#!/bin/sh

echo "CONFIGURATION =" "$CONFIGURATION"
echo "ARCHITECTURES =" "$ARCHS"

IS_DEBUG="$PROJECT_TEMP_DIR/is_debug"
IS_RELEASE="$PROJECT_TEMP_DIR/is_release"

if [[ "$CONFIGURATION" == "Debug" ]] ; then
  if [ ! -f "$IS_DEBUG" ] ; then
    make clean
    touch "$IS_DEBUG"
  fi
  rm "$IS_RELEASE" > /dev/null 2>&1 || true
  OBJECT_TYPE=debug-object
else
  if [ ! -f "$IS_RELEASE" ] ; then
    make clean
    touch "$IS_RELEASE"
  fi
  rm "$IS_DEBUG" > /dev/null 2>&1 || true
  OBJECT_TYPE=release-object
fi

## Create compiler switches for architectures
#
for arch_ in $ARCHS ; do
  ARCH_OPTS+="-arch $arch_ "
  if ! file output/bin/purescript.o | grep -q $arch_; then
    make clean
  fi
  # TODO: make this more future-proof
  # Note: -flto optimization causes problems with the source-level
  # debugger. Disable bitcode temporarily if necessary.
  if [[ $ENABLE_BITCODE == YES && $arch == arm64 ]] ; then
    FLTO="-flto"
  fi
done

## Look for psc-package, in case it's already in the PATH
#
if [ -x "$(command -v psc-package)" ] ; then
  PSC_PKG="PSC_PACKAGE=$(command -v psc-package)"
fi

make $OBJECT_TYPE ${PSC_PKG} CXXFLAGS="$ARCH_OPTS $FLTO -isysroot $SDKROOT" LD="clang -Wl,-r -nostdlib $ARCH_OPTS" -j`sysctl -n hw.ncpu`
