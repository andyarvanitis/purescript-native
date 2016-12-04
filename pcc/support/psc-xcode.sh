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
  OBJECT_TYPE=debug
else
  if [ ! -f "$IS_RELEASE" ] ; then
    make clean
    touch "$IS_RELEASE"
  fi
  rm "$IS_DEBUG" > /dev/null 2>&1 || true
  OBJECT_TYPE=release
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

make $OBJECT_TYPE CXXFLAGS="$ARCH_OPTS $FLTO -isysroot $SDKROOT" LDFLAGS="-r -nostdlib $ARCH_OPTS" BIN=purescript.o -j`sysctl -n hw.ncpu`
