#!/bin/bash
ARCH="x86_64-linux"
VERSION=$(git tag --points-at)
if [ "$VERSION" == "" ]
then
    VERSION=git-$(git log -n1 --format='%H')
fi
echo "VERSION: $VERSION"
SRCDIR=$(realpath "$0" | xargs dirname)
RELEASEDIR_LOCAL=fg-efb-server-$VERSION
RELEASEDIR_PARENT="$SRCDIR/release/$ARCH"
RELEASEDIR="$RELEASEDIR_PARENT/$RELEASEDIR_LOCAL"
TARBALL="$SRCDIR/release/fg-efb-server-$VERSION-$ARCH.tar.gz"
mkdir -p "$RELEASEDIR" || exit 1
rm -rf "$RELEASEDIR/*"
rm -f "$TARBALL"
cabal install --installdir="$RELEASEDIR" || exit 2
cp "$SRCDIR/README.markdown" "$RELEASEDIR"
cp "$SRCDIR/CHANGELOG" "$RELEASEDIR"
cp "$SRCDIR/providers.yaml.dist" "$RELEASEDIR"
cp -R "$SRCDIR/provider-scripts" "$RELEASEDIR"
(
    cd "$RELEASEDIR_PARENT"
    tar cf "$TARBALL" "$RELEASEDIR_LOCAL"
)
