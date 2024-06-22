#!/bin/sh
#
# This file will be called from Makefile
#
PRGNAM=clarafx
VERSION=${VERSION:-$(cat ../version.lisp-expr| cut -d '"' -f2)}
BUILD=${BUILD:-ecl_$(ecl --version | cut -d ' ' -f2)}
TAG=${TAG:-_SBo}
PKGTYPE=${PKGTYPE:-txz}

if [ -z "$ARCH" ]; then
  case "$( uname -m )" in
    i?86) ARCH=i486 ;;
    arm*) ARCH=arm ;;
       *) ARCH=$( uname -m ) ;;
  esac
fi

CWD=$(pwd)
TMP=${TMP:-$CWD/SBo}
PKG=$TMP/package-$PRGNAM
PREFIX=${PREFIX:-/usr/local}
OUTPUT=${OUTPUT:-$CWD}

set -e

rm -rf $PKG
mkdir -p $TMP $PKG $OUTPUT

make

chmod a+x $PRGNAM

cd $TMP

mkdir -p $PKG$PREFIX/bin
cp $CWD/$PRGNAM $PKG$PREFIX/bin

mkdir -p $PKG$PREFIX/doc/$PRGNAM-$VERSION
cp $CWD/../README.md $PKG$PREFIX/doc/$PRGNAM-$VERSION
cp $CWD/../LICENSE $PKG$PREFIX/doc/$PRGNAM-$VERSION

mkdir -p $PKG/install
cat $CWD/slack-desc > $PKG/install/slack-desc

cd $PKG
/sbin/makepkg -l y -c n $OUTPUT/$PRGNAM-$VERSION-$ARCH-$BUILD$TAG.$PKGTYPE
