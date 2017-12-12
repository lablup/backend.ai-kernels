#! /bin/sh
set -e

PKGNAME="proj-4.9.3"

cd /tmp
wget "http://download.osgeo.org/proj/$PKGNAME.tar.gz"
tar xzvf "$PKGNAME.tar.gz"
cd "$PKGNAME"
./configure
make -j4
make install
ldconfig

cd /tmp
rm -rf "/tmp/$PKGNAME"
