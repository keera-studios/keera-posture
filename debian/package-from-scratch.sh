#!/bin/bash
sudo -n true
if [[ "$?" -ge "1" ]]; then
  echo Cannot sudo without password. Execute sudo first, then run this command.
  exit 1
fi
PACKAGE=keera-posture
REPO=https://bitbucket.org/iperezdominguez/$PACKAGE-prerelease/raw/master
echo [1] Installing debian dependencies
wget $REPO/debian/debian-deps.sh  
chmod a+x debian-deps.sh
./debian-deps.sh 
echo [2] Extracting and compiling all packages
wget $REPO/installation.sh
chmod a+x installation.sh
DEBIAN_BUILD=1 ./installation.sh
sudo -v # Refresh sudo
echo [3] Building debian package
$PACKAGE/debian/create-package.sh
