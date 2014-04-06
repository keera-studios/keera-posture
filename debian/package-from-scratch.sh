#!/bin/bash
sudo -n true
if [[ "$?" -ge "1" ]]; then
  echo Cannot sudo without password. Execute sudo first, then run this command.
  exit 1
fi
PACKAGE=keera-posture
REPO=git@bitbucket.org:iperezdominguez/$PACKAGE-prerelease.git
git archive --remote=$REPO HEAD installation.sh | tar -x > installation.sh
chmod a+x installation.sh
DEBIAN_BUILD=1 ./installation.sh
sudo -v # Refresh sudo
$PACKAGE/debian/create-package.sh
