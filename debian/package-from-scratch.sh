#!/bin/bash
PACKAGE=keera-posture
REPO=git@bitbucket.org:iperezdominguez/$PACKAGE-prerelease.git
git archive --remote=$REPO HEAD installation.sh | tar -x > installation.sh
DEBIAN_BUILD=1 ./installation.sh
$PACKAGE/debian/create-package.sh
