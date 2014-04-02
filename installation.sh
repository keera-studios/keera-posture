#!/bin/bash

# Copyright 2010-2014 Ivan Perez Dominguez & Keera Studios Ltd (UK)
# BSD3

# This script is ad-hoc. As such, it will install keera-posture, but
# it will not create any DEB for it. That being said, you may want
# to use it in order to install it in your $HOME and take the program
# out for a spin

# Check out all necessary repositories
git clone --depth=1 git://github.com/keera-studios/gtk-helpers.git
git clone --depth=1 git://github.com/keera-studios/hails-i18n.git
git clone --depth=1 git://github.com/keera-studios/hails-mvc-controller.git
git clone --depth=1 git://github.com/keera-studios/hails-mvc-environment-gtk.git
git clone --depth=1 git://github.com/keera-studios/hails-mvc-model-protectedmodels.git
git clone --depth=1 git://github.com/keera-studios/hails-mvc-solutions-config.git
git clone --depth=1 git://github.com/keera-studios/hails-mvc-solutions-gtk.git
git clone --depth=1 git://github.com/keera-studios/hails-mvc-view.git
git clone --depth=1 git://github.com/keera-studios/hails-mvc-view-gtk.git
git clone --depth=1 git://github.com/keera-studios/hails-reactive-gtk.git
git clone --depth=1 git://github.com/keera-studios/hails-reactivevalues.git
git clone --depth=1 git://github.com/keera-studios/keera-hails.git
git clone --depth=1 git://github.com/keera-studios/keera-posture.git
git clone --depth=1 git://github.com/keera-studios/MissingK.git
git clone --depth=1 git://github.com/sinelaw/HOpenCV.git

# Why cabal-dev does not do this automatically, I don't know
export PATH=$PATH:`pwd`/cabal-dev/bin

# Installation requires three things: auxiliary tools, haskell dependencies and
# the program itself. There are four necessary tools: alex, happy, gtk2hs,
# hails.  Because of the way cabal-dev adds packages to the DB (it runs cabal
# configure first, I believe) it is necessary to add some sources, install some
# tools, add more sources and so on and so forth, until every dependency is
# installed.
#
# Too bad, I know.

# Install alex and happy if they are not installed.
# It would be better to create a tool that checks if the programs
# exist, otherwise gives a choice to install them from
# binaries or from cabal. But there isn't.

which alex
if [[ "$?" -gt "0" ]] ; then
   cabal-dev install alex ;
fi

which happy
if [[ "$?" -gt "0" ]] ; then
   cabal-dev install happy;
fi

# Add some packages to cabal-dev's local package DB
cabal-dev add-source gtk-helpers
cabal-dev add-source hails-i18n
cabal-dev add-source hails-mvc-controller
cabal-dev add-source hails-mvc-environment-gtk
cabal-dev add-source hails-mvc-model-protectedmodels
cabal-dev add-source hails-mvc-solutions-config
cabal-dev add-source hails-mvc-solutions-gtk
cabal-dev add-source hails-mvc-view
cabal-dev add-source hails-mvc-view-gtk
cabal-dev add-source hails-reactive-gtk
cabal-dev add-source hails-reactivevalues
cabal-dev add-source keera-hails
cabal-dev add-source MissingK
cabal-dev add-source HOpenCV

# Install more dependencies
cabal-dev install gtk2hs-buildtools
cabal-dev install keera-hails

# Keera Posture
cabal-dev add-source keera-posture
cabal-dev install keera-posture
