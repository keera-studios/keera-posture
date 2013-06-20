#!/bin/bash


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

export PATH=$PATH:`pwd`/cabal-dev/bin

# Install alex and happy
which alex
if [[ "$?" -gt "0" ]] ; then
   cabal-dev install alex ;
fi

which happy
if [[ "$?" -gt "0" ]] ; then
   cabal-dev install happy;
fi

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

cabal-dev install gtk2hs-buildtools
cabal-dev install keera-hails

cabal-dev add-source keera-posture
cabal-dev install keera-posture
