# Welcome to Keera Posture

This application tries to detect your posture while you sit and warns you when
it deviates too much from a reference posture given during configuration. It
may be used, among other things, to help you detect when you are sitting in a
straining position.

WARNING: No promises are made about this program. If you use it, you'll do so
under your own risk. This program might make your computer explode, kill your
cat, or make your back pain problems worse. Use it with care and under medical
supervision only. Report any issues with the program to Keera Studios by email,
facebook, linkedin or google plus. It is best used in combination with programs
that force you to take regular breaks, stretch, etc. By continuing to use
keera-posture, you are agreeing not to sue us or Keera Studios Ltd.

# INSTALLATION (default)

Keera Posture is available on hackage. Unless you need the latest development
version, the recommended way is to install the version there.

Install C/C++ development headers. That includes gtk, sdl, opengl, opencv.

Then:
```
$ cabal sandbox init
$ cabal update
$ cabal install gtk2hs-buildtools
$ export PATH=$PWD/.cabal-sandbox/bin:$PATH
$ cabal install keera-posture
```

# INSTALLATION ON DEBIAN (the not-so-easy way; but still quite easy)

You need to have git, cabal, cabal-dev and ghc installed. Everything else will
be installed for you. You also need to run sudo (at least once) so that the script
can sudo without a password whenever it needs to.

If you do not know how to install cabal-dev, run:

    $ cabal install cabal-dev
    $ export PATH=$PATH:$HOME/.cabal/bin

After that, all you need is:

    $ mkdir -p keera-posture && cd keera-posture && (wget -O- https://raw.githubusercontent.com/keera-studios/keera-posture/master/debian/package-from-scratch.sh | bash)

This will create all the necessary directories, check out all sources, prompt
to install dependencies, compile the whole program, and create a DEB file for your
architecture and version of debian/ubuntu. If it works, install that.

# COMPILATION ON OTHER LINUX/UNIX SYSTEMS (not so easy, not so hard)

Again, you need to have ghc, cabal, cabal-dev and git. You also need some
libraries installed (GL, SDL, SDL-mixer, FTGL, OpenCV, Gtk2, Glade2). The list
for debian systems is:

- libopencv-dev libglade2-dev libgl-dev libglu1-mesa-dev libftgl-dev libsdl1.2-dev libsdl-mixer1.2-dev 

I don't use Fedora, so you have to find which ones are yours. Then, run the following:

    $ mkdir -p keera-posture && cd keera-posture && (wget -O- https://raw.githubusercontent.com/keera-studios/keera-posture/master/installation.sh | bash)

This will check out, compile and install everything you need.

# DOING EVERYTHING BY HAND (only for developers)

Dependencies (tools):
- alex (gtk2hs-buildtools), happy (gtk2hs-buildtools), gtk2hs-buildtools.

Dependencies (libraries):
- libopencv-dev libglade2-dev libgl-dev libglu1-mesa-dev libftgl-dev
  libsdl1.2-dev libsdl-mixer1.2-dev 

 (If you use debian/ubuntu, you can install all of those with apt-get)

Installation instructions (for ubuntu, but the program is known to work on Windows):

1) Assuming you have ghc and cabal, create a sandbox and fix your PATH.
```
    $ cabal sandbox init
    $ export PATH=$PWD/.cabal-sandbox/bin:$PATH
```
2) Install all the libraries that this depends on:
```
    $ apt-get install libopencv-dev libglade2-dev libgl-dev libglu1-mesa-dev libftgl-dev libsdl1.2-dev libsdl-mixer1.2-dev
```
3) Update your package list, install dependencies.
```
    $ cabal install alex
    $ cabal install happy
    $ cabal install gtk2hs-buildtools
```
4) Install keera-posture
```
    $ cabal install keera-posture
```
5) Keera posture will be installed in keera-posture/cabal-dev/bin/keera-posture.
If you want to create a desktop launcher, https://raw.github.com/keera-studios/keera-posture/master/data/icon-good-posture.png can make a good icon.

6) Go to your preferred social network and like Keera Studios, share our posts,
follow us, tell all your friends about it. Record a video of you using the
program, and upload it to youtube.

# Calibration

The first time you run the program, you have to calibrate it. Just sit how you
think you should sit (and consult a physician if you don't know how to sit
properly) and right-click on its icon, and click on "Calibrate". Follow the
assistant, and the program will detect you and record your position.

After that, the position will be saved in a config file and the program will
warn you (using sound, a window and/or changing the tray icon) when your
posture is too different from what it was when you calibrated the program.

Test the program. The placement of your webcam and the amount of light
in the room can influence the accuracy of this system, so feel free to
experiment and find out how to get better results. Also, if you can think of
new features that might be useful, go online and open a ticket on the program's
github page.

# References

Keera Hails - Reactive Values: https://github.com/keera-studios/hails-reactivevalues
