#!/bin/bash
DEPS="libopencv-dev libglade2-dev libgl1-mesa-dev libglu1-mesa-dev libftgl-dev libsdl1.2-dev libsdl-mixer1.2-dev"
REAL_DEPS=""
for dep in $DEPS; do
  installed=$(apt-cache policy $dep | grep -e 'Installed: ' | grep -oe ': .*$' | grep -oe ' .*$' | grep -oe '[^ ]\+$')
  if [ -z "$installed" ] || [ "$installed" = "(none)" ] ; then
      REAL_DEPS+=" $dep"
  fi
done
if [[ ! -z "$REAL_DEPS" ]]; then
  echo Some packages are not installed: $REAL_DEPS
  sudo apt-get install $REAL_DEPS
else
  echo All system dependencies are installed. Proceed.
fi
