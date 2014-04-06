PACKAGE_NAME=keera-posture
DEBIAN_LIB_DIR=/usr/lib/$PACKAGE_NAME
DEBIAN_BIN_DIR=/usr/bin
DEBIAN_DATA_DIR=/usr/share/$PACKAGE_NAME/data
ROOT_DIR=$PACKAGE_NAME
DIST_DIR=$ROOT_DIR/dist/build/
DEB_PACKAGE_PLAIN_TOP_DIRS=usr
DEB_PACKAGE_TOP_DIRS=$DEST/usr
DEB_PACKAGE_BIN_DIR=$DEST/$DEBIAN_BIN_DIR
DEB_PACKAGE_LIB_DIR=$DEST/$DEBIAN_LIB_DIR
DEB_PACKAGE_DATA_DIR=$DEST/$DEBIAN_DATA_DIR
DEB_PACKAGE_MAN_DIR=$DEST/usr/share/man/man1/
DEB_PACKAGE_LAUNCHER_DIR=$DEST/usr/share/applications/
DEB_PACKAGE_DOC_DIR=$DEST/usr/share/doc/$PACKAGE_NAME/
DEB_PACKAGE_PIXMAP_DIR=$DEST/usr/share/
DEB_PACKAGE_DEBIAN_DIR=$DEST/DEBIAN
ARCH=$(apt-cache -v | grep -oe '\(amd64\|i386\)')

sanity_check(){
 # Can sudo?
 sudo -n true
 if [[ "$?" -ge "1" ]]; then
   echo Cannot sudo. Execute sudo first, or make sure this user can sudo without a password
   exit 1
 fi 

 if [[ -z "$DISTRO" ]]; then
   echo Distro not set \(env DISTRO\);
   exit 1;
 fi

 if [[ -z "$OTHER" ]]; then
   echo Cannot find auxiliary dir \(env OTHER\);
   exit 1;
 fi

 if [[ -z "$DEST" ]]; then
   echo Cannot find destination dir \(env DEST\);
   exit 1
 fi

 # is DEST empty?
 local empty=$(find $DEST -maxdepth 0 -type d -empty -exec echo 1 \; );
 if [[ "$empty" -ne "1" ]]; then
   echo Destination directory is not empty. Remove contents first.
   exit 1
 fi

}

maybe_create_dir() {
    if [[ ! -d "$1" ]] ; then
       echo -n [CREATING $2 DIR]...
       mkdir -p $1
    fi
}

copy_program() {
  echo -n Copying executable $1...
  maybe_create_dir "$DEB_PACKAGE_BIN_DIR" "BIN"

  echo -n [COPYING]...
  cp $DIST_DIR/$1/$1 $DEB_PACKAGE_BIN_DIR

  echo -n [STRIPPING]...
  strip $DEB_PACKAGE_BIN_DIR/$1

  echo [DONE]
}

copy_man_page() {
  echo -n Copying man page for $1...
  local mandir=$DEB_PACKAGE_MAN_DIR
  local manpage=$1.1
  local manfile=$OTHER/$manpage
  if [[ -e "$manfile" && -f "$manfile" ]]; then
    maybe_create_dir $mandir "MAN PAGE"

    echo -n [COPYING]...
    cp $manfile $mandir

    echo -n [COMPRESSING]...
    gzip -f -9 $mandir/$manpage

    echo [DONE]
  else
    echo [NOT FOUND]
  fi
}

copy_launcher() {
  echo -n Copying launcher for $1...
  local launcherdir=$DEB_PACKAGE_LAUNCHER_DIR
  local launcher=$1.desktop
  local launcherfile=$OTHER/$launcher
  if [[ -e "$launcherfile" && -f "$launcherfile" ]]; then
    maybe_create_dir $launcherdir "LAUNCHER"

    echo -n [COPYING]...
    cp $launcherfile $launcherdir

    echo -n [ADJUSTING ICON]...
    sed -i "s@^Icon=.*@Icon=$DEBIAN_DATA_DIR/$1.png@g" $launcherdir/$launcher 

    echo [DONE]
  else
    echo [NOT FOUND]
  fi
}

copy_data() {
  echo -n Copying data files...
  maybe_create_dir $DEB_PACKAGE_DATA_DIR "DATA"

  echo -n [COPYING]...
  cp -r cabal-dev/share/*linux-ghc*/$PACKAGE_NAME-*/data/* $DEB_PACKAGE_DATA_DIR
  echo [DONE]
}

copy_doc() {
  echo -n Copying doc files...
  maybe_create_dir $DEB_PACKAGE_DOC_DIR "DOC"

  echo -n [COPYING]...
  cp -r $OTHER/doc/* $DEB_PACKAGE_DOC_DIR
  echo [DONE]
}

adjust_permissions() {
  echo -n Adjusting permissions...
  echo -n [ROOT OWNERSHIP]...
  sudo chown root:root -R $DEB_PACKAGE_TOP_DIRS
  echo -n [CAN READ TOP DIRS]...
  find $DEB_PACKAGE_TOP_DIRS -type d | xargs sudo chmod 755
  echo -n [CAN RUN PROGRAMS]...
  find $DEB_PACKAGE_BIN_DIR -type f | xargs sudo chmod 755
  echo -n [can traverse dirs]...
  echo [DONE]
}

generate_debian_dir() {

  local installed_size=$(du -s $DEB_PACKAGE_TOP_DIRS | cut -d '	' -f 1)

  echo -n [Generating control info]...
  maybe_create_dir $DEB_PACKAGE_DEBIAN_DIR "DEBIAN"

  echo -n [COPYING]...
  cp $OTHER/control-$DISTRO-$ARCH $DEB_PACKAGE_DEBIAN_DIR/control

  echo -n [ADJUSTING SIZE]...
  sed -i "s/^Installed-size: .*$/Installed-size: $installed_size/g" $DEB_PACKAGE_DEBIAN_DIR/control

  echo -n [ADJUSTING PERMISSIONS]...
  sudo chmod 755 $DEB_PACKAGE_DEBIAN_DIR

  echo -n [CALCULATING CHECKSUMS]...
  pushd $DEST > /dev/null
  sudo find $DEB_PACKAGE_PLAIN_TOP_DIRS -type f | sudo xargs md5sum | sudo tee DEBIAN/md5sums > /dev/null
  popd > /dev/null
  echo [DONE]
}

package () {
  echo "Packaging..."
  sudo dpkg-deb --build $DEST
  echo "[RENAMING FILE]..."
  local version=$(cat $OTHER/control-$DISTRO-$ARCH | grep -e '^Version:' | cut -d ' ' -f 2-)
  mv $DEST.deb $PACKAGE_NAME-$version.deb
  echo "[DONE]"
}

# run_lintian () {
#   
# }

sanity_check

# Copy programs
for PROGRAM in $(cat $ROOT_DIR/$PACKAGE_NAME.cabal | grep Executable | cut -d ' ' -f 2-); do
  copy_program $PROGRAM
  copy_man_page $PROGRAM
  copy_launcher $PROGRAM
done

copy_data

copy_doc

adjust_permissions

generate_debian_dir

package
