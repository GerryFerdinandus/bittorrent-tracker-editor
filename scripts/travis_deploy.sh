#!/bin/bash

#Create a zip file for Windows, Linux and Apple OS X

#this will be set later
unset RELEASE_ZIP_FILE

#----------- check for Windows, Linux and Apple OS X build
if [ "$TRAVIS_OS_NAME" = "linux" ]
then

  #wine = windows
  if [ "$LAZ_ENV" = "wine" ]
  then

  #windows
  export RELEASE_ZIP_FILE="trackereditor_win32.zip"
  zip -j $RELEASE_ZIP_FILE enduser/*.txt enduser/trackereditor.exe

  else

  #linux
  export RELEASE_ZIP_FILE="trackereditor_linux_amd64.zip"
  zip -j $RELEASE_ZIP_FILE enduser/*.txt enduser/trackereditor

  fi

elif [ "$TRAVIS_OS_NAME" = "osx" ]
then
  #Apple os x
  export RELEASE_ZIP_FILE="trackereditor_os_x.zip"
  cd enduser

  #move the executable to the application bundle
  mv trackereditor trackereditor.app/Contents/MacOS

  #move the trackers list to application bundle
  mv add_trackers.txt trackereditor.app/Contents/MacOS
  mv remove_trackers.txt trackereditor.app/Contents/MacOS

  #Create the zip file.
  zip -j ../$RELEASE_ZIP_FILE *.txt
  zip -r ../$RELEASE_ZIP_FILE trackereditor.app
  cd ..

fi

echo "Created $RELEASE_ZIP_FILE for GitHub"
