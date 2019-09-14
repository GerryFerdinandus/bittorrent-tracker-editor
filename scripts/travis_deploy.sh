#!/bin/sh

# Create a zip file for Linux and Apple macOS


#----------- check for Linux and Apple macOS build
if [ "$TRAVIS_OS_NAME" = "linux" ]
then
  # Linux
  echo "Building zip file for Linux amd64"
  zip -j $RELEASE_ZIP_FILE enduser/*.txt enduser/trackereditor

elif [ "$TRAVIS_OS_NAME" = "osx" ]
then
  # Apple macOS
  echo "Building zip file for macOS"
  cd enduser

  # Move the executable to the application bundle
  mv trackereditor trackereditor.app/Contents/MacOS

  # Move the trackers list to application bundle
  mv add_trackers.txt trackereditor.app/Contents/MacOS
  mv remove_trackers.txt trackereditor.app/Contents/MacOS

  # Create the zip file.
  zip -j ../$RELEASE_ZIP_FILE *.txt
  zip -r ../$RELEASE_ZIP_FILE trackereditor.app
  cd ..

fi

echo "Create Amazon s3 folder"
mkdir -p s3
cp $RELEASE_ZIP_FILE s3/$(date +%F_%R)_$RELEASE_ZIP_FILE

echo "Created $RELEASE_ZIP_FILE for GitHub"
