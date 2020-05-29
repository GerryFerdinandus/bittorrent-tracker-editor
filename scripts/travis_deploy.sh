#!/bin/sh

# Create a zip file for Windows, Linux and macOS.

#----------- check for Windows, Linux and macOS build
if [ "$TRAVIS_OS_NAME" = "linux" ]
then
  # Linux
  sudo apt-get install zip -y
  echo "Building zip file for Linux amd64"
  zip -j $RELEASE_ZIP_FILE enduser/*.txt enduser/trackereditor

elif [ "$TRAVIS_OS_NAME" = "osx" ]
then
  # Apple macOS
  echo "Building zip file for macOS"

  # Add certificate into the macOS system
  source ./scripts/travis_add_macos_cert.sh

  # sign + zip the app
  source ./scripts/travis_sign_macos_app.sh

elif [ "$TRAVIS_OS_NAME" = "windows" ]
then
  # Windows
  echo "Building zip file for windows"
  choco install zip
  zip -j $RELEASE_ZIP_FILE enduser/*.txt enduser/trackereditor.exe enduser/*.dll
fi

echo "Create Amazon s3 folder"
mkdir -p s3
cp $RELEASE_ZIP_FILE s3/$(date +%F_%R)_$RELEASE_ZIP_FILE

echo "Created $RELEASE_ZIP_FILE for GitHub"
