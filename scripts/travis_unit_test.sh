#!/bin/sh

# Run unit test in Windows, Linux and macOS

#----------- check for Windows, Linux and macOS build
if [ "$TRAVIS_OS_NAME" = "linux" ]
then
  # show Linux OS version
  uname -a

  # show openSSL version
  openssl version

  # Exit immediately if a command exits with a non-zero status.
  set -e
  xvfb-run enduser/test_trackereditor -a --format=plain
  set +e

elif [ "$TRAVIS_OS_NAME" = "osx" ]
then
  # show macOS version
  sw_vers

  # show openSSL version
  openssl version

  # Exit immediately if a command exits with a non-zero status.
  set -e
  enduser/test_trackereditor -a --format=plain
  set +e

elif [ "$TRAVIS_OS_NAME" = "windows" ]
then
  # Exit immediately if a command exits with a non-zero status.
  set -e
  enduser/test_trackereditor -a --format=plain
  set +e
fi


# Remove all the extra file created by test
# We do not what it in the ZIP release files.
rm -f enduser/console_log.txt
rm -f enduser/export_trackers.txt

# Undo all changes made by testing.
git reset --hard
