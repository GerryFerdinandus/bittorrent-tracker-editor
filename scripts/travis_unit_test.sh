#!/bin/bash

#Run unit test in Windows, Linux and macOS


#----------- check for Windows, Linux and macOS build
if [ "$TRAVIS_OS_NAME" = "linux" ]
then
  # show Linux OS version
  uname -a

  #show openSSL version
  openssl version

  #wine = windows
  if [ "$LAZ_ENV" = "wine" ]
  then

  #windows via 'Wine' does not work.
  #wine enduser/test_trackereditor.exe -a --format=plain
  echo There is no unit test for wine windows.
  echo unit test is run via AppVeyor

  else

  #linux
  enduser/test_trackereditor -a --format=plain

  fi

elif [ "$TRAVIS_OS_NAME" = "osx" ]
then
  # show macOS version
  sw_vers

  #show openSSL version
  openssl version

  #macOS
  # does not work.
  #enduser/test_trackereditor -a --format=plain
  echo There is no unit test for macOS.

fi

#Remove all the extra file created by test
rm enduser/console_log.txt
rm enduser/export_trackers.txt

#Undo all changes made by testing.
git reset --hard
