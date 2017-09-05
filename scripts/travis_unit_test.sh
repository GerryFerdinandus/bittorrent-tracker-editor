#!/bin/bash

#Run unit test in Windows, Linux and macOS


#----------- check for Windows, Linux and macOS build
if [ "$TRAVIS_OS_NAME" = "linux" ]
then

  #wine = windows
  if [ "$LAZ_ENV" = "wine" ]
  then

  #windows
  # does not work.
  #wine enduser/test_trackereditor.exe -a --format=plain

  # Will exit with status of last command.
  exit

  else

  #linux
  enduser/test_trackereditor -a --format=plain

  # Will exit with status of last command.
  exit

  fi

elif [ "$TRAVIS_OS_NAME" = "osx" ]
then
  #macOS
  # does not work.
  #enduser/test_trackereditor -a --format=plain

  # Will exit with status of last command.
  exit

fi
