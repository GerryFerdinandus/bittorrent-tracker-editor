#!/bin/bash

# for local testing of the project in Ubuntu 14.04 (via VirtualBox)

#create download folder 'download_FPC'
cd ~
mkdir download_FPC
cd download_FPC

#download the FPC + lazarus IDE (1.6.2)
wget http://mirrors.iwi.me/lazarus/releases/Lazarus%20Linux%20amd64%20DEB/Lazarus%201.6.2/fpc_3.0.0-151205_amd64.deb
wget http://mirrors.iwi.me/lazarus/releases/Lazarus%20Linux%20amd64%20DEB/Lazarus%201.6.2/fpc-src_3.0.0-151205_amd64.deb
wget http://mirrors.iwi.me/lazarus/releases/Lazarus%20Linux%20amd64%20DEB/Lazarus%201.6.2/lazarus-project_1.6.2-1_amd64.deb

#Install the downloaded FPC + lazarus IDE
sudo dpkg -i ./*.deb
#install all the software dependency it needed
sudo apt install -f -y

#install git
sudo apt-get install git -y

#Download the source code in the home folder
cd ~
git clone --recursive https://github.com/GerryFerdinandus/bittorrent-tracker-editor.git

#build the source code trackereditor. (optional:  --build-mode=Release)
lazbuild --build-mode=Debug bittorrent-tracker-editor/source/project/tracker_editor/trackereditor.lpi

#build the source code tracker_editor_test
lazbuild --build-mode=Debug bittorrent-tracker-editor/source/project/unit_test/tracker_editor_test.lpi

#Run the unit test
./bittorrent-tracker-editor/enduser/test_trackereditor -a  --format=plain






