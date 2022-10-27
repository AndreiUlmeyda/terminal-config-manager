#! /usr/bin/env bash

mkdir terminal-config-manager
mkdir -p terminal-config-manager/usr/local/bin

cp ../../bin/terminal-config-manager terminal-config-manager/usr/local/bin

mkdir terminal-config-manager/DEBIAN

touch terminal-config-manager/DEBIAN/control

echo 'Package: terminal-config-manager' >> terminal-config-manager/DEBIAN/control
echo 'Version: 1.0' >> terminal-config-manager/DEBIAN/control
echo 'Architecture: amd64' >> terminal-config-manager/DEBIAN/control
echo 'Maintainer: Adrian Schurz <adrian.schurz@check24.de>' >> terminal-config-manager/DEBIAN/control
echo 'Description: Terminal-Config-Manager ist ein Linux-Programm mit welchem Passagen innerhalb meherer Textdateien schnell zwischen einer Reihe vorkonfigurierter Passagen umgeschalten werden können.' >> terminal-config-manager/DEBIAN/control

dpkg-deb --build --root-owner-group terminal-config-manager

rm -rf terminal-config-manager