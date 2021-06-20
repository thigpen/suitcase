#!/bin/sh

PKGS=$(cat brew_package_list.txt)
FAILED_PKGS=""

TAPS=$(cat brew_tap_list.txt)

for TAP in $TAPS
do
    echo "--------------------------------------------------------------------------------"
    echo "brew tap $TAP"
    echo "--------------------------------------------------------------------------------"
    brew tap $TAP
    STATUS=$?
    echo "STATUS: $STATUS"
    if [ "$STATUS" -ne 0 ]; then
	FAILED_TAPS="$FAILED_TAPS $TAP"
    fi
    echo "--------------------------------------------------------------------------------"
    echo    
done

for PACKAGE in $PKGS
do
    echo "--------------------------------------------------------------------------------"
    echo "brew install $PACKAGE"
    echo "--------------------------------------------------------------------------------"
    brew install $PACKAGE
    STATUS=$?
    echo "STATUS: $STATUS"
    if [ "$STATUS" -ne 0 ]; then
	FAILED_PKGS="$FAILED_PKGS $PACKAGE"
    fi
    echo "--------------------------------------------------------------------------------"
    echo
done

if [ "$FAILED_PKGS" ]; then
    echo "Failed installs:"
    for FAILED_PKG in $FAILED_PKGS
    do
	echo "> $FAILED_PKG"
    done
fi

exit 0
