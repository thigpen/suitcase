#!/bin/sh

THIS_HOME=`pwd`"/github"

for subdir in $THIS_HOME/*
do
echo $subdir
cd $subdir && git pull
done
