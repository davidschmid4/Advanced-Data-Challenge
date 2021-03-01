#!/bin/bash
target="../01_Data/01_Raw/github/"
dir="covid-policy-tracker"
src="https://github.com/OxCGRT/covid-policy-tracker.git"

if [ ! -d $target$dir ];
then
echo "Try cloning repository";
#mkdir -p $target && echo "Success";
#echo $target
git -C $target clone $src;
else
echo "Try pulling repository";
git  -C $target$dir pull;
fi


