#!/bin/bash
target="../01_Data/01_Raw/github/"
dir="COVID-19"
src="https://github.com/CSSEGISandData/COVID-19.git"

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


