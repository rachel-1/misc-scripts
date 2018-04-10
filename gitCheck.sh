#!/bin/bash

# Modified version of gitCheck provided by @mzabriskie
# source: https://gist.github.com/mzabriskie/6631607/

dir="$1"

# No directory has been provided, use current
if [ -z "$dir" ]
then
    dir="`pwd`"
fi

# Make sure directory ends with "/"
if [[ $dir != */ ]]
then
	dir="$dir/*"
else
	dir="$dir*"
fi

# Loop all sub-directories
for f in $dir
do
	# Only interested in directories
	[ -d "${f}" ] || continue

	# Check if directory is a git repository
	if [ -d "$f/.git" ]
	then

		echo -en "\033[0;35m"
		echo "${f}"
		echo -en "\033[0m"

		mod=0
		cd $f

		# Check branch
		git status | head -n1

		# Check for modified files
		if [ $(git status | grep modified -c) -ne 0 ]
		then
			mod=1
			echo -en "\e[45m"
			echo "Modified files"
			echo -en "\e[49m"
		fi

		# Check for untracked files
		if [ $(git status | grep Untracked -c) -ne 0 ]
		then
			mod=1
			echo -en "\033[0;31m"
			echo "Untracked files"
			echo -en "\033[0m"
		fi

		# Check if everything is peachy keen
		if [ $mod -eq 0 ]
		then
			echo "Nothing to commit"
		fi

		cd ../

		echo
	fi

done
