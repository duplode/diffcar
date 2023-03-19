#!/bin/sh
shopt -s nocaseglob
if [ -n "$3" ]; then
	rm "$3"
	for i1 in $1/CAR*.RES; do
		i2=${i1/$1/$2}
		if [ -n "$2" ]; then
			cabal run --verbose=0 diffcar -- \
				--plain --header --skip-equals \
				"$i1" "$i2" >> "$3"
		fi
	done
fi
