#!/bin/sh

function fix {
    file=$1
    out=$(mktemp)

    echo "> Fixing $file ..."
    cabal new-exec indent < $file > $out
    diff $file $out > /dev/null
    mv $out $file
}

for file in $(git ls-files '*.hs'); do
    fix $file
done
