#!/bin/sh

indent=$(cabal new-exec which indent)

function fix {
    file=$1
    out=$(mktemp)

    echo "> Fixing $file ..."
    $indent < $file > $out
    diff $file $out > /dev/null
    mv $out $file
}

for file in $(git ls-files '*.hs'); do
    fix $file
done
