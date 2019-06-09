#!/bin/sh

indent=$(cabal new-exec which indent)

function check {
    file=$1
    out=$(mktemp)

    echo "> Checking $file ..."
    $indent < $file > $out
    diff $file $out
    status=$?

    rm $out

    if [[ $status != 0 ]]; then
        echo "> Failed: $file"
        exit $status
    fi
}

for file in $(git ls-files '*.hs'); do
    check $file
done
