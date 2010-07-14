#!/bin/bash

BASE_URL=$(echo "$1" | sed 's/\/[^\/]*$//')
echo $BASE_URL

function get_attributes {
curl "$BASE_URL/$1" -o tag.html

ex tag.html << 'HERE'
1,/<h2>Optional Attributes<\/h2>/d
/<h2>/,$d
g!/<td><a/d
%s/.*>\([^<>]*\)<\/a><\/td>/\1/
wq
HERE

cat tag.html >> attributes
}

curl "$1" -o tags.html

ex tags.html << 'HERE'
g!/<td><a href="tag_.*">.*<\/a><\/td>/d
wq
HERE

sed 's/^.*"\(.*\)".*$/\1/g' tags.html | sed "s/.*/get_attributes &/" > tags
source tags
sed 's/^.*&lt;\(.*\)&gt;.*$/\1/' tags.html > tags
