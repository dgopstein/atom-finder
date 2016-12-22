#!/bin/bash

echo -n "no  atom  no  bug: "; grep false $1 | grep -c '#{}'
echo -n "no  atom  yes bug: "; grep false $1 | grep -c '#{.\+}'
echo -n "yes atom  no  bug: "; grep true  $1 | grep -c '#{}'
echo -n "yes atom  yes bug; "; grep true  $1 | grep -c '#{.\+}'
