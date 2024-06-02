#!/usr/bin/env bash
EBASE=`cd $(dirname $0); cd ..; pwd`
EMACS=emacs

MACUNAME=$(uname -s | grep -i darwin)
if [ X"$MACUNAME" != X"" ]; then
    EMACS="$HOME/Applications/Emacs.app/Contents/MacOS/Emacs"
fi

$EMACS --script $EBASE/scripts/instpkg.el
