#!/bin/sh

DIRECT='[-0-9A-Za-z_\/.\*]'
ESCAPED='(%[0-9A-Fa-f][0-9A-Fa-f])'

VALUE="($DIRECT|$ESCAPED)+"
KEY="$VALUE"
ADDRESS="($VALUE):(($KEY)=($VALUE)(,($KEY)=($VALUE))*)?"
EXPR="($ADDRESS)(;($ADDRESS))*"

echo $EXPR
