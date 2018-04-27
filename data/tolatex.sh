#!/bin/bash
MYCSCV=$1
MYOUTPUT=$2
cat $MYCSCV | tr -d '"' |tr \, \. |tr \; \,  > $MYOUTPUT

