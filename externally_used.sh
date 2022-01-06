#!/bin/bash

FILE=$1
ack $FILE: src/ | awk -F"${FILE}:" '{print $2}' | awk -v file=$FILE -F"(" '{print file":"$1}' | sort | uniq
