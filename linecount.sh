#!/bin/sh

grep -v '^%' $1 | grep . | wc -l

