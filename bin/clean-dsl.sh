#!/bin/bash

if [ "$ARGON_HOME" == "" ]; then
  echo "Please set ARGON_HOME environment variable."
  exit 1
fi

rm -rf $ARGON_HOME/target
