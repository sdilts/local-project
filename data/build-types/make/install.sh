#!/bin/sh
# the install script runs as superuser, no need for 'sudo'
make -C $1 install
