#!/bin/sh
rpm --queryformat "%{INSTALLTIME}\n" -q $1
