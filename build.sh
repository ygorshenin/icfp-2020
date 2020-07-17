#!/bin/sh

cd app
mkdir ../build
g++ -std=c++17 -O2 -o ../build/main main.cpp
