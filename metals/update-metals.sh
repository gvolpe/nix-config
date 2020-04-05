#!/bin/bash

coursier bootstrap --java-opt -Xss4m --java-opt -Xms100m --java-opt -Dmetals.client=vim org.scalameta:metals_2.12:0.8.3 -r bintray:scalacenter/releases -r sonatype:snapshots -o $HOME/metals/metals-vim -f
