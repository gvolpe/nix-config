#!/bin/bash

# Git config
git config --global user.email "volpegabriel@gmail.com"
git config --global user.name "Gabriel Volpe"

# Git aliases
git config --global alias.amend 'commit --amend -m'
git config --global alias.br branch
git config --global alias.cm 'commit -m'
git config --global alias.co checkout
git config --global alias.ls 'log --pretty=format:"%C(yellow)%h%Cred%d\\ %Creset%s%Cblue\\ [%cn]" --decorate'
git config --global alias.ll 'log --pretty=format:"%C(yellow)%h%Cred%d\\ %Creset%s%Cblue\\ [%cn]" --decorate --numstat'
git config --global alias.st status

# SSH key
ssh-keygen -t rsa -b 4096 -C "volpegabriel@gmail.com"
eval "$(ssh-agent -s)"
ssh-add ~/.ssh/id_rsa
