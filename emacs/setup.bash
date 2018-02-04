#!/bin/bash

# Download emacs, then emacs prelude
sudo apt install emacs
sudo apt install curl
curl -L https://git.io/epre | sh

# Link customizations
cd ~/.emacs.d/personal/preload
ln -s ~/misc-scripts/emacs/config.el config.el
