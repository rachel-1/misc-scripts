#!/bin/bash

# Download emacs, then emacs prelude
sudo apt install emacs
curl -L https://git.io/epre | sh

# Link customizations
cd ~/.emacs.d/personal/preload
ln -s ~/misc-scripts/config.el config.el
