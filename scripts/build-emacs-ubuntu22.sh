#!/usr/bin/env bash
EBASE=`cd $(dirname $0); cd ..; pwd`
EMVER="29.3"
EMSRC="/opt/src"
EINST="/opt/emacs"

# prepare depends
sudo apt-get update && apt-get upgrade
sudo apt-get install -y build-essential libncurses-dev autoconf make gcc \
     texinfo libgtk-3-dev libxpm-dev libjpeg-dev libgif-dev libtiff5-dev \
     libncurses5-dev libxml2-dev libgnutls28-dev

if [ ! -d $EMSRC ]; then
  echo "set env '$EMSRC' please, $EMSRC not found"
  exit 1
fi

# download source
cd $EMSRC
wget -c https://mirrors.tuna.tsinghua.edu.cn/gnu/emacs/emacs-${EMVER}.tar.xz
tar xavf emacs-${EMVER}.tar.xz
cd emacs-${EMVER}

# build, install
./autogen.sh
./configure --prefix=$EINST
make -j $(nproc)
sudo make install
