# .emacs.d
My emacs settings.

## Requirements
**Ubuntu 18.04 LTS**
**Emacs>=25.1**
- [leaf](https://github.com/conao3/leaf.el)
- [leaf-keywords](https://github.com/conao3/leaf-keywords.el#hydra-keyword)
- [HackGen](https://github.com/yuru7/HackGen)
- [ccls](https://github.com/MaskRay/ccls/wiki/Build)
- [CMake>=3.8](https://github.com/Kitware/CMake)
- gcc>7.2
- llvm, clang >=8.0

## Installation
First, clone this repository.
```bash
$ cd ~
$ git clone https://github.com/RyodoTanaka/.emacs.d.git
```
### Install HackGen
This is the programming fonts.
```bash
$ cd ~
$ mkdir .fonts
$ cd .fonts
$ wget https://github.com/yuru7/HackGen/releases/download/v1.2.1/HackGen_v1.2.1.zip
$ unzip HackGen_v1.2.1.zip
```

### Install ccls
#### Install CMake>=3.8
```bash
$ cd ~/.emacs.d/
$ mkdir lib
$ cd lib
$ git clone -b release --depth=1 https://github.com/Kitware/CMake.git
$ cd CMake
$ ./bootstrap && make && sudo make install
$ echo "alias cmake=/usr/local/bin/cmake" >> ~/.bashrc
$ source ~/.bashrc
```
#### Install gcc>=7.2
```bash
$ sudo add-apt-repository ppa:ubuntu-toolchain-r/test
$ sudo apt install gcc7
```
#### Install ccls itself
```bash
$ cd ~/.emacs.d/lib
$ git clone --depth=1 --recursive https://github.com/MaskRay/ccls
$ cd ccls
$ wget -c http://releases.llvm.org/8.0.0/clang+llvm-8.0.0-x86_64-linux-gnu-ubuntu-18.04.tar.xz
$ tar xf clang+llvm-8.0.0-x86_64-linux-gnu-ubuntu-18.04.tar.xz
$ cmake -H. -BRelease -DCMAKE_BUILD_TYPE=Release -DCMAKE_PREFIX_PATH=$PWD/clang+llvm-8.0.0-x86_64-linux-gnu-ubuntu-18.04
$ cmake --build Release --target install
```
### Initialize Emacs
```bash
$ emacs
```
Then, you will get several messages after it initialized.  
And if the message does not include Error (if just warning).  
It succeed to install my emacs environment.

## Trouble Shooting
### The icon looks not good...
This occurs when the computer have not downloaded the icon fonts yet...
To fix this, 
1. run emacs
2. Do `M-x all-the-icons-install-fonts
