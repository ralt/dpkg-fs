# Dpkg-Fs

Implementation of [pkgfs][0] for dpkg.

## Usage

    ./dpkg-fs /path/to/mount

## What's in it?

- Mountpoint features both `index/` and `installed/`!
- Each package has a description, version and size files.
- Packages have a `dependencies` folder with symlinks to their dependencies.
- Install packages via `/pkg/index/name/install`
- And uninstall via `/pkg/installed/name/uninstall`
- Sync to remote with `/pkg/sync`
- Installed packages have a `files/` directory, which contains the tree of files the package installed on the filesystem.

## Installation

Since this is still in development, no package is provided yet.

SBCL and quicklisp are build dependencies.

    $ git clone https://github.com/ralt/dpkg-fs ~/common-lisp/dpkg-fs
    $ cd ~/common-lisp/dpkg-fs
    $ make
    $ ./dpkg-fs /pkg

## Bonus

Here is a `dpkgfs.service` file you can put in `/etc/systemd/system/` to have `systemctl enable dpkgfs` available:

    [Unit]
    Description=dpkg as filesystem

    [Service]
    ExecStart=/home/$USER/common-lisp/dpkg-fs/dpkg-fs /pkg
    Type=simple

    [Install]
    WantedBy=multi-user.target


  [0]: https://docs.google.com/document/d/1Fi1ebe_rAq4v-JNW8i2IbT4iUHIPro-wbVT86tBhW14/edit#heading=h.y92gnqagqz2j
