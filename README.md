# alien-works-demo

Demonstration of [`alien-works`](https://github.com/borodust/alien-works) features.

[Git LFS](https://git-lfs.github.com/) required to properly clone and manage
this repository.

## Requirements

* `X86_64` `Linux`-based system equal or newer than Ubuntu 18.04
* OpenGL 4.1+
* CPU with AVX support (since around 2011)

## Installation

#### Quicklisp
```lisp
(ql-dist:install-dist "http://dist.borodu.st/alien-works.txt" :prompt nil)

(ql:quickload :alien-works-demo)

(alien-works:run)
```

#### Bundle

Go to https://github.com/borodust/alien-works-demo/releases and download latest
version.

```sh
chmod +x /path/to/alien-works-demo-x86_64.AppImage

/path/to/alien-works-demo-x86_64.AppImage

# or with firejail for added security
firejail --profile=steam --appimage /path/to/alien-works-demo-x86_64.AppImage
```

## Troubleshooting

#### Quicklisp
I'm not taking much care of my alien-works quicklisp dist, so if you
encountering weird errors about missing packages, just reinstall the dist:

```lisp
(ql-dist:install-dist "http://dist.borodu.st/alien-works.txt" :prompt nil :replace t)
```
