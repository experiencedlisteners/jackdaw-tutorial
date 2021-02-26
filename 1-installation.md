# Installation

## Required tools

In order to easily run jackdaw models, you'll need to install SBCL, Emacs, and Quicklisp and to setup SLIME.

* SBCL is a widely used open-source Common Lisp implementation.
* Emacs is a text editor (and much, much more) whose history is intertwined with Lisp's. It is not strictly required to use Emacs to write Lisp code, but will make it much easier and it will allow you to run your code from within the editor.
* Quicklisp is a package manager for Common Lisp, which we'll use to install dependencies for IDyOM and jackdaw and for loading IDyOM and jackdaw.
* SLIME stands for "Superior Lisp Interaction Mode for Emacs" and pretty much does what it says on the tin. It'll make it possible to run your Lisp code from within Emacs.
* Finally, Jackdaw can optionally make use of IDyOM's implementation of variable-order Markov models to predict sequences, therefore, we'll download a slightly modified version of IDyOM too.

If you've already installed IDyOM you should be all set.
The only thing you need to do is download jackdaw and a slightly customized version of IDyOM supplied with this tutorial.
To do this, skip ahead to [downloading jackdaw and IDyOM](#downloading-jackdaw-and-idyom)

You can download this version of IDyOM alongside your existing IDyOM installation.
Don't worry; it won't interfere with your existing IDyOM installation and "uninstalling" is as easy as removing the downloaded files after you're done with this tutorial.

If you haven't installed SBCL, Emacs and Quicklisp, you can either follow the instructions on [this IDyOM wiki page](https://github.com/mtpearce/idyom/wiki/Installation) *up to step three* and skip ahead to [downloading jackdaw and IDyOM](#downloading-jackdaw-and-idyom).
These instructions apply to both Mac and Linux.
Alternatively, if you are using a Debian-based Linux, you can follow the condensed instructions [below](#installing-a-lisp-environment).

# Installing a Lisp environment

Below, I'll walk you through the installation of an implementation of Common Lisp and some software that makes it easy to write and interact with Common Lisp code.

First, install *Emacs and SBCL*.
Open a terminal and run

```
sudo apt-get update && sudo apt-get -y install emacs sbcl
curl -O https://beta.quicklisp.org/quicklisp.lisp
```

To install *Quicklisp* and ensure that SBCL loads it automatically, run 

```bash
echo '(quicklisp-quickstart:install)(ql:add-to-init-file) | sbcl --load quicklisp.lisp
```

Now run

```bash
echo '(ql:quickload "quicklisp-slime-helper")'  | sbcl
```

to install SLIME.

Copy the last bit of the output that the output tells you to copy and paste it add the end of `~/.emacs` (create the file if it does not exist).
The lines that you're told to copy will look something like this:

```common-lisp
(load (expand-file-name "~/quicklisp/slime-helper.el"))
;; Replace "sbcl" with the path to your implementation
(setq inferior-lisp-program "sbcl")
```

## Installing git

You will need to have *git* installed.
On Debian-based Linuxes, run

```bash
sudo apt-get update && sudo apt-get -y install git
```

Otherwise, see [this page](https://git-scm.com/book/en/v2/Getting-Started-Installing-Git) and follow the instructions for your OS.

## Downloading jackdaw and IDyOM

In order to keep all materials for this workshop in one place, I recommend that you to create a single directory in which you download all the materials.
In fact, you can simply clone this tutorial into your home directory.
I'll assume throughout that you have done this and that all materials are in `~/jackdaw-tutorial` (on Windows, you could pick something like `C:\jackdaw-tutorial`).
You can download these files to any other location if you prefer, just remember to correct the paths mentioned below to your custom location.

To clone this tutorial, open a terminal (make sure you've [installed git](#installing-git)) and type

```bash
cd ~/
git clone https://github.com/experiencedlisteners/jackdaw-tutorial.git
cd ~/jackdaw-tutorial
```

Now clone jackdaw and idyom

```bash
git clone https://github.com/experiencedlisteners/jackdaw.git
git clone https://github.com/experiencedlisteners/idyom.git
```

This will create two folders: `jackdaw` and `idyom`.
If you were in the folder `~/jackdaw-tutorial` when you ran the above commands, these folders should be located at `~/jackdaw-tutorial/idyom` and `~/jackdaw-tutorial/jackdaw`.

In order to use the modified version of IDyOM, we need to "switch branches" (don't worry if you don't know what that means, you can just treat as magic that will make sure we're using the right version of IDyOM).
Just run

```
cd idyom
git checkout jackdaw-tutorial
cd ..
```

Now you're all set to run jackdaw models!
