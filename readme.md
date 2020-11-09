# Introduction

Welcome!

*If you're here to install the prerequisites for the tutorial, you can do this either by following the [IDyOM installation instructions](https://www.wikihow.com/Install-Ubuntu-on-VirtualBox) up to step three (if you've already installed IDyOM you can skip this), or by following [my condensed instructions below](#installing-a-lisp-environment).
After that, skip ahead to [downloading jackdaw and IDyOM](#downloading-jackdaw-and-idyom) to download the materials for the tutorial. Feel free to read the introduction below too.*

In this tutorial, we'll cover some basic concepts in the jackdaw modeling framework and installation of jackdaw and some of its basic concepts.
You'll learn how to make a simple model using the framework and you'll learn how run an existing model, train it on a corpus, and generate and analyse simulation results.

Jackdaw is a framework for defining probabilistic models of sequences.
It is based on a formalism for defining probabilistic generative models (specifically, dynamic Bayesian networks) with deterministic constraints, described in Chapter 3 of [my PhD thesis](https://dare.uva.nl/search?identifier=dd3e25aa-6006-486e-afcf-c0692e0afacd).
The advantage of using this framework is that it allows you to construct a variety of probabilistic generative models by writing very little custom code.
For example, the code below is a complete and working implementation of (a version) of a metre perception model that I developed in my thesis.


```common-lisp
(defmodel enculturation (generative-model)
  ((ioi-domain :initarg :ioi-domain :reader ioi-domain)
   (meter-domain :initarg :meter-domain :reader meter-domain)
   (training? :initarg :training? :reader training? :initform nil))
  ((M (^m) (persistent (meter-domain model))
      :inputs (period pulses))
   (D (^d ^p)
      (recursive (loop for ioi in (ioi-domain model)
		       collect (cons (+ $^p ioi) $^d))
		 (deterministic '(*))))
   (P (^p m d)
      (recursive (loop for phase below (car $m)
		       collect phase)
		 (list (mod (car $d) (car $m)))))
   (IOI (d ^p)
	(recursive (list (- (car $d) $^p))
		   (list '*))
	:inputs (ioi)))
  ((D (m) (accumulator-model))
   (M () (categorical)))
  :required-fields (ioi-domain))
```

However, the same framework can be used to write similarly condensed versions of Temperley's (2007) meter-finding model, or of derived viewpoints in multiple-viewpoint systems.

## Jackdaw vs. IDyOM

Many of you may be familiar with [IDyOM](http://mtpearce.github.io/idyom/).
If you are, it may be more easy to understand what jackdaw models can do by comparing the framework to IDyOM.

Jackdaw is designed to model sequences of events, or *moments* as they are called in jackdaw.
These sequences may represent music or any other domain that can be represented as as a sequence.

Below are, in a nutshell, the main commonalities and differences between jackdaw and IDyOM.

They can both be used to

* *predict* each event/moment in a sequence that represents for example a melody
* calculate information-theoretic properties of events and sequences such as *information content*

Unlike IDyOM, however, jackdaw can

* be used to define a wider variety of models
* base its predictions on inferred underlying, but not directly observable, aspects of the sequence (such as its metre or key)
* infer posterior distribution of these aspects (i.e., what is the most likely metre/key given a melody)

IDyOM allows you to design a combinatorically exhaustive space of models defined by viewpoints and using procedures such as viewpoint selection.
Furthermore, it makes it easy to define new custom viewpoints to expand the space of possible models, or extent it to other domains or representations.
Similarly, jackdaw makes it easy to define models of sequences, but has greater expressive power than IDyOM.
Jackdaw, by contrast, does not focus provide tools for exploring an exhaustive space of possible models.

Finaly, jackdaw is less of a "batteries included" package than IDyOM; using it requires a bit more knowledge of Common Lisp and probabilistic modeling.

# Prerequisites

Familiarity with the Unix command line, Common Lisp, and probability theory are independently helpful but not strictly required.

If you use a modern Debian-based GNU/Linux distribution such as Ubuntu, you should be able to follow along by copying and pasting from the tutorial.

If you use Mac OS/X, you can copy and paste some of the commands below but need to follow alternative instructions for Mac in some places.

If you are using Windows, you are welcome to try and set everything up in.
If you know what you are doing, it should be possible.

Otherwise, I recommend installing Ubuntu in VirtualBox on Windows.
This installs the Ubuntu operating system in a *virtual machine*: a computer that is simulated from within Windows.
After doing this, you can boot up your virtual Ubuntu computer and follow the instructions below.

You could for example follow [these instructions](https://www.wikihow.com/Install-Ubuntu-on-VirtualBox), but download Ubuntu 20.04 (the Desktop edition), instead of 19.04.
It shouldn't be hard to do, but it might take some time.
While you don't need to attend to it all the time, downloading and installing Ubuntu may take between 10 and 60 minutes, depending on the speed of your computer and internet connection).

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
;;; The following lines added by ql:add-to-init-file:
  #-quicklisp
  (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                         (user-homedir-pathname))))
    (when (probe-file quicklisp-init)
      (load quicklisp-init)))
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
I'll assume throughout that you have done this and that all materials are in `~/jackdaw-tutorial' (on Windows, you could pick something like `C:\jackdaw-tutorial`).
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
If you were in the folder `~/jackdaw-tutorial` when you ran the above commands, these folders should be located at `~/jackdaw-tutorial/idyom` and `~/jackdaw-tutorial/idyom`.

In order to use the modified version of IDyOM, we need to "switch branches" (don't worry if you don't know what that means, you can just treat as magic that will make sure we're using the right version of IDyOM).
Just run

```
cd idyom
git checkout jackdaw-tutorial
cd ..
```

Now, you're all set to run jackdaw models!

The tutorial itself will be uploaded here later.
