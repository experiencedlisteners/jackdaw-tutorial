# Jackdaw tutorial

This repository contains a few hands-on tutorials on using Jackdaw for probabilistic generative modeling as well as sample data used in these tutorials.

*If you're here to install the prerequisites for the tutorial, you can do this either by following the [IDyOM installation instructions](https://www.wikihow.com/Install-Ubuntu-on-VirtualBox) up to step three (if you've already installed IDyOM you can skip this), or by following [my condensed instructions below](#installing-a-lisp-environment).
After that, skip ahead to [downloading jackdaw and IDyOM](#downloading-jackdaw-and-idyom) to download the materials for the tutorial. Feel free to read the introduction below too.*

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

# Tutorials

[0. Prerequisites](-1-installation)

[1. Installation](0-installation)

[2. The basics ](1-the-basics)

3\. Defining and training models (coming soon)
