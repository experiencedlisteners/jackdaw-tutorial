# Introduction

Welcome! In this tutorial, we'll cover some basic concepts in the jackdaw modeling framework and installation of jackdaw and some of its basic concepts.
You'll learn how to make a simple model using the framework and you'll learn how run an existing model, train it on a corpus, and generate and analyse simulation results.

Jackdaw is a framework for defining probabilistic models of sequences.
It is based on a formalism for defining probabilistic generative models (specifically, dynamic Bayesian networks) with deterministic constraints, described in Chapter 3 of my [PhD thesis](https://dare.uva.nl/search?identifier=dd3e25aa-6006-486e-afcf-c0692e0afacd).
The advantage of using this framework is that it allows you to construct a variety of probabilistic generative models by writing very little custom code.
For example, the code below is a complete and working implementation of (a version) of a metre perception model that I developed in my thesis.


```common-lisp
(defmodel enculturation (ppm-generative-model)
  (training? ioi-domain meter-distribution) 
  ((M ; meter
      (^m)
      (categorical ()
		   :training? (training? enculturation)
		   :p (meter-distribution enculturation))
      (persistent (meter-domain model))
      :key (lambda (m) (list (getf m 'period) (getf m 'pulses))))
   (D ; downbeat distance
      (^d ^p m)
      (accumulator-model (m)
			 :training? (training? enculturation))
      (recursive (loop for ioi in (ioi-domain model)
		       collect (cons (+ $^p ioi) $^d))
		 (deterministic '(*))))
   (P ; phase
      (^p m d)
      (uniform ())
      (recursive (loop for phase below (car $m)
		       collect phase)
		 (list (mod (car $d) (car $m)))))
   (IOI (d ^p ^ioi)
	(uniform ())
	(recursive (list (- (car $d) $^p))
		   (list '*)))))
```

Jackdaw is designed to model sequences of events, or *moments* as they are called in jackdaw.
These sequences may represent music or any other domain that can be represented as as a sequence.

Like IDyOM, jackdaw models can be used to *predict* what happens in the next moment in a sequence given what happened in previous moments.
It is for example possible to calculate *information content* of a melody, given a jackdaw model of melodies.
However, unlike IDyOM, the predictions of jackdaw models may be based on properties that are not explicitly represented such as the key or metre of a melody.
Such properties are represented by *latent variables*: aspects of an event or moment that are not *observed* (represented explicitly in the event), but must be *inferred*.
IDyOM allows you to design a combinatorically exhaustive space of models defined by viewpoints and using procedures such as viewpoint selection.
Furthermore, it makes it easy to define new custom viewpoints to expand the space of possible models, or extent it to other domains or representations.
Similarly, jackdaw makes it easy to define models of sequences, but has greater expressive power than IDyOM.
Simultaneously, it does not focus on exploring an exhaustive space of possible models.
Unlike IDyOM it does not "come with batteries included" and requires more knowledge of Common Lisp and probabilistic modeling to use.

# Prerequisites

Familiarity with the Unix command line, Common Lisp, and probability theory are independently helpful but not strictly required.
If you use a modern Debian-based GNU/Linux distribution, you should be able to follow along by copying and pasting from the tutorial.
If you use Mac OS/X, you can copy and paste some of the
You are welcome to try and set everything up in Windows.
If you know what you are doing it should be possible.
For Windows users, I recommend installing VirtualBox and either downloading a pre-installed ubuntu image or installing it yourself (if you don't know what this means I recommend doing the former).

## Setup

In order to easily run jackdaw models, you'll need to install SBCL, Emacs, and Quicklisp and to setup SLIME.

* SBCL is a widely used open-source Common Lisp implementation.
* Emacs is a text editor (and much, much more) whose history is intertwined with Lisp's. It is not strictly required to use Emacs to write Lisp code, but will make it much easier and it will allow you to run your code from within the editor.
* Quicklisp is a package manager for Common Lisp, which we'll use to install dependencies for IDyOM and jackdaw and for loading IDyOM and jackdaw.
* SLIME stands for "Superior Lisp Interaction Mode for Emacs" and pretty much does what it says on the tin. It'll make it possible to run your Lisp code from within Emacs.
* Finally, Jackdaw can optionally make use of IDyOM's implementation of variable-order Markov models to predict sequences, therefore, we'll download a slightly modified version of IDyOM too.

If you've already installed IDyOM you should be all set.
The only thing you need to do is download jackdaw and a slightly customized version of IDyOM supplied with this tutorial.
You can download this version of IDyOM alongside your existing IDyOM installation.
Don't worry; it won't interfere with your existing IDyOM installation and "uninstalling" is as easy as removing the downloaded files after you're done with this tutorial.
To do this, skip ahead to [downloading jackdaw and IDyOM](#downloading-jackdaw-and-idyom)

If you haven't installed SBCL, Emacs and Quicklisp, you can either follow the instructions on [this IDyOM wiki page](https://github.com/mtpearce/idyom/wiki/Installation) *up to step three* and skip ahead to [downloading jackdaw and IDyOM](#downloading-jackdaw-and-idyom).
These instructions apply to both Mac and Linux.
Alternatively, if you are using a Debian-based Linux, you can follow the condensed instructions [below](#installing-a-lisp-environment).

# Installing a Lisp environment

Below I'll walk you through installing software that make it easy to write and interact with Common Lisp code.

First, install *Emacs and SBCL*.
Open a terminal and run

```
sudo apt-get update && sudo apt-get -y install emacs sbcl
curl -O https://beta.quicklisp.org/quicklisp.lisp
```

To install *Quicklisp* and make sure that SBCL loads it automatically, run 

```bash
echo '(quicklisp-quickstart:install)(ql:add-to-init-file) | sbcl --load quicklisp.lisp
```

Now run

```bash
echo '(ql:quickload "quicklisp-slime-helper")'  | sbcl
```

Copy the last bit of the output that the output tells you to copy and paste it add the end of `~/.emacs` (create the file if it does not exist).
The lines that you're told to copy will look something like this:

```commonlisp
;;; The following lines added by ql:add-to-init-file:
  #-quicklisp
  (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                         (user-homedir-pathname))))
    (when (probe-file quicklisp-init)
      (load quicklisp-init)))
```

That's it! You're now ready to use jackdaw.

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
I'll assume throughout that you have done this and that all materials are in `~/jackdaw-tutorial'.
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

This will create two folders, `jackdaw` and `idyom`, in `~/jackdaw-tutorial`.

In order to use the modified version of IDyOM, we need to "switch branches" (don't worry if you don't know what that means, you can just treat as magic that will make sure we're using the right version of IDyOM).
Just run

```
cd idyom
git checkout jackdaw-tutorial
cd ..
```

Now you're all set to run jackdaw models!

# Loading jackdaw in Emacs

If you're already familiar with Emacs and SLIME you can skip most of the next two sections

### Emacs basics

You can use Emacs with a mouse mostly like you would a simple text editor like Notepad, but basic keyboard shortcuts that you might be used to like Ctrl-S for save and Ctrl-C and Ctrl-V for copy and paste are a little different, because Emacs offers a much more extensive palette of keyboard commands than most editors.
Learning these will take a while but it is a worthy investment if you plan to use Emacs and Lisp more often.

An overview of the basic commands that might be useful immediately (like copying, pasting, saving, exiting) can be found [here](http://mally.stanford.edu/~sr/computing/emacs.html), but in principle you should be able to get along using just the mouse and arrow keys.

Most key combinations involve either the `Control` or `Meta` key.
The `Meta` key corresponds to the `Alt` key on Linux and Windows machines and to the `⌘` on Mac machines. 
We'll be using the shorthand `C` and `M` for these keys.

### Loading SLIME and interacting with SBCL

Open Emacs.
You'll be greeted with an information page.
I recommend enlarging the Window so it takes up all or most of the screen.

Now start slime by pressing `M-x`.
This moves the cursor to the narrow box at the bottom (called *mini buffer* in Emacs terminology), which you can think of as a space to give commands to Emacs.

Type `slime` and hit enter.

This will open a new window with an SBCL session.

<img src=img/repl.png height=50>

This session can be thought of as a command line, except that in Lisp lingo it is called the REPL (*read-eval-print loop*).

Here, you can type and evaluate lisp *forms* (think of it as a command).
For example, try to type `(+ 1 1)` and hit enter.
If the input is complete, SBCL will evaluate the form and print the result.

Now try typing `(print "Hello world")` and hit enter.

An input will evaluated only if it is complete.
For example, on a new, empty line, type `(print` and hit enter.
As you can see, nothing happens and SLIME is waiting for more input.
To complete the form, you must match all openened brackets.

To return to the state where we can enter a new command, type the missing closing bracket `)` and hit enter.
Now, you will first be shown an error message (either in another window, or in the same window), because our input was `(print)`, and the print function requires an argument.
You can close the error message and return to the REPL by pressing `q`.

You can cycle back to commands entered previously by typing `M-p` (for *previous*) and `M-n` (for *next*, if you've gone back too far).
After you've brought up a command, you can edit it to re-run it.
`M-p` will actually perform a backwards search using all characters up to the cursor position to match commands, if you just want to go to the previous command, make sure the cursor is located at the beginning of the line.

These habits can save you a lot of typing, so try it out: place the cursor on the beginning of the line and press `M-p` twice to bring back the `(print "Hello world")` form.
Press `M-n` once to cycle forward to the erronous `(print)` form.

# Jackdaw basics

Jackdaw allows you to define generative models (Bayesian networks) defined by a set of random variables, each of which is described by a (discrete) conditional probability distribution.

In a Bayesian network, each random variable can be thought of as a node in a network graph.
Defining a model in jackdaw entails defining the network graph that describes the dependency relations between random variables.
In order to do this, you only need to specify the *parents* of each random variable.

For example, a simple generative model involving two random variables, `X` and `Y`, could be defined as the product of two probability distributions: `P(X)P(X|Y)`.
Here, `X` corresponds to  a root node in the network graph and `Y` has one parent, namely `X`.
All you need to know in order to draw the graph is that `X` has no parents (it is a root node) and `Y` has one (namely `X`).

The network graph thus looks like this: 

```
X --> Y
```

A generative model is nothing more than a particular way of describing the probability of a set of stochastic events.

For example, the generative model above could be used to define a model of the probability of the outcome of a coin toss Jack's mood.
Of course we simplify things compared to the real world and assume that the possible outcomes of a coin toss are `{heads, tails}` and the possible states of Jack's mood are `{happy, sad}`.
The coin is a fair coin, so `P(Coin = heads) = P(Coin = tails) = 0.5`.
Let's say that Jack always bets on heads and that if he wins, he's happy with 80% probability, so `P(Mood = happy | Coin = heads) = 0.8` and `P(Mood = sad | Coin = heads) = 0.2`.
Furthermore, Jack is not a sore loser and if he loses (Coin = tails), he's happy with 50% probability.

We are now going to implement this model in jackdaw, but first we need to load it.

## Loading jackdaw 

The form below can be used to load jackdaw.

```common-lisp
(let ((idyom-root "~/jackdaw-tutorial/idyom/")
      (jackdaw-root "~/jackdaw-tutorial/jackdaw/"))
    (defvar *idyom-root* idyom-root)
    (push idyom-root asdf:*central-registry*)
    (push jackdaw-root asdf:*central-registry*)
    (ql:quickload "jackdaw"))
```

Copy it in its entirety and paste it (C-y in Emacs, or use the paste option from the edit menu) onto an new and empy line of the REPL and hit enter.
If you've saved IDyOM and jackdaw in custom locations you will need to replace the `~/jackdaw-tutorial` parts with the paths to idyom and jackdaw.
Don't forget the trailing `/`.

The first time you run this command, it will download a bunch of packages on which jackdaw depends.
One of these dependencies is IDyOM, 
After the dependencies have been downloaded and loaded, jackdaw will be loaded.

If you've used IDyOM before, you'll be familiar with the `(start-idyom)` command.
The form above can be thought of as the equivalent of the `(start-idyom)` command for jackdaw.
In fact, `(start-idyom)` is a shortcut for some parts of the above form, except that the above form does not provide a database connection.

To run the forms in the remainder of this tutorial, enter the following form:

```common-lisp
(in-package #:jackdaw)
```

This ensures that we're in jackdaw's namespace so we can simply type `defmodel` instead of `jackdaw:defmodel`.

## A first jackdaw model

In jackdaw, this generative model of Jack's mood can be defined as follows:

```common-lisp
(defmodel jack () ()
   ((Coin () (bernouilli () :p .5 :psymbol 'heads)
          '(heads tails))
    (Mood (coin) (bernouilli (coin) :p '(((heads) . .8)
                                         ((tails) . .5))
                                    :psymbol 'happy)
          '(happy sad))))
```

Copy and paste (C-y) this model definition onto the REPL.
After pressing enter, the model will be loaded into jackdaw.

*NOTE: the first time you run this, it will print a lot of warnings in scary red capital letters.* This is a bug that I did not have time to fix before this tutorial. If you bring the model definition up again (M-p) and re-load it it should no longer print any warnings. If you still see errors or warnings, something has gone wrong.

The defmodel macro will define the model and define a function that we can invoke to obtain an *instantiation* of the model.
The name of this function depends on the name (class) of the model and is called `make-CLASS-instance`, where `CLASS` is replaced by the name of the model.
In our case, the model was named `jack`, so we can create an instance of it by invoking `(make-jack-instance)`.

But not so fast! What can we actually do with the model? 

Jackdaw's main interface to using a jackdaw model consists of the `PROCESS-SEQUENCE` and `PROCESS-DATASET` (a dataset, in this case, is simply a list of sequences) functions.
Recall that the framework is designed for sequential data. 
These sequences consist of events, or *moments* as they are called in jackdaw.
Each moment corresponds to an (partially observable) outcome of the generative model.

In our example, a moment is an instance of a coin having been tossed *and* Jack being happy or sad.

Let's first model a single moment.

Jackdaw's primary function is to generate all the possible outcomes of the model and constrain them using the values of observed variables.
In this case, we have two variables and (since we haven't explicitly *hidden* them) they are both observable.

All possible outcomes of the model involve all combinations of Jack's mood and the outcome of a coin toss.

To do this, copy and paste the following code (don't worry about understanding all of it):

```common-lisp
(let ((probabilities:*log-space* nil) ; disable the use of log probabilities
      (m (make-jack-instance :output t))) ; make an instance of the jack model
  (let ((states (process-sequence m '((:coin tails :mood happy))))) ; process a sequence
    (format t "~%Evidence: ~a~%~%" (evidence m states)) ; print the model evidence
    (dolist (state states) ; print each congruent state
      (pprint-state m state)))) 
```

Before you run it, let's try to understand some of it.
You can ignore the bit on the first line involving `probabilities:*log-space*`, this just ensures that probabilities are printed in a format that's legible by mortals.

On the second line, an instance of the `jack` model is created and associated with the symbol `m`.
Notice that we pass it a *keyword* parameter (this is signaled by the fact that it is preceded by a parameter name `:output`, which starts with a colon).
This parameter specifies whereto `jackdaw` should write its output.
In this case `t` causes jackdaw to write output to the REPL, but we could also redirect the output to a file.

On the third line, we are using `process-sequence` to evaluate the model on a sequence consisting of one moment: `((:coin tails :mood happy))`.
In this moment, we are observing that the value of `Coin` is tails and the value of `Mood` is happy.
A sequence of two moments would look like this `((:coin tails :mood happy) (:coin heads :mood happy))`.

The final two lines print some additional information that we'll discuss in a moment.

## Jackdaw's output

Now, hit enter to run the code.

The output generated by `process-sequence` is shown below.

```
sequence,moment,mood,coin,congruent,probability
NIL,0,HAPPY,TAILS,T,0.25
NIL,0,SAD,TAILS,NIL,0.25
NIL,0,HAPPY,HEADS,NIL,0.4
NIL,0,SAD,HEADS,NIL,0.099999994
```

Notice that this output is in CSV format.
This data can be written to a file for further analysis using data analysis tools such as R or Python+pandas.

Each row in this table represents a *state* of the *joint distribution* of the generative model.
A state represents one of the possible outcomes that the model can generate; in this case the outcome of the coin toss together with Jack's mood.

The first column can be ignored, it refers to the sequence index when processing multiple sequences with `process-dataset`.

The second column indicates the moment index.
Since we're processing only a single moment, it is always zero.

The next two columns represent the state (Jack's mood and the outcome of the coin toss).

The penultimate column is called "congruent" and represents whether the state is congruent.
Congruent, in jackdaw terminology, indicates whether the state is congruent with the observation.
Since we can observe the value of all variables, only one state is congruent (indicate by `T`, which means "true" in lisp; `NIL` means "false"), namely the state in which Mood=happy and Coin=tails, corresponding to the values provided in the moment.

The probability of this state corresponds to the probability `P(Mood = happy, Coin = tails) = P(Coin = tails)P(Mood = happy | Coin = tails)`.

**Excercise.** verify using the probability distributions given earlier that this probability is correct.

## Impossible moments

Note that jackdaw prints a warning when it none of its states are congruent.

**Excercise.** try setting the value of mood in the first moment to something that the model cannot generate and see what happens when you try to process this sequence.

## Model evidence 

After the output of `process-sequence`, we see a line that says "Evidence: " followed by a probability.
Thus number represents the *model evidence* which is defined as *the sum of the probabilities of all states in which the observed variables have their observed value.*
In other words, the sum of the probabilities all congruent states.

The evidence value printed in the output can be seen as the probability that the model assigns to the situation in the moment, or sequence of moments in its input.

**Excercise.** use jackdaw to calculate the probability that in two subsequent moment, the coin toss is heads and Jack is happy in the first moment and the coin toss is tails and Jack is unhappy in the second moment. (If you get stuck here, ask me for clarification!)

So far, we've only seen so-called *fully observed* models, in which there is only one congruent states.
However, we can make the model *partially observed* by *hiding* one of its variables.

Imagine that we can only observe whether Jack is happy, but we know that there is a causal relation between Jack's mood and the outcome of the coin toss (in other words, we know the generative model that underlies Jack's mood).

The function `hide` can be used to hide variables in an instance of a jackdaw model.
Let's hide the value of coin and run process sequence again.

```
(let ((probabilities:*log-space* nil) 
	  (m (make-jack-instance :output t)))
  (hide m 'coin) ; <----- here we hide the variable Coin
  (let ((states (process-sequence m '((:coin tails :mood happy))))) 
    (format t "~%Evidence: ~a~%~%" (evidence m states))
    (dolist (state states)
       (pprint-state m state))))
```

We can leave the value of coin in the representation of the moment, or we can take it out.
It doesn't matter; jackdaw will ignore it.

Run the code above and observe what changes with regard to which states are congruent in the output.
Also note what happens to the model evidence, which now represents the *marginal* probability that Jack is happy.

**Excercise.** What is the marginal probability that Jack is sad? HINT: you can change the moment processed, or you can use jackdaw's output. In case you find this too easy to calculate without jackdaw, make sure you understand how you could read this probability from jackdaw's output, or try the next excerise, which is harder to do manually.

**Excercise.** Use jackdaw to calculate the probability that Jack is sad in two subsequent moments.

**Excercise.** Try hiding all variables. What happens to the model evidence, and why?

## Inference

Model evidence can be used to perform inference.
For example, if we are interested in the probability that the coin was tails given that Jack is unhappy, we can apply Bayes formula to find that.

`P(Coin = heads | Mood = sad) = P(Mood = sad | Coin = heads)P(Coin = heads) / P(Mood = sad)`.

**Excercise.** Calculate this probability. HINT: remember that `P(Mood = sad | Coin = heads)P(Coin = heads) = P(Mood = sad, Coin = heads)`.

Jackdaw does not yet have any utility functions for performing inference since I have tended to perform such calculations in the data analysis phase using Python+pandas.

## A closer look at defmodel

*NOTE: This section can be skipped if you're prefer understanding the big picture over the details*

As we've seen, jackdaw models are defined using a macro called `DEFMODEL`.

To explore its syntax, on a fresh line in the REPL, typ `(DEFMODEL ` (don't hit enter).
In the mini-buffer (the bar at the bottom of the screen), SLIME will show you the arguments to `DEFMODEL`:

`(DEFMODEL CLASS SUPERCLASSES PARAMETERS VARIABLES)`

`CLASS` is the name of the model (for those familiar with Common Lisp, the model is stored as a CLOS class).

`SUPERCLASSES` is a list of superclasses.
It allows you to inherit properties from other models (this is not covered this here).

`PARAMETERS` is a list of model parameters.

`VARIABLES` is a list of variable definitions.

Each variable definition in `VARIABLES` has the following structure:

`(NAME PARENTS DISTRIBUTION CONSTRAINT &KEY OUTPUT KEY FORMATTER HIDDEN)`

For example, we defined the variable describe Jack's mood as `(Mood (coin) (...) '(heads tails))`.

`NAME` is the name of the variable. This can be any valid lisp symbol like `X` or `A-very-long-name!`.

`PARENTS` is a list of parent-variables, e.g., `(coin)`.

`DISTRIBUTION` is a definition of the probability distribution of the possible values of the variable (see below).

`CONSTRAINT` is defines the possible values of the variable as a function the values of `PARENTS`.
It can also be simply a list of possible values, such as `'(happy sad)`.

Everything after `&KEY` represents optional keyword arguments.
We will discuss some of these later.

Finally, `DISTRIBUTION` definitions follow this format: `(CLASS ARGS PARAMETERS)`.
For example, when we defined the distribution of the outcome of a coin toss as follows

```common-lisp
(bernouilli (coin) :psymbol 'happy
                   :p '(((heads) . .8)
                        ((tails) . .5)))
```

The `ARGS` of the distribution are `(coin)` indicating that it is conditioned on the value of coin.
The distribution has two parameters, the probability of the symbol indicated by `:psymbol` given each possible value of `coin`.

The class of the distribution must be one of the distributions impemented in `jackdaw/distributions.lisp`.
The implementation is not very complete yet, but there is a Bernouilli distribution, a categorical distribution, and a conditional probability distribution estimated by a variable-order Markov model, using IDyOM's implementation of PPM.

## Parameterization

So far, we written the parameters of the two Bernouilli distributions (binary probability distributions) in the model definition.
However, it is easy to define modify the model so that these parameters can be given each time an instantiation is created.
To do this, we use the `PARAMETERS` field of defmodel (see the [previous section](#a-closer-look-at-defmodel)):

```common-lisp
(defmodel jack () (p-happy-win p-happy-lose &key (p-heads 0.5))
   ((Coin ()
          (bernouilli () :p (p-heads jack) :psymbol 'heads)
          '(heads tails))
    (Mood (coin)
          (bernouilli (coin) :psymbol 'happy 
                             :p (list (cons '(heads) (p-happy-win jack)) 
                                      (cons '(tails) (p-happy-lose jack))))
	  '(happy sad))))
```

We've now given the model three customizable parameters: p-happy-win and p-happy-lose, representing the probabilities that Jack is happy given that he wins or loses (corresponding to whether the toss comes up heads or tails since he always bets heads).

The final parameter, p-heads, is an optional keyword parameter that assumes the default value of 0.5 if it isn't provided.

The effect of this is that the function `make-jack-instance`, now accepts additional parameters.
You can see this by typing `(make-jack-instance` on a fresh line in the REPL and looking at the mini-buffer in the bottom left of your screen, where SLIME will show something that looks like this:

<img src=img/slime-help.png height=50>

For example, to obtain the same parameterization we used before, the model should be instantiated as follows:

```(make-jack-instance 0.8 0.5)```

To see what happens to Jack's mood if the coin is unfair, we can instantiate the model as follows:

```(make-jack-instance 0.8 0.5 :p-heads 0.1)```

**Excercise.** Try to modify the code we used before to process moments to work with the parameterized model and calculate `P(Mood = sad)` with different kinds of unfair coins. Experiment with very low and very high probabilities of heads.

# Deterministic constraints

The fact that a constraint defines the possible values of the variable as a function of the values of its parents represents what distinguishes a jackdaw model from a regular Bayesian network.
In a regular Bayesian network, random variables always generate all of their values.
In a jackdaw model, the values generated by a variable are a function of the values of its parents.

It turns out that this twist makes it possible to give very compact definitions of some existing generative models, such as Temperley's Bayesian meter perception model, the semantics of derived viewpoints of a multiple viewpoint system, and the meter perception model proposed in my PhD dissertation.

The following (pointless) model illustrates how this works:

```common-lisp
(defmodel logical-and () ()
   ((P () (uniform ()) '(nil t) :hidden t)
    (Q () (uniform ()) '(nil t) :hidden t)
    (P&Q (P Q) (uniform ()) (list (and $p $q)) :hidden t)))
```

Since we're temporarily not interested in probabilities, all variables are defined to have a uniform probability distribution and are set to be hidden by default.

We've defined three variables.
The third variable, `P&Q` depends on both `P` and `Q`.
It is an example of a *deterministic variable* since it always generates one outcome whose value corresponds to the logical and of `P` and `Q`.

The part of the variable definition that defines its possible values as a function of its dependencies (parents in the network graph) is called the constraint function.

The constraint function of `P&Q` is `(list (and $p $q))`.
Notice that the values of the parents of `P&Q` are accessible to this function by their names preceded by `$`.

To see the possible outcomes of this model, run the following snippet.

```
(let ((probabilities:*log-space* nil) 
	  (m (make-logical-and-instance :output t)))
  (process-sequence m '(())))
```

Pay close attention to how the value of `P&Q` depends on `P` and `Q`.

The function `make-observable` can be used to make a hidden variable observable.

As an excersise, try to make `P&Q` observable, and study what happens to the congruent states when observing different values of `P&Q` (`t` or `nil`), using the code snippet below.

```
(let ((probabilities:*log-space* nil) 
	  (m (make-logical-and-instance :output t)))
  (make-observable m 'p&q)
  (process-sequence m '((:p&q t))))
```

# That's it for now

The deterministic constraints described in the last section may seem somewhat arbitrary, but they become useful when describing more complex models.
For an example of such a model, see the enculturation model displayed at the beginning of this tutorial.

There's much more to cover: one-shot variables, recursive variables, using jackdaw with the CLI interface, more complex models such as derived viewpoints of a multiple viewpoint system, models with meter and key as hidden variables.

Let me know if you're interested, I'll be glad to help.
