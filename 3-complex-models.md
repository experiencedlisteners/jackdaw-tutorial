# Preface

Recall that last time, we learned how to define a parameterised model:

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

And we saw how *congruency constraints* can be used to restrict the possible values that random variables may assume in a generative model:

```common-lisp
(defmodel logical-and () ()
   ((P () (uniform ()) '(nil t) :hidden t)
    (Q () (uniform ()) '(nil t) :hidden t)
    (P&Q (P Q) (uniform ()) (list (and $p $q)) :hidden t)))
```

Since then, a few small things have already changed!

## Changes to the API

There have been some small changes to the API.
The code below shows how the model above is defined in the updated version of jackdaw:

```common-lisp
(defmodel jack (dynamic-bayesian-network)
  (p-happy-win p-happy-lose &key (p-win 0.5))
   ((Coin () (bernouilli () :p p-win :psymbol 'win)
          '(heads tails)
          :hidden t)
    (Mood (Coin)
          (cpt (coin) :alist-cpt
                      (list (cons '(happy heads)  p-happy-win)
                            (cons '(sad heads)    (- 1 p-happy-win))
                            (cons '(happy tails) p-happy-lose)
                            (cons '(sad tails)   (- 1 p-happy-lose))))
          '(happy sad))))
```

Note that

* model parameters are no longer referenced by `(parameter model-name)`, but simply by `parameter`
* Bernouilli distributions can no longe be conditional distributions
    * Instead, you can use the more general `CPT` (for *conditional probability table*) distribution
* You should specify whether the superclass of the model as `dynamic-bayesian-network`
    * by default, models will be subclasses of `bayesian-network`, but since regular Bayesian networks are not fully supported yet, we'll exclusively use `dynamic-bayesian-network`
* model variables are latent by default, we'll see below how to hide and observe variables

Some other changes:

* `MAKE-XYZ-INSTANCE` is now called `MAKE-XYZ-MODEL`
* We won't be using CSV output as much as the API now offers better methods for inspecting model behaviour
* `MODEL-SEQUENCE` is no more, we now use `GENERATE`, as explained below
* Jackdaw now *does* have tools for showing posterior distributions 

## Updating and loading jackdaw

If you have already downloaded jackdaw and IDyOM, make sure you have the latest version by navigating to the folder where you downloaded jackdaw and running 

```bash
git pull
```

If you haven't, follow the instructions [here](1-installation.md) to get set up.

Loading jackdaw can be done in two ways:

(1) This is what we did last time.
Open Emacs, hit M-x, type slime, and hit enter to get a REPL.
On the REPL, type the following and hit enter.

```common-lisp
(let ((idyom-root "~/jackdaw-tutorial/idyom/")
      (jackdaw-root "~/jackdaw-tutorial/jackdaw/"))
    (defvar *idyom-root* idyom-root)
    (push idyom-root asdf:*central-registry*)
    (push jackdaw-root asdf:*central-registry*)
    (ql:quickload "jackdaw"))
```

(2) (I have only tested this on a Linux system.)

```bash
mkdir -p ~/.local/share/common-lisp/source/jackdaw-models
ln -sf ~/jackdaw-tutorial/jackdaw ~/.local/share/common-lisp/source/jackdaw
ln -sf ~/jackdaw-tutorial/idyom ~/.local/share/common-lisp/source/idyom
```

Change `~/jackdaw-tutorial/jackdaw` and `~/jackdaw-tutorial/idyom` to wherever you downloaded jackdaw and IDyOM (but make sure it's the patched IDyOM from `github.com/experiencedlisteners/idyom`, see [the installation instructions](1-installation.md)).
After creating these links, you can load jackdaw by running `(ql:quickload "jackdaw")` in SLIME.

## Running the examples in this tutorial

Last time, we were working directly within the jackdaw package by using `(cl:in-package :jackdaw)`.
This time, we'll do things a bit more cleanly: after loading jackdaw (with `(ql:quickload "jackdaw")`), we'll use the alias for jackdaw, `jd` to access functions, methods, macros, and classes defined by jackdaw.
I recommend that you open a text file in Emacs where into which you copy the model definitions given below.
You can load them into the REPL session by placing the cursor in a model definition and hitting `C-c C-c`.
The other code given in the tutorial can be evaluated interactively on the REPL. 

# Overview

In the previous part of the tutorial, we learned how to define a network of random variables using jackdaw and studied how to impose deterministic constraints on the values that can be assumed by the variables in the model.

In this part we look at some more complex models and introduce some new concepts.

* Model estimation
* Inference with evidence and posterior distribution
* Modeling sequences
    * Backward references and recursive dependencies
    * Language models
* Music models
    * Derived viewpoints
    * Tonality

The tutorial also includes links to a number of examples of more complex models: a language classifier model, a general implementation of viewpoints in jackdaw, and a model of rhythm and meter.

# Estimating models and modelling sequences

Before we get to more interesting models, we need to work through two less exciting examples to introduce some new concepts: estimating models from data and modelling sequences with Markov variables, ngram models, and PPM sequence models.

## Model estimation and inference

Below, we'll see how to use jackdaw to perform simple maximum likelihood estimation and to inspect model evidence and posterior distributions.

We will not return to our old friend Jack, but to his friend, Jackie, who doesn't bet on coin flips, but plays an unnamed game in which the probability that she wins or loses depends on whether she's alert or sleepy.

Last, time we defined a parameterised model of Jack's mood.
Since we're going to estimate the model from data, we may as well leave such parameters out of Jackie's model.
Technically, we only remove the parameters from the generative model as a whole.
The parameters still exist as parameters of the probability distributions that constitute the model.

```common-lisp
(jd:defmodel jackie (jd:dynamic-bayesian-network) () 
  ((Focus ()
	 (jd:bernouilli ())
	 '(alert sleepy)
	 :observer #'first)
   (Game (Focus)
	 (jd:cpt (Focus))
	 '(win lose)
	 :observer (lambda (m) (if (listp m) (second m)  m)))))
```

Note that we have included an *observer function* for each variable.
Observer functions dictate how the value of the variable should be observed in a moment.
The observer function for Game is defined such that it works both if both Focus and Game are observed and when only Game is observed.
Thus `(sleepy win)` is a moment that encodes both Focus and Game, and `lose` is a moment that encodes just Game.

Let's create an instance of Jackie on the REPL:

```common-lisp
CL-USER> (defparameter *jackie* (make-jackie-model))
```

Now let's estimate the model.
In order to do this, we need to make sure the model is *fully observed*.
This is done as follows:

```common-lisp
CL-USER> (jd:observe *jacke* 'Focus 'Game)
```

Now we need some data.
Remember that Jackie is dynamical Bayesian network.
That means that a single observation corresponds to a *sequence of moments* (of arbitrary length).
Since we estimate a model from a *set of observations*, we'll need to create a list of sequences of moments.
Since we're observing both Focus and Game, each moment is a list `(focus game)`.

```common-lisp
(let ((data '(((sleepy lose) (sleepy win) (sleepy lose))
	          ((alert lose) (alert win) (alert win) (alert win))
	          ((sleepy lose) (alert win) (sleepy lose) (sleepy win)))))
  (jd:estimate *jackie* data))
```

When we have the data, estimating the model is a matter of calling `(estimate model data)`.

We can check that this worked by evaluating the probability of an observation:

```common-lisp
CL-USER> (jd:probability *jackie* '((sleepy lose)))
```

That might look like a surprisingly low probability, since we know that Jackie is quite likely to lose when she's sleepy.
The catch is that we evaluated the *joint probability distribution* for the probability that loses *and* that she's sleepy.

We can also inspect what the individual probability distributions predict.
If, for example, we are interested in the prior probability that 

```common-lisp
CL-USER> (jd:probability ; obtains the probability of an outcome
          (jd:distribution ; obtains the probability distribution of a variable
           (jd:model-variable *jackie* 'focus)) ; returns the variable Focus
          (list 'sleepy)) ; the outcome we're interested in
```

The reason that we represent the outcome `sleepy` as a list is that, in general, values of a conditional distribution `P(X | C0, .. Cn)` are represented in jackdaw as `(x c0 ... cn)`.
Since the distribution of Focus is not conditioned on anything, its outcomes are represented as singleton lists.

**Excercise:**
Use jackdaw to obtain the probability that Jackie wins given that she is sleepy: `P(Game = win | Focus = sleepy)`. 
You should use the distribution of Game to do this.

We can see from the data that Jackie tends to win when she's alert, but unfortunately she's sleepy most of the time.
We might be interested in the overall probability, according to the model, that Jackie wins or loses.
For this purpose, we can again use the probability function, but first we need to make the model *partially observed* by hiding Focus.
This is achieved as follows:

```common-lisp
CL-USER> (jd:hide *jackie* 'Focus)
```

Now we can evaluate the probability of the observations that Jackie wins or loses

```common-lisp
CL-USER> (jd:probability *jackie* '(lose))
...
CL-USER> (jd:probability *jackie* '(win))
...
```

Recall the definition of the observer function of Game: `(lambda (m) (if (listp m) (second m)  m))`.
Since we're only observing the outcome of Game, we do not need to represent each moment as a list, but can instead simply use the symbol `win` or `lose`.
It is instructive to investigate what happens if you evaluate the probability of an outcome that cannot be generated by the model, say `draw`.

Of course we can also assess the probability that Jackie wins multiple times in a row:

```common-lisp
CL-USER> (jd:probability *jackie* '(win win win))
...
```

But since moments are independent, the results are not very interesting.
It would indeed make sense to define Jackie as a regular Bayesian network, but I expect to implement this in jackdaw soon, regular Bayesian networks are currently not fully tested.

Furthermore, we might be interested in the outcomes that are possible in a partially observed network as well as their probabilities.
The function `GENERATE` generates the possible outcomes of a (dynamic) Bayesian network given an observation.

```common-lisp
CL-USER> (jd:generate *jackie* '(lose))
...
```

The output is a list of states, however since states are represented by hash tables, the contents of which are not printed.
We can use the function `STATE-PROBABILITY-TABLE` to create a table (list of lists) the values that different variables have

```common-lisp
CL-USER> (jd:state-probability-table (jd:generate *jackie* '(lose)))
```

To get a very clean and legible output, we can format this table using `TABLE` from the [`cl-ansi-term`](https://github.com/vindarel/cl-ansi-term) package:

```common-lisp
CL-USER> (ql:quickload "cl-ansi-term")
CL-USER> (term:table 
          (jd:state-probability-table (jd:generate *jackie* '(lose)))
          :column-width 12)
```

What if we are interested in the *posterior* probabilities of Jackie's focus, rather than joint probabilities? That is `P(Focus | Game)`.

To obtain the these, we simply divide each joint probability by the model evidence.
For example:

```common-lisp
(jd:observe *jackie* 'focus)
(defparameter *joint-probability* (jd:probability *jackie* '((sleepy win))))
(jd:hide *jackie* 'focus) 
(defparameter *evidence* (jd:probability *jackie* '(win)))
(/ *joint-probability* *evidence*)
```

We can use the function `POSTERIOR` function, which operates on a list of states produced by `GENERATE`.
It returns a list of states (the `POSTERIOR` function simply performs a normalisation).

```common-lisp
(jd:posterior (jd:generate *jackie* '(lose))) 
```

This returns the distribution `P(Focus | Game = lose) = P(Game = lose, Focus) / P(Game = lose)`.
Since this distribution is simply a list of states, we can display it as we did with the output of `GENERATE`:

```common-lisp
CL-USER> (term:table 
          (jd:state-probability-table 
           (jd:posterior 
            (jd:generate *jackie* '(lose))))
          :column-width 12)
```

A *list of states* represents a probability distribution or a partitition of a probability distribution.
We can think of it as a probability table that enumerates the values of a set of variables and their corresponding probabilities.
Lists of states are produced by the `GENERATE` method of dynamic Bayesian networks.
We have so far seen two functions that operate on lists of states: `POSTERIOR` and `STATE-PROBABILITY-TABLE`.

There is one more function operating on lists of states that is sometimes useful: `MARGINALIZE`.
For example, to obtain a table of model evidences for two possible observations, we can hide both variables and produce congruent states with `GENERATE`.
The result is a table representing the full joint distribution.
We can then marginalise out Focus by calling `MARGINALIZE` on the list of states and a list of variables that we would like to keep, in this case `(Game)`.

```common-lisp
CL-USER> (jd:hide *jackie* 'Focus 'Game)
...
CL-USER> (term:table 
	      (jd:state-probability-table 
           (jd:marginalize 
            (jd:generate *jackie* '(nil))
	       '(Game)))
          :column-width 12)
```

In summary:

* `(HIDE MODEL VAR1 VAR2 ...)` and `(OBSERVE MODEL VAR1 VAR2 ...)` can be used to hide or observe variables in a model;
* `(PROBABILITY DISTRIBUTION OBSERVATION)` gives us the probability of an observation given a distribution (which may be a bayesian network);
* `(GENERATE MODEL OBSERVATION)` generates lists of states congruent with an observation given a generative model;
* `(POSTERIOR STATES)` generates a posterior distribution given a list of states produced by `GENERATE`;
* `(MARGINALIZE STATES VARIABLES)` marginalizes a list of states to the given variables.

As an aside, note that both jackdaw models (dynamic Bayesian networks) and distributions like `BERNOUILLI` and `CPT` can be regarded as parameterised probability distributions.
The jackdaw API attempts to reflect this equivalence in the interface offered by these types of objects.
All probability distributions are a subtype of `PROBABILITY-DISTRIBUTION`
All generative models are a subtype of `BAYESIAN-NETWORK`, which itself is a subtype of `PROBABILITY-DISTRIBUTION`.

As we have seen when we looked at the probability distributions of Jackie's constituent variables, all probability distributions offer the interface `(PROBABILITY DISTRIBUTION OBSERVATION)`.
Since it doesn't matter what entity generates the probabilities of different values of a variable, it is possible to use a (partially or fully observed) Bayesian network as the probability distribution of a variable.
However, I haven't tested this functionality yet in jackdaw so it's pretty much guaranteed not to work.

## Modeling sequences

Let's forget about inference for a moment and work with a simple, fully observed, model.


```common-lisp
(jd:defmodel letters () ()
  ((Letter ()
	   (jd:cpt ())
	   '(#\a #\b)
       :observe #'identity)))
```

We now know how to instantiate and estimate the model:

```common-lisp
(defparameter *letter* (make-letter-model))
(jd:estimate *letter* '((#\a #\b #\b #\b)))
```

We can make writing input a bit more convenient with this utility function:

```common-lisp
(defun characters (s)
  (coerce s 'list))
```

Now, we can do estimation as follows:

```common-lisp
(defparameter *letter* (make-letter-model))
(jd:estimate *letter* (list (characters "abbbaabb")))
```

We also know by now how to evaluate the probability of a sequence of new letters.
Note, however, that since the probability of each letter is independent from the previous letters, different orderings of the same letters have the same probability. 

```common-lisp
(jd:probability *letter* '(characters "a")
(jd:probability *letter* '(characters "b")
(jd:probability *letter* '(characters "ab")
(jd:probability *letter* '(characters "ba")
```

We can change this by making the probability of each letter dependent on the previous letter.
In dynamic Bayesian networks, each variable can be conditioned the variable of any variable in the model *in the previous moment*, including itself.
Such dependencies are created in jackdaw by simply inserting the `^` character in front of the variable name in the dependency list.
Thus, the model below conditions Letter on its value in the previous moment, ^letter. 

```common-lisp
(jd:defmodel letter-bigrams () 
  (&key (alphabet (characters "ab"))
  ((Letter (^Letter)
	   (jd:cpt (^Letter))
	   alphabet
       :observe #'identity)))
```

Note that we also add ^Letter to the list of dependencies of its CPT distribution, ensuring that the probability of each letter depends on the previous letter.
If we did not do this, the only effect of adding the ^Letter dependency would be that it would be available in the congruency constraint of Letter.
Furthermore, we have, for increased generality, made the alphabet a parameter of the model.

If you like, try to estimate this model as we did above and ensure that the model does indeed learn letter transition probabilities. 

The above model is called a first-order Markov model since the model is independent of all previous observations except the one immediately preceding the current.
That's all well and good, but what if we are interested in higher-order Markov models?
Since we can only condition variables on their value in the previous moment it micht seem that we're out of luck.

It turns out that this problem can be overcome easily and can make models dependent on an indefinitely long history of preceding events by taking the previous value of a variable and combining it with the current.
In other words, we change the representation of a variable's possible values.
We can do this using congruency constraints.

Below we have extended the letter-bigram model above to keep track of the preceding two events.

```common-lisp
(defvar +filler+ '✳)
(defmodel letter-trigram () (&key (alphabet '(a b)))
  ((Letters (^Letters)
	   (ngram-model (^Letters))
	   (if (inactive? $^letters)
	       (loop for l in alphabet 
            collect (list l +filler+ +filler+))
	       (loop for l in alphabet
            collect (list l (first $^letters)
                             (second $^letters)))))))
```

One thing here is particularly noteworthy: the if statemement `(if (inactive? $^letters) ...)`.
The symbol `$^letters` here refers to the previous value of Letters.
(Recall that in congruency constraints, variables are accessed by prefixing their names with the `$` character.)
The function `(inactive? x)` checks if the variable `x` has a special value that indicates that we are in the first moment and there is no previous value of x.

The congruency constraint of Letters states that *if we're in the first moment*, generate a trigram consisting of each letter of the alpabet and two arbitrary filler symbols.
If we're not in the first moment, generate a trigram consisting of a new letter from the alphabet and the first and second item of the previous trigram.
Note that the first item in the trigram represents the most recently generated letter.

Note also that the Letter variable has been renamed Letters to more accurately reflect what its values represent.

The model above involves a lot of boilerplate code for something that is a very general mechanism: create an *n*-gram by taking the current value and adding it to the first $n$ items of the previous *n*-gram.
Among various macros that jackdaw provides for writing congruency constraints is one called `markov` which implements this general patern.
The code below illustrates this.

```common-lisp
(jd:defmodel letter-trigram () (&key (alphabet '(a b)))
  ((Letters (^Letters)
	   (jd:ngram-model ())
       (jd:markov 3 ^letters alphabet))))
```

Note that rather than a CPT conditioned on the previous value of Letters, we now use the special distribution `ngram-model` that jackdaw provides.
We should not condition this distribution on the previous value of Letter since it uses its current value (which is a trigram) to determine the probability that the current letter follows the preceding two.

Finally, we might as well make *n* a parameter of the model as follows:

```common-lisp
(jd:defmodel letter-trigram () (n &key (alphabet '(a b)))
  ((Letters (^Letters)
	   (jd:ngram-model ())
       (jd:markov (- n 1) ^letters alphabet))))
```

But how do we observe such *n*-gram models?
It would be very tedious to write observations of the trigram model as `((a * *) (b a *) (a b a) ...)`.
Luckily we can avoid this by making use of a *deterministic variable*. 
We can use congruency constraints to add another variable to the model, Current-letter, which depends on Letter and whose only possible value is the last letter added to the *n*-gram.

```common-lisp
(jd:defmodel letter-ngram () (n &key (alphabet '(a b)))
  ((Letters (^Letters)
	   (jd:ngram ())
       (jd:markov (1- n) ^letters n alphabet))
   (Current-letter (Letters)
	   (jd:uniform ())
       (jd:deterministic (car $letters))
       :observer #'identity)))
```

Note the use of `DETERMINISTIC` in the congruency constraint of `Current-letter`.
This is an alias for `(list (car $letters))`.
Why do this? It doesn't even save typing.
Well, it's a matter of taste, but using this macro makes it explicit that this variable is deterministic and must generate exactly one value.
Being explicit in code is often a good thing: it can prevent mistakes (accidentally giving Current-letter two possible values) and makes code easier to read.

It's time to introduce another method from the jackdaw API.
To see if a model is doing what we expect it to be doing, the method `GENERATE-CONGRUENT-VALUES` often comes in handy.
This method is similar to `GENERATE` but does not calculate any probabilities.
Instead, it generates the combinations of variable values congruent with a sequence of observations.
Furthermore, unlike `GENERATE`, it produces a list of congruent values for each moment, rather than only the states that remained congruent after the last moment.

Thus, to see the congruent values generated by the model above, when no variables are observed, we can do:

```common-lisp
(defparameter *letter-ngram* (make-letter-ngram-model 2))
(jd:generate-congruent-values *letter-ngram* '(nil)) ; congruent values for a sequence of one moment  
```

The code below displays a table of congruent states in the second moment.
Note that for each bigram, the Current-letter variable contains the leftmost item (that is, the most recent).

```common-lisp
(term:table 
 (cons (jd::vertices *language*) 
       (second (jd::generate-congruent-values *language* '(nil nil nil))))
 :column-width 15)
```

If you feel like you need to develop more intuition for what's going on, experiment with the code above by replacing `second` with `first` or `third`, or observing Current-letter.

### PPM sequence models

For doing some serious sequence-prediction heavy lifting, jackdaw models can make use of IDyOM's implementation of PPM models.
PPM models are variable-order Markov model and can use contexts of various lengths up to a specific *order-bound* to make predictions.
It is also possible to set the maximum context length to unbounded and use all the (preceding) context available for making predictions.

To use these models, all we need to do is define a variable whose congruency constraint is wrapped in the `MARKOV` macro and use the `PPMS` distribution.
PPMS is indeed a plural of PPM, given that this distribution can be conditioned on other variables, in which case multiple PPM models will be used.

The code below illustrates how we turn the `letter-ngram` model into one that uses PPM sequence prediction.

```common-lisp
(jd:defmodel letter-ppm (jd:dynamic-bayesian-network) 
  (&key 
   order
   update-exclusion?
   (escape :c)
   (mixtures t)
   (alphabet '(a b c d r)))
  ((Letters (^Letters)
	    (jd:ppms () :order-bound order)
	    (jd:markov order $^letters alphabet))
   (Current-letter (Letters)
		   (jd:uniform ())
		   (list (car $letters))
		   :observer
		   (lambda (m)
		     (if (listp m) (second m) m)))))
```

Note that we have made `order` a parameter of the model and that we can also use this parameter to set the `order-bound` parameter of the PPM model.
Satisfyingly, this will also work if we do not want to use an order bound: the `markov` macro is defined such that if `order` is `NIL`, it will accumulate states indefinitely and when the order-bound of a PPM model is `NIL`, it will use all available context in its predictions.
So to use this model without an order bound, order should be set to `NIL`, which is in fact its default value.

The PPM implementation has a bunch of parameters that may affect its performance on different types of data.
To make the model more configurable, we could define these as model parameters (and provide sensible defaults) and pass them on to the PPM model:

```common-lisp
(jd:defmodel letter-ppm (jd:dynamic-bayesian-network) 
  (&key 
   order
   update-exclusion?
   (mixtures t)
   (escape :c)
   (alphabet '(a b c d r)))
  ((Letters (^Letters)
	    (jd:ppms ()
		     :order-bound order
		     :update-exclusion? update-exclusion?
		     :mixtures mixtures
		     :escape escape)
	    (jd:markov order $^letters alphabet))
   (Current-letter (Letters)
		   (jd:uniform ())
		   (list (car $letters))
		   :observer
		   (lambda (m)
		     (if (listp m) (second m) m)))))
```

If you'd like to try out this model data more interesting than "Sheep language" ("bbaaabaabaa"), have a look at the extra material below.

## Extra material: classifying languages

The file [`examples/language.lisp`](examples/language.lisp) contains a toy model for classifying languages as well as code for estimating the model we have defined above from empirical data.
The empirical data in question consists two classic books from English and Dutch literature that are now in the public domain.
The books have been digitized and archived by the fantastic [Project Gutenberg](https://www.gutenberg.org/).

To try this out, open the file [`examples/language.lisp`](examples/language.lisp) in Emacs and try running the commented out code at the bottom at the REPL.

To get started, try some of the things below:

To train the model on Dutch language data:

```common-lisp
CL-USER> (defparameter *letter-ppm* (estimate-letter-ppm-model "materials/de-komedianten.txt"))
CL-USER> (text-info-rate *letter-ppm* "Dit is een Nederlandse zin")
CL-USER> (text-info-rate *letter-ppm* "This is an English sentence")
CL-USER> (text-info-rate *letter-ppm* "Lasdf ewk fds Balkejw iesefhj")
```

You may need to augment to path `"materials/de-komedianten.txt"` with the path to where you cloned this repository.

To train the model on English language data:

```common-lisp
CL-USER> (defparameter *letter-ppm* (estimate-letter-ppm-model "materials/alice-in-wonderland.txt"))
CL-USER> (text-info-rate *letter-ppm* "Dit is een Nederlandse zin")
CL-USER> (text-info-rate *letter-ppm* "This is an English sentence")
CL-USER> (text-info-rate *letter-ppm* "Lasdf ewk fds Balkejw iesefhj")
```

# Music

Finally, let's see how we can leverage what we have learned so far to building two types of models that can be applied to music.
First we'll go over a jackdaw implementation of a *derived viewpoint*.
This is mainly of theoretical interest as it illustrates how the rules and constraints translate to congruency constraints in jackdaw.

Second, we'll consider how we can use jackdaw to infer latent structure in music, namely the key of a melody.
This type of model is the same as the type of generative models that David Temperley introduces in his book *Music and Probability*.

## Viewpoints

IDyOM is a *multiple* viewpoint system.
Multiple-viewpoint-system prediction combine the predictions of single viewpoints in a way that cannot be captured by Bayesian networks.
However, the behavior of a *single* viewpoint can be captured by a Bayesian network as we'll see below.

A viewpoint transforms an *event* into a derived representation.
We can safely equate events to what we previously described as *moments*.

We'll start with a simple viewpoint that represents the scale degree given a pitch and key.

A scale degree is a number that represents which half step a given note is in a scale.
For example, given that we're in the key of Gb-major, the note Gb4 has scale degree 0.
Scale degrees are invariant across octaves, so Gb0, Gb2 and Gb5 all are represented by scale degree 0.
We will actually not bother with the specific scale, and represent scale degrees as the number of half steps a note is up from the tonic.
Thus, given a scale in Gb, G has scale degree 1, Ab has scale degree 2, etc.

We'll represent pitches simply as the number of half steps up from the lowest C.

In a multiple viewpoint system, we would proceed by defining a viewpoint function that calculates the scale-degree given a pitch and a tonic.
That function could for example look as follows:

```common-lisp
(mod (- pitch tonic) 12)
```

When formulating the system as a generative model, it turns out that we instead need to specify a function that calculates the pitches consistent with a given scale degree and tonic.
That function could like this:

```common-lisp
(loop for pitch in pitch-alphabet 
 collect pitch 
 if (eq (mod (- pitch tonic) 12) scale-degree))
```

Additionally, we need to specify a function that generates the derived-viewpoint alphabet: the possible scale degrees.
This alphabet consists of simply the numbers zero to 11.

```common-lisp
(0 1 2 3 4 5 6 7 8 9 10 11)
```

We're now ready to define the generative model.

```
(jd:defmodel scale-degree-viewpoint (jd:dynamic-bayesian-network)
  (tonic &key order-bound (octave 12)
	 (pitch-alphabet (loop for p below 24 collect p)))
  ((Scale-degree (^Scale-degree)
		 (jd:ppms ())
		 (jd:markov
		  $^scale-degree
		  order-bound
		  (loop for deg below octave collect deg)))
   (Pitch (Scale-degree)
	  (jd:uniform ())
	  (loop for pitch in pitch-alphabet
		if (eq (mod (- pitch tonic) octave)
		       (first $scale-degree))
		  collect pitch)
	  :observer #'identity)))
```

To check that it works, let's generate its congruent values. 

```
(defparameter *scale-degree* (make-scale-degree-viewpoint-model 5))
(term:table (cons (jd::vertices *scale-degree*) (first (jd::generate-congruent-values *scale-degree* '(nil)))) :column-width 15)
```

We've made the tonic a parameter of the model, but to be more general, we could assume that each event has a pitch and tonic attribute, as is the case in IDyOM.

```
(jd:defmodel scale-degree-viewpoint (jd:dynamic-bayesian-network)
  (tonic &key order-bound (octave 12)
	 (pitch-alphabet (loop for p below 24 collect p)))
  ((Scale-degree (^Scale-degree)
		 (jd:ppms ())
		 (jd:markov
		  $^scale-degree
		  order-bound
		  (loop for deg below octave collect deg)))
   (Event (Scale-degree)
	  (jd:uniform ())
      (let ((events))
      (dotimes (tonic octave)
      (dolist (pitch pitch-alphabet)
		(when (eq (mod (- pitch tonic) octave)
		          (first $scale-degree))
                  (push (list pitch tonic) events)))))
	  :observer #'identity)))
```

```common-lisp
(defparameter *scale-degree* (make-scale-degree-viewpoint-model 5))
(term:table (cons (jd::vertices *scale-degree*) (first (jd::generate-congruent-values *scale-degree* '((5 4))))) :column-width 15)
```

However, since the first variant of the scale-degree-viewpoint is a bit more legible, we'll continue to use that one.

All those numbers are a bit dizzying though! Can we not use more eye-friendly musical note names?
It turns out that deterministic variables can be used for this purpose too.

First, we'll define a function that converts our pitch representation into 

```common-lisp
(defparameter *pitch-names-with-sharps* '(c c# d d# e f f# g g# a a# b ))
(defparameter *pitch-names-with-flats* '(c db d eb e f gb g ab a bb b))

(defun pitch-name (tonic pitch &key (octave-size 12) exclude-octave)
  (let* ((octave (floor (/ pitch octave-size)))
	 (degree (mod pitch octave-size))
	 (name
	   (if (<= tonic 7)
	       (elt *pitch-names-with-flats* degree)
	       (elt *pitch-names-with-sharps* degree))))
    (intern (format nil "~a~a" name (if exclude-octave "" octave)))))
```

Let's check that it works.

```common-lisp
(pitch-name 0 0) ; -> C0
(pitch-name 0 12) ; -> C1
(pitch-name 5 10) ; -> Bb0 (in the key of Bb)
(pitch-name 11 10) ; -> A#0 (in the key of B)
```

Now to our earlier definition, we'll add a deterministic variable, Pitch-name, that, given a value of Pitch, has one congruent state containing the name of that pitch.

```common-lisp
(jd:defmodel scale-degree-viewpoint (jd:dynamic-bayesian-network)
  (tonic &key order-bound (octave 12)
	 (pitch-alphabet (loop for p below 24 collect p)))
  ((Scale-degree (^Scale-degree)
		 (jd:ppms ())
		 (jd:markov
		  $^scale-degree
		  order-bound
		  (loop for deg below octave collect deg)
          :observer #'identity))
   (Pitch (Scale-degree)
	  (jd:uniform ())
	  (loop for pitch in pitch-alphabet
		if (eq (mod (- pitch tonic) octave)
		       (first $scale-degree))
		  collect pitch)
	  :observer #'identity)
   (Pitch-name (Pitch)
	       (jd:uniform ())
	       (jd:deterministic
		(pitch-name tonic $pitch octave))
          :observer #'identity)))
```

We've given all variables the function `IDENTITY` as an observer since we'll assume that we never need to observe multiple variables at the same time.

Let's explore this model further.
We'll create a scale-degree model in the key of F, and set scale degree to observed.

```common-lisp
(defparameter *scale-degree* (make-scale-degree-viewpoint-model 5))
(jd:observe *scale-degree* 'scale-degree)
```

Now let's see which pitches are consistent with an observed scale degree.

```common-lisp
(term:table (cons (jd::vertices *scale-degree*) (first (jd::generate-congruent-values *scale-degree* '((4))))) :column-width 15)
```

In our model definition, we can see that Pitch has a uniform distribution given scale degree.
Thus the probability of each pitch is (in this case) 1/2 times the probability of the scale degree itself.
The distribution of scale degree depends on the sequence of preceding scale degrees and is estimated by a PPM model.
It looks like we have successfully recreated a derived viewpoint!

Finally, let's observe Pitch-name (and hide Scale-degree) so we can evaluate the model on a melody written as a sequence of note names.

```common-lisp
(jd:hide *scale-degree* 'Scale-degree)
(jd:observe *scale-degree* 'Pitch-name)
(last (jd::generate-congruent-values *scale-degree* '(F0 G0 A0)))
```

This yields the congruent values of the model in the last moment.
Only one combination of variable values remains congruent: `(((4 2 0) 9 A0))`.
Here, `(4 2 0)` corresponds to the (reversed) list of scale degrees corresponding to the input, 9 is the Pitch of the last note and `A0` is its name.

### Extra material: derivatives and a general derived viewpoint model

The file [`examples/viewpoints.lisp`](examples/viewpoints.lisp) shows how we can use jackdaw to implement viewpoints that might be undefined during the first few moments.
This is the case for viewpoints modeling derivatives of some basic attribute such as pitch or onset interval, as well as higher-order viewpoints like pitch contour or IOI ratio.

Furthermore, an example is shown of how a jackdaw model that implements derived viewpoints in general.
This model is parameterized by a viewpoint function and will behave like a derived viewpoint with that viewpoint function.

## Inferring key

We'll now turn to a generative model of melodies in which key is a latent variable.
Above, we developed a model that converts a sequence of pitches into a sequence of scale degrees and assess the probability of this sequence using a PPM model.
It turns out that we have already done most of the work for a model that can infer the key from a given melody.

Instead of a fixed parameter, we can make the tonic a random variable with a uniform distribution.
(If we're intending this to be a model of perception, making the prior distribution of the tonic uniform corresponds to assuming no effect of absolute pitch perception.)
The result is the model below (where for convenience, we've also added a deterministic variable Tonic-name, the is analogous to Pitch-name):

```common-lisp
(jd:defmodel musical-key (jd:dynamic-bayesian-network)
  (&key order (octave 12) (pitch-alphabet (loop for p below 120 collect p)))
  ((Tonic (^tonic) ; now a variable rather than a constant
	  (jd:uniform ())
	  (jd:persist $^tonic
	   (loop for tonic below octave collect tonic))
	  :observer #'first)
   (Tonic-name (Tonic) ; only for convenience
	       (jd:deterministic ())
	       (jd:deterministic
		(pitch-name $tonic $tonic :octave-size octave :exclude-octave t))
	       :observer #'first)
   (Scale-degree (^Scale-degree)
		 (jd:ppms () :order-bound order)
		 (jd:markov $^scale-degree order
			    (loop for deg below octave collect deg))
		 :observer #'second)
   (Pitch (Tonic Scale-degree) ; now also depends on Tonic
	  (jd:uniform ())
	  (loop for pitch in pitch-alphabet
		if (eq (mod (- pitch $tonic) 12)
		       (car $scale-degree)) ; use variable $tonic, rather than constant tonic
		  collect pitch)
	  :observer (lambda (m) (if (listp m) (second m) m)))
   (Pitch-name (Tonic Pitch)
	       (jd:deterministic ())
	       (jd:deterministic
		(pitch-name $tonic $pitch :octave-size octave))
	       :observer (lambda (m) (if (listp m) (second m) m)))))
```

Note that the Tonic variable uses the `PERSIST` macro in its congruency constraint.
This is a simple macro that ensures that Tonic is generated only in the first moment.
In subsequent moments, Tonic is a deterministic variable whose only possible value is the value that it assumed in the previous moment.

While this model may work for inferring the tonic, we might also hypothesise that the statistics of sequences of scale degrees are strongly influenced by the whether we are in a major or minor scale.
This idea can easily be incorporated in the above model.
First, we'll add a variable called Scale, which can assume two values: major or minor.
Next, we make the distribution of Scale-degree depend on Scale.
We do this by adding Scale to the dependency list of scale-degree, and also to the dependency list of its PPM model.

The latter is necessary because jackdaw distinguishes between the dependencies of a variable and the dependencies of the distribution of a variable.
The dependencies of a variable's distribution must be a subset of the variable dependencies.
This is useful because sometimes the congruency constraints require additional dependencies, as is the case for scale-degree whose congruency constraint additionally requires ^Scale-degree.

The final result is shown below.

```common-lisp
(jd:defmodel musical-key (jd:dynamic-bayesian-network)
  (&key order (octave 12) (pitch-alphabet (loop for p below 120 collect p)))
  ((Tonic (^tonic) ; now a variable rather than a constant
	  (jd:uniform ())
	  (jd:persist $^tonic
	   (loop for tonic below octave collect tonic))
	  :observer #'first)
   (Tonic-name (Tonic) ; only for convenience
	       (jd:deterministic ())
	       (jd:deterministic
		(pitch-name $tonic $tonic :octave-size octave :exclude-octave t))
	       :observer #'first)
   (Scale ()
	  (jd:cpt ())
	  (list 'major 'minor)
	  :observer #'second)
   (Scale-degree (^Scale-degree Scale)
		 (jd:ppms (Scale) :order-bound order)
		 (jd:markov $^scale-degree order
			    (loop for deg below octave collect deg))
		 :observer #'third)
   (Pitch (Tonic Scale-degree) ; now also depends on Tonic
	  (jd:uniform ())
	  (loop for pitch in pitch-alphabet
		if (eq (mod (- pitch $tonic) 12)
		       (car $scale-degree)) ; use variable $tonic, rather than constant tonic
		  collect pitch)
	  :observer (lambda (m) (if (listp m) (third m) m)))
   (Pitch-name (Tonic Pitch)
	       (jd:deterministic ())
	       (jd:deterministic
		(pitch-name $tonic $pitch :octave-size octave))
	       :observer (lambda (m) (if (listp m) (third m) m)))))
```

You can find this model in the file `examples/tonality.lisp`, along with some functions for estimating it from empirical data.
The empirical data in question is either a set of folk melodies from the Meertens Tunes Collection, or from the Essen Folksong Collection.
These can be found in the `materials` folder.

If you open the file [`examples/tonality.lisp`](examples/tonality.lisp) in Emacs and hit `C-c C-k` to compile it, you should be able to do the following:

```common-lisp
(defparameter *key* (make-musical-key-model :order 0))
```

to create an instance of the musical key model with an order bound of zero: that is, we consider scale degrees without context.
If you want to bring out the big guns immediately, you could set :order to NIL, which creates a model with unlimited.
The following will load some folk melodies, annotated with key and scale, and estimate the musical key model.

```common-lisp
(parameterize-model *key* "materials/mtc-melodies.csv")
```

Now, we can for example verify that the model makes predictions that we would expect.
To show the probabilities it assigns to different scale degrees within a key, we can observe only Tonic and Scale, and let the model generate congruent states.
Then, we marginalize these to find out the probabilities for just scale degrees.

```common-lisp
(jd:hide *key*) ; hide everything
(jd:observe *key* 'tonic scale) 
(term:table 
 (jd:state-probability-table 
  (jd:marginalize 
   (jd:posterior 
    (jd:generate *key* '((C# major))))
   '(scale-degree))
  :variables '(scale-degree) :sort t)
 :column-width 15)
```

Finally, to infer a posterior distribution over Tonic and Scale given a melody, we hide all variables except for pitch and evaluate the model on a pitch sequence:

```common-lisp
(jd:hide *key*)
(jd:observe *key* 'pitch) 
(term:table 
 (jd:state-probability-table 
  (jd:posterior (jd:generate *key* '(0 7 8 7 3)))
  :variables '(tonic-name scale) :sort t)
 :column-width 15)
```

## Extra material

The file [`examples/rhythm.lisp`](examples/rhythm.lisp) contains an implementation of the rhythm model presented in [my PhD thesis](https://hdl.handle.net/11245.1/dd3e25aa-6006-486e-afcf-c0692e0afacd), as well as some code for trying it out.
Preprocessed data 

# Summary

Here's a list of concepts introduced above:

* `HIDE` and `OBSERVE` for making variables observed or hidden
* Evaluating the probability of observations (model evidence) with `PROBABILITY`
* Generating lists congruent states with a sequence of observations with `GENERATE`
* Manipulating lists of states with `POSTERIOR` and `MARGINALIZE`
* Viewing lists of states with `STATE-PROBABILITY-TABLE and `TABLE` from the `cl-ansi-term` package
* Across-moment dependencies (like ^Letters and ^Scale-degrees)
* The `MARKOV` macro and the `NGRAM-MODEL` distribution
* The `PPMS` distribution
* Building complex models for inferring musical structure 

We have seen how to use the tools provided by jackdaw to define probabilistic generative models and we have had a taste of the flexibility that congruency constraints offer:
A single model can easily be made to work with different representations, as we saw for the scale-degree model where we added Pitch-name and Tonic-name. 
A variety of different generative models, that include language models, viewpoints, key-inferring models, and meter-inferring models.

While jackdaw is restricted to models consisting of discrete conditional probability tables, there are lots of interesting models to explore within this space.
