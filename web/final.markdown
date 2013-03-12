---
title: Final project
---

Overview/important dates
------------------------

For CIS 194 you will complete a final project which will tie together
some of the things you have learned and give you some practical
Haskell development experience.  The expectation is for you to spend
around **10-15 hours** working on the project.  Here are some
important dates:

  * **Monday, March 18** -- Project proposals due
  * **April 8--12** -- Checkpoint meetings
  * **Tuesday, April 23** -- Final project submission deadline

Get started early!

Resources
---------

Here is a document explaining a few things relevant to
[Haskell programming in the large](/docs/inthelarge.pdf).

Format
------

You may work by yourself, or in groups of up to *three* students.
Note, however, that projects for groups of three will be held to
somewhat higher standards than those for individuals or pairs.  Groups
of five are *right out*.

There are two types of projects you may complete:

1. Application/library

    For your project you may write some sort of Haskell application or
    library which does something fun/useful/interesting.  Your
    imagination is the limit.  Some possibilities/suggestions include:

    + A program to play a game (like tic-tac-toe, Connect 4, othello,
      gomoku, poker, mancala, ...) against the user.

    + A program to solve puzzles like sudoku or kenken.

    + A program to generate random mazes and let the user
      interactively solve them, or to solve mazes input by the user.

    + An implementation of some interesting data structure like
      red-black trees, 2-3-4 trees, binomial heaps, or Fibonacci heaps.

    + An parser and interpreter for a small programming language, such
      as a
      [while language](http://www.program-analysis.com/while.html).

    + A raytracer.

    + Create a web site using [hakyll](http://jaspervdj.be/hakyll/).

    + Take an interesting program you have written in some other
      language, and figure out how to port/re-implement it in
      idiomatic Haskell.

    + Whatever else your creativity suggests!

2. Open-source contribution

    For your project you may choose an open-source library or
    application on [Hackage](http://haskell.org/hackage/) to
    contribute to.  Contributions may include bug fixes, new features,
    and/or documentation.  Here are a few suggestions---these are
    projects whose authors/maintainers have indicated that there would
    be good ways for beginning Haskell students to contribute.  (But
    you are free to work on any project you like, as long as you can
    find a reasonable way to contribute.)  If you want to try
    contributing to one of these projects, you should contact the
    relevant person(s) and discuss it with them prior to submitting
    your project proposal.

    **Note**: if you are thinking of making an open-source
      contribution you may turn in your proposal by **Monday, March
      25**.

    Open-source projects students have contributed to in prior years
    include a package to efficiently compute prime numbers using a
    mutable-array-based sieve and Haskell bindings to the Kinect.

    *Open-source project suggestions coming soon.*

Project proposal
----------------

You must submit a project proposal by **Monday, March 18** (unless you
are thinking about working on an open-source contribution, in which
case you have until **Monday, March 25** in order to have time to
communicate with the maintainer(s).  This gives us a chance
to discuss your proposal and ensure it will make a suitable project.
You are encouraged to submit your proposal earlier than March 18 if
you already have an idea.  You should also feel free to submit several
project proposals if you would like help deciding which is most
suitable.

To submit your proposal, send an email of a few paragraphs to me
(byorgey at cis) with the subject "CIS 194 final project proposal".
Try to answer the questions: What do you propose to do?  What do you
hope to learn from the project?  What are some concrete goals,
i.e. how will we judge the success of your project?

Checkpoint
----------

Sometime during the week of April 8-12 or thereabouts, you should
schedule a meeting with one of Brent, Zach, or Adi to show off the
progress you have made on your project, get any guidance or help you
might need, and discuss your plans for completing the project.  Note
that *the checkpoint meeting will constitute part of your final
project grade*.  More information on how to schedule a meeting and
exactly what is expected will be posted soon.

Final submission
----------------

Final submissions are due by **Tuesday, April 23**.  Extensions to the
final deadline will be cheerfully granted, but you *must ask for one
by **Tuesday, April 16*** (one week in advance).

Your final submission should consist of any and all code you have
written, along with a document describing your project (a simple text
file is fine).  The document should contain

  * a description of your project and what you accomplished;

  * instructions on how to compile/run/try out/play with your project;

  * a description of work you did and things you learned along the way.

You may submit your project in one of two ways:

  * By emailing me a `.tar`, `.tgz` or `.zip` file

  * By emailing me a link to a publically accessible source
    repository, *e.g.* on github, bitbucket, or hub.darcs.net. Be sure
    to submit a link to a specific commit, tag, *etc.* representing the
    version that you would like to be graded, rather than just to the
    repository in general.

Grading will be as follows:

  * Checkpoint (25%).  Did you make some progress on your
    project by the time of the checkpoint meeting?

  * Style (25%).  Your project should use good Haskell style and be
    well-documented.

  * Correctness (25%).  Your project should be free of compilation
    errors and should correctly accomplish whatever it is supposed to
    accomplish.  This means that if the deadline is looming, your time
    would be better spent fixing bugs in what you already have than
    adding one last feature.

  * Effort/accomplishment (25%).  We will be looking for evidence that
    you put energy and effort (~10-15 hours) into your project and
    that you have learned something.  This is where the document you
    submit along with your project comes in: be sure to use it to
    highlight work you did and things you learned, especially if it is
    not obvious from looking at the final product.  For example, if
    you spent two hours trying an approach that ultimately did not
    work, you should write about that and what you learned from the
    experience.  However, we will not necessarily look with sympathy
    on *unnecessary* work: for example, if you spent five hours trying
    to track down a bug without asking for help, that's just plain
    silly stubbornness.  If you are stuck on something, please ask for
    help.  We want you to spend your time making progress on your
    project, not banging your head against a wall (although a small
    amount of head-banging can be healthy).
