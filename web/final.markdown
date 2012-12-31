---
title: Final project
---

Overview/important dates
------------------------

For CIS 194 you will complete a final project which will tie together
some of the things you have learned and give you some practical
Haskell development experience.

  * **Thursday, March 22** -- Project proposal due
  * **Thursday, April 12** -- Checkpoint
  * **Tuesday, April 24** -- Final project submission deadline

Get started early!

Resources
---------

Here is a document explaining a few things relevant to
[Haskell programming in the large](/static/inthelarge.pdf).

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

    + A program to play a game (like tic-tac-toe or Connect 4) against
      the user.
      
    + A program to generate random mazes and let the user
      interactively solve them.
      
    + An implementation of some interesting data structure like
      red-black trees, 2-3-4 trees, or Fibonacci heaps.
      
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
    relevant person and discuss it with them prior to submitting your
    project proposal.

    + [Pandoc](http://johnmacfarlane.net/pandoc/) is a universal
        document format converter (which is in fact used in generating
        this website).
      
        Contact: [John MacFarlane](mailto:fiddlosopher@gmail.com)
      
        * Pandoc's LaTeX reader currently applies LaTeX macros in math
            contexts, but not outside them.  One project would be to
            make them work everywhere.  This would mostly concern
            `Text.Pandoc.Readers.LaTeX`, but would also involve using
            or porting some code from the `texmath` library, which
            currently handles parsing the macros and applying them in
            math. The student would have to be somewhat familiar with
            LaTeX, and of course with parsec.
            
        * Implement an EPUB reader for pandoc.  This would have to
            unzip the input file, look in the metadata to see what's there,
            extract the chapters and parse them with pandoc's HTML reader, and do
            something with images and the like (maybe make them into data: URIs --
            see `Text.Pandoc.SelfContained`).
            It would be handy to be able to convert an epub directly into
            to markdown, HTML, LaTeX, docx, or PDF. And the pieces are all
            pretty much there in pandoc already.
      
    + Contact: [Christopher Done](mailto:chrisdone@googlemail.com)
    
        I have a bunch of small Haskell projects and I would enjoy helping
        someone contribute to them. The problem would be finding projects that
        are actually interesting to a student. The only ones I can think of,
        that are trivial to work on, are:

        * [freenect](https://github.com/chrisdone/freenect): Requires
          a Kinect device (your students have X-Box right?). This is
          my Kinect interface. Who doesn't love devices with video and
          depth perception? Currently it only supports depth
          perception, as that's all I wanted from it, but one could
          fairly straight-forwardly add video support. This would
          require some mentoring and helping along as it requires not
          only Haskell knowledge, but it needs some C code and using
          the FFI. It took me a weekend to figure out and write the
          depth perception part, with help a newbie could tackle video
          within four weeks. Alternatively -- there's also the
          opportunity to write some simple motion detection stuff with
          the existing code.

        * [stepeval](https://github.com/chrisdone/stepeval): This is
          benmachine's project to evaluate Haskell in steps. It's
          currently on hpaste.org, but it's rather
          incomplete. Fleshing this out to support more syntax would
          be nice. Not sure if this is actually interesting to anyone
          else. But it's a good way to solidify your understanding of
          Haskell's evaluation model and syntax, maybe.

        * [css](https://github.com/chrisdone/css): Making this very
          trivial CSS library well-typed could be easy and useful.

        * [wordnik](https://github.com/chrisdone/wordnik): A little
          interface to the Wordnik online dictionary service. I kinda
          started this but didn't finish it. Once done though we can
          send it to Wordnik and they'll for sure stick it on their
          libraries page.

        * [amelie](https://github.com/chrisdone/amelie): (Powers
          [hpaste.org](http://hpaste.org)) The only one that is
          relevant to the Haskell community, but I don't have any
          features that need doing on it, as far as I'm aware. I think
          the code is fairly easy to grok, though. Could be an
          opportunity for adding some feature, and it'll be used by a
          fair chunk of the Haskell community.

        * [pgsql](https://github.com/chrisdone/pgsql-simple): The
          PostgreSQL library that amelie uses, it's a raw tcp/ip
          socket interface to the server, fairly trivial and yet
          interesting (to me) and useful. Needs more authentication
          methods, and I have some opportunities for optimizing some
          things. Tests and benchmarks for it would be good too, and
          probably easy to write.

        * [hulk](https://github.com/chrisdone/hulk): My IRC server
          that we use at work could do with a better logging mechanism
          than a file full of JSON.  Probably a DB backend. I don't
          know if any student would care at all about such a project.
      
    + Contact: [Heinrich Apfelmus](mailto:apfelmus@quantentunnel.de)
    
        I do have a small task that may be suitable and that is useful
        to me in the context of my
        [reactive-banana](http://www.haskell.org/haskellwiki/Reactive-banana)
        library and my yet undisclosed tomato-rubato project.

        The task is to implement a small audio synthesizer in
        Haskell. Of course, implementing high-performance audio
        synthesis is too challenging a task for a Haskell beginner,
        but there is one particular approach that I would like to see
        performance measurements of.

        More specifically, the idea is the following:
        
        1. Implement a handful of combinators for generating audio as a lazy list of
           samples

                type Audio = [Sample]

        2. Get it out of the speakers. (I can find a library for that.) This will be
        slooow.
        
        3. Implement the same handful of combinators for a different representation,
           namely a lazy list of memory blocks with 64 samples each

                type Block = Data.Vector.Vector  -- 64 samples
                type Audio = [Block]

            In other words, each block is filled in an aggressively
            optimized inner loop while the blocks are shuffled around
            with ordinary Haskell functions.
           
        4. Do performance measurements on 3 and test whether it can be run in
           real-time.

        So, the task does involve an external library and some knowledge about GHC's
        optimization, but hopefully nothing too fancy.

        How is this task useful for me? If the performance is good
        enough, I can replace the lazy lists with `Event`/`Behavior`
        from `reactive-banana`, giving a real-time audio synthesizer in
        the style of functional reactive programming.  If it doesn't
        work out, then the students had a fun project to work on,
        which is just as well.
      
    + [Diagrams](http://projects.haskell.org/diagrams) is my own
      project, a domain-specific language embedded in Haskell for
      creating vector graphics.  For potential ways to contribute, see
      the [bug tracker](http://code.google.com/p/diagrams/issues/list)
      and look for tickets with a Length field of Short, Normal, or
      Long and a Difficulty field of VeryEasy, Easy, or Normal.

    + [Chris Smith](mailto:cdsmith@gmail.com) has a project,
      [CodeWorld](http://www.codeworld.info), which uses Haskell to
      teach basic programming to kids (based on
      [gloss](http://hackage.haskell.org/package/gloss)).
    
        The project would be to write a logical sequence of short and
        simple but creative examples that run on
        [http://www.codeworld.info](http://www.codeworld.info) and
        motivate and demonstrate a sequence of ideas in programming,
        including in approximately this order: basic shapes, nested
        expressions and parentheses, transformations and combining
        pictures, defining variables, coordinates and polygons, simple
        types (including lists and tuples, but using only the sugared
        definition of lists), list ranges and comprehensions, defining
        functions, evaluating functions by substitution, building a
        fractal using recursion, animations as functions from time,
        linear motion and y=mx+b, a couple other interesting functions
        like sine, if statements, simple recursion, top-down design
        using functions, gloss simulations, linear motion in
        simulations, constant acceleration in simulations, pattern
        matching, defining algebraic data types (product types only, and
        then including sums), conditionals in simulations, guards on
        functions, top-down design in simulations...

        Okay, that's enough that no one will get anywhere near that far
        in a class project!  So I'll stop there.  I'm expecting each
        example to be on the order of maybe 5 to 25 lines of code...

Project proposal
----------------

You must submit a project proposal by **Thursday, March 22**.  This
gives us a chance to discuss your proposal and ensure it will make a
suitable project.  You are encouraged to submit your proposal earlier
than March 22 if you already have an idea.  You should also feel
free to submit several project proposals if you would like help
deciding which is most suitable.

Checkpoint
----------

By **Thursday, April 12** you must submit whatever work you have
completed so far on your project, along with a **text file describing
your progress**.  The work itself will not be graded, but you will
receive a grade based on the amount of progress you have made: it
should be evident that you have already spent at least a few hours
working on your project.  It's not necessarily a problem if you have
no work to show for your time yet (as may easily be the case for
open-source contributions in particular), as long as the submitted
text file makes it clear what you have spent your time doing.  For
example, your text file should include the following sorts of
information:

  * Anything you have spent time reading, along with a summary of what
    you learned from it

  * Examples or demos you have played with and a summary of what you
    learned

  * Difficulties or challenges you have faced

  * Choices or decisions you have made (what to focus on, what
    tools/libraries to use, etc.) and why you chose the way you did

  * What you plan to do next

This is not an exhaustive list; feel free to write about anything you
think may be helpful in demonstrating your progress.

Final submission
----------------

Final submissions are due by **Friday, April 24**.  Extensions to the
final deadline will be cheerfully granted, but you *must ask for one*.
Otherwise, the deadline is firm.

Your final submission should consist of any and all code you have
written, along with a document describing your project (a simple text
file is fine).  The document should contain

  * a description of your project and what you accomplished;
  
  * instructions on how to compile/run/try out/play with your project;

  * a description of work you did and things you learned along the way.

Grading will be as follows:

  * Style (25%).  Your project should use good Haskell style and be
    well-documented.
    
  * Correctness (25%).  Your project should be free of compilation
    errors and should correctly accomplish whatever it is supposed to
    accomplish.  This means that if the deadline is looming, your time
    would be better spent fixing bugs in what you already have than
    adding one last feature.
    
  * Effort/accomplishment (50%).  We will be looking for evidence that
    you put energy and effort (~10 hours at a minimum) into your
    project and that you have learned something.  This is where the
    document you submit along with your project comes in: be sure to
    use it to highlight work you did and things you learned,
    especially if it is not obvious from looking at the final product.
    For example, if you spent two hours trying an approach that
    ultimately did not work, you should write about that and what you
    learned from the experience.  However, we will not necessarily
    look with sympathy on *unnecessary* work: for example, if you
    spent five hours trying to track down a bug without asking for
    help, that's just plain silly stubbornness.  If you are stuck on
    something, please ask for help.  We want you to spend your time
    making progress on your project, not banging your head against a
    wall (although a small amount of head-banging can be healthy).
