---
title: A first entry
---

This is the first entry of this blog.
I've been meaning to make one for the longest time but never got around to
actually write stuff down. After bike-shedding endlessly about which static-site
generator would be best for the task at end, and trying to build my own in
Haskell with no specification in mind, I finally settled on a minimal setup to
get this out of the way. Yes, it is *very* limited. But it does work for now, is
not bloated and easy to customize.

Every page is generated with `pandoc`_, and a tiny Makefile takes care of
dependency tracking and incremental builds. I also use `jq`_ to transform pandoc
metadata.  Ultimately I would prefer something leaner than pandoc but
this is for later.

Till next time.

----

**2020/06/05:**
Of course I was going to ditch my janky setup at some point.
It happened sooner than expected but I'm trying to convince myself that the first
version allowed me to identity exactly what I need from my static site
generator. I am now using a slightly modified version of `Hakyll`_.
Here's to hoping I'll manage to craft the perfect workflow™ from it.

----

**2020/06/12:**
Hm, the move to `Hakyll`_ wasn't for long. I was fed up with how complicated
it is, while still being very limiting. So I started to work on a new project
called **achille**. You can find the `WIP documentation here </projects/achille/>`_.
My site is already built with it and so far it's really neat!

.. _pandoc: https://github.com/jgm/pandoc
.. _jq: https://stedolan.github.io/jq/
.. _Hakyll: https://jaspervdj.be/hakyll/
