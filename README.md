# Goddinpotty

A  generator for Digital Gardens based on graph-based Personal Knowledge Management tools like [Logseq](https://logseq.com) and [Roam](https://roamresearch.com/). 

This is a fork from [static-roam](https://github.com/TomLisankie/static-roam) by Tom Lisankie. It's pretty diverged from the original by now, maybe 20% of the code is inherited.

[The name explained](http://www.hyperphor.com/ammdi/pages/goddinpotty.html).

# Status

I use this to generate my own site, [AMMDI](http://www.hyperphor.com/ammdi/). Nobody else to my knowledge is using it, although I'm happy to help anyone interested.

# Features and why you would need this

Roam only allows you to publish entire graphs. Goddinpotty has a flexible scheme for declaring parts of a graph public or private.

- user-definable Entrance and Exit Points for privacy (see below)
- generates index pages, and graphic maps, client-side search
- sidenotes 

# Logseq

I've moved from Roam to Logseq, in a rather sloppy fashion, so the code that adapts to Logseq is not very well organized. It probably no longer works with Roam any more, because I don't care, but it could easily be made to work again.

Currently Logseq site generation is driven from a Logseq EDN export. This may change; it's logistically easier to parse the markdown which is logseq's source of truth directly.

Because Logseq stores images locally rather than on the cloud, we now have to publish them along with the html. To suppor this, the config has a pointer into the repo, and there is logic to figure out which images are published and need to be copied to the output.


# Usage

- create a configuration file by editing `resources/example-config.edn`
- Export All from Roam, choose the JSON format
- `lein run <config file>`

By default, the program will look for the latest Roam Export file in `~/Downloads` and generate output in `./output`

# What gets published

With Roam, there are no limits on how you can connect notes. This is one of Roam's greatest strengths. However, it's its greatest weakness when it comes to sharing notes publicly. Since any note can be connected to any other block, things you'd be alright with being publicly known get connected with ideas that you'd like to keep private. Roam's graph publishing is an all-or-nothing proposition, but I wanted finer control, so it would be easy to move things from private to public and the inverse, while preserving the flexibility of connection. Goddinpotty started as an effort to adress this need.

The basic goal is to allow a user to share notes they are fine with being seen publicly while minimizing private information exposure. It works by having the user specify **entry points** and **exit points**. 

Entry points are simply where you want Goddinpotty to *start* including pages/blocks from your Roam database. Entry points are defined using a Roam tag, by default `#EntryPoint` although you can change or extend the tags used by means of the config file.

Goddinpotty will walk the graph and include every page it finds, until it hits an exit point. Exit points are also defined by tag, by default `#ExitPoint` or `#Private`.  If this tag is in the first block of a page, the whole page is excluded. If it's in an inner block, the containing block and all of its children are excluded. 

This convention is a bit unintuitive, so an example:

```
- This text is included (public)
- So is this
- But this text will be omitted because of 
  - the #Private tag
  - and so will this
- This will be included
```

Daily notes pages are excluded by default, but can be included by adding 
``` 
:daily-notes? true
```
 to the config.

There is currently no way to specify "export everything", but that wouldn't be hard to add.

# Sidenotes

Sidenotes are awkward due to Roam and CSS limitiations. Goddinpotty leverages block references ... (actually write this stuff in Roam and incorporate it into AMMDI)


# Config

There are a bunch of options in the config file and there probably should be more. 

TODO document them.

