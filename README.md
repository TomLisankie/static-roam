# Static-Roam

A static-site generator for [Roam Research](https://roamresearch.com/)

# Status

This is a fork from [the original](https://github.com/TomLisankie/static-roam) by Tom Lisankie. I'm using it for a personal project and it's very much in flux. 

It's pretty diverged from the original by now, and includes a bunch of new features, notably:
- uses definable Exit Points for privacy (see below)
- generates index pages, recently changed, and graphic map pages

# Theory

static-roam is somewhat more page-oriented that Roam itself. Navigation is by page, the identity of blocks doesn't really appear (Except for generating excerpts for the backlink and recent changes views).


# Usage

- create a configuration file by editing `resources/example-config.edn`
- Export All from Roam, choose the JSON format
- `lein run <config file>`

By default, the program will look for the latest Roam Export file in `~/Downloads` and generate output in `./output`

# What gets published

With Roam, there are no limits on how you can connect notes. This is one of Roam's greatest strengths. However, it's its greatest weakness when it comes to sharing notes publicly. Since any note can be connected to any other block, things you'd be alright with being publicly known get connected with ideas that you'd like to keep private. Personally, I think there are great benefits that can be seen from sharing notes publicly that are structured in the ways Roam allows. Static-Roam is an early attempt at solving this.

The basic goal is to allow a user to share notes they are fine with being seen publicly while minimizing private information exposure. It works by having the user specify **entry points** and **exit points**. 

Entry points are simply where you want Static-Roam to *start* including pages/blocks from your Roam database. Entry points are defined using a Roam tag, by default `#EntryPoint` although you can change or extend the tags used by means of the config file.

Static-roam will walk the graph and include every page it finds, until it hits an exit point. Exit points are also defined by tag, by default `#ExitPoint` or `#Private`.  If this tag is in the first block of a page, the whole page is excluded. If it's in an inner block, the containing block and all of its children are excluded. 

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

Sidenotes are awkward due to Roam and CSS limitiations. Static-roam leverages block references ... (actually write this stuff in Roam and incorporate it into AMMDI)



# Config

There are a bunch of options in the config file and there probably should be more. 

TODO document them.

