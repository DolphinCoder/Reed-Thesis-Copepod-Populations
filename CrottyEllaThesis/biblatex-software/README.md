# biblatex software style

This directory contains the reference `biblatex` implementation of a
bibliography style extension that includes software-specific BibTeX entries.

## Relevant files 

The key files are the following:

 - `software.bbx` for the bibliography style extension
 - `software.dbx` for the data model extension to support the new entries
 - `english-software.lbx` and `french-software.lbx` for the localization
 - `software-biblatex.sty` the LaTeX package to include for using the style as an extension

The distribution contains other material, for demonstration purposes, and for
more advanced use.

## Documentation

Full documentation of the style, with a description of the new entries and
fields and detailed examples, is in `software-biblatex.tex`; a compiled PDF
version is included as `software-biblatex.pdf` for ease of reference.

We recall briefly here the two main approaches available for using this style extension.

### Using the software biblatex style as an extension

The simplest way to use this style is to follow the example given
in the file `sample-use-sty.tex` that shows how one can *extend* any
existing `biblatex` style by simply doing the following:

 - pass the `datamodel=software` option to the `biblatex` package
 - load the software biblatex style with `\usepackage{software-biblatex}`
 - set software specific bibliography options using the macro `\ExecuteBibliographyOptions`;
   the options with their default values are as in
   `\ExecuteBibliographyOptions{halid=true,swhid=true,swlabels=true,vcs=true,license=true}`

This approach is extremely simple: it only requires that you include the relevant files
mentioned above in the directory where your LaTeX file is located.

### Generating biblatex styles that include the software entries

It may be useful to generate a new biblatex style that includes support for the
software entries right away.

A simple mechanism is provided for this use case:

 - add to the `stublist` file the names of all the existing styles one needs to extend
 - run `make biblatex-styles` to produce new style files, with an added `+sw` suffix,
   for each of the existing style
 - install the newly generated files in the standard path where `biblatex` files are found

The stock `stublist` file contains the names of all the standard `biblatex`
styles.  If this approach is followed, then one can load directly the extended
file, and the software specific bibliography options become available when
loading the `biblatex` package directly.  See the `sample.tex` file for a
concrete example.

# License

This material is subject to the LATEX Project Public License version 1.3 or (at
your option) any later version.

# Contact author

This style is maintained by Roberto Di Cosmo <roberto@dicosmo.org>


