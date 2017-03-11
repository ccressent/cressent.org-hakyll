# cressent.org

This website is built using [Hakyll] (http://jaspervdj.be/hakyll), a static site
generator written in [Haskell] (http://haskell.org)

To get started, with stack:

    git clone https://github.com/ccressent/cressent.org
    cd cressent.org
    stack build
    stack exec website help


## SASS support

To use SASS, first install the sass ruby gem:

    gem install sass

The website generator will then be able to call sass to compile the
*.{sass,scss} files to css.
