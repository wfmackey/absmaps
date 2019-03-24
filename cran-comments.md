# Round 1

## Test environments

* local OS X install, R 3.5.3
* ubuntu 14.04.5 LTS (on travis-ci), R 3.5.2
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE:

* checking CRAN incoming feasibility ... NOTE
    Maintainer: ‘Will Mackey <wfmackey@gmail.com>’

    New submission


## Downstream dependencies
There are currently no downstream dependencies for this package

## CRAN reviewer comments

    Matthias Sterrer 2019-03-24:

    Thanks for submitting!

    Please write package names, software names and API names in undirected 
    single quotes (e.g. 'sf') and add '()' behind all function names (e.g. 
    geom_sf() from 'ggplot2') in your Description text.

    If there are references describing the methods in your package, please 
    add these in the Description field of your DESCRIPTION file in the form
    authors (year) <doi:...>
    authors (year) <arXiv:...>
    authors (year, ISBN:...)
    with no space after 'doi:', 'arXiv:' and angle brackets for auto-linking.

    Please provide small executable examples (which are not wrapped in 
    /dontrun) in all of your Rd-files.

    Please ensure that your functions do not write by default or in your 
    examples/vignettes/tests in the user's home filespace. That is not allow 
    by CRAN policies. Please only write/save files if the user has specified 
    a directory. In your examples/vignettes/tests you can write to 
    tempdir(). E.g.

    Fix and resubmit.

# Resubmission (Round 2)

Fixed package names in Description text.

There are no references describing methods in the package.

'dontrun' has **not** been removed from examples in Rd files. This would download a file (20.5MB is the smallest possible) and I am concerned this is not CRAN-appopriate. Can implement if this is desirable.

Fixed default writing to user's filespace. Functions now require a user-specified directory for writing.