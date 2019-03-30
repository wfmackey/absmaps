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

## CRAN reviewer comments

_Review by Swetlana Herbrandt_

DESCRIPTION issues have been addressed

**Re:** usage of the `download_absmaps` function:

CRAN comment:

    We also get:

    download_absmaps("state", 2016, saveDirectory = "data")
    Downloading state2016 data from abs.gov.au
    Error in utils::download.file(get(paste0(area, year, "_url")), 
    paste0(saveDirectory,  :
       cannot open destfile 'data/state2016.zip', reason 'No such file or 
    directory'

    Any ideas?

The `saveDirectory` that was specified, `data`, did not exist in the user's system. But, I agree that this could be made more clear for the user. The `.Rd` note has been updated to `'...an existing directory...'` and the function now gives a clear stop message if the directory isn't found: `"Your save directory doesn't exist!"`



## Downstream dependencies
There are currently no downstream dependencies for this package
