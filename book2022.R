
####################################

# the development version from GitHub (recommended)
install.packages('devtools')
devtools::install_github("pzhaonet/bookdownplus")
# or the stable version from CRAN
install.packages("renv")
install.packages("bookdown")
install.packages("servr")
install.packages("bookdownplus")
require(bookdownplus)
get_template()
#bookdownplus()

bookdownplus(template = "journal",author = "Jiahuan Niu",title = "My journal")

#get_output()
#bookdownplus(more_output = get_output())
#debugging
update.packages(ask = FALSE, checkBuilt = TRUE)
#tinytex::tlmgr_update()
options(tinytex.verbose = TRUE)
tinytex::install_tinytex()

#! LaTeX3 Error: Mismatched LaTeX support files detected.
#(LaTeX3)        Loading 'expl3.sty' aborted!
#  (LaTeX3)        
#(LaTeX3)        The L3 programming layer in the LaTeX format
#(LaTeX3)        is dated 2021-11-22, but in your TeX tree the files require
#(LaTeX3)        at least 2022-02-05.

#! LaTeX Error: File `scrpage2.sty' not found.
#\RequirePackage{scrlfile}
#\ReplacePackage{scrpage2}{scrlayer-scrpage}











