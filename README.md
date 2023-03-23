# WGC-age-report-demo
*A demo of a tentative framework for automated communication of age data products to end users at the AFSC*


The Age and Growth Program at the Alaska Fisheries Science Center is developing a set of tools to automate summary reports that accompany age data released to end users. These reports describe results of inter-reader testing and properties underlying the aged sample. We provide an example here to support an interactive demonstration at the Western Groundfish Conference in Juneau, Alaska, on April 24, 2024.

Prior to the demo, users should ensure they have a version of RStudio that supports Quarto and that TinyTeX is installed (to create PDFs). RStudio may require a restart after installing TinyTex. More information is available [here](https://yihui.org/tinytex/).

`install.packages('tinytex')`

`tinytex::install_tinytex(TRUE)`

Users may also want to check to ensure they have the following packages (available from CRAN) installed prior to the demo.

`tidyverse`
`FSA`
`patchwork`
`knitr`
`kableExtra`