sacbox
=======

R functions often needed...It gets old looking for that function you sometimes, or often, need but can't remember where you put it. Now I can just load these as I would an R package, with roxygen documentation with examples.  This makes any research that uses these functions reproducible since they are open for all to use. 

Install using install_github within Hadley's devtools package.

```R
install.packages("devtools")
require(devtools)
install_github("sacbox", "schamberlain")
require(sacbox)
```