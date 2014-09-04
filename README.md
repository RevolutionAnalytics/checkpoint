# RRT - The Reproducibile R Toolkit


Build status

master: [![](https://api.travis-ci.org/RevolutionAnalytics/RRT.png?branch=master)](https://travis-ci.org/RevolutionAnalytics/RRT)
dev: [![](https://api.travis-ci.org/RevolutionAnalytics/RRT.png?branch=dev)](https://travis-ci.org/RevolutionAnalytics/RRT)

Active development is on the `dev` branch - A more stable version on the `master` branch



## Installation

### System requirements

On Linux, get xml C library first (on the command line)

```
(sudo) apt-get update
(sudo) apt-get install r-cran-xml
```

You may need libcurl too. Do report in the issues tab if you run into this problem.


### Installing RRT from github

Then install `RRT` from your R session:

```
install.packages("devtools")
library("devtools")
devtools::install_github("RevolutionAnalytics/RRT")
library("RRT")
```


## More information

### Examples

See the [RRT wiki](https://github.com/RevolutionAnalytics/RRT/wiki/Examples) for examples.

### Bugs or feature requests

Post an issue on the [Issue tracker](https://github.com/RevolutionAnalytics/RRT/issues)

### IRC channel on Freenode
`#RRT`

### Made by

[Revolution Analytics](http://www.revolutionanalytics.com/)
