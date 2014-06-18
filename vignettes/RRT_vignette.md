<!--
%\VignetteEngine{knitr::knitr}
%\VignetteIndexEntry{RRT vignette}
-->

RRT vignette
======

### Install RRT


```r
install.packages("devtools")
library("devtools")
devtools::install_github("RevolutionAnalytics/RRT")
```

Load `RRT`


```r
library("RRT")
```

### Create an RRT repository

#### Create a repo without user input


```r
rrt_init("~/onemorerepo")
```

```
## Checking to see if repository exists already...
## Creating repository ~/onemorerepo
## Checing to make sure rrt directory exists inside your repository...
## Creating rrt directory ~/onemorerepo/rrt/lib/x86_64-apple-darwin13.1.0/3.1.0
## Looking for packages used in your repository...
## Writing repository manifest...
```

```
## Warning: cannot open file
## '/Users/sacmac/mynewrepository/rrt/rrt_manifest.txt': No such file or
## directory
```

```
## 
## >>> RRT initialization completed.
```

You should now have a RRT repository

#### Or create a repo interactively

This process will ask you questions


```r
rrt_init(interactive=TRUE)
```

With similar message as above for other checks

### Refresh repository

`rrt_refresh()` is used to update the packages installed locally in your repository by looking through the repository files again for new packages. After we initiated a new repo above with `rrt_init()` we may add some code in a `code.R` file. Then we want to update the packages in the repo, which can be done with `rrt_refresh()`.


```r
rrt_refresh("~/onemorerepo")
```

```
## Checking to make sure repository exists...
## Checing to make sure rrt directory exists inside your repository...
## Looking for packages used in your repository...
## Getting new packages...
## Writing repository manifest...
## 
## >>> RRT refresh completed.
```

As you can see `rrt_refresh()` scans the repo for new packages used, downloads them if any new ones, and updates the manifest file.

### Install packages

`rrt_init()` and `rrt_refresh()` do not install any packages. Packages are downloaded, but not installed yet. The installation process is separate on purpose, but if needed could become part of the initialization and/or refresh functions.

`rrt_install()` installs packages into the repository itself


```r
rrt_install("~/onemorerepo")
```

```
## Checking to make sure repository exists...
## Checing to make sure rrt directory exists inside your repository...
## Looking for packages used in your repository...
## Getting new packages...
## No packages found to install
```

### Package compatibility check

This will be done by `rrt_compat()` - this function is not done yet...

### Browse your RRT repositories

This function uses `rrt_repos_list()` (see below) internally, and uses the `whisker` R package to build a series of web pages to easily understand what RRT repos exist on your machine, their details, etc.


```r
rrt_browse()
```

Should open up a web page in your default browser

![](../inst/img/browse_main.png)

You can click on each green button to get to more detailed data for each repository

![](../inst/img/browse_singlepage.png)

### Get a list of repositories within R

All repositories on your machine


```r
rrt_repos_list()
```

```
## Warning: cannot open file
## '/Users/sacmac/mynewrepository/rrt/rrt_manifest.txt': No such file or
## directory
```

```
## Error: cannot open the connection
```

Get details for a single repository


```r
rrt_repos_list()[[1]]
```

```
## Warning: cannot open file
## '/Users/sacmac/mynewrepository/rrt/rrt_manifest.txt': No such file or
## directory
```

```
## Error: cannot open the connection
```

### Starting R from a repo

If you start R from a RRT repository R will use the repository specific `.Rprofile` file and look for packages in the repository to install instead of the global R packages library.
