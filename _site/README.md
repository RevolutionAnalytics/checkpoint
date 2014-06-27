## Recology

Modified from [Erjjones.Github.com](Erjjones.Github.com) (thanks!). 

Page made with Jekyll and Twitter Bootstrap. 

### Workflow for posting on this blog:

+ Fork the repo.
+ Write a post in a .Rmd file (see examples in the /_drafts folder).  Take a look at one of the recent examples in the /_drafts folder, and notice that I just put the yaml heading stuff in the post so that I don't have to add it later - it should all be ready to go after running the next step. 
+ When done writing the post, use the function `knitpost` (see below), which prepares the .md file and outputs any image files needed. You can use it by installing a repo I have on github (sacbox [like sandbox, which many people have for their own personal stuff, but just different name]: https://github.com/sckott/sacbox/blob/master/R/knitpost.r).  You can copy the .md file that knitpost outputs into the /_posts folder, and any images to the /img folder.  
    + When running knitpost, I run something like the following for a post: `knitpost("/Users/ScottMac/github/sac/sckott.github.io/_drafts/2012-10-08-rgbif-newfxns.Rmd")`
+ That should be it.  Try running `jekyll --server` in the terminal to see if it worked.
+ Submit a pull request.

-- This is what `knitpost` looks like:

```r
knitpost <- function(input, base.url = "/") 
{
    opts_knit$set(base.url = base.url)
    fig.path <- paste0("img/", sub(".Rmd$", "", basename(input)), "/")
    opts_chunk$set(fig.path = fig.path)
    opts_chunk$set(fig.cap = "center")
    render_jekyll()
    knit(input, envir = parent.frame())
}
``` 