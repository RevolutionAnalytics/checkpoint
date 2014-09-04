#' Browse rrt libraries
#'
#' Open up a dashboard in your default browse.
#'
#' @import whisker
#' @param repoid Respository id, default is NULL, so gets all repos
#' @param output File to output RRT dashboard file to.
#' @param browse (logical) If TRUE (default), web page opens in your default browse. If FALSE, html
#' file written to disk and path to that file printed to console.
#' @export
#' @family rrt
#' @examples \dontrun{
#' rrt_browse()
#' }

rrt_browse <- function(repoid=NULL, output=NULL, browse=TRUE)
{
  wwwdir <- file.path(rrtPath(type="rootdir"), "www")
  if(!file.exists(wwwdir)) dir.create(wwwdir, recursive = TRUE)

  if(is.null(output))
    output <- file.path(wwwdir, "rrt.html")

  repos <- rrt_repos_list(repoid=repoid)
  names(repos) <- NULL
  for(i in seq_along(repos)) 
    repos[[i]]$singlepage <- file.path(
      wwwdir, sprintf("%s.html", repos[[i]]$RepoID))
  
  for(i in seq_along(repos)) 
    repos[[i]]$homepage <- output
  
  for(i in seq_along(repos)) 
    repos[[i]]$Packages <- gsub(",", ", ", repos[[i]]$Packages)
  
  for(i in seq_along(repos)) 
    repos[[i]]$Packages <- paste(repos[[i]]$Packages, collapse=",  ")
  

  # get check results, if any, for each repository
  bb <- lapply(repos, function(x){
    tmp <- suppressWarnings(tryCatch(
      readRDS(file.path(x$repo_root, "rrt/check_result.rds")), 
      error=function(e) e))
    if("error" %in% class(tmp)) NA else tmp
  })
  names(bb) <- vapply(repos, "[[", "", "RepoID")
  for(i in seq_along(repos)){
    tt <- bb[[ repos[[i]]$RepoID ]]
    ttt <- if(is.data.frame(tt)){
      tt <- lapply(rowSplit(tt), as.list)
      names(tt) <- NULL
      checktab <- whisker.render(checktemplate)
      checktab <- gsub("<td>TRUE", '<td class="success">TRUE', checktab)
      gsub("<td>NA", '<td class="info">NA', checktab)
    } else {
      '
      <div style="background-color: #ecf0f1;">
        <div class="container">
          <h4><button class="btn btn-xs btn-success">Checks:</button> none</h4>
        </div>
      </div>'
    }
    repos[[i]]$checkres <- ttt
  }

  rendered <- whisker.render(template)
  for(i in seq_along(repos)){
    single <- repos[[i]]
    tmp <- whisker.render(template_onepage)
    write(tmp, file = repos[[i]]$singlepage)
  }
  write(rendered, file = output)
  if(browse) browseURL(output)
}

template <-
  '<!DOCTYPE html>
    <html style="background-color: #ecf0f1;">
      <head>
        <meta charset="utf-8">
        <title>RRT - Dashboard</title>
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
      	<meta name="description" content="View highlights from rplos search">
      	<meta name="author" content="RRT">

      	<!-- Le styles -->
      	<link href="http://netdna.bootstrapcdn.com/bootstrap/3.1.1/css/bootstrap.min.css" rel="stylesheet">
        <link href="http://netdna.bootstrapcdn.com/font-awesome/4.0.3/css/font-awesome.css" rel="stylesheet">
      </head>

      <body>

      <div style="background-color: #ecf0f1;">
        <div class="container">
        <center><h2>RRT Dashboard</h2></center>
        <table class="table table-hover table-responsive" align="center">
        	<thead>
        		<tr>
        			<th>RepoID</th>
              <th>RepoDir</th>
              <th>R_ver</th>
              <th>DateCreated</th>
              <th>DateUpdated</th>
        		</tr>
        	</thead>
        	<tbody>
          {{#repos}}
            <tr>
              <td><a href="{{singlepage}}" class="btn btn-info btn-xs">{{RepoID}}</a></td>
              <td>{{repo_root}}</td>
              <td>{{R_version}}</td>
              <td>{{DateCreated}}</td>
              <td>{{DateUpdated}}</td>
            </tr>
          {{/repos}}
          </tbody>
        </table>
        </div>
      </div>

      <script src="http://code.jquery.com/jquery-2.0.3.min.js"></script>
      <script src="http://netdna.bootstrapcdn.com/bootstrap/3.1.1/js/bootstrap.min.js"></script>

      </body>
      </html>'

template_onepage <-
  '<!DOCTYPE html>
    <html style="background-color: #ecf0f1;">
        <head>
        <meta charset="utf-8">
        <title>RRT - Dashboard</title>
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <meta name="description" content="View highlights from rplos search">
        <meta name="author" content="RRT">

        <!-- Le styles -->
        <link href="http://netdna.bootstrapcdn.com/bootstrap/3.1.1/css/bootstrap.min.css" rel="stylesheet">
        <link href="http://netdna.bootstrapcdn.com/font-awesome/4.0.3/css/font-awesome.css" rel="stylesheet">

        <style>
        background-color: #ecf0f1;
        </style>

        </head>

        <body>

        <div style="background-color: #ecf0f1;">
          {{#single}}
          <center><h2><a href="{{homepage}}"><i class="fa fa-home"></i></a>  RRT Dashboard - {{#single}} {{RepoID}} {{/single}}</h2></center>
          <div class="container">
            <ul style="list-style-type: none;">
              <li><h4><button class="btn btn-xs btn-success">Repo Name:</button> {{RepositoryName}}</h4></li>
              <li><h4><button class="btn btn-xs btn-success">Authors:</button> {{Authors}}</h4></li>
              <li><h4><button class="btn btn-xs btn-success">License:</button> {{License}}</h4></li>
              <li><h4><button class="btn btn-xs btn-success">Description:</button> {{Description}}</h4></li>
              <li><h4><button class="btn btn-xs btn-success">Remote:</button> {{Remote}}</h4></li>
              <li><h4><a href="file:///{{repo_root}}" class="btn btn-xs btn-success">Repo directory: </a> {{repo_root}}</h4></li>
              <li><h4><button class="btn btn-xs btn-success">RRT Installed with:</button> {{InstalledWith}}</h4></li>
              <li><h4><button class="btn btn-xs btn-success">RRT Installed from:</button> {{InstalledFrom}}</h4></li>
              <li><h4><button class="btn btn-xs btn-success">RRT version:</button> {{RRT_version}}</h4></li>
              <li><h4><button class="btn btn-xs btn-success">RRT snapshot ID:</button> {{RRT_snapshotID}}</h4></li>
              <li><h4><button class="btn btn-xs btn-success">R version:</button> {{R_version}}</h4></li>
              <li><h4><button class="btn btn-xs btn-success">Date repo created:</button> {{DateCreated}}</h4></li>
              <li><h4><button class="btn btn-xs btn-success">Date repo updated:</button> {{DateUpdated}}</h4></li>
              <li><h4><button class="btn btn-xs btn-success">Packages installed:</button> {{Packages}}</h4></li>
              <li><h4><button class="btn btn-xs btn-success">System requirements:</button> {{SystemRequirements}}</h4></li>
            </ul>
          </div>
        </div>

        {{{checkres}}}
        {{/single}}

        <script src="http://code.jquery.com/jquery-2.0.3.min.js"></script>
        <script src="http://netdna.bootstrapcdn.com/bootstrap/3.1.1/js/bootstrap.min.js"></script>

        </body>
        </html>'



checktemplate <- '
<div style="background-color: #ecf0f1;">
        <div class="container">

  <div class="panel-group" id="accordion">
  <div class="panel panel-default">
    <div class="panel-heading">
      <h4 class="panel-title">
        <a data-toggle="collapse" data-parent="#accordion" href="#collapseOne">
          Checks
        </a>
      </h4>
    </div>
    <div id="collapseOne" class="panel-collapse collapse in">
        <div class="container">
          <table class="table table-hover table-responsive" align="center">
                <thead>
                  <tr>
                    <th>Package</th>
                    <th>Check Result</th>
                    <th>Test Result</th>
                    <th>Examples Result</th>
                    <th>Update</th>
                  </tr>
                </thead>
        <tbody>
        {{#tt}}
        <tr>
        <td>{{pkg}}</td>
        <td>{{check_result}}</td>
        <td><a href="{{testfile}}">{{testfile}}</a></td>
        <td><a href="{{examplesfile}}">{{examplesfile}}</a></td>
        <td>{{update}}</td>
        </tr>
        {{/tt}}
        </tbody>
            </table>
      </div>
      </div>
  </div>
</div>
'
