#' Browse rrt libraries
#'
#' Open up a dashboard in your default browse.
#'
#' @export
#' @param repoid Respository id, default is NULL, so gets all repos
#' @param output File to output RRT dashboard file to.
#' @examples
#' rrt_browse()
rrt_browse <- function(repoid=NULL, output=NULL){

  repos <- rrt_repos_list(repoid=repoid)
  names(repos) <- NULL
  for(i in seq_along(repos)) repos[[i]]$singlepage <- tempfile(fileext=".html")

  template <-
    '<!DOCTYPE html>
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

      <div class="container">

      <center><h2><i class="fa fa-list-ul"></i>  RRT Dashboard</h2></center>

      <table class="table table-striped table-hover table-responsive" align="center">
      	<thead>
      		<tr>
      			<th>RepoID</th>
            <th>RepoDir</th>
      			<th>InstalledWith</th>
            <th>InstalledFrom</th>
            <th>RRT_ver</th>
            <th>R_ver</th>
            <th>DateCreated</th>
            <th>Pkgs</th>
            <th>SysReq</th>
      		</tr>
      	</thead>
      	<tbody>
        {{#repos}}
          <tr>
            <td><a href="{{singlepage}}" class="btn btn-info btn-xs">{{RepoID}}</a></td>
            <td>{{repo}}</td>
            <td>{{InstalledWith}}</td>
            <td>{{InstalledFrom}}</td>
            <td>{{RRT_ver}}</td>
            <td>{{R_ver}}</td>
            <td>{{DateCreated}}</td>
            <td>{{Pkgs}}</td>
            <td>{{SysReq}}</td>
          </tr>
        {{/repos}}
        </tbody>
      </table>
      </div>

      <script src="http://code.jquery.com/jquery-2.0.3.min.js"></script>
      <script src="http://netdna.bootstrapcdn.com/bootstrap/3.1.1/js/bootstrap.min.js"></script>

      </body>
      </html>'

  template_onepage <-
      '<!DOCTYPE html>
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

        <center><h2><i class="fa fa-list-ul"></i>  RRT Dashboard - {{#single}} {{RepoID}} {{/single}}</h2></center>

        <div class="jumbotron" span>

          {{#single}}
          <ul>
            <li>RepoID: {{RepoID}}</li>
            <li>Repo directory: {{repo}}</li>
            <li>RRT Installed with: {{InstalledWith}}</li>
            <li>RRT Installed from: {{InstalledFrom}}</li>
            <li>RRT version: {{RRT_ver}}</li>
            <li>R version: {{R_ver}}</li>
            <li>Date repo created: {{DateCreated}}</li>
            <li>Packages installed: {{Pkgs}}</li>
            <li>System requirements: {{SysReq}}</li>
          </ul>
          {{/single}}

        </div>

        <script src="http://code.jquery.com/jquery-2.0.3.min.js"></script>
        <script src="http://netdna.bootstrapcdn.com/bootstrap/3.1.1/js/bootstrap.min.js"></script>

        </body>
        </html>'

  rendered <- whisker.render(template)
  for(i in seq_along(repos)){
    single <- repos[[i]]
    tmp <- whisker.render(template_onepage)
    write(tmp, file = repos[[i]]$singlepage)
  }
#   rendered <- gsub("&lt;em&gt;", "<b>", rendered)
#   rendered <- gsub("&lt;/em&gt;", "</b>", rendered)
  if(is.null(output))
    output <- tempfile(fileext=".html")
  write(rendered, file = output)
  if(browse) browseURL(output)
}