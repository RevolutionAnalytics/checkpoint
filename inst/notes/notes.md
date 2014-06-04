# Notes

* Marmoset server:
  * taking a subset of the vienna CRAN mirror
    * don't' pull down binaries
  * taking snapshots 2x/day
  * then diff with zfs the whole system
  * ~~testing for package compatibility~~ (note: down the road)
  * HTTP server with endpoints for
    * `http::..baseurl/snapshots`
    * `http::..baseurl/snapshots/<snapshotdate>`
      * include just date (maybe something else if needed)
      * date format: `YY-MM-DDTHH:MM` in GMT
  * Web Server: `nginx`
  * create metadata catalog
    * Do all in R first - see how painful
    * use `tools::write_PACKAGES` to create metadata for each package
    * convert to JSON? or not?
    * add additional metadata (with a schema predefined)
      * snapshot metadata
      * diff metadata
      * including compatability (down the road) - Define key-value pair, but leave as `NULL` for now
  * only serves current and archived source pkgs
  * how RRT interfaces the server
    * user specifies a date: snapshots with dates in names, or
    * user specifies package version numbers (BUT pkgs from diff dates may not be compatible)
      * If pkg version named in manifest, force user to accept whatever Marmoset has, but can optionally use older Marmoset versions (i.e., whatever snapshots are avaialable on the server)
  * What about old versions of CRAN? From here forward we are fine, but...

* RRT
  * New function to interface with the "marmoset" server form RRT
  * Function to do `GET` requests for

* For the v0.1 release
  * Snapshots based on date that can be used from RRT
