---
layout: page
title: MRAN Overview
---

* [What is MRAN?](#whatismran)
* [MRAN](#indetail)
* [How are snapshots created?](#snapshots)
* [Important note regarding dates](#dates)
* [Difference between snapshots of MRAN](#diffsnaps)
* [Metadata and RRT integration](#metadatarrt)

### <a href="#whatismran" name="whatismran">#</a> What is MRAN?
MRAN is a server side component that works hand-in-hand with the client side
RRT package.
The goal of MRAN is to better organize the various places that R packages
live on CRAN, and add point in time capabilities.

### <a href="#indetail" name="indetail">#</a> In detail - MRAN (Marmoset R Archive Network)
MRAN is downstream snapshot of CRAN. The main differentiation of MRAN to CRAN
is that MRAN consists of a series of snapshots that are taken every three
hours using a script that points to the master CRAN server in Vienna, Austria.
Each MRAN snapshot holds all source versions of a package in the same
per-package directory on the MRAN server.  
i.e. mran.revolutionanalytics.com/snapshots/2014-06-26_1400/zoo

### <a href="#snapshots" name="snapshots">#</a> How are snapshots created?
Snapshots are created using [ZFS](http://open-zfs.org/wiki/Main_Page).
The MRAN server is specifically using
[ZFS-on-Linux](http://zfsonlinux.org/).
The ZFS-on-linux project was started at Lawrence Livermore National Laboratory.
Open-ZFS is an open source community project that has a wide range of contributors 
and sponsors that comprise it's ecosystem.

ZFS was chosen as the snapshot method for MRAN as it works on the block level,
not the file level like many other tools. It is efficient for storing binary
files, unlike other tools such as git, mercurial or subversion.
ZFS is very space efficient: ZFS snapshots only take up the amount of
space that has changed between the snapshot and the 'live' file system. i.e.
very small when looking at the daily churn of R packages, but great space
savings when looking at the ecosystem of packages hosted on CRAN as a whole
over the course of a year.  
A current overview of space usage on the current MRAN server can be found at:  
http://mran.revolutionanalytics.com/accounting.txt  
All MRAN snapshots are exposed at:  
http://marmoset.revolutionanalytics.com/snapshots/


### <a href="#dates" name="dates">#</a> Important note regarding dates
MRAN snapshots are taken using the UTC time zone as the basis for dates.


### <a href="#diffsnaps" name="diffsnaps">#</a> Differences between snapshots of MRAN

The `zfs diff` command is the method that diffs are created with.
Diffs are taken between the most recent snapshot and the previous
snapshot so that RRT or a user can compare differences between them
at any point in time.

The output of the diffs are exposed at:  
http://marmoset.revolutionanalytics.com/diffs/

### <a href="#metadatarrt" name="metadatarrt">#</a> Metadata and RRT integration

mran_metadata.R creates metadata from each snapshot of source packages.
A variety of metadata above and beyond what a CRAN package description file would contain is stored for each package.  
All metadata is exposed at:  
http://marmoset.revolutionanalytics.com/metadata/
