Paul E. Johnson <pauljohn at ku.edu>
Center for Research Methods and Data Analysis, University of Kansas
2012-11-09

Github readme for top-level repository.

This is the trunk of the rgroup-crmda repository. Members of the
University of Kansas Center for Research Methods and Data Analysis
should create github accounts for themselves and we will work
together.

In the future, we expect this repository to be linked together with
the effors of the University of Kansas student user group, KUseR. That
coordination is a work in progress.

Web view (in a browser)

https://github.com/pauljohn32/rgroup-crmda

To work with the repository, 

Read only access:

git://github.com/pauljohn32/rgroup-crmda.git

Read/write access:

ssh protocol: requires a registered security key

git@github.com:pauljohn32/rgroup-crmda.git

https access:

https://github.com/pauljohn32/rgroup-crmda.git



The Golden Rules

1. Write readable code following established guidelines (below) for
style and presentation.
2. Keep good Changelogs (in Emacs, M-x add-changelog-entry)
3. Respect the work of others by not damaging it. The git system
allows us to "roll back" mistakes, but we'd rather not bother.



The folders are defined as follows

1. documentation


2. packages: R packages.  Make nested folders like so:  


packages
    rockchalk
      	  {build scripts and other useful information about rockchalk,
	  but material that is NOT redistributed with the package}
        rockchalk
	  {the package folders R, man, inst, and so forth}

Before you ask: Yes, this does mean we have a folder and a subfolder
with exactly the same name. 


projects: Example start-to-finish R code and accompanying
    material. Most of these are accumulated from advising on graduate
    projects or developing example code that might later go into packages.

WorkingExamples: single file example R programs that demonstrate
particular functions or packages.

The style for WorkingExamples is based on the R DESCRIPTION file
    format and other standards we follow. The style is explained 
    in the file WorkingExamples: r_style_guide.R

Here is the gist of it.

1. At the top, have the DESCRIPTION information.
2. In the code, follow these elementary bits
   A. indent 4 spaces (not a tab) for each level of scope.
   B. spaces are mandatory around assignments and mathematical
   operators, such as "<-", "*", "+" and so forth.

