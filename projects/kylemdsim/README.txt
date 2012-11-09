Here's a transition from a job that crashes the cluster because it requires more than 10GB of RAM in order to complete its work.

The objective is to make it run in a much smaller memory footprint.

That is done in two stages.  

First, figure out why the program uses more and more storage.

Second, re-design to 
1. make the program save work to disk periodically.

2. reduce the scale of storage required at any given
instant by changing the design.  DO NOT generate an
array of data sets and then run complicated stuff
on each one and save all that into a list. That's a
memory disaster waiting to happen. Instead, create
one data set, analyze it, keep what you need in a 
list, throw away what is not needed. Then generate
another, and so forth.  Basically, it is necessary
to stop thinking of this like a SAS project.

That work is all done on the serial version of the
program.  After that, we turn our attention to a plan
to run that in parallel on the cluster.
But we won't do that until we are sure we are getting
the output we intend from this version, of course.


Other points of interest.
1. It is dangerous/confusing when all of the work
has to be lumped into a single function.  This is 
needed because we want to parallelize that function
easily, but it makes it really tough to feel sure that
 the data is "clean" and free of misunderstandings.

At several points, I renamed objects to give them
unique names to make sure there was no accidental
"out of intended scope" access to the wrong values.

It is especially dangerous when one variable name, "x"
exists in the scope at several levels with different
meanings.  That is not completely cleaned up yet.

2. The style for naming functions should not use
periods in a gratuitous way.  R uses the period as a
substantively significant punctuator of object-oriented
design. So I rename go.baby.go to goBabyGo.  Lots of
other work could be done in that way here.
