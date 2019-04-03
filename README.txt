This folder contains a few different types of files:

The 6 numbered folders represent different parts of a full act-r voting module. To combine them, load combine.lisp into ccl with act r loaded as well and call one of the functions within (currently just first-for-each). To do this one might have to move combine.lisp into one's ccl folder. Combine.lisp will create a new file called combination.lisp. Importantly, before calling combine.lisp one needs to edit it and hardcode the location of A-Voting-Folder, as well as the desired location for combination.lisp (it makes sense to just make it A-Voting-Folder/combination.lisp).

The Contract pdf defines the requirements for writing new parts for each of the folder types (1Ballot, 2Memory, etc.). One can also look at the examples within each folder for a good example.

To get a better sense of how everything comes together, one can look at the file within the fully combined example folder, which contains a nicely formatted full model (note that the creation of combination.lisp from the combine.lisp file will result in an act-r model that is not very human readable).