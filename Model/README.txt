The 6 numbered folders represent different parts of a full act-r voting model (as well as the parameters.lisp file not in any of the folders). If you want to combine them into a full model and run the model, take the following 5 steps:
1. Download A-Voting-Folder with all of its files onto a location, call it LOCATION, on your computer
2. Open up the file combine.lisp and change the parameter base-file-name to be set to LOCATION/A-Voting-Folder
3. Open up a ccl environment (other Lisp implementations may also work) and load ACT-R
4. Load combine.lisp into ccl (i.e. (load "LOCATION/combine.lisp"))
4. Call one of the functions within (see documentation for these two functions in combine.lisp)


If you want to write new pieces for the model (e.g. to create a new macronavigation strategy), the contract pdf (Contract.pdf) defines the requirements for writing new parts for each of the folder types (1Ballot, 2Memory, etc.). One can also look at the examples within each folder for a good example. Note that if you are writing new pieces that are not micronavigation or memory, you will have to change run-lists to loop through these as well, as currently run-lists only loops through every possibility of memory and micronavigation.

Other files in this folder:
"Contract.pdf", mentioned above. It is an attempt to rigirously define the responsibilities of each piece of the model so that combine can put them together successfully. 
"highlevel with strategies changed.R", an R analysis file that creates plots from data files. Might need to change bounds of graphs if using mouse noise, will need to change the name of the input file in the folder to redirect to the newest set of data (useful to rename data files of good runs so they are not just a meaningless date).
"conversion.text", a text file describing how the old complete models were changed to the current micronavigation model. Will probably not need to be referenced any more as all files have been converted.
"logging.lisp", a file that is not explicitly part of the model but is loaded in by combine.lisp to track the model's actions (every time the model runs a trace of its voting behavior is saved into the data folder)
