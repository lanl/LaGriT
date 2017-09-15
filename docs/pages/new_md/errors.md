---
GENERATOR: 'Mozilla/4.7C-SGI [en] (X11; I; IRIX64 6.5 IP30) [Netscape]'
---

ERRORS

 

 Errors in Parsing or Executing Commands:

 These errors cause an error message to be written to the output batch
 file, called logx3dgen by default and to the standard output file. 
 The messages are self-explanatory and will alert the user to the
 required change.  For example one of the errors related to the region
 command is "*token* is an invalid operation or a non-defined surface
 or region."   In this case the user must inspect the corresponding
 command in the vicinity of *token* which is the place in parsing the
 string that LaGriT could not proceed, to see if there might be a
 spelling error in specifying a operator, region name or surface name
 or perhaps the user left out an operator.  If the correct commands
 should read:

 surface/s1/reflect/box/0,0,0/1,1,1

 surface/s2/intrface/box/.2,.2,.2/.4,.4,.4/

 region/r1/le s1 and ge s2

 region/r2/ le s2

 If the user had mistakenly typed:

 region/r1/le s1 and gg s2

 the error "REGION ERROR: gg is an invalid operation or a non-defined
 surface or region" would appear

 followed by "error in command : region/r1/le s1 and gg s2

 Out of Memory Errors:

 If a  user specifies a probelem that is too large, LaGriT will fail
 with and "MALLOCF: Out of memory" or "REALLOCF: Reallocation error -
 aborting" mesage.  The user must decrease the size of the problem or
 move to a more powerful platform.

 Fatal Memory Management Errors:

 Fatal memory management errors are signaled by a message containing
 ":PANIC!"  This type of error should be reported to the code manager,
 and the report should include all files and other information
 necessary to reproduce the error.

  

