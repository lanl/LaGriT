---
GENERATOR: 'Mozilla/4.7C-SGI 
[en
] (X11; I; IRIX64 6.5 IP30) 
[Netscape
]'
Generator: Microsoft Word 98
title: b
---

**Issuing Commands from a user program.**
Any LaGriT command can be issued by calling the subroutine dotask , for
example:
call dotask ('**cmo****/select**/3dmesh; **finish**', ier1)
will select the Mesh Object named 3dmesh*.*ier1 will be zero if the
commands are executed with no error, non-zero otherwise. A sequence of
commands can also be executed using dotask,  for example:

           call dotask ('**cmo****/addatt**//mo;
**cmo****/list**;**finish**',ier1)

will execute the two commands and then return to the next executable
statement.  A **finish** command must always be the last command in the
command list for dotask.  The user should also be aware that mesh object
pointers might be changed by the dotask commands and should be refreshed
using [cmo\_get\_info.](meshob.md#cmo_get_info)  The parser arrays
cmsgin, xmsgin etc. will not be the same upon return from dotask*.*

By using the LaGriT command **infile**, a series of commands may be
executed, for example:
call dotask ('**infile**/mydeck'; **finish**', ier1)
will execute all the LaGriT commands that are in the user's file named
mydeck*.* The final command in the file mydeck should be **finish.**
mydeck itself may contain additional **infile** commands.
