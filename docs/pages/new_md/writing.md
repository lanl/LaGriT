---
GENERATOR: 'Mozilla/4.7C-SGI 
[en
] (X11; I; IRIX64 6.5 IP30) 
[Netscape
]'
Generator: Microsoft Word 98
---

 

**Writing user commands**

The access to user written subroutines is through the LaGriT subroutine,
user\_sub. It is passed the parsed command input line. The parser breaks
up the input line into tokens and returns to LaGriT a count of number of
tokens, an array containing the token types, and the tokens themselves.
The parameters returned by the parser are:

nwds ( number of tokens)

msgtyp (integer array of token types : 1 for integer, 2 for real, 3 for
character, msgtyp(nwds+1) = -1)

imsgin (array of integer tokens, e.g. if msgtyp(i)=1 then the ith token
is type integer and imsgin(i) contains its value)

xmsgin (array of real tokens, e.g. if msgtyp(i)=2 then the ith token is
type real and xmsgin(i) contains its value)

cmsgin (array of character tokens, e.g. if msgtyp(i)=3 then the ith
token is type character and cmsgin(i) contains its value)

Null fields are given the integer value 0, real value 0, and character
value '-def-'. If the user has written a subroutine, my\_routine,that
responds to the command, my\_comnd, the call from user\_sub should look
like:

elseif (cmsgin(1).eq. 'my\_comnd')

  x           call my\_routine(nwds,imsgin,xmsgin,cmsgin,msgtyp,ierr1)

The subroutine my\_routine should set ierr1 to zero if the command is
processed successfully and should use the [cmo](meshob.md) interface
routines to access the components of the Mesh Object that it needs, for
example:

 

       subroutine my\_routine(nwds,imsgin,xmsgin,cmsgin,msgtyp,ierr1)

      integer nwds, imsgin(
*),msgtyp(
*),ierr1

      real xmsgin(
*)

      character
*32 cmsgin(
*)

        ....

      character
*32 cmo

      pointer (ipimt1, imt1(
*))

      integer ierror,ilen,ityp,nnodes

        ....

c get the name of the current mesh object

      call cmo\_get\_name(cmo\_name,ierror)

c get the number of nodes and the material ids

      call
cmo\_get\_intinfo('nnodes',cmo\_name,nnodes,ilen,ityp,ierror)

      call cmo\_get\_info('imt1',cmo\_name,ipimt1,ilen,ityp,ierror)

 

LaGriT will check it's internal command list and if the input command is
not found it will call user\_sub.  If user\_sub does not find the
command, LaGriT will issue  the error message: 'Illegal command' and
continue with the next command.

An empty user\_sub is distributed with the sample driver program
contained in the file adrivgen.f
