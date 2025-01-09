**Character Length**

  The following functions are provided to return character string
  length (number of characters in iword ignoring terminator
  character).

**icharln**(iword)

Get length by searching forward for first blank or
null.

**icharlnf**(iword) 

Ignore leading blanks then search for terminating
blank or null.

**icharlnb**(iword) 

Search backwards for first non-blank or non-null -
uses FORTRAN function **len** to give starting point (this is a risky
assumption). 

**nulltoblank\_lg**(iword, length)

iword is the character string that will be searched for null
characters. All null characters will be replaced by blanks.

length is the number of characters in iword.
