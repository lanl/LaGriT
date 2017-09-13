---
GENERATOR: 'Mozilla/4.7C-SGI [en] (X11; I; IRIX64 6.5 IP30) [Netscape]'
Generator: Microsoft Word 98
title: 3
---

**   Point Selection**

 **getptyp(**point\_type\_name,point\_type,ierror)

  This routine converts point type names to point types.

  [See III.a](meshobject.md)for a list of point types, names and
  meanings

  point\_type\_name            name of point type (character
*32)

  point\_type                       value of point type (integer)

 **unpackpc(**npoints,itp,isn,iparents)

  This routine returns in the array iparents the parent point
  corresponding to each child point i, if point i is a child point.
  Ordinary points are their own parents.  The first three arguments
  are usually mesh object attributes described in [Section
  III.a](meshobject.md)

  npoints                        input number of nodes (integer)

  itp1                             input array of point types (integer
  array)

  isn1                             input array of parent child links
  (integer array)

  iparents                       input/output array of parent node
  number for each point (integer array)

 **unpacktp**(ioptitp,iopt2,inum,ipitp1,ipitp2,ierror)

 This routine sets, or's in, or and's in (depending on iopt2) a 1 in
 the array pointed to by ipitp2 for each point that fits the criterion
 specified by ioptitp. ioptitp uses the point types as defined in
 [Section III.a ](meshobject.md)  and ipitp1is the pointer to the
 integer array of point types.

 A zero is set, or'd or and'd otherwise.

 ioptitp criterion

 **  allreal             ** (0&lt;itp1(i)&lt;19)

 **  interior           ** (itp1(i)=0)

 **  inteintf**                (itp1(i)=2,3,4)

 **  matlintr           ** (itp1(i)=2,4,8,9,12,13,15,19)

 **  boundary         ** (8&lt;itp1(i)&lt;19)

 **  reflect             ** (itp1(i)=9,10, 12, 14, 15,16,18,19)

 **  free                ** (itp1(i)=8,9,11, 13, 14, 15,17,18)

 **  intrface           ** (itp1(i)=2,3,4,8,9,12, 13,15,16,17,18,19)

 **  virtual**                (itp(i)=3,4,8,9,16,17,18,19)

 **  removed          ** (20&lt;itp1(i)&lt;29)

 **  merged           ** (itp1(i)=20)

 **  dudded           ** (itp1(i)=21)

 opt2     operation

 **          set                    ** set itp2 to 1 or 0

 **          or                     ** or in a 1 or 0 in itp2

 **          and                  ** and in a 1 or 0 in itp2

                    inum                         input number of nodes
in the array pointed to by ipitp1

                    ipitp1                        input pointer to
integer array of point types (length inum)

                    ipitp2                        input pointer to
output integer array of 1's or 0's (length inum)

                                                      on output array
pointed to will have been filled.
