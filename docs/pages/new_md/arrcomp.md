---
GENERATOR: 'Mozilla/4.7C-SGI [en] (X11; I; IRIX64 6.5 IP30) [Netscape]'
Generator: Microsoft Word 98
title: 6
---

**  Array Compression**

 The following utility routines compress arrays. Note that the output
 array may be the same as the input array in which case the compression
 is done in place. Also the mask array may be the same as the input
 array. The name suffixes of the compression routine may be decoded as
 **m** minus (negative), **n** non-zero, **p** positive, **z** equal to
 zero. If the routine name ends in **rrr**, the mask, input and output
 arrays are all real. If the name ends in a single **r** , the mask is
 real, the input and output arrays are integers. Otherwise the mask,
 input and output arrays are all integers. For example
 kmprsn(100,int,1,int,1,int,1,num) will compress all the zeros out of
 array int.
 **kmprsm(n,z,iz,x,ix,y,iy,count)**
 n              length of z and x

 z               array of masks

 iz              stride in z

 x               array of source

 ix              stride in x

 y               array of output

 iy              stride in y

 count       length of y
 **kmprsn(n,z,iz,x,ix,y,iy,count)**

 **kmprsnr(n,z,iz,x,ix,y,iy,count)**

 **kmprsnrrr(n,z,iz,x,ix,y,iy,count)**

 **kmprsp(n,z,iz,x,ix,y,iy,count)**

 **kmprspr(n,z,iz,x,ix,y,iy,count)**

 **kmprsz(n,z,iz,x,ix,y,iy,count)**

 **kmprszr(n,z,iz,x,ix,y,iy,count)**



