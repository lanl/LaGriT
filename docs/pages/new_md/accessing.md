---
GENERATOR: 'Mozilla/4.7C-SGI [en] (X11; I; IRIX64 6.5 IP30) [Netscape]'
Generator: Microsoft Word 98
title: d
---

**Accessing the Mesh Object**
The following template is an example of using the an existing mesh
object and of creating a new mesh object. The existing mesh object is a
3d object. The object to be created is a 2d object. It is first
necessary to set up the pointer statements for both the existing and new
mesh objects. All mesh object attributes are integers except for
xic,yix,zic which are real
*8.
C Definitions for incoming (existing) cmo

C

pointer (ipimt1, imt1)

pointer (ipitp1, itp1)

pointer (ipicr1, icr1)

pointer (ipisn1, isn1)

integer imt1(1000000), itp1(1000000),

* icr1(1000000), isn1(1000000)

pointer (ipxic, xic)

pointer (ipyic, yic)

pointer (ipzic, zic)

real
*8 xic(1000000), yic(1000000), zic(1000000)

pointer (ipitetclr, itetclr)

pointer (ipitettyp, itettyp)

pointer (ipitetoff, itetoff)

pointer (ipjtetoff, jtetoff)

pointer (ipitet, itet)

pointer (ipjtet, jtet)

integer itetclr(1000000), itettyp(1000000),

* itetoff(1000000), jtetoff(1000000)

integer itet(4,1000000) , jtet(4,1000000)

C

C Definitions for cmo that is to be created

C

pointer (ipimt1a, imt1a)

pointer (ipitp1a, itp1a)

pointer (ipicr1a, icr1a)

pointer (ipisn1a, isn1a)

integer imt1a(1000000), itp1a(1000000),

* icr1a(1000000), isn1a(1000000)

pointer (ipxica, xica)

pointer (ipyica, yica)

pointer (ipzica, zica)

real
*8 xica(1000000), yica(1000000), zica(1000000)

pointer (ipitetclra, itetclra)

pointer (ipitettypa, itettypa)

pointer (ipitetoffa, itetoffa)

pointer (ipjtetoffa, jtetoffa)

pointer (ipiteta, iteta)

pointer (ipjteta, jteta)

integer itetclra(1000000), itettypa(1000000),

* itetoffa(1000000), jtetoffa(1000000)

integer iteta(3,1000000) , jteta(3,1000000)

C Get the existing cmo - its name is in the variable cmoin

C

call cmo\_get\_name(cmoin,ier)

C

C Get the scalar mesh variables

call cmo\_get\_intinfo('nnodes',cmoin,npoints,lencm,itypcm,ier)

call cmo\_get\_intinfo('nelements',cmoin,ntets,lencm,itypcm,ier)

call cmo\_get\_intinfo('ndimensions\_topo',cmoin,ndt,lencm,itypcm,ier)

call cmo\_get\_intinfo('ndimensions\_geom',cmoin,ndg,lencm,itypcm,ier)

call
cmo\_get\_intinfo('nodes\_per\_element',cmoin,npe,lencm,itypcm,ier)

call
cmo\_get\_intinfo('faces\_per\_element',cmoin,nfpe,lencm,itypcm,ier)

call cmo\_get\_intinfo('mbndry',cmoin,mbndry,lencm,itypcm,ier)

C

C Get pointers to the vector variables

call cmo\_get\_info('ialias',cmoin,ipialias,lenialias,ictype,ier)

call cmo\_get\_info('imt1',cmoin,ipimt1,lenimt1,ictype,ier)

call cmo\_get\_info('itp1',cmoin,ipitp1,lenitp1,ictype,ier)

call cmo\_get\_info('icr1',cmoin,ipicr1,lenicr1,ictype,ier)

call cmo\_get\_info('isn1',cmoin,ipisn1,lenisn1,ictype,ier)

call cmo\_get\_info('xic',cmoin,ipxic,lenxic,ictype,ier)

call cmo\_get\_info('yic',cmoin,ipyic,lenyic,ictype,ier)

call cmo\_get\_info('zic',cmoin,ipzic,lenzic,ictype,ier)

call cmo\_get\_info('itetclr',cmoin,ipitetclr,lenitetclr,ictype,ier)

call cmo\_get\_info('itettyp',cmoin,ipitettyp,lenitettyp,ictype,ier)

call cmo\_get\_info('itetoff',cmoin,ipitetoff,lenitetoff,ictype,ier)

call cmo\_get\_info('jtetoff',cmoin,ipjtetoff,lenjtetoff,ictype,ier)

call cmo\_get\_info('itet',cmoin,ipitet,lenitet,ictype,ier)

call cmo\_get\_info('jtet',cmoin,ipjtet,lenjtet,icmotype,ier)

C

C Create the new 2d cmo - call it cmoout.

C

call cmo\_exist(cmoout,ier)

C

C ier.eq.0 means that the cmo already exists - if so release it.

C

if(ier.eq.0) call cmo\_release(cmoout,idelete)

C

C Set active cmo to cmoout

call cmo\_set\_name(cmoout,ier)

C

C set scalar mesh variables

C

call cmo\_set\_intinfo('nnodes',cmoout,npoints,1,1,ier)

call cmo\_set\_intinfo('nelements',cmoout,ntets,1,1,ier)

C

C the following scalars need to be set for a 2d cmo

C

call cmo\_set\_info('ndimensions\_topo',cmoout,2,1,1,ier)

call cmo\_set\_info('ndimensions\_geom',cmoout,3,1,1,ier)

call cmo\_set\_info('nodes\_per\_element',cmoout,3,1,1,ier)

call cmo\_set\_info('faces\_per\_element',cmoout,3,1,1,ier)

C

C allocate memory for vector variables

call cmo\_newlen(cmoout,ier)

C

C now get the pointers to the allocated memory for the vector data

call cmo\_get\_info('imt1',cmoout,ipimt1a,lenimt1a,icmotype,ier)

call cmo\_get\_info('itp1',cmoout,ipitp1a,lenitp1a,icmotype,ier)

call cmo\_get\_info('icr1',cmoout,ipicr1a,lenicr1a,icmotype,ier)

call cmo\_get\_info('isn1',cmoout,ipisn1a,lenisn1a,icmotype,ier)

call cmo\_get\_info('xic',cmoout,ipxica,lenxica,icmotype,ier)

call cmo\_get\_info('yic',cmoout,ipyica,lenyica,icmotype,ier)

call cmo\_get\_info('zic',cmoout,ipzica,lenzica,icmotype,ier)

call cmo\_get\_info('itetclr',cmoout,ipitetclra,lenclra,icmotype,ier)

call cmo\_get\_info('itettyp',cmoout,ipitettypa,lentypa,icmotype,ier)

call cmo\_get\_info('itetoff',cmoout,ipitetoffa,lenoffa,icmotype,ier)

call cmo\_get\_info('jtetoff',cmoout,ipjtetoffa,lenoffa,icmotype,ier)

call cmo\_get\_info('itet',cmoout,ipiteta,leniteta,icmotype,ier)

call cmo\_get\_info('jtet',cmoout,ipjteta,lenjteta,icmotype,ier)

C

C now the values for the vector components of the 2d mesh

C object can be set.
