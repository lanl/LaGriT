*dk,filholes
      subroutine filholes(ipiholes,nholes,ipdum,
     *                    npoints,ntets)
C
       implicit none
C
       character*132 logmess
C
C ######################################################################
C
C     PURPOSE -
C
C        Move tets from the bottom of the 'itet' array into vacnt spots.
C        In the physics code only, corresponding kfix information is
C        also moved to help in forming the reconnection list.
C
C     INPUT -
C
C        ipiholes - pointer to the array of empty positions in 'itet'
C        nholes   - the number of holes
C        ipdum    - pointer to a dummy array that must be at least as
C                   long as the 'iholes' array
C
C     OUTPUT -
C
C        None
C
C     CHANGE HISTORY -
C
C        $Log:   /pvcs.config/t3d/src/filholes.f_a  $
CPVCS    
CPVCS       Rev 1.11   17 Jun 2003 16:12:50   dcg
CPVCS    skip holes whose number is greater than the current nelements value
CPVCS    
CPVCS       Rev 1.8   Wed Nov 10 09:19:00 1999   dcg
CPVCS    remove references to icdname
CPVCS
CPVCS       Rev 1.7   Fri Jan 22 11:54:24 1999   dcg
CPVCS    add include file cnames
CPVCS
CPVCS       Rev 1.6   Mon Apr 14 16:47:50 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.5   09/06/95 09:59:50   dcg
CPVCS    reset nelements before call to tettestd
CPVCS
CPVCS       Rev 1.4   08/30/95 21:09:24   het
CPVCS    Put cmo table data into the cmoatt storage block
CPVCS
CPVCS       Rev 1.3   12/02/94 15:04:52   het
CPVCS    Added an option for the "cmo" access functions
CPVCS
CPVCS
CPVCS       Rev 1.2   12/01/94 18:46:28   het
CPVCS    Added a data type to the "cmo" calles
CPVCS       and added the "cmo.h" include file.
CPVCS
CPVCS       Rev 1.1   11/17/94 21:50:36   het
CPVCS    Added include files for chydro, neibor, cmerge, comdict. Added
CPVCS    pointer statements for current_mesh_object database access.
CPVCS
CPVCS       Rev 1.0   11/10/94 12:11:50   pvcs
CPVCS    Original version.
C
C ######################################################################
C
      include "cmo.h"
      include "chydro.h"
      include "neibor.h"
C
C ######################################################################
C
      integer ierrfls,it2,it2sum,ierrdum,it1sum,it1,jtemp,itetnum,
     *  ier,leni,i1,i2,i3,i4,ierror,length,icmotype,i,j,k,npoints,ntets,
     *  nholes,iholes,dum,iatt,natt,nlen,lout,itout,it,itold,index
      integer icharln
      real*8 crosx1,crosy1,crosz1
      real*8 volume,volit1,volit2
      pointer( ipiholes , iholes(1) )
      pointer( ipdum    , dum(1)    )
      character*4 if1,if2,if3,if4
      character*32 isubname,ctype,crank,cattr_name,clength,cio,cpers,
     *  cinter
      pointer (ipitalias, italias)
      integer italias(*)
C
      pointer (ipxcmo,xcmo),(ipxcmo,icmo),(ipxcmo,ccmo)
      integer icmo(*)
      real*8 xcmo(*)
      character*32 ccmo(*)

C ######################################################################
C
C     DEFINE THE STATEMENT FUNCTIONS NEEDED TO CALCULATE TET VOLUMES.
C
      crosx1(i,j,k)=(yic(j)-yic(i))*(zic(k)-zic(i))-
     *              (yic(k)-yic(i))*(zic(j)-zic(i))
      crosy1(i,j,k)=(xic(k)-xic(i))*(zic(j)-zic(i))-
     *              (xic(j)-xic(i))*(zic(k)-zic(i))
      crosz1(i,j,k)=(xic(j)-xic(i))*(yic(k)-yic(i))-
     *              (xic(k)-xic(i))*(yic(j)-yic(i))
      volume(i1,i2,i3,i4)=(xic(i4)-xic(i1))*crosx1(i1,i2,i3)+
     *                    (yic(i4)-yic(i1))*crosy1(i1,i2,i3)+
     *                    (zic(i4)-zic(i1))*crosz1(i1,i2,i3)
C
C ######################################################################
C
C
      isubname='filholes'
C
C     ******************************************************************
C     FETCH MESH OBJECT NAME AND POINTER INFORMATION.
C
      if(icmoget.eq.1) then
C
      call cmo_get_name(cmo,ierror)
C
      call cmo_get_info('mbndry',cmo,mbndry,length,icmotype,ierror)
      call cmo_get_info('itp1',cmo,ipitp1,leni,icmotype,ierror)
      call cmo_get_info('icr1',cmo,ipicr1,leni,icmotype,ierror)
      call cmo_get_info('xic',cmo,ipxic,leni,icmotype,ierror)
      call cmo_get_info('yic',cmo,ipyic,leni,icmotype,ierror)
      call cmo_get_info('zic',cmo,ipzic,leni,icmotype,ierror)
      call cmo_get_info('itet',cmo,ipitet,leni,icmotype,ierror)
      call cmo_get_info('jtet',cmo,ipjtet,leni,icmotype,ierror)
      endif
C
C
      call isort(iholes,dum,nholes,2)
      call mmgetblk('italias',isubname,ipitalias,ntets,1,ierror)
      do 10 i=1,nholes
         itetnum=iholes(i)
         do while (itet(1,ntets).le.0) 
            ntets=ntets-1
         enddo
c  if hole number is greater than ntets skip this hole
         if(itetnum.ge.ntets) go to 10
         italias(itetnum)=ntets
         itet(1,itetnum)=itet(1,ntets)
         itet(2,itetnum)=itet(2,ntets)
         itet(3,itetnum)=itet(3,ntets)
         itet(4,itetnum)=itet(4,ntets)
         jtet(1,itetnum)=jtet(1,ntets)
         jtet(2,itetnum)=jtet(2,ntets)
         jtet(3,itetnum)=jtet(3,ntets)
         jtet(4,itetnum)=jtet(4,ntets)
C
C        ...............................................................
C        IN THE PHYSICS CODE, ALSO MOVE KFIX INFORMATION FOR USE IN
C        CONSTRUCTING THE RECONNECTION LIST IN RECON2.
C
C
         ntets=ntets-1
         if(jtet(1,itetnum).lt.mbndry) then
            jtet1(jtet(1,itetnum))=4*(itetnum-1)+1
         elseif(jtet(1,itetnum).gt.mbndry) then
            jtemp=jtet(1,itetnum)-mbndry
            jtet1(jtemp)=4*(itetnum-1)+1+mbndry
         endif
         if(jtet(2,itetnum).lt.mbndry) then
            jtet1(jtet(2,itetnum))=4*(itetnum-1)+2
         elseif(jtet(2,itetnum).gt.mbndry) then
            jtemp=jtet(2,itetnum)-mbndry
            jtet1(jtemp)=4*(itetnum-1)+2+mbndry
         endif
         if(jtet(3,itetnum).lt.mbndry) then
            jtet1(jtet(3,itetnum))=4*(itetnum-1)+3
         elseif(jtet(3,itetnum).gt.mbndry) then
            jtemp=jtet(3,itetnum)-mbndry
            jtet1(jtemp)=4*(itetnum-1)+3+mbndry
         endif
         if(jtet(4,itetnum).lt.mbndry) then
            jtet1(jtet(4,itetnum))=4*(itetnum-1)+4
         elseif(jtet(4,itetnum).gt.mbndry) then
            jtemp=jtet(4,itetnum)-mbndry
            jtet1(jtemp)=4*(itetnum-1)+4+mbndry
         endif
         if(idebug.gt.1) then
            it1=ntets+1
            i1=itet(1,it1)
            i2=itet(2,it1)
            i3=itet(3,it1)
            i4=itet(4,it1)
            it1sum=i1+i2+i3+i4
            volit1=volume(i1,i2,i3,i4)
            if1=' '
            if(jtet(1,it1).ge.mbndry) if1='*'
            if2=' '
            if(jtet(2,it1).ge.mbndry) if2='*'
            if3=' '
            if(jtet(3,it1).ge.mbndry) if3='*'
            if4=' '
            if(jtet(4,it1).ge.mbndry) if4='*'
            write(logmess,9000) 'filholes',it1,'old=',
     *                         i1,if1,i2,if2,i3,if3,i4,if4,
     *                         it1sum,volit1
            call writloga('bat',0,logmess,0,ierrdum)
            it2=itetnum
            i1=itet(1,it2)
            i2=itet(2,it2)
            i3=itet(3,it2)
            i4=itet(4,it2)
            it2sum=i1+i2+i3+i4
            volit2=volume(i1,i2,i3,i4)
            if1=' '
            if(jtet(1,it2).ge.mbndry) if1='*'
            if2=' '
            if(jtet(2,it2).ge.mbndry) if2='*'
            if3=' '
            if(jtet(3,it2).ge.mbndry) if3='*'
            if4=' '
            if(jtet(4,it2).ge.mbndry) if4='*'
            write(logmess,9010) '        ',it2,'new=',
     *                         i1,if1,i2,if2,i3,if3,i4,if4,
     *                         it2sum,volit2
            call writloga('bat',0,logmess,0,ierrdum)
 9000       format(a8,' it2=',i8,1x,a4,4(i7,a1),i8,1x,1pe10.3)
 9010       format(a8,' it2=',i8,1x,a4,4(i7,a1),i8,1x,1pe10.3)
            call writfls('bat',ierrfls)
         endif
 10   continue
C
C
C        ***************************************************************
C        REMAP ANY ELEMENT QUANTITIES TO THE NEW ELEMENT ORDERING.
C
C
C        LOOP THROUGH MESH OBJECT ATTRIBUTES AND LOOK FOR ELEMENT
C           BASED SCALAR ATTRIBUTES. Note:  itetoff,
C           and jtetoff are special and taken care of in the loops
C           above.
C
      call cmo_get_info('number_of_attributes',cmo,natt,
     *                   leni,icmotype,ierror)
      do iatt=1,natt
         call cmo_get_attribute_name(cmo,iatt,cattr_name,ier)
         nlen=icharln(cattr_name)
         if (cattr_name(1:nlen).eq. 'itetoff' .or.
     *       cattr_name(1:nlen).eq. 'jtetoff' ) then
         else
            call cmo_get_attparam(cattr_name,cmo,index,ctype,crank,
     *         clength,cinter,cpers,cio,ierror)
            if(ierror.eq.0 .and.
     *            clength(1:9).eq.'nelements' .and.
     *            ctype(1:7).eq.'VDOUBLE' .and.
     *            crank(1:6).eq.'scalar') then
               call cmo_get_info(cattr_name,cmo,
     *                              ipxcmo,lout,itout,ier)
               do it=1,nholes
                  itold=italias(iholes(it))
                  xcmo(iholes(it))=xcmo(itold)
               enddo
            elseif(ierror.eq.0 .and.
     *            clength(1:9).eq.'nelements' .and.
     *            ctype(1:4).eq.'VINT' .and.
     *            crank(1:6).eq.'scalar') then
               call cmo_get_info(cattr_name,cmo,
     *                              ipxcmo,lout,itout,ier)
               do it=1,nholes
                  itold=italias(iholes(it))
                  icmo(iholes(it))=icmo(itold)
               enddo
            elseif(ierror.eq.0 .and.
     *            clength(1:9).eq.'nelements' .and.
     *            ctype(1:4).eq.'VCHAR' .and.
     *            crank(1:6).eq.'scalar') then
               call cmo_get_info(cattr_name,cmo,
     *                              ipxcmo,lout,itout,ier)
               do it=1,nholes
                  itold=italias(iholes(it))
                  ccmo(iholes(it))=ccmo(itold)
               enddo
            endif
         endif
      enddo
C
C
C  reset number of elements
C
      if(icmoset.eq.1) then
         call cmo_set_info('nelements',cmo,ntets,1,1,ierror)
      endif
C
C     ..................................................................
C     CHECK THE JTET ARRAY FOR AN INCONSISTENCY.
      if(idebug.gt.1) then
         call tettestd
      endif
C
      call mmrelprt(isubname,ierror)
      return
      end
