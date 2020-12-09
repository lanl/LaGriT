      subroutine addmesh_append(cmoc,cmob,imat_offset,ierror)
C
C ######################################################################
C
C     PURPOSE -
C
C        This routine appends two mesh objects.
C
C     INPUT ARGUMENTS -
C
C        cmoc - The master mesh_object (source1).
C        cmob - The slave mesh_object (source2).
C
C     OUTPUT ARGUMENTS -
C
C        cmoc   - The master mesh_object (sink).
C        ierror - Error flag.
C
C     CHANGE HISTORY -
C
C        $Log: addmesh_append.f,v $
C        Revision 2.00  2007/11/05 19:45:45  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.5   24 Jan 2007 09:34:24   gable
CPVCS    Made changes to handle cases where one MO is empty or has not elements.
CPVCS    
CPVCS       Rev 1.4   23 Jun 2006 07:58:20   tam
CPVCS    error checking and reporting has been improved
CPVCS    and is based on the sink cmo being the master
CPVCS    loop through sink attributes, and find matching source attribute
CPVCS    allows appending mixed VINT and VDOUBLE attribute types
CPVCS    does not try to append scalar attributes
CPVCS    
CPVCS    
CPVCS       Rev 1.3   25 Jul 2005 12:54:10   gable
CPVCS    Added error checking and output warnings.
CPVCS    
CPVCS       Rev 1.2   13 May 2004 11:18:30   gable
CPVCS    Added support of maintaining all attribute types (VINT, VDOUBLE, VCAR)
CPVCS    for both element based vectors and node based vectors.
CPVCS    
CPVCS       Rev 1.1   21 Apr 2000 07:01:32   gable
CPVCS    Made setting and getting of mbndry value dynamic and problem size dependent.
CPVCS    
CPVCS       Rev 1.0   26 Jan 2000 14:01:30   dcg
CPVCS    Initial revision.
CPVCS    
CPVCS       Rev 1.13   Wed Oct 08 12:44:34 1997   gable
CPVCS    Copy changes from x3d version of this code. Added
CPVCS    argument to allow the option of append vs. merge.
CPVCS    
CPVCS       Rev 1.12   Mon Sep 15 10:13:00 1997   het
CPVCS    Change the definition of _append and _merge
CPVCS    
CPVCS       Rev 1.11   Mon Dec 09 09:01:56 1996   het
CPVCS    Save the "ipointi" and "ipointj" counters.
CPVCS    
CPVCS       Rev 1.10   Thu Nov 21 19:09:02 1996   het
CPVCS    
CPVCS    
CPVCS       Rev 1.9   Mon Nov 11 20:59:28 1996   het
CPVCS    Make changes for adding hybrid grids.
CPVCS    
CPVCS       Rev 1.8   Fri Feb 02 14:20:40 1996   dcg
CPVCS    remove references to explicit vector attributes (u,w,v,e,r,pic)
CPVCS
CPVCS       Rev 1.7   09/21/95 09:20:44   dcg
CPVCS    clean up mesh object declarations
CPVCS
CPVCS       Rev 1.6   09/20/95 09:19:46   dcg
CPVCS    look for added attributes to mesh objects
CPVCS
CPVCS       Rev 1.5   08/29/95 12:17:08   het
CPVCS    Add the cmowrk storage block for each CMO
CPVCS
CPVCS       Rev 1.4   07/11/95 15:24:10   het
CPVCS    Correct an error with getting/using MBNDRY
CPVCS
CPVCS       Rev 1.3   05/30/95 07:52:36   het
CPVCS    Replace mesh_object subroutine parameters by cmo-calls
C
C ######################################################################
C
      implicit none
C
C ######################################################################
C
      include "local_element.h"
C
C ######################################################################
C
      character*(*) cmoc, cmob
      integer imat_offset, ierror
C
      character*132 logmess
C
C ######################################################################
C
C
      pointer (ipisetwd1, isetwd1(1000000))
      pointer (ipialias1, ialias1(1000000))
      pointer (ipimt1, imt1(1000000))
      pointer (ipicr1, icr1(1000000))
      pointer (ipisn1, isn1(1000000))
      pointer (ipitetclr1, itetclr1(10000000))
      pointer (ipitettyp1, itettyp1(10000000))
      pointer (ipitetoff1, itetoff1(10000000))
      pointer (ipjtetoff1, jtetoff1(10000000))
      pointer (ipitet1, itet1(10000000))
      pointer (ipjtet1, jtet1(10000000))
C
      pointer (ipisetwd2, isetwd2(1000000))
      pointer (ipialias2, ialias2(1000000))
      pointer (ipimt2, imt2(1000000))
      pointer (ipicr2, icr2(1000000))
      pointer (ipisn2, isn2(1000000))
C        pointer (ipxic2, xic2(1000000))
C        pointer (ipyic2, yic2(1000000))
C        pointer (ipzic2, zic2(1000000))
      pointer (ipitetclr2, itetclr2(10000000))
      pointer (ipitettyp2, itettyp2(10000000))
      pointer (ipitetoff2, itetoff2(10000000))
      pointer (ipjtetoff2, jtetoff2(10000000))
      pointer (ipitet2, itet2(10000000))
      pointer (ipjtet2, jtet2(10000000))
C
C
C ######################################################################
C
      pointer (ipitetpar1, itetpar1)
      pointer (ipitetkid1, itetkid1)
      pointer (ipitetlev1, itetlev1)
      integer itetpar1(1000000),
     *        itetkid1(1000000),
     *        itetlev1(1000000)
      pointer (ipitetpar2, itetpar2)
      pointer (ipitetkid2, itetkid2)
      pointer (ipitetlev2, itetlev2)
      integer itetpar2(1000000),
     *        itetkid2(1000000),
     *        itetlev2(1000000)
C
C
C ######################################################################
C
C
      integer  nmcmoatt_b, nmcmoatt_c
C
      pointer(ipicmo_c,icmo_c)
      pointer(ipicmo_c,xcmo_c)
      pointer(ipicmo_c,ccmo_c)
      pointer(ipicmo_b,icmo_b)
      pointer(ipicmo_b,xcmo_b)
      pointer(ipicmo_b,ccmo_b)
      integer icmo_c(100000),icmo_b(100000)
      real*8  xcmo_c(100000),xcmo_b(1000000)
      character*32 ccmo_c(*),ccmo_b(*)

      character*32 cname,ctype,clength,defname,cmoattnam,
     *  cio,cpers,cinter,crank

      character*32 cname_c,ctype_c,clength_c,defname_c,cmoattnam_c,
     *  cio_c,cpers_c,cinter_c,crank_c

      character*32 cname_b,ctype_b,clength_b,defname_b,cmoattnam_b,
     *  cio_b,cpers_b,cinter_b,crank_b, cname_last

      integer iaddonly,ipointi,ipointj,npoints1_save,icscode,
     * lenout,jf,jt,ielmnef,jtoff,jtoff1,ielmnen,itoff,it,length,
     * i2,ier1,icl,ict,iclc,ierror_return,index,i1,i,itype,icr1max1,
     * imt1max1,nef2,nen2,nsdgeom2,nsdtopo2,nsdtopo1,nsdgeom1,nen1,nef1,
     *  ier,mbndry,mbndry2,numtet2,npoints1,mbndry1,ierr,numtet1,
     *  ilen,itet1,jtet1,jtetoff2,itetoff2,itettyp2,itetclr2,isn2,
     *  jtetoff1,itet2,jtet2,isetwd2,ialias2,imt2,icr2,itetoff1,
     *  itettyp1,itetclr1,isn1,icr1,imt1,ialias1,isetwd1,icscode1,
     *  icscode2,icmotype,npoints2,icmotp 
      integer ierror_exist, icopy, nskip, nmixed

      integer iskip(1000)
      integer imixed(1000)
      integer icharlnf

      character*32 isubname
C
      data iaddonly / 0 /
C
      isubname="addmesh_append"
      defname='default'
      nskip=0
      nmixed=0
      icscode=0
C
      call cmo_get_info('nnodes',cmoc,npoints1,ilen,icmotp,ierror)
      call cmo_get_info('nelements',cmoc,numtet1,ilen,icmotp,ierr)
      call cmo_get_info('mbndry',cmoc,mbndry1,ilen,icmotp,ierr)
      
      call cmo_get_info('nnodes',cmob,npoints2,ilen,icmotp,ierror)
      call cmo_get_info('nelements',cmob,numtet2,ilen,icmotp,ierr)
      call cmo_get_info('mbndry',cmob,mbndry2,ilen,icmotp,ierr)
C
      call cmo_set_info('nnodes',cmoc,npoints1+npoints2,1,1,ier)
      call cmo_set_info('nelements',cmoc,numtet1+numtet2,1,1,ier)
C
      call cmo_newlen(cmoc,ierror)
C
C     We need to be careful because mbndry1 .ne. mbndry2 .ne. mbndry is possible
C
      call cmo_get_intinfo('mbndry',cmoc,mbndry,
     *                  length,icmotype,ierror)
      call cmo_get_info('ndimensions_topo',cmoc,
     *                  nsdtopo1,length,icmotype,ierror)
      call cmo_get_info('ndimensions_geom',cmoc,
     *                  nsdgeom1,length,icmotype,ierror)
      call cmo_get_info('nodes_per_element',cmoc,
     *                  nen1,length,icmotype,ierror)
      call cmo_get_info('faces_per_element',cmoc,
     *                  nef1,length,icmotype,ierror)
      call cmo_get_info('isetwd',cmoc,ipisetwd1,length,icmotp,ierror)
      call cmo_get_info('ialias',cmoc,ipialias1,length,icmotp,ierror)
      call cmo_get_info('imt1',cmoc,ipimt1,length,icmotp,ierror)
      call cmo_get_info('icr1',cmoc,ipicr1,length,icmotp,ierror)
      call cmo_get_info('isn1',cmoc,ipisn1,length,icmotp,ierror)
      call cmo_get_info('itetclr',cmoc,
     *                  ipitetclr1,length,icmotp,ier)
      call cmo_get_info('itettyp',cmoc,
     *                  ipitettyp1,length,icmotp,ier)
      call cmo_get_info('itetoff',cmoc,
     *                  ipitetoff1,length,icmotp,ier)
      call cmo_get_info('jtetoff',cmoc,
     *                  ipjtetoff1,length,icmotp,ier)
      call cmo_get_info('itet',cmoc,ipitet1,length,icmotp,ierror)
      call cmo_get_info('jtet',cmoc,ipjtet1,length,icmotp,ierror)
C
C
      call cmo_get_info('ndimensions_topo',cmob,
     *                  nsdtopo2,length,icmotype,ierror)
      call cmo_get_info('ndimensions_geom',cmob,
     *                  nsdgeom2,length,icmotype,ierror)
      call cmo_get_info('nodes_per_element',cmob,
     *                  nen2,length,icmotype,ierror)
      call cmo_get_info('faces_per_element',cmob,
     *                  nef2,length,icmotype,ierror)
C
      call cmo_get_info('isetwd',cmob,ipisetwd2,length,icmotp,ierror)
      call cmo_get_info('ialias',cmob,ipialias2,length,icmotp,ierror)
      call cmo_get_info('imt1',cmob,ipimt2,length,icmotp,ierror)
      call cmo_get_info('icr1',cmob,ipicr2,length,icmotp,ierror)
      call cmo_get_info('isn1',cmob,ipisn2,length,icmotp,ierror)
      call cmo_get_info('itetclr',cmob,
     *                  ipitetclr2,length,icmotp,ier)
      call cmo_get_info('itettyp',cmob,
     *                  ipitettyp2,length,icmotp,ier)
      call cmo_get_info('itetoff',cmob,
     *                  ipitetoff2,length,icmotp,ier)
      call cmo_get_info('jtetoff',cmob,
     *                  ipjtetoff2,length,icmotp,ier)
      call cmo_get_info('itet',cmob,ipitet2,length,icmotp,ierror)
      call cmo_get_info('jtet',cmob,ipjtet2,length,icmotp,ierror)
C
      imt1max1=0
      icr1max1=0
      if(imat_offset.eq.0) then
      else
        if(npoints1 .ge. 1)then
         do i=1,npoints1
            imt1max1=max(imt1max1,imt1(i))
            icr1max1=max(icr1max1,icr1(i))
         enddo
        endif
      endif
C  get number of  attributes for master mesh object and slave

      call cmo_get_info('number_of_attributes',cmob,nmcmoatt_b,
     *    ilen,itype,icscode)
      call cmo_get_info('number_of_attributes',cmoc,nmcmoatt_c,
     *    ilen,itype,icscode)
C
C     loop through attributes copy in matching field where it exists
C     Append cmob (source) attributes to cmoc (sink)
C     scalar attributes do not get appended, and has value of sink cmo
C     __________________________________________________________________
C
      do i1=1,nmcmoatt_c
      
C        get next attribute name from cmoc indexed by i1
C        find matching attribute in cmob so copy can be made 
C        if ierror is 0, then attribute is valid 

         call cmo_get_attribute_name(cmoc,i1,cname_c,icscode)
         call cmo_get_attparam(cname_c,cmoc,index,ctype_c,crank_c,
     *    clength_c,cinter_c,cpers_c,cio_c,ierror_return)
         call cmo_get_info(clength_c,cmoc,length,iclc,ict,ierror)

C        get cname, ctype, crank, clength from cmoc 
         call cmo_get_attribute_name(cmoc,i1,cname,icscode)
         call cmo_get_attparam(cname,cmoc,index,ctype,crank,
     *    clength,cinter,cpers,cio,ierror_return)
         call cmo_get_info(clength,cmoc,length,iclc,ict,ierror)

C        find matching attribute in cmob, assign length and iclc
C        ierror_exist will be 0 if it does exist for cmo_b
         cname_b = cname_c
         call cmo_get_attparam(cname_b,cmob,index,ctype_b,crank_b,
     *   clength_b,cinter_b,cpers_b,cio_b,ierror_exist)
         if (ierror_exist .eq. 0) then
            call cmo_get_info(clength_b,cmob,length,iclc,ict,ierror)
            call cmo_get_info(clength,cmoc,length,iclc,ict,ierror)
            call cmo_get_info(cname,cmoc,ipicmo_c,iclc,ict,ierror)
            call cmo_get_info(cname,cmob,ipicmo_b,icl ,ict,ier1)
         else
           nskip=nskip+1
           iskip(nskip) = i1
         endif

C        _______________________________________________________________
C        do some type checking before trying to copy attributes
C        cannot add node attribute to element attribute
C        can add real to integer, values are converted

C        _______________________________________________________________
C        types match so copy if length is correct and not scalar
         if (ierror_exist.eq.0) then 
 
         icopy=1
         if(ctype_b(1:4) .ne. ctype_c(1:4)) then
           if (ctype_c(1:4).eq.'VINT' .and. 
     *       ctype_b(1:7).eq.'VDOUBLE') then
             icopy=1
           else if (ctype_c(1:7).eq.'VDOUBLE' .and. 
     *       ctype_b(1:4).eq.'VINT') then
             icopy=1
           else
             nmixed=nmixed+1
             imixed(nmixed) = i1
             icopy=0
           endif
         endif 
         if (clength_b(1:6).ne.clength_c(1:6)) then
           nmixed=nmixed+1
           imixed(nmixed) = i1
           icopy=0
         endif
         if (clength_c(1:6).eq.'scalar') icopy=0


         if(length.eq.iclc .and. icopy.gt.0) then

C        copy node attributes
         if (clength(1:6).eq.'nnodes' .and. length.ge.npoints2) then

         if (ctype_c(1:4).eq.'VINT' .and.
     *       ctype_b(1:4).eq.'VINT') then
               if(npoints2 .ge. 1)then
                do i2=1,npoints2
                   icmo_c(npoints1+i2)=icmo_b(i2)
                enddo
               endif
         endif
         if (ctype_c(1:4).eq.'VINT' .and.
     *       ctype_b(1:7).eq.'VDOUBLE') then
               if(npoints2 .ge. 1)then
                do i2=1,npoints2
                   icmo_c(npoints1+i2)=xcmo_b(i2)
                enddo
               endif
         endif
         if (ctype_c(1:7).eq.'VDOUBLE' .and.
     *       ctype_b(1:7).eq.'VDOUBLE') then
               if(npoints2 .ge. 1)then
                do i2=1,npoints2
                   xcmo_c(npoints1+i2)=xcmo_b(i2)
                enddo
               endif
         endif
         if (ctype_c(1:7).eq.'VDOUBLE' .and.
     *       ctype_b(1:4).eq.'VINT') then
               if(npoints2 .ge. 1)then
                do i2=1,npoints2
                   xcmo_c(npoints1+i2)=icmo_b(i2)
                enddo 
               endif
         endif
         if (ctype_c(1:7).eq.'VCHAR') then
               if(npoints2 .ge. 1)then
                do i2=1,npoints2
                   ccmo_c(npoints1+i2)=ccmo_b(i2)
                enddo
               endif
         endif
         endif

C        copy element attributes
         if (clength(1:9).eq.'nelements' .and. length.ge.numtet2) then

         if (ctype_c(1:4).eq.'VINT' .and.
     *       ctype_b(1:4).eq.'VINT') then
              if(numtet2 .ge. 1)then
               do i2=1,numtet2
                  icmo_c(numtet1+i2)=icmo_b(i2)
               enddo
              endif
         endif
         if (ctype_c(1:4).eq.'VINT' .and.
     *       ctype_b(1:7).eq.'VDOUBLE') then
              if(numtet2 .ge. 1)then
               do i2=1,numtet2
                  icmo_c(numtet1+i2)=xcmo_b(i2)
               enddo
              endif
         endif
         if (ctype_c(1:7).eq.'VDOUBLE' .and.
     *       ctype_b(1:7).eq.'VDOUBLE') then
              if(numtet2 .ge. 1)then
               do i2=1,numtet2
                  xcmo_c(numtet1+i2)=xcmo_b(i2)
               enddo
              endif
         endif
         if (ctype_c(1:7).eq.'VDOUBLE' .and.
     *       ctype_b(1:4).eq.'VINT') then
              if(numtet2 .ge. 1)then
               do i2=1,numtet2
                  xcmo_c(numtet1+i2)=icmo_b(i2)
               enddo
              endif
         endif
         if (ctype(1:7).eq.'VCHAR') 
     *      then
              if(numtet2 .ge. 1)then
               do i2=1,numtet2
                  ccmo_c(numtet1+i2)=ccmo_b(i2)
               enddo
              endif
         endif
         endif

       endif
C      end copy or skip for the attribute 
C      _______________________________________________________________
       endif
      enddo
C
C     error reporting for attribute append
C
      if (nskip+nmixed .gt. 0) then

        write(logmess,'(a,a,a,a)') 
     * 'searching for ',cmoc(1:icharlnf(cmoc)),' attributes in ',
     *  cmob(1:icharlnf(cmob))
        call writloga('default',0,logmess,0,ierr)

        if (nmixed.gt.0) then
          do i=1,nmixed
            call cmo_get_attribute_name(cmoc,imixed(i),cname_c,icscode)
            call cmo_get_attparam(cname_c,cmoc,index,ctype_c,crank_c,
     *      clength_c,cinter_c,cpers_c,cio_c,ierror_return)
            call cmo_get_attparam(cname_c,cmob,index,ctype_b,crank_b,
     *      clength_b,cinter_b,cpers_b,cio_b,ierror_return)

            if (ctype_c .ne. ctype_b) then
              write(logmess,'(a,i7,2x,a,2x,a,2x,a)') 
     *        ' type mismatch: ',imixed(i),
     *        cname_c(1:icharlnf(cname_c)),
     *        ctype_c(1:icharlnf(ctype_c)),
     *        ctype_b(1:icharlnf(ctype_b)) 
              call writloga('default',0,logmess,0,ierr)
            endif
            if (clength_c .ne. clength_b) then
              write(logmess,'(a,i7,2x,a,2x,a,2x,a)')
     *        ' length mismatch: ',imixed(i),
     *        cname_c(1:icharlnf(cname_c)),
     *        clength_c(1:icharlnf(clength_c)),
     *        clength_b(1:icharlnf(clength_b))
              call writloga('default',0,logmess,0,ierr)
            endif

          enddo
        endif
        if (nskip.gt.0) then
          do i=1,nskip
            call cmo_get_attribute_name(cmoc,iskip(i),cname_c,icscode)
            write(logmess,'(a,i7,a2,a)') ' attribute not found:     ',
     *      iskip(i),'   ',cname_c(1:icharlnf(cname_c)) 
            call writloga('default',0,logmess,0,ierr)
          enddo
        endif

        write(logmess,'(a,i5,a,a,a,a)') 
     * 'ADDMESH: ',nskip+nmixed,' attributes not copied from ',
     *  cmob(1:icharlnf(cmob)),' to ',
     *  cmoc(1:icharlnf(cmoc))
        call writloga('default',0,logmess,1,ierr)
      endif
C
C
C     __________________________________________________________________
C     Update the connectivity
C
      if(npoints2 .ge. 1)then
      do i=1,npoints2
         isetwd1(i+npoints1)=isetwd2(i)
         ialias1(i+npoints1)=ialias2(i)+npoints1
         imt1(i+npoints1)=imt2(i)+imt1max1
         icr1(i+npoints1)=icr2(i)+icr1max1
         if(isn2(i).ne.0) then
            isn1(i+npoints1)=isn2(i)+npoints1
         else
            isn1(i+npoints1)=isn2(i)
         endif
      enddo
      endif
C
C***********
C
      if(numtet2 .ge. 1)then
      do it=1,numtet2
         itetclr1(numtet1+it)=itetclr2(it)+imt1max1
         itettyp1(numtet1+it)=itettyp2(it)
      enddo
      itoff=itetoff1(numtet1)+nelmnen(itettyp1(numtet1))
      do it=1,numtet2
         itetoff1(numtet1+it)=itoff
         ielmnen=nelmnen(itettyp2(it))
         do i=1,ielmnen
            itet1(itoff+i)=itet2(itetoff2(it)+i)+npoints1
         enddo
         itoff=itoff+ielmnen
      enddo
      jtoff1=jtetoff1(numtet1)+nelmnef(itettyp1(numtet1))
      jtoff=jtoff1
      do it=1,numtet2
         jtetoff1(numtet1+it)=jtoff
         ielmnef=nelmnef(itettyp2(it))
         do i=1,ielmnef
            if(jtet2(jtetoff2(it)+i).le.0 .or.
     *         jtet2(jtetoff2(it)+i).eq.mbndry) then
               jtet1(jtoff+i)=jtet2(jtetoff2(it)+i)
            elseif(jtet2(jtetoff2(it)+i).gt.mbndry) then
               jt=1+(jtet2(jtetoff2(it)+i)-mbndry-1)/nef2
               jf=jtet2(jtetoff2(it)+i)-mbndry-nef2*(jt-1)
               jtet1(jtoff+i)=mbndry+nef1*(numtet1+jt-1)+jf
            else
               jt=1+(jtet2(jtetoff2(it)+i)-1)/nef2
               jf=jtet2(jtetoff2(it)+i)-nef2*(jt-1)
               jtet1(jtoff+i)=nef1*(numtet1+jt-1)+jf
            endif
         enddo
         jtoff=jtoff+ielmnef
      enddo
      cmoattnam='itetpar'
      call mmfindbk(cmoattnam,cmob,ipitetpar2,lenout,icscode2)
      call mmfindbk(cmoattnam,cmoc,ipitetpar1,lenout,icscode1)
      if(icscode2.eq.0.and.icscode1.eq.0) then
         do it=1,numtet2
            if(itetpar2(it).gt.0) then
               itetpar1(numtet1+it)=itetpar2(it)+numtet1
            else
               itetpar1(numtet1+it)=0
            endif
         enddo
      endif
      cmoattnam='itetkid'
      call mmfindbk(cmoattnam,cmob,ipitetkid2,lenout,icscode2)
      call mmfindbk(cmoattnam,cmoc,ipitetkid1,lenout,icscode1)
      if(icscode2.eq.0.and.icscode1.eq.0) then
         do it=1,numtet2
            if(itetkid2(it).gt.0) then
               itetkid1(numtet1+it)=itetkid2(it)+numtet1
            else
               itetkid1(numtet1+it)=0
            endif
         enddo
      endif
      cmoattnam='itetlev'
      call mmfindbk(cmoattnam,cmob,ipitetlev2,lenout,icscode2)
      call mmfindbk(cmoattnam,cmoc,ipitetlev1,lenout,icscode1)
      if(icscode2.eq.0.and.icscode1.eq.0) then
         do it=1,numtet2
            itetlev1(numtet1+it)=itetlev2(it)
         enddo
      endif
      endif
C
C***********
C
      npoints1_save=npoints1
      npoints1=npoints1+npoints2
      numtet1=numtet1+numtet2
      call cmo_set_info('nnodes',cmoc,npoints1,1,1,ier)
      call cmo_set_info('nelements',cmoc,numtet1,1,1,ier)
C
      ipointi = npoints1_save+1
      ipointj = npoints1
      call cmo_set_info('ipointi',cmoc,ipointi,
     &                ilen,itype,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'get_info_i')
      call cmo_set_info('ipointj',cmoc,ipointj,
     &                ilen,itype,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'get_info_i')
C
      goto 9999
 9999 continue
      return
      end
