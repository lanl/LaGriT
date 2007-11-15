      subroutine eset(imsgin,xmsgin,cmsgin,msgtype,nwds,ierror)
      implicit none
C
      integer nplen,ntlen
      parameter (nplen=1000000,ntlen=1000000)
C
      character*132 logmess
C
      pointer (ipisetwd, isetwd)
      pointer (ipimt1, imt1)
      pointer (ipitp1, itp1)
      pointer (ipicr1, icr1)
      pointer (ipisn1, isn1)
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      pointer(ipattribute,attribute)
      pointer(ipattribute,iattribute)
      pointer (ipxtetwd, xtetwd)
      integer isetwd(nplen)
      integer imt1(nplen), itp1(nplen),
     *        icr1(nplen), isn1(nplen)
      real*8 xic(nplen), yic(nplen), zic(nplen)
      integer xtetwd(nplen)
      pointer(ipxcntr1,xcntr1)
      pointer(ipycntr1,ycntr1)
      pointer(ipzcntr1,zcntr1)
      real*8 attribute(ntlen),xcntr1(ntlen),ycntr1(ntlen),zcntr1(ntlen)
      integer iattribute(ntlen)
      pointer (ipitetclr, itetclr(ntlen))
      pointer (ipitettyp, itettyp(ntlen))
      pointer (ipitetoff, itetoff(ntlen))
      pointer (ipitet, itet(ntlen))
      integer itetclr,itettyp,itetoff,itet
C
C ######################################################################
C
C     PURPOSE -
C
C        This routine builds element sets in a specified manner.
C
C     FORMAT: ELTSET/name/attribute_name/operation/value
C                      /UNION/set1/set2/...
C                      /INTER/
C                      /LIST /
C                      /DELETE/
C                      /INCLUSIVE|EXCLUSIVE|FACE/pset_name
C                      /VOLUME/operation/value
C                      /ASPECT/operation/value
C            attribute_name is any element attribute
C            operation is ge, le, gt, lt, eq, neq
C            value is value to test against attribute_name values
C               e.g itetclr/eq/4
C                   itetclr/eq/mreg1  value will be retrieved for the
C                                     material region mreg1
C
C            use assign/// idebug=1 to print element sets
C
C     INPUT ARGUMENTS -
C
C        imsgin()  - Integer array of command input tokens
C        xmsgin()  - Real array of command input tokens
C        cmsgin()  - Character array of command input tokens
C        msgtype() - Integer array of command input token types
C        nwds      - Number of command input tokens
C
C     OUTPUT ARGUMENTS -
C
C        ierror - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C     CHANGE HISTORY -
C
C$Log: eset.f,v $
CRevision 2.00  2007/11/05 19:45:54  spchu
CImport to CVS
C
CPVCS    
CPVCS       Rev 1.17   01 Oct 2007 08:18:44   gable
CPVCS    Modified to give warning and continue instead of crash when the MO
CPVCS    does not exist or is empty.
CPVCS    
CPVCS       Rev 1.16   31 Aug 2006 13:50:04   gable
CPVCS    Add FACE option.
CPVCS    
CPVCS       Rev 1.15   08 Feb 2006 14:35:30   dcg
CPVCS    "enforce lower case - add external statements for shift routines
CPVCS    these changes needed to compile with absoft pro fortran"
CPVCS    
CPVCS       Rev 1.14   06 Oct 2004 15:37:46   gable
CPVCS    Added error checking. A warning will be issued if
CPVCS    token element_attribute_name has length different
CPVCS    than number of elements. The warning will be issued
CPVCS    but the eltset action will continue.
CPVCS    
CPVCS       Rev 1.13   04 Oct 2004 17:21:22   dcg
CPVCS    use ior and iand in place of .and. and .or. for integer variables
CPVCS    
CPVCS       Rev 1.12   05 Feb 2003 09:47:18   gable
CPVCS    Bug in exclusive/pset get p_remove . Bug resulted in
CPVCS    incorrect result except in the case of tet's or quads.
CPVCS    Fixed for all element types now.
CPVCS    
CPVCS       Rev 1.11   06 Dec 2001 16:06:26   gable
CPVCS    Fixed multiple calls to eltset so that it behaves like pset.
CPVCS    Made some changes to format statements so that entire
CPVCS    eltset name is printed. Old version truncated names a 8 char.
CPVCS    
CPVCS       Rev 1.10   27 Jun 2001 07:18:12   gable
CPVCS     Changed list output to 8 entries per line from 10 per line.
CPVCS    
CPVCS       Rev 1.9   03 Oct 2000 09:47:20   dcg
CPVCS    remove unused references to ialias
CPVCS    
CPVCS       Rev 1.8   03 Sep 2000 11:46:46   gable
CPVCS      Changed format 6011 so element numbers gt 999999
CPVCS      do not run together.
CPVCS    
CPVCS       Rev 1.7   28 Mar 2000 14:09:14   dcg
CPVCS    remove include 'machine.h'
CPVCS    
CPVCS       Rev 1.6   Tue Mar 07 10:28:16 2000   dcg
CPVCS    change call to tstreg from getreg for region option
CPVCS    this allows an element to be in more than one region
CPVCS    
CPVCS       Rev 1.5   Fri Feb 04 16:35:34 2000   dcg
CPVCS    
CPVCS       Rev 1.4   Thu Feb 03 08:40:16 2000   dcg
CPVCS    
CPVCS       Rev 1.3   Mon Jan 31 17:30:06 2000   dcg
CPVCS    
CPVCS       Rev 1.2   28 Jan 2000 12:07:12   dcg
CPVCS    
CPVCS       Rev 1.1   13 Jan 2000 14:47:52   dcg
CPVCS    
CPVCS       Rev 1.0   05 Jan 2000 17:32:32   dcg
CPVCS     
CPVCS       Rev 1.15   Mon Dec 13 14:22:36 1999   dcg
CPVCS    change format to allow for more nodes
CPVCS
CPVCS       Rev 1.14   Wed Nov 10 09:35:28 1999   dcg
CPVCS    remove test on miscellaneous storage block
CPVCS
CPVCS       Rev 1.13   Mon Mar 15 20:17:22 1999   tam
CPVCS    don't print eset members unless idebug >=1
CPVCS
CPVCS       Rev 1.12   Fri Aug 28 14:24:46 1998   dcg
CPVCS    remove single precision constants
CPVCS
CPVCS       Rev 1.11   Mon Mar 16 16:40:52 1998   dcg
CPVCS    test lengths of names when comparing
CPVCS
CPVCS       Rev 1.10   Thu Feb 26 12:13:32 1998   dcg
CPVCS    remove
CPVCS    calls to 'loc'
CPVCS    change some printing formats to use eltset
CPVCS
CPVCS       Rev 1.9   Mon Jan 12 14:09:42 1998   dcg
CPVCS    more fixes for inclusive, exclusive options
CPVCS
CPVCS       Rev 1.8   Fri Jan 09 16:40:44 1998   dcg
CPVCS    fix region, mregion options
CPVCS
CPVCS       Rev 1.7   Fri Jan 09 09:23:12 1998   dcg
CPVCS    correct length for temporary storage
CPVCS
CPVCS       Rev 1.6   Mon Nov 24 16:32:30 1997   dcg
CPVCS    use geom.h and calls to get_regions, get_mregions, get_surfaces
CPVCS    to access geometry data - start to isolate integer*8 dependencies
CPVCS
CPVCS       Rev 1.5   Fri Oct 17 09:57:42 1997   dcg
CPVCS    fix length on memory get for itmpwd (should be
CPVCS    number of element not number of nodes)
CPVCS
CPVCS       Rev 1.4   Tue Sep 09 16:31:34 1997   dcg
CPVCS    add volume and aspect options
CPVCS    change to implicit none
CPVCS
CPVCS       Rev 1.3   Thu Jul 31 16:43:48 1997   dcg
CPVCS    fix typo 'ge' should have been 'gt'
CPVCS
CPVCS       Rev 1.2   Mon Apr 14 16:44:48 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.1   Mon Mar 04 11:11:02 1996   dcg
CPVCS    remove icn1, int1 unused in this routine
CPVCS
CPVCS       Rev 1.0   Tue Jan 30 14:24:36 1996   dcg
CPVCS    Initial revision.
C
C ######################################################################
C
      include 'local_element.h'
      include 'geom_lg.h'
      include 'cmo_lg.h'
C
C ######################################################################
C
      integer nwds, imsgin(nwds), msgtype(nwds)
      REAL*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
C
C
      character*32 isubname,name,mode,iregnam,geom_name
C
      integer nbitsmax
      parameter (nbitsmax=32)
 
      character*32  names(nbitsmax)
C
      character*32 cpt1, cpt2, cpt3,operation
      character*32  cpset
      character*32 cmo
C
      integer itest(nbitsmax),ibpos(nbitsmax)
 
      integer emask
C
      external shiftr,shiftl,compl
      integer shiftr,shiftl,compl
      pointer( ipmpary1 ,  mpary1(1000000) )
      pointer( ipmpary1 , xmpary1(1000000) )
      pointer( ipmpary4 ,  mpary4(1000000) )
      pointer( ipmpary4 , xmpary4(1000000) )
      pointer(iptmpwd, itmpwd(1000000))
      integer mpary1,mpary4,itmpwd
      real*8 xmpary1,xmpary4
      pointer (ipeltsetnames,eltsetnames)
      character*32 eltsetnames(*)
C
      pointer(ipiregno,iregno)
      pointer(ipisurfno,isurfno)
      integer iregno(ntlen),isurfno(ntlen)
C
       real*8 xv(10),yv(10),zv(10),volelm,eps,srchval,value,
     * xfac,xxlarge,xxsmall,xsmall,
     * xcntr,ycntr,zcntr
      integer ityp,ierror,npoints,length,icmotype,ipointi,ipointj,
     * icscode,i,j,it,ie,icnt,isetchg,len1,len2,ibitpos,ir,len3,
     * iprint,lenname,index,
     * ierr,ndup,nsetcnt,mask,imask,nmask,ict,
     * iunion,ipt1,iregck,npts,mask2,nmask2,
     * ilen,ivalue,lenop,number_of_eltsets
      integer nwd1,nwd2,ipos1,ipos2,iintrfce,nsets,iout,lout,itype,
     * lenmode,mpno,ierrw,nen,nef,nnf,inode,i1,
     * idebug, n_entry_per_line,
     * ntets
      pointer(ipout,out)
      real*8 out(*),rout
      integer icharlnf
      real*8 alargenumber
      parameter (alargenumber=1.d+20)
C
C
C ######################################################################
C
C
      imask(i)=2**(i-1)
 
C
C ######################################################################
C
C
 
      isubname='esetnames'
 
C
      ierror = 0
C
C     ******************************************************************
C
C  get mesh object
C
      call cmo_get_name(cmo,ierror)
      call cmo_get_attinfo('geom_name',cmo,iout,rout,geom_name,
     *                        ipout,lout,itype,ierror)
      call cmo_get_info('idebug',cmo,idebug,length,icmotype,ierror)
      call cmo_get_info('nnodes',cmo,
     *                  npoints,length,icmotype,ierror)
      call cmo_get_info('nelements',cmo,
     *                  ntets,length,icmotype,ierror)
      call cmo_get_info('isetwd',cmo,
     *                  ipisetwd,length,icmotype,ierror)
      call cmo_get_info('imt1',cmo,ipimt1,length,icmotype,ierror)
      call cmo_get_info('itp1',cmo,ipitp1,length,icmotype,ierror)
      call cmo_get_info('icr1',cmo,ipicr1,length,icmotype,ierror)
      call cmo_get_info('isn1',cmo,ipisn1,length,icmotype,ierror)
      call cmo_get_info('xic',cmo,ipxic,length,icmotype,ierror)
      call cmo_get_info('yic',cmo,ipyic,length,icmotype,ierror)
      call cmo_get_info('zic',cmo,ipzic,length,icmotype,ierror)
      call cmo_get_info('xtetwd',cmo,ipxtetwd,length,icmotype,ierror)
      call cmo_get_info('itetclr',cmo,
     *                  ipitetclr,length,icmotype,ierror)
      call cmo_get_info('itettyp',cmo,
     *                  ipitettyp,length,icmotype,ierror)
      call cmo_get_info('itetoff',cmo,
     *                  ipitetoff,length,icmotype,ierror)
      call cmo_get_info('itet',cmo,ipitet,length,icmotype,ierror)
      call cmo_get_info('ipointi',cmo,ipointi,length,icmotype,icscode)
      call cmo_get_info('ipointj',cmo,ipointj,length,icmotype,icscode)
      call cmo_get_info('number_of_eltsets',cmo,number_of_eltsets,
     *    length,icmotype,icscode)
C
C
      iprint=0
C
C     ALLOCATE SOME LOCAL MEMORY FOR THIS ROUTINE.
C
      length=max(npoints,ntets)
C
      if(length .le. 0)then
         write(logmess,'(a)')'WARNING: NO NODES, NO ELEMENTS'
         call writloga('default',0,logmess,0,ierr)
         write(logmess,'(a)')'WARNING: NO ACTION'
         call writloga('default',0,logmess,0,ierr)
         write(logmess,'(a)')'RETURN'
         call writloga('default',0,logmess,0,ierr)
         goto 9998
      endif
C      
      call mmgetblk('xmpary1',isubname,ipmpary1,length,2,icscode)
      call mmgetblk('xmpary4',isubname,ipmpary4,length,2,icscode)
      length=ntets
      call mmgetblk('itmpwd',isubname,iptmpwd,length,2,icscode)
C
C     ******************************************************************
C
C     SET CONSTANTS, NAME AND MODE
C
      xxlarge=alargenumber
      xxsmall=1.0e-20
      xsmall=1.0e-20
C
      name=cmsgin(2)
      lenname=icharlnf(name)
      mode=cmsgin(3)
      lenmode=icharlnf(mode)
      if ((name(1:lenname).eq.'0' .or. name(1:lenname).eq. ' ') .and.
     &    (mode(1:lenmode).eq.'0' .or. mode(1:lenmode).eq. ' ')) then
         mode='list'
      endif
C
C     ******************************************************************
C
      isetchg=0
      index=0
      call mmfindbk('eltsetnames',cmo,ipeltsetnames,ilen,icscode)
      do i=1,nbitsmax
        if(eltsetnames(i).eq.name) then
          index=i
          go to 12
        endif
      enddo
 
C     ******************************************************************
C
C     PROCESS THE 'LIST' MODE
C
 12   if (mode(1:lenmode) .eq. 'list') then
C
C        ---------------------------------------------------------------
C        IF NO NAME ENTERED, DISPLAY LIST OF PSET NAMES
C
         if ((msgtype(2).eq.1.and.imsgin(2).eq.0) .or.
     *       (msgtype(2).eq.3.and.cmsgin(2).eq.' ').or.
     *       (msgtype(2).eq.3.and.cmsgin(2)(1:5).eq.'-def-')) then
            ierror=0
            icnt=0
            do 1 i=1,nbitsmax
               if (eltsetnames(i) .ne.' '.and.
     *             eltsetnames(i)(1:5).ne.'-def-')
     *                 icnt=i
    1       continue
            write(logmess,6000) icnt
 6000       format(' THERE ARE ',i2,' ELTSETS DEFINED')
            call writloga('default',1,logmess,0,ierr)
            if (icnt .eq. 0) go to 9998
            do 2 i=1,icnt,2
               write(logmess,6001) (eltsetnames(j),j=i,min0(i+1,icnt))
 6001          format(2(2x,a32))
               call writloga('default',0,logmess,0,ierr)
    2       continue
            go to 9998
C
C        ---------------------------------------------------------------
C        IF NAME ENTERED, CHECK THAT IT EXISTS
C
         else
            icnt=0
            do 5 i=1,nbitsmax
               len2=icharlnf(eltsetnames(i))
               len1=max(lenname,len2)
               if (name(1:len1) .eq. eltsetnames(i)(1:len1)) icnt=i
    5       continue
            if (icnt .eq. 0) then
               write(logmess,1005) name
               call writloga('default',1,logmess,1,ierr)
               go to 9998
            else
               iprint=1
               ierror=0
               go to 9997
            endif
         endif
      endif
C
C     ******************************************************************
C
C     CHECK TO SEE IF THE NAME HAS ALREADY BEEN USED AND SET THE
C     BIT POSITION.
C
      if(msgtype(2).ne.3.or.msgtype(3).ne.3) goto 9998
      do i=1,nbitsmax
         len2=icharlnf(eltsetnames(i))
         len1=max(lenname,len2)
         itest(i)=0
         if(name(1:len1).eq.eltsetnames(i)(1:len1)) itest(i)=i
      enddo
      call kmprsn(nbitsmax,itest,1,itest,1,itest,1,ndup)
      if(ndup.eq.0) then
         do 15 i=1,nbitsmax
            if(eltsetnames(i).eq.' '.or.eltsetnames(i)(1:5).eq.'-def-')
     *       then
               nsetcnt=i
               goto 16
            endif
 15      continue
 16      continue
         ibitpos=nsetcnt
         if(nsetcnt.gt.nbitsmax) then
            write(logmess,1000) nbitsmax
            call writloga('default',1,logmess,1,ierr)
            goto 9998
         endif
         if(nsetcnt.le.0) nsetcnt=1
         isetchg=isetchg+1
         eltsetnames(nsetcnt)=name(1:lenname)
         number_of_eltsets=number_of_eltsets+1
      else
         ibitpos=itest(1)
      endif
      mask=imask(ibitpos)
      nmask=not(mask)
C
C     ******************************************************************
C
C     SAVE OLD DATA AND ZERO OUT THE POINT SET.
C
      do 30 i=1,ntets
         itmpwd(i)=iand(xtetwd(i),nmask)
 30   continue
C     PROCESS THE 'UNION' AND 'INTER' (OR INTERSECTION) MODE.
C
      if(mode(1:lenmode).eq.'union' .or.
     *   mode(1:lenmode).eq.'inter' .or.
     *   mode(1:lenmode).eq.'not') then
         nsets=nwds-3
         if(nsets.le.0.or.nsets.ge.nbitsmax) goto 9998
C
C        ...............................................................
C        READ IN THE NAMES OF THE SETS TO BE UNIONED AND CHECK TO
C        SEE IF ANY OF THESE MATCH THE NEW SET'S NAME.
C
         ict=0
         do 50 i=1,nsets
            ict=ict+1
            names(ict)=cmsgin(ict+3)
 50      continue
C
C        ...............................................................
C        FIND THE BIT POSITIONS OF THE SETS TO BE OPERATED ON.
C
         do 70 i=1,nsets
            do 60 j=1,nbitsmax
               len1=icharlnf(eltsetnames(j))
               len2=icharlnf(names(i))
               len1=max(len1,len2)
               itest(j)=0
               if(eltsetnames(j)(1:len1).eq.names(i)(1:len1)) itest(j)=j
 60         continue
            call kmprsn(nbitsmax,itest,1,itest,1,itest,1,ndup)
            if(ndup.eq.0) then
               write(logmess,1005) names(i)
               call writloga('default',1,logmess,1,ierr)
               goto 9998
            endif
            ibpos(i)=itest(1)
            ibpos(i+1)=itest(1)
 70      continue
C
C        ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
C        Perform the 'union' operation.
C
         if(mode(1:lenmode).eq.'union') then
            ipos1=ibpos(1)
            ipos2=ibpos(2)
            do 80 j=1,ntets
               nwd1=shiftr(iand(xtetwd(j),imask(ipos1)),ipos1-1)
               nwd2=shiftr(iand(xtetwd(j),imask(ipos2)),ipos2-1)
               iunion=shiftl(ior(nwd1,nwd2),ibitpos-1)
               itmpwd(j)=ior(iand(xtetwd(j),nmask),iunion)
 80         continue
            ipos1=ibitpos
            do 100 i=3,nsets
               ipos2=ibpos(i)
               do 90 j=1,ntets
                  nwd1=shiftr(iand(itmpwd(j),imask(ipos1)),ipos1-1)
                  nwd2=shiftr(iand(xtetwd(j),imask(ipos2)),ipos2-1)
                  iunion=shiftl(ior(nwd1,nwd2),ibitpos-1)
                  itmpwd(j)=ior(iand(itmpwd(j),nmask),iunion)
 90            continue
 100        continue
C
C        ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
C        Perform the 'intersection' operation.
C
         elseif(mode(1:lenmode).eq.'inter') then
            ipos1=ibpos(1)
            ipos2=ibpos(2)
            do 110 j=1,ntets
               nwd1=shiftr(iand(xtetwd(j),imask(ipos1)),ipos1-1)
               nwd2=shiftr(iand(xtetwd(j),imask(ipos2)),ipos2-1)
               iintrfce=shiftl(iand(nwd1,nwd2),ibitpos-1)
               itmpwd(j)=ior(iand(xtetwd(j),nmask),iintrfce)
 110        continue
            ipos1=ibitpos
            do 130 i=3,nsets
               ipos2=ibpos(i)
               do 120 j=1,ntets
                  nwd1=shiftr(iand(xtetwd(j),imask(ipos1)),ipos1-1)
                  nwd2=shiftr(iand(xtetwd(j),imask(ipos2)),ipos2-1)
  
                  iintrfce=shiftl(iand(nwd1,nwd2),ibitpos-1)
                  itmpwd(j)=ior(iand(itmpwd(j),nmask),iintrfce)
 120           continue
 130        continue
C
C        ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
C        Perform the 'not' operation.
C
         elseif(mode(1:lenmode).eq.'not') then
            if(nsets.eq.1) then
               ipos1=ibpos(1)
               do 145 j=1,ntets
                  nwd1=shiftr(iand(xtetwd(j),imask(ipos1)),ipos1-1)
                  iintrfce=shiftl(not(nwd1),ibitpos-1)
                  itmpwd(j)=ior(iand(xtetwd(j),nmask),iintrfce)
  145          continue
            elseif(nsets.gt.1) then
               ipos1=ibpos(1)
               ipos2=ibpos(2)
               do 140 j=1,ntets
                  nwd1=shiftr(iand(xtetwd(j),imask(ipos1)),ipos1-1)
                  nwd2=shiftr(iand(xtetwd(j),imask(ipos2)),ipos2-1)
                  iintrfce=shiftl(iand(nwd1,not(nwd2)),ibitpos-1)
                  itmpwd(j)=ior(iand(xtetwd(j),nmask),iintrfce)
 140           continue
               ipos1=ibitpos
               do 160 i=3,nsets
                  ipos2=ibpos(i)
                  do 150 j=1,ntets
                     nwd1=shiftr(iand(itmpwd(j),imask(ipos1)),ipos1-1)
                     nwd2=shiftr(iand(xtetwd(j),imask(ipos2)),ipos2-1)
                     iintrfce=iand(shiftl(nwd1,not(nwd2)),ibitpos-1)
                     itmpwd(j)=ior(iand(itmpwd(j),nmask),iintrfce)
 150              continue
 160           continue
            endif
         else
            goto 9998
         endif
         ierror=0
         goto 9996
      endif
C
C     __________________________________________________________________
C
C     PROCESS THE 'EXCLUSIVE/INCLUSIVE/FACE' MODE.
C
      if(mode(1:lenmode).eq.'inclusive'   .or.
     *   mode(1:lenmode).eq.'face'        .or.
     *   mode(1:lenmode).eq.'exclusive' ) then
         if(msgtype(6).eq.3) then
            cpset=cmsgin(6)
            call pntlimc('pset','get',cpset,ipmpary4,mpno,
     *                   npoints,isetwd,itp1)
      elseif (msgtype(6).eq.1) then
         call pntlimn(imsgin(4),imsgin(5),imsgin(6),ipmpary4,mpno,
     *                 npoints,isetwd,ipt1)
      else
         write(logmess,"('Error in eltset command - no point set')")
         call writloga('default',1,logmess,1,ierrw)
         go to 9998
      endif
         if (mpno .le. 0) goto 9998
         do i=1,npoints
           mpary1(i)=0
         enddo
         do i=1,mpno
           mpary1(mpary4(i))=1
         enddo
C loop through tets looking for nodes that belong to pset
      if(mode(1:lenmode).eq.'inclusive'    .or.
     *   mode(1:lenmode).eq.'exclusive' ) then
         do i=1,ntets
            nen=nelmnen(itettyp(i))
            emask=0
            do j=1,nen
               inode=itet(itetoff(i)+j)
               if(mpary1(inode).eq.1) then
                  if(mode(1:lenmode).eq.'inclusive')
     *                xtetwd(i)=ior(xtetwd(i),mask)
                  if(mode(1:lenmode).eq.'exclusive') then
                     if(j.eq.1) emask=1
                     if(j.gt.1.and.j.lt.nen) emask=emask+1
                     if(j.eq.nen.and.emask.eq.nen-1)
     *                     xtetwd(i)=ior(xtetwd(i),mask)
                  endif
               endif
            enddo
         enddo
      elseif(mode(1:lenmode).eq.'face') then
C
C     To loop through all the nodes, k, of all elements in the mesh by faces
C     looking for elements that have all nodes of a face members of the pset.
C
      do ie=1,ntets
        nef = nelmnef(itettyp(ie))
        do i=1,nef
          nnf = ielmface0(i,itettyp(ie))
          emask=0
          do j=1,nnf
            inode=itet(itetoff(ie) + ielmface1(j,i,itettyp(ie)))
            if(mpary1(inode).eq.1) then
                if(j.eq.1) emask=1
                if(j.gt.1.and.j.lt.nnf) emask=emask+1
                if(j.eq.nnf.and.emask.eq.nnf-1)
     *                     xtetwd(ie)=ior(xtetwd(ie),mask)
            endif
          enddo
        enddo
      enddo
      endif
      go to 9997
      endif
C     __________________________________________________________________
C
C     PROCESS 'DELETE' MODE
C
      if (mode(1:lenmode) .eq. 'delete') then
C
C        ...............................................................
C        SEE IF THE SET EXISTS
C
         if (ndup .eq. 0) then
            write(logmess,1005) name
            call writloga('default',1,logmess,1,ierr)
            goto 9998
         endif
C
C        ...............................................................
C        FIND LAST SET
C
         icnt=0
         do 410 i=1,nbitsmax
            if (eltsetnames(i)(1:1).ne.' '.and.
     *          eltsetnames(i)(1:5).ne.'-def-')
     *                  icnt=i
  410    continue
C
C        ...............................................................
C        GET THE LIST OF POINTS FOR THE LAST SET
C
         cpt1='esetnames'
         cpt2='get'
         cpt3=eltsetnames(icnt)
         mpno=ntets
         call eltlimc(cpt1,cpt2,cpt3,ipmpary1,mpno,
     *                ntets,xtetwd)
 
C
C        ...............................................................
C        SET POINTS FOR LAST SET TO CURRENT SET
C
         do 420 i=1,mpno
            i1=mpary1(i)
            itmpwd(i1)=ior(itmpwd(i1),mask)
 420     continue
         isetchg=isetchg+1
         eltsetnames(ibitpos)=eltsetnames(icnt)
C
C        ...............................................................
C        ZERO OUT LAST POINT SET
C
         mask2=imask(icnt)
         nmask2=not(mask2)
         do 430 i=1,ntets
            xtetwd(i)=iand(itmpwd(i),nmask2)
 430     continue
         isetchg=isetchg+1
         eltsetnames(icnt)='-def-'
C
         write(logmess,6020) name
 6020    format(' ELTSET ',a32,' DELETED')
         call writloga('default',1,logmess,1,ierrw)
         ierror=0
         go to 9998
      endif
C     _______________________________________________________________
C   PROCESS MREGION ELTSET COMMAND
C
      if(mode(1:lenmode).eq.'mregion') then
         iregnam=cmsgin(4)
C
C   GET REGION INFORMATION
C
         call mmfindbk('cmregs',geom_name,ipcmregs,length,ierror)
C        ...............................................................
C        CHECK THAT THIS REGION EXISTS
C
         do ir=1,nmregs
            len1=icharlnf(iregnam)
            len2=icharlnf(cmregs(ir))
            len3=max(len1,len2)
            if (cmregs(ir)(1:len3).eq.iregnam(1:len3).and.
     *             len1.eq.len2) then
               iregck=ir
               go to 405
            endif
         enddo
         ierror=1
         write(logmess,9001) iregnam
 9001    format('  ERROR - MREGION ',a8,' DOES NOT EXIST')
         call writloga('default',1,logmess,1,ierr)
         go to 9998
C        ...............................................................
C        GET THE SEARCH RANGE.
C
 405     call get_epsilon('epsilonl', srchval)
C           ............................................................
C           MREGION DATA AVAILABLE TO SET TETRAHEDRAL MATERIAL INDEX.
C
         length=6*ntets
         call mmgetblk('xcntr1',isubname,ipxcntr1,length,2,icscode)
         call mmgetblk('ycntr1',isubname,ipycntr1,length,2,icscode)
         call mmgetblk('zcntr1',isubname,ipzcntr1,length,2,icscode)
         call mmgetblk('iregno',isubname,ipiregno,length,2,icscode)
         call mmgetblk('isurfno',isubname,ipisurfno,length,2,icscode)
C
         npts=0
         do it = 1,ntets
C
                  xcntr=0.0
                  ycntr=0.0
                  zcntr=0.0
                  nen=nelmnen(itettyp(it))
                  do j=1,nelmnen(itettyp(it))
                     i1=itet(itetoff(it)+j)
                     xcntr=xcntr+xic(i1)
                     ycntr=ycntr+yic(i1)
                     zcntr=zcntr+zic(i1)
                  enddo
                  xfac=1.0/real(nen)
                  npts=npts+1
                  xcntr1(npts)=xfac*xcntr
                  ycntr1(npts)=xfac*ycntr
                  zcntr1(npts)=xfac*zcntr
C
         enddo
C        ...............................................................
C        CALL getregv TO FIND THE REGIONS THE POINTS LIE IN
C
         call getregv1(xcntr1,ycntr1,zcntr1,itp1,npts,srchval,
     &                'mregion',0,cmo,
     &                iregno,isurfno,
     &                ierr)
C
         do it=1,ntets
            if(iregno(it).eq.iregck) xtetwd(it)=
     *              ior(xtetwd(it),mask)
         enddo
         go to 9997
      endif
C   PROCESS REGION ELTSET COMMAND
      if(mode(1:lenmode).eq.'region') then
         iregnam=cmsgin(4)
         call mmfindbk('cregs',geom_name,ipcregs,length,ierror)
C
C        ...............................................................
C        CHECK THAT THIS REGION EXISTS
C
         do ir=1,nregs
            len1=icharlnf(iregnam)
            len2=icharlnf(cregs(ir))
            len3=max(len1,len2)
            if (cregs(ir)(1:len3).eq.iregnam(1:len3).and.
     *             len1.eq.len2) then
               iregck=ir
               go to 305
            endif
         enddo
         ierror=1
         write(logmess,9000) iregnam
 9000    format('  ERROR - REGION ',a8,' DOES NOT EXIST')
         call writloga('default',1,logmess,1,ierr)
         go to 9998
C        ...............................................................
C        GET THE SEARCH RANGE.
C
 305     call get_epsilon('epsilonl', srchval)
c
c  get temporary memory
c
         length=6*ntets
         call mmgetblk('xcntr1',isubname,ipxcntr1,length,2,icscode)
         call mmgetblk('ycntr1',isubname,ipycntr1,length,2,icscode)
         call mmgetblk('zcntr1',isubname,ipzcntr1,length,2,icscode)
         call mmgetblk('iregno',isubname,ipiregno,length,2,icscode)
         call mmgetblk('isurfno',isubname,ipisurfno,length,2,icscode)
C
         npts=0
         do it = 1,ntets
C
                  xcntr=0.0
                  ycntr=0.0
                  zcntr=0.0
                  nen=nelmnen(itettyp(it))
                  do j=1,nelmnen(itettyp(it))
                     i1=itet(itetoff(it)+j)
                     xcntr=xcntr+xic(i1)
                     ycntr=ycntr+yic(i1)
                     zcntr=zcntr+zic(i1)
                  enddo
                  xfac=1.0/real(nen)
                  npts=npts+1
                  xcntr1(npts)=xfac*xcntr
                  ycntr1(npts)=xfac*ycntr
                  zcntr1(npts)=xfac*zcntr
C
         enddo
C
C        ...............................................................
C        CALL getregv TO FIND THE REGIONS THE POINTS LIE IN
C
         call tstregv(xcntr1,ycntr1,zcntr1,npts,srchval,
     &                    iregnam,iregno,
     &                    ierr)
C
         do it=1,ntets
            if(iregno(it).eq.1) xtetwd(it)=
     *              ior(xtetwd(it),mask)
         enddo
         go to 9997
 
      endif
C
C   PROCESS VOLUME ELTSET COMMAND
C
      if(mode(1:lenmode).eq.'volume') then
         do it=1,ntets
            ityp=itettyp(it)
            do j=1,nelmnen(ityp)
               xv(j)=xic(itet(itetoff(it)+j))
               yv(j)=yic(itet(itetoff(it)+j))
               zv(j)=zic(itet(itetoff(it)+j))
            enddo
            call volume_element(ityp,xv,yv,zv,volelm)
            if (cmsgin(4)(1:2).eq.'le'.and.volelm.le.xmsgin(5)) then
               xtetwd(it)=ior(xtetwd(it),mask)
            elseif (cmsgin(4)(1:2).eq.'lt'.and.volelm.lt.xmsgin(5)) then
               xtetwd(it)=ior(xtetwd(it),mask)
            elseif (cmsgin(4)(1:2).eq.'ge'.and.volelm.ge.xmsgin(5)) then
                xtetwd(it)=ior(xtetwd(it),mask)
            elseif (cmsgin(4)(1:2).eq.'gt'.and.volelm.gt.xmsgin(5)) then
                xtetwd(it)=ior(xtetwd(it),mask)
            elseif (cmsgin(4)(1:2).eq.'eq'.and.volelm.eq.xmsgin(5)) then
                xtetwd(it)=ior(xtetwd(it),mask)
            elseif (cmsgin(4)(1:2).eq.'ne'.and.volelm.ne.xmsgin(5)) then
                xtetwd(it)=ior(xtetwd(it),mask)
            endif
         enddo
         go to 9997
      endif
C
C   PROCESS ASPECT ELTSET COMMAND
C
      if(mode(1:lenmode).eq.'aspect') then
         ict=0
         do it=1,ntets
            ityp=itettyp(it)
            do j=1,nelmnen(ityp)
               xv(j)=xic(itet(itetoff(it)+j))
               yv(j)=yic(itet(itetoff(it)+j))
               zv(j)=zic(itet(itetoff(it)+j))
            enddo
            call aratio_element(ityp,xv,yv,zv,volelm,eps)
            if (cmsgin(4)(1:2).eq.'le'.and.volelm.le.xmsgin(5)) then
               xtetwd(it)=ior(xtetwd(it),mask)
            ict=ict+1
            elseif (cmsgin(4)(1:2).eq.'lt'.and.volelm.lt.xmsgin(5)) then
               xtetwd(it)=ior(xtetwd(it),mask)
            ict=ict+1
            elseif (cmsgin(4)(1:2).eq.'ge'.and.volelm.ge.xmsgin(5)) then
               xtetwd(it)=ior(xtetwd(it),mask)
            ict=ict+1
            elseif (cmsgin(4)(1:2).eq.'gt'.and.volelm.gt.xmsgin(5)) then
               xtetwd(it)=ior(xtetwd(it),mask)
            ict=ict+1
            elseif (cmsgin(4)(1:2).eq.'eq'.and.volelm.eq.xmsgin(5)) then
               xtetwd(it)=ior(xtetwd(it),mask)
            ict=ict+1
            elseif (cmsgin(4)(1:2).eq.'ne'.and.volelm.ne.xmsgin(5)) then
               xtetwd(it)=ior(xtetwd(it),mask)
            ict=ict+1
            endif
         enddo
         go to 9997
      endif
 
C
C     PROCESS THE attribute name MODE.
C
      call cmo_get_info(mode,cmo,ipattribute,ilen,ityp,ierr)
      if(ierr.ne.0) then
         write(logmess,"('Attribute ',a10, ' not found in mesh object '
     *            ,a10)") mode,cmo
         call writloga('default',1,logmess,1,ierrw)
         go to 9998
      endif
C
C     Test for case where attribute is not nelements long.
C     This would be non-standard but not an automatic error.
C     Test and issue a warning.
C
      if(ilen.ne.ntets) then
         write(logmess,"('WARNING ELTSET: Attribute ',a10)") mode
         call writloga('default',0,logmess,0,ierrw)
         write(logmess,"('WARNING ELTSET: length= ',i10)")ilen
         call writloga('default',0,logmess,0,ierrw)
         write(logmess,"('WARNING ELTSET: NELEMENTS = ',i10)") ntets
         call writloga('default',0,logmess,0,ierrw)
         write(logmess,"('WARNING ELTSET: eltset will still be created')
     *           ") 
         call writloga('default',0,logmess,1,ierrw)
      endif
C  get value to test
      if(msgtype(5).eq.3) then
C  get material number corresponding to this name
        iregnam=cmsgin(5)
C  see if region exists
        call mmfindbk('cregs',geom_name,ipcregs,length,ierror)
        call mmfindbk('matregs',geom_name,ipmatregs,length,ierror)
C
C        ...............................................................
C        CHECK THAT THIS REGION EXISTS
C
         do ir=1,nregs
            len1=icharlnf(iregnam)
            len2=icharlnf(cregs(ir))
            len3=max(len1,len2)
            if (cregs(ir)(1:len3).eq.iregnam(1:len3).and.len1.eq.len2)
     *             then
               iregck=ir
               ivalue=matregs(ir)
               go to 307
            endif
         enddo
         ierror=1
      else
        if(msgtype(5).eq.1) ivalue=imsgin(5)
        if(msgtype(5).eq.2) value=xmsgin(5)
      endif
307   if(msgtype(4).ne.3) go to 9998
      operation=cmsgin(4)
      lenop=icharlnf(operation)
      do it=1,ntets
      if(msgtype(5).eq.2) then
         if(operation(1:lenop).eq.'eq'.and.
     *       attribute(it).eq.value) itmpwd(it)=ior(itmpwd(it),mask)
         if(operation(1:lenop).eq.'le'.and.
     *       attribute(it).le.value) itmpwd(it)=ior(itmpwd(it),mask)
         if(operation(1:lenop).eq.'lt'.and.
     *       attribute(it).lt.value) itmpwd(it)=ior(itmpwd(it),mask)
         if(operation(1:lenop).eq.'ge'.and.
     *       attribute(it).ge.value) itmpwd(it)=ior(itmpwd(it),mask)
         if(operation(1:lenop).eq.'gt'.and.
     *       attribute(it).gt.value) itmpwd(it)=ior(itmpwd(it),mask)
         if(operation(1:lenop).eq.'ne'.and.
     *       attribute(it).ne.value) itmpwd(it)=ior(itmpwd(it),mask)
      else
         if(operation(1:lenop).eq.'eq'.and.
     *        iattribute(it).eq.ivalue) itmpwd(it)=ior(itmpwd(it),mask)
         if(operation(1:lenop).eq.'le'.and.
     *        iattribute(it).le.ivalue) itmpwd(it)=ior(itmpwd(it),mask)
         if(operation(1:lenop).eq.'lt'.and.
     *        iattribute(it).lt.ivalue) itmpwd(it)=ior(itmpwd(it),mask)
         if(operation(1:lenop).eq.'ge'.and.
     *        iattribute(it).ge.ivalue) itmpwd(it)=ior(itmpwd(it),mask)
         if(operation(1:lenop).eq.'gt'.and.
     *        iattribute(it).gt.ivalue) itmpwd(it)=ior(itmpwd(it),mask)
         if(operation(1:lenop).eq.'ne'.and.
     *        iattribute(it).ne.ivalue) itmpwd(it)=ior(itmpwd(it),mask)
      endif
      enddo
      go to 9996
C
C     ******************************************************************
C
C     PLACE TEMP. DATA INTO SETWORD.
C
 9996 do 490 i=1,ntets
         xtetwd(i)=itmpwd(i)
  490 continue
C
C     ******************************************************************
C
C     PRINT OUT ELTSET INFO.
C
 9997 continue
      cpt1='esetnames'
      cpt2='get'
      cpt3=name
      mpno=ntets
      call eltlimc(cpt1,cpt2,cpt3,ipmpary1,mpno,
     *             ntets,xtetwd)
 
      write(logmess,6010) name,mpno
 6010 format(' THE ELTSET ',a32,' HAS ',i10,' ELEMENTS')
      call writloga('default',1,logmess,0,ierr)
      if(idebug.ge.1.or.iprint.eq.1) then
      n_entry_per_line = 8
        do 500 i=1,mpno,n_entry_per_line
           write(logmess,6011)
     *     (mpary1(j),j=i,min0(i-1+n_entry_per_line,mpno))
 6011      format(2x,8(i8,1x))
           if (iprint .eq. 0)
     *       call writloga('bat',0,logmess,0,ierr)
           if (iprint .eq. 1)
     *       call writloga('default',0,logmess,0,ierr)
  500   continue
      endif
C
C
C     ******************************************************************
C
C     RELEASE THE LOCAL MEMORY ALLOCATED FOR THIS ROUTINE.
C
 9998 continue
C     ******************************************************************
C
      call mmrelprt(isubname,icscode)
C
C     ******************************************************************
C
 1000 format ('ERROR - THE NUMBER OF ELTSETS EXCEEDS ',i4)
 1005 format(' ERROR - THE NAME ',2x,a32,2x,' DOES NOT EXIST !')
C
C
C     ******************************************************************
C
C     SET UP THE CFT IMMUNE STATEMENT FOR DDT.
C
      goto 9999
 9999 continue
C
      return
      end
