      subroutine fset(imsgin,xmsgin,cmsgin,msgtype,nwds,ierror)
      implicit none
C
C ######################################################################
C
C     PURPOSE -
C
C        This routine builds boundary face sets in a specified manner.
C
C     FORMAT: FSET/name/pset,get,pointset/
C             ('name' must be an int type or character string)        
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
C
C ######################################################################
C
      integer nplen,ntlen,nef,flen
      parameter (nplen=1000000,ntlen=1000000)
C
      character*132 logmess
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
C
      pointer (ipftet, ftet)
      integer ftet(ntlen)
C
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
      pointer (ipitet, itet)
      integer itet(4*1000000)
      pointer (ipjtet, jtet)
      integer jtet(4*1000000)
      integer itetclr,itettyp,itetoff,mbndry
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
      character*32 isubname,fname,mode,iregnam,geom_name,ename
      integer nbitsmax
      parameter (nbitsmax=32) 
      character*32  fnames(nbitsmax)
      character*32 cpt1, cpt2, cpt3,operation
      character*32  cpset
      character*32 cmo
      integer itest(nbitsmax),ibpos(nbitsmax) 
      integer emask
      integer shiftr,shiftl,compl
      pointer( ipmpary1 ,  mpary1(1000000) )
      pointer( ipmpary1 , xmpary1(1000000) )
      pointer( ipmpary2 ,  mpary2(1000000) )
      pointer( ipmpary4 ,  mpary4(1000000) )
      pointer( ipmpary4 , xmpary4(1000000) )
      pointer(iptmpwd, itmpwd(1000000))                 
      integer mpary1,mpary4,itmpwd,mpary2
      real xmpary1,xmpary4
      pointer (ipeltsetnames,eltsetnames)
      character*32 eltsetnames(*)
      pointer (ipfsetnames,fsetnames)
      character*32 fsetnames(*)
C
      pointer(ipiregno,iregno)
      pointer(ipisurfno,isurfno)
      integer iregno(ntlen),isurfno(ntlen)
C
       real*8 xv(10),yv(10),zv(10),volelm,eps,srchval,
     * xfac,xxlarge,xxsmall,xsmall,
     * xcntr,ycntr,zcntr
      integer ityp,ierror,npoints,length,icmotype,ipointi,ipointj,
     * icscode,i,j,it,icnt,isetchg,len1,len2,ibitpos,ir,len3,
     * iprint,lenname,index,xlenname,
     * ierr,ndup,nsetcnt,mask,imask,nmask,ict,fsetcnt,
     * iunion,ipt1,iregck,npts,mask2,nmask2,
     * ilen,ivalue,lenop,number_of_eltsets,number_of_fsets
      integer nwd1,nwd2,ipos1,ipos2,iintrfce,nsets,iout,lout,itype,
     * lenmode,mpno,ierrw,nen,inode,i1,
     * idebug, n_entry_per_line,
     * ntets
      pointer(ipout,out)
      real*8 out(*),rout
      integer icharlnf
      real*8 alargenumber
      parameter (alargenumber=1.d+20)
      integer count, elmember
      character*132 cbuf
C
C ######################################################################
C
      imask(i)=2**(i-1)
C
C ######################################################################
C     ******************************************************************
      isubname='fsetnames'
      ierror = 0
C     ******************************************************************
C     get mesh object
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
      call cmo_get_info('faces_per_element',cmo,
     *                  nef,length,icmotype,ierror)
      call cmo_get_info('imt1',cmo,ipimt1,length,icmotype,ierror)
      call cmo_get_info('itp1',cmo,ipitp1,length,icmotype,ierror)
      call cmo_get_info('icr1',cmo,ipicr1,length,icmotype,ierror)
      call cmo_get_info('isn1',cmo,ipisn1,length,icmotype,ierror)
      call cmo_get_info('xic',cmo,ipxic,length,icmotype,ierror)
      call cmo_get_info('yic',cmo,ipyic,length,icmotype,ierror)
      call cmo_get_info('zic',cmo,ipzic,length,icmotype,ierror)
      call cmo_get_info('xtetwd',cmo,ipxtetwd,length,icmotype,ierror)
      call cmo_get_info('mbndry',cmo,mbndry,length,icmotype,ierror)
      call cmo_get_info('itetclr',cmo,
     *                  ipitetclr,length,icmotype,ierror)
      call cmo_get_info('itettyp',cmo,
     *                  ipitettyp,length,icmotype,ierror)
      call cmo_get_info('itetoff',cmo,
     *                  ipitetoff,length,icmotype,ierror)
      call cmo_get_info('itet',cmo,ipitet,length,icmotype,ierror)
      call cmo_get_info('jtet',cmo,ipjtet,length,icmotype,ierror)
      call cmo_get_info('ipointi',cmo,ipointi,length,icmotype,icscode)
      call cmo_get_info('ipointj',cmo,ipointj,length,icmotype,icscode)
      call cmo_get_info('number_of_eltsets',cmo,number_of_eltsets,
     *    length,icmotype,icscode)
      call cmo_get_info('number_of_fsets',cmo,number_of_fsets,
     *    length,icmotype,icscode)
C
C
      iprint=0
C
C     ALLOCATE SOME LOCAL MEMORY FOR THIS ROUTINE.
C
      length=max(npoints,ntets)
      call mmgetblk('xmpary1',isubname,ipmpary1,length,2,icscode)
      call mmgetblk('xmpary4',isubname,ipmpary4,length,2,icscode)
C
      length=ntets*nef 
      call mmgetblk('mpary2',isubname,ipmpary2,length,1,icscode)
C
C     FSETS PRELIMINARIES
C     SETUP FOR FSET BEFORE CREATING TEMPORARY ESET
C
C     ******************************************************************
C
C     SET CONSTANTS, NAME AND MODE
C
      xxlarge=alargenumber
      xxsmall=1.0e-20
      xsmall=1.0e-20
C
      if(msgtype(2).eq.3)then
        mode='name'
      elseif(msgtype(2).eq.1)then
        mode='number'
      else
        write(logmess,10) 
        call writloga('default',1,logmess,1,ierr)
 10     format ('ERROR - FSET NAME MUST BE INT OR A CHAR TYPE ')
        goto 9998
      endif  
      if(mode.eq.'name')then
        fname=cmsgin(2)
        lenname=icharlnf(fname)
      endif
C
C     ******************************************************************
C
      isetchg=0
      index=0
      if(mode.eq.'number')then
        fsetcnt=imsgin(2)
      else
        call mmfindbk('fsetnames',cmo,ipfsetnames,ilen,icscode)
        do i=1,nbitsmax
          if(fsetnames(i).eq.fname) then
            fsetcnt=i
            go to 12
          endif
        enddo
      endif
 12   continue
C     ******************************************************************
C     ******************************************************************
C     ******************************************************************
C
C     CHECK TO SEE IF THE NAME HAS ALREADY BEEN USED AND SET THE
C     BIT POSITION FOR FSET NAME.
C
      if(mode.eq.'name')then
        do i=1,nbitsmax
           len2=icharlnf(fsetnames(i))
           len1=max(lenname,len2)
           itest(i)=0
           if(fname(1:len1).eq.fsetnames(i)(1:len1)) itest(i)=i
        enddo
        call kmprsn(nbitsmax,itest,1,itest,1,itest,1,ndup)
        if(ndup.eq.0) then
           do 15 i=1,nbitsmax
              if(fsetnames(i).eq.' '.or.fsetnames(i)(1:5).eq.'-def-')
     *        then
                 fsetcnt=i
                 goto 16
              endif
 15        continue
 16        continue
           ibitpos=fsetcnt
           if(fsetcnt.gt.nbitsmax) then
              write(logmess,1000) nbitsmax
              call writloga('default',1,logmess,1,ierr)
              goto 9998
           endif
           if(fsetcnt.le.0) fsetcnt=1
           isetchg=isetchg+1
           fsetnames(fsetcnt)=fname(1:lenname)
           number_of_fsets=number_of_fsets+1
        else
           ibitpos=itest(1)
        endif
      endif
C
C     ******************************************************************
C
C     SET THE BIT POSITION FOR TEMPORARY ESET
C
      ename='tempeset'
      xlenname=icharlnf(ename)
      isetchg=0
      index=0
      call mmfindbk('eltsetnames',cmo,ipeltsetnames,ilen,icscode)
c
      do i=1,nbitsmax
         len2=icharlnf(eltsetnames(i))
         len1=max(xlenname,len2)
         itest(i)=0
         if(ename(1:len1).eq.eltsetnames(i)(1:len1)) itest(i)=i
      enddo
      call kmprsn(nbitsmax,itest,1,itest,1,itest,1,ndup)
      if(ndup.eq.0) then
         do 17 i=1,nbitsmax
            if(eltsetnames(i).eq.' '.or.eltsetnames(i)(1:5).eq.'-def-')
     *       then
               nsetcnt=i
               goto 18
            endif
 17      continue
 18      continue
         ibitpos=nsetcnt
         if(nsetcnt.gt.nbitsmax) then
            write(logmess,1000) nbitsmax
            call writloga('default',1,logmess,1,ierr)
            goto 9998
         endif
         if(nsetcnt.le.0) nsetcnt=1
         isetchg=isetchg+1
         eltsetnames(nsetcnt)=ename(1:xlenname)
         number_of_eltsets=number_of_eltsets+1
      else
         ibitpos=itest(1)
      endif
      mask=imask(ibitpos)
      nmask=not(mask)
C
C ***************************************************************
C     PROCESS THE ESET FIRST 
C
      if(msgtype(5).eq.3)then
         cpset=cmsgin(5)
         call pntlimc('pset','get',cpset,ipmpary4,mpno,
     *                   npoints,isetwd,itp1)
      else
         write(logmess,"('Error in fset command - no point set')")
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
C        LOOP THROUGH TETS LOOKING FOR NODES THAT BELONG TO PSET
C        SEARCH IS INCLUSIVE BY DEFAULT
         do i=1,ntets
            nen=nelmnen(itettyp(i))
            emask=0
            do j=1,nen
               inode=itet(itetoff(i)+j)
               if(mpary1(inode).eq.1) then
                 xtetwd(i)=ior(xtetwd(i),mask) 
               endif
            enddo
         enddo
C
C
C     ******************************************************************
C     PRINT OUT TEMPORARY ELTSET INFO.
C
      cpt1='eltsetnames'
      cpt2='get'
      cpt3=ename
      mpno=ntets
      call eltlimc(cpt1,cpt2,cpt3,ipmpary1,mpno,
     *             ntets,xtetwd)
 
      write(logmess,19) ename,mpno
 19   format(' THE TEMPORARY ESET ',a32,' HAS ',i10,' ELEMENTS')
      call writloga('default',1,logmess,0,ierr)
      if(idebug.ge.1.or.iprint.eq.1) then
      n_entry_per_line = 8
        do 21 i=1,mpno,n_entry_per_line
           write(logmess,20)
     *     (mpary1(j),j=i,min0(i-1+n_entry_per_line,mpno))
 20        format(2x,8(i8,1x))
           if (iprint .eq. 0)
     *       call writloga('bat',0,logmess,0,ierr)
           if (iprint .eq. 1)
     *       call writloga('default',0,logmess,0,ierr)
 21     continue
      endif
C
C     ******************************************************************
C     THERE ARE TWO PHILOSOPHIES: 1) ALLOCATE THE SPACE BY DEFAULT IN THE
C     SBINIT SUBROUTINE, OR 2) ALLOCATE THE SPACE AS A USER ATTRIBUTE WITH
C     THE DOTASKX3D COMMANAD BELOW (UNCOMMENT AND COMMENT OUT ATTRIBUTE 66
C     IN SBINT.
      call cmo_get_info('ftet',cmo,ipftet,length,icmotype,ierror)
      if(ierror.ne.0)then
        write(cbuf,*)'cmo/addatt/' //
     *                 cmo //
     *                 '/ftet/vint/faces_per_element/nelements' //
     *                 '/copy/permanent/l/0; finish'
        call dotaskx3d(cbuf,ierr)
        call cmo_get_info('ftet',cmo,ipftet,length,icmotype,ierror)
        do i=1,(ntets*nef)
          ftet(i)=0
        enddo
      endif
C     ******************************************************************
C     EXTRACT FSET FROM ESET
      do i=1,mpno
        elmember=mpary1(i)
        do j=1,nef
            if(jtet((elmember-1)*nef+j).eq.mbndry)then
              ftet((elmember-1)*nef+j)=fsetcnt
            endif
        enddo
      enddo
C     ******************************************************************
C     PRINT OUT FSET INFO.
C
      cpt1='fsetnames'
      cpt2='get'
      cpt3=fname
      ivalue=fsetcnt
      mpno=ntets*nef
      flen=ntets*nef	
      call flimc(cpt1,cpt2,cpt3,ipmpary2,mpno,
     *             flen,ftet,mode,ivalue)
 
      write(logmess,22) fname,mpno
 22   format(' THE FSET ',a32,' HAS ',i10,' FACES')
      call writloga('default',1,logmess,0,ierr)
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
 1000 format ('ERROR - THE NUMBER OF FSETS EXCEEDS ',i4)
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
