      subroutine coupling_coef_wrapper(imsgin,xmsgin,cmsgin,msgtype,
     *                              nwds,ierror)
 
C---------------------------------------------------------------------------
C
c
c#######################################################################
c
c     purpose -
C
C  This routine controls dectecting and optionally fixing
C  negative coupling coefficients that occur on the
C  exterior boundary of a grid
C
c     input arguments -
c
C        imsgin()  - Integer array of command input tokens
C        xmsgin()  - Real array of command input tokens
C        cmsgin()  - Character array of command input tokens
C        msgtype() - Integer array of command input token types
C        nwds      - Number of command input tokens
c
c     output arguments -
c
C        ierror - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C    format
C    negative_aij
C    negative_aij/identify.
C    negative_aij/refine/num_iters
C    negative_aij/eltset/eset_name
c  negative_aij will compute the negative coupling coefficients on
C  the external boundary by calling test_coupling_coef.  The
C  attribute ietet_aij will be created (this attribute contains the
C  bad tet,bad face, bad edge for each negative coupling coefficient
C
C  negative_aij/identify -- identifies the negative coupling coefficient
C  tets and places them into an element based attribute neg_coup_coeff.  this
c  attribute is 1.0 if the tet is good and less than 1.0 if the tet is
c  bad.  the closer the value is to 0, the "worse" the tet is.
c
C  negative_aij/refine/num_iters will compute the negative coupling
C  coefficients on the external boundary by calling test_coupling_coef.
C  Then it will call refine_coupling_coef to attempt to fix the
C  bad edges - it will repeat these two call a max of num_iters
C  times
C  negative_aij/eset/eset_name will compute the negative coupling
C  coefficients on the external boundary by calling test_coupling_coef.
C  It will then create an eset called eset_name that will contain the
C  bad tets.
c
c     change history -
C
C $Log:   /pvcs.config/t3d/src/coupling_coef_wrapper_nosb.f_a  $
CPVCS    
CPVCS       Rev 1.4   30 Sep 2004 09:36:34   dcg
CPVCS    replace .or. syntax of integers with call to ior
CPVCS    
CPVCS       Rev 1.3   28 Mar 2000 14:09:12   dcg
CPVCS    remove include 'machine.h'
CPVCS    
CPVCS       Rev 1.2   Fri Feb 04 16:35:38 2000   dcg
CPVCS    
CPVCS       Rev 1.1   03 Feb 2000 09:20:26   dcg
CPVCS    
CPVCS       Rev 1.0   28 Jan 2000 14:46:52   dcg
CPVCS    Initial revision.
CPVCS
CPVCS       Rev 1.3   Fri Jul 24 14:55:52 1998   dcg
CPVCS    add rivara option
CPVCS
CPVCS       Rev 1.2   Thu Mar 19 16:30:10 1998   murphy
CPVCS    Added negative_aij/identify to wrapper
CPVCS
CPVCS       Rev 1.1   Fri Feb 27 11:48:24 1998   dcg
CPVCS    fix problem with losing values of nwds and other
CPVCS    input arguments after recursion.  This fix
CPVCS    avoids the problem but does not address the cause.
CPVCS
CPVCS       Rev 1.0   Thu Feb 26 12:14:42 1998   dcg
CPVCS    Initial revision.
 
C ######################################################################
C
      implicit none
      include 'cmo_lg.h'
      integer nbitsmax
      parameter (nbitsmax=32)
      integer itest(nbitsmax)
      integer nwds, imsgin(nwds), msgtype(nwds)
      REAL*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
      character*32 isubname,cmo
      pointer (ipxtetwd, xtetwd)
      integer xtetwd(*)
 
      integer icharlnf,imask
      integer ierror,num_neg,ntets,length,itype,icscode,
     *  it,i,len1,len2,ndup,nsetcnt,number_of_eltsets,
     *  isetchg,n,mask,lenname,niter,nitermax,
     *  ibitpos,icount,nwds_save,ilen,index
      pointer (ipietet,ietet)
      integer ietet(3,*)
      character*132 logmess
      character*32 option,ename
      pointer (ipeltsetnames,eltsetnames)
      character*32 eltsetnames(*)
C
      imask(i)=2**(i-1)
C
      isubname='coupling_wrapper'
      num_neg=0
      option='test'
      nwds_save=nwds
 
 
      if(nwds_save.ge.2.and.msgtype(2).eq.3) option=cmsgin(2)
      if(nwds_save.ge.3.and.option(1:6).eq.'refine')
     *       nitermax=imsgin(3)
      if(nwds_save.ge.3.and.option(1:6).eq.'eltset')
     *       ename=cmsgin(3)
 
      print *,'in coup-coeff-wrapper.  option = ',option
 
      if (option(1:8).eq.'identify') then
         call ident_coupling_coef(ierror)
         goto 9999
      endif
 
      call test_coupling_coef(num_neg)
      if (num_neg.eq.0) go to 9999
C  check if we should refine or build eset
      if(option(1:4).eq.'test') go to 9999
      if(option(1:6).eq.'eltset') then
C  make an eset
c
C  get mesh object
C
         call cmo_get_name(cmo,ierror)
         call cmo_get_info('nelements',cmo,
     *                  ntets,length,itype,ierror)
         call cmo_get_info('number_of_eltsets',cmo,
     *                  number_of_eltsets,length,itype,ierror)
         call cmo_get_info('xtetwd',cmo,ipxtetwd,length,itype,ierror)
         call cmo_get_info('ietet_aij',cmo,ipietet,length,itype,ierror)
         isetchg=0
         index=0
         call mmfindbk('eltsetnames',cmo,ipeltsetnames,ilen,
     *      icscode)
 
C     CHECK TO SEE IF THE NAME HAS ALREADY BEEN USED AND SET THE
C     BIT POSITION.
C
 12      do i=1,nbitsmax
            lenname=icharlnf(ename)
            len2=icharlnf(eltsetnames(i))
            len1=max(lenname,len2)
            itest(i)=0
            if(ename(1:len1).eq.eltsetnames(i)(1:len1)) itest(i)=i
         enddo
         call kmprsn(nbitsmax,itest,1,itest,1,itest,1,ndup)
         if(ndup.eq.0) then
            do 15 i=1,nbitsmax
               if(eltsetnames(i).eq.' '.or.eltsetnames(i)(1:5).eq.
     *           '-def-') then
                  nsetcnt=i
                  goto 16
               endif
 15         continue
 16         continue
            ibitpos=nsetcnt
            if(nsetcnt.gt.nbitsmax) then
               write(logmess,1000) nbitsmax
1000           format ('ERROR - THE NUMBER OF ELTSETS EXCEEDS ',i4)
               call writloga('default',1,logmess,1,ierror)
               goto 9999
            endif
            if(nsetcnt.le.0) nsetcnt=1
            isetchg=isetchg+1
            eltsetnames(nsetcnt)=ename(1:lenname)
            number_of_eltsets=number_of_eltsets+1
         else
            ibitpos=itest(1)
         endif
         mask=imask(ibitpos)
C
C  Loop through tets and set mask bit for marked tets
C
         icount=0
         do it=1,ntets
            do n=1,num_neg
               if(ietet(1,n).eq.it) then
                  icount=icount+1
                  xtetwd(it)=ior(xtetwd(it),mask)
               endif
            enddo
         enddo
         write(logmess,45)ename(1:lenname),icount
 45      format('ELTSET ',a32,' created with ',i10,' elements')
         call writloga('default',0,logmess,0,ierror)
C
C  see if refine is called for
C
      elseif (option(1:6).eq.'refine'.or.option(1:6).eq.'rivara') then
         if (option(1:6).eq.'refine') then
            call refine_coupling_coef()
         else
            do i=1,100
               call refine_cc_rivara()
C  rivara refine does just one edge at a time
C  get next bad edge
C  and loop
               num_neg=0
               call test_coupling_coef(num_neg)
               if (num_neg.eq.0) then
                  write(logmess,95)
                  call writloga('default',0,logmess,0,ierror)
                  go to 100
               endif
            enddo
            go to 97
         endif
         nitermax=1
         if (nwds.gt.2) nitermax =imsgin(3)
         niter=1
C
C  iterate if asked to till nitermax iterations or till numneg is zero
C
         do while(niter.lt.nitermax)
            num_neg=0
            call test_coupling_coef(num_neg)
c            if (num_neg.eq.0) then
            if(num_neg.eq.1)then
               write(logmess,95)
 95            format('All boundary coupling coefficients are positive')
               call writloga('default',0,logmess,0,ierror)
               go to 100
            endif
            if (option(1:6).eq.'refine') then
               call refine_coupling_coef()
            else
               call refine_cc_rivara()
            endif
            niter=niter+1
         enddo
 97      write(logmess,96) num_neg
 96      format(i10,' boundary coupling coefficients are negative')
         call writloga('default',0,logmess,0,ierror)
      else
         write(logmess,99) option
 99      format(' Illegal option to negative_aij ',a32)
         call writloga('default',0,logmess,0,ierror)
         go to 9999
      endif
100   continue
9999  call mmrelprt(isubname,ierror)
      return
      end
 
 
 
 
