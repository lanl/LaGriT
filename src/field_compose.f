*dk,field_compose
      subroutine field_compose(imsgin,xmsgin,cmsgin,msgtype,nwds,
     &                    ierror)
C
C #####################################################################
C
C     PURPOSE - FIELD_COMPOSE replaces the node values of a field
C        with those field values composed with the function LOG or 
C        ASINH, or with the 'inverse' functions EXP or SINH.  LINEAR 
C        (the identity map) is available for consistency reasons.
C
C     INPUT ARGUMENTS -
C
C         imsgin()  - Integer array of command input tokens
C         xmsgin()  - Real array of command input tokens
C         cmsgin()  - Character array of command input tokens
C         msgtype() - Integer array of command input token types
C         nwds      - Number of command input tokens
C
C     OUTPUT ARGUMENTS -
C
C         ierror - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C     CHANGE HISTORY -
C
C        $Log:   /pvcs.config/t3d/src/field_compose.f_a  $
CPVCS    
CPVCS       Rev 1.4   Mon Apr 14 16:47:42 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.3   11/07/95 17:17:16   dcg
CPVCS    change flag to 2 in mmgetblk calls
CPVCS    
CPVCS       Rev 1.2   09/12/95 21:58:20   kuprat
CPVCS    Performed composition using 'xinterpolate' instead of by hand.
CPVCS    Now support 'inverse' functions as well as identity map.
CPVCS    
CPVCS    
CPVCS       Rev 1.1   09/10/95 19:51:38   kuprat
CPVCS    Fixed bug in command parameter list indexing.
CPVCS    
CPVCS       Rev 1.0   08/24/95 16:37:32   kuprat
CPVCS    Initial revision.
C
C ######################################################################
C    
c   FIELD / COMPOSE / firstpt,lastpt,stride / field / function
c
C   For all points in the specified point set of the current cmo, we 
c   replace the values of FIELD with those values composed with the 
c   specified function.  That is,
c 
c   field(i) := log(field(i))   or
c   field(i) := asinh(field(i)) or
c   field(i) := exp(field(i))   or
c   field(i) := sinh(field(i))  or
c   field(i) unchanged.
C
      implicit none
      include 'consts.h'

      integer nwds, imsgin(nwds), msgtype(nwds)
      real*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
      integer ierror

      pointer (ipfield,field)
      pointer (ipisetwd,isetwd)
      pointer (ipitp1, itp1)
      real*8 field(10000000)
      integer isetwd(10000000)
      integer itp1(10000000)

      pointer (ipmpary,mpary)
      integer mpary(10000000)

      integer icharln,ierrw,length,icmotype,mpno,icscode,
     &   ipt1,ipt2,ipt3
      character*132 logmess
      character*32 ich1,ich2,ich3,cmo,isubname,blkname,charstr

      integer i,nnodes
      real*8 val,xinterpolate

      isubname = 'field_compose'

      ierror=0
C
C  get name of current mesh object.
C
      call cmo_get_name(cmo,ierror)
      if(ierror.ne.0) then
         write(logmess,'(a)')
     *         'FIELD_COMPOSE: bad current mesh object'
         call writloga('default',0,logmess,0,ierrw)
         goto 9999
      endif
C
C  Allocate local arrays
C
      call cmo_get_info('nnodes',cmo,
     *                        nnodes,length,icmotype,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')

      call mmgetblk('mpary',isubname,ipmpary,nnodes,2,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'mmgetblk') 
c 
c    get cmo array pointers
c
      call cmo_get_info('isetwd',cmo,
     *                        ipisetwd,length,icmotype,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('itp1',cmo,
     *                        ipitp1,length,icmotype,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')

      blkname=cmsgin(7)(1:icharln(cmsgin(7)))
      call mmgetpr(blkname,cmo,ipfield,icscode)
      if(icscode.ne.0) then
         ierror=icscode
         write(logmess,'(a)')
     *      'FIELD_COMPOSE: bad field'
         call writloga('default',0,logmess,0,ierrw)
         goto 9999
      endif
C
C    set the point index boundaries
C
      ich1=' '
      ich2=' '
      ich3=' '

      mpno=0

      if (msgtype(4) .eq. 1) then
         ipt1=imsgin(4)
         ipt2=imsgin(5)
         ipt3=imsgin(6)
         call pntlimn(ipt1,ipt2,ipt3,ipmpary,mpno,
     *                                nnodes,isetwd,itp1)
      else
         ich1=cmsgin(4)
         ich2=cmsgin(5)
         ich3=cmsgin(6)
         call pntlimc(ich1,ich2,ich3,ipmpary,mpno,
     *                                nnodes,isetwd,itp1)
      endif

      write(logmess,'(a)')
     *      'FIELD_COMPOSE: compose function with field values.'
      call writloga('default',0,logmess,0,ierrw)

      if (mpno.gt.0) then
         write(logmess,'(a,i10)')
     *         'nodes in point set  = ',mpno
         call writloga('default',0,logmess,0,ierrw)
      else
         write(logmess,'(a)') 'No points to compose!'
         call writloga('default',0,logmess,0,ierrw)
         goto 9999
      endif
c
c     Perform composition.
c
      charstr=cmsgin(3)(1:icharln(cmsgin(3)))
      if (charstr.eq.'log') then
         do i=1,mpno
            val=field(mpary(i))
            field(mpary(i))=xinterpolate(1,2,val)
         enddo
      elseif (charstr.eq.'asinh') then
         do i=1,mpno
            val=field(mpary(i))
            field(mpary(i))=xinterpolate(1,3,val)
         enddo
      elseif (charstr.eq.'exp') then
         do i=1,mpno
            val=field(mpary(i))
            field(mpary(i))=xinterpolate(2,2,val)
         enddo
      elseif (charstr.eq.'sinh') then
         do i=1,mpno
            val=field(mpary(i))
            field(mpary(i))=xinterpolate(2,3,val)
         enddo
      elseif (charstr.eq.'linear') then
         do i=1,mpno
            val=field(mpary(i))
            field(mpary(i))=xinterpolate(1,1,val)
         enddo
      else
         write(logmess,'(a)') 'FIELD_COMPOSE:  ',
     &      'Bad composition function.'
         call writloga('default',0,logmess,0,ierrw)
         ierror=-1
         goto 9999
      endif

 9999 continue
      call mmrelprt(isubname,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'mmrelprt')
      return
      end
