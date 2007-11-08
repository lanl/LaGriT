      subroutine field_scale(imsgin,xmsgin,cmsgin,msgtype,nwds,
     &                    ierror)
C
C
C #####################################################################
C
C     PURPOSE - FIELD_SCALE scales field values.
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
C        $Log:   /pvcs.config/t3d/src/field_scale_nosb.f_a  $
CPVCS    
CPVCS       Rev 1.1   Tue Feb 08 15:22:24 2000   dcg
CPVCS
CPVCS       Rev 1.0   Tue Feb 08 14:18:54 2000   dcg
CPVCS    Initial revision.
CPVCS
CPVCS       Rev 1.5   Fri Sep 25 15:37:24 1998   kuprat
CPVCS    Echo multiplication factor for FIELD/SCALE/NORMALIZE.
CPVCS
CPVCS       Rev 1.4   Mon Apr 14 16:47:46 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.3   11/16/95 15:21:50   dcg
CPVCS    replace character literals in calls
CPVCS
CPVCS       Rev 1.2   11/07/95 17:17:22   dcg
CPVCS    change flag to 2 in mmgetblk calls
CPVCS
CPVCS       Rev 1.1   10/16/95 23:17:44   kuprat
CPVCS    Added scale inquiry options GETMAX, GETMIN, and GETSPREAD.
CPVCS
CPVCS       Rev 1.0   08/24/95 16:38:22   kuprat
CPVCS    Initial revision.
C
C ######################################################################
c*                                                                    $&
c*  FIELD/SCALE/scale option/value/ifirst,ilast,istride/field list    $&
c*                                                                    $&
c*         For all points in the specified point set, we scale or     $&
c*      obtain the scale of the specified field.  SCALE OPTION can    $&
c*      take on the values NORMALIZE, MULTIPLY, DIVIDE, GETMAX,       $&
c*      GETMIN, GETSPREAD.                                            $&
c*                                                                    $&
c*         If NORMALIZE is specified, we multiply all the field       $&
c*      values by VALUE/(fieldmax-fieldmin), where 'fieldmax' and     $&
c*      'fieldmin' are the maximum and minimum values taken over the  $&
c*      point set.  This has the effect of normalizing the field so   $&
c*      that the new 'spread' between the maximum and minimum         $&
c*      values is equal to VALUE.                                     $&
c*                                                                    $&
c*        If MULTIPLY is specified, we multiply all the field values  $&
c*      in the point set by VALUE.                                    $&
c*                                                                    $&
c*        If DIVIDE is specified, we divide all the field values in   $&
c*      the point set by VALUE.                                       $&
c*                                                                    $&
c*        If GETMAX is specified, we write the maximum field value    $&
c*      encountered into the code variable VALUE.  (VALUE here must be$&
c*      a valid code variable appearing in the 'dictionary', such as  $&
c*      VAR0,..,VAR9.)                                                $&
c*                                                                    $&
c*        If GETMIN is specified, we write the minimum field value    $&
c*      encountered into the code variable VALUE.                     $&
c*                                                                    $&
c*        If GETSPREAD is specified, we write the spread between the  $&
c*      maximum and minimum field value encountered into the code     $&
c*      variable VALUE.                                               $&
c*                                                                    $&
C ######################################################################
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
 
      integer icharln,lenc,ierrw,length,icmotype,mpno,icscode,
     &   ipt1,ipt2,ipt3
      character*132 logmess
      character*32 ich1,ich2,ich3,cmo,isubname,blkname,option,cvalue,
     *  cout
      character*8 cglobal, cdefault
 
      integer i,nnodes,iout
      real*8 val,valspread,valmin,valmax,value
 
      isubname = 'field_scale'
      cglobal='global'
      cdefault='default'
 
      ierror=0
C
C  get name of current mesh object.
C
      call cmo_get_name(cmo,ierror)
      if(ierror.ne.0) then
         write(logmess,'(a)')
     *         'FIELD_SCALE: bad current mesh object'
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
 
      blkname=cmsgin(8)
      lenc=icharln(blkname)
      call mmgetpr(blkname(1:lenc),cmo,ipfield,icscode)
      if(icscode.ne.0) then
         ierror=icscode
         write(logmess,'(a)')
     *      'FIELD_SCALE: bad field'
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
 
      if (msgtype(5) .eq. 1) then
         ipt1=imsgin(5)
         ipt2=imsgin(6)
         ipt3=imsgin(7)
         call pntlimn(ipt1,ipt2,ipt3,ipmpary,mpno,
     *                                nnodes,isetwd,itp1)
      else
         ich1=cmsgin(5)
         ich2=cmsgin(6)
         ich3=cmsgin(7)
         call pntlimc(ich1,ich2,ich3,ipmpary,mpno,
     *                                nnodes,isetwd,itp1)
      endif
 
      write(logmess,'(a)')
     *      'FIELD_SCALE: scale field values.'
      call writloga('default',0,logmess,0,ierrw)
 
      if (mpno.gt.0) then
         write(logmess,'(a,i10)')
     *         'Nodes in point set  = ',mpno
         call writloga('default',0,logmess,0,ierrw)
      else
         write(logmess,'(a)') 'No points to scale!'
         call writloga('default',0,logmess,0,ierrw)
         goto 9999
      endif
c
c     Perform rescaling or obtain scale.
c
      option=cmsgin(3)(1:icharln(cmsgin(3)))
      if (option.eq.'normalize') then
         value=xmsgin(4)
         valmin=field(mpary(1))
         valmax=field(mpary(1))
         do i=2,mpno
            valmin=min(valmin,field(mpary(i)))
            valmax=max(valmax,field(mpary(i)))
         enddo
         valspread=valmax-valmin
         if (valspread.eq.zero) then
            write(logmess,'(a)')
     &         'Cannot normalize field with zero variation.'
            call writloga('default',0,logmess,0,ierrw)
            goto 9999
         endif
 
         write(logmess,'(a,e16.8)')
     &      'FIELD: normalizing field by multiplying field values by',
     &      value/valspread
         call writloga('default',0,logmess,0,ierrw)
 
         do i=1,mpno
            val=field(mpary(i))
            field(mpary(i))=val*value/valspread
         enddo
      elseif (option.eq.'multiply') then
         value=xmsgin(4)
         do i=1,mpno
            val=field(mpary(i))
            field(mpary(i))=val*value
         enddo
      elseif (option.eq.'divide') then
         value=xmsgin(4)
         do i=1,mpno
            val=field(mpary(i))
            field(mpary(i))=val/value
         enddo
      elseif (option.eq.'getmax') then
         cvalue=cmsgin(4)(1:icharln(cmsgin(4)))
         valmax=field(mpary(1))
         do i=2,mpno
            valmax=max(valmax,field(mpary(i)))
         enddo
         iout=0
         cout= ' '
         call cmo_set_attinfo(cvalue,cmo,iout,
     *      valmax,cout,2,icscode)
         if (icscode .ne. 0) call x3d_error(isubname,'cmo_set')
      elseif (option.eq.'getmin') then
         cvalue=cmsgin(4)(1:icharln(cmsgin(4)))
         valmin=field(mpary(1))
         do i=2,mpno
            valmin=min(valmin,field(mpary(i)))
         enddo
         call cmo_set_attinfo(cvalue,cmo,iout,
     *      valmin,cout,2,icscode)
         if (icscode .ne. 0) call x3d_error(isubname,'cmo_set')
      elseif (option.eq.'getspread') then
         cvalue=cmsgin(4)(1:icharln(cmsgin(4)))
         valmin=field(mpary(1))
         valmax=field(mpary(1))
         do i=2,mpno
            valmin=min(valmin,field(mpary(i)))
            valmax=max(valmax,field(mpary(i)))
         enddo
         valspread=valmax-valmin
         call cmo_set_attinfo(cvalue,cmo,iout,valspread,cout,
     *          2,icscode)
         if (icscode .ne. 0) call x3d_error(isubname,'set_attinfo')
      else
         write(logmess,'(a)') 'Bad rescaling option.'
         call writloga('default',0,logmess,0,ierrw)
         goto 9999
      endif
 
 9999 continue
      call mmrelprt(isubname,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'mmrelprt')
      return
      end
