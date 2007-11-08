*dk,constrainv
      subroutine constrainv(imsgin,xmsgin,cmsgin,msgtype,nwds,ierror)
C
C
C ######################################################################
C
C     PURPOSE -
C
C      THIS ROUTINE ADDS THE XCONTAB ATTRIBUTE TO THE MESH OBJECT
C      IT THEN CALLS GET_XCONTAB TO CREATE THE 3 BY 3 MATRIX3
C      DEFINED AT EACH NODE THAT WHEN MULTIPLIED BY THE VELOCITY
C      VECTOR WILL CONSTRAIN MOTION TO REMAIN ON A SURFACE OR
C      ON A LINE OR AT A POINT
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
C$Log:   /pvcs.config/t3d/src/constrainv.f_a  $
CPVCS    
CPVCS       Rev 1.1   Mon Apr 14 16:42:24 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.0   Mon Jul 29 15:01:44 1996   dcg
CPVCS    Initial revision.
C
C
C ######################################################################
C
      implicit none
      character*32 cmo
      character*8 cxconnm
      character*132 ibuff
      integer icscode,iconlen,imsgin(*),msgtype(*),nwds,ierror,
     *     ictyp,itensor
      real*8 xmsgin(*)
      character*32 cmsgin(*),coper
      pointer (ipxcontab,xcontab)
      real*8 xcontab(1000000)
      integer icharlnf,iptemp
C
      cxconnm='xcontab'
C
C     CHECK IF THERE ARE ARGUMENTS TO COMMAND
      call cmo_get_name(cmo,icscode)
      if(nwds.gt.1) then
         coper=' '
         coper=cmsgin(2)(1:icharlnf(cmsgin(2)))
         if(coper(1:5).eq.'clear') then
            call mmfindbk(cxconnm,cmo,iptemp,iconlen,icscode)
            if(icscode.eq.0) then
               ibuff='cmo/modatt/-def-/xcontab/persistence/temporary' //
     x               ' ; finish'
               call dotaskx3d(ibuff,icscode)
               call cmo_delatt_cmo(cmo,cxconnm,icscode)
            endif
            go to 9999
         endif
      else
         coper='default'
      endif
C
C     DEFINE CONSTRAINT OPERATORS.
C   SEE IF XCONTAB ATTRIBUTE EXISTS
C
      call mmfindbk('xcontab',cmo,ipxcontab,iconlen,icscode)
      if(icscode.ne.0) then
C
C   IF NOT THERE CREATE
C   ADD NCONMAT VARIABLE TO CMO - LENGTH OF BLOCK
C   ADD ICONTAB TO CMO
C
         call cmo_get_info('tensor',cmo,itensor,iconlen,ictyp,icscode)
         if(icscode.ne.0) then
            ibuff='cmo/addatt//tensor/INT/' //
     *         'scalar/scalar/constant/permanent/x/0' //
     *         ' ; finish'
            call dotaskx3d(ibuff,icscode)
            call cmo_set_info('tensor',cmo,9,1,1,icscode)
         else
            if(itensor.ne.9) then
               call cmo_set_info('tensor',cmo,9,1,1,icscode)
            endif
         endif
         ibuff='cmo/addatt//xcontab/VDOUBLE/' //
     *         'tensor/nnodes/constant/permanent/x/0.0' //
     *         ' ; finish'
         call dotaskx3d(ibuff,icscode)
         call mmfindbk('xcontab',cmo,ipxcontab,iconlen,icscode)
      endif
C
C  CALL GET_XCONTAB TO CREATE 3X3 CONSTRAINT MATRIX
C
      call get_xcontab(coper,cmo,ipxcontab)
C
 9999 continue
      return
      end
