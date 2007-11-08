*dk,getdof
      subroutine getdof(ioption,cmo,ipidof1,icscode)
C
C
C#######################################################################
C
C      PURPOSE -
C
C         THIS ROUTINE DETERMINES THE DEGREE-OF-FREEDOM FOR EACH NODE
C              OF A CMO. (dof1=0 ==> Point constraint
C                         dof1=1 ==> Line constraint
C                         dof1=2 ==> Plane constraint
C                         dof1=3 ==> No constraint)
C
C      INPUT ARGUMENTS -
C
C         ioption - (character) The Option.
C         cmo     - Current-Mesh_Object (CMO) name.
C
C      OUTPUT ARGUMENTS -
C
C         ipidof1 - (Integer) Pointer to the DOFs.
C         icscode - (integer) Error flag.
C
C      CHANGE HISTORY -
C
C        $Log:   /pvcs.config/t3d/src/getdof.f_a  $
CPVCS    
CPVCS       Rev 1.5   Mon Apr 14 16:49:44 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.4   Wed May 22 10:15:40 1996   dcg
CPVCS    get nconbnd from mesh object
CPVCS
CPVCS       Rev 1.3   Thu May 16 10:26:22 1996   dcg
CPVCS     changes for new interface type 3 and for new icontab, xcontab
CPVCS
CPVCS       Rev 1.2   Mon May 06 12:28:32 1996   dcg
CPVCS    use itsttp to test for point types
CPVCS
CPVCS       Rev 1.1   09/06/95 04:49:34   het
CPVCS    Add the correct dof1 values for the smooth option
CPVCS
CPVCS       Rev 1.0   07/17/95 16:24:04   dcg
CPVCS    Initial revision.
C
C#######################################################################
C
      implicit none
C
C#######################################################################
C
      include "chydro.h"
C
C#######################################################################
C
      character*(*) ioption, cmo
C
      pointer (ipidof1, idof1 )
      integer idof1(1000000)
C
      integer icscode
C
C#######################################################################
C
      integer i1, len1, itp, idof, ilen, itype, nconbnd
      character*32 isubname
C
      integer nnodes
C
      pointer (ipitp1, itp1 )
      integer itp1(1000000)
      pointer (ipicr1, icr1 )
      integer icr1(1000000)
      pointer (ipicontab,icontab)
      integer icontab(50,1000000)
C
      logical itsttp
C
C#######################################################################
C
      integer icharlnf
C
C#######################################################################
C
C
      isubname='setdof'
C
      icscode = -1
C
      call cmo_get_info('nnodes',cmo,nnodes,ilen,itype,icscode)
         if(icscode.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('nconbnd',cmo,nconbnd,ilen,itype,icscode)
         if(icscode.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('itp1',cmo,ipitp1,ilen,itype,icscode)
         if(icscode.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('icr1',cmo,ipicr1,ilen,itype,icscode)
         if(icscode.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('icontab',cmo,ipicontab,ilen,itype,icscode)
         if(icscode.ne.0) call x3d_error(isubname,'cmo_get_info')
C
      do i1=1,nnodes
         idof1(i1)=3
      enddo
C
      len1=icharlnf(ioption)
C
      if (ioption(1:len1) .eq. 'cfd') then
         do i1=1,nnodes
            idof=3
            itp=itp1(i1)
            if (itp.ge.ifitpst1.and.itp.le.ifitpen1) then
                if(itp.eq.ifitpint) then
                   idof=3
                elseif(itp.eq.ifitpini) then
                   idof=2
                else
                   idof=3
                endif
            elseif (itp.ge.ifitpst2.and.itp.le.ifitpen2) then
               if(nconbnd.le.0) then
                  idof=2
               elseif(icontab(2,icr1(i1)).eq.0) then
                  idof=0
               elseif(icontab(2,icr1(i1)).eq.1) then
                  idof=1
               elseif(icontab(2,icr1(i1)).eq.2) then
                  idof=2
               else
                  idof=2
               endif
            endif
            idof1(i1)=idof
         enddo
 
      elseif (ioption(1:len1) .eq. 'smooth') then
C
C****    Determine the smoothing degree-of-freedom (dof) for each point.
C
         do i1=1,nnodes
            idof=3
            itp=itp1(i1)
            if (itp.ge.ifitpst1.and.itp.le.ifitpen1) then
                if(itp.eq.ifitpint) then
                   idof=3
                elseif(itp.eq.ifitpini.or.
     *                itp.eq.ifitpvrt.or.itp.eq.ifitpvin) then
                   idof=2
                else
                   idof=3
                endif
            elseif (itp.ge.ifitpst2.and.itp.le.ifitpen2) then
               if (itsttp('intrface',itp)) then
                  if(icr1(i1).eq.0) then
                     idof=2
                  else
                     if(nconbnd.le.0) then
                        idof=2
                     elseif(icontab(2,icr1(i1)).eq.0) then
                        idof=0
                     elseif(icontab(2,icr1(i1)).eq.1) then
                        idof=0
                     elseif(icontab(2,icr1(i1)).eq.2) then
                        idof=1
                     else
                        idof=2
                     endif
                  endif
               else
                  if(nconbnd.le.0) then
                     idof=2
                  elseif(icontab(2,icr1(i1)).eq.0) then
                     idof=0
                  elseif(icontab(2,icr1(i1)).eq.1) then
                     idof=1
                  elseif(icontab(2,icr1(i1)).eq.2) then
                     idof=2
                  else
                     idof=2
                  endif
               endif
            endif
            idof1(i1)=idof
         enddo
 
      endif
C
      goto 9999
 9999 continue
C
      return
      end
