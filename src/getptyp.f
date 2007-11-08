*dk,getptyp
      subroutine getptyp(name,iityp,ierror)
C
C
C ######################################################################
C
C     PURPOSE -
C
C        This routine returns the point type for a given name
C
C     INPUT ARGUMENTS -
C
C        name    - the name of the point type
C
C     OUTPUT ARGUMENTS -
C
C        iityp - the point tpye
C        ierror - error return(0=ok, -1=error)
C
C     CHANGE HISTORY -
C
C$Log:   /pvcs.config/t3d/src/getptyp.f_a  $
CPVCS    
CPVCS       Rev 1.2   Mon Apr 14 16:49:56 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.1   Fri May 03 10:46:50 1996   dcg
CPVCS    changes for new point type --virtual interfaces
CPVCS
CPVCS       Rev 1.0   09/20/95 09:52:22   dcg
CPVCS    Initial revision.
      implicit none
 
C
      include 'chydro.h'
C
      character *(*) name
      character*8 isubname
      integer iityp,ierror
C
      isubname='getptyp'
      iityp=-1
      ierror=0
C  interior point
      if (name(1:3).eq.inampint) then
        iityp=ifitpint
C  interface point
      elseif (name(1:3).eq.inampini) then
         iityp=ifitpini
C  virtual interface point
      elseif (name(1:3).eq.inampvrt) then
         iityp=ifitpvrt
C  virtual interface + interaface point
      elseif (name(1:3).eq.inampvin) then
         iityp=ifitpvin
C   Relected external boundary
      elseif (name(1:3).eq.inamprfl) then
         iityp=ifitprfl
C  Free external boundary
      elseif (name(1:3).eq.inampfre) then
         iityp=ifitpfre
C  Interface node on reflected boundary
      elseif (name(1:3).eq.inampirb) then
         iityp=ifitpirb
C  Interface node on free boundary
      elseif (name(1:3).eq.inampifb) then
         iityp=ifitpifb
C  Virtual Interface node on free boundary
      elseif (name(1:3).eq.inampvfb) then
         iityp=ifitpvfb
C  Virtual Interface node on reflective boundary
      elseif (name(1:3).eq.inampvrb) then
         iityp=ifitpvrb
C  Virtual Interface node on reflective and free boundary
      elseif (name(1:3).eq.inampvrf) then
         iityp=ifitpvrf
C  Virtual Interface and interface node on reflective boundary
      elseif (name(1:3).eq.inampvir) then
         iityp=ifitpvir
C  Virtual Interface and interface node on free boundary
      elseif (name(1:3).eq.inampvif) then
         iityp=ifitpvif
C  Virtual Interface and interface node on free reflect boundary
      elseif (name(1:3).eq.inampalb) then
         iityp=ifitpalb
C   Merged node
      elseif (name(1:3).eq.inampmrg) then
         iityp=ifitpmrg
C  Dudded node
      elseif (name(1:3).eq.inampdud) then
         iityp=ifitpdud
C  Parent node
      elseif (name(1:3).eq.inamppar) then
         iityp=ifitpcup
C illegal node type
      else
          ierror=-1
      endif
      return
      end
