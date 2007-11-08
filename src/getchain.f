*dk,getchain
      subroutine getchain(kpt,ichain,imatl,nmulti,ict1,ipar1)
       implicit real*8 (a-h,o-z)
C
C#######################################################################
C
C     PURPOSE -
C
C        This routine loops through the coupled chain of the interface
C        point KPT to assimilate the coupled points and their material
C        types.  The list of points does not include the parent point
C        nor the point KPT.
C
C     INPUT ARGUMENTS -
C
C        kpt     - the first pointer into the coupled chain
C        ichain  - the array in which to store the point list
C        imatl   - the array in which to store the material types
C        nmulti  - the dimension of the ichain and imatl arrays
C
C     OUTPUT ARGUMENTS -
C
C        ichain  - contains the coupled points not including the
C                  parent point nor the point KPT
C        ict1    - the number of points returned in the lists
C        ipar1   - the parent point
C
C     HISTORY -
C
C        $Log:   /pvcs.config/t3d/src/getchain.f_a  $
CPVCS    
CPVCS       Rev 1.8   29 Jul 2005 07:33:10   gable
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.7   Mon Apr 14 16:49:42 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.6   Wed Apr 02 13:45:38 1997   dcg
CPVCS    look for zero terminated parent/child chain
CPVCS    
CPVCS       Rev 1.5   Fri Jan 24 13:47:14 1997   het
CPVCS    Fix an error to stop infinite loop parent/child stuff.
CPVCS    
CPVCS       Rev 1.4   Tue Oct 01 08:54:56 1996   dcg
CPVCS    declare logmess to be typ character
CPVCS
CPVCS       Rev 1.3   12/02/94 15:06:12   het
CPVCS    Added an option for the "cmo" access functions
CPVCS
CPVCS
CPVCS       Rev 1.2   12/01/94 18:48:20   het
CPVCS    Added a data type to the "cmo" calles
CPVCS       and added the "cmo.h" include file.
C
C#######################################################################
C
C
C ######################################################################
C
      include "cmo.h"
      include "chydro.h"
      include "neibor.h"
C
C#######################################################################
C
      dimension ichain(nmulti),imatl(nmulti)
      character*132 logmess
C
C ######################################################################
C
C
C
C     ******************************************************************
C     FETCH MESH OBJECT NAME AND POINTER INFORMATION.
C
      if(icmoget.eq.1) then
C
      call cmo_get_name(cmo,ierror)
C
      call cmo_get_info('imt1',cmo,ipimt1,lenimt1,icmotype,ierror)
      call cmo_get_info('itp1',cmo,ipitp1,lenitp1,icmotype,ierror)
      call cmo_get_info('isn1',cmo,ipisn1,lenisn1,icmotype,ierror)
C
      endif
C
C     ******************************************************************
C
C
      ict=0
      ict1=0
      isnx=-1
      mpnt1=kpt
 10   continue
         ict=ict+1
         if(ict.ge.nmulti) then
            write (logmess,3333) kpt, isnx
 3333       format(1x,'ICT.ge.NMULTI: STOP IN GETCHAIN: 1st point=',
     *             i10,' last point=',i10)
            call writloga("default",0,logmess,0,ierrwrt)
         endif
         if(ict.ge.nmulti) call termcode(1)
         isnx=isn1(mpnt1)
C  
C  if there is no link in the chain - it is probable that
C  settets has not been called
C  return parent point = inquiry point and length of chain = 0
C         
         if (isnx.eq.0) then
            write(logmess,3332) kpt,mpnt1
            call writloga('default',0,logmess,0,ierrwrt)
3332        format (' zero terminated parent/child chain ',
     *              ' first point ',i10, ' last point ',i10)
            ipar1=kpt
            ict1=0
            go to 9999
         endif 
         mpnt1=isnx
C
C  test if this is the parent point
C         
         if(itp1(isnx).eq.ifitpcup) then
            ipar1=isnx
            goto 10
C
C  add this point to the chain
C  print a warning if an point in the chain has been dudded or 
C  merged ( this should signify an error)
C
         elseif(isnx.ne.kpt) then
            imtx=imt1(isnx)
            ict1=ict1+1
            ichain(ict1)=isnx
            imatl(ict1)=imtx
            if(itp1(isnx).ge.ifitpmrg) then        
               write (logmess,3334) isnx
               call writloga("default",0,logmess,0,ierrwrt)
 3334          format(1x,'ITP(ISNX).GE.IFITPMRG:IN GETCHAIN: point=',
     *             i10)
            endif
            goto 10
         endif
      goto 9999
 9999 continue
      end
