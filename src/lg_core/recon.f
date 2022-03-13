*dk,recon
      subroutine recon(imsgin,xmsgin,cmsgin,msgtype,nwds,ierror)
C
C
C #####################################################################
C
C     PURPOSE -
C
C        This routine call the correct grid reconnection routine
C           depending on the type and dimensionality of the CMO.
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
C        $Log: recon.f,v $
C        Revision 2.00  2007/11/09 20:04:00  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.9   24 Apr 2003 17:12:48   dcg
CPVCS    add check for ivornoi==5 which means recon has been disabled
CPVCS
CPVCS       Rev 1.8   15 May 2001 15:07:42   kuprat
CPVCS    We now compute TOLDAMAGE if user has not provided it, and
CPVCS    we pass it on to RECONLOOP2D and RECONLOOP3D.  We accept
CPVCS    the key word CHECKAXY and, if provided, pass a corresponding
CPVCS    flag on to RECONLOOP2D so that new triangles formed will
CPVCS    have an xy-projected area greater than EPSILONA.
CPVCS
CPVCS       Rev 1.7   12 Jun 2000 08:37:14   jan
CPVCS    changed line 230 from idum= ' ' to idum=0
CPVCS
CPVCS       Rev 1.6   Thu Apr 06 13:44:00 2000   dcg
CPVCS    replace get_info_i call
CPVCS
CPVCS       Rev 1.5   Wed Feb 03 15:24:02 1999   dcg
CPVCS    remove calls to fluxing routines and associated memory.
CPVCS
CPVCS       Rev 1.4   Fri Apr 17 15:35:28 1998   dcg
CPVCS    separate recon routines
CPVCS
CPVCS       Rev 1.61   Thu Apr 16 12:44:12 1998   dcg
CPVCS    add code to do 4to4 interface and volume flips if
CPVCS    ivoronoi = 2 or -2
CPVCS
CPVCS       Rev 1.59   Fri Feb 13 13:37:58 1998   kuprat
CPVCS    Deleted a "control C" that somehow got into the file.
CPVCS
CPVCS       Rev 1.58   Fri Feb 13 13:34:46 1998   kuprat
CPVCS    Commented out section writing to file called reconlist.
CPVCS
CPVCS       Rev 1.57   Mon Dec 01 16:41:22 1997   dcg
CPVCS    remove ipid
CPVCS    declare ipiholes as a pointer
CPVCS
CPVCS       Rev 1.56   Thu Oct 16 14:31:58 1997   kuprat
CPVCS    RECON format is now
CPVCS    recon/{0,1}/toldamage
CPVCS    2to2r flips causing boundary 'damage' exceeding TOLDAMAGE
CPVCS    are now prohibited.
CPVCS
CPVCS       Rev 1.55   Thu Oct 09 09:52:24 1997   dcg
CPVCS    fix memory management calls for merglst and merglst2
CPVCS
CPVCS       Rev 1.54   Wed Oct 08 16:55:58 1997   dcg
CPVCS    add error flag argument to one of the flip calls
CPVCS
CPVCS       Rev 1.53   Wed Oct 08 14:37:24 1997   dcg
CPVCS    fix typo on format statement
CPVCS
CPVCS       Rev 1.52   Wed Oct 08 13:55:20 1997   dcg
CPVCS    replace print statements
CPVCS
CPVCS       Rev 1.51   Wed Oct 01 14:40:14 1997   dcg
CPVCS    use mmfindbk with merge temp storage
CPVCS
CPVCS       Rev 1.50   Mon Sep 29 15:01:42 1997   dcg
CPVCS    call cmo_newlen at end of reconloop3d not at end of recon2
CPVCS
CPVCS       Rev 1.49   Tue Sep 16 16:04:58 1997   dcg
CPVCS    release merglst block
CPVCS
CPVCS       Rev 1.48   Wed Jul 09 09:12:34 1997   dcg
CPVCS    recheck insphere test at each iteration
CPVCS    flips from previous iterations may change results of insphere test
CPVCS    improve diagnostics if idebug is set
CPVCS
CPVCS       Rev 1.47   Fri Jun 06 11:27:00 1997   dcg
CPVCS    fix memory problems with kfix and xfix
CPVCS
CPVCS       Rev 1.46   Mon Jun 02 16:02:56 1997   dcg
CPVCS    change test to increment kfix allocation to account
CPVCS    for possible increase in ntets of 100 during next
CPVCS    iteration
CPVCS
CPVCS       Rev 1.45   Fri May 16 10:31:04 1997   kuprat
CPVCS    Added guard on 'N5' calculation.
CPVCS
CPVCS       Rev 1.44   Wed May 14 12:44:26 1997   dcg
CPVCS    fix memory problem with kfix overwritten if
CPVCS    3 to 2 flips increase ntets
CPVCS    fix output line to make more sense
CPVCS
CPVCS       Rev 1.43   Mon May 05 17:18:50 1997   dcg
CPVCS    resolve temporary storage conflicts between
CPVCS    recon and merge
CPVCS
CPVCS       Rev 1.42   Thu Apr 17 16:04:46 1997   dcg
CPVCS    set nelements by calling cmo_set_info before
CPVCS    call to tettestd
CPVCS
CPVCS       Rev 1.0   Thu Apr 17 16:03:32 1997   dcg
CPVCS    Initial revision.
CPVCS
CPVCS       Rev 1.41   Mon Apr 14 16:58:22 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.40   Wed Apr 02 11:38:02 1997   dcg
CPVCS    change default 3d recon to recon/0
CPVCS
CPVCS       Rev 1.39   Tue Feb 04 03:30:30 1997   het
CPVCS    Add the neibor.h include file to reconloop3d.
CPVCS
CPVCS       Rev 1.38   Fri Jan 24 13:39:06 1997   het
CPVCS    Change the matrecon()=1 values and put unreconnected connections
CPVCS       into a file called reconlist.
CPVCS
CPVCS       Rev 1.37   Tue Dec 03 13:18:10 1996   het
CPVCS    Write out a list of unreconnect faces to a file.
CPVCS
CPVCS       Rev 1.36   Tue Nov 12 09:54:40 1996   dcg
CPVCS    test for ivoronoi =2 to avoid loop to fix
CPVCS    negative coupling coefficients
CPVCS
CPVCS       Rev 1.35   Thu Oct 17 16:12:14 1996   dcg
CPVCS
CPVCS
CPVCS
CPVCS
CPVCS    add ivoronoi=-2 option to set igeom_gwt to 1
CPVCS
CPVCS       Rev 1.34   Mon Jul 15 12:37:46 1996   dcg
CPVCS    fix call to mmfindbk
CPVCS
CPVCS       Rev 1.33   Thu Jun 27 15:26:56 1996   dcg
CPVCS    add constrainv command to create xcontab table as needed
CPVCS
CPVCS       Rev 1.32   Wed May 22 10:16:30 1996   dcg
CPVCS    get nconbnd from mesh object
CPVCS
CPVCS       Rev 1.31   Tue May 07 17:23:52 1996   het
CPVCS    Add the GWT reconnection algorithm.
CPVCS
CPVCS       Rev 1.30   Tue Apr 16 14:36:38 1996   dcg
CPVCS    try2to2b.f try2to4r.f try4to4x.f trymtonr.f
CPVCS    replace pointer ipitets with array itets
CPVCS
CPVCS       Rev 1.29   Tue Apr 02 02:29:28 1996   het
CPVCS    Fix an error the jtet1(jtet1)=something.
CPVCS
CPVCS       Rev 1.28   Sat Mar 09 10:29:00 1996   dcg
CPVCS    set t1,t2 to zero note timing not implemented
CPVCS
CPVCS       Rev 1.27   Tue Mar 05 12:37:46 1996   het
CPVCS    Move the (cmo)cmo_get_info calles from the driver into this routine.
CPVCS
CPVCS       Rev 1.26   Tue Feb 13 14:56:30 1996   het
CPVCS    Correct an error with setting interface mbndry to the external mbndry flag.
CPVCS
CPVCS       Rev 1.25   Fri Feb 02 14:23:20 1996   dcg
CPVCS    remove references to explicit vector attributes (u,w,v,e,r,pic)
CPVCS
CPVCS       Rev 1.24   Fri Dec 22 14:11:08 1995   het
CPVCS    Extract the iopt2to2 variable from the dictionary.
CPVCS
CPVCS       Rev 1.23   11/17/95 15:55:04   dcg
CPVCS    fix set_info_i calls
CPVCS
CPVCS       Rev 1.22   11/16/95 15:22:12   dcg
CPVCS    replace character literals in calls
CPVCS
CPVCS       Rev 1.21   11/07/95 17:23:36   dcg
CPVCS    change flag to 2 in mmgetblk calls
CPVCS
CPVCS       Rev 1.20   11/07/95 11:28:50   het
CPVCS    Modify the 2D triangle refinement algorithms.
CPVCS
CPVCS       Rev 1.19   10/22/95 13:18:24   het
CPVCS    Correct a memory management error
CPVCS
CPVCS       Rev 1.18   10/20/95 15:49:30   dcg
CPVCS    disable debug print
CPVCS
CPVCS       Rev 1.17   09/29/95 09:13:30   het
CPVCS    Put in added attributes inheritance
CPVCS
CPVCS       Rev 1.16   08/30/95 21:09:12   het
CPVCS    Put cmo table data into the cmoatt storage block
CPVCS
CPVCS       Rev 1.15   08/29/95 12:02:52   dcg
CPVCS    set length for names to 40 characters
CPVCS
CPVCS       Rev 1.14   08/16/95 00:23:26   het
CPVCS    Correct errors in recon and refine calles
CPVCS
CPVCS       Rev 1.13   08/15/95 18:23:50   het
CPVCS    Cleanup code and correct errors
C
c
c.... Format:
c....         RECON/[0,1]/[toldamage]/[CHECKAXY]
c....
c.... 1=Reconnection WITH edge refinement if
c....   necessary to eliminate negative coupling coefficients.
c.... 0=We only do reconnections, leaving any negative
c....   coupling coeffients that may still exist afterwards.
c.... TOLDAMAGE= maximum normal deformation allowed by reconnection.
c.... CHECKAXY:  If .true., then for the case of 2-D triangular
c....            meshes, we check xy- projected areas are positive
c....            and larger than EPSILONA.
C ######################################################################
C
      implicit none
C
      include "local_element.h"
      include "chydro.h"
      include "neibor.h"
      include "consts.h"

C arguments
      integer nwds, imsgin(nwds), msgtype(nwds)
      real*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)

C variables
      pointer (ipimt1, imt1)
      pointer (ipitp1, itp1)
      integer imt1(*), itp1(*)

      pointer (ipitet, itet )
      integer itet(*)

      pointer (ipitlist, itlist)
      integer itlist(*)

      pointer (iptable2, table2 )
      integer table2(*)

      pointer (ipxcontab, xcontab )
      real*8 xcontab(*)

      integer ierror,idum,ierrwrt,ijob,ilen,itype,icscode,nen,nef,nsd
     &   ,nelements,length,icmotype,ier,nelements_save,npoints,l,i
     &   ,nconbnd,ntets,it,lenfix,ntlist,i1,ics,lenout,icharlnf

      real*8 toldamage,xmin,xmax,ymin,ymax,zmin,zmax,epsilona,epsilonv

      logical lcheckaxy
 
      character*132 logmess
      character*32 isubname,contblnm
      character*32 cmo
      character*8 cdum
C
C#######################################################################
C BEGIN begin
C
      isubname="recon"
      contblnm='contable'
      cdum='-notset-'
      idum=0
C
      call cmo_get_name(cmo,ierror)
      if(ierror.ne.0) then
         write(logmess,9000)
 9000    format('No CMOs defined')
         call writloga('default',1,logmess,1,ierrwrt)
         goto 9999
      endif
      if(nwds.ge.2) then
         ijob=imsgin(2)
      else
         ijob=0
      endif

C     check for 0 elements
      call cmo_get_info('nelements',cmo,ntets,length,icmotype,ierror)
      if (ntets .le. 0) then
         write(logmess,'(a)')
     *   'WARNING Recon Early Exit: 0 elements'
         call writloga('default',1,logmess,1,ierror)
         ierror = -1
         goto 9999
      endif

C
c.... If provided, get damage tolerance from argument list.
c.... Otherwise, we get the dimensions of mesh object (using setsize and getsize)
c.... and we set TOLDAMAGE equal to .01 times the diameter of the
c.... mesh object.
 
      call setsize()
      call getsize(xmin,xmax,ymin,ymax,zmin,zmax,epsilona,epsilonv)
      if (msgtype(3).gt.0.and.cmsgin(3)(1:5).ne.'-def-') then
         call test_argument_type(1,2,3,imsgin,xmsgin,cmsgin,msgtype,
     *      nwds)
         toldamage=xmsgin(3)
      else
         toldamage=.01*sqrt((xmax-xmin)**2+(ymax-ymin)**2+
     &      (zmax-zmin)**2)
      endif
 
c.... If CHECKAXY is given, we make sure the projected areas of the
c.... triangles (in the case of 2-D flipping) are positive and
c.... greater than EPSILONA.
 
      lcheckaxy=.false.
      if (msgtype(4).gt.0.and.cmsgin(4)(1:5).ne.'-def-') then
         call test_argument_type(1,3,4,imsgin,xmsgin,cmsgin,msgtype,
     *      nwds)
         if (cmsgin(4)(1:8).eq.'checkaxy') then
            lcheckaxy=.true.
         endif
      endif
 
      call cmo_get_info('ivoronoi',cmo,
     *                ivoronoi,ilen,itype,icscode)
         if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
      if(ivoronoi.eq.5) go to 9999
      if(abs(ivoronoi).eq.2) then
         call mega_hessian()
         call mega_error()
      endif
      if(ivoronoi.eq.2.and.ijob.eq.1) ijob=0
C
      call cmo_get_info('nodes_per_element',cmo,nen,ilen,itype,icscode)
      call cmo_get_info('faces_per_element',cmo,nef,ilen,itype,icscode)
      call cmo_get_info('ndimensions_topo',cmo,nsd,ilen,itype,icscode)
C
      call cmo_get_info('nelements',cmo,nelements,length,icmotype,ier)
      nelements_save=nelements
C
      if(nen.eq.nelmnen(ifelmtri).and.nef.eq.nelmnef(ifelmtri)) then
         call reconloop2d(ijob,toldamage,lcheckaxy,epsilona)

      elseif(nen.eq.nelmnen(ifelmtet).and.nef.eq.nelmnef(ifelmtet).and.
     *       nsd.eq.3) then
C
         call cmo_get_name(cmo,ierror)
         call cmo_get_info('nnodes',cmo,npoints,l,i,ierror)
         call cmo_get_info('nconbnd',cmo,nconbnd,l,i,ierror)
         call cmo_get_info('nelements',cmo,ntets,l,i,ierror)
         call cmo_get_info('imt1',cmo,ipimt1,l,i,ierror)
         call cmo_get_info('itp1',cmo,ipitp1,l,i,ierror)
         call cmo_get_info('itet',cmo,ipitet,l,i,ierror)
C  set up temporary storage
C  set 1000 as min length to avoid single elements lacking memory
         l=max(l,4*ntets)
         length=max(1000,(l-1)/4+1)
         call mmgetblk("ifittet",isubname,ipifittet,length,2,icscode)
         do it=1,ntets
            ifittet(it)=0
         enddo
         length=max(1000,nint(1.7*4*length))
         call mmfindbk('kfix',isubname,ipkfix,lenfix,icscode)
         if(icscode.ne.0) then
            call mmgetblk('kfix',isubname,ipkfix,length,1,icscode)
         else
            if(lenfix.lt.length)
     *      call mmnewlen('kfix',isubname,ipkfix,length,icscode)
         endif
         call mmfindbk('xfix',isubname,ipxfix,lenfix,icscode)
         if(icscode.ne.0) then
            call mmgetblk('xfix',isubname,ipxfix,length,2,icscode)
         else
            if(lenfix.lt.length)
     *      call mmnewlen('xfix',isubname,ipxfix,length,icscode)
         endif
         do i=1,4*ntets
            kfix1(i)=0
         enddo
         ntlist=max(ntets,(l-1)/4+1)
         length=ntlist
         call mmgetblk("itlist",isubname,ipitlist,length,2,icscode)
         do it=1,ntets
            itlist(it)=it
         enddo
         do i=1,ntlist
            kfix1(i)=itlist(i)
         enddo
C
         length=6*npoints
         call mmgetblk('iinter',isubname,ipinter,length,2,icscode)
         ninter=0
         do i1=1,npoints
            if(itp1(i1).eq.ifitpcup) then
               ninter=ninter+1
               iinter(1,ninter)=i1
            endif
         enddo
C
         nmats=0
         do i1=1,npoints
            nmats=max(nmats,imt1(i1))
         enddo
         length=nmats
         call mmgetblk('matrecon',isubname,ipmatrecon,length,2,ics)
         do i=1,nmats
            matrecon(i)=1
         enddo
         call initnbr()
         if (nconbnd .gt. 0) then
            call mmfindbk('mtable2',contblnm,iptable2,lenout,ics)
            if(icscode.ne.0) call contable()
         endif
C
         call cmo_get_name(cmo,ierror)
C
C
         call reconloop3d(ijob,toldamage)
C
         call mmrelblk("itlist",isubname,ipitlist,icscode)
         call mmrelblk("ifittet",isubname,ipifittet,icscode)
         call mmrelblk("kfix",isubname,ipkfix,icscode)
         call mmrelblk("xfix",isubname,ipxfix,icscode)
         call mmrelblk('iinter',isubname,ipinter,icscode)
         call mmrelblk('matrecon',isubname,ipmatrecon,icscode)
         if (nconbnd .gt. 0) then
            call mmrelblk('mtable2',contblnm,iptable2,icscode)
         endif
      else
         write(logmess,9010) cmo(1:icharlnf(cmo))
 9010    format('Refine on this CMO type is not implemented: ',a)
         call writloga('default',1,logmess,1,ierrwrt)
         goto 9999
      endif
C
      goto 9999
 9999 continue
C
C      CLEAR XCONTAB IF IT EXISTS
      call mmfindbk ('xcontab',cmo,ipxcontab,lenout,ier)
      if(ier.eq.0) call dotaskx3d('constrainv/clear ; finish',ier)
c
c  clear flip memory
c
      call mflip(99,idum,cdum)
C
      write(logmess,'(a)') 'RECON: done.'
      call writloga('default',0,logmess,0,ier)

      return
      end
