*dk,refine_face_add
      subroutine refine_face_add(cmo_name,
     *                           nadd,
     *                           ipitadd,ipifadd,
     *                           ipiadd,ipxadd,ipyadd,ipzadd)
C
C
C #####################################################################
C
C     PURPOSE -
C
C        This routine calls the correct grid refinement routine
C           depending on the type and dimensionality of the CMO.
C
C     INPUT ARGUMENTS -
C
C        cmo_name - The name of the CMO.
C        nadd     - Number of nodes to add.
C        (ip)itadd  - Integer pointer to the list of elements where
C                        nodes will be added.
C        (ip)itadd  - Integer pointer to the list of local faces where
C                        nodes will be added.
C        (ip)iadd(nadd) - The "names" of the nodes to add.
C        (ip)xadd(nadd) - The X-coordinate of the nodes to add.
C        (ip)yadd(nadd) - The Y-coordinate of the nodes to add.
C        (ip)zadd(nadd) - The Z-coordinate of the nodes to add.
C
C     OUTPUT ARGUMENTS -
C
C     CHANGE HISTORY -
C
C        $Log: refine_face_add.f,v $
C        Revision 2.00  2007/11/09 20:04:01  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.44   30 Oct 2006 14:06:38   gable
CPVCS    Put commented out lines back in so that refine/addpts adds points
CPVCS    at the specified location when points land on a face (edge in 2d).
CPVCS    Add debugger IO options. 
CPVCS    
CPVCS       Rev 1.43   15 May 2001 10:56:14   kuprat
CPVCS    We commented out lines that overwrote any possible
CPVCS    nonlinear (user) interpolation of the x, y, or z values.
CPVCS    
CPVCS       Rev 1.42   19 Oct 2000 16:28:20   dcg
CPVCS    use itet not itetnn to set points for next pass - this gets
CPVCS    children not parents
CPVCS    set new point type and icr values with correct index for
CPVCS    2d cases
CPVCS
CPVCS       Rev 1.41   27 Sep 2000 16:19:20   dcg
CPVCS    skip call to settets in refine_face_add_tri if skipsettets /= 0
CPVCS
CPVCS       Rev 1.40   05 May 2000 15:39:00   dcg
CPVCS     refresh mbndry value after cmo_newlen
CPVCS
CPVCS       Rev 1.39   Wed Sep 01 14:38:58 1999   dcg
CPVCS    always set isn1 of new nodes to zero
CPVCS
CPVCS       Rev 1.38   Fri Jul 23 10:02:42 1999   dcg
CPVCS    fix arguments to refine_face_add_tet to match call
CPVCS
CPVCS       Rev 1.37   Wed Jun 16 10:17:26 1999   nnc
CPVCS    Fixed missing and multiple variable declarations.
CPVCS
CPVCS       Rev 1.36   Mon Jun 14 17:35:24 1999   dcg
CPVCS    change to implicit none
CPVCS    add call to refine_fix_add to set itp and icr values correctly
CPVCS
CPVCS       Rev 1.35   Fri Jan 22 15:48:06 1999   dcg
CPVCS    fix type ifitpvfr to ifitpvrf
CPVCS
CPVCS       Rev 1.34   Mon Nov 03 14:21:36 1997   dcg
CPVCS    use correct subroutine name in isubname= statements
CPVCS
CPVCS       Rev 1.33   Wed Oct 08 16:54:44 1997   dcg
CPVCS    fix number of arguments in calls to x3d_error
CPVCS
CPVCS       Rev 1.32   Wed Oct 08 15:44:40 1997   dcg
CPVCS    add logical declarations
CPVCS
CPVCS       Rev 1.31   Wed Sep 24 15:10:58 1997   dcg
CPVCS    refresh pointers correctly - change mmgetblk to correct lengths
CPVCS
CPVCS       Rev 1.28   Wed Jul 16 09:59:24 1997   dcg
CPVCS    add code to set point types and constraint values for
CPVCS    points added as a result of the refinement
CPVCS
CPVCS       Rev 1.27   Wed May 07 14:19:46 1997   dcg
CPVCS    fix previous bad fix
CPVCS
CPVCS       Rev 1.26   Wed May 07 10:57:20 1997   dcg
CPVCS    check for mbndry values in jtet when refreshing
CPVCS    face lists
CPVCS
CPVCS       Rev 1.25   Mon Apr 28 14:17:40 1997   dcg
CPVCS    pick up correct child point for triangle face refinement
CPVCS
CPVCS       Rev 1.24   Mon Apr 14 16:59:30 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.23   Thu Mar 13 17:36:02 1997   dcg
CPVCS    add code to check which element face a point to be
CPVCS    added is in.  Needed when element has been modified
CPVCS    by a previously added point.
CPVCS
CPVCS       Rev 1.22   Thu Mar 13 11:00:40 1997   dcg
CPVCS    update information correctly for subsequent interaction
CPVCS    end point of edges must be set from new refined element
CPVCS    change loop on 4 faces to pick up number of faces from cmo.
CPVCS
CPVCS       Rev 1.21   Fri Jan 24 14:28:04 1997   het
CPVCS    Add new children points if necessary.
CPVCS
CPVCS       Rev 1.20   Wed Nov 06 16:46:52 1996   dcg
CPVCS    fix typo - nefnef should be nef1
CPVCS
CPVCS       Rev 1.19   Wed Jul 24 17:34:32 1996   dcg
CPVCS    use mesh object 'nef' attribute to pack element and
CPVCS    face number into jtet array
CPVCS
CPVCS       Rev 1.18   Thu Jun 27 14:56:28 1996   het
CPVCS    For addpts use the names of points without duplicating the points.
CPVCS
CPVCS       Rev 1.17   Tue Apr 02 02:28:42 1996   het
CPVCS    Change this routine to give new nodes names.
CPVCS
CPVCS       Rev 1.16   Thu Mar 14 13:39:12 1996   het
CPVCS    Change the call to the refine commands to add names.
CPVCS
CPVCS       Rev 1.15   Wed Jan 31 12:58:54 1996   het
CPVCS    Correct an error with creating negative volume triangles
CPVCS
CPVCS       Rev 1.14   Fri Dec 22 14:14:20 1995   het
CPVCS    Fix an error with element colors.
CPVCS
CPVCS       Rev 1.13   11/16/95 17:12:58   het
CPVCS    Start to add all the functions for refine.
CPVCS
CPVCS       Rev 1.10   11/07/95 17:24:48   dcg
CPVCS    change flag to 2 in mmgetblk calls
CPVCS
CPVCS       Rev 1.9   11/07/95 11:28:18   het
CPVCS    Modify the 2D triangle refinement algorithms.
C
C ######################################################################
C
      implicit none
C
      character*132 logmess
C
      include "local_element.h"
C
C ######################################################################
C
C
      character*(*) cmo_name
      integer nadd
      pointer (ipitadd, itadd)
      integer itadd(nadd)
      pointer (ipifadd, ifadd)
      integer ifadd(nadd)
      pointer (ipiadd, iadd)
      integer iadd(1000000)
      pointer (ipxadd, xadd)
      pointer (ipyadd, yadd)
      pointer (ipzadd, zadd)
      real*8 xadd(nadd), yadd(nadd), zadd(nadd)
      integer ierror,icharlnf,ierrwrt,nen,ilen,itype,icscode,nef,
     *  nsd
C
C#######################################################################
C
C
      call cmo_exist(cmo_name,ierror)
      if(ierror.ne.0) then
         write(logmess,9000) cmo_name(1:icharlnf(cmo_name))
 9000    format('CMO does not exist: ',a)
         call writloga('default',1,logmess,1,ierrwrt)
         goto 9999
      endif
C
      call cmo_get_intinfo('nodes_per_element',cmo_name,
     *                  nen,ilen,itype,icscode)
      call cmo_get_intinfo('faces_per_element',cmo_name,
     *                  nef,ilen,itype,icscode)
      call cmo_get_intinfo('ndimensions_topo',cmo_name,
     *                  nsd,ilen,itype,icscode)
C
      if(nen.eq.nelmnen(ifelmtri).and.nef.eq.nelmnef(ifelmtri)) then
         call refine_face_add_tri(cmo_name,
     *                            nadd,
     *                            ipitadd,ipifadd,
     *                            ipiadd,xadd,yadd,zadd)
      elseif(nen.eq.nelmnen(ifelmtet).and.nef.eq.nelmnef(ifelmtet).and.
     *       nsd.eq.3) then
         call refine_face_add_tet(cmo_name,
     *                            nadd,
     *                            ipitadd,ipifadd,
     *                            ipiadd,xadd,yadd,zadd)
      else
         write(logmess,9010) cmo_name(1:icharlnf(cmo_name))
 9010    format('Refine on this CMO type is not implemented: ',a)
         call writloga('default',1,logmess,1,ierrwrt)
         goto 9999
      endif
C
      goto 9999
 9999 continue
      return
      end
*dk,refine_face_add_tri
      subroutine refine_face_add_tri(cmo_name,
     *                               nadd,
     *                               ipitadd,ipifadd,
     *                               ipiadd,xadd,yadd,zadd)
C
C #####################################################################
C
C     PURPOSE -
C
C        This routine refines triangle faces (edges) by adding
C        the points listed to the listed elements (triangles)
C         at the listed faces.
C
C     INPUT ARGUMENTS -
C
C        cmo_name - The name of the CMO.
C        nadd     - Number of nodes to add.
C        (ip)itadd  - Integer pointer to the list of elements where
C                        nodes will be added.
C        (ip)ifadd  - Integer pointer to the list of local faces where
C                        nodes will be added.
C        (ip)iadd(nadd) - The "names" of the nodes to add.
C        (ip)xadd(nadd) - The X-coordinate of the nodes to add.
C        (ip)yadd(nadd) - The Y-coordinate of the nodes to add.
C        (ip)zadd(nadd) - The Z-coordinate of the nodes to add.
C
C     OUTPUT ARGUMENTS -
C     CHANGE HISTORY -
C
C        $Log: refine_face_add.f,v $
C        Revision 2.00  2007/11/09 20:04:01  spchu
C        Import to CVS
C
C
C     ##################################################################
C
      implicit none
C
      character*132 logmess
C
C     ##################################################################
C
      include "chydro.h"
      include "local_element.h"
C
C     ##################################################################
C
      character*(*) cmo_name
      integer nadd
C
      pointer (ipitadd, itadd)
      pointer (ipifadd, ifadd)
      integer itadd(nadd), ifadd(nadd)
C
      pointer (ipiadd,iadd)
      integer iadd(nadd)
      real*8 xadd(nadd), yadd(nadd), zadd(nadd)
C
C     ##################################################################
C
      pointer (ipint1add, int1add)
      integer int1add(nadd)
C
      pointer (ipiaddorder1, iaddorder1)
      pointer (ipiaddorder2, iaddorder2)
      integer iaddorder1(nadd), iaddorder2(nadd)
C
      pointer (ipitp1, itp1)
      pointer (ipisn1, isn1)
      pointer (ipint1, int1)
      pointer (ipicr1, icr1)
      integer itp1(*), isn1(*), int1(*), icr1(*)
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      real*8 xic(*), yic(*), zic(*)
C
      pointer (ipitet, itet)
      pointer (ipitet, itet1)
      pointer (ipjtet, jtet)
      pointer (ipjtet, jtet1)
      pointer (ipicontab, icontab)
      integer icontab(50,*)
      integer itet(3,1000000), jtet(3,1000000)
      integer itet1(3*1000000), jtet1(3*1000000)
C
      pointer (ipitetclr, itetclr)
      pointer (ipitettyp, itettyp)
      pointer (ipitetoff, itetoff)
      pointer (ipjtetoff, jtetoff)
      integer itetclr(1000000), itettyp(1000000),
     *        itetoff(1000000), jtetoff(1000000)
C
      pointer (ipiparent, iparent)
      integer iparent(1000000)
C
      pointer (ipitflag, itflag)
      pointer (ipitetnn, itetnn)
      pointer (ipitetnn1, itetnn1)
      pointer (ipitetnn2, itetnn2)
C
      pointer (ipiface_p1, iface_p1)
      pointer (ipiface_p2, iface_p2)
      pointer (ipictemp,ictemp)
      pointer (ipicrnew,icrnew), (ipitpnew,itpnew)
      integer itpnew(*)
      integer iface_p1(nadd), iface_p2(nadd),ictemp(*),icrnew(*)
      integer itflag(1000000),
     *        itetnn(3,1000000), itetnn1(3,1000000), itetnn2(3,1000000)
C
      pointer (iplist_sink, list_sink)
      pointer (iplist_source, list_source)
      pointer (ipxweight_source, xweight_source)
C
      character*32 cmo
      character*32 cmolength
      character*32 isubname, iblknam, iprtnam
      logical ifound
      integer icscode,i,ierror,icmotype,nen,nef,ier,
     *   nconbnd,iface,nface1,it,jt,lencmo,i1,i2,i3,length,icmot,
     *  npoints,icount,jcount,if1,if2,itype,kk,k,ilen,idum,inc1,
     *  inc2,kt,kf,ntetsinc,inc,ierr,npointsnew1,npointsinc,
     *  j2,j3,i5,jtf,itf,inew,itnew,ip1,ip2,nface1_save,ierrw,
     *  npointsnew,ntetsnew,ifaceiter,nef1,if,j,mbndry,ntets,
     *  irefine,nadd1,ip3,intadd,jf,nelementsmm,nnodesmm,ics,iflag,
     *  nvalues,iskip
      parameter (nvalues=2)
 
      integer list_sink(nadd), list_source(nvalues,nadd)
      real*8 xweight_source(nvalues,nadd),xxsmall,x1,y1,z1,x2,y2,z2
     *  ,x3,y3,z3
C
C
C     ###################################################################
C
      isubname='refine_face_add_tri'
C
      xxsmall=1.0d-30
C
      length=nadd
      call mmgetblk('icrnew',isubname,ipicrnew,length,1,icscode)
      call mmgetblk('itpnew',isubname,ipitpnew,length,1,icscode)
      call mmgetblk('ictemp',isubname,ipictemp,length,1,icscode)
      call mmgetblk('int1add',isubname,ipint1add,length,1,icscode)
      call mmgetblk('iaddorder1',isubname,ipiaddorder1,length,1,icscode)
      call mmgetblk('iaddorder2',isubname,ipiaddorder2,length,1,icscode)
      do i=1,nadd
         int1add(i)=0
         iaddorder1(i)=0
         iaddorder2(i)=0
c
c  convert face number to edge number
c
         ictemp(i)=ifadd(i)
         if(ifadd(i).eq.3) ictemp(i)=1
         if(ifadd(i).eq.1) ictemp(i)=3
      enddo
c
c  get icr and itp of new node and save for later
c
      call refine_fix_add(cmo_name,nadd,ipitadd,ipictemp,ipiadd,
     *  ipitpnew,ipicrnew)
C
      length=nadd
      call mmgetblk('list_sink',isubname,iplist_sink,length,1,icscode)
      length=nvalues*nadd
      call mmgetblk('list_source',isubname,iplist_source,length,1,
     *              icscode)
      call mmgetblk('xweight_source',isubname,ipxweight_source,length,2,
     *              icscode)
C
      call cmo_get_name(cmo,ierror)
      call cmo_get_intinfo('nnodes',cmo,npoints,length,icmotype,ierror)
      call cmo_get_intinfo('nelements',cmo,ntets,length,icmotype,ierror)
      call cmo_get_intinfo('mbndry',cmo,mbndry,length,icmotype,ierror)
      call cmo_get_intinfo('nodes_per_element',cmo,
     *                  nen,length,icmotype,ierror)
      call cmo_get_intinfo('faces_per_element',cmo,
     *                  nef,length,icmotype,ierror)
      call cmo_get_info('itp1',cmo,ipitp1,lencmo,icmotype,ier)
      call cmo_get_info('isn1',cmo,ipisn1,lencmo,icmotype,ier)
      call cmo_get_info('icr1',cmo,ipicr1,lencmo,icmotype,ier)
      call cmo_get_info('itetclr',cmo,ipitetclr,lencmo,icmotype,ier)
      call cmo_get_info('itettyp',cmo,ipitettyp,lencmo,icmotype,ier)
      call cmo_get_info('itetoff',cmo,ipitetoff,lencmo,icmotype,ier)
      call cmo_get_info('jtetoff',cmo,ipjtetoff,lencmo,icmotype,ier)
      call cmo_get_info('itet',cmo,ipitet,lencmo,icmotype,ierror)
      call cmo_get_info('jtet',cmo,ipjtet,lencmo,icmotype,ierror)
      call cmo_get_info('xic',cmo,ipxic,lencmo,icmotype,ierror)
      call cmo_get_info('yic',cmo,ipyic,lencmo,icmotype,ierror)
      call cmo_get_info('zic',cmo,ipzic,lencmo,icmotype,ierror)
      call cmo_get_intinfo('nconbnd',cmo,nconbnd,length,icmotype,
     *                   ierror)
      if(ierror.ne.0)  then
         nconbnd=0
      else
         call cmo_get_info('icontab',cmo_name,ipicontab,length,
     *                   icmotype,   ierror)
      endif
C
      length=npoints
      call mmgetblk('iparent',isubname,ipiparent,length,1,icscode)
      call unpackpc(npoints,itp1,isn1,iparent)
C
      length=ntets
      call mmgetblk('itflag',isubname,ipitflag,length,1,icscode)
      length=4*ntets
      call mmgetblk('itetnn' ,isubname,ipitetnn ,length,1,icscode)
      call mmgetblk('itetnn1',isubname,ipitetnn1,length,1,icscode)
      call mmgetblk('itetnn2',isubname,ipitetnn2,length,1,icscode)
      length=nadd
      call mmgetblk('iface_p1',isubname,ipiface_p1,length,1,icscode)
      call mmgetblk('iface_p2',isubname,ipiface_p2,length,1,icscode)
C
      do it=1,ntets
         itflag(it)=0
      enddo
C
      do j=1,nadd
         int1add(j)=j
         iaddorder1(j)=j
         iaddorder2(j)=0
         it=itadd(j)
         if=ifadd(j)
         iface_p1(j)=itet(ielmface1(1,if,itettyp(it)),it)
         iface_p2(j)=itet(ielmface1(2,if,itettyp(it)),it)
      enddo
      irefine=0
      nadd1=0
      npointsnew=npoints
      ntetsnew=ntets
      nface1=nadd
      ifaceiter=0
C
C    Loop to here if more than one iteration is needed
C    More than one iteration needed if more than one node
C    is to be added to a given element.
C
  11  continue
      do it=1,ntets
         do i=1,nelmnen(itettyp(it))
            itetnn(i,it)=itet(i,it)
         enddo
         nef1=nelmnef(itettyp(it))
         do i=1,nef1
            if(jtet(i,it).eq.mbndry) then
               itetnn1(i,it)=0
               itetnn2(i,it)=0
            elseif(jtet(i,it).gt.mbndry) then
               itetnn1(i,it)=1+(jtet(i,it)-mbndry-1)/nef
               itetnn2(i,it)=jtet(i,it)-mbndry-nef*(itetnn1(i,it)-1)
            else
               itetnn1(i,it)=1+(jtet(i,it)-1)/nef
               itetnn2(i,it)=jtet(i,it)-nef*(itetnn1(i,it)-1)
            endif
         enddo
      enddo
      do it=1,ntets
         itflag(it)=0
      enddo
      npointsnew=npoints
      ntetsnew=ntets
        ifaceiter=ifaceiter+1
      write(logmess,'(a,2i10)') 'Face iteration, number of faces: ',
     x                           ifaceiter,nface1
      call writloga('default',0,logmess,0,ierrw)
      do it=1,ntets
         itflag(it)=0
      enddo
C      write(logmess,'(a,i10)')
C     *   'Interface face that need reconnecting: ',nface1
C      call writloga('default',0,logmess,0,ierrw)
      if(nface1.ne.0) then
         nface1_save=0
C
C  Loop over number of points to add on faces.
C  Initially set to nadd.
C  Subsequent iterations set it to remaining points
C  as only one point may be added to a given element
C  per iteration.
C
         do iface=1,nface1
            it=itadd(iface)
            if=ifadd(iface)
            nen=nelmnen(itettyp(it))
            nef1=nelmnef(itettyp(it))
            i2=itet(ielmface1(1,if,itettyp(it)),it)
            i3=itet(ielmface1(2,if,itettyp(it)),it)
            i1=itet(1,it)+itet(2,it)+itet(3,it)-i2-i3
            ip1=iface_p1(iface)
            ip2=iface_p2(iface)
            if((ip1.eq.i2.and.ip2.eq.i3) .or.
     *         (ip1.eq.i3.and.ip2.eq.i2)) then
            else
               do itnew=1,ntetsnew
                  do inew=1,nef1
                     i2=itetnn(ielmface1(1,inew,itettyp(itnew)),itnew)
                     i3=itetnn(ielmface1(2,inew,itettyp(itnew)),itnew)
                     i1=itet(1,itnew)+itet(2,itnew)+itet(3,itnew)-i2-i3
                     if((ip1.eq.i2.and.ip2.eq.i3) .or.
     *                  (ip1.eq.i3.and.ip2.eq.i2)) then
                        it=itnew
                        if=inew
                        itadd(iface)=it
                        ifadd(iface)=if
                        goto 51
                     endif
                  enddo
               enddo
               write(logmess,'(a,3i10)')
     *            'Error: cannot match add-face ',ip1,ip2,ip3
               call writloga('default',0,logmess,0,ierrw)
               goto 20
   51          continue
            endif
            if(jtet(if,it).gt.0.and.jtet(if,it).lt.mbndry) then
               intadd=0
               jt=1+(jtet(if,it)-1)/nef
               jf=jtet(if,it)-nef*(jt-1)
            elseif(jtet(if,it).gt.mbndry) then
               intadd=1
               jt=1+(jtet(if,it)-mbndry-1)/nef
               jf=jtet(if,it)-mbndry-nef*(jt-1)
            else
               intadd=0
               jt=1+(jtet(if,it)-1)/nef
               jf=-1
            endif
            if(jt.gt.0.and.jt.le.ntets) then
               itf=itflag(it)
               jtf=itflag(jt)
               i5=itet(jf,jt)
               j2=itet(ielmface1(1,jf,itettyp(jt)),jt)
               j3=itet(ielmface1(2,jf,itettyp(jt)),jt)
            else
               itf=itflag(it)
               jtf=-1
               jt=0
               jf=0
               i5=0
            endif
            if(it.lt.0.or.jt.lt.0) then
               write(logmess,'(a,2i10)') 'Error1: ',it,jt
               call writloga('default',0,logmess,0,ierrw)
               stop
            endif
            if(it.gt.ntets.or.jt.gt.ntets) then
               write(logmess,'(a,2i10)') 'Error2: ',it,jt
               call writloga('default',0,logmess,0,ierrw)
               goto 20
            endif
C
C  itf and jtf have been set from itflag
C  if they are non-zero the corresponding element has
C  already had a point added to it.
C
            if(itf.eq.0.and.jtf.le.0) then
               irefine=irefine+1
               if(iadd(iaddorder1(iface)).gt.0) then
                  npointsnew1=iadd(iaddorder1(iface))
                  nadd1=nadd1+1
                  int1add(nadd1)=0
                  iaddorder2(nadd1)=iaddorder1(iface)
                  list_sink(nadd1)=npointsnew1
                  list_source(1,nadd1)=npointsnew1
                  list_source(2,nadd1)=npointsnew1
                  xweight_source(1,nadd1)=1.0
                  xweight_source(2,nadd1)=1.0
               elseif(iadd(iaddorder1(iface)).lt.0) then
                  npointsnew1=iabs(iadd(iaddorder1(iface)))
                  nadd1=nadd1+1
                  int1add(nadd1)=0
                  iaddorder2(nadd1)=iaddorder1(iface)
                  list_sink(nadd1)=npointsnew1
                  list_source(1,nadd1)=i2
                  list_source(2,nadd1)=i3
                  xweight_source(1,nadd1)=1.0
                  xweight_source(2,nadd1)=1.0
               else
                  npointsnew=npointsnew+1
                  npointsnew1=npointsnew
                  iadd(iaddorder1(iface))=npointsnew1
                  nadd1=nadd1+1
                  int1add(nadd1)=intadd
                  iaddorder2(nadd1)=iaddorder1(iface)
                  list_sink(nadd1)=npointsnew1
                  list_source(1,nadd1)=i2
                  list_source(2,nadd1)=i3
                  xweight_source(1,nadd1)=1.0
                  xweight_source(2,nadd1)=1.0
                  call mmfindbk('xic',cmo,ipxic,length,icscode)
                  if((npointsnew+1+intadd).gt.length) then
                     npointsinc=npointsnew+1000
                     call cmo_set_info('nnodes',cmo,
     *                                 npointsinc,1,1,ierror)
                     call mmgetlen(ipitetclr,nelementsmm,icscode)
                     call cmo_set_info('nelements',cmo,
     *                                 nelementsmm,1,1,ier)
                     call cmo_newlen(cmo,ierror)
                     call cmo_get_intinfo('mbndry',cmo,mbndry,length,
     *                     icmotype,ierror)
                     call cmo_get_info('itetclr',cmo,
     *                              ipitetclr,lencmo,icmotype,ier)
                     call cmo_get_info('itettyp',cmo,
     *                              ipitettyp,lencmo,icmotype,ier)
                     call cmo_get_info('itetoff',cmo,
     *                              ipitetoff,lencmo,icmotype,ier)
                     call cmo_get_info('jtetoff',cmo,
     *                              ipjtetoff,lencmo,icmotype,ier)
                     call cmo_get_info('itet',cmo,
     *                              ipitet,lencmo,icmotype,ierror)
                     call cmo_get_info('jtet',cmo,
     *                              ipjtet,lencmo,icmotype,ierror)
                     call cmo_get_info('xic',cmo,ipxic,lencmo,
     *                              icmot,ierr)
                     call cmo_get_info('yic',cmo,ipyic,lencmo,
     *                              icmot,ierr)
                     call cmo_get_info('zic',cmo,ipzic,lencmo,
     *                              icmot,ierr)
 
                     call mmnewlen('iparent',isubname,
     *                             ipiparent,npointsinc,icscode)
 
                  endif
               endif
C
C    update xic,yic and zic value for new points - will be
C    reset by cmo_interpolate later
C
               xic(npointsnew1)=0.5*(xic(ip2)+xic(ip1))
               yic(npointsnew1)=0.5*(yic(ip2)+yic(ip1))
               zic(npointsnew1)=0.5*(zic(ip2)+zic(ip1))
               iparent(npointsnew1)=npointsnew1
C
               inc=2
               if(jt.gt.0) inc=inc+2
               call mmgetlen(ipitetclr,length,icscode)
               if((ntetsnew+inc).gt.length) then
                  inc=1000
                  ntetsinc=ntetsnew+inc
                  call cmo_set_info('nelements',cmo,ntetsinc,1,1,ierror)
                  call mmfindbk('xic',cmo,ipxic,nnodesmm,icscode)
                  call cmo_set_info('nnodes',cmo,nnodesmm,1,1,ierror)
                  call cmo_newlen(cmo,ierror)
                  call cmo_get_intinfo('mbndry',cmo,mbndry,length,
     *                     icmotype,ierror)
                  call cmo_get_info('itetclr',cmo,
     *                              ipitetclr,lencmo,icmotype,ier)
                  call cmo_get_info('itettyp',cmo,
     *                              ipitettyp,lencmo,icmotype,ier)
                  call cmo_get_info('itetoff',cmo,
     *                              ipitetoff,lencmo,icmotype,ier)
                  call cmo_get_info('jtetoff',cmo,
     *                              ipjtetoff,lencmo,icmotype,ier)
                  call cmo_get_info('itet',cmo,
     *                              ipitet,lencmo,icmotype,ierror)
                  call cmo_get_info('jtet',cmo,
     *                              ipjtet,lencmo,icmotype,ierror)
                  call cmo_get_info('xic',cmo,ipxic,lencmo,icmot,ierr)
                  call cmo_get_info('yic',cmo,ipyic,lencmo,icmot,ierr)
                  call cmo_get_info('zic',cmo,ipzic,lencmo,icmot,ierr)
               endif
               inc=2
               if(jt.gt.0) inc=inc+2
               call mmgetlen(ipitflag,length,icscode)
               if((ntetsnew+inc).gt.length) then
                  inc=1000
                  call mmgetnam(ipitflag,iblknam,iprtnam,ics)
                  call mmincblk(iblknam,iprtnam,ipitflag,inc,
     *                          ics)
                  do idum=ntetsnew+1,ntetsnew+inc
                     itflag(idum)=0
                  enddo
                  inc1=nen*inc
                  call mmgetnam(ipitetnn,iblknam,iprtnam,ics)
                  call mmincblk(iblknam,iprtnam,ipitetnn,inc1,
     *                          ics)
                  inc2=nef*inc
                  call mmgetnam(ipitetnn1,iblknam,iprtnam,ics)
                  call mmincblk(iblknam,iprtnam,ipitetnn1,inc2,
     *                          ics)
                  call mmgetnam(ipitetnn2,iblknam,iprtnam,ics)
                  call mmincblk(iblknam,iprtnam,ipitetnn2,inc2,
     *                          ics)
               endif
C
C  prepare to add point to mesh
C  mark elements as having a point added
C  update the connectivity and create the new element(s)
C  it is the original element, jt is the neighbor element
C  that shares the face on which the point is being added.
C
               itflag(it)=1
               itetnn(1,it)=i1
               itetnn(2,it)=i2
               itetnn(3,it)=npointsnew1
               itetnn1(1,it)=-1
               itetnn1(2,it)=-1
               itetnn1(3,it)=-1
               itetnn2(1,it)=-1
               itetnn2(2,it)=-1
               itetnn2(3,it)=-1
               ntetsnew=ntetsnew+1
                  itflag(ntetsnew)=1
                  itetclr(ntetsnew)=itetclr(it)
                  itettyp(ntetsnew)=itettyp(it)
                  itetoff(ntetsnew)=nen*(ntetsnew-1)
                  jtetoff(ntetsnew)=jtetoff(ntetsnew-1)+nelmnef(
     *                         itettyp(ntetsnew-1))
                  itetnn(1,ntetsnew)=i1
                  itetnn(2,ntetsnew)=npointsnew1
                  itetnn(3,ntetsnew)=i3
                  itetnn1(1,ntetsnew)=-1
                  itetnn1(2,ntetsnew)=-1
                  itetnn1(3,ntetsnew)=-1
                  itetnn2(1,ntetsnew)=-1
                  itetnn2(2,ntetsnew)=-1
                  itetnn2(3,ntetsnew)=-1
               if(i5.le.0) then
               else
                  ntetsnew=ntetsnew+1
                     itflag(ntetsnew)=1
                     itetclr(ntetsnew)=itetclr(jt)
                     itettyp(ntetsnew)=itettyp(jt)
                     itetoff(ntetsnew)=nen*(ntetsnew-1)
                     jtetoff(ntetsnew)=jtetoff(ntetsnew-1)+nelmnef(
     *                         itettyp(ntetsnew-1))
                     itetnn(1,ntetsnew)=i5
                     itetnn(2,ntetsnew)=j2
                     itetnn(3,ntetsnew)=npointsnew1
                     itetnn1(1,ntetsnew)=-1
                     itetnn1(2,ntetsnew)=-1
                     itetnn1(3,ntetsnew)=-1
                     itetnn2(1,ntetsnew)=-1
                     itetnn2(2,ntetsnew)=-1
                     itetnn2(3,ntetsnew)=-1
                  itflag(jt)=1
                  itetoff(jt)=nen*(jt-1)
                  jtetoff(jt)=nef*(jt-1)
                  itetnn(1,jt)=i5
                  itetnn(2,jt)=npointsnew1
                  itetnn(3,jt)=j3
                  itetnn1(1,jt)=-1
                  itetnn1(2,jt)=-1
                  itetnn1(3,jt)=-1
                  itetnn2(1,jt)=-1
                  itetnn2(2,jt)=-1
                  itetnn2(3,jt)=-1
               endif
            else
C
C  get here if need to save this point for next iteration
C  count and save tet numbers for later
C
               nface1_save=nface1_save+1
               iaddorder1(nface1_save)=iaddorder1(iface)
               itadd(nface1_save)=itadd(iface)
               ifadd(nface1_save)=ifadd(iface)
            endif
   20       continue
         enddo
      endif
      if (idebug.ge.1)  then
         write(logmess,'(a,i10,a,i10)')
     *     'Face-refined tri: old=',ntets,' new=',ntetsnew
         call writloga('default',0,logmess,0,ierrw)
      endif
      if(ntetsnew.gt.ntets) then
         do it=1,ntetsnew
            do i=1,nelmnen(itettyp(it))
               itet(i,it)=itetnn(i,it)
            enddo
            do i=1,nelmnen(itettyp(it))
               itetnn(i,it)=iparent(itetnn(i,it))
            enddo
         enddo
         do it=1,ntets
            if(itflag(it).ne.0) then
               do i=1,nelmnef(itettyp(it))
                  kt=1+(jtet(i,it)-1)/nelmnef(itettyp(it))
                  kf=jtet(i,it)-nelmnef(itettyp(it))*(kt-1)
                  if(kt.le.ntets) then
                     itetnn1(kf,kt)=-1
                     itetnn2(kf,kt)=-1
                  endif
                  itetnn1(i,it)=-1
                  itetnn2(i,it)=-1
               enddo
            endif
         enddo
         npoints=npointsnew
         ntets=ntetsnew
         call cmo_set_info('nnodes',cmo,npoints,1,1,ierror)
         call cmo_set_info('nelements',cmo,ntets,1,1,ierror)
         call geniee(itetnn,itetnn1,itetnn2,3,3,ntets,npoints,
     *               3,npoints,ntets)
         call cmo_get_info('itetclr',cmo,ipitetclr,ilen,icmotype,ier)
         do it=1,ntets
            do i=1,nelmnen(itettyp(it))
               if(itetnn1(i,it).gt.0.and.itetnn1(i,it).le.ntets) then
                  if(itetclr(it).eq.itetclr(itetnn1(i,it))) then
                     jtet(i,it)=nelmnef(itettyp(it))*(itetnn1(i,it)-1)+
     *                                                itetnn2(i,it)
                  else
                     jtet(i,it)=mbndry+
     *                          nelmnef(itettyp(it))*(itetnn1(i,it)-1)+
     *                                                itetnn2(i,it)
                  endif
               else
                  jtet(i,it)=mbndry
               endif
            enddo
         enddo
         if(nface1_save.gt.0) then
            do k = 1,nface1_save
               it=itadd(k)
               kk = iaddorder1(k)
               ifound=.false.
               x1=xic(itetnn(1,it))
               x2=xic(itetnn(2,it))
               x3=xic(itetnn(3,it))
               y1=yic(itetnn(1,it))
               y2=yic(itetnn(2,it))
               y3=yic(itetnn(3,it))
               z1=zic(itetnn(1,it))
               z2=zic(itetnn(2,it))
               z3=zic(itetnn(3,it))
               call inside_tri2d(x1,y1,z1,x2,y2,z2,x3,y3,z3,
     *             xadd(kk),yadd(kk),zadd(kk),iflag)
               if(iflag.gt.0) then
                  ifound=.true.
                  itadd(k)=it
                  ifadd(k)=iflag
                  itype=itettyp(it)
                  if1=ielmface1(1,iflag,itype)
                  if2=ielmface1(2,iflag,itype)
                  iface_p1(k)=itet(if1,it)
                  iface_p2(k)=itet(if2,it)
               else
                  do j=1,nelmnef(itettyp(it))
                     if(jtet(j,it).ne.mbndry) then
                        jt=jtet(j,it)
                        if(jt.gt.mbndry) jt=jt-mbndry
                        jt=1+(jt-1)/nef
                        x1=xic(itetnn(1,jt))
                        x2=xic(itetnn(2,jt))
                        x3=xic(itetnn(3,jt))
                        y1=yic(itetnn(1,jt))
                        y2=yic(itetnn(2,jt))
                        y3=yic(itetnn(3,jt))
                        z1=zic(itetnn(1,jt))
                        z2=zic(itetnn(2,jt))
                        z3=zic(itetnn(3,jt))
                        call inside_tri2d(x1,y1,z1,x2,y2,z2,x3,y3,z3,
     *                    xadd(kk),yadd(kk),zadd(kk),iflag)
                        if(iflag.gt.0) then
                           ifound=.true.
                           itadd(k)=jt
                           ifadd(k)=iflag
                           iface_p1(k)=itet(ielmface1(1,iflag,
     *                       itettyp(jt)),jt)
                           iface_p2(k)=itet(ielmface1(2,iflag,
     *                       itettyp(jt)),jt)
                        endif
                     endif
                  enddo
               endif
               if(.not.ifound) then
                  write(logmess,"('Bad Point in tet ',i5,3e15.7)")
     *                    it,xadd(kk),yadd(kk),zadd(kk)
                  call writloga('default',0,logmess,0,ierror)
               endif
            enddo
            nface1=nface1_save
            goto 11
         endif
      endif
      goto 9999
 9999 continue
C
      cmolength='nnodes'

      call cmo_interpolate(cmo_name,cmo_name,
     *                     cmolength,
     *                     nadd1,nvalues,
     *                     list_sink,list_source,xweight_source,
     *                     ierror)
      if(ierror.ne.0) call x3d_error(isubname,'cmo_interpolate')
C
      call cmo_get_info('itp1',cmo,ipitp1,ilen,icmotype,ier)
      call cmo_get_info('isn1',cmo,ipisn1,ilen,icmotype,ier)
      call cmo_get_info('icr1',cmo,ipicr1,ilen,icmotype,ier)
      call cmo_get_info('xic',cmo,ipxic,ilen,icmotype,ier)
      call cmo_get_info('yic',cmo,ipyic,ilen,icmotype,ier)
      call cmo_get_info('zic',cmo,ipzic,ilen,icmotype,ier)
C
      call cmo_get_info('itp1',cmo,ipitp1,length,icmotype,ierror)
      length=npoints
      call mmgetblk('int1',isubname,ipint1,length,1,icscode)
      call unpacktp('intrface','set',npoints,ipitp1,ipint1,ierr)
      if(ierr.ne.0) call x3d_error('refine_edge_add', 'unpacktp')
C
      icount=0
      jcount=0
      do i=1,nadd1
         i1=list_sink(i)
         i2=iaddorder2(i)
         if(int1(i1).eq.1) then
            jcount=jcount+1
         endif
         if(int1add(i).eq.1) then
            itp1(i1)=ifitpini
            icount=icount+1
         endif
         isn1(i1)=0
         icr1(i1)=icrnew(i2)
         itp1(i1)=itpnew(i2)

c.... We comment out these lines, so that we let stand
c.... possible nonlinear (user) interpolation of 
c.... x-, y-, or z- values done in cmo_interpolate.
         xic(i1)=xadd(i2)
         yic(i1)=yadd(i2)
         zic(i1)=zadd(i2)
      enddo
C
      call cmo_get_name(cmo,ierror)
      call cmo_set_info('nnodes',cmo,npoints,1,1,ierror)
      call cmo_set_info('nelements',cmo,ntets,1,1,ierror)
C
      if(jcount.gt.0.or.icount.gt.0) then
c
c.... see if we want to do settets here
c
         iskip=0
         call cmo_get_intinfo('skipsettets',cmo,iskip,
     *    length,icmotype,ierror)
         if(ierror.ne.0.or.iskip.eq.0) then
            call dotaskx3d('settets/parents ; finish',ierror)
            call cmo_get_intinfo('nnodes',cmo,npoints,length,
     *         icmotype,ierror)
            call cmo_get_intinfo('nelements',cmo,ntets,length,
     *         icmotype,ierror)
         endif
      endif
C
      call cmo_get_name(cmo,ierror)
      call cmo_set_info('nnodes',cmo,npoints,1,1,ierror)
      call cmo_set_info('nelements',cmo,ntets,1,1,ierror)
      call cmo_newlen(cmo,ierror)
      call cmo_get_intinfo('mbndry',cmo,mbndry,length,
     *                     icmotype,ierror)
      call mmrelprt(isubname,icscode)
C
      return
      end
*dk,refine_face_add_tet
      subroutine refine_face_add_tet(cmo_name,
     *                               nadd,
     *                               ipitadd,ipifadd,
     *                               ipiadd,xadd,yadd,zadd)
C
C #####################################################################
C
C     PURPOSE -
C
C        This routine refines element faces  by adding
C        the points listed to the listed elements
C        at the listed faces.
C
C     INPUT ARGUMENTS -
C
C        cmo_name - The name of the CMO.
C        nadd     - Number of nodes to add.
C        (ip)itadd  - Integer pointer to the list of elements where
C                        nodes will be added.
C        (ip)ifadd  - Integer pointer to the list of local faces where
C                        nodes will be added.
C        (ip)iadd(nadd) - The "names" of the nodes to add.
C        (ip)xadd(nadd) - The X-coordinate of the nodes to add.
C        (ip)yadd(nadd) - The Y-coordinate of the nodes to add.
C        (ip)zadd(nadd) - The Z-coordinate of the nodes to add.
C
C     OUTPUT ARGUMENTS -
C     CHANGE HISTORY -
C
C        $Log: refine_face_add.f,v $
C        Revision 2.00  2007/11/09 20:04:01  spchu
C        Import to CVS
C
CPVCS
CPVCS       Rev 1.12   11/07/95 17:24:44   dcg
CPVCS    change flag to 2 in mmgetblk calls
CPVCS
CPVCS       Rev 1.11   10/20/95 10:48:12   het
CPVCS    Fix iparent memory management error and add new refine options.
CPVCS
CPVCS       Rev 1.10   10/05/95 15:46:54   het
CPVCS    Add the intrface refinement option
CPVCS
CPVCS       Rev 1.9   10/04/95 07:43:16   het
CPVCS    Add
CPVCS
CPVCS       Rev 1.8   09/29/95 09:14:00   het
CPVCS    Put in added attributes inheritance
CPVCS
CPVCS       Rev 1.7   08/16/95 06:51:30   het
CPVCS    Correct an error
CPVCS
CPVCS       Rev 1.6   08/15/95 18:23:30   het
CPVCS    Cleanup code and correct errors
CPVCS
CPVCS       Rev 1.5   06/05/95 10:36:44   het
CPVCS    Make changes for hybrid_grids
CPVCS
CPVCS       Rev 1.4   05/30/95 07:51:20   het
CPVCS    Replace mesh_object subroutine parameters by cmo-calls
CPVCS
CPVCS       Rev 1.3   05/26/95 13:18:12   het
CPVCS    Replace subroutine parameter list with subroutine calles.
CPVCS
CPVCS       Rev 1.2   03/28/95 12:34:38   het
CPVCS    Add the binary dumpx3d/readx3d commands and correct associated mm-errors.
CPVCS
CPVCS       Rev 1.1   03/13/95 16:15:04   het
CPVCS    Get mbndry from cmo and fixed 1st edge selection error
CPVCS
CPVCS       Rev 1.0   11/10/94 12:18:06   pvcs
CPVCS    Original version.
C
C     ##################################################################
C
      implicit none
C
      character*132 logmess
C
C     ##################################################################
C
      include "local_element.h"
      include "chydro.h"
C
C     ##################################################################
C
      character*(*) cmo_name
      integer nadd
C
      pointer (ipiadd,iadd)
      pointer (ipitadd, itadd)
      pointer (ipifadd, ifadd)
      integer itadd(nadd), ifadd(nadd)
C
      integer iadd(nadd)
      real*8 xadd(nadd), yadd(nadd), zadd(nadd)
C
C     ##################################################################
C
      pointer (ipint1add, int1add)
      integer int1add(nadd)
C
      pointer (ipiaddorder1, iaddorder1)
      pointer (ipiaddorder2, iaddorder2)
      integer iaddorder1(nadd), iaddorder2(nadd)
C
      pointer (ipitp1, itp1)
      pointer (ipisn1, isn1)
      pointer (ipint1, int1)
      pointer (ipicr1, icr1)
      integer itp1(10000000), isn1(10000000), int1(1000000),
     * icr1(1000000)
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      real*8 xic(10000000), yic(10000000), zic(10000000)
C
      pointer (ipitet, itet)
      pointer (ipitet, itet1)
      pointer (ipjtet, jtet)
      pointer (ipjtet, jtet1)
      integer itet(4,1000000), jtet(4,1000000)
      integer itet1(4*1000000), jtet1(4*1000000)
C
      pointer (ipitetclr, itetclr)
      pointer (ipitettyp, itettyp)
      pointer (ipitetoff, itetoff)
      pointer (ipjtetoff, jtetoff)
      integer itetclr(1000000), itettyp(1000000),
     *        itetoff(1000000), jtetoff(1000000)
      pointer (ipicontab, icontab)
      integer icontab(50,1000000)
C
      pointer (ipiparent, iparent)
      integer iparent(1000000)
C
      pointer (ipitflag, itflag)
      pointer (ipitetnn, itetnn)
      pointer (ipitetnn1, itetnn1)
      pointer (ipitetnn2, itetnn2)
      pointer (ipitpnew,itpnew)
      pointer (ipicrnew,icrnew)
      integer itpnew(1000000),icrnew(1000000)
      pointer (ipictemp,ictemp)
      integer ictemp(1000000)
C
      pointer (ipiface_p1, iface_p1)
      pointer (ipiface_p2, iface_p2)
      pointer (ipiface_p3, iface_p3)
      integer iface_p1(nadd), iface_p2(nadd), iface_p3(nadd)
      integer itflag(1000000),
     *        itetnn(4,1000000), itetnn1(4,1000000), itetnn2(4,1000000)
      integer nvalues
C
      parameter (nvalues=3)
      pointer (iplist_sink, list_sink)
      pointer (iplist_source, list_source)
      pointer (ipxweight_source, xweight_source)
      integer list_sink(nadd), list_source(nvalues,nadd)
      real*8 xweight_source(nvalues,nadd)
      logical itsttp,ivrt,ifre,irfl
      integer jcount,icount,npoints,ierrdum,kf,kt,inc1,
     *  inc2,idum,ics,nnodesmm,ntetsinc,inc,npointsnew1,j2,j3,j4,
     *  i5,jtf,itf,icnt,n,n1,n2,n3,nsb,nsa,nsd,icrnw,icrc,
     *  icrb,icra,itpnw,jt,intadd,inew,itnew,ip1,ip2,ip3,
     *  i1,i2,i3,ierrw,ifaceiter,nface1,ntetsnew,npointsnew,
     *  nadd1,irefine,if,j,it,nconbnd,ier,nef,nen,mbndry,
     *  icmotype,ntets,jf,i4,iface,
     *  nelementsmm,npointsinc,nsc,nface1_save,ierror,i,
     *  icscode,length,ilen
      real*8 xxsmall
C
      character*32 cmo
      character*32 cmolength
      character*32 isubname, iblknam, iprtnam
      integer itetface0(4), itetface1(4,4)
C     top,back,left,right
      data itetface0 / 3, 3, 3, 3 /
      data itetface1 / 2, 3, 4, 1,
     *                 1, 4, 3, 2,
     *                 1, 2, 4, 3,
     *                 1, 3, 2, 4 /
      integer itetface2(3,3,4)
      data itetface2 / 3, 4, 2,
     *                         4, 2, 3,
     *                         2, 3, 4,
     *                         4, 3, 1,
     *                         3, 1, 4,
     *                         1, 4, 3,
     *                         2, 4, 1,
     *                         4, 1, 2,
     *                         1, 2, 4,
     *                         3, 2, 1,
     *                         2, 1, 3,
     *                         1, 3, 2 /
      integer itetface3(2,3,4)
      data itetface3 / 2, 1,
     *                 3, 1,
     *                 4, 1,
     *                 1, 1,
     *                 4, 3,
     *                 3, 2,
     *                 1, 2,
     *                 2, 3,
     *                 4, 2,
     *                 1, 3,
     *                 3, 3,
     *                 2, 2 /
C
C
C     ###################################################################
C
      isubname='refine_face_add_tet'
C
      xxsmall=1.0d-30
C
      length=nadd
      call mmgetblk('int1add',isubname,ipint1add,length,1,icscode)
      call mmgetblk('itpnew',isubname,ipitpnew,length,1,icscode)
      call mmgetblk('icrnew',isubname,ipicrnew,length,1,icscode)
      call mmgetblk('iaddorder1',isubname,ipiaddorder1,length,1,icscode)
      call mmgetblk('iaddorder2',isubname,ipiaddorder2,length,1,icscode)
      do i=1,nadd
         icrnew(i)=0
         itpnew(i)=0
         int1add(i)=0
         iaddorder1(i)=0
         iaddorder2(i)=0
      enddo
C
      length=nadd
      call mmgetblk('list_sink',isubname,iplist_sink,length,1,icscode)
      length=nvalues*nadd
      call mmgetblk('list_source',isubname,iplist_source,length,1,
     *              icscode)
      call mmgetblk('xweight_source',isubname,ipxweight_source,length,2,
     *              icscode)
C
      call cmo_get_name(cmo,ierror)
      call cmo_get_intinfo('nnodes',cmo,npoints,length,icmotype,ierror)
      call cmo_get_intinfo('nelements',cmo,ntets,length,icmotype,ierror)
      call cmo_get_intinfo('mbndry',cmo,mbndry,length,icmotype,ierror)
      call cmo_get_intinfo('nodes_per_element',cmo,
     *                  nen,length,icmotype,ierror)
      call cmo_get_intinfo('faces_per_element',cmo,
     *                  nef,length,icmotype,ierror)
      call cmo_get_info('itp1',cmo,ipitp1,length,icmotype,ierror)
      call cmo_get_info('isn1',cmo,ipisn1,length,icmotype,ierror)
      call cmo_get_info('icr1',cmo,ipicr1,length,icmotype,ierror)
      call cmo_get_info('itetclr',cmo,ipitetclr,length,icmotype,ier)
      call cmo_get_info('itettyp',cmo,ipitettyp,length,icmotype,ier)
      call cmo_get_info('itetoff',cmo,ipitetoff,length,icmotype,ier)
      call cmo_get_info('jtetoff',cmo,ipjtetoff,length,icmotype,ier)
      call cmo_get_info('itet',cmo,ipitet,length,icmotype,ierror)
      call cmo_get_info('jtet',cmo,ipjtet,length,icmotype,ierror)
      call cmo_get_intinfo('nconbnd',cmo,nconbnd,length,icmotype,
     *                   ierror)
      if(ierror.ne.0)  then
         nconbnd=0
      else
         call cmo_get_info('icontab',cmo_name,ipicontab,length,
     *                   icmotype,   ierror)
         call mmgetblk('ictemp',isubname,ipictemp,200,1,icscode)
      endif
 
C
      length=npoints
      call mmgetblk('iparent',isubname,ipiparent,length,1,icscode)
      call unpackpc(npoints,itp1,isn1,iparent)
C
      length=ntets
      call mmgetblk('itflag',isubname,ipitflag,length,1,icscode)
      length=4*ntets
      call mmgetblk('itetnn' ,isubname,ipitetnn ,length,1,icscode)
      call mmgetblk('itetnn1',isubname,ipitetnn1,length,1,icscode)
      call mmgetblk('itetnn2',isubname,ipitetnn2,length,1,icscode)
      length=nadd
      call mmgetblk('iface_p1',isubname,ipiface_p1,length,1,icscode)
      call mmgetblk('iface_p2',isubname,ipiface_p2,length,1,icscode)
      call mmgetblk('iface_p3',isubname,ipiface_p3,length,1,icscode)
C
      do it=1,ntets
         itflag(it)=0
      enddo
C
      do j=1,nadd
         int1add(j)=j
         iaddorder1(j)=j
         iaddorder2(j)=0
         it=itadd(j)
         if=ifadd(j)
         iface_p1(j)=itet(itetface1(1,if),it)
         iface_p2(j)=itet(itetface1(2,if),it)
         iface_p3(j)=itet(itetface1(3,if),it)
      enddo
      irefine=0
      nadd1=0
      npointsnew=npoints
      ntetsnew=ntets
      nface1=nadd
      ifaceiter=0
  11  continue
      do it=1,ntets
         do i=1,4
            itetnn(i,it)=itet(i,it)
            if(jtet(i,it).eq.mbndry) then
               itetnn1(i,it)=0
               itetnn2(i,it)=0
            elseif(jtet(i,it).gt.mbndry) then
               itetnn1(i,it)=1+(jtet(i,it)-mbndry-1)/4
               itetnn2(i,it)=jtet(i,it)-mbndry-4*(itetnn1(i,it)-1)
            else
               itetnn1(i,it)=1+(jtet(i,it)-1)/4
               itetnn2(i,it)=jtet(i,it)-4*(itetnn1(i,it)-1)
            endif
         enddo
      enddo
      do it=1,ntets
         itflag(it)=0
      enddo
      npointsnew=npoints
      ntetsnew=ntets
        ifaceiter=ifaceiter+1
      write(logmess,'(a,2i10)') "Face iteration, number of faces: ",
     x                ifaceiter,nface1
      call writloga('default',0,logmess,0,ierrw)
      do it=1,ntets
         itflag(it)=0
      enddo
C      if(idebug .ge.1) then
C         write(logmess,'(a,i10)')
C     *     "Faces to be refined: ",nface1
C         call writloga('default',0,logmess,0,ierrw)
C      endif
C
C  see if skipped point(s) is on saved element or one of its
C  neighbors
C
      if(nface1.ne.0) then
         nface1_save=0
         do iface=1,nface1
            it=itadd(iface)
            if=ifadd(iface)
            i1=itet(if,it)
            i2=itet(itetface1(1,if),it)
            i3=itet(itetface1(2,if),it)
            i4=itet(itetface1(3,if),it)
            ip1=iface_p1(iface)
            ip2=iface_p2(iface)
            ip3=iface_p3(iface)
            if((ip1.eq.i2.and.ip2.eq.i3.and.ip3.eq.i4) .or.
     *         (ip1.eq.i3.and.ip2.eq.i4.and.ip3.eq.i2) .or.
     *         (ip1.eq.i4.and.ip2.eq.i2.and.ip3.eq.i3)) then
            else
               do itnew=1,ntetsnew
                  do inew=1,4
                     i1=itetnn(itetface1(4,inew),itnew)
                     i2=itetnn(itetface1(1,inew),itnew)
                     i3=itetnn(itetface1(2,inew),itnew)
                     i4=itetnn(itetface1(3,inew),itnew)
                     if((ip1.eq.i2.and.ip2.eq.i3.and.ip3.eq.i4) .or.
     *                  (ip1.eq.i3.and.ip2.eq.i4.and.ip3.eq.i2) .or.
     *                  (ip1.eq.i4.and.ip2.eq.i2.and.ip3.eq.i3)) then
                        it=itnew
                        if=inew
                        itadd(iface)=it
                        ifadd(iface)=if
                        goto 51
                     endif
                  enddo
               enddo
               write(logmess,'(a,3i10)')
     *            "Error: cannot match add-face ",ip1,ip2,ip3
               call writloga('default',0,logmess,0,ierrw)
               goto 20
   51          continue
            endif
            if(jtet(if,it).gt.0.and.jtet(if,it).lt.mbndry) then
               intadd=0
               jt=1+(jtet(if,it)-1)/4
               jf=jtet(if,it)-4*(jt-1)
               itpnw=ifitpint
            elseif(jtet(if,it).gt.mbndry) then
               intadd=1
               jt=1+(jtet(if,it)-mbndry-1)/4
               jf=jtet(if,it)-mbndry-4*(jt-1)
               itpnw=ifitpini
            else
               itpnw=ifitprfl
               ivrt=.false.
               ifre=.false.
               irfl=.false.
               if (itsttp('virtual',itp1(i2)).and.
     *             itsttp('virtual',itp1(i3)).and.
     *             itsttp('virtual',itp1(i4))) ivrt=.true.
               if (itsttp('free',itp1(i2)).and.
     *             itsttp('free',itp1(i3)).and.
     *             itsttp('free',itp1(i4))) ifre=.true.
               if (itsttp('reflect',itp1(i2)).and.
     *             itsttp('reflect',itp1(i3)).and.
     *             itsttp('reflect',itp1(i4))) irfl=.true.
               if (ivrt.and.ifre.and.irfl) itpnw=ifitpvrf
               if (ivrt.and.ifre.and..not.irfl) itpnw=ifitpvfb
               if (ivrt.and..not.ifre.and.irfl) itpnw=ifitpvrt
               if (.not.ivrt.and.ifre.and.irfl) itpnw=ifitprfb
               if (.not.ivrt.and.ifre.and..not.irfl) itpnw=
     *              ifitpfre
               intadd=0
               jt=1+(jtet(if,it)-1)/4
               jf=-1
            endif
            icra=icr1(i2)
            icrb=icr1(i3)
            icrc=icr1(i4)
            if(nconbnd.eq.0.or.icra.eq.0.or.icrb.eq.0
     *         .or.icrc.eq.0) then
               icrnw=0
               go to 100
            endif
            nsd=0
            nsa=icontab(1,icra)
            nsb=icontab(1,icrb)
            nsc=icontab(1,icrc)
            do n1=1,nsa
               do n2=1,nsb
                  do n3=1,nsc
                    if ((icontab(2+n1,icra).eq.icontab(2+n2,icrb))
     *                .and.(icontab(2+n3,icrc).eq.icontab(2+n1,icra)))
     *                then
                         nsd=nsd+1
                         ictemp(nsd)=icontab(2+n1,icra)
                    endif
                  enddo
               enddo
            enddo
C
C  if the intersection is empty then icrnew =0
C
            if( nsd.eq.0) then
               icrnw=0
               go to 100
            endif
C
C  if the intersection is not empty
C  find the correct icontab entry
C  print message if it doesn't exist
C
            do n=1,nconbnd
               if(icontab(1,n).eq.nsd) then
                  icnt=0
                  do nsa=1,nsd
                    do nsb=1,nsd
                        if(ictemp(nsa).eq.icontab(2+nsb,n))
     *                     icnt=icnt+1
                    enddo
                  enddo
                  if(icnt.eq.nsd) then
                     icrnw=n
                     go to 100
                  endif
               endif
            enddo
            write(logmess,22)
 22         format('error in refine_face_add',
     *         'cant find constraint entry')
            call writloga('default',0,logmess,0,ierror)
 100        continue
            if(jt.gt.0.and.jt.le.ntets) then
               itf=itflag(it)
               jtf=itflag(jt)
               i5=itet(jf,jt)
               j2=itet(itetface1(1,jf),jt)
               j3=itet(itetface1(2,jf),jt)
               j4=itet(itetface1(3,jf),jt)
            else
               itf=itflag(it)
               jtf=-1
               jt=0
               jf=0
               i5=0
            endif
            if(it.lt.0.or.jt.lt.0) then
               write(logmess,'(a,2i10)') "Error1: ",it,jt
               call writloga('default',0,logmess,0,ierrw)
               stop
            endif
            if(it.gt.ntets.or.jt.gt.ntets) then
               write(logmess,'(a,2i10)') "Error2: ",it,jt
               call writloga('default',0,logmess,0,ierrw)
               goto 20
            endif
            if(itf.eq.0.and.jtf.le.0) then
               irefine=irefine+1
               if(iadd(iaddorder1(iface)).gt.0) then
                  npointsnew1=iadd(iaddorder1(iface))
                  nadd1=nadd1+1
                  int1add(nadd1)=0
                  itpnew(nadd1)=itpnw
                  icrnew(nadd1)=icrnw
                  iaddorder2(nadd1)=iaddorder1(iface)
                  list_sink(nadd1)=npointsnew1
                  list_source(1,nadd1)=npointsnew1
                  list_source(2,nadd1)=npointsnew1
                  list_source(3,nadd1)=npointsnew1
                  xweight_source(1,nadd1)=1.0
                  xweight_source(2,nadd1)=1.0
                  xweight_source(3,nadd1)=1.0
               elseif(iadd(iaddorder1(iface)).lt.0) then
                  npointsnew1=iabs(iadd(iaddorder1(iface)))
                  nadd1=nadd1+1
                  int1add(nadd1)=0
                  itpnew(nadd1)=itpnw
                  icrnew(nadd1)=icrnw
                  iaddorder2(nadd1)=iaddorder1(iface)
                  list_sink(nadd1)=npointsnew1
                  list_source(1,nadd1)=i2
                  list_source(2,nadd1)=i3
                  list_source(3,nadd1)=i4
                  xweight_source(1,nadd1)=1.0
                  xweight_source(2,nadd1)=1.0
                  xweight_source(3,nadd1)=1.0
               else
                  npointsnew=npointsnew+1
                  npointsnew1=npointsnew
                  iadd(iaddorder1(iface))=npointsnew1
                  call mmfindbk('xic',cmo,ipxic,length,icscode)
                  if((npointsnew+1+intadd).gt.length) then
                     npointsinc=npointsnew+1000
                     call cmo_set_info('nnodes',cmo,
     *                                 npointsinc,1,1,ierror)
                     call mmgetlen(ipitetclr,nelementsmm,icscode)
                     call cmo_set_info('nelements',cmo,
     *                                 nelementsmm,1,1,ier)
                     call cmo_newlen(cmo,ierror)
                     call cmo_get_intinfo('mbndry',cmo,mbndry,length,
     *                     icmotype,ierror)
                     call mmnewlen('iparent',isubname,
     *                             ipiparent,npointsinc,icscode)
                  endif
                  nadd1=nadd1+1
                  int1add(nadd1)=intadd
                  itpnew(nadd1)=itpnw
                  icrnew(nadd1)=icrnw
                  iaddorder2(nadd1)=iaddorder1(iface)
                  list_sink(nadd1)=npointsnew1
                  list_source(1,nadd1)=i2
                  list_source(2,nadd1)=i3
                  list_source(3,nadd1)=i4
                  xweight_source(1,nadd1)=1.0
                  xweight_source(2,nadd1)=1.0
                  xweight_source(3,nadd1)=1.0
               endif
C
               iparent(npointsnew1)=npointsnew1
C
C
               inc=2
               if(jt.gt.0) inc=inc+2
               call mmgetlen(ipitetclr,length,icscode)
               if((ntetsnew+inc).gt.length) then
                  inc=1000
                  ntetsinc=ntetsnew+inc
                  call cmo_set_info('nelements',cmo,ntetsinc,1,1,ierror)
                  call mmfindbk('xic',cmo,ipxic,nnodesmm,icscode)
                  call cmo_set_info('nnodes',cmo,nnodesmm,1,1,ierror)
                  call cmo_newlen(cmo,ierror)
                  call cmo_get_intinfo('mbndry',cmo,mbndry,length,
     *                     icmotype,ierror)
                  call cmo_get_info('itetclr',cmo,
     *                              ipitetclr,length,icmotype,ierror)
                  call cmo_get_info('itettyp',cmo,
     *                              ipitettyp,length,icmotype,ierror)
                  call cmo_get_info('itetoff',cmo,
     *                              ipitetoff,length,icmotype,ierror)
                  call cmo_get_info('jtetoff',cmo,
     *                              ipjtetoff,length,icmotype,ierror)
                  call cmo_get_info('itet',cmo,
     *                              ipitet,length,icmotype,ierror)
                  call cmo_get_info('jtet',cmo,
     *                              ipjtet,length,icmotype,ierror)
                  call cmo_get_info('itp1',cmo,ipitp1,
     *                              length,icmotype,ierror)
                  call cmo_get_info('isn1',cmo,ipisn1,
     *                              length,icmotype,ierror)
                  call cmo_get_info('icr1',cmo,ipicr1,
     *                              length,icmotype,ierror)
                  call cmo_get_info('nconbnd',cmo,nconbnd,
     *                              length,icmotype,ierror)
                  if(ierror.eq.0)  then
                     call cmo_get_info('icontab',cmo_name,ipicontab,
     *                              length,icmotype,ierror)
                  endif
               endif
               inc=2
               if(jt.gt.0) inc=inc+2
               call mmgetlen(ipitflag,length,icscode)
               if((ntetsnew+inc).gt.length) then
                  inc=1000
                  call mmgetnam(ipitflag,iblknam,iprtnam,ics)
                  call mmincblk(iblknam,iprtnam,ipitflag,inc,
     *                          ics)
                  do idum=ntetsnew+1,ntetsnew+inc
                     itflag(idum)=0
                  enddo
                  inc1=nen*inc
                  call mmgetnam(ipitetnn,iblknam,iprtnam,ics)
                  call mmincblk(iblknam,iprtnam,ipitetnn,inc1,
     *                          ics)
                  inc2=nef*inc
                  call mmgetnam(ipitetnn1,iblknam,iprtnam,ics)
                  call mmincblk(iblknam,iprtnam,ipitetnn1,inc2,
     *                          ics)
                  call mmgetnam(ipitetnn2,iblknam,iprtnam,ics)
                  call mmincblk(iblknam,iprtnam,ipitetnn2,inc2,
     *                          ics)
               endif
               itflag(it)=1
               itetnn(1,it)=i1
               itetnn(2,it)=i2
               itetnn(3,it)=i3
               itetnn(4,it)=npointsnew1
               itetnn1(1,it)=-1
               itetnn1(2,it)=-1
               itetnn1(3,it)=-1
               itetnn1(4,it)=-1
               itetnn2(1,it)=-1
               itetnn2(2,it)=-1
               itetnn2(3,it)=-1
               itetnn2(4,it)=-1
               ntetsnew=ntetsnew+1
                  itflag(ntetsnew)=1
                  itetclr(ntetsnew)=itetclr(it)
                  itettyp(ntetsnew)=itettyp(it)
                  itetoff(ntetsnew)=nen*(ntetsnew-1)
                  jtetoff(ntetsnew)=jtetoff(ntetsnew-1)+nelmnef(
     *                         itettyp(ntetsnew-1))
                  itetnn(1,ntetsnew)=i1
                  itetnn(2,ntetsnew)=i4
                  itetnn(3,ntetsnew)=i2
                  itetnn(4,ntetsnew)=npointsnew1
                  itetnn1(1,ntetsnew)=-1
                  itetnn1(2,ntetsnew)=-1
                  itetnn1(3,ntetsnew)=-1
                  itetnn1(4,ntetsnew)=-1
                  itetnn2(1,ntetsnew)=-1
                  itetnn2(2,ntetsnew)=-1
                  itetnn2(3,ntetsnew)=-1
                  itetnn2(4,ntetsnew)=-1
               if(i5.le.0) then
                  ntetsnew=ntetsnew+1
                     itflag(ntetsnew)=1
                     itetclr(ntetsnew)=itetclr(it)
                     itettyp(ntetsnew)=itettyp(it)
                     itetoff(ntetsnew)=nen*(ntetsnew-1)
                     jtetoff(ntetsnew)=jtetoff(ntetsnew-1)+nelmnef(
     *                         itettyp(ntetsnew-1))
                     itetnn(1,ntetsnew)=i1
                     itetnn(2,ntetsnew)=i3
                     itetnn(3,ntetsnew)=i4
                     itetnn(4,ntetsnew)=npointsnew1
                     itetnn1(1,ntetsnew)=-1
                     itetnn1(2,ntetsnew)=-1
                     itetnn1(3,ntetsnew)=-1
                     itetnn1(4,ntetsnew)=-1
                     itetnn2(1,ntetsnew)=-1
                     itetnn2(2,ntetsnew)=-1
                     itetnn2(3,ntetsnew)=-1
                     itetnn2(4,ntetsnew)=-1
               else
                  ntetsnew=ntetsnew+1
                     itflag(ntetsnew)=1
                     itetclr(ntetsnew)=itetclr(jt)
                     itettyp(ntetsnew)=itettyp(jt)
                     itetoff(ntetsnew)=nen*(ntetsnew-1)
                     jtetoff(ntetsnew)=jtetoff(ntetsnew-1)+nelmnef(
     *                         itettyp(ntetsnew-1))
                     itetnn(1,ntetsnew)=i5
                     itetnn(2,ntetsnew)=j4
                     itetnn(3,ntetsnew)=j2
                     itetnn(4,ntetsnew)=npointsnew1
                     itetnn1(1,ntetsnew)=-1
                     itetnn1(2,ntetsnew)=-1
                     itetnn1(3,ntetsnew)=-1
                     itetnn1(4,ntetsnew)=-1
                     itetnn2(1,ntetsnew)=-1
                     itetnn2(2,ntetsnew)=-1
                     itetnn2(3,ntetsnew)=-1
                     itetnn2(4,ntetsnew)=-1
                  ntetsnew=ntetsnew+1
                     itflag(ntetsnew)=1
                     itetclr(ntetsnew)=itetclr(jt)
                     itettyp(ntetsnew)=itettyp(jt)
                     itetoff(ntetsnew)=nen*(ntetsnew-1)
                     jtetoff(ntetsnew)=jtetoff(ntetsnew-1)+nelmnef(
     *                         itettyp(ntetsnew-1))
                     itetnn(1,ntetsnew)=i5
                     itetnn(2,ntetsnew)=j3
                     itetnn(3,ntetsnew)=j4
                     itetnn(4,ntetsnew)=npointsnew1
                     itetnn1(1,ntetsnew)=-1
                     itetnn1(2,ntetsnew)=-1
                     itetnn1(3,ntetsnew)=-1
                     itetnn1(4,ntetsnew)=-1
                     itetnn2(1,ntetsnew)=-1
                     itetnn2(2,ntetsnew)=-1
                     itetnn2(3,ntetsnew)=-1
                     itetnn2(4,ntetsnew)=-1
                  ntetsnew=ntetsnew+1
                     itflag(ntetsnew)=1
                     itetclr(ntetsnew)=itetclr(jt)
                     itettyp(ntetsnew)=itettyp(jt)
                     itetoff(ntetsnew)=nen*(ntetsnew-1)
                     jtetoff(ntetsnew)=jtetoff(ntetsnew-1)+nelmnef(
     *                         itettyp(ntetsnew-1))
                     itetnn(1,ntetsnew)=i5
                     itetnn(2,ntetsnew)=j2
                     itetnn(3,ntetsnew)=j3
                     itetnn(4,ntetsnew)=npointsnew1
                     itetnn1(1,ntetsnew)=-1
                     itetnn1(2,ntetsnew)=-1
                     itetnn1(3,ntetsnew)=-1
                     itetnn1(4,ntetsnew)=-1
                     itetnn2(1,ntetsnew)=-1
                     itetnn2(2,ntetsnew)=-1
                     itetnn2(3,ntetsnew)=-1
                     itetnn2(4,ntetsnew)=-1
                  itflag(jt)=1
                  itetclr(jt)=itetclr(it)
                  itettyp(jt)=itettyp(it)
                  itetoff(jt)=nen*(jt-1)
                  jtetoff(jt)=nef*(jt-1)
                  itetnn(1,jt)=i1
                  itetnn(2,jt)=i3
                  itetnn(3,jt)=i4
                  itetnn(4,jt)=npointsnew1
                  itetnn1(1,jt)=-1
                  itetnn1(2,jt)=-1
                  itetnn1(3,jt)=-1
                  itetnn1(4,jt)=-1
                  itetnn2(1,jt)=-1
                  itetnn2(2,jt)=-1
                  itetnn2(3,jt)=-1
                  itetnn2(4,jt)=-1
               endif
            else
               nface1_save=nface1_save+1
               iaddorder1(nface1_save)=iaddorder1(iface)
               itadd(nface1_save)=itadd(iface)
               ifadd(nface1_save)=ifadd(iface)
               iface_p1(nface1_save)=iface_p1(iface)
               iface_p2(nface1_save)=iface_p2(iface)
               iface_p3(nface1_save)=iface_p3(iface)
            endif
   20       continue
         enddo
      endif
      if(idebug .ge.1) then
         write(logmess,'(a,i10,a,i10)')
     *      "Face-refined tets: old=",ntets," new=",ntetsnew
         call writloga('default',0,logmess,0,ierrw)
      endif
      if(ntetsnew.gt.ntets) then
         do it=1,ntetsnew
            do i=1,4
               itet(i,it)=itetnn(i,it)
            enddo
            do i=1,4
               itetnn(i,it)=iparent(itetnn(i,it))
            enddo
         enddo
         do it=1,ntets
            if(itflag(it).ne.0) then
               do i=1,4
                  kt=1+(jtet(i,it)-1)/4
                  kf=jtet(i,it)-4*(kt-1)
                  if(kt.le.ntets) then
                     itetnn1(kf,kt)=-1
                     itetnn2(kf,kt)=-1
                  endif
                  itetnn1(i,it)=-1
                  itetnn2(i,it)=-1
               enddo
            endif
         enddo
         npoints=npointsnew
         ntets=ntetsnew
         call cmo_set_info('nnodes',cmo,npoints,1,1,ierror)
         call cmo_set_info('nelements',cmo,ntets,1,1,ierror)
         call geniee(itetnn,itetnn1,itetnn2,4,4,ntets,npoints,
     *               3,npoints,ntets)
         call cmo_get_info('itetclr',cmo,ipitetclr,ilen,icmotype,ier)
         do it=1,ntets
            do i=1,4
               if(itetnn1(i,it).gt.0.and.itetnn1(i,it).le.ntets) then
                  if(itetclr(it).eq.itetclr(itetnn1(i,it))) then
                     jtet(i,it)=4*(itetnn1(i,it)-1)+itetnn2(i,it)
                  else
                     jtet(i,it)=mbndry+4*(itetnn1(i,it)-1)+itetnn2(i,it)
                  endif
               else
                  jtet(i,it)=mbndry
               endif
            enddo
         enddo
         if(nface1_save.gt.0) then
            nface1=nface1_save
            goto 11
         endif
      endif
      goto 9999
 9999 continue
C
      cmolength='nnodes'
      call cmo_interpolate(cmo_name,cmo_name,
     *                     cmolength,
     *                     nadd1,nvalues,
     *                     list_sink,list_source,xweight_source,
     *                     ierror)
      if(ierror.ne.0) call x3d_error(isubname,'cmo_interpolate')
C
      call cmo_get_info('itp1',cmo,ipitp1,ilen,icmotype,ier)
      call cmo_get_info('isn1',cmo,ipisn1,ilen,icmotype,ier)
      call cmo_get_info('xic',cmo,ipxic,ilen,icmotype,ier)
      call cmo_get_info('yic',cmo,ipyic,ilen,icmotype,ier)
      call cmo_get_info('zic',cmo,ipzic,ilen,icmotype,ier)
C
      call cmo_get_info('itp1',cmo,ipitp1,length,icmotype,ierror)
      length=npoints
      call mmgetblk('int1',isubname,ipint1,length,1,icscode)
      call unpacktp('intrface','set',npoints,ipitp1,ipint1,ierrdum)
      if(ierrdum.ne.0) call x3d_error('refine_edge_add', 'unpacktp')
C
      icount=0
      do i=1,nadd1
         i1=list_sink(i)
         i2=iaddorder2(i)
         if(int1(i1).eq.1) then
            jcount=jcount+1
         endif
         if(int1add(i).eq.1) then
            itp1(i1)=ifitpini
            icount=icount+1
         endif
         isn1(i1)=0
         xic(i1)=xadd(i2)
         yic(i1)=yadd(i2)
         zic(i1)=zadd(i2)
         itp1(i1)=itpnew(i2)
         icr1(i1)=icrnew(i2)
      enddo
C
      call cmo_get_name(cmo,ierror)
      call cmo_set_info('nnodes',cmo,npoints,1,1,ierror)
      call cmo_set_info('nelements',cmo,ntets,1,1,ierror)
C
      if(jcount.gt.0.or.icount.gt.0) then
         call dotaskx3d('settets/parents ; finish',ierror)
         call cmo_get_info('nnodes',cmo,npoints,length,icmotype,ierror)
         call cmo_get_info('nelements',cmo,ntets,length,icmotype,ierror)
      endif
C     call cmo_get_name(cmo,ierror)
      call cmo_set_info('nnodes',cmo,npoints,1,1,ierror)
      call cmo_set_info('nelements',cmo,ntets,1,1,ierror)
      call cmo_newlen(cmo,ierror)
      call mmrelprt(isubname,icscode)
C
      return
      end
