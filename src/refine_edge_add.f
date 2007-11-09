c
      subroutine refine_edge_add(cmo_name,
     *                           nadd,
     *                           ipitadd,ipieadd,
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
C        (ip)ieadd  - Integer pointer to the list of local edges where
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
C        $Log:   /pvcs.config/t3d/src/refine_edge_add.f_a  $
CPVCS    
CPVCS       Rev 1.52   01 Mar 2002 14:45:26   dcg
CPVCS    adaptive merging
CPVCS    
CPVCS       Rev 1.50   30 Jan 2002 15:56:00   dcg
CPVCS    break up an if statement that might reference a null pointer
CPVCS    
CPVCS       Rev 1.49   30 Jan 2002 10:46:02   dcg
CPVCS    remove duplicate declarations
CPVCS
CPVCS       Rev 1.48   07 Jan 2002 13:40:50   dcg
CPVCS    add call to testdamage_refine - this test performs
CPVCS    the standard lagrit damage test which measures
CPVCS    the depth of the divot created on the surface
CPVCS    add parameter to call to refine_edge_add_tet to have
CPVCS    it return a error condition flag
CPVCS
CPVCS       Rev 1.47   02 Jan 2002 16:38:58   dcg
CPVCS    changes for 'discrete' refinement option
CPVCS
CPVCS       Rev 1.46   20 Dec 2001 16:22:56   dcg
CPVCS    changes for discrete option
CPVCS
CPVCS       Rev 1.44   Thu Apr 06 13:51:24 2000   dcg
CPVCS    replace get_info_i call
CPVCS
CPVCS       Rev 1.43   Mon Aug 30 15:16:42 1999   dcg
CPVCS    remove calls to ssort routines replace with hpsort
CPVCS
CPVCS       Rev 1.42   Thu Aug 26 13:16:10 1999   dcg
CPVCS    set isn1 of new nodes to zero
CPVCS    this will be reset by the call to settets
CPVCS
CPVCS       Rev 1.41   Mon Jun 14 17:35:06 1999   dcg
CPVCS    change argument in call to refine_face_add to ipiadd so as to
CPVCS    be able to use call to refine_fix_add from refine_face_add
CPVCS
CPVCS       Rev 1.40   Fri Apr 09 09:33:36 1999   dcg
CPVCS    in setting pset membership for new nodes - ignore
CPVCS    modifications if new node is not an interface node (if
CPVCS    no parent/child chain exits)
CPVCS
CPVCS       Rev 1.39   Wed Mar 17 10:52:48 1999   dcg
CPVCS    set pset membership of child nodes in refine_edge_add
CPVCS
CPVCS       Rev 1.38   Fri Jan 22 15:37:34 1999   dcg
CPVCS    remove duplicate declaration
CPVCS
CPVCS       Rev 1.37   Fri Oct 23 13:15:16 1998   dcg
CPVCS    move declaration of nadd before use - DEC compiler complaint
CPVCS
CPVCS       Rev 1.36   Tue Sep 22 13:40:34 1998   dcg
CPVCS    declare crosx and volume inline functions
CPVCS
CPVCS       Rev 1.35   Tue Sep 22 13:00:34 1998   dcg
CPVCS    move memory enlargement before potential memory overwrite
CPVCS    make subroutine implicit none
CPVCS
CPVCS       Rev 1.34   Wed Mar 11 15:53:28 1998   dcg
CPVCS    call settets/newtets in place of settets/parents
CPVCS    so that no itetclr values are overwritten
CPVCS
CPVCS       Rev 1.33   Tue Mar 03 12:36:36 1998   dcg
CPVCS    fix duplicate declarations and number of arguments in x3d_error
CPVCS
CPVCS       Rev 1.32   Mon Nov 03 13:23:44 1997   dcg
CPVCS    set isubname to match subroutine name
CPVCS
CPVCS       Rev 1.31   Wed Oct 08 16:54:52 1997   dcg
CPVCS    No change.
CPVCS
CPVCS       Rev 1.30   Tue Sep 16 12:15:00 1997   dcg
CPVCS    fix isubname
CPVCS
CPVCS       Rev 1.29   Tue Jul 15 16:05:12 1997   dcg
CPVCS    add call to refine_fix_add to correct point types and
CPVCS    constraint values (itp1, and icr1)
CPVCS
CPVCS       Rev 1.28   Thu Jun 05 10:28:10 1997   dcg
CPVCS    disable refine on mega algorithm by
CPVCS    requiring that ivoronoi be set to 999
CPVCS    to activate.  Algorithm was causing
CPVCS    infinite loops and other strange behavior
CPVCS    This logic should be moved to a higher level
CPVCS    subroutine and debugged.
CPVCS
CPVCS       Rev 1.27   Tue Apr 22 13:35:40 1997   dcg
CPVCS    refresh edge lists after each iteration
CPVCS
CPVCS       Rev 1.26   Mon Apr 14 16:59:22 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.25   Fri Jan 24 14:27:58 1997   het
CPVCS    No change.
CPVCS
CPVCS       Rev 1.24   Thu Oct 17 16:14:40 1996   dcg
CPVCS
CPVCS
CPVCS
CPVCS
CPVCS    add ivoronoi=-2 option to set igeom_gwt to 1
CPVCS
CPVCS       Rev 1.23   Thu Jun 27 14:56:36 1996   het
CPVCS    For addpts use the names of points without duplicating the points.
CPVCS
CPVCS       Rev 1.22   Fri May 24 13:59:14 1996   het
CPVCS    Correct an error in the addpts option for edge points.
CPVCS
CPVCS       Rev 1.21   Tue May 07 17:24:20 1996   het
CPVCS    Add the GWT reconnection algorithm.
CPVCS
CPVCS       Rev 1.20   Tue Apr 30 07:28:44 1996   het
CPVCS    Fix a error
CPVCS
CPVCS       Rev 1.19   Tue Apr 02 02:28:50 1996   het
CPVCS    Change this routine to give new nodes names.
CPVCS
CPVCS       Rev 1.18   Thu Mar 14 13:39:04 1996   het
CPVCS    Change the call to the refine commands to add names.
CPVCS
CPVCS       Rev 1.17   11/16/95 17:13:08   het
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
      include 'local_element.h'
      include 'chydro.h'  
      include 'massage.h'
C
C ######################################################################
C
C
      character*(*) cmo_name
      integer nadd
      pointer (ipitadd, itadd)
      integer itadd(nadd)
      pointer (ipieadd, ieadd)
      integer ieadd(nadd)
      pointer (ipiadd, iadd)
      integer iadd(nadd)
      pointer (ipxadd, xadd)
      pointer (ipyadd, yadd)
      pointer (ipzadd, zadd)
      real*8 xadd(nadd), yadd(nadd), zadd(nadd)
C
      pointer (ipitp1, itp1)
      pointer (ipicr1, icr1)
      pointer (ipisn1, isn1)
      integer itp1(1000000),icr1(1000000),isn1(1000000)
      pointer (ipitetclr, itetclr)
      pointer (ipitettyp, itettyp)
      pointer (ipitetoff, itetoff)
      pointer (ipjtetoff, jtetoff)
      integer itetclr(1000000), itettyp(1000000),
     *        itetoff(1000000), jtetoff(1000000)
C
      pointer (ipitet, itet)
      pointer (ipitet, itet1)
      pointer (ipjtet, jtet)
      pointer (ipjtet, jtet1)
      integer itet(4,1000000), jtet(4,1000000)
      integer itet1(4*1000000), jtet1(4*1000000)
      pointer (ipitpadd,itpadd)
      pointer (ipicradd,icradd)
      integer itpadd(1000000),icradd(1000000)
C
      integer i,icscode,ilen,itype,it,ie,i2p,i3p,j,i2,i3,
     *   nef,nen,nsd,node,node1,icharlnf,nnodes,flag
      character*32 isubname
C
C#######################################################################
C
      isubname='refine_edge_add'
      call cmo_exist(cmo_name,icscode)
      if(icscode.ne.0) then
         write(logmess,9000) cmo_name(1:icharlnf(cmo_name))
 9000    format('CMO does not exist: ',a)
         call writloga('default',1,logmess,1,icscode)
         goto 9999
      endif
C
      call cmo_get_info('nodes_per_element',cmo_name,
     *                  nen,ilen,itype,icscode)
      call cmo_get_info('faces_per_element',cmo_name,
     *                  nef,ilen,itype,icscode)
      call cmo_get_info('ndimensions_topo',cmo_name,
     *                  nsd,ilen,itype,icscode)
      call cmo_get_info('itetclr',cmo_name,
     *                  ipitetclr,ilen,itype,icscode)
      call cmo_get_info('itettyp',cmo_name,
     *                  ipitettyp,ilen,itype,icscode)
      call cmo_get_info('itetoff',cmo_name,
     *                  ipitetoff,ilen,itype,icscode)
      call cmo_get_info('jtetoff',cmo_name,
     *                  ipjtetoff,ilen,itype,icscode)
      call cmo_get_info('itet',cmo_name,ipitet,ilen,itype,icscode)
      call cmo_get_info('jtet',cmo_name,ipjtet,ilen,itype,icscode)
C
C  get temporary memory
C
      call mmgetblk('itpadd',isubname,ipitpadd,nadd,1,icscode)
      call mmgetblk('icradd',isubname,ipicradd,nadd,1,icscode)
C
      if(nen.eq.nelmnen(ifelmtri).and.nef.eq.nelmnef(ifelmtri)) then
         do i=1,nadd
            it=itadd(i)
            ie=ieadd(i)
            i2p=itet1(itetoff(it)+ielmedge1(1,ie,itettyp(it)))
            i3p=itet1(itetoff(it)+ielmedge1(2,ie,itettyp(it)))
            do j=1,nelmnef(itettyp(it))
               i2=itet1(itetoff(it)+ielmface1(1,j,itettyp(it)))
               i3=itet1(itetoff(it)+ielmface1(2,j,itettyp(it)))
               if((i2.eq.i2p.and.i3.eq.i3p) .or.
     *            (i3.eq.i2p.and.i2.eq.i2p)) then
                  ieadd(i)=j
               endif
            enddo
         enddo
         call refine_face_add_tri(cmo_name,
     *                            nadd,
     *                            ipitadd,ipieadd,
     *                            ipiadd,xadd,yadd,zadd)
      elseif(nen.eq.nelmnen(ifelmtet).and.nef.eq.nelmnef(ifelmtet).and.
     *       nsd.eq.3) then
         call refine_fix_add(cmo_name,nadd,ipitadd,ipieadd,ipiadd,
     *       ipitpadd,ipicradd)
         call refine_edge_add_tet(cmo_name,
     *                            nadd,
     *                            ipitadd,ipieadd,
     *                            iadd,xadd,yadd,zadd,flag)
c.... Fix up ITP1, ICR1 values.
 
         call cmo_get_info('nnodes',cmo_name,
     *      nnodes,ilen,itype,icscode)
         call cmo_get_info('itp1',cmo_name,
     *      ipitp1,ilen,itype,icscode)
         call cmo_get_info('icr1',cmo_name,
     *      ipicr1,ilen,itype,icscode)
         call cmo_get_info('isn1',cmo_name,
     *      ipisn1,ilen,itype,icscode)
         do i = 1,nadd
            if (iadd(i).ne.0) then
               node=iabs(iadd(i))
               if (isn1(node).eq.0) then
                  itp1(node)=itpadd(i)
                  icr1(node)=icradd(i)
               else
                  if (itp1(node).ne.ifitpcup) then
                     itp1(node)=itpadd(i)
                     icr1(node)=icradd(i)
                  endif
                  node1=isn1(node)
                  do while (node1.ne.node)
                     if (itp1(node1).ne.ifitpcup) then
                        itp1(node1)=itpadd(i)
                        icr1(node1)=icradd(i)
                     endif
                     node1=isn1(node1)
                  enddo
               endif
            endif
         enddo
      else
         write(logmess,9010) cmo_name(1:icharlnf(cmo_name))
 9010    format('Refine on this CMO type is not implemented: ',a)
         call writloga('default',1,logmess,1,icscode)
         goto 9999
      endif
C
      goto 9999
 9999 continue
      call mmrelprt(isubname,icscode)
      return
      end
c
      subroutine refine_edge_add_tet(cmo_name,nadd,ipitadd,ipieadd,
     *                               iadd,xadd,yadd,zadd,oflag)
 
C
C #####################################################################
C
C     PURPOSE -
C
C        This routine refines element edges by adding nodes
C
C     INPUT ARGUMENTS -
C
C        cmo_name - The name of the CMO.
C        nadd     - Number of nodes to add.
C        (ip)itadd  - Integer pointer to the list of elements where
C                        nodes will be added.
C        (ip)ieadd  - Integer pointer to the list of local edges where
C                        nodes will be added.
C        (ip)iadd(nadd) - The "names" of the nodes to add.
C        (ip)xadd(nadd) - The X-coordinate of the nodes to add.
C        (ip)yadd(nadd) - The Y-coordinate of the nodes to add.
C        (ip)zadd(nadd) - The Z-coordinate of the nodes to add.
C
C     OUTPUT ARGUMENTS -
C
c        oflag           -  0 no error
c                          1 refinment failed
C     CHANGE HISTORY -
C
C        $Log:   /pvcs.config/x3d/src/refine_edge_add.f_a  $
CPVCS
CPVCS       Rev 1.16   11/07/95 17:24:34   dcg
CPVCS    change flag to 2 in mmgetblk calls
CPVCS
CPVCS       Rev 1.15   10/22/95 13:19:32   het
CPVCS    Fix a memory management error
CPVCS
CPVCS       Rev 1.14   10/20/95 10:48:20   het
CPVCS    Fix iparent memory management error and add new refine options.
CPVCS
CPVCS       Rev 1.13   10/12/95 16:45:12   het
CPVCS    Correct the X3D restart dump routines
CPVCS
CPVCS       Rev 1.12   10/05/95 15:46:58   het
CPVCS    Add the intrface refinement option
CPVCS
CPVCS       Rev 1.11   10/04/95 07:44:44   het
CPVCS    Add the addpts option to the refine commands
CPVCS
CPVCS       Rev 1.10   09/29/95 09:13:56   het
CPVCS    Put in added attributes inheritance
CPVCS
CPVCS       Rev 1.9   08/15/95 18:23:24   het
CPVCS    Cleanup code and correct errors
CPVCS
CPVCS       Rev 1.8   06/27/95 16:37:26   dcg
CPVCS    remove second literal argument in memory management calls
CPVCS
CPVCS       Rev 1.7   06/05/95 10:36:50   het
CPVCS    Make changes for hybrid_grids
CPVCS
CPVCS       Rev 1.6   05/26/95 13:18:08   het
CPVCS    Replace subroutine parameter list with subroutine calles.
CPVCS
CPVCS       Rev 1.5   03/28/95 12:34:42   het
CPVCS    Add the binary dumpx3d/readx3d commands and correct associated mm-errors.
CPVCS
CPVCS       Rev 1.4   03/14/95 08:57:56   het
CPVCS    Correct a boundary flag error
CPVCS
CPVCS       Rev 1.3   03/13/95 16:15:00   het
CPVCS    Get mbndry from cmo and fixed 1st edge selection error
CPVCS
CPVCS       Rev 1.2   02/08/95 18:05:26   het
CPVCS    Correct a memory management error
CPVCS
CPVCS       Rev 1.1   12/27/94 23:02:24   het
CPVCS    Fixed an error when refining a boundary edge.
CPVCS
CPVCS
CPVCS       Rev 1.0   11/10/94 12:17:58   pvcs
CPVCS    Original version.
C
C     ##################################################################
C
       implicit none
C
      character*132 logmess,cbuff
C
C     ##################################################################
C
      include 'chydro.h'
      include 'consts.h'
      include 'local_element.h'
      include 'neibor.h'
      include 'cmo.h'
      include 'massage.h'
C
C     ##################################################################
C
      character*(*) cmo_name
C
      integer nadd
      pointer (ipitadd, itadd)
      pointer (ipieadd, ieadd)
      integer itadd(nadd), ieadd(nadd)
C
      integer iadd(nadd)
      real*8 xadd(nadd), yadd(nadd), zadd(nadd)
C
      pointer (ipint1add, int1add)
      integer int1add(nadd)
C
      pointer (ipiaddorder1, iaddorder1)
      pointer (ipiaddorder2, iaddorder2)
      integer iaddorder1(nadd), iaddorder2(nadd)
C
 
      pointer (ipitetoff, itetoff)
      pointer (ipjtetoff, jtetoff)
      integer itetoff(1000000), jtetoff(1000000)
C
      pointer (ipiparent, iparent)
      integer iparent(1000000)
C
      pointer (ipint1, int1)
      integer int1(1000000)
C
      pointer (ipitflag, itflag)
      pointer (ipitetnn, itetnn)
      pointer (ipitetnn1, itetnn1)
      pointer (ipitetnn2, itetnn2)
      integer itflag(1000000),
     *        itetnn(4,1000000), itetnn1(4,1000000), itetnn2(4,1000000)
C
      pointer (ipiedge_tet, iedge_tet)
      pointer (ipiedge_face, iedge_face)
      pointer (ipiedge_edge, iedge_edge)
      pointer (ipiedge_p1, iedge_p1)
      pointer (ipiedge_p2, iedge_p2)
      integer iedge_tet(6*1000000), iedge_face(6*1000000),
     *        iedge_edge(6*1000000), iedge_p1(6*1000000),
     *        iedge_p2(6*1000000)
C
      pointer (ipelts,elts)
      integer elts(*)
      pointer (ipedges,edges)
      integer edges(*)
      pointer (ipfaces,faces)
      integer faces(*)
      pointer (ipxics,xics),(ipyics,yics),(ipzics,zics)
      real*8 xics(*),yics(*),zics(*)
c
      integer nvalues
      parameter (nvalues=2)
      pointer (iplist_sink, list_sink)
      pointer (iplist_source, list_source)
      pointer (ipxweight_source, xweight_source)
      pointer (ipout,outs)
      integer outs(*)
      integer list_sink(nadd), list_source(nvalues,nadd)
      real*8 xweight_source(nvalues,nadd)
C
      pointer (ipitets,itets)
      integer itets(3,*)
      pointer (iplinkt, linkt)
      pointer (ipsbox, sbox)
      integer linkt(*)
      real*8 sbox(*)
      pointer (ipitfound,itfound)
      integer itfound(*)
      pointer (ipisurftst,isurftst)
      integer isurftst(*)
      pointer (ipwork,work)
      real*8 work(*)
c
      character*32 cmolength
      character*32 isubname, iblknam, iprtnam
      logical  isboundary
      character*8 eq
C
C#######################################################################
C
      integer ierror,npoints,icmotype,ntets,nen,nef,
     *  npointsinc,nelementsmm,inc,ntetsinc,nnodesmm,ics,idum,inc1,
     *  inc2,ilen,jj,jtettmp,nadd1,iedgeiter,npointsnew,
     *  ntetsnew,npointsnew1,nedge_save,jcount,ktlast,kflast,kelast,
     *  irefine,ierrw,kt,kf,itype,l1,kk,l2,it1,it2,oflag,
     *  jnew,inew,itnew,iedge,nedge,j2,j3,ip1,ip2,
     *  ie,it,ier,ict,ielt,indx,n1,n2,n3,n4,
     *  i,j,k,i1,i2,i3,i4,length,icscode,flag,
     *  imtmatch,istrta,istrtb,istrtc,nelts,if,iout,
     *  nelementss,nfound,icand,ipoint,minpt,
     *  node1,node2,node3,node4,ifac2,ifac1
      real*8 xa,ya,za,voltet1,voltet2,voltetold,voltetnew,
     *  rout,qx,qy,qz,xold,yold,zold,epsln,damage,epslnv,
     *  distmax,dist,x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4
c
      include 'statementfunctions.h'
c
C     ###################################################################
C
c   initialize
      oflag=0
      icmoget=1
      isubname='refine_edge_add_tet'
      call get_epsilon('epsilonl',epsln)
      call get_epsilon('epsilonv',epslnv)
      xold=1.e+30
      yold=1.e+30
      zold=1.e+30
C
c....
c.... Create temporary storage for elements that share an edge and
c.... their local edge numbers
c....
      nelts=100
      call mmgetblk('elts',isubname,ipelts,nelts,1,icscode)
      call mmgetblk('edges',isubname,ipedges,nelts,1,icscode)
      call mmgetblk('faces',isubname,ipfaces,nelts,1,icscode)
c
      length=nadd
      call mmgetblk('int1add',isubname,ipint1add,length,1,icscode)
      call mmgetblk('iaddorder1',isubname,ipiaddorder1,length,1,icscode)
      call mmgetblk('iaddorder2',isubname,ipiaddorder2,length,1,icscode)
      do i=1,nadd
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
C
      call cmo_get_intinfo('ivoronoi',cmo,
     *                ivoronoi,ilen,icmotype,icscode)
         if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
C
      call cmo_get_info('itp1',cmo,ipitp1,ilen,icmotype,ier)
      call cmo_get_info('isetwd',cmo,ipisetwd,ilen,icmotype,ier)
      call cmo_get_info('imt1',cmo,ipimt1,ilen,icmotype,ier)
      call cmo_get_info('isn1',cmo,ipisn1,ilen,icmotype,ier)
      call cmo_get_info('xic',cmo,ipxic,ilen,icmotype,ier)
      call cmo_get_info('yic',cmo,ipyic,ilen,icmotype,ier)
      call cmo_get_info('zic',cmo,ipzic,ilen,icmotype,ier)
      call cmo_get_info('itetclr',cmo,ipitetclr,ilen,icmotype,ier)
      call cmo_get_info('itettyp',cmo,ipitettyp,ilen,icmotype,ier)
      call cmo_get_info('itetoff',cmo,ipitetoff,ilen,icmotype,ier)
      call cmo_get_info('jtetoff',cmo,ipjtetoff,ilen,icmotype,ier)
      call cmo_get_info('itet',cmo,ipitet,ilen,icmotype,ierror)
      call cmo_get_info('jtet',cmo,ipjtet,ilen,icmotype,ierror)
C
c  see if discrete refinement cmo has been set
c  set up kdtree for surface after getting mesh object info
c  for surface mesh
c
      damage=one
      if(isdiscrete) then
         call cmo_get_intinfo('nelements',surfcmo_name,nelementss,
     *      ilen,icmotype,ier)
         if(ier.ne.0) then
            call x3d_error(surfcmo_name,'cmo_get')
            go to 9999
         endif
         if(toldamage_discrete.ne.zero) damage=toldamage_discrete
         call cmo_get_info('xic',surfcmo_name,ipxics,
     *      ilen,icmotype,ier)
         call cmo_get_info('yic',surfcmo_name,ipyics,
     *      ilen,icmotype,ier)
         call cmo_get_info('zic',surfcmo_name,ipzics,
     *      ilen,icmotype,ier)
         call cmo_get_info('itet',surfcmo_name,ipitets,
     *      ilen,icmotype,ier)
C
C     ADD ATTRIBUTES FOR k-D TREE OF THE surface if needed
C
         call cmo_get_info('linkt',surfcmo_name,iplinkt,
     *      ilen,icmotype,ier)
         if(ier.ne.0) then
            cbuff='cmo/addatt/' //
     &      '/' //
     &      'v2' //
     &      '/INT' //
     &      '/scalar/scalar/constant/permanent//2.0' //
     &      ' ; finish'
            call dotaskx3d(cbuff,ierror)
            cbuff='cmo/addatt/' //
     &      '/' //
     &      'linkt' //
     &      '/VINT' //
     &      '/v2/nelements//permanent/x/0.0' //
     &      ' ; finish'
            call dotaskx3d(cbuff,ierror)
         endif
C
         call cmo_get_info('sbox',surfcmo_name,ipsbox,
     *      ilen,icmotype,ier)
         if(ier.ne.0) then
            cbuff='cmo/addatt/' //
     &      '/' //
     &      'v12' //
     &      '/INT' //
     &      '/scalar/scalar/constant/permanent//12.0' //
     &      ' ; finish'
            call dotaskx3d(cbuff,ierror)
            cbuff='cmo/addatt/' //
     &      '/' //
     &      'sbox' //
     &      '/VDOUBLE' //
     &      '/v12/nelements/linear/permanent/x/0.0' //
     &      ' ; finish'
            call dotaskx3d(cbuff,ierror)
            call kdtree(xics,yics,zics,itets,nelementss,
     *      linkt,sbox,ier)
         endif
c
c  get space for local arrays
c
         call mmgetblk('itfound',isubname,ipitfound,5*nelementss
     *      ,1,icscode)
         call mmgetblk('isurftst',isubname,ipisurftst,npoints
     *      ,1,icscode)
         call mmgetblk('work',isubname,ipwork,nelementss
     *      ,2,icscode)
c
c  find nodes that are on surface
c
         eq='eq'
         call shttstv(xic,yic,zic,npoints,epsln,surfcmo_name,
     *      eq,isurftst)
      endif
C
C     ******************************************************************
C
C     Get the parents for each node.
C
      call mmfindbk('xic',cmo,ipxic,length,icscode)
      call mmgetblk('iparent',isubname,ipiparent,length,1,icscode)
      call unpackpc(npoints,itp1,isn1,iparent)
C
C     ******************************************************************
C
C     Do we have an interface:
C        int1() =  0 ==> not an interface point.
C        int1() =  1 ==> an interface point.
C
      call mmfindbk('xic',cmo,ipxic,length,icscode)
      call mmgetblk("int1",isubname,ipint1,length,2,icscode)
      call unpacktp("intrface","set",npoints,ipitp1,ipint1,ierror)
      if(ierror.ne.0) call x3d_error('refine_edge_add', 'unpacktp')
C
C     ******************************************************************
C
      length=3*nadd
      call mmgetblk('iedgetet',isubname,ipiedge_tet,length,1,icscode)
      call mmgetblk('iedgefac',isubname,ipiedge_face,length,1,icscode)
      call mmgetblk('iedgeedg',isubname,ipiedge_edge,length,1,icscode)
      call mmgetblk('iedgep1',isubname,ipiedge_p1,length,1,icscode)
      call mmgetblk('iedgep2',isubname,ipiedge_p2,length,1,icscode)
      length=ntets
      call mmgetblk('itflag',isubname,ipitflag,length,1,icscode)
      length=4*ntets
      call mmgetblk('itetnn' ,isubname,ipitetnn ,length,1,icscode)
      call mmgetblk('itetnn1',isubname,ipitetnn1,length,1,icscode)
      call mmgetblk('itetnn2',isubname,ipitetnn2,length,1,icscode)
      nedge=0
      itype=5
c
c  find a face that goes with this tet,edge pair
c
      do k=1,nadd
         it=itadd(k)
         ie=ieadd(k)
         ip1=itet(ielmedge1(1,ie,itype),it)
         ip2=itet(ielmedge1(2,ie,itype),it)
         do i=1,4
            do j=1,3
               kk=ielmface2(j,i,itype)
               j2=itet(ielmedge1(1,kk,itype),it)
               j3=itet(ielmedge1(2,kk,itype),it)
               if((ip1.eq.j2.and.ip2.eq.j3).or.
     *            (ip1.eq.j3.and.ip2.eq.j2)) then
                  nedge=nedge+1
                  iedge_tet(nedge)=it
                  iedge_face(nedge)=i
                  iedge_edge(nedge)=kk
                  iedge_p1(nedge)=ip1
                  iedge_p2(nedge)=ip2
                  iadd(nedge)=iadd(k)
                  xadd(nedge)=xadd(k)
                  yadd(nedge)=yadd(k)
                  zadd(nedge)=zadd(k)
                  goto 5
               endif
            enddo
         enddo
   5     continue
      enddo
C
      call cmo_get_intinfo('nnodes',cmo,npoints,length,icmotype,ierror)
      call cmo_get_intinfo('nelements',cmo,ntets,length,icmotype,ierror)
      call cmo_get_intinfo('mbndry',cmo,mbndry,length,icmotype,ierror)
      call cmo_get_intinfo('nodes_per_element',cmo,
     *                  nen,length,icmotype,ierror)
      call cmo_get_intinfo('faces_per_element',cmo,
     *                  nef,length,icmotype,ierror)
      call cmo_get_info('itp1',cmo,ipitp1,ilen,icmotype,ier)
      call cmo_get_info('imt1',cmo,ipimt1,ilen,icmotype,ier)
      call cmo_get_info('isetwd',cmo,ipisetwd,ilen,icmotype,ier)
      call cmo_get_info('isn1',cmo,ipisn1,ilen,icmotype,ier)
      call cmo_get_info('itetclr',cmo,ipitetclr,ilen,icmotype,ier)
      call cmo_get_info('itettyp',cmo,ipitettyp,ilen,icmotype,ier)
      call cmo_get_info('itetoff',cmo,ipitetoff,ilen,icmotype,ier)
      call cmo_get_info('jtetoff',cmo,ipjtetoff,ilen,icmotype,ier)
      call cmo_get_info('itet',cmo,ipitet,ilen,icmotype,ierror)
      call cmo_get_info('jtet',cmo,ipjtet,ilen,icmotype,ierror)
C
      do j=1,nedge
         int1add(j)=0
         iaddorder1(j)=j
         iaddorder2(j)=0
      enddo
      nadd1=0
      iedgeiter=0
  10  continue
c
c  put tet number of neighbor in itetnn1
c  put face number of neighbor in itetnn2
c
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
c
      iedgeiter=iedgeiter+1
      write(logmess,'(a,2i10)') 'Edge_add iteration: ',iedgeiter,nedge
      call writloga('default',0,logmess,0,ierrw)
      irefine=0
      npointsnew=npoints
      ntetsnew=ntets
      do it=1,ntets
         itflag(it)=0
      enddo
      nedge_save=0
c
c  start loop through edges to be refined
c  get tet number, face, edge and nodes
c  j2,j3 are node numbers of edge to be refined
c
      do iedge=1,nedge
         isboundary=.false.
         it=iedge_tet(iedge)
         i=iedge_face(iedge)
         j=iedge_edge(iedge)
         j2=iparent(itet(ielmedge1(1,j,itype),it))
         j3=iparent(itet(ielmedge1(2,j,itype),it))
         ip1=iparent(iedge_p1(iedge))
         ip2=iparent(iedge_p2(iedge))
         if((ip1.eq.j2.and.ip2.eq.j3) .or.
     *      (ip1.eq.j3.and.ip2.eq.j2)) then
         else
            do itnew=1,ntetsnew
               do inew=1,6
                  j2=iparent(itetnn(ielmedge1(1,inew,itype),itnew))
                  j3=iparent(itetnn(ielmedge1(2,inew,itype),itnew))
                  if((ip1.eq.j2.and.ip2.eq.j3).or.
     *               (ip1.eq.j3.and.ip2.eq.j2)) then
                     it=itnew
                     i=ielmedge2(1,inew,itype)
                     j=inew
                     goto 51
                  endif
               enddo
            enddo
            write(logmess,52) ip1,ip2
   52       format ('Could not match edge: ',2i10)
            call writloga('default',0,logmess,0,icscode)
            goto 130
   51       continue
            iedge_tet(iedge)=it
            iedge_face(iedge)=i
            iedge_edge(iedge)=j
            j2=iparent(itet(ielmedge1(1,j,itype),it))
            j3=iparent(itet(ielmedge1(2,j,itype),it))
         endif
c
c  check itflag - if it is not zero this tet should
c  not be refined this iteration because it was
c  already refined
c  so save these skipped edges to do later
c
         if(itflag(it).ne.0) then
            nedge_save=nedge_save+1
            iedge_tet(nedge_save)=it
            iedge_face(nedge_save)=i
            iedge_edge(nedge_save)=j
            iedge_p1(nedge_save)=iedge_p1(iedge)
            iedge_p2(nedge_save)=iedge_p2(iedge)
            itadd(nedge_save)=itadd(iedge)
            ieadd(nedge_save)=ieadd(iedge)
            iaddorder1(nedge_save)=iaddorder1(iedge)
            goto 130
         endif
C
c  get all elements that contain this edge
c  if we are doing discrete refinement see if
c  any are boundary nodes
c
        call get_elements_on_edge_test(it,j,nelts,
     *     ipelts,ipfaces,ipedges,ipitetoff,ipjtetoff,ipitet,
     *     ipjtet,ipitettyp,ipiparent,nef,mbndry,flag)
        if(isdiscrete.and.flag.ge.2)then
           if(isurftst(j2).eq.1.and .isurftst(j3).eq.1) then
c
c  get nearest point on surface to this node
c
              qx=(xic(j2)+xic(j3))/two
              qy=(yic(j2)+yic(j3))/two
              qz=(zic(j2)+zic(j3))/two
              call nearestpoint(qx,qy,qz,xics,yics,zics,itets,
     *                  xold,yold,zold,
     *                  linkt,sbox,
     *                  epsln,work,nfound,itfound,ierror)
C
              if (nfound .eq. 0) then
                write(logmess,'(a)') 'Error in refine:
     &          kd tree returns no triangles'
                call writloga('default',0,logmess,0,ierror)
                go to 9999
              endif
c
c  loop through candidate nodes find closest which
c  does not cause inversion or damage (i.e. sum
c  of new volumes all of which must be positive
c  must be equal to old volume)
c
  55          indx=0
              distmax=1.e+30
              voltetold=zero
              voltetnew=zero
              do icand=1,nfound
                if(itfound(icand).ne.0) then
                   do kk=1,3
                     ipoint=itets(kk,itfound(icand))
                     dist=dlen(qx,qy,qz,xics(ipoint),yics(ipoint),
     *               zics(ipoint))
                     if(dist.lt.distmax) then
                        distmax=dist
                        minpt=ipoint
                        indx=icand
                     endif
                   enddo
                 endif
              enddo
              if(indx.eq.0) then
                 write(logmess,'(a,2i10,a,a)') 'refining edge ',
     *            j2,j3, ' will invert tetrahedra ',
     *           'or cause unacceptable damage'
                 call writloga('default',0,logmess,0,ierror)
                 go to 130
              endif
              xa=xics(minpt)
              ya=yics(minpt)
              za=zics(minpt)
              xold=xa
              yold=ya
              zold=za
c
              isboundary=.true.
              do ielt=1,nelts
c  make sure none of the new tets will be inverted
                 it1=elts(ielt)
                 if=faces(ielt)
                 ie=edges(ielt)
                 i1=itet(1,it1)
                 i2=itet(2,it1)
                 i3=itet(3,it1)
                 i4=itet(4,it1)
                 x1=xic(i1)
                 y1=yic(i1)
                 z1=zic(i1)
                 x2=xic(i2)
                 y2=yic(i2)
                 z2=zic(i2)
                 x3=xic(i3)
                 y3=yic(i3)
                 z3=zic(i3)
                 x4=xic(i4)
                 y4=yic(i4)
                 z4=zic(i4)
                 call volume_tet(x1,y1,z1,x2,y2,z2,
     *            x3,y3,z3,x4,y4,z4,voltet1)
                 voltetold=voltetold+voltet1
                 l1=ielmedge1(1,ie,itype)
                 l2=ielmedge1(2,ie,itype)
                 if(l1.eq.1) then
                    x1=xa
                    y1=ya
                    z1=za
                 elseif(l1.eq.2) then
                    x2=xa
                    y2=ya
                    z2=za
                 elseif(l1.eq.3) then
                    x3=xa
                    y3=ya
                    z3=za
                 elseif(l1.eq.4) then
                    x4=xa
                    y4=ya
                    z4=za
                 endif
                 call volume_tet(x1,y1,z1,x2,y2,z2,
     *            x3,y3,z3,x4,y4,z4,voltet1)
                 if(voltet1.le.epslnv) then
                    itfound(indx)=0
                    go to 55
                 endif
                 voltetnew=voltetnew+voltet1
                 x1=xic(i1)
                 y1=yic(i1)
                 z1=zic(i1)
                 x2=xic(i2)
                 y2=yic(i2)
                 z2=zic(i2)
                 x3=xic(i3)
                 y3=yic(i3)
                 z3=zic(i3)
                 x4=xic(i4)
                 y4=yic(i4)
                 z4=zic(i4)
                 if(l2.eq.1) then
                    x1=xa
                    y1=ya
                    z1=za
                 elseif(l2.eq.2) then
                    x2=xa
                    y2=ya
                    z2=za
                 elseif(l2.eq.3) then
                    x3=xa
                    y3=ya
                    z3=za
                 elseif(l2.eq.4) then
                    x4=xa
                    y4=ya
                    z4=za
                 endif
                 call volume_tet(x1,y1,z1,x2,y2,z2,
     *            x3,y3,z3,x4,y4,z4,voltet2)
                 if(voltet2.le.epslnv) then
                    itfound(indx)=0
                    go to 55
                 endif
                 voltetnew=voltetnew+voltet2
c
c  make sure volumes of new = volume of old
c
              enddo
              if(abs(voltetold-voltetnew).gt.epsln) then
c
c  if volumes do not match check depth of divot or bump
c  first find which face is boundary face and then which
c  neighbor element's face is a boundary face
c
                 ifac1=0
                 ifac2=0
                 call find_boundary_faces (j2,j3,nelts,elts
     *             ,mbndry,jtet,itet,itettyp,
     *             it1,it2,ifac1,ifac2)
                 node1=j2
                 node2=j3
c
c  only one tet containing this edge - so give up
c
                 if(ifac2.eq.0.or.ifac1.eq.0)   go to 54
c
c  find other nodes on boundary faces
c
                 do k=1,3
                 if(itet(ielmface1(k,ifac1,itettyp(it1)),it1).ne.j2.and.
     *              itet(ielmface1(k,ifac1,itettyp(it1)),it1).ne.j3)then
                   node3=itet(ielmface1(k,ifac1,itettyp(it1)),it1)
                 endif
                 if(itet(ielmface1(k,ifac2,itettyp(it2)),it2).ne.j2.and.
     *              itet(ielmface1(k,ifac2,itettyp(it2)),it2).ne.j3)then
                   node4=itet(ielmface1(k,ifac2,itettyp(it2)),it2)
                 endif
                 enddo
c
c  put nodes in correct order
c
                 if(itet(ielmface1(1,ifac1,itettyp(it1)),it1).eq.j2)
     *              then
                    n1=j2
                    if(itet(ielmface1(2,ifac1,itettyp(it1)),it1).eq.j3)
     *                 then
                       n3=j3
                       n2=node4
                       n4=node3
                    else
                       n2=node3
                       n3=j3
                       n4=node4
                    endif
                 elseif(itet(ielmface1(1,ifac1,itettyp(it1)),it1).eq.j3)
     *              then
                    n1=j3
                    if(itet(ielmface1(2,ifac1,itettyp(it1)),it1).eq.j2)
     *              then
                       n3=j2
                       n2=node4
                       n4=node3
                    else
                       n2=node3
                       n3=j2
                       n4=node4
                    endif
                 else
                    if(itet(ielmface1(2,ifac1,itettyp(it1)),it1).eq.j2)
     *               then
                       n1=j2
                       n2=node4
                       n3=j3
                       n4=node3
                    else
                       n1=j3
                       n2=node4
                       n3=j2
                       n4=node3
                    endif
                 endif
                 call testdamage_refine(n1,n2,n3,n4,xa,ya,za,
     *            damage,flag)
                 if(flag.ne.0) go to 56
  54             itfound(indx)=0
                 go to 55
               endif
            endif
         endif
c
c  make sure none of the neighbor elements have been
c  marked as already refined
c
  56     do ielt=1,nelts
           if(itflag(elts(ielt)).ne.0) then
              nedge_save=nedge_save+1
              iedge_tet(nedge_save)=it
              iedge_face(nedge_save)=i
              iedge_edge(nedge_save)=j
              iedge_p1(nedge_save)=iedge_p1(iedge)
              iedge_p2(nedge_save)=iedge_p2(iedge)
              itadd(nedge_save)=itadd(iedge)
              ieadd(nedge_save)=ieadd(iedge)
              iaddorder1(nedge_save)=iaddorder1(iedge)
              goto 130
            endif
         enddo
C
 
         ktlast=elts(1)
         kflast=faces(1)
         kelast=edges(1)
c
c  get correct child point for this element
c
         i2=itet(ielmedge1(1,kelast,itype),ktlast)
         i3=itet(ielmedge1(2,kelast,itype),ktlast)
         if(iadd(iaddorder1(iedge)).gt.0) then
            npointsnew1=iadd(iaddorder1(iedge))
            nadd1=nadd1+1
            int1add(nadd1)=0
            iaddorder2(nadd1)=iaddorder1(iedge)
            list_sink(nadd1)=npointsnew1
            list_source(1,nadd1)=npointsnew1
            list_source(2,nadd1)=npointsnew1
            xweight_source(1,nadd1)=1.0
            xweight_source(2,nadd1)=1.0
         elseif(iadd(iaddorder1(iedge)).lt.0) then
            npointsnew1=iabs(iadd(iaddorder1(iedge)))
            nadd1=nadd1+1
            int1add(nadd1)=0
            iaddorder2(nadd1)=iaddorder1(iedge)
            list_sink(nadd1)=npointsnew1
            list_source(1,nadd1)=i2
            list_source(2,nadd1)=i3
            xweight_source(1,nadd1)=1.0
            xweight_source(2,nadd1)=1.0
         else
            npointsnew=npointsnew+1
            call mmfindbk('xic',cmo,ipxic,length,icscode)
            if((npointsnew+1).gt.length) then
               npointsinc=npointsnew+1000
               call cmo_set_info('nnodes',cmo,npointsinc,1,1,ierror)
               call mmgetlen(ipitetclr,nelementsmm,icscode)
               call cmo_set_info('nelements',cmo,nelementsmm,1,1,ierror)
               call cmo_newlen(cmo,ierror)
               call cmo_get_info('xic',cmo,ipxic,ilen,icmotype,ier)
               call cmo_get_info('yic',cmo,ipyic,ilen,icmotype,ier)
               call cmo_get_info('zic',cmo,ipzic,ilen,icmotype,ier)
               call mmnewlen('iparent',isubname,ipiparent,npointsinc,
     *                       icscode)
               call mmnewlen('int1',isubname,ipint1,npointsinc,
     *                       icscode)
            endif
            npointsnew1=npointsnew
            iadd(iaddorder1(iedge))=npointsnew1
            if(int1(i2).eq.1.and.int1(i3).eq.1) then
               int1(npointsnew1)=1
            else
               int1(npointsnew1)=0
            endif
            nadd1=nadd1+1
            int1add(nadd1)=int1(npointsnew1)
            iaddorder2(nadd1)=iaddorder1(iedge)
            list_sink(nadd1)=npointsnew1
            list_source(1,nadd1)=i2
            list_source(2,nadd1)=i3
            xweight_source(1,nadd1)=1.0
            xweight_source(2,nadd1)=1.0
         endif
C
         iparent(npointsnew1)=npointsnew1
C
         i3=itet(ielmedge1(1,kelast,itype),ktlast)
         i4=itet(ielmedge1(2,kelast,itype),ktlast)
         xic(npointsnew1)=0.5*(xic(i3)+xic(i4))
         yic(npointsnew1)=0.5*(yic(i3)+yic(i4))
         zic(npointsnew1)=0.5*(zic(i3)+zic(i4))
         if(isboundary) then
            xic(npointsnew1)=xa
            yic(npointsnew1)=ya
            zic(npointsnew1)=za
            xadd(iedge)=xa
            yadd(iedge)=ya
            zadd(iedge)=za
         endif
C
C
C*****   write(logmess,'(a,8i6,3(1pe15.7),i6)')
C*****      '   ',iedge,itadd(iedge),ieadd(iedge),it,i,j,j2,j3,
C*****      xadd(iedge),yadd(iedge),zadd(iedge),npointsnew1
C*****   call writloga('default',0,logmess,0,ierrw)
 120     continue
         do ielt=1,nelts
         ktlast=elts(ielt)
         kflast=faces(ielt)
         kelast=edges(ielt)
         call mmgetlen(ipitetclr,length,icscode)
         if((ntetsnew+1).gt.length) then
            inc=1000
            ntetsinc=ntetsnew+inc
            call cmo_set_info('nelements',cmo,ntetsinc,1,1,ierror)
            call mmfindbk('xic',cmo,ipxic,nnodesmm,icscode)
            call cmo_set_info('nnodes',cmo,nnodesmm,1,1,ierror)
            call cmo_newlen(cmo,ierror)
            call cmo_get_info('itetclr',cmo,
     *                        ipitetclr,ilen,icmotype,ier)
            call cmo_get_info('itettyp',cmo,
     *                        ipitettyp,ilen,icmotype,ier)
            call cmo_get_info('itetoff',cmo,
     *                        ipitetoff,ilen,icmotype,ier)
            call cmo_get_info('jtetoff',cmo,
     *                        ipjtetoff,ilen,icmotype,ier)
            call cmo_get_info('itet',cmo,
     *                        ipitet,ilen,icmotype,ierror)
            call cmo_get_info('jtet',cmo,
     *                        ipjtet,ilen,icmotype,ierror)
         endif
         call mmgetlen(ipitflag,length,icscode)
         if((ntetsnew+1).gt.length) then
            inc=1000
            call mmgetnam(ipitflag,iblknam,iprtnam,ics)
            call mmincblk(iblknam,iprtnam,ipitflag,inc,
     *                    ics)
            do idum=ntetsnew+1,ntetsnew+inc
               itflag(idum)=0
            enddo
            inc1=nen*inc
            call mmgetnam(ipitetnn,iblknam,iprtnam,ics)
            call mmincblk(iblknam,iprtnam,ipitetnn,inc1,
     *                    ics)
            inc2=nef*inc
            call mmgetnam(ipitetnn1,iblknam,iprtnam,ics)
            call mmincblk(iblknam,iprtnam,ipitetnn1,inc2,
     *                    ics)
            call mmgetnam(ipitetnn2,iblknam,iprtnam,ics)
            call mmincblk(iblknam,iprtnam,ipitetnn2,inc2,
     *                    ics)
         endif
         i1=itet(1,ktlast)
         i2=itet(2,ktlast)
         i3=itet(3,ktlast)
         i4=itet(4,ktlast)
         l1=ielmedge1(1,kelast,itype)
         l2=ielmedge1(2,kelast,itype)
         ntetsnew=ntetsnew+1
         itflag(ntetsnew)=1
         itetclr(ntetsnew)=itetclr(ktlast)
         itettyp(ntetsnew)=itettyp(ktlast)
         itetoff(ntetsnew)=nen*(ntetsnew-1)
         jtetoff(ntetsnew)=nef*(ntetsnew-1)
         itetnn(1,ntetsnew)=i1
         itetnn(2,ntetsnew)=i2
         itetnn(3,ntetsnew)=i3
         itetnn(4,ntetsnew)=i4
         if(l1.eq.1) then
            itetnn(1,ntetsnew)=npointsnew1
         elseif(l1.eq.2) then
            itetnn(2,ntetsnew)=npointsnew1
         elseif(l1.eq.3) then
            itetnn(3,ntetsnew)=npointsnew1
         elseif(l1.eq.4) then
            itetnn(4,ntetsnew)=npointsnew1
         endif
         itetnn1(1,ntetsnew)=-1
         itetnn1(2,ntetsnew)=-1
         itetnn1(3,ntetsnew)=-1
         itetnn1(4,ntetsnew)=-1
         itetnn2(1,ntetsnew)=-1
         itetnn2(2,ntetsnew)=-1
         itetnn2(3,ntetsnew)=-1
         itetnn2(4,ntetsnew)=-1
         itflag(ktlast)=1
         itetnn(1,ktlast)=i1
         itetnn(2,ktlast)=i2
         itetnn(3,ktlast)=i3
         itetnn(4,ktlast)=i4
         if(l2.eq.1) then
            itetnn(1,ktlast)=npointsnew1
         elseif(l2.eq.2) then
            itetnn(2,ktlast)=npointsnew1
         elseif(l2.eq.3) then
            itetnn(3,ktlast)=npointsnew1
         elseif(l2.eq.4) then
            itetnn(4,ktlast)=npointsnew1
         endif
         itetnn1(1,ktlast)=-1
         itetnn1(2,ktlast)=-1
         itetnn1(3,ktlast)=-1
         itetnn1(4,ktlast)=-1
         itetnn2(1,ktlast)=-1
         itetnn2(2,ktlast)=-1
         itetnn2(3,ktlast)=-1
         itetnn2(4,ktlast)=-1
         enddo
 130     continue
      enddo
      write(logmess,'(a,i10,a,i10)')
     *   'Edge-refined tets: old=',ntets,' new=',ntetsnew
      call writloga('default',0,logmess,0,ierrw)
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
      elseif(ntetsnew.eq.ntets) then
         write(logmess,'(a)') 'no elements refined so we will quit'
         call writloga('default',0,logmess,0,ierror)
         oflag=1
         go to 9999
      endif
      if(nedge_save.gt.0) then
C
C refresh tet and edge lists that have
C been corrupted by previous iterations
C desired edge should be on first neighbor
C of listed tet
C
       do iedge=1,nedge_save
         it=iedge_tet(iedge)
         i=iedge_face(iedge)
         j=iedge_edge(iedge)
         j2=iparent(itet(ielmedge1(1,j,itype),it))
         j3=iparent(itet(ielmedge1(2,j,itype),it))
         ip1=iparent(iedge_p1(iedge))
         ip2=iparent(iedge_p2(iedge))
         if((ip1.eq.j2.and.ip2.eq.j3) .or.
     *      (ip1.eq.j3.and.ip2.eq.j2)) then
         else
            do inew=1,4
               do jnew=1,3
                  l1=ielmface2(jnew,inew,itype)
                  j2=iparent(itet(ielmedge1(1,l1,itype),it))
                  j3=iparent(itet(ielmedge1(2,l1,itype),it))
                  if((ip1.eq.j2.and.ip2.eq.j3).or.
     *               (ip1.eq.j3.and.ip2.eq.j2)) then
                     i=inew
                     j=l1
                     goto 57
                  endif
               enddo
            enddo
            do jj = 1,4
              jtettmp=jtet(jj,it)
              if (jtettmp.ge.mbndry) jtettmp=jtettmp-mbndry
              if (jtettmp.gt.0) then
                itnew=1+(jtettmp-1)/nef
                do inew=1,4
                  do jnew=1,3
                     l1=ielmface2(jnew,inew,itype)
                     j2=iparent(itet(ielmedge1(1,l1,itype),itnew))
                     j3=iparent(itet(ielmedge1(2,l1,itype),itnew))
                     if((ip1.eq.j2.and.ip2.eq.j3).or.
     *                  (ip1.eq.j3.and.ip2.eq.j2)) then
                        it=itnew
                        i=inew
                        j=jnew
                        goto 57
                     endif
                  enddo
                enddo
              endif
            enddo
            write(logmess,52) ip1,ip2
            call writloga('default',0,logmess,0,icscode)
            goto 58
   57       continue
            iedge_tet(iedge)=kk
            iedge_face(iedge)=i
            iedge_edge(iedge)=j
         endif
   58  enddo
       nedge=nedge_save
       goto 10
      endif
C
C
      cmolength='nnodes'
      call cmo_interpolate(cmo_name,cmo_name,
     *                     cmolength,
     *                     nadd1,nvalues,
     *                     list_sink,list_source,xweight_source,
     *                     ierror)
      if(ierror.ne.0) call x3d_error(isubname,'cmo_interpolate')
C
      call cmo_get_info('imt1',cmo,ipimt1,ilen,icmotype,ier)
      call cmo_get_info('itp1',cmo,ipitp1,ilen,icmotype,ier)
      call cmo_get_info('isn1',cmo,ipisn1,ilen,icmotype,ier)
      call cmo_get_info('xic',cmo,ipxic,ilen,icmotype,ier)
      call cmo_get_info('yic',cmo,ipyic,ilen,icmotype,ier)
      call cmo_get_info('zic',cmo,ipzic,ilen,icmotype,ier)
C
      call cmo_get_info('itp1',cmo,ipitp1,length,icmotype,ierror)
      length=npoints
      call mmnewlen("int1",isubname,ipint1,length,icscode)
      call unpacktp("intrface","set",npoints,ipitp1,ipint1,ierror)
      if(ierror.ne.0) call x3d_error('refine_edge_add', 'unpacktp')
C
      jcount=0
      do i=1,nadd1
         i1=list_sink(i)
         i2=iaddorder2(i)
         if(int1(i1).eq.1) then
            jcount=jcount+1
         endif
         xic(i1)=xadd(i2)
         yic(i1)=yadd(i2)
         zic(i1)=zadd(i2)
         isn1(i1)=0
      enddo
c
      call cmo_get_name(cmo,ierror)
      call cmo_set_info('nnodes',cmo,npoints,1,1,ierror)
      call cmo_set_info('nelements',cmo,ntets,1,1,ierror)
C
      if(jcount.gt.0) then
         call dotaskx3d('settets/newtets ; finish',ierror)
         call cmo_get_intinfo('nnodes',cmo,npoints,length,icmotype,
     *      ierror)
         call cmo_get_intinfo('nelements',cmo,ntets,length,icmotype,
     *      ierror)
         call cmo_get_info('itp1',cmo,ipitp1,ilen,icmotype,ier)
         call cmo_get_info('imt1',cmo,ipimt1,ilen,icmotype,ier)
         call cmo_get_info('isetwd',cmo,ipisetwd,ilen,icmotype,ier)
         call cmo_get_info('isn1',cmo,ipisn1,ilen,icmotype,ier)
 
c
c  find common material pairs and set pset membership accordingly
c
        do i=1,nadd1
          i1=list_source(1,i)
          i2=list_source(2,i)
          if(isn1(i1).ne.0 .and. isn1(i2).ne.0.and.
     *      isn1(list_sink(i)).ne.0) then
            istrta=i1
            if(itp1(i1).eq.ifitpcup) istrta=isn1(i1)
            istrtb=i2
            if(itp1(i2).eq.ifitpcup) istrtb=isn1(i2)
            i3=istrta
            imtmatch=imt1(i3)
            i4=istrtb
 90         if(imtmatch.eq.imt1(i4)) then
c
c  found match so set pset membership (isetwd)
c
               i1=list_sink(i)
               istrtc=i1
               if(itp1(i1).eq.ifitpcup) istrtc=isn1(i1)
               i1=istrtc
               ict=0
  92           if(imt1(i1).eq.imtmatch) then
                 isetwd(i1)=iand(isetwd(i3),isetwd(i4))
                 i4=istrtb
                 i1=istrtc
                 go to 97
               else
                 i1=isn1(i1)
                 if(itp1(i1).eq.ifitpcup) i1=isn1(i1)
                 ict=ict+1
                 if(ict.gt.10000) then
                    write(logmess,'("Bad parent/child chain ",
     *               " in refine_edge_add_tet")')
                    call writloga('default',0,logmess,0,icscode)
                    isn1(istrtc)=0
                 endif
                 if(i1.ne.istrtc) go to 92
               endif
            endif
c
c  never found matching pair so go to next
c
            if(isn1(i4).eq.istrtb) go to 97
c
c  get next in 2nd chain
c
            i4=isn1(i4)
            if(itp1(i4).eq.ifitpcup) i4=isn1(i4)
            if(i4.ne.istrtb) go to 90
c
c  get next in 1st chain
c
  97        i3=isn1(i3)
            if(itp1(i3).eq.ifitpcup) i3=isn1(i3)
            if(i3.ne.istrta) then
               imtmatch=imt1(i3)
               go to 90
            endif
          endif
        enddo
      endif
C
 9999 continue
      call mmrelprt(isubname,icscode)
C
      return
      end
c
c
      subroutine find_boundary_faces (i1,i2,nelts,elts,mbndry,jtet,
     *   itet,itettyp,
     *   it1,it2,iface1,iface2)
c
c  starting at element it find boundary faces containing edge
c  with endpoints i1,i2.
c
      implicit none
      include 'local_element.h'
      integer i1,i2,it1,it2,iface1,iface2,nbnds,nelts,elts(*),k,i,it,
     *  jtet(4,*),itettyp(*),mbndry,ierror,ityp,nptfnd,kk,itet(4,*)
      character*132 logmess
 
      nbnds=0
c
c  loop through elements
c  must find exactly two boundary faces
c  count boundary faces found in nbnds
c
      do k=1,nelts
         it=elts(k)
         ityp=itettyp(it)
         do i=1,nelmnef(ityp)
            nptfnd=0
            if(jtet(i,it).eq.mbndry) then
c
c  loop through faces - face must contain both nodes count
c  nodes found in nptfnd
c
                do kk=1,ielmface0(i,ityp)
                   if(i1.eq.itet(ielmface1(kk,i,ityp),it)) then
                      nptfnd=nptfnd+1
                      if(nptfnd.eq.2) go to 50
                   endif
                   if(i2.eq.itet(ielmface1(kk,i,ityp),it)) then
                      nptfnd=nptfnd+1
                      if(nptfnd.eq.2) go to 50
                   endif
                enddo
                go to 90
 50             nbnds=nbnds+1
                if(nbnds.eq.1) then
                   it1=it
                   iface1=i
                else
                   it2=it
                   iface2=i
                endif
                if(nbnds.eq.2) go to 9999
c
c  if only one element both faces must be on the
c  same element
c
                if(nelts.eq.1) go to 90
                go to 100
             endif
 90      continue
         enddo
c
 
 100     continue
      enddo
      write(logmess,'(a,1x,2i10)')
     *      'cant find boundary faces for edge',i1,i2
      call writloga('default',0,logmess,0,ierror)
 9999 return
      end
 
