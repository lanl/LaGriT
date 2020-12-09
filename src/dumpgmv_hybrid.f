      subroutine dumpgmv_hybrid(ifile,cmo,iotype)
 
c #####################################################################
c
c     purpose -
c
c        create a gmv formatted file from the current mesh object
c
c     input arguments -
c
c        none
c
c     output arguments -
c
c        none
c
c     change history -
c
c        $Log: dumpgmv_hybrid.f,v $
c        Revision 2.00  2007/11/05 19:45:53  spchu
c        Import to CVS
c
CPVCS    
CPVCS       Rev 1.20   03 Oct 2005 08:39:16   gable
CPVCS    Fixed case where nnodes=nelements which caused incorrect output.
CPVCS    
CPVCS       Rev 1.19   06 Apr 2005 14:23:18   gable
CPVCS    Fix bug introduced by inversion of connectivity when output of
CPVCS    tet or hex elements. Ascii was working OK, but if binary output
CPVCS    was written, then when the GMV file was read, tets and hexs were
CPVCS    negative volume.
CPVCS    
CPVCS       Rev 1.18   22 Jul 2004 14:21:58   gable
CPVCS    Found error in tet and hex connectivity. Insert code to invert
CPVCS    connectivity so that tet and hex are not inside out. Also added
CPVCS    simdate and codename keyword to header of ascii GMV files. These
CPVCS    keywords are used in the read/gmv code now to detect the new
CPVCS    corrected connectivity.
CPVCS    
CPVCS       Rev 1.17   24 Mar 2003 15:17:22   gable
CPVCS    Fix syntax error.
CPVCS    
CPVCS       Rev 1.16   24 Mar 2003 14:34:36   gable
CPVCS    Fixed format so that ascii output can exceed 1 million nodes.
CPVCS    
CPVCS       Rev 1.15   16 Jul 2001 14:40:52   tam
CPVCS    added error checking to get_info commands
CPVCS    don't try to access imt1 or itp1 if they do not exist
CPVCS    
CPVCS       Rev 1.14   15 May 2001 13:00:36   jan
CPVCS    took out one embedded underscore
CPVCS    
CPVCS       Rev 1.12   15 May 2001 09:15:12   jan
CPVCS    deleted embedded underscores
CPVCS    
CPVCS       Rev 1.11   14 Mar 2001 13:55:06   dcg
CPVCS    move data statements to end 
CPVCS    get rid of upper case
CPVCS    
CPVCS       Rev 1.10   29 Nov 2000 10:32:10   dcg
CPVCS    check if geometry exists - if not set nmregs to zero
CPVCS    
CPVCS       Rev 1.9   29 Sep 2000 11:59:54   dcg
CPVCS    change i3.3 to i5.5 to allow up to 99999 different materials
CPVCS    
CPVCS       Rev 1.8   12 Jun 2000 11:35:28   jan
CPVCS    changed line 317 if(ipolydata.eq.'no') ipolydata=0 to if(cvor.eq.'no') ipolydata=0
CPVCS    
CPVCS       Rev 1.7   05 Jun 2000 13:48:20   dcg
CPVCS    change i3.0 formats to i5.5
CPVCS    
CPVCS       Rev 1.6   Thu Feb 03 08:29:52 2000   dcg
CPVCS    
CPVCS       Rev 1.5   Mon Jan 31 15:24:46 2000   dcg
CPVCS    
CPVCS       Rev 1.1   13 Jan 2000 14:47:48   dcg
CPVCS    
CPVCS       Rev 1.0   05 Jan 2000 17:32:26   dcg
CPVCS     
CPVCS
CPVCS       Rev 1.31   Fri Nov 05 17:06:26 1999   dcg
CPVCS    access cmo parameters for ipolydata and voronoi2d and 3d
CPVCS
CPVCS       Rev 1.30   Thu Jul 01 02:25:10 1999   jtg
CPVCS    changes made so nonexistant cmo, illegal element types don't
CPVCS    crash lagrit or gmv, at least up to the testing done.
CPVCS    illegal elements are dumped in the "polygon" field but
CPVCS    not the element field (unless there are no legal elements,
CPVCS    in which case neither is dumped)
CPVCS
CPVCS       Rev 1.29   Fri Apr 02 09:47:38 1999   nnc
CPVCS    Dump velocity data only if "g" is present in iomode.
CPVCS
CPVCS       Rev 1.28   Tue Mar 23 14:38:34 1999   dcg
CPVCS    fix warning message for attributes that gmv doesn't
CPVCS    know how to handle
CPVCS
CPVCS       Rev 1.27   Fri Mar 05 11:27:18 1999   dcg
CPVCS    modify -def- attribute so that it is not dumped out
CPVCS
CPVCS       Rev 1.26   Thu Feb 04 16:55:38 1999   dcg
CPVCS    write line cell type
CPVCS
CPVCS       Rev 1.25   Wed Feb 03 14:36:38 1999   nnc
CPVCS    Fixed problem with floating point overflows caused by numerous
CPVCS    real*8 to real*4 assignments; the new statement function safe8to4
CPVCS    clips a value to a range representable by a real*4 variable.
CPVCS
CPVCS       Rev 1.24   Fri Jan 29 14:30:16 1999   dcg
CPVCS    read binary/ascii from input line
CPVCS
CPVCS       Rev 1.23   Fri Aug 28 14:24:38 1998   dcg
CPVCS    remove single precision constants
CPVCS
CPVCS       Rev 1.22   Thu Jan 29 15:03:36 1998   kuprat
CPVCS    Changed PROBTIME to REAL*4.
CPVCS
CPVCS       Rev 1.21   Tue Nov 04 13:07:18 1997   dcg
CPVCS    add comdict.h
CPVCS
CPVCS       Rev 1.20   Mon Apr 14 16:44:20 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.19   Thu Mar 06 21:54:22 1997   het
CPVCS    Fix an error with prisms.
CPVCS
CPVCS       Rev 1.18   Mon Feb 24 08:01:16 1997   het
CPVCS    Get dictionary control variables.
CPVCS
CPVCS       Rev 1.17   Sun Feb 23 10:45:06 1997   het
CPVCS    Write triangles and quads as polygons.
CPVCS
CPVCS       Rev 1.16   Fri Jan 24 13:35:22 1997   het
CPVCS    Put in calles to the GMV binary library for doing binary dumps.
cpvcs
cpvcs       rev 1.9   05/01/95 08:51:48   het
cpvcs    modifiy all the storage block calles for long names
cpvcs
cpvcs       rev 1.8   04/14/95 09:00:54   het
cpvcs    add the 3d surface polygon option
cpvcs
cpvcs       rev 1.7   03/31/95 15:50:46   het
cpvcs    add the surface voronoi and median meshes
cpvcs
cpvcs       rev 1.6   03/31/95 09:03:26   het
cpvcs    add the buildid calles before all storage block calls
cpvcs
cpvcs       rev 1.5   03/28/95 14:16:48   het
cpvcs    correct an error in calculating mbndry
cpvcs
cpvcs       rev 1.4   03/03/95 11:07:18   dcg
cpvcs     remove hardwired 2dmesh object
cpvcs
cpvcs       rev 1.3   02/17/95 19:19:48   het
cpvcs    correct an error an line longer that 72 characters
cpvcs
cpvcs       rev 1.2   02/16/95 07:35:30   het
cpvcs    correct format errors in dump and read commands
cpvcs
cpvcs       rev 1.1   01/04/95 22:02:10   llt
cpvcs    unicos changes (made by het)
cpvcs
cpvcs       rev 1.0   11/13/94 11:44:20   pvcs
cpvcs    orginal version
c
c ######################################################################
c
      implicit none
c
      real*8 alargenumber
      parameter (alargenumber=1.d+20)
      include "local_element.h"
      include 'geom_lg.h'
c
      character ifile*(*)
      character cmo*(*),iotype*(*)
C
      integer nsd, nen, nef
c
      integer nenlin,nfacelin,nentri,nfacetri,nentet,nfacetet,nenprism,
     *  nfaceprism,nenhex,nfacehex
      parameter (nenlin=2, nfacelin=2)
      parameter (nentri=3, nfacetri=3)
      parameter (nentet=4, nfacetet=4)
      parameter (nenprism=6, nfaceprism=5)
      parameter (nenhex=8, nfacehex=6)
c
      integer ihexface0(nfacehex), ihexface1(4,nfacehex)
c     top,bottom,front,right,back,left
      integer iprismface0(nfaceprism), iprismface1(4,nfaceprism)
c     top,bottom,right,back,left
c
      integer intpairhex(2,12),jpt(10)
 
C     top,back,left,right
      integer itetface0(nfacetet), itetface1(4,nfacetet)
      integer itriface0(nfacetri), itriface1(3,nfacetri)
      integer ilinface0(nfacelin), ilinface1(4,nfacelin)
      integer intpairtet(2,6)
c
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      real*8 xic(*), yic(*), zic(*)
      pointer (ipvels,vels)
      real*8 vels(3,*)

      pointer (ipimt1, imt1)
      pointer (ipitp1, itp1)
      pointer (ipicr1, icr1)
      pointer (ipisn1, isn1)
      pointer (ipisetwd, isetwd)
      integer imt1(*), itp1(*),isetwd(*),
     *        icr1(*), isn1(*)

      pointer (ipitetclr, itetclr)
      pointer (ipitettyp, itettyp)
      pointer (ipitetoff, itetoff)
      pointer (ipjtetoff, jtetoff)
      pointer (ipitet, itet1)
      pointer (ipjtet, jtet1)
      integer itetclr(*), itettyp(*),
     *        itetoff(*), jtetoff(*), itet1(*), jtet1(*)

      integer*4 imatcolor,nmats,iclr,iclrv,iclrm,ihcycle
      integer*4 igmvtype,jgmvtype,nverts4,legalnelm4

      integer jt,jf,j1,j2,j3,i2,jtoff,ie
     *  lenout,maxclrelement,maxicr1,maxitp1,maxclrpoint,
     *  ics,mmlength,ierror_return,irank,lent,j,iflag,len1,itoff,
     *  i1,it,i,ierr
      integer imt_exist, itp_exist, icr_exist, isn_exist
      integer icharlnf
      integer j4,ity,n,k,iflagkid,i4,ivoronoi2d,
     *  index,icmotype,itype,lout,ftype

      real*8 xa,ya,za,xb,yb,zb,xd,yd,zd,xn1,yn1,zn1,
     *  xn,yn,zn,rn,dotb3,dot3,rb3,ql,xl,yl,zl,ds1,ds2,ds3,x12,y12,z12,
     *  x13,y13,z13,x23,y23,z23,x1,y1,z1,x2,y2,z2,x3,y3,z3,
     *  xm,ym,zm,xv,yv,zv,xfac,a,b,c,d,e,f,epsilonl,
     * distmax,dist,disttest

      integer i3,ie,ier,nverts,iunit,ityp,ilen,nef_cmo,nsdgeom,
     *  mbndry,nelements,ierror,length,nnodes,icscode,
     *  icskid,idumptype,iflag_all,ipolydata,ivoronoi3d,legalnelm,
     *  iout,nmcmoatt,lenout,nmatcolor

      real*8 time,crosx,crosz,crosy,distsqd,distsqc,distsqb,distsqa,
     *  xcen,ycen,zcen,x4,y4,z4,x34,y34,z34,x41,y41,z41,x14,y14,z14,
     *  x24,y24,z24,ds11,ds21,ds31,xv1,yv1,zv1,ds12,ds22,ds32,xv2,
     *  yv2,zv2,ds13,ds23,ds33,xv3,yv3,zv3,x124,y124,z124,x132,y132,
     *  z132,rout,
     *  xvor,yvor,zvor,xc,yc,zc,q,dvor,qvor2,xtestmax,xtest,
     *  x234,y234,z234,x143,y143,z143,voltot,voltet,
     *  ds14,ds24,ds34,xv4,yv4,zv4,xl1,yl1,zl1,xl2,yl2,zl2,xl3,yl3,zl3,
     *  xl4,yl4,zl4,ax1,ay1,az1,ax2,ay2,az2,ax3,ay3,az3,ax4,ay4,az4
C
      pointer (ipitetkid, itetkid)
      integer itetkid(*)
C
      pointer (ipxptemp, xptemp(*))
      real*8 xptemp
      pointer (ipxtemp, xtemp(*))
      real*8 xtemp

      pointer (ipitemp, itemp(*))
      integer*4 itemp
      pointer (ipireal1, ireal1(*))
      integer ireal1
C
C
      pointer (ipxvar, xvar)
      REAL*4 xvar(*)
C
      pointer (ipcmo_pointer, icmo_pointer)
      pointer (ipcmo_pointer, xcmo_pointer)
      integer icmo_pointer(*)
      REAL*8 xcmo_pointer(*)
c
      character*32 isubname,iprtname,cvor,geom_name
      pointer(ipout,out)
      real*8 out(*)
C
      character*32 cname, ctype, cioflag, ctabinterp, cvelnm,cout,
     *  clength,cpers,crank
      real*8 cinterpolate
C
      character*80 cgmvfile
      character*9 cgmvtype
      character*132 logmess
      integer nodeids(100)
      real*4 xicpoly(100), yicpoly(100), zicpoly(100)
C
      pointer (ipxgmv, xgmv)
      pointer (ipygmv, ygmv)
      pointer (ipzgmv, zgmv)
      real*4 xgmv(*), ygmv(*), zgmv(*)
      pointer (ipu, u)
      pointer (ipv, v)
      pointer (ipw, w)
      real*4 u(*), v(*), w(*)
      real*4 probtime
C
      integer i_invert1, i_invert2, i_invert3, i_invert4,
     1        if_invert_ele,if_lagrit

      character*8 string8
      character*24 string, fdate
      character*32 string32
C 
      real*8 huge4, value, safe8to4
      integer ihuge4, ivalue, isafe4
      parameter(huge4=1.d20)
      parameter(ihuge4=2000000000)
 
      safe8to4(value) = sign(min(huge4,abs(value)),value)
      isafe4(ivalue) = sign(min(ihuge4,abs(ivalue)),ivalue)
C
      data iprismface0 / 3, 3, 4, 4, 4 /
      data iprismface1 / 1, 2, 3, 0,
     *                   4, 6, 5, 0,
     *                   1, 4, 5, 2,
     *                   2, 5, 6, 3,
     *                   1, 3, 6, 4 /
      data intpairhex / 1,2, 2,3, 3,4, 4,1, 5,6, 6,7, 7,8,
     *                  8,5, 1,5, 2,6, 3,7, 4,8 /
      data itriface0 / 2, 2, 2 /
      data itriface1 / 2, 3, 1,
     *                 3, 1, 2,
     *                 1, 2, 3 /
      data itetface0 / 3, 3, 3, 3 /
      data itetface1 / 2, 3, 4, 0,
     *                 1, 4, 3, 0,
     *                 1, 2, 4, 0,
     *                 1, 3, 2, 0 /
      data ilinface0 / 1, 1 /
      data ilinface1 / 2, 1, 0, 0,
     *                 1, 2, 0, 0 /
      data intpairtet / 1,2, 1,3, 1,4, 2,3, 2,4, 3,4 /
      data ihexface0 / 4, 4, 4, 4, 4, 4 /
      data ihexface1 / 1, 2, 3, 4,
     *                 5, 8, 7, 6,
     *                 1, 5, 6, 2,
     *                 2, 6, 7, 3,
     *                 3, 7, 8, 4,
     *                 1, 4, 8, 5 /
      data ivoronoi2d / 1 /
      data ivoronoi3d / 0 /
      data ipolydata / 1 /
      data iflag_all / 0 /
      data imt_exist / 1 /
      data itp_exist / 1 /
      data icr_exist / 1 /
      data isn_exist / 1 /
c
      crosx(a,b,c,d,e,f)=b*f-c*e
      crosy(a,b,c,d,e,f)=c*d-a*f
      crosz(a,b,c,d,e,f)=a*e-b*d
C     ******************************************************************
C begin
c
      isubname="dump_gmv"
      ierror = 0
      icskid=1
C

C     exit if mesh object is empty
C     use 9995 to exit without closing un-opened file
      call cmo_get_info('nnodes',cmo,
     *                  nnodes,length,ityp,ierror)
      if (ierror.ne.0.or.nnodes.le.0) then
         write(logmess,'(a,a)') 
     *   'DUMP/GMV early exit: no nodes in cmo: ',
     *   cmo(1:icharlnf(cmo))
         call writloga('default',0,logmess,0,ics)
         ierror = -1
         goto 9995
      endif
 
C     ******************************************************************
C
      call cmo_get_attinfo('geom_name',cmo,iout,rout,geom_name,
     *                        ipout,lout,itype,ierror)
      call mmfindbk('cmregs',geom_name,ipcmregs,length,ierror)
      if(ierror.ne.0) nmregs=0
      idumptype=1
      if(iotype(1:5).eq.'ascii') idumptype=0
      call cmo_get_attinfo('vor2d',cmo,iout,rout,cvor,ipout,lout,
     *     itype,icscode)
      ivoronoi2d=1
      if(cvor.eq.'no') ivoronoi2d=0
      call cmo_get_attinfo('vor3d',cmo,iout,rout,cvor,ipout,lout,
     *     itype,icscode)
      if(cvor.eq.'no') ivoronoi3d=0
      call cmo_get_attinfo('ipolydat',cmo,iout,rout,cvor,ipout,lout,
     *     itype,icscode)
 
      ipolydata=1
      if(cvor.eq.'no') ipolydata=0
C
C     ******************************************************************
C
c  get information from  mesh object
 
C     Mesh Object attributes
      call cmo_get_info('nelements',cmo,
     *                  nelements,length,ityp,ierror)
      if(ierror.ne.0) call x3d_error(isubname,'get_info nelements')
      call cmo_get_info('mbndry',cmo,
     *                   mbndry,length,ityp,ierror)
      if(ierror.ne.0) call x3d_error(isubname,'get_info mbndry')
      call cmo_get_info('ndimensions_topo',cmo,
     *                  nsd,length,ityp,ierror)
      if(ierror.ne.0) 
     *   call x3d_error(isubname,'get_info ndimensions_topo')
      call cmo_get_info('ndimensions_geom',cmo,
     *                  nsdgeom,length,ityp,ierror)
      if(ierror.ne.0) 
     *   call x3d_error(isubname,'get_info ndimensions_geom')
      call cmo_get_info('nodes_per_element',cmo,
     *                  nen,length,ityp,ierror)
      if(ierror.ne.0) 
     *   call x3d_error(isubname,'get_info nodes_per_element')
      call cmo_get_info('faces_per_element',cmo,
     *                  nef_cmo,length,ityp,ierror)
      if(ierror.ne.0) 
     *   call x3d_error(isubname,'get_info faces_per_element')
      call cmo_get_info('isetwd',cmo,
     *                        ipisetwd,ilen,ityp,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'get_info isetwd')

C     Mesh Object coordinates
      call cmo_get_info('xic',cmo,ipxic,ilen,ityp,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'get_info xic')
      call cmo_get_info('yic',cmo,ipyic,ilen,ityp,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'get_info yic')
      call cmo_get_info('zic',cmo,ipzic,ilen,ityp,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'get_info zic')

C     Mesh Object element information
      call cmo_get_info('itetclr',cmo,
     *                        ipitetclr,ilen,ityp,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'get_info itetclr')
      call cmo_get_info('itettyp',cmo,
     *                        ipitettyp,ilen,ityp,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'get_info itettyp')
      call cmo_get_info('itetoff',cmo,
     *                        ipitetoff,ilen,ityp,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'get_info itetoff')
      call cmo_get_info('jtetoff',cmo,
     *                        ipjtetoff,ilen,ityp,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'get_info jtetoff')
      call cmo_get_info('itet',cmo,ipitet,ilen,ityp,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'get_info itet')
      call cmo_get_info('jtet',cmo,ipjtet,ilen,ityp,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'get_info jtet')

C     Node Attributes
      call cmo_get_info('imt1',cmo,ipimt1,ilen,ityp,ierr)
      if (ilen.le.0) imt_exist = 0
      call cmo_get_info('itp1',cmo,ipitp1,ilen,ityp,ierr)
      if (ilen.le.0) itp_exist = 0
      call cmo_get_info('icr1',cmo,ipicr1,ilen,ityp,ierr)
      if (ilen.le.0) icr_exist = 0
      call cmo_get_info('isn1',cmo,ipisn1,ilen,ityp,ierr)
      if (ilen.le.0) isn_exist = 0


c     Hardwire turning off output of -def- field.
      call dotask
     * ('cmo/modatt/-def-/-def-/ioflag/x;finish',icscode)

      length=nnodes
      call mmgetblk("xptemp",isubname,ipxptemp,length,2,icscode)
      length=max(nnodes,nelements)
      call mmgetblk("xtemp",isubname,ipxtemp,length,2,icscode)
      length=max(nnodes,nelements)
      call mmgetblk("itemp",isubname,ipitemp,length,2,icscode)
      length=nnodes
      call mmgetblk("ireal1",isubname,ipireal1,length,2,icscode)
      if (icscode .ne. 0) then
        call x3d_error(isubname,'can not mmgetblk temp arrays')
        goto 9995
      endif

C
c     check that itp1 exists, else may get segmentation fault 
      if (itp_exist.gt.0) then 
        call unpacktp("allreal","set",nnodes,ipitp1,ipireal1,ierror)
        if(ierror.ne.0) call x3d_error('unpacktp', isubname)
      endif
      if(idumptype.eq.0) then
         iunit=-1
         call hassign(iunit,ifile,ierror)
         if (iunit.lt.0 .or. ierror.lt.0) then
           call x3d_error(isubname,'hassign bad file unit')
           goto 9995
         endif
         write(iunit,"('gmvinput ascii')")
         write(iunit,"('codename LaGriT')")
C
C     Get a time stamp for the file header.
C
         string = fdate()
         call fdate_2_mmddyy(string8,string)
         write(iunit,250)'simdate ',string8
  250    format(a8,a8)
      else
        cgmvfile=ifile
     	call fgmvwriteopenfile(cgmvfile)
      endif

C     file has been successfully opened
C     use 9999 for exit instead of 9995 now that file is open

      if(idumptype.eq.0) then
         write(iunit,"('nodes   ',i10)") nnodes
         write(iunit,"(10(1pe14.5e3))") (xic(i),i=1,nnodes)
         write(iunit,"(10(1pe14.5e3))") (yic(i),i=1,nnodes)
         write(iunit,"(10(1pe14.5e3))") (zic(i),i=1,nnodes)
      else
         length=nnodes
         call mmgetblk('xgmv',isubname,ipxgmv,length,1,icscode)
         call mmgetblk('ygmv',isubname,ipygmv,length,1,icscode)
         call mmgetblk('zgmv',isubname,ipzgmv,length,1,icscode)
         if (icscode .ne. 0) then
           call x3d_error(isubname,'can not mmgetblk xyz arrays')
           goto 9999
         endif

         do i1=1,nnodes
            xgmv(i1)=safe8to4(xic(i1))
            ygmv(i1)=safe8to4(yic(i1))
            zgmv(i1)=safe8to4(zic(i1))
         enddo
         nverts4=isafe4(nnodes)
         call fgmvwritenodedata(nverts4, xgmv, ygmv, zgmv)
         if (nverts4 .eq. 0) then
           call x3d_error(isubname,'can not write node data')
         endif
         call mmrelblk('xgmv',isubname,ipxgmv,icscode)
         call mmrelblk('ygmv',isubname,ipygmv,icscode)
         call mmrelblk('zgmv',isubname,ipzgmv,icscode)
      endif
C
      legalnelm=nelements
      do it=1,nelements
          i=itettyp(it)
          if (    i.ne.ifelmlin .and. i.ne.ifelmtri
     *       .and.i.ne.ifelmqud .and. i.ne.ifelmtet
     *       .and.i.ne.ifelmpyr .and. i.ne.ifelmpri
     *       .and.i.ne.ifelmhex )   legalnelm=legalnelm-1
      enddo
      if(idumptype.eq.0) then
         write(iunit,"('cells   ',i10)") legalnelm
      else
        legalnelm4=isafe4(legalnelm)
	call fgmvwritecellheader(legalnelm4)
      endif
      if(legalnelm.gt.0) then
         do it=1,nelements
            itoff=itetoff(it)
            if(itettyp(it).eq.ifelmlin) then
               cgmvtype='line'
               nverts=0
               do i=1,nelmnen(itettyp(it))
                  nverts=nverts+1
                  nodeids(nverts)=itet1(itoff+i)
               enddo
            elseif(itettyp(it).eq.ifelmtri) then
               cgmvtype='tri'
               nverts=0
               do i=1,nelmnen(itettyp(it))
                  nverts=nverts+1
                  nodeids(nverts)=itet1(itoff+i)
               enddo
            elseif(itettyp(it).eq.ifelmqud) then
               cgmvtype='quad'
               nverts=0
               do i=1,nelmnen(itettyp(it))
                  nverts=nverts+1
                  nodeids(nverts)=itet1(itoff+i)
               enddo
            elseif(itettyp(it).eq.ifelmtet) then
               if(itet1(itoff+1).eq.itet1(itoff+4)) then
                  cgmvtype='tri'
                  nverts=0
                  do i=1,nelmnen(ifelmtri)
                     nverts=nverts+1
                     nodeids(nverts)=itet1(itoff+i)
                  enddo
               else
                  cgmvtype='tet'
                  if_invert_ele = 1
                  if(idumptype .ne. 0)if_invert_ele = 0
                  nverts=0
                  do i=1,nelmnen(itettyp(it))
                     nverts=nverts+1
                     nodeids(nverts)=itet1(itoff+i)
                  enddo
C
C     Found error and LaGriT was not following GMV convention
C     Only invert connectivity if output is ascii.
C
                  if(if_invert_ele .eq. 1)then
                    i_invert1 = nodeids(nverts-2)
                    i_invert2 = nodeids(nverts-1)
                    nodeids(nverts-1) = i_invert1
                    nodeids(nverts-2) = i_invert2
                  endif
               endif
            elseif(itettyp(it).eq.ifelmpyr) then
               cgmvtype='pyramid'
               nverts=0
               nverts=nverts+1
               nodeids(nverts)=itet1(itoff+5)
               nverts=nverts+1
               nodeids(nverts)=itet1(itoff+1)
               nverts=nverts+1
               nodeids(nverts)=itet1(itoff+4)
               nverts=nverts+1
               nodeids(nverts)=itet1(itoff+3)
               nverts=nverts+1
               nodeids(nverts)=itet1(itoff+2)
            elseif(itettyp(it).eq.ifelmpri) then
               cgmvtype='prism'
               nverts=0
               do i=1,nelmnen(itettyp(it))
                  nverts=nverts+1
                  nodeids(nverts)=itet1(itoff+i)
               enddo
            elseif(itettyp(it).eq.ifelmhex) then
               if((itet1(itoff+1).eq.itet1(itoff+5)) .and.
     *            (itet1(itoff+2).eq.itet1(itoff+6)) .and.
     *            (itet1(itoff+3).eq.itet1(itoff+7)) .and.
     *            (itet1(itoff+4).eq.itet1(itoff+8))) then
                  cgmvtype='quad'
                  nverts=0
                  do i=1,nelmnen(ifelmqud)
                     nverts=nverts+1
                     nodeids(nverts)=itet1(itoff+i)
                  enddo
               else
                  cgmvtype='hex'
                  if_invert_ele = 1
                  if(idumptype .ne. 0)if_invert_ele = 0
                  nverts=0
                  do i=1,nelmnen(itettyp(it))
                     nverts=nverts+1
                     nodeids(nverts)=itet1(itoff+i)
                  enddo
C
C     Found error and LaGriT was not following GMV convention.
C     Only invert connectivity if output is ascii.
C
                  if(if_invert_ele .eq. 1)then
                    i_invert1 = nodeids(nverts-7)
                    i_invert2 = nodeids(nverts-6)
                    i_invert3 = nodeids(nverts-5)
                    i_invert4 = nodeids(nverts-4)
                    nodeids(nverts-7) = nodeids(nverts-3)
                    nodeids(nverts-6) = nodeids(nverts-2)
                    nodeids(nverts-5) = nodeids(nverts-1)
                    nodeids(nverts-4) = nodeids(nverts-0)
                    nodeids(nverts-3) = i_invert1
                    nodeids(nverts-2) = i_invert2
                    nodeids(nverts-1) = i_invert3
                    nodeids(nverts-0) = i_invert4
                  endif
               endif
            else
               nverts=0
            endif
            if (nverts.gt.0) then
               if (idumptype.eq.0.and.nverts.gt.0) then
C
C Deal with format problem when number of nodes is greater than one million
C
C    Use largest 32 bit integer, 2147483647, as max value.
C
                  
                  if(nnodes .lt. 1000000)then
                    write(iunit,"(a8,' ',i4,10i7)")
     *                cgmvtype,nverts,(nodeids(i),i=1,nverts)
                  elseif((nnodes .ge.    1000000) .and.
     *                   (nnodes .lt. 2147483647))then
                    write(iunit,"(a8,' ',i4,10i10)")
     *                cgmvtype,nverts,(nodeids(i),i=1,nverts)
                  else
                    write(logmess,'(a)')'ERROR: dumpgmv_hybrid '
                    call writloga('default',0,logmess,0,ics)
                    write(logmess,'(a)')'ERROR: ascii format i10'
               write(logmess,'(a)')' nnodes gt 2147483647 max value'
                    call writloga('default',0,logmess,0,ics)
                    return
                  endif
               else
                  do i=1,nverts
                     itemp(i)=isafe4(nodeids(i))
                  enddo
                  nverts4=isafe4(nverts)
                  call fgmvwritecelldata(cgmvtype,nverts4,itemp)
                  if (nverts4.eq.0) then
               call x3d_error(isubname,'can not write cell data.')
                  endif
               endif
            endif
         enddo
      endif
c
c  get name of velocity attribute - see if it exists
c
      call cmo_get_attinfo('velname',cmo,iout,rout,cvelnm,
     *                        ipout,lout,itype,ier)
      call cmo_get_info(cvelnm,cmo,ipvels,ilen,icmotype,ier)
c
c  if it exists get ioflag
c
      if(ier.eq.0) then
         call cmo_get_attparam(cvelnm,cmo,index,ctype,crank,
     *    clength,ctabinterp,cpers,cioflag,ierror_return)
 
         len1=icharlnf(cioflag)
         iflag=0
         do j=1,len1
            if(cioflag(j:j).eq.'g') iflag=1
         enddo
         if(iflag.eq.1) then
            if(idumptype.eq.0) then
               write(iunit,"('velocity 1')")
               write(iunit,"(4(1x,1pe22.14e3))") (vels(1,i),i=1,nnodes)
               write(iunit,"(4(1x,1pe22.14e3))") (vels(2,i),i=1,nnodes)
               write(iunit,"(4(1x,1pe22.14e3))") (vels(3,i),i=1,nnodes)
            else
               igmvtype=1
               length=nnodes
               call mmgetblk('u',isubname,ipu,length,2,icscode)
               call mmgetblk('v',isubname,ipv,length,2,icscode)
               call mmgetblk('w',isubname,ipw,length,2,icscode)
               do i1=1,nnodes
                     u(i1)=safe8to4(vels(1,i1))
                     v(i1)=safe8to4(vels(2,i1))
                     w(i1)=safe8to4(vels(3,i1))
               enddo
               call fgmvwritevelocitydata(igmvtype, u, v, w)
               call mmrelblk('u',isubname,ipu,icscode)
               call mmrelblk('v',isubname,ipv,icscode)
               call mmrelblk('w',isubname,ipw,icscode)
            endif
         endif
      endif
c
c
c     write the variable data block
c
      if(idumptype.eq.0) then
         write(iunit,"('variable')")
      else
	call fgmvwritevariableheader()
      endif
C
C
C
C.... Get the Current Mesh Object storage block info.
C
      call cmo_get_info('number_of_attributes',cmo,
     *                   nmcmoatt,ilen,itype,icscode)
C
C....    loop through all attriutes get NAME Field.
C
      do i=1,nmcmoatt
C
         call cmo_get_attribute_name(cmo,i,cname,icscode)
C
C....    IOFLAG TYPE, Intepolation, length and rank Field.
C
         call cmo_get_attparam(cname,cmo,index,ctype,
     *          crank,clength,ctabinterp,cpers,cioflag,ierror_return)
 
         len1=icharlnf(cioflag)
         iflag=0
         do j=1,len1
            if(cioflag(j:j).eq.'g') iflag=1
         enddo
         if(iflag.eq.1) then
            lent=icharlnf(ctype)
            if(ctype(1:lent).eq.'VINT') then
C
               call cmo_get_length(cname,cmo,length,irank,
     *                             ierror_return)
C
               mmlength=irank*length
C
               call mmfindbk(cname,
     *                       cmo,
     *                       ipcmo_pointer,mmlength,
     *                       ierror_return)
C
               if(ierror_return.ne.0) then
               call x3d_error(isubname,'mmfindbk attribute')
               else
                  call mmgetblk('xvar',isubname,
     *                          ipxvar,mmlength,2,ics)
                  do j=1,length*irank
                     xvar(j)=icmo_pointer(j)
                  enddo
                  if((length      .eq.  nnodes) .and.
     *              (clength(1:6) .eq. 'nnodes')) then
                     if(idumptype.eq.0) then
                        write(iunit,"(a,' 1')") cname
                        write(iunit,"(10(1pe14.5e3))")
     *                     (xvar(j),j=1,length)
                     else
                        cgmvtype=cname
                        igmvtype=1
                        call fgmvwritevariablenamedata(cgmvtype,
     *                                                 igmvtype,
     *                                                   xvar)
                     endif
                  elseif((length      .eq.  nelements) .and.
     *                  (clength(1:9) .eq. 'nelements')) then
                     if(idumptype.eq.0) then
                        write(iunit,"(a,' 0')") cname
                        write(iunit,"(10(1pe14.5e3))")
     *                     (xvar(j),j=1,length)
                     else
                        cgmvtype=cname
                        igmvtype=0
                        call fgmvwritevariablenamedata(cgmvtype,
     *                                                 igmvtype,
     *                                                   xvar)
                     endif
                  else
                     write(logmess,9100) cname(1:icharlnf(cname)),
     *                                   length,irank
                     call writloga('default',0,logmess,0,ics)
 9100                format("Invalid length for GMV variable: ",
     *                      a,' length=',i10,' rank=',i10)
                     call x3d_error(isubname,'Illegal cmo_name')
                  endif
                  call mmrelblk('xvar',isubname,ipxvar,ics)
               endif
            elseif(ctype(1:lent).eq.'VDOUBLE') then
C
               call cmo_get_length(cname,cmo,length,irank,
     *                             ierror_return)
C
               mmlength=irank*length
C
               call mmfindbk(cname,
     *                       cmo,
     *                       ipcmo_pointer,mmlength,
     *                       ierror_return)
               if(ierror_return.ne.0) then
                  call x3d_error(isubname,'mmfindbk')
               else
                  call mmgetblk('xvar',isubname,
     *                          ipxvar,mmlength,2,ics)
                  do j=1,length*irank
                     xvar(j)=safe8to4(
     *                 cinterpolate('function',ctabinterp,
     *                                 xcmo_pointer(j)))
                  enddo
                  if((length      .eq.  nnodes) .and.
     *              (clength(1:6) .eq. 'nnodes')) then
                     if(idumptype.eq.0) then
                        write(iunit,"(a,' 1')") cname
                        write(iunit,"(10(1pe14.5e3))")
     *                     (xvar(j),j=1,length)
                     else
                        cgmvtype=cname
                        igmvtype=1
                        call fgmvwritevariablenamedata(cgmvtype,
     *                                                   igmvtype,
     *                                                   xvar)
                     endif
                  elseif((length      .eq.  nelements) .and.
     *                  (clength(1:9) .eq. 'nelements')) then
                     if(idumptype.eq.0) then
                        write(iunit,"(a,' 0')") cname
                        write(iunit,"(10(1pe14.5e3))")
     *                     (xvar(j),j=1,length)
                     else
                        cgmvtype=cname
                        igmvtype=0
                        call fgmvwritevariablenamedata(cgmvtype,
     *                                                   igmvtype,
     *                                                   xvar)
                     endif
                  else
                     write(logmess,9100) cname(1:icharlnf(cname)),
     *                                   length,irank
                     call writloga('default',0,logmess,0,ics)
                     call x3d_error(isubname,
     *                 'Attribute not GMV compatible')
                  endif
                  call mmrelblk('xvar',isubname,ipxvar,ics)
               endif
            endif
         endif
      enddo
C
      if(idumptype.eq.0) then
         write(iunit,"('endvars')")
      else
	call fgmvwritevariableendvars()
      endif

      maxclrpoint=0
      if (imt_exist.gt.0) then
        do i=1,nnodes
           maxclrpoint=max(maxclrpoint,max(1,imt1(i)))
        enddo
      endif
      if(idumptype.eq.0) then
         write(iunit,"('flags    ')")
         write(iunit,"('imt1   ',i10,'         1')") maxclrpoint+1
      else
         cgmvtype='imt1'
         igmvtype=1
         jgmvtype=maxclrpoint+1
         call fgmvwriteflagheader( )
         call fgmvwriteflagname(cgmvtype, igmvtype, jgmvtype )
      endif

      do i=1,maxclrpoint
         if(nmregs.ge.i) then
             iprtname=cmregs(i)
         else
             write(iprtname,"('mat',i5.5)") i
         endif
         if(ierror.eq.0) then
            if(idumptype.eq.0) then
               write(iunit,'(a8)') iprtname
            else
               cgmvtype=iprtname
               call fgmvwriteflagtype(cgmvtype)
            endif
         else
            if(idumptype.eq.0) then
               write(iunit,'(a3,i5.5)') 'imt-',i
            else
               write(cgmvtype,'(a3,i5.5)') 'imt-',i
               call fgmvwriteflagtype(cgmvtype)
            endif
         endif
      enddo
      if(idumptype.eq.0) then
         write(iunit,'(a5)') 'ERROR'
      else
         write(cgmvtype,'(a5)') 'ERROR'
         call fgmvwriteflagtype(cgmvtype)
      endif
      do i=1,nnodes
         if(ireal1(i).eq.0) then
            itemp(i)=maxclrpoint+1
         elseif(imt_exist .eq. 0) then
            itemp(i)=maxclrpoint+1
         elseif(imt1(i).le.0) then
            itemp(i)=maxclrpoint+1
         elseif(imt1(i).gt.maxclrpoint) then
            itemp(i)=maxclrpoint+1
         elseif(imt_exist.gt.0) then
            itemp(i)=imt1(i)
         endif
      enddo
      if(idumptype.eq.0) then
         write(iunit,"(20i5)") (itemp(i),i=1,nnodes)
      else
         call fgmvwriteflagdata(igmvtype, itemp)
      endif

c     iflag_all does not change from 0 in current code
c     otherwise, code will have to check that attributes exist
      if(iflag_all.eq.1) then
         maxitp1=0
         do i=1,nnodes
            maxitp1=max(maxitp1,itp1(i))
         enddo
         if(idumptype.eq.0) then
            write(iunit,"('itp1   ',i10,'         1')") maxitp1
         else
            cgmvtype='itp1'
            igmvtype=1
            jgmvtype=maxitp1
            call fgmvwriteflagname(cgmvtype,igmvtype,jgmvtype)
         endif
         do i=1,maxitp1
            if(idumptype.eq.0) then
               write(iunit,'(a3,i5.5)') 'itp-',i
            else
               write(cgmvtype,'(a3,i5.5)') 'itp-',i
               call fgmvwriteflagtype(cgmvtype)
            endif
         enddo
         if(idumptype.eq.0) then
            write(iunit,"(20i5)") (itp1(i),i=1,nnodes)
         else
            do i=1,nnodes
               itemp(i)=itp1(i)
            enddo
            call fgmvwriteflagdata(igmvtype, itemp)
         endif

         maxicr1=0
         do i=1,nnodes
            maxicr1=max(maxicr1,icr1(i))
         enddo
         if(idumptype.eq.0) then
            write(iunit,"('icr1   ',i10,'         1')") maxicr1
         else
            cgmvtype='icr1'
            igmvtype=1
            jgmvtype=maxicr1
            call fgmvwriteflagname(cgmvtype,igmvtype,jgmvtype)
         endif
         do i=1,maxicr1
            if(idumptype.eq.0) then
               write(iunit,'(a3,i5.5)') 'icr-',i
            else
               write(cgmvtype,'(a3,i5.5)') 'icr-',i
               call fgmvwriteflagtype(cgmvtype)
            endif
         enddo
         if(idumptype.eq.0) then
            write(iunit,"(20i5)") (icr1(i),i=1,nnodes)
         else
            do i=1,nnodes
               itemp(i)=icr1(i)
            enddo
            call fgmvwriteflagdata(igmvtype, itemp)
         endif
      endif
      if(idumptype.eq.0) then
         write(iunit,"('endflag')")
      else
         call fgmvwriteflagendflag()
      endif
c
c
c     write polygon data
c
C
C     DETERMINE THE COLOR THAT WILL BE WRITTEN TO THE "MATERIAL" COLOR
C        BLOCK.
C
      call mmfindbk('itetclr',cmo,ipitetclr,lenout,icscode)
      if(icscode.eq.0) then
         maxclrelement=0
         do it=1,nelements
            maxclrelement=max(maxclrelement,max(1,itetclr(it)))
         enddo
         imatcolor=0
         nmatcolor=nelements
         do it=1,nelements
            if(itetclr(it).le.0) then
               itemp(it)=maxclrelement+1
            elseif(itetclr(it).gt.maxclrelement) then
               itemp(it)=maxclrelement
            else
               itemp(it)=itetclr(it)
            endif
         enddo
         call mmfindbk('itetkid',cmo,ipitetkid,lenout,icskid)
         if(icskid.eq.0) then
            do it=1,nelements
               if(itetkid(it).ne.0) itemp(it)=0
            enddo
         endif
      else
         maxclrelement=maxclrpoint
         imatcolor=1
         nmatcolor=nnodes
         do i=1,nnodes
            if(imt_exist.gt.0) then
              if(imt1(i).le.0) then
                 itemp(i)=maxclrpoint+1
              elseif(imt1(i).gt.maxclrpoint) then
                 itemp(i)=maxclrpoint
              else
                 itemp(i)=imt1(i)
              endif
            else
                 itemp(i)=maxclrpoint
            endif
         enddo
      endif
C
      if(ipolydata.eq.0) then
         if(nelements.gt.0) then
            if(idumptype.eq.0) then
               write(iunit,9020) maxclrelement+1, imatcolor
 9020          format('material   ',2i10)
            else
               nmats=maxclrelement+1
               call fgmvwritematerialheader(nmats, imatcolor)
            endif
            do i=1,maxclrelement
               if(nmregs.ge.i) then
                iprtname=cmregs(i)
               else
                 write(iprtname,"('mat',i5.5)") i
               endif
               if(ierror.eq.0) then
                  if(idumptype.eq.0) then
                     write(iunit,'(a8)') iprtname
                  else
                     cgmvtype=iprtname
                     call fgmvwritematerialname(cgmvtype)
                  endif
               else
                  if(idumptype.eq.0) then
                     write(iunit,'(a3,i5.5)') 'mat',i
                  else
                     write(cgmvtype,'(a3,i5.5)') 'mat',i
                     call fgmvwritematerialname(cgmvtype)
                  endif
               endif
            enddo
            if(idumptype.eq.0) then
               write(iunit,'(a5)') 'ERROR'
            else
               cgmvtype='ERROR'
               call fgmvwritematerialname(cgmvtype)
            endif
            if(idumptype.eq.0) then
               write(iunit,"(20i5)") (itemp(it),it=1,nmatcolor)
            else
               call fgmvwritematerialids(itemp, imatcolor)
            endif
         endif
      endif
      if(ipolydata.eq.1) then
         call get_epsilon('epsilonl', epsilonl)
         do ie=1,nelements
            itoff=itetoff(ie)
            jtoff=jtetoff(ie)
C
            distmax=epsilonl
            do i=1,nelmnee(itettyp(ie))
               i1=itet1(itoff+ielmedge1(1,i,itettyp(ie)))
               i2=itet1(itoff+ielmedge1(2,i,itettyp(ie)))
               dist=(xic(i2)-xic(i1))**2+
     *              (yic(i2)-yic(i1))**2+
     *              (zic(i2)-zic(i1))**2
               distmax=max(distmax,dist)
            enddo

c Old test was:
c            disttest=epsilonl*distmax
c Although this was approximately right, this was an accident.
c In fact tolerance for square of length should be square of
c cell diameter multiplied by square of dimensionless unit roundoff
c Since we are outputting in single precision, square of unit
c roundoff is (1e-8)^2=1e-16.
            disttest=1.e-16*distmax
C
         if(itettyp(ie).eq.ifelmlin) then
            if(nelements.gt.0.and.ie.eq.1) then
               if(idumptype.eq.0) then
                  write(iunit,9020) 3*maxclrelement+1, imatcolor
               else
                  nmats=3*maxclrelement+1
                  call fgmvwritematerialheader(nmats, imatcolor)
               endif
               do i=1,maxclrelement
                  if(nmregs.ge.i) then
                    iprtname=cmregs(i)
                  else
                     write(iprtname,"('mat',i5.5)") i
                  endif
                  if(ierror.eq.0) then
                     if(idumptype.eq.0) then
                        write(iunit,'(a8)') iprtname
                     else
                        cgmvtype=iprtname
                        call fgmvwritematerialname(cgmvtype)
                     endif
                  else
                     if(idumptype.eq.0) then
                        write(iunit,'(a3,i5.5)') 'mat',i
                     else
                        write(cgmvtype,'(a3,i5.5)') 'mat',i
                        call fgmvwritematerialname(cgmvtype)
                     endif
                  endif
               enddo
               if(idumptype.eq.0) then
                  write(iunit,'(a5)') 'ERROR'
               else
                  cgmvtype='ERROR'
                  call fgmvwritematerialname(cgmvtype)
               endif
               do i=1,maxclrelement
                  if(idumptype.eq.0) then
                     write(iunit,'(a3,i5.5)') 'vor',i
                  else
                     write(cgmvtype,'(a3,i5.5)') 'vor',i
                     call fgmvwritematerialname(cgmvtype)
                  endif
               enddo
               do i=1,maxclrelement
                  if(idumptype.eq.0) then
                     write(iunit,'(a3,i5.5)') 'med',i
                  else
                     write(cgmvtype,'(a3,i5.5)') 'med',i
                     call fgmvwritematerialname(cgmvtype)
                  endif
               enddo
               if(idumptype.eq.0) then
                  write(iunit,"(20i5)") (itemp(it),it=1,nmatcolor)
               else
                  call fgmvwritematerialids(itemp, imatcolor)
               endif
               if(idumptype.eq.0) then
                  write(iunit,"('polygons')")
               else
                  call fgmvwritepolygonsheader()
               endif
            endif
            it=ie
            i1=itet1(itetoff(it)+1)
            i2=itet1(itetoff(it)+2)
            if(itetclr(it).le.0) then
               iclr=maxclrelement+1
            elseif(itetclr(it).gt.maxclrelement) then
               iclr=maxclrelement+1
            else
               iclr=itetclr(it)
            endif
            if(idumptype.eq.0) then
               write(iunit,9010)
     *               iclr,2,
     *               xic(i1),xic(i2),
     *               yic(i1),yic(i2),
     *               zic(i1),zic(i2)
 9010          format(2i5,2(1x,1pe22.14e3),/,2(1x,1pe22.14e3),/,
     *                      2(1x,1pe22.14e3))
            else
               nverts=1
               xicpoly(nverts)=safe8to4(xic(i1))
               yicpoly(nverts)=safe8to4(yic(i1))
               zicpoly(nverts)=safe8to4(zic(i1))
               nverts=nverts+1
               xicpoly(nverts)=safe8to4(xic(i2))
               yicpoly(nverts)=safe8to4(yic(i2))
               zicpoly(nverts)=safe8to4(zic(i2))
               nverts4=isafe4(nverts)
               call fgmvwritepolygonsdata(nverts4,
     *                                     iclr,
     *                                     xicpoly,
     *                                     yicpoly,
     *                                     zicpoly)
            endif
         elseif(itettyp(ie).eq.ifelmtri) then
            if(nelements.gt.0.and.ie.eq.1) then
               if(idumptype.eq.0) then
                  write(iunit,9020) 3*maxclrelement+1, imatcolor
               else
                  nmats=3*maxclrelement+1
                  call fgmvwritematerialheader(nmats, imatcolor)
               endif
               do i=1,maxclrelement
                  if(nmregs.ge.i) then
                   iprtname=cmregs(i)
                  else
                    write(iprtname,"('mat',i5.5)") i
                  endif
 
                  if(ierror.eq.0) then
                     if(idumptype.eq.0) then
                        write(iunit,'(a8)') iprtname
                     else
                        cgmvtype=iprtname
                        call fgmvwritematerialname(cgmvtype)
                     endif
                  else
                     if(idumptype.eq.0) then
                        write(iunit,'(a3,i5.5)') 'mat',i
                     else
                        write(cgmvtype,'(a3,i5.5)') 'mat',i
                        call fgmvwritematerialname(cgmvtype)
                     endif
                  endif
               enddo
               if(idumptype.eq.0) then
                  write(iunit,'(a5)') 'ERROR'
               else
                  cgmvtype='ERROR'
                  call fgmvwritematerialname(cgmvtype)
               endif
               do i=1,maxclrelement
                  if(idumptype.eq.0) then
                     write(iunit,'(a3,i5.5)') 'vor',i
                  else
                     write(cgmvtype,'(a3,i5.5)') 'vor',i
                     call fgmvwritematerialname(cgmvtype)
                  endif
               enddo
               do i=1,maxclrelement
                  if(idumptype.eq.0) then
                     write(iunit,'(a3,i5.5)') 'med',i
                  else
                     write(cgmvtype,'(a3,i5.5)') 'med',i
                     call fgmvwritematerialname(cgmvtype)
                  endif
               enddo
               if(idumptype.eq.0) then
                  write(iunit,"(20i5)") (itemp(it),it=1,nmatcolor)
               else
                  call fgmvwritematerialids(itemp, imatcolor)
               endif
               if(idumptype.eq.0) then
                  write(iunit,"('polygons')")
               else
                  call fgmvwritepolygonsheader()
               endif
            endif
            it=ie
            nef=nelmnef(ifelmtri)
            do i=1,nef
               iflag=0
               if(jtet1(jtoff+i).le.0.or.
     *            jtet1(jtoff+i).eq.mbndry) then
                  iflag=1
               elseif(jtet1(jtoff+i).gt.mbndry) then
                  jt=1+(jtet1(jtoff+i)-mbndry-1)/nef_cmo
                  jf=jtet1(jtoff+i)-mbndry-nef_cmo*(jt-1)
                  if(itetclr(it).ne.itetclr(jt)) iflag=2
               else
                  jt=1+(jtet1(jtoff+i)-1)/nef_cmo
                  jf=jtet1(jtoff+i)-nef_cmo*(jt-1)
                  if(itetclr(it).ne.itetclr(jt)) iflag=2
               endif
               iflag=0
               if(iflag.ne.0) then
                  j1=itet1(itoff+itriface1(1,i))
                  j2=itet1(itoff+itriface1(2,i))
                  if(itetclr(it).le.0) then
                     iclr=maxclrelement+1
                  elseif(itetclr(it).gt.maxclrelement) then
                     iclr=maxclrelement+1
                  else
                     iclr=itetclr(it)
                  endif
                  if(idumptype.eq.0) then
                     write(iunit,9010)
     *                     iclr,2,
     *                     xic(j1),xic(j2),
     *                     yic(j1),yic(j2),
     *                     zic(j1),zic(j2)
                  else
                     nverts=1
                     xicpoly(nverts)=safe8to4(xic(j1))
                     yicpoly(nverts)=safe8to4(yic(j1))
                     zicpoly(nverts)=safe8to4(zic(j1))
                     nverts=nverts+1
                     xicpoly(nverts)=safe8to4(xic(j2))
                     yicpoly(nverts)=safe8to4(yic(j2))
                     zicpoly(nverts)=safe8to4(zic(j2))
                     nverts4=isafe4(nverts)
                     call fgmvwritepolygonsdata(nverts4,
     *                                           iclr,
     *                                           xicpoly,
     *                                           yicpoly,
     *                                           zicpoly)
                  endif
               endif
            enddo
            i1=itet1(itoff+1)
            i2=itet1(itoff+2)
            i3=itet1(itoff+3)
            if(itetclr(it).le.0) then
               iclr=maxclrelement+1
            elseif(itetclr(it).gt.maxclrelement) then
               iclr=maxclrelement+1
            else
               iclr=itetclr(it)
            endif
            if(idumptype.eq.0) then
               write(iunit,9010)
     *               iclr,3,
     *               xic(i1),xic(i2),xic(i3),
     *               yic(i1),yic(i2),yic(i3),
     *               zic(i1),zic(i2),zic(i3)
            else
               nverts=1
               xicpoly(nverts)=safe8to4(xic(i1))
               yicpoly(nverts)=safe8to4(yic(i1))
               zicpoly(nverts)=safe8to4(zic(i1))
               nverts=nverts+1
               xicpoly(nverts)=safe8to4(xic(i2))
               yicpoly(nverts)=safe8to4(yic(i2))
               zicpoly(nverts)=safe8to4(zic(i2))
               nverts=nverts+1
               xicpoly(nverts)=safe8to4(xic(i3))
               yicpoly(nverts)=safe8to4(yic(i3))
               zicpoly(nverts)=safe8to4(zic(i3))
               nverts4=isafe4(nverts)
               call fgmvwritepolygonsdata(nverts4,
     *                                     iclr,
     *                                     xicpoly,
     *                                     yicpoly,
     *                                     zicpoly)
            endif

C           ivoronoi2d for tri 
            if(ivoronoi2d.gt.0) then
               i1=itet1(itoff+1)
               i2=itet1(itoff+2)
               i3=itet1(itoff+3)
               x1=xic(i1)
               y1=yic(i1)
               z1=zic(i1)
               x2=xic(i2)
               y2=yic(i2)
               z2=zic(i2)
               x3=xic(i3)
               y3=yic(i3)
               z3=zic(i3)
               xm=(xic(i1)+xic(i2)+xic(i3))/3.0
               ym=(yic(i1)+yic(i2)+yic(i3))/3.0
               zm=(zic(i1)+zic(i2)+zic(i3))/3.0
               xv=xm
               yv=ym
               zv=zm
               xa=xic(i1)
               ya=yic(i1)
               za=zic(i1)
               xfac=1.0
               xb=xfac*(xic(i2)-xa)
               yb=xfac*(yic(i2)-ya)
               zb=xfac*(zic(i2)-za)
               xd=xfac*(xic(i3)-xa)
               yd=xfac*(yic(i3)-ya)
               zd=xfac*(zic(i3)-za)
               xn1=crosx(xb,yb,zb,xd,yd,zd)
               yn1=crosy(xb,yb,zb,xd,yd,zd)
               zn1=crosz(xb,yb,zb,xd,yd,zd)
               xn=crosx(xb,yb,zb,xn1,yn1,zn1)
               yn=crosy(xb,yb,zb,xn1,yn1,zn1)
               zn=crosz(xb,yb,zb,xn1,yn1,zn1)
               rn=1.0/sqrt(xn*xn+yn*yn+zn*zn)
               xn=xn*rn
               yn=yn*rn
               zn=zn*rn
               dotb3=xb*xd+yb*yd+zb*zd
               dot3=dotb3/(xd*xd+yd*yd+zd*zd)
               rb3=1.0/(xb*xb+yb*yb+zb*zb)
               ql=(1.0-dot3)/(1.0-dot3*dotb3*rb3+epsilonl)
               xl=0.5*(ql*(xd-dotb3*rb3*xb)+xb)
               yl=0.5*(ql*(yd-dotb3*rb3*yb)+yb)
               zl=0.5*(ql*(zd-dotb3*rb3*zb)+zb)
               ds1=sqrt((xl)**2+(yl)**2+(zl)**2)
               ds2=sqrt((xl-xb)**2+(yl-yb)**2+(zl-zb)**2)
               ds3=sqrt((xl-xd)**2+(yl-yd)**2+(zl-zd)**2)
               xv=xl+xa
               yv=yl+ya
               zv=zl+za
               x12=0.5*(xic(i1)+xic(i2))
               y12=0.5*(yic(i1)+yic(i2))
               z12=0.5*(zic(i1)+zic(i2))
               x13=0.5*(xic(i1)+xic(i3))
               y13=0.5*(yic(i1)+yic(i3))
               z13=0.5*(zic(i1)+zic(i3))
               x23=0.5*(xic(i2)+xic(i3))
               y23=0.5*(yic(i2)+yic(i3))
               z23=0.5*(zic(i2)+zic(i3))
               xcen=xv
               ycen=yv
               zcen=zv
               if(itetclr(it).le.0) then
                  iclrv=max(1,maxclrelement+1)
                  iclrm=max(1,maxclrelement+1)
               elseif(itetclr(it).gt.maxclrelement) then
                  iclrv=max(1,maxclrelement+1)
                  iclrm=max(1,maxclrelement+1)
               else
                  iclrv=  maxclrelement+1+itetclr(it)
                  iclrm=2*maxclrelement+1+itetclr(it)
               endif
C
               dist=(xcen-x12)**2+(ycen-y12)**2+(zcen-z12)**2
               if(dist.gt.disttest) then
                  if(idumptype.eq.0) then
                     write(iunit,9010) iclrv,2,
     *                                 xcen,x12,
     *                                 ycen,y12,
     *                                 zcen,z12
                  else
                     nverts=1
                     xicpoly(nverts)=safe8to4(xcen)
                     yicpoly(nverts)=safe8to4(ycen)
                     zicpoly(nverts)=safe8to4(zcen)
                     nverts=nverts+1
                     xicpoly(nverts)=safe8to4(x12)
                     yicpoly(nverts)=safe8to4(y12)
                     zicpoly(nverts)=safe8to4(z12)
                     nverts4=isafe4(nverts)
                     call fgmvwritepolygonsdata(nverts4,
     *                                           iclrv,
     *                                           xicpoly,
     *                                           yicpoly,
     *                                           zicpoly)
                  endif
               endif
               dist=(xcen-x13)**2+(ycen-y13)**2+(zcen-z13)**2
               if(dist.gt.disttest) then
                  if(idumptype.eq.0) then
                     write(iunit,9010) iclrv,2,
     *                                 xcen,x13,
     *                                 ycen,y13,
     *                                 zcen,z13
                  else
                     nverts=1
                     xicpoly(nverts)=safe8to4(xcen)
                     yicpoly(nverts)=safe8to4(ycen)
                     zicpoly(nverts)=safe8to4(zcen)
                     nverts=nverts+1
                     xicpoly(nverts)=safe8to4(x13)
                     yicpoly(nverts)=safe8to4(y13)
                     zicpoly(nverts)=safe8to4(z13)
                     nverts4=isafe4(nverts)
                     call fgmvwritepolygonsdata(nverts4,
     *                                           iclrv,
     *                                           xicpoly,
     *                                           yicpoly,
     *                                           zicpoly)
                  endif
               endif
               dist=(xcen-x23)**2+(ycen-y23)**2+(zcen-z23)**2
               if(dist.gt.disttest) then
                  if(idumptype.eq.0) then
                     write(iunit,9010) iclrv,2,
     *                                 xcen,x23,
     *                                 ycen,y23,
     *                                 zcen,z23
                  else
                     nverts=1
                     xicpoly(nverts)=safe8to4(xcen)
                     yicpoly(nverts)=safe8to4(ycen)
                     zicpoly(nverts)=safe8to4(zcen)
                     nverts=nverts+1
                     xicpoly(nverts)=safe8to4(x23)
                     yicpoly(nverts)=safe8to4(y23)
                     zicpoly(nverts)=safe8to4(z23)
                     nverts4=isafe4(nverts)
                     call fgmvwritepolygonsdata(nverts4,
     *                                           iclrv,
     *                                           xicpoly,
     *                                           yicpoly,
     *                                           zicpoly)
                  endif
               endif

C
               xcen=xm
               ycen=ym
               zcen=zm
               dist=(xcen-x12)**2+(ycen-y12)**2+(zcen-z12)**2
               if(dist.gt.disttest) then
                  if(idumptype.eq.0) then
                     write(iunit,9010) iclrm,2,
     *                                 xcen,x12,
     *                                 ycen,y12,
     *                                 zcen,z12
                  else
                     nverts=1
                     xicpoly(nverts)=safe8to4(xcen)
                     yicpoly(nverts)=safe8to4(ycen)
                     zicpoly(nverts)=safe8to4(zcen)
                     nverts=nverts+1
                     xicpoly(nverts)=safe8to4(x12)
                     yicpoly(nverts)=safe8to4(y12)
                     zicpoly(nverts)=safe8to4(z12)
                     nverts4=isafe4(nverts)
                     call fgmvwritepolygonsdata(nverts4,
     *                                           iclrm,
     *                                           xicpoly,
     *                                           yicpoly,
     *                                           zicpoly)
                  endif
               endif
               dist=(xcen-x13)**2+(ycen-y13)**2+(zcen-z13)**2
               if(dist.gt.disttest) then
                  if(idumptype.eq.0) then
                     write(iunit,9010) iclrm,2,
     *                                 xcen,x13,
     *                                 ycen,y13,
     *                                 zcen,z13
                  else
                     nverts=1
                     xicpoly(nverts)=safe8to4(xcen)
                     yicpoly(nverts)=safe8to4(ycen)
                     zicpoly(nverts)=safe8to4(zcen)
                     nverts=nverts+1
                     xicpoly(nverts)=safe8to4(x13)
                     yicpoly(nverts)=safe8to4(y13)
                     zicpoly(nverts)=safe8to4(z13)
                     nverts4=isafe4(nverts)
                     call fgmvwritepolygonsdata(nverts4,
     *                                           iclrm,
     *                                           xicpoly,
     *                                           yicpoly,
     *                                           zicpoly)
                  endif
               endif
               dist=(xcen-x23)**2+(ycen-y23)**2+(zcen-z23)**2
               if(dist.gt.disttest) then
                  if(idumptype.eq.0) then
                     write(iunit,9010) iclrm,2,
     *                                 xcen,x23,
     *                                 ycen,y23,
     *                                 zcen,z23
                  else
                     nverts=1
                     xicpoly(nverts)=safe8to4(xcen)
                     yicpoly(nverts)=safe8to4(ycen)
                     zicpoly(nverts)=safe8to4(zcen)
                     nverts=nverts+1
                     xicpoly(nverts)=safe8to4(x23)
                     yicpoly(nverts)=safe8to4(y23)
                     zicpoly(nverts)=safe8to4(z23)
                     nverts4=isafe4(nverts)
                     call fgmvwritepolygonsdata(nverts4,
     *                                           iclrm,
     *                                           xicpoly,
     *                                           yicpoly,
     *                                           zicpoly)
                  endif
               endif
            endif
C           end ivoronoi2d for tri

         elseif(itettyp(ie).eq.ifelmqud) then
            if(nelements.gt.0.and.ie.eq.1) then
               if(idumptype.eq.0) then
                  write(iunit,9020) 3*maxclrelement+1, imatcolor
               else
                  nmats=3*maxclrelement+1
                  call fgmvwritematerialheader(nmats, imatcolor)
               endif
               do i=1,maxclrelement
                 if(nmregs.ge.i) then
                   iprtname=cmregs(i)
                  else
                   write(iprtname,"('mat',i5.5)") i
                  endif
                  if(ierror.eq.0) then
                     if(idumptype.eq.0) then
                        write(iunit,'(a8)') iprtname
                     else
                        cgmvtype=iprtname
                        call fgmvwritematerialname(cgmvtype)
                     endif
                  else
                     if(idumptype.eq.0) then
                        write(iunit,'(a3,i5.5)') 'mat',i
                     else
                        write(cgmvtype,'(a3,i5.5)') 'mat',i
                        call fgmvwritematerialname(cgmvtype)
                     endif
                  endif
               enddo
               if(idumptype.eq.0) then
                  write(iunit,'(a5)') 'ERROR'
               else
                  cgmvtype='ERROR'
                  call fgmvwritematerialname(cgmvtype)
               endif
               do i=1,maxclrelement
                  if(idumptype.eq.0) then
                     write(iunit,'(a3,i5.5)') 'vor',i
                  else
                     write(cgmvtype,'(a3,i5.5)') 'vor',i
                     call fgmvwritematerialname(cgmvtype)
                  endif
               enddo
               do i=1,maxclrelement
                  if(idumptype.eq.0) then
                     write(iunit,'(a3,i5.5)') 'med',i
                  else
                     write(cgmvtype,'(a3,i5.5)') 'med',i
                     call fgmvwritematerialname(cgmvtype)
                  endif
               enddo
               if(idumptype.eq.0) then
                  write(iunit,"(20i5)") (itemp(it),it=1,nmatcolor)
               else
                  call fgmvwritematerialids(itemp, imatcolor)
               endif
               if(idumptype.eq.0) then
                  write(iunit,"('polygons')")
               else
                  call fgmvwritepolygonsheader()
               endif
            endif
            it=ie
            nef=nelmnef(ifelmqud)
            iflagkid=0
            if(icskid.eq.0) then
               if(itetkid(it).ne.0) iflagkid=1
            endif
            if(iflagkid.eq.0) then
               i1=itet1(itoff+1)
               i2=itet1(itoff+2)
               i3=itet1(itoff+3)
               i4=itet1(itoff+4)
               if(itetclr(it).le.0) then
                  iclr=maxclrelement+1
               elseif(itetclr(it).gt.maxclrelement) then
                  iclr=maxclrelement+1
               else
                  iclr=itetclr(it)
               endif
               if(idumptype.eq.0) then
                  write(iunit,9010)
     *                  iclr,4,
     *                  xic(i1),xic(i2),xic(i3),xic(i4),
     *                  yic(i1),yic(i2),yic(i3),yic(i4),
     *                  zic(i1),zic(i2),zic(i3),zic(i4)
               else
                  nverts=1
                  xicpoly(nverts)=safe8to4(xic(i1))
                  yicpoly(nverts)=safe8to4(yic(i1))
                  zicpoly(nverts)=safe8to4(zic(i1))
                  nverts=nverts+1
                  xicpoly(nverts)=safe8to4(xic(i2))
                  yicpoly(nverts)=safe8to4(yic(i2))
                  zicpoly(nverts)=safe8to4(zic(i2))
                  nverts=nverts+1
                  xicpoly(nverts)=safe8to4(xic(i3))
                  yicpoly(nverts)=safe8to4(yic(i3))
                  zicpoly(nverts)=safe8to4(zic(i3))
                  nverts=nverts+1
                  xicpoly(nverts)=safe8to4(xic(i4))
                  yicpoly(nverts)=safe8to4(yic(i4))
                  zicpoly(nverts)=safe8to4(zic(i4))
                  nverts4=isafe4(nverts)
                  call fgmvwritepolygonsdata(nverts4,
     *                                        iclr,
     *                                        xicpoly,
     *                                        yicpoly,
     *                                        zicpoly)
               endif
               do i=1,nef
                  iflag=0
                  if(jtet1(jtoff+i).le.0.or.
     *               jtet1(jtoff+i).eq.mbndry) then
                     iflag=1
                  elseif(jtet1(jtoff+i).gt.mbndry) then
                     jt=1+(jtet1(jtoff+i)-mbndry-1)/nef_cmo
                     jf=jtet1(jtoff+i)-mbndry-nef_cmo*(jt-1)
                     if(itetclr(it).ne.itetclr(jt)) iflag=2
                  else
                     jt=1+(jtet1(jtoff+i)-1)/nef_cmo
                     jf=jtet1(jtoff+i)-nef_cmo*(jt-1)
                     if(itetclr(it).ne.itetclr(jt)) iflag=2
                  endif
                  iflag=0
                  if(iflag.ne.0) then
                     j1=itet1(itoff+ielmface1(1,i,ifelmqud))
                     j2=itet1(itoff+ielmface1(2,i,ifelmqud))
                     if(itetclr(it).le.0) then
                        iclr=maxclrelement+1
                     elseif(itetclr(it).gt.maxclrelement) then
                        iclr=maxclrelement+1
                     else
                        iclr=itetclr(it)
                     endif
                     if(idumptype.eq.0) then
                        write(iunit,9010)
     *                        iclr,2,
     *                        xic(j1),xic(j2),
     *                        yic(j1),yic(j2),
     *                        zic(j1),zic(j2)
                     else
                        nverts=1
                        xicpoly(nverts)=safe8to4(xic(j1))
                        yicpoly(nverts)=safe8to4(yic(j1))
                        zicpoly(nverts)=safe8to4(zic(j1))
                        nverts=nverts+1
                        xicpoly(nverts)=safe8to4(xic(j2))
                        yicpoly(nverts)=safe8to4(yic(j2))
                        zicpoly(nverts)=safe8to4(zic(j2))
                        nverts4=isafe4(nverts)
                        call fgmvwritepolygonsdata(nverts4,
     *                                              iclr,
     *                                              xicpoly,
     *                                              yicpoly,
     *                                              zicpoly)
                     endif
                  endif
               enddo

C              ivoronoi2d for quad
               if(ivoronoi2d.gt.0) then
                  i1=itet1(itoff+1)
                  i2=itet1(itoff+2)
                  i3=itet1(itoff+3)
                  i4=itet1(itoff+4)
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
                  x12=0.5*(xic(i1)+xic(i2))
                  y12=0.5*(yic(i1)+yic(i2))
                  z12=0.5*(zic(i1)+zic(i2))
                  x23=0.5*(xic(i2)+xic(i3))
                  y23=0.5*(yic(i2)+yic(i3))
                  z23=0.5*(zic(i2)+zic(i3))
                  x34=0.5*(xic(i3)+xic(i4))
                  y34=0.5*(yic(i3)+yic(i4))
                  z34=0.5*(zic(i3)+zic(i4))
                  x41=0.5*(xic(i4)+xic(i1))
                  y41=0.5*(yic(i4)+yic(i1))
                  z41=0.5*(zic(i4)+zic(i1))
                  xm=(xic(i1)+xic(i2)+xic(i3)+xic(i4))/4.0
                  ym=(yic(i1)+yic(i2)+yic(i3)+yic(i4))/4.0
                  zm=(zic(i1)+zic(i2)+zic(i3)+zic(i4))/4.0
                  xcen=xm
                  ycen=ym
                  zcen=zm
C
                  if(itetclr(it).le.0) then
                     iclrm=max(1,maxclrelement+1)
                  elseif(itetclr(it).gt.maxclrelement) then
                     iclrm=max(1,maxclrelement+1)
                  else
                     iclrm=2*maxclrelement+1+itetclr(it)
                  endif
C
                  dist=(xcen-x12)**2+(ycen-y12)**2+(zcen-z12)**2
                  if(dist.gt.disttest) then
                     if(idumptype.eq.0) then
                        write(iunit,9010) iclrm,2,
     *                                    xcen,x12,
     *                                    ycen,y12,
     *                                    zcen,z12
                     else
                        nverts=1
                        xicpoly(nverts)=safe8to4(xcen)
                        yicpoly(nverts)=safe8to4(ycen)
                        zicpoly(nverts)=safe8to4(zcen)
                        nverts=nverts+1
                        xicpoly(nverts)=safe8to4(x12)
                        yicpoly(nverts)=safe8to4(y12)
                        zicpoly(nverts)=safe8to4(z12)
                        nverts4=isafe4(nverts)
                        call fgmvwritepolygonsdata(nverts4,
     *                                              iclrm,
     *                                              xicpoly,
     *                                              yicpoly,
     *                                              zicpoly)
                     endif
                  endif
                  dist=(xcen-x23)**2+(ycen-y23)**2+(zcen-z23)**2
                  if(dist.gt.disttest) then
                     if(idumptype.eq.0) then
                        write(iunit,9010) iclrm,2,
     *                                    xcen,x23,
     *                                    ycen,y23,
     *                                    zcen,z23
                     else
                        nverts=1
                        xicpoly(nverts)=safe8to4(xcen)
                        yicpoly(nverts)=safe8to4(ycen)
                        zicpoly(nverts)=safe8to4(zcen)
                        nverts=nverts+1
                        xicpoly(nverts)=safe8to4(x23)
                        yicpoly(nverts)=safe8to4(y23)
                        zicpoly(nverts)=safe8to4(z23)
                        nverts4=isafe4(nverts)
                        call fgmvwritepolygonsdata(nverts4,
     *                                              iclrm,
     *                                              xicpoly,
     *                                              yicpoly,
     *                                              zicpoly)
                     endif
                  endif
                  dist=(xcen-x34)**2+(ycen-y34)**2+(zcen-z34)**2
                  if(dist.gt.disttest) then
                     if(idumptype.eq.0) then
                        write(iunit,9010) iclrm,2,
     *                                    xcen,x34,
     *                                    ycen,y34,
     *                                    zcen,z34
                     else
                        nverts=1
                        xicpoly(nverts)=safe8to4(xcen)
                        yicpoly(nverts)=safe8to4(ycen)
                        zicpoly(nverts)=safe8to4(zcen)
                        nverts=nverts+1
                        xicpoly(nverts)=safe8to4(x34)
                        yicpoly(nverts)=safe8to4(y34)
                        zicpoly(nverts)=safe8to4(z34)
                        nverts4=isafe4(nverts)
                        call fgmvwritepolygonsdata(nverts4,
     *                                              iclrm,
     *                                              xicpoly,
     *                                              yicpoly,
     *                                              zicpoly)
                     endif
                  endif
                  dist=(xcen-x41)**2+(ycen-y41)**2+(zcen-z41)**2
                  if(dist.gt.disttest) then
                     if(idumptype.eq.0) then
                        write(iunit,9010) iclrm,2,
     *                                    xcen,x41,
     *                                    ycen,y41,
     *                                    zcen,z41
                     else
                        nverts=1
                        xicpoly(nverts)=safe8to4(xcen)
                        yicpoly(nverts)=safe8to4(ycen)
                        zicpoly(nverts)=safe8to4(zcen)
                        nverts=nverts+1
                        xicpoly(nverts)=safe8to4(x41)
                        yicpoly(nverts)=safe8to4(y41)
                        zicpoly(nverts)=safe8to4(z41)
                        nverts4=isafe4(nverts)
                        call fgmvwritepolygonsdata(nverts4,
     *                                              iclrm,
     *                                              xicpoly,
     *                                              yicpoly,
     *                                              zicpoly)
                     endif
                  endif
               endif
            endif
C           end ivoronoi2d for quad

         elseif(itettyp(ie).eq.ifelmtet) then
            if(nelements.gt.0.and.ie.eq.1) then
               iclr=maxclrelement+1
               call mmfindbk('itetkid',cmo,ipitetkid,lenout,icskid)
               if(ivoronoi3d.gt.0) iclr=2*maxclrelement+1
               if(idumptype.eq.0) then
                  write(iunit,"('material   ',2i10)") iclr,imatcolor
               else
                  nmats=iclr
                  call fgmvwritematerialheader(nmats, imatcolor)
               endif
               do i=1,maxclrelement
                 if(nmregs.ge.i) then
                   iprtname=cmregs(i)
                  else
                    write(iprtname,"('mat',i5.5)") i
                  endif
 
                  if(ierror.eq.0) then
                     if(idumptype.eq.0) then
                        write(iunit,'(a8)') iprtname
                     else
                        cgmvtype=iprtname
                        call fgmvwritematerialname(cgmvtype)
                     endif
                  else
                     if(idumptype.eq.0) then
                        write(iunit,'(a3,i5.5)') 'mat',i
                     else
                        write(cgmvtype,'(a3,i5.5)') 'mat',i
                        call fgmvwritematerialname(cgmvtype)
                     endif
                  endif
               enddo
               if(idumptype.eq.0) then
                  write(iunit,'(a5)') 'ERROR'
               else
                  write(cgmvtype,'(a5)') 'ERROR'
                  call fgmvwritematerialname(cgmvtype)
               endif
               if(ivoronoi3d.eq.1) then
                  do i=1,maxclrelement
                     if(idumptype.eq.0) then
                        write(iunit,'(a3,i5.5)') 'vor',maxclrelement+1+i
                     else
                        write(cgmvtype,'(a3,i5.5)') 'vor',
     *                                               maxclrelement+1+i
                        call fgmvwritematerialname(cgmvtype)
                     endif
                  enddo
               elseif(ivoronoi3d.eq.2) then
                  do i=1,maxclrelement
                     if(idumptype.eq.0) then
                        write(iunit,'(a3,i5.5)') 'med',maxclrelement+1+i
                     else
                        write(cgmvtype,'(a3,i5.5)') 'med',
     *                                              maxclrelement+1+i
                        call fgmvwritematerialname(cgmvtype)
                     endif
                  enddo
               elseif(ivoronoi3d.eq.3) then
                  do i=1,maxclrelement
                     if(idumptype.eq.0) then
                        write(iunit,'(a3,i5.5)') 'surf-vor',
     *                                           maxclrelement+1+i
                     else
                        write(cgmvtype,'(a3,i5.5)') 'surf-vor',
     *                                              maxclrelement+1+i
                        call fgmvwritematerialname(cgmvtype)
                     endif
                  enddo
               endif
               if(idumptype.eq.0) then
                  write(iunit,"(20i5)") (itemp(it),it=1,nmatcolor)
               else
                  call fgmvwritematerialids(itemp, imatcolor)
               endif
               if(idumptype.eq.0) then
                  write(iunit,"('polygons')")
               else
                  call fgmvwritepolygonsheader()
               endif
            endif
            it=ie
            nef=nelmnef(ifelmtet)
            do i=1,nef
               iflag=0
               if(jtet1(jtoff+i).le.0.or.
     *            jtet1(jtoff+i).eq.mbndry) then
                  iflag=1
               elseif(jtet1(jtoff+i).gt.mbndry) then
                  jt=1+(jtet1(jtoff+i)-mbndry-1)/nef_cmo
                  jf=jtet1(jtoff+i)-mbndry-nef_cmo*(jt-1)
                  if(itetclr(it).ne.itetclr(jt)) iflag=2
               else
                  jt=1+(jtet1(jtoff+i)-1)/nef_cmo
                  jf=jtet1(jtoff+i)-nef_cmo*(jt-1)
                  if(itetclr(it).ne.itetclr(jt)) iflag=2
               endif
               if(iflag.ne.0) then
                  j1=itet1(itoff+itetface1(1,i))
                  j2=itet1(itoff+itetface1(2,i))
                  j3=itet1(itoff+itetface1(3,i))
                  if(itetclr(it).le.0) then
                     iclr=maxclrelement+1
                  elseif(itetclr(it).gt.maxclrelement) then
                     iclr=maxclrelement+1
                  else
                     iclr=itetclr(it)
                  endif
                  if(idumptype.eq.0) then
                     write(iunit,9030) iclr,3,
     *                     xic(j1),xic(j2),xic(j3),
     *                     yic(j1),yic(j2),yic(j3),
     *                     zic(j1),zic(j2),zic(j3)
 9030                format(2i5,3(1x,1pe22.14e3),/,3(1x,1pe22.14e3),/,
     *                          3(1x,1pe22.14e3))
                  else
                     nverts=1
                     xicpoly(nverts)=safe8to4(xic(j1))
                     yicpoly(nverts)=safe8to4(yic(j1))
                     zicpoly(nverts)=safe8to4(zic(j1))
                     nverts=nverts+1
                     xicpoly(nverts)=safe8to4(xic(j2))
                     yicpoly(nverts)=safe8to4(yic(j2))
                     zicpoly(nverts)=safe8to4(zic(j2))
                     nverts=nverts+1
                     xicpoly(nverts)=safe8to4(xic(j3))
                     yicpoly(nverts)=safe8to4(yic(j3))
                     zicpoly(nverts)=safe8to4(zic(j3))
                     nverts4=isafe4(nverts)
                     call fgmvwritepolygonsdata(nverts4,
     *                                           iclr,
     *                                           xicpoly,
     *                                           yicpoly,
     *                                           zicpoly)
                  endif
               endif
            enddo
            if(ivoronoi3d.gt.0) then
               if(itetclr(it).le.0) then
                  iclr=maxclrelement+1
               elseif(itetclr(it).gt.maxclrelement) then
                  iclr=maxclrelement+1
               else
                  iclr=maxclrelement+1+itetclr(it)
               endif
               i1=itet1(itoff+1)
               i2=itet1(itoff+2)
               i3=itet1(itoff+3)
               i4=itet1(itoff+4)
               x12=0.5*(xic(i1)+xic(i2))
               y12=0.5*(yic(i1)+yic(i2))
               z12=0.5*(zic(i1)+zic(i2))
               x13=0.5*(xic(i1)+xic(i3))
               y13=0.5*(yic(i1)+yic(i3))
               z13=0.5*(zic(i1)+zic(i3))
               x14=0.5*(xic(i1)+xic(i4))
               y14=0.5*(yic(i1)+yic(i4))
               z14=0.5*(zic(i1)+zic(i4))
               x23=0.5*(xic(i2)+xic(i3))
               y23=0.5*(yic(i2)+yic(i3))
               z23=0.5*(zic(i2)+zic(i3))
               x24=0.5*(xic(i2)+xic(i4))
               y24=0.5*(yic(i2)+yic(i4))
               z24=0.5*(zic(i2)+zic(i4))
               x34=0.5*(xic(i3)+xic(i4))
               y34=0.5*(yic(i3)+yic(i4))
               z34=0.5*(zic(i3)+zic(i4))
               if(ivoronoi3d.eq.1) then
                  xa=xic(i2)
                  ya=yic(i2)
                  za=zic(i2)
                  xfac=1.0d+00
                  xb=xfac*(xic(i3)-xa)
                  yb=xfac*(yic(i3)-ya)
                  zb=xfac*(zic(i3)-za)
                  xd=xfac*(xic(i4)-xa)
                  yd=xfac*(yic(i4)-ya)
                  zd=xfac*(zic(i4)-za)
                  xn1=crosx(xb,yb,zb,xd,yd,zd)
                  yn1=crosy(xb,yb,zb,xd,yd,zd)
                  zn1=crosz(xb,yb,zb,xd,yd,zd)
                  xn=crosx(xb,yb,zb,xn1,yn1,zn1)
                  yn=crosy(xb,yb,zb,xn1,yn1,zn1)
                  zn=crosz(xb,yb,zb,xn1,yn1,zn1)
                  rn=1.0/sqrt(xn*xn+yn*yn+zn*zn)
                  xn=xn*rn
                  yn=yn*rn
                  zn=zn*rn
                  dotb3=xb*xd+yb*yd+zb*zd
                  dot3=dotb3/(xd*xd+yd*yd+zd*zd)
                  rb3=1.0/(xb*xb+yb*yb+zb*zb)
                  ql=(1.0-dot3)/(1.0-dot3*dotb3*rb3+epsilonl)
                  xl=0.5*(ql*(xd-dotb3*rb3*xb)+xb)
                  yl=0.5*(ql*(yd-dotb3*rb3*yb)+yb)
                  zl=0.5*(ql*(zd-dotb3*rb3*zb)+zb)
                  ds11=sqrt((xl)**2+(yl)**2+(zl)**2)
                  ds21=sqrt((xl-xb)**2+(yl-yb)**2+(zl-zb)**2)
                  ds31=sqrt((xl-xd)**2+(yl-yd)**2+(zl-zd)**2)
                  xv1=xl+xa
                  yv1=yl+ya
                  zv1=zl+za
                  xa=xic(i1)
                  ya=yic(i1)
                  za=zic(i1)
                  xfac=1.0d+00
                  xb=xfac*(xic(i4)-xa)
                  yb=xfac*(yic(i4)-ya)
                  zb=xfac*(zic(i4)-za)
                  xd=xfac*(xic(i3)-xa)
                  yd=xfac*(yic(i3)-ya)
                  zd=xfac*(zic(i3)-za)
                  xn1=crosx(xb,yb,zb,xd,yd,zd)
                  yn1=crosy(xb,yb,zb,xd,yd,zd)
                  zn1=crosz(xb,yb,zb,xd,yd,zd)
                  xn=crosx(xb,yb,zb,xn1,yn1,zn1)
                  yn=crosy(xb,yb,zb,xn1,yn1,zn1)
                  zn=crosz(xb,yb,zb,xn1,yn1,zn1)
                  rn=1.0/sqrt(xn*xn+yn*yn+zn*zn)
                  xn=xn*rn
                  yn=yn*rn
                  zn=zn*rn
                  dotb3=xb*xd+yb*yd+zb*zd
                  dot3=dotb3/(xd*xd+yd*yd+zd*zd)
                  rb3=1.0/(xb*xb+yb*yb+zb*zb)
                  ql=(1.0-dot3)/(1.0-dot3*dotb3*rb3+epsilonl)
                  xl=0.5*(ql*(xd-dotb3*rb3*xb)+xb)
                  yl=0.5*(ql*(yd-dotb3*rb3*yb)+yb)
                  zl=0.5*(ql*(zd-dotb3*rb3*zb)+zb)
                  ds12=sqrt((xl)**2+(yl)**2+(zl)**2)
                  ds22=sqrt((xl-xb)**2+(yl-yb)**2+(zl-zb)**2)
                  ds32=sqrt((xl-xd)**2+(yl-yd)**2+(zl-zd)**2)
                  xv2=xl+xa
                  yv2=yl+ya
                  zv2=zl+za
                  xa=xic(i1)
                  ya=yic(i1)
                  za=zic(i1)
                  xfac=1.0d+00
                  xb=xfac*(xic(i2)-xa)
                  yb=xfac*(yic(i2)-ya)
                  zb=xfac*(zic(i2)-za)
                  xd=xfac*(xic(i4)-xa)
                  yd=xfac*(yic(i4)-ya)
                  zd=xfac*(zic(i4)-za)
                  xn1=crosx(xb,yb,zb,xd,yd,zd)
                  yn1=crosy(xb,yb,zb,xd,yd,zd)
                  zn1=crosz(xb,yb,zb,xd,yd,zd)
                  xn=crosx(xb,yb,zb,xn1,yn1,zn1)
                  yn=crosy(xb,yb,zb,xn1,yn1,zn1)
                  zn=crosz(xb,yb,zb,xn1,yn1,zn1)
                  rn=1.0/sqrt(xn*xn+yn*yn+zn*zn)
                  xn=xn*rn
                  yn=yn*rn
                  zn=zn*rn
                  dotb3=xb*xd+yb*yd+zb*zd
                  dot3=dotb3/(xd*xd+yd*yd+zd*zd)
                  rb3=1.0/(xb*xb+yb*yb+zb*zb)
                  ql=(1.0-dot3)/(1.0-dot3*dotb3*rb3+epsilonl)
                  xl=0.5*(ql*(xd-dotb3*rb3*xb)+xb)
                  yl=0.5*(ql*(yd-dotb3*rb3*yb)+yb)
                  zl=0.5*(ql*(zd-dotb3*rb3*zb)+zb)
                  ds13=sqrt((xl)**2+(yl)**2+(zl)**2)
                  ds23=sqrt((xl-xb)**2+(yl-yb)**2+(zl-zb)**2)
                  ds33=sqrt((xl-xd)**2+(yl-yd)**2+(zl-zd)**2)
                  xv3=xl+xa
                  yv3=yl+ya
                  zv3=zl+za
                  xa=xic(i1)
                  ya=yic(i1)
                  za=zic(i1)
                  xfac=1.0d+00
                  xb=xfac*(xic(i3)-xa)
                  yb=xfac*(yic(i3)-ya)
                  zb=xfac*(zic(i3)-za)
                  xd=xfac*(xic(i2)-xa)
                  yd=xfac*(yic(i2)-ya)
                  zd=xfac*(zic(i2)-za)
                  xn1=crosx(xb,yb,zb,xd,yd,zd)
                  yn1=crosy(xb,yb,zb,xd,yd,zd)
                  zn1=crosz(xb,yb,zb,xd,yd,zd)
                  xn=crosx(xb,yb,zb,xn1,yn1,zn1)
                  yn=crosy(xb,yb,zb,xn1,yn1,zn1)
                  zn=crosz(xb,yb,zb,xn1,yn1,zn1)
                  rn=1.0/sqrt(xn*xn+yn*yn+zn*zn)
                  xn=xn*rn
                  yn=yn*rn
                  zn=zn*rn
                  dotb3=xb*xd+yb*yd+zb*zd
                  dot3=dotb3/(xd*xd+yd*yd+zd*zd)
                  rb3=1.0/(xb*xb+yb*yb+zb*zb)
                  ql=(1.0-dot3)/(1.0-dot3*dotb3*rb3+epsilonl)
                  xl=0.5*(ql*(xd-dotb3*rb3*xb)+xb)
                  yl=0.5*(ql*(yd-dotb3*rb3*yb)+yb)
                  zl=0.5*(ql*(zd-dotb3*rb3*zb)+zb)
                  ds14=sqrt((xl)**2+(yl)**2+(zl)**2)
                  ds24=sqrt((xl-xb)**2+(yl-yb)**2+(zl-zb)**2)
                  ds34=sqrt((xl-xd)**2+(yl-yd)**2+(zl-zd)**2)
                  xv4=xl+xa
                  yv4=yl+ya
                  zv4=zl+za
                  x1=xic(i1)
                  y1=yic(i1)
                  z1=zic(i1)
                  x2=xic(i2)
                  y2=yic(i2)
                  z2=zic(i2)
                  x3=xic(i3)
                  y3=yic(i3)
                  z3=zic(i3)
                  xl1=xic(i1)
                  yl1=yic(i1)
                  zl1=zic(i1)
                  xl2=xic(i2)
                  yl2=yic(i2)
                  zl2=zic(i2)
                  xl3=xic(i3)
                  yl3=yic(i3)
                  zl3=zic(i3)
                  xl4=xic(i4)
                  yl4=yic(i4)
                  zl4=zic(i4)
                  ax1=  (yl3-yl2)*(zl4-zl2)-(zl3-zl2)*(yl4-yl2)
                  ay1=-((xl3-xl2)*(zl4-zl2)-(zl3-zl2)*(xl4-xl2))
                  az1=  (xl3-xl2)*(yl4-yl2)-(yl3-yl2)*(xl4-xl2)
                  ax2=  (yl4-yl1)*(zl3-zl1)-(zl4-zl1)*(yl3-yl1)
                  ay2=-((xl4-xl1)*(zl3-zl1)-(zl4-zl1)*(xl3-xl1))
                  az2=  (xl4-xl1)*(yl3-yl1)-(yl4-yl1)*(xl3-xl1)
                  ax3=  (yl2-yl1)*(zl4-zl1)-(zl2-zl1)*(yl4-yl1)
                  ay3=-((xl2-xl1)*(zl4-zl1)-(zl2-zl1)*(xl4-xl1))
                  az3=  (xl2-xl1)*(yl4-yl1)-(yl2-yl1)*(xl4-xl1)
                  ax4=  (yl3-yl1)*(zl2-zl1)-(zl3-zl1)*(yl2-yl1)
                  ay4=-((xl3-xl1)*(zl2-zl1)-(zl3-zl1)*(xl2-xl1))
                  az4=  (xl3-xl1)*(yl2-yl1)-(yl3-yl1)*(xl2-xl1)
                  voltet=-((xl4-xl1)*ax4+(yl4-yl1)*ay4+(zl4-zl1)*az4)
                  voltot=voltot+voltet
                  x234=(xl2+xl3+xl4)/3.0
                  y234=(yl2+yl3+yl4)/3.0
                  z234=(zl2+zl3+zl4)/3.0
                  x143=(xl1+xl4+xl3)/3.0
                  y143=(yl1+yl4+yl3)/3.0
                  z143=(zl1+zl4+zl3)/3.0
                  x124=(xl1+xl2+xl4)/3.0
                  y124=(yl1+yl2+yl4)/3.0
                  z124=(zl1+zl2+zl4)/3.0
                  x132=(xl1+xl3+xl2)/3.0
                  y132=(yl1+yl3+yl2)/3.0
                  z132=(zl1+zl3+zl2)/3.0
                  xtestmax=alargenumber
                  xtest=xtestmax
                  xa=xl2
                  ya=yl2
                  za=zl2
                  xb=xl3-xa
                  yb=yl3-ya
                  zb=zl3-za
                  xc=xl4-xa
                  yc=yl4-ya
                  zc=zl4-za
                  xd=xl1-xa
                  yd=yl1-ya
                  zd=zl1-za
                  xn=  yb*zc-yc*zb
                  yn=-(xb*zc-xc*zb)
                  zn=  xb*yc-xc*yb
                  x2=  yn*zb-yb*zn
                  y2=-(xn*zb-xb*zn)
                  z2=  xn*yb-xb*yn
                  q=-0.5*(xc*xb+yc*yb+zc*zb-xc*xc-yc*yc-zc*zc)/
     *                   (x2*xc+y2*yc+z2*zc+1.0e-30)
                  xl=q*x2+0.5*xb
                  yl=q*y2+0.5*yb
                  zl=q*z2+0.5*zb
                  dvor=-0.5*(xd*xd+yd*yd+zd*zd)
                  qvor2=-(xd*xl+yd*yl+zd*zl+dvor)/(xd*xn+yd*yn+zd*zn+
     *                  1.0d-30)
                  xvor=qvor2*xn+xl+xa
                  yvor=qvor2*yn+yl+ya
                  zvor=qvor2*zn+zl+za
                  distsqa=(xvor-xl2)**2+(yvor-yl2)**2+(zvor-zl2)**2
                  distsqb=(xvor-xl3)**2+(yvor-yl3)**2+(zvor-zl3)**2
                  distsqc=(xvor-xl4)**2+(yvor-yl4)**2+(zvor-zl4)**2
                  distsqd=(xvor-xl1)**2+(yvor-yl1)**2+(zvor-zl1)**2
                  if(idumptype.eq.0) then
                     write(iunit,9000) iclr,4,
     *                       xvor,xv3,x12,xv4,
     *                       yvor,yv3,y12,yv4,
     *                       zvor,zv3,z12,zv4
                     write(iunit,9000) iclr,4,
     *                       xvor,xv4,x13,xv2,
     *                       yvor,yv4,y13,yv2,
     *                       zvor,zv4,z13,zv2
                     write(iunit,9000) iclr,4,
     *                       xvor,xv2,x14,xv3,
     *                       yvor,yv2,y14,yv3,
     *                       zvor,zv2,z14,zv3
                     write(iunit,9000) iclr,4,
     *                       xvor,xv1,x23,xv4,
     *                       yvor,yv1,y23,yv4,
     *                       zvor,zv1,z23,zv4
                     write(iunit,9000) iclr,4,
     *                       xvor,xv3,x24,xv1,
     *                       yvor,yv3,y24,yv1,
     *                       zvor,zv3,z24,zv1
                     write(iunit,9000) iclr,4,
     *                       xvor,xv1,x34,xv2,
     *                       yvor,yv1,y34,yv2,
     *                       zvor,zv1,z34,zv2
                  else
                     nverts=1
                     xicpoly(nverts)=safe8to4(xvor)
                     yicpoly(nverts)=safe8to4(yvor)
                     zicpoly(nverts)=safe8to4(zvor)
                     nverts=nverts+1
                     xicpoly(nverts)=safe8to4(xv3)
                     yicpoly(nverts)=safe8to4(yv3)
                     zicpoly(nverts)=safe8to4(zv3)
                     nverts=nverts+1
                     xicpoly(nverts)=safe8to4(x12)
                     yicpoly(nverts)=safe8to4(y12)
                     zicpoly(nverts)=safe8to4(z12)
                     nverts=nverts+1
                     xicpoly(nverts)=safe8to4(xv4)
                     yicpoly(nverts)=safe8to4(yv4)
                     zicpoly(nverts)=safe8to4(zv4)
                     nverts4=isafe4(nverts)
                     call fgmvwritepolygonsdata(nverts4,
     *                                           iclr,
     *                                           xicpoly,
     *                                           yicpoly,
     *                                           zicpoly)
                     nverts=1
                     xicpoly(nverts)=safe8to4(xvor)
                     yicpoly(nverts)=safe8to4(yvor)
                     zicpoly(nverts)=safe8to4(zvor)
                     nverts=nverts+1
                     xicpoly(nverts)=safe8to4(xv4)
                     yicpoly(nverts)=safe8to4(yv4)
                     zicpoly(nverts)=safe8to4(zv4)
                     nverts=nverts+1
                     xicpoly(nverts)=safe8to4(x13)
                     yicpoly(nverts)=safe8to4(y13)
                     zicpoly(nverts)=safe8to4(z13)
                     nverts=nverts+1
                     xicpoly(nverts)=safe8to4(xv2)
                     yicpoly(nverts)=safe8to4(yv2)
                     zicpoly(nverts)=safe8to4(zv2)
                     nverts4=isafe4(nverts)
                     call fgmvwritepolygonsdata(nverts4,
     *                                           iclr,
     *                                           xicpoly,
     *                                           yicpoly,
     *                                           zicpoly)
                     nverts=1
                     xicpoly(nverts)=safe8to4(xvor)
                     yicpoly(nverts)=safe8to4(yvor)
                     zicpoly(nverts)=safe8to4(zvor)
                     nverts=nverts+1
                     xicpoly(nverts)=safe8to4(xv2)
                     yicpoly(nverts)=safe8to4(yv2)
                     zicpoly(nverts)=safe8to4(zv2)
                     nverts=nverts+1
                     xicpoly(nverts)=safe8to4(x14)
                     yicpoly(nverts)=safe8to4(y14)
                     zicpoly(nverts)=safe8to4(z14)
                     nverts=nverts+1
                     xicpoly(nverts)=safe8to4(xv3)
                     yicpoly(nverts)=safe8to4(yv3)
                     zicpoly(nverts)=safe8to4(zv3)
                     nverts4=isafe4(nverts)
                     call fgmvwritepolygonsdata(nverts4,
     *                                           iclr,
     *                                           xicpoly,
     *                                           yicpoly,
     *                                           zicpoly)
                     nverts=1
                     xicpoly(nverts)=safe8to4(xvor)
                     yicpoly(nverts)=safe8to4(yvor)
                     zicpoly(nverts)=safe8to4(zvor)
                     nverts=nverts+1
                     xicpoly(nverts)=safe8to4(xv1)
                     yicpoly(nverts)=safe8to4(yv1)
                     zicpoly(nverts)=safe8to4(zv1)
                     nverts=nverts+1
                     xicpoly(nverts)=safe8to4(x23)
                     yicpoly(nverts)=safe8to4(y23)
                     zicpoly(nverts)=safe8to4(z23)
                     nverts=nverts+1
                     xicpoly(nverts)=safe8to4(xv4)
                     yicpoly(nverts)=safe8to4(yv4)
                     zicpoly(nverts)=safe8to4(zv4)
                     nverts4=isafe4(nverts)
                     call fgmvwritepolygonsdata(nverts4,
     *                                           iclr,
     *                                           xicpoly,
     *                                           yicpoly,
     *                                           zicpoly)
                     nverts=1
                     xicpoly(nverts)=safe8to4(xvor)
                     yicpoly(nverts)=safe8to4(yvor)
                     zicpoly(nverts)=safe8to4(zvor)
                     nverts=nverts+1
                     xicpoly(nverts)=safe8to4(xv3)
                     yicpoly(nverts)=safe8to4(yv3)
                     zicpoly(nverts)=safe8to4(zv3)
                     nverts=nverts+1
                     xicpoly(nverts)=safe8to4(x24)
                     yicpoly(nverts)=safe8to4(y24)
                     zicpoly(nverts)=safe8to4(z24)
                     nverts=nverts+1
                     xicpoly(nverts)=safe8to4(xv1)
                     yicpoly(nverts)=safe8to4(yv1)
                     zicpoly(nverts)=safe8to4(zv1)
                     nverts4=isafe4(nverts)
                     call fgmvwritepolygonsdata(nverts4,
     *                                           iclr,
     *                                           xicpoly,
     *                                           yicpoly,
     *                                           zicpoly)
                     nverts=1
                     xicpoly(nverts)=safe8to4(xvor)
                     yicpoly(nverts)=safe8to4(yvor)
                     zicpoly(nverts)=safe8to4(zvor)
                     nverts=nverts+1
                     xicpoly(nverts)=safe8to4(xv1)
                     yicpoly(nverts)=safe8to4(yv1)
                     zicpoly(nverts)=safe8to4(zv1)
                     nverts=nverts+1
                     xicpoly(nverts)=safe8to4(x34)
                     yicpoly(nverts)=safe8to4(y34)
                     zicpoly(nverts)=safe8to4(z34)
                     nverts=nverts+1
                     xicpoly(nverts)=safe8to4(xv2)
                     yicpoly(nverts)=safe8to4(yv2)
                     zicpoly(nverts)=safe8to4(zv2)
                     nverts4=isafe4(nverts)
                     call fgmvwritepolygonsdata(nverts4,
     *                                           iclr,
     *                                           xicpoly,
     *                                           yicpoly,
     *                                           zicpoly)
                  endif
               elseif(ivoronoi3d.eq.2) then
                  xv1=(xic(i2)+xic(i3)+xic(i4))/3.0
                  yv1=(yic(i2)+yic(i3)+yic(i4))/3.0
                  zv1=(zic(i2)+zic(i3)+zic(i4))/3.0
                  xv2=(xic(i1)+xic(i3)+xic(i4))/3.0
                  yv2=(yic(i1)+yic(i3)+yic(i4))/3.0
                  zv2=(zic(i1)+zic(i3)+zic(i4))/3.0
                  xv3=(xic(i1)+xic(i2)+xic(i4))/3.0
                  yv3=(yic(i1)+yic(i2)+yic(i4))/3.0
                  zv3=(zic(i1)+zic(i2)+zic(i4))/3.0
                  xv4=(xic(i1)+xic(i2)+xic(i3))/3.0
                  yv4=(yic(i1)+yic(i2)+yic(i3))/3.0
                  zv4=(zic(i1)+zic(i2)+zic(i3))/3.0
                  xm=(xic(i1)+xic(i2)+xic(i3)+xic(i4))/4.0
                  ym=(yic(i1)+yic(i2)+yic(i3)+yic(i4))/4.0
                  zm=(zic(i1)+zic(i2)+zic(i3)+zic(i4))/4.0
                  xvor=xm
                  yvor=ym
                  zvor=zm
                  if(idumptype.eq.0) then
                     write(iunit,9000) iclr,4,
     *                       xvor,xv3,x12,xv4,
     *                       yvor,yv3,y12,yv4,
     *                       zvor,zv3,z12,zv4
                     write(iunit,9000) iclr,4,
     *                       xvor,xv4,x13,xv2,
     *                       yvor,yv4,y13,yv2,
     *                       zvor,zv4,z13,zv2
                     write(iunit,9000) iclr,4,
     *                       xvor,xv2,x14,xv3,
     *                       yvor,yv2,y14,yv3,
     *                       zvor,zv2,z14,zv3
                     write(iunit,9000) iclr,4,
     *                       xvor,xv1,x23,xv4,
     *                       yvor,yv1,y23,yv4,
     *                       zvor,zv1,z23,zv4
                     write(iunit,9000) iclr,4,
     *                       xvor,xv3,x24,xv1,
     *                       yvor,yv3,y24,yv1,
     *                       zvor,zv3,z24,zv1
                     write(iunit,9000) iclr,4,
     *                       xvor,xv1,x34,xv2,
     *                       yvor,yv1,y34,yv2,
     *                       zvor,zv1,z34,zv2
                  else
                     nverts=1
                     xicpoly(nverts)=safe8to4(xvor)
                     yicpoly(nverts)=safe8to4(yvor)
                     zicpoly(nverts)=safe8to4(zvor)
                     nverts=nverts+1
                     xicpoly(nverts)=safe8to4(xv3)
                     yicpoly(nverts)=safe8to4(yv3)
                     zicpoly(nverts)=safe8to4(zv3)
                     nverts=nverts+1
                     xicpoly(nverts)=safe8to4(x12)
                     yicpoly(nverts)=safe8to4(y12)
                     zicpoly(nverts)=safe8to4(z12)
                     nverts=nverts+1
                     xicpoly(nverts)=safe8to4(xv4)
                     yicpoly(nverts)=safe8to4(yv4)
                     zicpoly(nverts)=safe8to4(zv4)
                     nverts4=isafe4(nverts)
                     call fgmvwritepolygonsdata(nverts4,
     *                                           iclr,
     *                                           xicpoly,
     *                                           yicpoly,
     *                                           zicpoly)
                     nverts=1
                     xicpoly(nverts)=safe8to4(xvor)
                     yicpoly(nverts)=safe8to4(yvor)
                     zicpoly(nverts)=safe8to4(zvor)
                     nverts=nverts+1
                     xicpoly(nverts)=safe8to4(xv4)
                     yicpoly(nverts)=safe8to4(yv4)
                     zicpoly(nverts)=safe8to4(zv4)
                     nverts=nverts+1
                     xicpoly(nverts)=safe8to4(x13)
                     yicpoly(nverts)=safe8to4(y13)
                     zicpoly(nverts)=safe8to4(z13)
                     nverts=nverts+1
                     xicpoly(nverts)=safe8to4(xv2)
                     yicpoly(nverts)=safe8to4(yv2)
                     zicpoly(nverts)=safe8to4(zv2)
                     nverts4=isafe4(nverts)
                     call fgmvwritepolygonsdata(nverts4,
     *                                           iclr,
     *                                           xicpoly,
     *                                           yicpoly,
     *                                           zicpoly)
                     nverts=1
                     xicpoly(nverts)=safe8to4(xvor)
                     yicpoly(nverts)=safe8to4(yvor)
                     zicpoly(nverts)=safe8to4(zvor)
                     nverts=nverts+1
                     xicpoly(nverts)=safe8to4(xv2)
                     yicpoly(nverts)=safe8to4(yv2)
                     zicpoly(nverts)=safe8to4(zv2)
                     nverts=nverts+1
                     xicpoly(nverts)=safe8to4(x14)
                     yicpoly(nverts)=safe8to4(y14)
                     zicpoly(nverts)=safe8to4(z14)
                     nverts=nverts+1
                     xicpoly(nverts)=safe8to4(xv3)
                     yicpoly(nverts)=safe8to4(yv3)
                     zicpoly(nverts)=safe8to4(zv3)
                     nverts4=isafe4(nverts)
                     call fgmvwritepolygonsdata(nverts4,
     *                                           iclr,
     *                                           xicpoly,
     *                                           yicpoly,
     *                                           zicpoly)
                     nverts=1
                     xicpoly(nverts)=safe8to4(xvor)
                     yicpoly(nverts)=safe8to4(yvor)
                     zicpoly(nverts)=safe8to4(zvor)
                     nverts=nverts+1
                     xicpoly(nverts)=safe8to4(xv1)
                     yicpoly(nverts)=safe8to4(yv1)
                     zicpoly(nverts)=safe8to4(zv1)
                     nverts=nverts+1
                     xicpoly(nverts)=safe8to4(x23)
                     yicpoly(nverts)=safe8to4(y23)
                     zicpoly(nverts)=safe8to4(z23)
                     nverts=nverts+1
                     xicpoly(nverts)=safe8to4(xv4)
                     yicpoly(nverts)=safe8to4(yv4)
                     zicpoly(nverts)=safe8to4(zv4)
                     nverts4=isafe4(nverts)
                     call fgmvwritepolygonsdata(nverts4,
     *                                           iclr,
     *                                           xicpoly,
     *                                           yicpoly,
     *                                           zicpoly)
                     nverts=1
                     xicpoly(nverts)=safe8to4(xvor)
                     yicpoly(nverts)=safe8to4(yvor)
                     zicpoly(nverts)=safe8to4(zvor)
                     nverts=nverts+1
                     xicpoly(nverts)=safe8to4(xv3)
                     yicpoly(nverts)=safe8to4(yv3)
                     zicpoly(nverts)=safe8to4(zv3)
                     nverts=nverts+1
                     xicpoly(nverts)=safe8to4(x24)
                     yicpoly(nverts)=safe8to4(y24)
                     zicpoly(nverts)=safe8to4(z24)
                     nverts=nverts+1
                     xicpoly(nverts)=safe8to4(xv1)
                     yicpoly(nverts)=safe8to4(yv1)
                     zicpoly(nverts)=safe8to4(zv1)
                     nverts4=isafe4(nverts)
                     call fgmvwritepolygonsdata(nverts4,
     *                                           iclr,
     *                                           xicpoly,
     *                                           yicpoly,
     *                                           zicpoly)
                     nverts=1
                     xicpoly(nverts)=safe8to4(xvor)
                     yicpoly(nverts)=safe8to4(yvor)
                     zicpoly(nverts)=safe8to4(zvor)
                     nverts=nverts+1
                     xicpoly(nverts)=safe8to4(xv1)
                     yicpoly(nverts)=safe8to4(yv1)
                     zicpoly(nverts)=safe8to4(zv1)
                     nverts=nverts+1
                     xicpoly(nverts)=safe8to4(x34)
                     yicpoly(nverts)=safe8to4(y34)
                     zicpoly(nverts)=safe8to4(z34)
                     nverts=nverts+1
                     xicpoly(nverts)=safe8to4(xv2)
                     yicpoly(nverts)=safe8to4(yv2)
                     zicpoly(nverts)=safe8to4(zv2)
                     nverts4=isafe4(nverts)
                     call fgmvwritepolygonsdata(nverts4,
     *                                           iclr,
     *                                           xicpoly,
     *                                           yicpoly,
     *                                           zicpoly)
                  endif
 9000             format(2i5,4(1x,1pe22.14e3),/,
     *                       4(1x,1pe22.14e3),/,
     *                       4(1x,1pe22.14e3))
               elseif(ivoronoi3d.eq.3) then
                  do i=1,4
                  if(jtet1(jtoff+i).ge.mbndry) then
                  i1=itet1(itoff+itetface1(1,i))
                  i2=itet1(itoff+itetface1(2,i))
                  i3=itet1(itoff+itetface1(3,i))
                  x1=xic(i1)
                  y1=yic(i1)
                  z1=zic(i1)
                  x2=xic(i2)
                  y2=yic(i2)
                  z2=zic(i2)
                  x3=xic(i3)
                  y3=yic(i3)
                  z3=zic(i3)
                  xm=(xic(i1)+xic(i2)+xic(i3))/3.0
                  ym=(yic(i1)+yic(i2)+yic(i3))/3.0
                  zm=(zic(i1)+zic(i2)+zic(i3))/3.0
                  xv=xm
                  yv=ym
                  zv=zm
                  xa=xic(i1)
                  ya=yic(i1)
                  za=zic(i1)
                  xfac=1.0
                  xb=xfac*(xic(i2)-xa)
                  yb=xfac*(yic(i2)-ya)
                  zb=xfac*(zic(i2)-za)
                  xd=xfac*(xic(i3)-xa)
                  yd=xfac*(yic(i3)-ya)
                  zd=xfac*(zic(i3)-za)
                  xn1=crosx(xb,yb,zb,xd,yd,zd)
                  yn1=crosy(xb,yb,zb,xd,yd,zd)
                  zn1=crosz(xb,yb,zb,xd,yd,zd)
                  xn=crosx(xb,yb,zb,xn1,yn1,zn1)
                  yn=crosy(xb,yb,zb,xn1,yn1,zn1)
                  zn=crosz(xb,yb,zb,xn1,yn1,zn1)
                  rn=1.0/sqrt(xn*xn+yn*yn+zn*zn)
                  xn=xn*rn
                  yn=yn*rn
                  zn=zn*rn
                  dotb3=xb*xd+yb*yd+zb*zd
                  dot3=dotb3/(xd*xd+yd*yd+zd*zd)
                  rb3=1.0/(xb*xb+yb*yb+zb*zb)
                  ql=(1.0-dot3)/(1.0-dot3*dotb3*rb3+epsilonl)
                  xl=0.5*(ql*(xd-dotb3*rb3*xb)+xb)
                  yl=0.5*(ql*(yd-dotb3*rb3*yb)+yb)
                  zl=0.5*(ql*(zd-dotb3*rb3*zb)+zb)
                  ds1=sqrt((xl)**2+(yl)**2+(zl)**2)
                  ds2=sqrt((xl-xb)**2+(yl-yb)**2+(zl-zb)**2)
                  ds3=sqrt((xl-xd)**2+(yl-yd)**2+(zl-zd)**2)
                  xv=xl+xa
                  yv=yl+ya
                  zv=zl+za
                  x12=0.5*(xic(i1)+xic(i2))
                  y12=0.5*(yic(i1)+yic(i2))
                  z12=0.5*(zic(i1)+zic(i2))
                  x13=0.5*(xic(i1)+xic(i3))
                  y13=0.5*(yic(i1)+yic(i3))
                  z13=0.5*(zic(i1)+zic(i3))
                  x23=0.5*(xic(i2)+xic(i3))
                  y23=0.5*(yic(i2)+yic(i3))
                  z23=0.5*(zic(i2)+zic(i3))
C
                  xcen=xv
                  ycen=yv
                  zcen=zv
                  dist=(xcen-x12)**2+(ycen-y12)**2+(zcen-z12)**2
                  if(dist.gt.disttest) then
                     iclrm=maxclrelement+2
                     if(idumptype.eq.0) then
                        write(iunit,9010) iclrm,2,
     *                                    xcen,x12,
     *                                    ycen,y12,
     *                                    zcen,z12
                     else
                        nverts=1
                        xicpoly(nverts)=safe8to4(xcen)
                        yicpoly(nverts)=safe8to4(ycen)
                        zicpoly(nverts)=safe8to4(zcen)
                        nverts=nverts+1
                        xicpoly(nverts)=safe8to4(x12)
                        yicpoly(nverts)=safe8to4(y12)
                        zicpoly(nverts)=safe8to4(z12)
                        nverts4=isafe4(nverts)
                        call fgmvwritepolygonsdata(nverts4,
     *                                              iclrm,
     *                                              xicpoly,
     *                                              yicpoly,
     *                                              zicpoly)
                     endif
                  endif
                  dist=(xcen-x13)**2+(ycen-y13)**2+(zcen-z13)**2
                  if(dist.gt.disttest) then
                     iclrm=maxclrelement+2
                     if(idumptype.eq.0) then
                        write(iunit,9010) iclrm,2,
     *                                    xcen,x13,
     *                                    ycen,y13,
     *                                    zcen,z13
                     else
                        nverts=1
                        xicpoly(nverts)=safe8to4(xcen)
                        yicpoly(nverts)=safe8to4(ycen)
                        zicpoly(nverts)=safe8to4(zcen)
                        nverts=nverts+1
                        xicpoly(nverts)=safe8to4(x13)
                        yicpoly(nverts)=safe8to4(y13)
                        zicpoly(nverts)=safe8to4(z13)
                        nverts4=isafe4(nverts)
                        call fgmvwritepolygonsdata(nverts4,
     *                                              iclrm,
     *                                              xicpoly,
     *                                              yicpoly,
     *                                              zicpoly)
                     endif
                  endif
                  dist=(xcen-x23)**2+(ycen-y23)**2+(zcen-z23)**2
                  if(dist.gt.disttest) then
                     iclrm=maxclrelement+2
                     if(idumptype.eq.0) then
                        write(iunit,9010) iclrm,2,
     *                                    xcen,x23,
     *                                    ycen,y23,
     *                                    zcen,z23
                     else
                        nverts=1
                        xicpoly(nverts)=safe8to4(xcen)
                        yicpoly(nverts)=safe8to4(ycen)
                        zicpoly(nverts)=safe8to4(zcen)
                        nverts=nverts+1
                        xicpoly(nverts)=safe8to4(x23)
                        yicpoly(nverts)=safe8to4(y23)
                        zicpoly(nverts)=safe8to4(z23)
                        nverts4=isafe4(nverts)
                        call fgmvwritepolygonsdata(nverts4,
     *                                              iclrm,
     *                                              xicpoly,
     *                                              yicpoly,
     *                                              zicpoly)
                     endif
                  endif
                  xcen=xm
                  ycen=ym
                  zcen=zm
                  dist=(xcen-x12)**2+(ycen-y12)**2+(zcen-z12)**2
                  if(dist.gt.disttest) then
                     iclrm=maxclrelement+3
                     if(idumptype.eq.0) then
                        write(iunit,9010) iclrm,2,
     *                                    xcen,x12,
     *                                    ycen,y12,
     *                                    zcen,z12
                     else
                        nverts=1
                        xicpoly(nverts)=safe8to4(xcen)
                        yicpoly(nverts)=safe8to4(ycen)
                        zicpoly(nverts)=safe8to4(zcen)
                        nverts=nverts+1
                        xicpoly(nverts)=safe8to4(x12)
                        yicpoly(nverts)=safe8to4(y12)
                        zicpoly(nverts)=safe8to4(z12)
                        nverts4=isafe4(nverts)
                        call fgmvwritepolygonsdata(nverts4,
     *                                              iclrm,
     *                                              xicpoly,
     *                                              yicpoly,
     *                                              zicpoly)
                     endif
                  endif
                  dist=(xcen-x13)**2+(ycen-y13)**2+(zcen-z13)**2
                  if(dist.gt.disttest) then
                     iclrm=maxclrelement+3
                     if(idumptype.eq.0) then
                        write(iunit,9010) iclrm,2,
     *                                    xcen,x13,
     *                                    ycen,y13,
     *                                    zcen,z13
                     else
                        nverts=1
                        xicpoly(nverts)=safe8to4(xcen)
                        yicpoly(nverts)=safe8to4(ycen)
                        zicpoly(nverts)=safe8to4(zcen)
                        nverts=nverts+1
                        xicpoly(nverts)=safe8to4(x13)
                        yicpoly(nverts)=safe8to4(y13)
                        zicpoly(nverts)=safe8to4(z13)
                        nverts4=isafe4(nverts)
                        call fgmvwritepolygonsdata(nverts4,
     *                                              iclrm,
     *                                              xicpoly,
     *                                              yicpoly,
     *                                              zicpoly)
                     endif
                  endif
                  dist=(xcen-x23)**2+(ycen-y23)**2+(zcen-z23)**2
                  if(dist.gt.disttest) then
                     iclrm=maxclrelement+3
                     if(idumptype.eq.0) then
                        write(iunit,9010) iclrm,2,
     *                                    xcen,x23,
     *                                    ycen,y23,
     *                                    zcen,z23
                     else
                        nverts=1
                        xicpoly(nverts)=safe8to4(xcen)
                        yicpoly(nverts)=safe8to4(ycen)
                        zicpoly(nverts)=safe8to4(zcen)
                        nverts=nverts+1
                        xicpoly(nverts)=safe8to4(x23)
                        yicpoly(nverts)=safe8to4(y23)
                        zicpoly(nverts)=safe8to4(z23)
                        nverts4=isafe4(nverts)
                        call fgmvwritepolygonsdata(nverts4,
     *                                              iclrm,
     *                                              xicpoly,
     *                                              yicpoly,
     *                                              zicpoly)
                     endif
                  endif
                  endif
                  enddo
               endif
            endif
         elseif(itettyp(ie).eq.ifelmhex) then
            if(nelements.gt.0.and.ie.eq.1) then
               if(idumptype.eq.0) then
                  write(iunit,"('material   ',2i10)")
     *                  maxclrelement+3,imatcolor
               else
                  nmats=maxclrelement+3
                  call fgmvwritematerialheader(nmats, imatcolor)
               endif
               do i=1,maxclrelement
                 if(nmregs.ge.i) then
                   iprtname=cmregs(i)
                  else
                    write(iprtname,"('mat',i5.5)") i
                  endif
 
                  if(ierror.eq.0) then
                     if(idumptype.eq.0) then
                        write(iunit,'(a8)') iprtname
                     else
                        cgmvtype=iprtname
                        call fgmvwritematerialname(cgmvtype)
                     endif
                  else
                     if(idumptype.eq.0) then
                        write(iunit,'(a3,i5.5)') 'mat',i
                     else
                        write(cgmvtype,'(a3,i5.5)') 'mat',i
                        call fgmvwritematerialname(cgmvtype)
                     endif
                  endif
               enddo
               if(idumptype.eq.0) then
                  write(iunit,'(a5)') 'ERROR'
                  write(iunit,'(a7)') 'voronoi'
                  write(iunit,'(a6)') 'median'
               else
                  write(cgmvtype,'(a5)') 'ERROR'
                  call fgmvwritematerialname(cgmvtype)
                  write(cgmvtype,'(a7)') 'voronoi'
                  call fgmvwritematerialname(cgmvtype)
                  write(cgmvtype,'(a6)') 'median'
                  call fgmvwritematerialname(cgmvtype)
               endif
               if(idumptype.eq.0) then
                  write(iunit,"(20i5)") (itemp(it),it=1,nmatcolor)
               else
                  call fgmvwritematerialids(itemp, imatcolor)
               endif
               if(idumptype.eq.0) then
                  write(iunit,"('polygons')")
               else
                  call fgmvwritepolygonsheader()
               endif
            endif
            it=ie
            nef=nelmnef(ifelmhex)
            iflagkid=0
            if(icskid.eq.0) then
               if(itetkid(it).ne.0) iflagkid=1
            endif
            if(iflagkid.eq.0) then
               do i=1,nef
                  iflag=0
                  if(jtet1(jtoff+i).le.0.or.
     *               jtet1(jtoff+i).eq.mbndry) then
                     iflag=1
                  elseif(jtet1(jtoff+i).gt.mbndry) then
                     jt=1+(jtet1(jtoff+i)-mbndry-1)/nef_cmo
                     jf=jtet1(jtoff+i)-mbndry-nef_cmo*(jt-1)
                     if(itetclr(it).ne.itetclr(jt)) iflag=2
                  else
                     jt=1+(jtet1(jtoff+i)-1)/nef_cmo
                     jf=jtet1(jtoff+i)-nef_cmo*(jt-1)
                     if(itetclr(it).ne.itetclr(jt)) iflag=2
                  endif
                  if(iflag.ne.0) then
                     j1=itet1(itoff+ihexface1(1,i))
                     j2=itet1(itoff+ihexface1(2,i))
                     j3=itet1(itoff+ihexface1(3,i))
                     j4=itet1(itoff+ihexface1(4,i))
                     if(itetclr(it).le.0) then
                        iclr=maxclrelement+1
                     elseif(itetclr(it).gt.maxclrelement) then
                        iclr=maxclrelement+1
                     else
                        iclr=itetclr(it)
                     endif
                     if(idumptype.eq.0) then
                        write(iunit,9000)
     *                     iclr,
     *                     4,
     *                     xic(j1),xic(j2),xic(j3),xic(j4),
     *                     yic(j1),yic(j2),yic(j3),yic(j4),
     *                     zic(j1),zic(j2),zic(j3),zic(j4)
                     else
                        nverts=1
                        xicpoly(nverts)=safe8to4(xic(j1))
                        yicpoly(nverts)=safe8to4(yic(j1))
                        zicpoly(nverts)=safe8to4(zic(j1))
                        nverts=nverts+1
                        xicpoly(nverts)=safe8to4(xic(j2))
                        yicpoly(nverts)=safe8to4(yic(j2))
                        zicpoly(nverts)=safe8to4(zic(j2))
                        nverts=nverts+1
                        xicpoly(nverts)=safe8to4(xic(j3))
                        yicpoly(nverts)=safe8to4(yic(j3))
                        zicpoly(nverts)=safe8to4(zic(j3))
                        nverts=nverts+1
                        xicpoly(nverts)=safe8to4(xic(j4))
                        yicpoly(nverts)=safe8to4(yic(j4))
                        zicpoly(nverts)=safe8to4(zic(j4))
                        nverts4=isafe4(nverts)
                        call fgmvwritepolygonsdata(nverts4,
     *                                              iclr,
     *                                              xicpoly,
     *                                              yicpoly,
     *                                              zicpoly)
                     endif
                  endif
               enddo
            endif
         elseif(itettyp(ie).eq.ifelmpyr) then
            if(nelements.gt.0.and.ie.eq.1) then
               if(idumptype.eq.0) then
                  write(iunit,"('material   ',2i10)")
     *                  maxclrelement+3,imatcolor
               else
                  nmats=maxclrelement+3
                  call fgmvwritematerialheader(nmats, imatcolor)
               endif
               do i=1,maxclrelement
                  if(nmregs.ge.i) then
                   iprtname=cmregs(i)
                  else
                    write(iprtname,"('mat',i5.5)") i
                  endif
                  if(ierror.eq.0) then
                     if(idumptype.eq.0) then
                        write(iunit,'(a8)') iprtname
                     else
                        cgmvtype=iprtname
                        call fgmvwritematerialname(cgmvtype)
                     endif
                  else
                     if(idumptype.eq.0) then
                        write(iunit,'(a3,i5.5)') 'mat',i
                     else
                        write(cgmvtype,'(a3,i5.5)') 'mat',i
                        call fgmvwritematerialname(cgmvtype)
                     endif
                  endif
               enddo
               if(idumptype.eq.0) then
                  write(iunit,'(a5)') 'ERROR'
                  write(iunit,'(a7)') 'voronoi'
                  write(iunit,'(a6)') 'median'
               else
                  write(cgmvtype,'(a5)') 'ERROR'
                  call fgmvwritematerialname(cgmvtype)
                  write(cgmvtype,'(a7)') 'voronoi'
                  call fgmvwritematerialname(cgmvtype)
                  write(cgmvtype,'(a6)') 'median'
                  call fgmvwritematerialname(cgmvtype)
               endif
               if(idumptype.eq.0) then
                  write(iunit,"(20i5)") (itemp(it),it=1,nmatcolor)
               else
                  call fgmvwritematerialids(itemp, imatcolor)
               endif
               if(idumptype.eq.0) then
                  write(iunit,"('polygons')")
               else
                  call fgmvwritepolygonsheader()
               endif
            endif
            it=ie
            ity=ifelmpyr
            nef=nelmnef(ifelmpyr)
            do i=1,nef
               iflag=0
               if(jtet1(jtoff+i).le.0.or.
     *            jtet1(jtoff+i).eq.mbndry) then
                  iflag=1
               elseif(jtet1(jtoff+i).gt.mbndry) then
                  jt=1+(jtet1(jtoff+i)-mbndry-1)/nef_cmo
                  jf=jtet1(jtoff+i)-mbndry-nef_cmo*(jt-1)
                  if(itetclr(it).ne.itetclr(jt)) iflag=2
               else
                  jt=1+(jtet1(jtoff+i)-1)/nef_cmo
                  jf=jtet1(jtoff+i)-nef_cmo*(jt-1)
                  if(itetclr(it).ne.itetclr(jt)) iflag=2
               endif
               if(iflag.ne.0) then
                  n=ielmface0(i,ity)
                  do j=1,n
                     jpt(j)=itet1(itoff+ielmface1(j,i,ity))
                  enddo
                  if(itetclr(it).le.0) then
                     iclr=maxclrelement+1
                  elseif(itetclr(it).gt.maxclrelement) then
                     iclr=maxclrelement+1
                  else
                     iclr=itetclr(it)
                  endif
                  if(idumptype.eq.0) then
                     write(iunit,9000)
     *                  iclr,
     *                  n,
     *                  (xic(jpt(k)),k=1,n),
     *                  (yic(jpt(k)),k=1,n),
     *                  (zic(jpt(k)),k=1,n)
                  else
                     nverts=n
                     do k=1,n
                        xicpoly(k)=safe8to4(xic(jpt(k)))
                        yicpoly(k)=safe8to4(yic(jpt(k)))
                        zicpoly(k)=safe8to4(zic(jpt(k)))
                     enddo
                     nverts4=isafe4(nverts)
                     call fgmvwritepolygonsdata(nverts4,
     *                                           iclr,
     *                                           xicpoly,
     *                                           yicpoly,
     *                                           zicpoly)
                  endif
                 endif
              enddo
         elseif(itettyp(ie).eq.ifelmpri) then
            if(nelements.gt.0.and.ie.eq.1) then
               if(idumptype.eq.0) then
                  write(iunit,"('material   ',2i10)")
     *                  maxclrelement+3,imatcolor
               else
                  nmats=maxclrelement+3
                  call fgmvwritematerialheader(nmats, imatcolor)
               endif
               do i=1,maxclrelement
 
                  if(nmregs.ge.i) then
                   iprtname=cmregs(i)
                  else
                    write(iprtname,"('mat',i5.5)") i
                  endif
                  if(ierror.eq.0) then
                     if(idumptype.eq.0) then
                        write(iunit,'(a8)') iprtname
                     else
                        cgmvtype=iprtname
                        call fgmvwritematerialname(cgmvtype)
                     endif
                  else
                     if(idumptype.eq.0) then
                        write(iunit,'(a3,i5.5)') 'mat',i
                     else
                        write(cgmvtype,'(a3,i5.5)') 'mat',i
                        call fgmvwritematerialname(cgmvtype)
                     endif
                  endif
               enddo
               if(idumptype.eq.0) then
                  write(iunit,'(a5)') 'ERROR'
                  write(iunit,'(a7)') 'voronoi'
                  write(iunit,'(a6)') 'median'
               else
                  write(cgmvtype,'(a5)') 'ERROR'
                  call fgmvwritematerialname(cgmvtype)
                  write(cgmvtype,'(a7)') 'voronoi'
                  call fgmvwritematerialname(cgmvtype)
                  write(cgmvtype,'(a6)') 'median'
                  call fgmvwritematerialname(cgmvtype)
               endif
               if(idumptype.eq.0) then
                  write(iunit,"(20i5)") (itemp(it),it=1,nmatcolor)
               else
                  call fgmvwritematerialids(itemp, imatcolor)
               endif
               if(idumptype.eq.0) then
                  write(iunit,"('polygons')")
               else
                  call fgmvwritepolygonsheader()
               endif
            endif
            it=ie
            ity=ifelmpri
            nef=nelmnef(ifelmpri)
            do i=1,nef
               iflag=0
               if(jtet1(jtoff+i).le.0.or.
     *            jtet1(jtoff+i).eq.mbndry) then
                  iflag=1
               elseif(jtet1(jtoff+i).gt.mbndry) then
                  jt=1+(jtet1(jtoff+i)-mbndry-1)/nef_cmo
                  jf=jtet1(jtoff+i)-mbndry-nef_cmo*(jt-1)
                  if(itetclr(it).ne.itetclr(jt)) iflag=2
               else
                  jt=1+(jtet1(jtoff+i)-1)/nef_cmo
                  jf=jtet1(jtoff+i)-nef_cmo*(jt-1)
                  if(itetclr(it).ne.itetclr(jt)) iflag=2
               endif
               if(iflag.ne.0) then
                  n=ielmface0(i,ity)
                  do j=1,n
                     jpt(j)=itet1(itoff+ielmface1(j,i,ity))
                  enddo
                  if(itetclr(it).le.0) then
                     iclr=maxclrelement+1
                  elseif(itetclr(it).gt.maxclrelement) then
                     iclr=maxclrelement+1
                  else
                     iclr=itetclr(it)
                  endif
                  if(idumptype.eq.0) then
                     write(iunit,9000)
     *                  iclr,
     *                  n,
     *                  (xic(jpt(k)),k=1,n),
     *                  (yic(jpt(k)),k=1,n),
     *                  (zic(jpt(k)),k=1,n)
                  else
                     nverts=n
                     do k=1,n
                        xicpoly(k)=safe8to4(xic(jpt(k)))
                        yicpoly(k)=safe8to4(yic(jpt(k)))
                        zicpoly(k)=safe8to4(zic(jpt(k)))
                     enddo
                     nverts4=isafe4(nverts)
                     call fgmvwritepolygonsdata(nverts4,
     *                                           iclr,
     *                                           xicpoly,
     *                                           yicpoly,
     *                                           zicpoly)
                  endif
                 endif
              enddo
         else ! if (itettyp(ie).eq.ifelmhyb.or.itettyp(ie).eq.ifelmply) then
c hope same as pyr,pri works for hybrid: need re "polygons" being written
            if(nelements.gt.0.and.ie.eq.1) then
               if(idumptype.eq.0) then
                  write(iunit,"('material   ',2i10)")
     *                  maxclrelement+3,imatcolor
               else
                  nmats=maxclrelement+3
                  call fgmvwritematerialheader(nmats, imatcolor)
               endif
               do i=1,maxclrelement
                  if(nmregs.ge.i) then
                   iprtname=cmregs(i)
                  else
                     write(iprtname,"('mat',i5.5)") i
                  endif
                  if(ierror.eq.0) then
                     if(idumptype.eq.0) then
                        write(iunit,'(a8)') iprtname
                     else
                        cgmvtype=iprtname
                        call fgmvwritematerialname(cgmvtype)
                     endif
                  else
                     if(idumptype.eq.0) then
                        write(iunit,'(a3,i5.5)') 'mat',i
                     else
                        write(cgmvtype,'(a3,i5.5)') 'mat',i
                        call fgmvwritematerialname(cgmvtype)
                     endif
                  endif
               enddo
               if(idumptype.eq.0) then
                  write(iunit,'(a5)') 'ERROR'
                  write(iunit,'(a7)') 'voronoi'
                  write(iunit,'(a6)') 'median'
               else
                  write(cgmvtype,'(a5)') 'ERROR'
                  call fgmvwritematerialname(cgmvtype)
                  write(cgmvtype,'(a7)') 'voronoi'
                  call fgmvwritematerialname(cgmvtype)
                  write(cgmvtype,'(a6)') 'median'
                  call fgmvwritematerialname(cgmvtype)
               endif
               if(idumptype.eq.0) then
                  write(iunit,"(20i5)") (itemp(it),it=1,nmatcolor)
               else
                  call fgmvwritematerialids(itemp, imatcolor)
               endif
               if(idumptype.eq.0) then
                  write(iunit,"('polygons')")
               else
                  call fgmvwritepolygonsheader()
               endif
            endif
            it=ie
            ity=itettyp(ie)
            nef=nelmnef(ity)
            do i=1,nef
               iflag=0
               if(jtet1(jtoff+i).le.0.or.
     *            jtet1(jtoff+i).eq.mbndry) then
                  iflag=1
               elseif(jtet1(jtoff+i).gt.mbndry) then
                  jt=1+(jtet1(jtoff+i)-mbndry-1)/nef_cmo
                  jf=jtet1(jtoff+i)-mbndry-nef_cmo*(jt-1)
                  if(itetclr(it).ne.itetclr(jt)) iflag=2
               else
                  jt=1+(jtet1(jtoff+i)-1)/nef_cmo
                  jf=jtet1(jtoff+i)-nef_cmo*(jt-1)
                  if(itetclr(it).ne.itetclr(jt)) iflag=2
               endif
               if(iflag.ne.0) then
                  n=ielmface0(i,ity)
                  if (n.le.0) then
                     n=1
                     jpt(j)=itet1(itoff+1)
                  else
                     do j=1,n
                        jpt(j)=itet1(itoff+ielmface1(j,i,ity))
                     enddo
                  endif
                  if(itetclr(it).le.0) then
                     iclr=maxclrelement+1
                  elseif(itetclr(it).gt.maxclrelement) then
                     iclr=maxclrelement+1
                  else
                     iclr=itetclr(it)
                  endif
                  if(idumptype.eq.0) then
                     write(iunit,9000)
     *                  iclr,
     *                  n,
     *                  (xic(jpt(k)),k=1,n),
     *                  (yic(jpt(k)),k=1,n),
     *                  (zic(jpt(k)),k=1,n)
                  else
                     nverts=n
                     do k=1,n
                        xicpoly(k)=safe8to4(xic(jpt(k)))
                        yicpoly(k)=safe8to4(yic(jpt(k)))
                        zicpoly(k)=safe8to4(zic(jpt(k)))
                     enddo
                     nverts4=isafe4(nverts)
                     call fgmvwritepolygonsdata(nverts4,
     *                                           iclr,
     *                                           xicpoly,
     *                                           yicpoly,
     *                                           zicpoly)
                  endif
                 endif
              enddo
         endif
         enddo
         if(nelements.gt.0) then
            if(idumptype.eq.0) then
               write(iunit,"('endpoly')")
            else
               call fgmvwritepolygonsendpoly()
            endif
         endif
      endif
 
      call get_global('ihcycle',ihcycle,rout,cout,itype,icscode)
      if (icscode .eq. 0) then
         if(idumptype.eq.0) then
            write(iunit,"(a8,i10)") 'cycleno ',ihcycle
         else
            call fgmvwritecycleno(ihcycle)
         endif
      endif
 
      call get_global('time',iout,time,cout,itype,icscode)
      if (icscode .eq. 0) then
         if(idumptype.eq.0) then
            write(iunit,"(a9,1pe14.5e3)") 'probtime ',time
         else
            probtime=time
            call fgmvwriteprobtime(probtime)
         endif
      endif

      if(idumptype.eq.0) then
         write(iunit,"('endgmv')")
      endif

      goto 9999
 9999 continue

C     Need to add messages for error flags if set

C     try to close file even if errors only if opened
C     seg fault if trying to close file before it is open

      if(idumptype.eq.0) then
         close(iunit)
      else
         call fgmvwriteclosefile()
      endif
      call mmrelprt(isubname,icscode)

C     early exit before file is opened
 9995 if (ierror .ne. 0) then
        write(logmess,'(a,a)') 
     *  "DUMP/GMV ERROR: could not open file for: ",
     *   cmo(1:icharlnf(cmo))
        call writloga('default',0,logmess,1,ics)
      else
        write(logmess,'(a,a)') 
     *  "DUMP/GMV Done writing file for: ",
     *   cmo(1:icharlnf(cmo))
      endif
      
      return
      end

C##################################################
      subroutine fdate_2_mmddyy(string8,string)
      character*24 string
      character*8 string8
      character*2 mm, dd, yy
      integer int_string

C     remove stop command
C     write 00 where date does not function properly


C     linux with gfortran 4.5 string appears as
C      123456789012345678901234
C      Wed Dec 22 12:38:47 2010

C
C     Convert month name to month number
C      
      if    (string(5:7) .eq. 'Jan')then
         mm = '01'
      elseif(string(5:7) .eq. 'Feb')then
         mm = '02'
      elseif(string(5:7) .eq. 'Mar')then
         mm = '03'
      elseif(string(5:7) .eq. 'Apr')then
         mm = '04'
      elseif(string(5:7) .eq. 'May')then
         mm = '05'
      elseif(string(5:7) .eq. 'Jun')then
         mm = '06'
      elseif(string(5:7) .eq. 'Jul')then
         mm = '07'
      elseif(string(5:7) .eq. 'Aug')then
         mm = '08'
      elseif(string(5:7) .eq. 'Sep')then
         mm = '09'
      elseif(string(5:7) .eq. 'Oct')then
         mm = '10'
      elseif(string(5:7) .eq. 'Nov')then
         mm = '11'
      elseif(string(5:7) .eq. 'Dec')then
         mm = '12'
      else
         print *, 'ERROR: fdate_2_mmddyy'
         print *, 'Invalid month: ',string(5:7)
         mm = '00'
        print *, 'ERROR: Using 00'
      endif
C
C     Convert day of the month to dd
C
      read(string(9:10),100)int_string
  100 format(i2)
      if    ((int_string .ge. 1).and.
     1       (int_string .le. 9))then
         write(dd(1:1),110)0
         write(dd(2:2),110)int_string
  110    format(i1)
      elseif((int_string .ge. 10).and.
     1       (int_string .le. 31))then
         write(dd,100)int_string
  120    format(i2)
      else
C
C       ERROR
C
        print *, 'ERROR: fdate_2_mmddyy'
        print *, 'Invalid day, dd'
        print *, string(9:10)
        print *, 'ERROR: Using 00'
        write(dd(1:2),100)"00"
      endif
C
C     Convert year (char 21,22,23,24) to yy
C
      read(string(23:24),100)int_string
      if    ((int_string .ge. 1).and.
     1       (int_string .le. 9))then
         write(yy(1:1),110)0
         write(yy(2:2),110)int_string
      elseif((int_string .ge. 10).and.
     1       (int_string .le. 99))then
         write(yy,100)int_string
      endif

C
C     Put mm/dd/yy together
C
      write(string8,200)mm,dd,yy
  200 format(a2,'/',a2,'/',a2)

C      print*,"mm,dd,yy = ",mm,dd,yy
C      print*,"string8  = ",string8

      return
      end
