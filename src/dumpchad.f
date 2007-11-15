*dk,dumpchad
      subroutine dumpchad(ifile,ierror)
C
C #####################################################################
C
C     PURPOSE -
C
C        THIS ROUTINE WRITES A DUMP FILE FOR CHAD.
C
C     INPUT ARGUMENTS -
C
C        ifile - CHARACTER FILE NAME.
C
C     OUTPUT ARGUMENTS -
C
C        ierror - ERROR FLAG (=0 ==> OK, <>0 ==> ERROR).
C
C     CHANGE HISTORY -
C
C        $Log: dumpchad.f,v $
C        Revision 2.00  2007/11/05 19:45:53  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.1   Wed Apr 05 13:48:56 2000   dcg
CPVCS    fix call to cmo_get_attinfo
CPVCS
CPVCS       Rev 1.0   Tue Feb 08 15:55:28 2000   dcg
CPVCS    Initial revision.
CPVCS
CPVCS       Rev 1.16   Wed Apr 07 14:40:14 1999   dcg
CPVCS    remove duplicate comman in calling sequence
CPVCS
CPVCS       Rev 1.15   Fri Jan 22 11:52:22 1999   dcg
CPVCS    remove unused declarations
CPVCS
CPVCS       Rev 1.14   Mon Apr 14 16:43:52 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.13   Mon Nov 11 20:58:24 1996   het
CPVCS    Make Manjits most recent changes.
CPVCS
CPVCS       Rev 1.12   Mon Jul 22 16:16:16 1996   dcg
CPVCS    add code to handle pyramid and prism elements
CPVCS
CPVCS       Rev 1.11   Tue Mar 05 12:36:44 1996   het
CPVCS    Correct an error with the contab array
CPVCS
CPVCS       Rev 1.10   Wed Feb 28 10:10:28 1996   het
CPVCS    Correct errors
CPVCS
CPVCS       Rev 1.9   Fri Feb 16 21:48:14 1996   het
CPVCS    Add the itp1_chad and xcontab for cylinders.
CPVCS
CPVCS       Rev 1.8   Wed Jan 03 10:08:18 1996   het
CPVCS    Change maximum dimensions for UNICOS.
CPVCS
CPVCS       Rev 1.7   11/07/95 17:16:14   dcg
CPVCS    change flag to 2 in mmgetblk calls
CPVCS
CPVCS       Rev 1.6   10/16/95 10:25:48   het
CPVCS    Correct a point type error.
CPVCS
CPVCS       Rev 1.5   09/11/95 14:40:36   het
CPVCS    Change to the storage block based CMO stuff.
CPVCS
CPVCS       Rev 1.4   08/29/95 12:16:08   het
CPVCS    Add the cmowrk storage block for each CMO
CPVCS
CPVCS       Rev 1.3   08/15/95 18:22:34   het
CPVCS    Cleanup code and correct errors
CPVCS
CPVCS       Rev 1.2   06/07/95 06:21:34   het
CPVCS    Add more local_element.h references
CPVCS
CPVCS       Rev 1.1   02/16/95 07:35:28   het
CPVCS    Correct format errors in dump and read commands
CPVCS
CPVCS       Rev 1.0   01/17/95 16:36:00   pvcs
CPVCS    Original Version
C
C ######################################################################
C
      implicit real*8 (a-h,o-z)
C
C ######################################################################
C
      include 'chydro.h'
      include 'local_element.h'
C
      pointer (ipisetwd, isetwd)
      pointer (ipimt1, imt1)
      pointer (ipitp1, itp1)
      pointer (ipicr1, icr1)
      pointer (ipisn1, isn1)
      pointer (ipign1, ign1)
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      pointer (ipitet, itet)
      pointer (ipitet, itet1)
      pointer (ipjtet, jtet)
      pointer (ipjtet, jtet1)
      integer isetwd(1000000)
      integer imt1(1000000), itp1(1000000),
     *        icr1(1000000), isn1(1000000),
     *        ign1(1000000),
     *        itet(4,1000000), jtet(4,1000000)
      integer itet1(4*1000000), jtet1(4*1000000)
      dimension   xic(1000000), yic(1000000), zic(1000000)
      pointer (ipitetclr, itetclr)
      pointer (ipitettyp, itettyp)
      pointer (ipitetoff, itetoff)
      pointer (ipjtetoff, jtetoff)
      integer itetclr(1000000), itettyp(1000000),
     *        itetoff(1000000), jtetoff(1000000)
C
C ######################################################################
C
      character ifile*(*)
C
      pointer (ipitp2, itp2)
      integer itp2(1000000)
C
      pointer (ipiparent, iparent)
      integer  iparent(1000000)
C
      pointer (ipitp1_chad, itp1_chad)
      integer itp1_chad(1000000)
      pointer(ipvels,vels)
      real*8 vels(3,*)
C
      integer  nen, nef
C
      parameter (nentri=3, nfacetri=3)
      parameter (nentet=4, nfacetet=4)
      parameter (nenprism=6, nfaceprism=5)
      parameter (nenhex=8, nfacehex=6)
C
      dimension ihexface0(nfacehex), ihexface1(4,nfacehex)
C     top,bottom,front,right,back,left
      data ihexface0 / 4, 4, 4, 4, 4, 4 /
      data ihexface1 / 1, 2, 3, 4,
     *                 5, 8, 7, 6,
     *                 1, 5, 6, 2,
     *                 2, 6, 7, 3,
     *                 3, 7, 8, 4,
     *                 1, 4, 8, 5 /
      dimension iprismface0(nfaceprism), iprismface1(4,nfaceprism)
C     top,bottom,right,back,left
      data iprismface0 / 3, 3, 4, 4, 4 /
      data iprismface1 / 1, 2, 3, 0,
     *                   4, 6, 5, 0,
     *                   1, 4, 5, 2,
     *                   2, 5, 6, 3,
     *                   1, 3, 6, 4 /
C
      integer intpairhex(2,12)
      data intpairhex / 1,2, 2,3, 3,4, 4,1, 5,6, 6,7, 7,8,
     *                  8,5, 1,5, 2,6, 3,7, 4,8 /
 
      dimension itetface0(nfacetet), itetface1(4,nfacetet)
C     top,back,left,right
      data itetface0 / 3, 3, 3, 3 /
      data itetface1 / 2, 3, 4, 0,
     *                 1, 4, 3, 0,
     *                 1, 2, 4, 0,
     *                 1, 3, 2, 0 /
      dimension itriface0(nfacetri), itriface1(3,nfacetri)
C     top,back,left,right
      data itriface0 / 2, 2, 2 /
      data itriface1 / 2, 3, 1,
     *                 3, 1, 2,
     *                 1, 2, 3 /
      integer intpairtet(2,6)
      data intpairtet / 1,2, 1,3, 1,4, 2,3, 2,4, 3,4 /
C
C
       real*8       xid(3,3),xc(3)
      dimension xcontab1(3,3)
C
      character*32 isubname, cmo,  cvelnm
      character*8 sbname,defname
      logical ifvels
C
      data ivoronoi2d / 1 /
      data ivoronoi3d / 0 /
      data iflag/0/
C
C ######################################################################
C
C
      crosx(a,b,c,d,e,f)=b*f-c*e
      crosy(a,b,c,d,e,f)=c*d-a*f
      crosz(a,b,c,d,e,f)=a*e-b*d
C
C ######################################################################
C
C*****cmohex='cmo1'
C*****cmotet5='cmo2'
C*****call seticr( cmohex,cmohex,cmotet5)
C*****call seticr(cmotet5,cmohex,cmotet5)
C
C     ******************************************************************
C
C     DEFINE IDENTITY OPERATOR.
C
      do j = 1,3
         do i = 1,3
            if(i.eq.j) then
               xid(i,j) = 1.0
            else
               xid(i,j) = 0.0
            endif
         enddo
      enddo
C
      isubname="dumpchad"
      sbname='sbcmoprm'
      defname='default'
C
      call cmo_get_name(cmo,ierror)
C
      call cmo_get_info('nnodes',cmo,nnodes,lencmo,itpcmo,ierror)
      call cmo_get_info('nelements',cmo,nelements,lencmo,itpcmo,ierror)
      call cmo_get_info('mbndry',cmo,mbndry,length,icmotype,ierror)
      call cmo_get_info('ndimensions_topo',cmo,
     *                  nsdtopo,length,icmotype,ierror)
      call cmo_get_info('ndimensions_geom',cmo,
     *                  nsdgeom,length,icmotype,ierror)
      call cmo_get_info('nodes_per_element',cmo,
     *                  nen,length,icmotype,ierror)
      call cmo_get_info('faces_per_element',cmo,
     *                  nef,length,icmotype,ierror)
      call cmo_get_info('isetwd',cmo,ipisetwd,lenisetwd,icmotype,ier)
      call cmo_get_info('imt1',cmo,ipimt1,lenimt1,icmotype,ierror)
      call cmo_get_info('itp1',cmo,ipitp1,lenitp1,icmotype,ierror)
      call cmo_get_info('icr1',cmo,ipicr1,lenicr1,icmotype,ierror)
      call cmo_get_info('isn1',cmo,ipisn1,lenisn1,icmotype,ierror)
      call cmo_get_info('ign1',cmo,ipign1,lenign1,icmotype,ierror)
      call cmo_get_info('xic',cmo,ipxic,lenxic,icmotype,ierror)
      call cmo_get_info('yic',cmo,ipyic,lenyic,icmotype,ierror)
      call cmo_get_info('zic',cmo,ipzic,lenzic,icmotype,ierror)
      call cmo_get_attinfo('velname',cmo,iout,rout,cvelnm,ipout,ilen,
     *     ityp,ierror)
      call cmo_get_info(cvelnm,cmo,ipvel,lenc,icmotype,ierror)
      if(ierror.eq.0) then
         ifvels=.true.
      call cmo_get_info('vels',cmo,ipvels,len,icmotype,ierror)
      else
         ifvels=.false.
      endif
      call cmo_get_info('uic',cmo,ipuic,lenuic,icmotype,ierror)
      call cmo_get_info('vic',cmo,ipvic,lenvic,icmotype,ierror)
      call cmo_get_info('wic',cmo,ipwic,lenwic,icmotype,ierror)
      call cmo_get_info('itetclr',cmo,ipitetclr,lenitetclr,icmotype,ier)
      call cmo_get_info('itettyp',cmo,ipitettyp,lenitettyp,icmotype,ier)
      call cmo_get_info('itetoff',cmo,ipitetoff,lenitetoff,icmotype,ier)
      call cmo_get_info('jtetoff',cmo,ipjtetoff,lenjtetoff,icmotype,ier)
      call cmo_get_info('itet',cmo,ipitet,lenitet,icmotype,ierror)
      call cmo_get_info('jtet',cmo,ipjtet,lenjtet,icmotype,ierror)
C
      call mmgetblk('iparent',isubname,ipiparent,nnodes,2,icscode)
      call unpackpc(nnodes,itp1,isn1,iparent)
C
      iunit=-1
      call hassign(iunit,ifile,ierror)
C
      write(iunit,9030) nelements, nelements, nnodes, nnodes, 0, 0, 0
 9030 format(7(i10,1x))
      write(iunit,'("X")')
      write(iunit,*) (xic(i),i=1,nnodes)
      write(iunit,'("Y")')
      write(iunit,*) (yic(i),i=1,nnodes)
      write(iunit,'("Z")')
      write(iunit,*) (zic(i),i=1,nnodes)
      write(iunit,'("NODESV")')
      do it=1,nelements
         index=itetoff(it)
         if(itettyp(it).eq.ifelmtet) then
            i1=itet1(index+1)
            i2=itet1(index+2)
            i3=itet1(index+3)
            i4=itet1(index+4)
            write(iunit,*) i1,i2,i3,i3,i4,i4,i4,i4
         elseif(itettyp(it).eq.ifelmhex) then
            i1=itet1(index+1)
            i2=itet1(index+2)
            i3=itet1(index+3)
            i4=itet1(index+4)
            i5=itet1(index+5)
            i6=itet1(index+6)
            i7=itet1(index+7)
            i8=itet1(index+8)
            write(iunit,*) i1,i2,i3,i4,i5,i6,i7,i8
         elseif(itettyp(it).eq.ifelmpyr) then
            i1=itet1(index+1)
            i2=itet1(index+2)
            i3=itet1(index+3)
            i4=itet1(index+4)
            i5=itet1(index+5)
            i6=itet1(index+5)
            i7=itet1(index+5)
            i8=itet1(index+5)
            write(iunit,*) i1,i2,i3,i4,i5,i6,i7,i8
         elseif(itettyp(it).eq.ifelmpri) then
            i1=itet1(index+1)
            i2=itet1(index+2)
            i3=itet1(index+3)
            i4=itet1(index+3)
            i5=itet1(index+4)
            i6=itet1(index+5)
            i7=itet1(index+6)
            i8=itet1(index+6)
            write(iunit,*) i1,i2,i3,i4,i5,i6,i7,i8
         else
            write(logmess,9000) it,itettyp(it)
 9000       format('Not a supported CHAD element: ',2i10)
            call writloga('default',0,logmess,0,ierrwrt)
         endif
      enddo
      write(iunit,'("ICOLORN")')
      write(iunit,*) (1,i=1,nnodes)
      write(iunit,'("MATNON")')
      write(iunit,*) (imt1(i),i=1,nnodes)
      length=nnodes
      call mmgetblk("itp2",isubname,ipitp2,length,2,icscode)
      call mmfindbk('chad_itp1',cmo,ipitp1_chad,lenout,icscode)
      if(icscode.eq.0) then
         do i=1,nnodes
            itp2(i)=itp1_chad(i)
         enddo
      else
         do i=1,nnodes
            itype=itp1(i)
            if(itype.ge.ifitpmrg) then
               itype=-1
            elseif(itype.eq.ifitpcup) then
               itype=-1
            elseif(itype.eq.ifitpint) then
               itype=0
            elseif(itype.eq.ifitpfre) then
               itype=11
            elseif(itype.eq.ifitprfl) then
               itype=21
            elseif(itype.eq.ifitpinb) then
               itype=21
            elseif(itype.eq.ifitpini) then
               itype=91
            else
               itype=0
            endif
            itp2(i)=itype
         enddo
      endif
      write(iunit,'("NODETYPE")')
      write(iunit,*) (itp2(i),i=1,nnodes)
      write(iunit,'("CONTAB")')
C
      xmin1=1.0d+30
      ymin1=1.0d+30
      zmin1=1.0d+30
      xmax1=-xmin1
      ymax1=-ymin1
      zmax1=-zmin1
      rmax1=-zmin1
      do i=1,nnodes
         xmin1=min(xmin1,xic(i))
         ymin1=min(ymin1,yic(i))
         zmin1=min(zmin1,zic(i))
         xmax1=max(xmax1,xic(i))
         ymax1=max(ymax1,yic(i))
         zmax1=max(zmax1,zic(i))
         ra=sqrt(xic(i)**2+yic(i)**2)
         rmax1=max(rmax1,ra)
      enddo
      do i1=1,nnodes
         icr=icr1(i1)
         if(icr.le.0) then
            do j=1,3
               do i=1,3
                  xcontab1(i,j)=xid(i,j)
               enddo
            enddo
         elseif(icr.le.6) then
            xc(1) = 0.0
            xc(2) = 0.0
            xc(3) = 0.0
            if (ifvels) then
               xc(1) = vels(1,i1)
               xc(2) = vels(2,i1)
               xc(3) = vels(3,i1)
            endif
            do j = 1,3
               do i = 1,3
                  ii = ii+1
                  xcontab1(i,j) = xid(i,j)-xc(i)*xc(j)
               enddo
            enddo
         elseif(icr.le.(6+6*6)) then
            xc(1) = 0.0
            xc(2) = 0.0
            xc(3) = 0.0
            if (ifvels) then
               xc(1) = vels(1,i1)
               xc(2) = vels(2,i1)
               xc(3) = vels(3,i1)
            endif
            do j = 1,3
               do i = 1,3
                  xcontab1(i,j) = xc(i)*xc(j)
               enddo
            enddo
         elseif(icr.le.(6+6*6)) then
            do j = 1,3
               do i = 1,3
                  xcontab1(i,j) = 0.0
               enddo
            enddo
         else
            do j = 1,3
               do i = 1,3
                  xcontab1(i,j) = 0.0
               enddo
            enddo
         endif
         do j = 1,3
            do i = 1,3
               xcontab1(i,j) = 0.0
               if(i.eq.j) xcontab1(i,j)=1.0
            enddo
         enddo
C*****   xa=xic(i1)
C*****   ya=yic(i1)
C*****   za=zic(i1)
C*****   ra=sqrt(xa**2+ya**2)
C*****   xn=xa/(ra+1.0d-10)
C*****   yn=ya/(ra+1.0d-10)
C*****   zn=0.0d+00
C*****   iflag=0
C*****   if(ra.ge.(rmax1-1.0d-10).and.za.le.(zmin1+1.0d-10)) then
C*****      iflag=1
C*****      xcontab1(1,1)=1.0d-00 - xn**2
C*****      xcontab1(1,2)=-yn*xn
C*****      xcontab1(2,2)=1.0d-00 - yn**2
C*****      xcontab1(3,3)=0.0d-00
C*****   elseif(ra.ge.(rmax1-1.0d-10).and.za.ge.(zmax1-1.0d-10)) then
C*****      iflag=1
C*****      xcontab1(1,1)=1.0d-00 - xn**2
C*****      xcontab1(1,2)=-yn*xn
C*****      xcontab1(2,2)=1.0d-00 - yn**2
C*****      xcontab1(3,3)=0.0d-00
C*****   elseif(ra.ge.(rmax1-1.0d-10)) then
C*****      iflag=1
C*****      zn=0.0d-00
C*****      xcontab1(1,1)=1.0d-00 - xn**2
C*****      xcontab1(1,2)=-yn*xn
C*****      xcontab1(2,2)=1.0d-00 - yn**2
C*****      xcontab1(1,3)=-zn*xn
C*****      xcontab1(2,3)=-zn*yn
C*****      xcontab1(3,3)=1.0d-00 - zn**2
C*****   elseif(za.le.(zmin1+1.0d-10)) then
C*****      iflag=1
C*****      xcontab1(1,1)=1.0d-00
C*****      xcontab1(2,2)=1.0d-00
C*****      xcontab1(3,3)=0.0d-00
C*****   elseif(za.ge.(zmax1-1.0d-10)) then
C*****      iflag=1
C*****      xcontab1(1,1)=1.0d-00
C*****      xcontab1(2,2)=1.0d-00
C*****      xcontab1(3,3)=0.0d-00
C*****   endif
         write(iunit,*) xcontab1(1,1),xcontab1(1,2),
     *                  xcontab1(1,3),xcontab1(2,2),
     *                  xcontab1(2,3),xcontab1(3,3)
         if(ifvels) then
         if(iflag.eq.0) then
            vels(1,i1)=0.0
            vels(2,i1)=0.0
            vels(3,i1)=0.0
         else
            vels(1,i1)=xcontab1(1,1)+xcontab1(1,2)+xcontab1(1,3)
            vels(2,i1)=xcontab1(1,2)+xcontab1(2,2)+xcontab1(2,3)
            vels(3,i1)=xcontab1(1,3)+xcontab1(2,3)+xcontab1(3,3)
         endif
         endif
      enddo
      write(iunit,'("EPS")')
C*****write(iunit,*) (pic(i),i=1,nnodes)
      write(iunit,*) (0.0,i=1,nnodes)
      write(iunit,'("P")')
C*****write(iunit,*) (pic(i),i=1,nnodes)
      write(iunit,*) (0.0,i=1,nnodes)
      write(iunit,'("T")')
C*****write(iunit,*) (eic(i),i=1,nnodes)
      write(iunit,*) (0.0,i=1,nnodes)
      write(iunit,'("TKE")')
C*****write(iunit,*) (pic(i),i=1,nnodes)
      write(iunit,*) (0.0,i=1,nnodes)
      write(iunit,'("U")')
C*****write(iunit,*) (vels(1,i1),i=1,nnodes)
      write(iunit,*) (0.0,i=1,nnodes)
      write(iunit,'("V")')
C*****write(iunit,*) (vels(2,i1),i=1,nnodes)
      write(iunit,*) (0.0,i=1,nnodes)
      write(iunit,'("W")')
C*****write(iunit,*) (vels(3,i1),i=1,nnodes)
      write(iunit,*) (0.0,i=1,nnodes)
      write(iunit,'("ICOLORE")')
      write(iunit,*) (itetclr(it),it=1,nelements)
      write(iunit,'("REGNOE")')
      write(iunit,*) (itetclr(it),it=1,nelements)
C*****write(iunit,'("IPARENT")')
C*****write(iunit,*) (iparent(i1),i1=1,nnodes)
      write(iunit,'("ROTXX")')
      write(iunit,*) (1.0,it=1,nnodes)
      write(iunit,'("ROTXY")')
      write(iunit,*) (0.0,it=1,nnodes)
      write(iunit,'("ROTXZ")')
      write(iunit,*) (0.0,it=1,nnodes)
      write(iunit,'("ROTYX")')
      write(iunit,*) (0.0,it=1,nnodes)
      write(iunit,'("ROTYY")')
      write(iunit,*) (1.0,it=1,nnodes)
      write(iunit,'("ROTYZ")')
      write(iunit,*) (0.0,it=1,nnodes)
      write(iunit,'("ROTZX")')
      write(iunit,*) (0.0,it=1,nnodes)
      write(iunit,'("ROTZY")')
      write(iunit,*) (0.0,it=1,nnodes)
      write(iunit,'("ROTZZ")')
      write(iunit,*) (1.0,it=1,nnodes)
      write(iunit,'("STRAINP")')
      write(iunit,*) (0.0,it=1,nnodes)
      write(iunit,'("STRAINPDOT")')
      write(iunit,*) (0.0,it=1,nnodes)
      write(iunit,'("STRETCHXX")')
      write(iunit,*) (1.0,it=1,nnodes)
      write(iunit,'("STRETCHXY")')
      write(iunit,*) (0.0,it=1,nnodes)
      write(iunit,'("STRETCHXZ")')
      write(iunit,*) (0.0,it=1,nnodes)
      write(iunit,'("STRETCHYX")')
      write(iunit,*) (0.0,it=1,nnodes)
      write(iunit,'("STRETCHYY")')
      write(iunit,*) (1.0,it=1,nnodes)
      write(iunit,'("STRETCHYZ")')
      write(iunit,*) (0.0,it=1,nnodes)
      write(iunit,'("STRETCHZX")')
      write(iunit,*) (0.0,it=1,nnodes)
      write(iunit,'("STRETCHZY")')
      write(iunit,*) (0.0,it=1,nnodes)
      write(iunit,'("STRETCHZZ")')
      write(iunit,*) (1.0,it=1,nnodes)
      write(iunit,'("STRSXX")')
      write(iunit,*) (0.0,it=1,nnodes)
      write(iunit,'("STRSXY")')
      write(iunit,*) (0.0,it=1,nnodes)
      write(iunit,'("STRSXZ")')
      write(iunit,*) (0.0,it=1,nnodes)
      write(iunit,'("STRSYY")')
      write(iunit,*) (0.0,it=1,nnodes)
      write(iunit,'("STRSYZ")')
      write(iunit,*) (0.0,it=1,nnodes)
      write(iunit,'("STRSZZ")')
      write(iunit,*) (0.0,it=1,nnodes)
C
      write(iunit,'("FACES")')
      do it=1,nelements
         index=itetoff(it)
         ity=itettyp(it)
         nef=nelmnef(ity)
         if(ity.eq.ifelmtet) then
            j1=1+(jtet1(index+1)-1)/nef
            j2=1+(jtet1(index+2)-1)/nef
            j3=1+(jtet1(index+3)-1)/nef
            j4=1+(jtet1(index+4)-1)/nef
            j5=0
            j6=0
            if(j1.gt.nelements) j1=0
            if(j2.gt.nelements) j2=0
            if(j3.gt.nelements) j3=0
            if(j4.gt.nelements) j4=0
C*****      write(iunit,*) j1,j2,j3,j4,j5,j6
         elseif(ity.eq.ifelmhex) then
            j1=1+(jtet1(index+1)-1)/nef
            j2=1+(jtet1(index+2)-1)/nef
            j3=1+(jtet1(index+5)-1)/nef
            j4=1+(jtet1(index+6)-1)/nef
            j5=1+(jtet1(index+4)-1)/nef
            j6=1+(jtet1(index+3)-1)/nef
            if(j1.gt.nelements) j1=0
            if(j2.gt.nelements) j2=0
            if(j3.gt.nelements) j3=0
            if(j4.gt.nelements) j4=0
            if(j5.gt.nelements) j5=0
            if(j6.gt.nelements) j6=0
C*****       write(iunit,*) j1,j2,j3,j4,j5,j6
         elseif(ity.eq.ifelmpyr.or.ity.eq.ifelmpri) then
            j1=1+(jtet1(index+1)-1)/nef
            j2=1+(jtet1(index+2)-1)/nef
            j3=1+(jtet1(index+5)-1)/nef
            j4=1+(jtet1(index+6)-1)/nef
            j5=1+(jtet1(index+4)-1)/nef
            j6=0
            if(j1.gt.nelements) j1=0
            if(j2.gt.nelements) j2=0
            if(j3.gt.nelements) j3=0
            if(j4.gt.nelements) j4=0
            if(j5.gt.nelements) j5=0
C*****       write(iunit,*) j1,j2,j3,j4,j5,j6
         endif
         write(iunit,*) 0,0,0,0,0,0
      enddo
C
      close(iunit)
C
      goto 9999
 9999 continue
      call mmrelprt(isubname,icscode)
      return
      end
*dk,seticr
      subroutine seticr(cmo,cmohex,cmotet5)
C
C #####################################################################
C
C     PURPOSE -
C
C        THIS ROUTINE WRITES A DUMP FILE FOR CHAD.
C
C     INPUT ARGUMENTS -
C
C        ifile - CHARACTER FILE NAME.
C
C     OUTPUT ARGUMENTS -
C
C        ierror - ERROR FLAG (=0 ==> OK, <>0 ==> ERROR).
C
C     CHANGE HISTORY -
C
C        $Log: dumpchad.f,v $
C        Revision 2.00  2007/11/05 19:45:53  spchu
C        Import to CVS
C
CPVCS
CPVCS       Rev 1.1   02/16/95 07:35:28   het
CPVCS    Correct format errors in dump and read commands
CPVCS
CPVCS       Rev 1.0   01/17/95 16:36:00   pvcs
CPVCS    Original Version
C
C ######################################################################
C
      implicit real*8 (a-h,o-z)
C
C ######################################################################
C
      include 'chydro.h'
      include 'local_element.h'
C
      character*(*) cmo, cmohex, cmotet5
C
C
      pointer (ipisetwd, isetwd)
      pointer (ipimt1, imt1)
      pointer (ipitp1, itp1)
      pointer (ipicr1, icr1)
      pointer (ipisn1, isn1)
      pointer (ipign1, ign1)
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      pointer (ipuic, uic)
      pointer (ipvic, vic)
      pointer (ipwic, wic)
      pointer (ippic, pic)
      pointer (ipric, ric)
      pointer (ipeic, eic)
      pointer (ipitet, itet)
      pointer (ipitet, itet1)
      pointer (ipjtet, jtet)
      pointer (ipjtet, jtet1)
      integer isetwd(1000000)
      integer imt1(1000000), itp1(1000000),
     *        icr1(1000000), isn1(1000000),
     *        ign1(1000000),
     *        itet(4,1000000), jtet(4,1000000)
      integer itet1(4*1000000), jtet1(4*1000000)
      dimension   xic(1000000), yic(1000000), zic(1000000)
      dimension   uic(1000000), vic(1000000), wic(1000000)
      dimension   pic(1000000), ric(1000000), eic(1000000)
      pointer (ipitetclr, itetclr)
      pointer (ipitettyp, itettyp)
      pointer (ipitetoff, itetoff)
      pointer (ipjtetoff, jtetoff)
      integer itetclr(1000000), itettyp(1000000),
     *        itetoff(1000000), jtetoff(1000000)
C
      pointer (ipicr2, icr2)
      integer icr2(1000000)
      pointer (ipitp2, itp2)
      integer itp2(1000000)
      pointer (ipuic2, uic2)
      pointer (ipvic2, vic2)
      pointer (ipwic2, wic2)
      dimension   uic2(1000000), vic2(1000000), wic2(1000000)
C
C ######################################################################
C
      pointer (ipibnd1, ibnd1)
      integer ibnd1(10,100000)
      pointer (ipibnd2, ibnd2)
      integer ibnd2(10,100000)
C
      integer  nen, nef
C
      parameter (nentri=3, nfacetri=3)
      parameter (nentet=4, nfacetet=4)
      parameter (nenprism=6, nfaceprism=5)
      parameter (nenhex=8, nfacehex=6)
C
      dimension ihexface0(nfacehex), ihexface1(4,nfacehex)
C     top,bottom,front,right,back,left
      data ihexface0 / 4, 4, 4, 4, 4, 4 /
      data ihexface1 / 1, 2, 3, 4,
     *                 5, 8, 7, 6,
     *                 1, 5, 6, 2,
     *                 2, 6, 7, 3,
     *                 3, 7, 8, 4,
     *                 1, 4, 8, 5 /
      dimension iprismface0(nfaceprism), iprismface1(4,nfaceprism)
C     top,bottom,right,back,left
      data iprismface0 / 3, 3, 4, 4, 4 /
      data iprismface1 / 1, 2, 3, 0,
     *                   4, 6, 5, 0,
     *                   1, 4, 5, 2,
     *                   2, 5, 6, 3,
     *                   1, 3, 6, 4 /
C
      integer intpairhex(2,12)
      data intpairhex / 1,2, 2,3, 3,4, 4,1, 5,6, 6,7, 7,8,
     *                  8,5, 1,5, 2,6, 3,7, 4,8 /
 
      dimension itetface0(nfacetet), itetface1(4,nfacetet)
C     top,back,left,right
      data itetface0 / 3, 3, 3, 3 /
      data itetface1 / 2, 3, 4, 0,
     *                 1, 4, 3, 0,
     *                 1, 2, 4, 0,
     *                 1, 3, 2, 0 /
      dimension itriface0(nfacetri), itriface1(3,nfacetri)
C     top,back,left,right
      data itriface0 / 2, 2, 2 /
      data itriface1 / 2, 3, 1,
     *                 3, 1, 2,
     *                 1, 2, 3 /
      integer intpairtet(2,6)
      data intpairtet / 1,2, 1,3, 1,4, 2,3, 2,4, 3,4 /
C
C
      character*32 isubname,cout
      pointer(ipout,out)
      real*8 out(*),rout
      integer iout,ilen,ityp
C
      data ivoronoi2d / 1 /
      data ivoronoi3d / 0 /
C
C ######################################################################
C
C
      crosx(a,b,c,d,e,f)=b*f-c*e
      crosy(a,b,c,d,e,f)=c*d-a*f
      crosz(a,b,c,d,e,f)=a*e-b*d
C
C ######################################################################
C
C
      isubname="seticr"
C
C*****call cmo_get_name(cmo,ierror)
C
      call cmo_get_info('nnodes',cmo,nnodes,lencmo,itpcmo,ierror)
      call cmo_get_info('nelements',cmo,nelements,lencmo,itpcmo,ierror)
      call cmo_get_info('mbndry',cmo,mbndry,length,icmotype,ierror)
      call cmo_get_info('ndimensions_topo',cmo,
     *                  nsdtopo,length,icmotype,ierror)
      call cmo_get_info('ndimensions_geom',cmo,
     *                  nsdgeom,length,icmotype,ierror)
      call cmo_get_info('nodes_per_element',cmo,
     *                  nen,length,icmotype,ierror)
      call cmo_get_info('faces_per_element',cmo,
     *                  nef,length,icmotype,ierror)
      call cmo_get_info('isetwd',cmo,ipisetwd,lenisetwd,icmotype,ier)
      call cmo_get_info('imt1',cmo,ipimt1,lenimt1,icmotype,ierror)
      call cmo_get_info('itp1',cmo,ipitp1,lenitp1,icmotype,ierror)
      call cmo_get_info('icr1',cmo,ipicr1,lenicr1,icmotype,ierror)
      call cmo_get_info('isn1',cmo,ipisn1,lenisn1,icmotype,ierror)
      call cmo_get_info('ign1',cmo,ipign1,lenign1,icmotype,ierror)
      call cmo_get_info('xic',cmo,ipxic,lenxic,icmotype,ierror)
      call cmo_get_info('yic',cmo,ipyic,lenyic,icmotype,ierror)
      call cmo_get_info('zic',cmo,ipzic,lenzic,icmotype,ierror)
      call cmo_get_info('uic',cmo,ipuic,lenuic,icmotype,ierror)
      call cmo_get_info('vic',cmo,ipvic,lenvic,icmotype,ierror)
      call cmo_get_info('wic',cmo,ipwic,lenwic,icmotype,ierror)
      call cmo_get_info('pic',cmo,ippic,lenpic,icmotype,ierror)
      call cmo_get_info('ric',cmo,ipric,lenric,icmotype,ierror)
      call cmo_get_info('eic',cmo,ipeic,leneic,icmotype,ierror)
      call cmo_get_info('itetclr',cmo,ipitetclr,lenitetclr,icmotype,ier)
      call cmo_get_info('itettyp',cmo,ipitettyp,lenitettyp,icmotype,ier)
      call cmo_get_info('itetoff',cmo,ipitetoff,lenitetoff,icmotype,ier)
      call cmo_get_info('jtetoff',cmo,ipjtetoff,lenjtetoff,icmotype,ier)
      call cmo_get_info('itet',cmo,ipitet,lenitet,icmotype,ierror)
      call cmo_get_info('jtet',cmo,ipjtet,lenjtet,icmotype,ierror)
C
C
      if(nen.eq.4.and.nef.eq.4) then
         do i=1,nnodes
            uic(i)=0.0d+00
            vic(i)=0.0d+00
            wic(i)=0.0d+00
         enddo
         do it=1,nelements
            do i=1,nef
               if(jtet1(nef*(it-1)+i).eq.mbndry) then
                  i1=itet1(nef*(it-1)+itetface1(4,i))
                  i2=itet1(nef*(it-1)+itetface1(1,i))
                  i3=itet1(nef*(it-1)+itetface1(2,i))
                  i4=itet1(nef*(it-1)+itetface1(3,i))
                  x2=xic(i2)
                  y2=yic(i2)
                  z2=zic(i2)
                  x3=xic(i3)
                  y3=yic(i3)
                  z3=zic(i3)
                  x4=xic(i4)
                  y4=yic(i4)
                  z4=zic(i4)
                  xarea= ((y2-y3)*(z4-z3)-(y4-y3)*(z2-z3))
                  yarea=-((x2-x3)*(z4-z3)-(x4-x3)*(z2-z3))
                  zarea= ((x2-x3)*(y4-y3)-(x4-x3)*(y2-y3))
                  uic(i2)=uic(i2)+xarea/6.0d+00
                  vic(i2)=vic(i2)+yarea/6.0d+00
                  wic(i2)=wic(i2)+zarea/6.0d+00
                  uic(i3)=uic(i3)+xarea/6.0d+00
                  vic(i3)=vic(i3)+yarea/6.0d+00
                  wic(i3)=wic(i3)+zarea/6.0d+00
                  uic(i4)=uic(i4)+xarea/6.0d+00
                  vic(i4)=vic(i4)+yarea/6.0d+00
                  wic(i4)=wic(i4)+zarea/6.0d+00
               endif
            enddo
         enddo
         xareamax=0.0d+00
         do i=1,nnodes
            pic(i)=sqrt(uic(i)**2+vic(i)**2+wic(i)**2)
            xareamax=max(xareamax,pic(i))
         enddo
         do i=1,nnodes
            if(pic(i).gt.1.0d-06*xareamax) then
               uic(i)=-uic(i)/pic(i)
               vic(i)=-vic(i)/pic(i)
               wic(i)=-wic(i)/pic(i)
            endif
         enddo
         do i=1,nnodes
C*****      if(icr1(i).eq.0) then
C*****         uic(i)=1.0
C*****         vic(i)=1.0
C*****         wic(i)=1.0
C*****         elseif(icr1(i).le.6) then
C*****         elseif(icr1(i).le.(6+6*6)) then
C*****            do j=1,nnodes
C*****               if(j.ne.i.and.icr1(j).eq.icr1(i)) then
C*****                  uic(i)=xic(j)-xic(i)
C*****                  vic(i)=yic(j)-yic(i)
C*****                  wic(i)=zic(j)-zic(i)
C*****                  pic(i)=sqrt(uic(i)**2+vic(i)**2+wic(i)**2)
C*****                  if(pic(i).gt.1.0d-06*xareamax) then
C*****                     uic(i)=-uic(i)/pic(i)
C*****                     vic(i)=-vic(i)/pic(i)
C*****                     wic(i)=-wic(i)/pic(i)
C*****                  endif
C*****                  goto 100
C*****               endif
C*****            enddo
C*****100           continue
C*****         elseif(icr1(i).le.(6+6*6+6*6*6)) then
C*****            uic(i)=0.0
C*****            vic(i)=0.0
C*****            wic(i)=0.0
C*****         endif
         enddo
         call cmo_get_info('uic',cmohex,ipuic2,lenuic2,icmotype,ierror)
         call cmo_get_info('vic',cmohex,ipvic2,lenvic2,icmotype,ierror)
         call cmo_get_info('wic',cmohex,ipwic2,lenwic2,icmotype,ierror)
         do i=1,nnodes
            uic2(i)=uic(i)
            vic2(i)=vic(i)
            wic2(i)=wic(i)
         enddo
      elseif(nen.eq.8.and.nef.eq.6) then
         length=10*nnodes
         call mmgetblk('ibnd1',isubname,ipibnd1,length,2,icscode)
         call mmgetblk('ibnd2',isubname,ipibnd2,length,2,icscode)
         do j=1,nnodes
            do i=1,10
               ibnd1(i,j)=0
               ibnd2(i,j)=0
            enddo
         enddo
         do it=1,nelements
            do i=1,nelmnef(itettyp(it))
               if(jtet1(jtetoff(it)+i).eq.mbndry) then
                  do j=1,ielmface0(i,itettyp(it))
                     i1=itet1(itetoff(it)+ielmface1(j,i,itettyp(it)))
                     do k=1,10
                        if(ibnd2(k,i1).eq.i) then
                           goto 200
                        elseif(ibnd2(k,i1).eq.0) then
                           ibnd1(k,i1)=it
                           ibnd2(k,i1)=i
                           goto 200
                        endif
                     enddo
 200                 continue
                  enddo
               endif
            enddo
         enddo
         ibnd4=0
         icountmax=0
         do i=1,nnodes
            icount=0
            do j=1,10
               if(ibnd1(j,i).ne.0) then
                  icount=icount+1
               endif
            enddo
            if(icount.eq.0) then
               icr1(i)=0
            elseif(icount.eq.1) then
               icr1(i)=ibnd2(1,i)
            elseif(icount.eq.2) then
               icr1(i)=6+ibnd2(1,i)*ibnd2(2,i)
            elseif(icount.eq.3) then
               icr1(i)=6+6*6+ibnd2(1,i)*ibnd2(2,i)*ibnd2(3,i)
            else
               ibnd4=ibnd4+1
               icr1(i)=6+6*6+6*6*6+1
            endif
            icountmax=max(icountmax,icount)
         enddo
         icount0=0
         icount1=0
         icount2=0
         icount3=0
         do i=1,nnodes
            if(icr1(i).eq.0) then
               icount0=icount0+1
            elseif(icr1(i).le.6) then
               icount1=icount1+1
            elseif(icr1(i).le.(6+6*6)) then
               icount2=icount2+1
            elseif(icr1(i).le.(6+6*6+6*6*6)) then
               icount3=icount3+1
            else
               icount4=icount4+1
            endif
         enddo
         call cmo_get_info('icr1',cmotet5,ipicr2,lenicr2,icmotype,ier)
         do i=1,nnodes
            if(icr1(i).ne.0) icr1(i)=1
            icr2(i)=icr1(i)
         enddo
         call cmo_get_info('itp1',cmotet5,ipitp2,lenitp2,icmotype,ier)
         do i=1,nnodes
            itp1(i)=0
            itp2(i)=0
         enddo
         do it=1,nelements
            do i=1,nelmnef(itettyp(it))
               if(jtet1(jtetoff(it)+i).eq.mbndry) then
                  do j=1,ielmface0(i,itettyp(it))
                     i1=itet1(itetoff(it)+ielmface1(j,i,itettyp(it)))
                     itp1(i1)=10
                     itp2(i1)=10
                  enddo
               endif
            enddo
         enddo
      endif
C
      goto 9999
 9999 continue
      call mmrelprt(isubname,icscode)
      return
      end
