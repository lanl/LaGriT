      subroutine recon2(npoints,ntets,toldamage)
C
C
C #####################################################################
C
C     PURPOSE -
C
C        This routine performs direct reconnections on a given
C        list of tetrahedrons
C
C     INPUT ARGUMENTS -
C
C        None
C
C     OUTPUT ARGUMENTS -
C
C        None
C
C     CHANGE HISTORY -
C
C        $Log: recon2.f,v $
C        Revision 2.00  2007/11/09 20:04:00  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.9   30 Sep 2004 13:58:56   dcg
CPVCS    use iand, ior in place of .and. .or. with integer variables
CPVCS    
CPVCS       Rev 1.8   30 Sep 2004 09:26:54   dcg
CPVCS    replace calls to real( with calls to dble(
CPVCS    remove unneeded calls to real(vol....
CPVCS    
CPVCS       Rev 1.7   01 Apr 2004 16:23:00   gable
CPVCS    Changed screen info output of flips from i5 to i9.
CPVCS    
CPVCS       Rev 1.6   18 Jun 2003 08:10:34   dcg
CPVCS    pass iopt2to2 to try4to4xv so as to be able to skip interface recons if
CPVCS    iopt2to2 is equal to zero
CPVCS    
CPVCS       Rev 1.5   25 Feb 2002 09:59:20   dcg
CPVCS    fix error in debug output ( skip tets marked for deletion when
CPVCS    checking for positive volumes
CPVCS    
CPVCS       Rev 1.4   15 May 2001 15:04:20   kuprat
CPVCS    We now obtain TOLDAMAGE on the call line.
CPVCS    
CPVCS       Rev 1.3   05 Jan 2001 12:57:14   dcg
CPVCS    use dble in place of float so as not to lose digits
CPVCS    
CPVCS       Rev 1.2   05 May 2000 15:14:34   dcg
CPVCS    refresh mbndry value after cmo_newlen
CPVCS    
CPVCS       Rev 1.1   03 Feb 2000 12:35:16   dcg
CPVCS    
CPVCS       Rev 1.0   25 Jan 2000 15:42:06   dcg
CPVCS    Initial revision.
CPVCS
CPVCS       Rev 1.77   Wed Nov 10 15:19:48 1999   dcg
CPVCS    remove references to ihcycle
CPVCS    make xmin... ioptinv local variables
CPVCS
CPVCS       Rev 1.76   Wed Nov 10 11:28:10 1999   dcg
CPVCS    comment out calls to timing (it was a noop)
CPVCS
CPVCS       Rev 1.75   Wed Nov 10 09:18:28 1999   dcg
CPVCS    remove references to icdname
CPVCS
CPVCS       Rev 1.74   Tue Sep 07 17:05:46 1999   dcg
CPVCS    allow for icr1 values of zero on external boundaries
CPVCS
CPVCS       Rev 1.73   Tue Aug 03 13:33:00 1999   dcg
CPVCS    test for existence of icontab
CPVCS
CPVCS       Rev 1.72   Wed Feb 03 15:23:56 1999   dcg
CPVCS    remove calls to fluxing routines and associated memory.
CPVCS
CPVCS       Rev 1.71   Mon Feb 01 13:32:42 1999   dcg
CPVCS    remove 'mega' related attributes at end of routine for
CPVCS    ivornoi =2 or -2
CPVCS
CPVCS       Rev 1.70   Fri Aug 28 14:25:10 1998   dcg
CPVCS    remove single precision constants
CPVCS
CPVCS       Rev 1.69   Wed Aug 05 13:18:48 1998   dcg
CPVCS    fix order of nodes in new tets for 2to3 flips with
CPVCS    ivoronoi=-2 or 2
CPVCS
CPVCS       Rev 1.68   Tue Jun 30 08:46:10 1998   dcg
CPVCS    fix 2to2 boundary flips when ivoronoi=-2
CPVCS
CPVCS       Rev 1.68   Tue Jun 30 08:45:34 1998   dcg
CPVCS    fix 2to2 boundary flips when ivoronoi=-1
CPVCS
CPVCS       Rev 1.67   Tue Jun 16 14:13:48 1998   dcg
CPVCS    call filholes only if there are holes to fill after 3to2 flips
CPVCS    and ivoronoi =2 ,-2
CPVCS
CPVCS       Rev 1.66   Wed Jun 10 16:58:54 1998   dcg
CPVCS    fix errors in ivoronoi=-1 option
CPVCS
CPVCS       Rev 1.65   Tue May 26 14:48:50 1998   dcg
CPVCS    fix typo
CPVCS
CPVCS       Rev 1.64   Tue May 26 10:37:22 1998   kuprat
CPVCS    Put in calls to TESTDAMAGE ahead of call to FND2TO3I.
CPVCS
CPVCS       Rev 1.63   Mon May 25 00:47:58 1998   kuprat
CPVCS    Disabled FLP2TO3I call.
CPVCS
CPVCS       Rev 1.62   Fri Apr 17 15:34:42 1998   dcg
CPVCS    separate recon routines
CPVCS
CPVCS       Rev 1.12   06/28/95 11:08:10   het
CPVCS    Delete some extra mmrelprt statements
CPVCS
CPVCS       Rev 1.11   05/01/95 16:12:14   het
CPVCS    replace the double quotes in the mmrelprt calles
CPVCS
CPVCS       Rev 1.10   03/15/95 13:49:20   dcg
CPVCS    increment temp memory for recon if needed
CPVCS
CPVCS       Rev 1.7   02/10/95 08:27:32   het
CPVCS    Change the nfaces variable to nface because of cmo.h
CPVCS
CPVCS       Rev 1.6   01/11/95 22:23:20   het
CPVCS
CPVCS
CPVCS       Rev 1.5   01/09/95 17:31:58   het
CPVCS    Unicos changes.
CPVCS
CPVCS
CPVCS       Rev 1.4   12/02/94 16:19:08   dcg
CPVCS     changes required to compile on IBM RS6000
CPVCS
CPVCS       Rev 1.3   12/02/94 15:06:32   het
CPVCS    Added an option for the "cmo" access functions
CPVCS
CPVCS
CPVCS       Rev 1.2   12/01/94 18:48:50   het
CPVCS    Added a data type to the "cmo" calles
CPVCS       and added the "cmo.h" include file.
CPVCS
CPVCS       Rev 1.1   11/17/94 21:54:30   het
CPVCS    Added include files for chydro, neibor, cmerge, comdict. Added calles and
CPVCS    pointer statements for current_mesh_object database access.
C
C ######################################################################
C
      implicit none
C
      include "local_element.h"
      include "cmo.h"
      include "chydro.h"
      include "consts.h"
      include "neibor.h"
      include "cmerge.h"

C arguments
      integer npoints,ntets
      real*8 toldamage

C constants
      real*8 alargenumber,asmallnumber,atolerance
      parameter (alargenumber=1.0d+30)
      parameter (asmallnumber=1.0d-30)
      parameter (atolerance=1.0d-09)

C variables
      integer ntet(10), mtet(10),idnew(16),jdnew(16)
      integer ipos(3),itets(4),id(12),jd(12)
      integer invert(maxmerg)

      pointer (ipitetoff, itetoff)
      pointer (ipjtetoff, jtetoff)
      integer itetoff(*), jtetoff(*)
 
      pointer(ipiopen, lst)
      integer lst(*)
      pointer (ipicontab,icontab)
      integer icontab(50,*)
      pointer (ipiholes,iholes)
      integer iholes(*)

      pointer (ipxmegah, xmegah)
      pointer (ipxmegadet, xmegadet)
      pointer (ipxmegaerr, xmegaerr)
      real*8 xmegah(*), xmegadet(*), xmegaerr(*)

      integer ierror,length,icmotype,ioptinv,
     *  icscode,i2ndtime,ione,itwo,ithree,nnegvol,
     *  niters,n22,n32,n23,n23i,n32i,n32x,n44,n44i,n20,
     *  nface,indx,i,n2to2,n3to2,n2to3,n2to3i,
     *  n3to2i,n3to2x,n4to4,n4to4i,n2to0,n1to0,nface2,n,
     *  len,ier,ntetsmax,icmoerr,lenout,it,jt,jf,
     *  i1,i2,i3,i4,i5,n1,n2,n3,j,k1,k2,ip,it2,it3,k
     *  ,m,mm,isum,ipos1,ipos2,icrnbr1,icrnbr2,icrnbr4,
     *  icrnbr5,j1,j2,j4,j5,irb1,irb2,irb4,irb5,
     *  nntet2,ict,it4,nface0,np3,np4,k10,k20,np1,np2,
     *  j0,ierrwrt,npeeloff,nzerovol,ntry,
     *  nvacnto
      integer icksign1,icksign2,icksign3,jtemp,maxiter,
     *  maxpairs, n23r,n23b,n20r,n20b,ictinvrt,ito,itx,
     *  itnext,nxtilt,ierflg,ifpos2,ichoice1,ichoice2,
     *  iofs,iflip,ibdytet,itpgsum,itt,idum,jf1,jf2,n4,
     *  n5,igo,lenfix,ivt,itv,ierr,nbfaces,num,it1,j3,
     *  nvts,nface1,ifc,ifpos,iface,itest,ierrdum,
     *  nsd,kpe,nconbnd,ilen,itype,nen,nef,nbdyflps,
     *  nregflps,itk,ninvrt,ifposx,ntest,ictmrg,niter,
     *  n2to3r,icksign,index,n2to0r,n2to0b,
     *  nholes,iflag,iepos,ip1,ip2,jpos,nppos,nflips,
     *  iflg,j1max,j2max,j4max,j5max
      integer nflipsb,n2to3b, nbdyfc,monitor,nnfreq,iopt2to2

      real*8 cvmgm,cvmgpr,cvmgn,cvmgzr,cvmgmr
      real*8 crosx,crosy,crosz,crosx1,crosy1,crosz1,
     *   volume,xxlarge,xxsmall,xst,xst2,en,
     *  a124x, a124y,a124z,a215x,a215y,a215z,atotx,flag,
     *  atoty,atotz,atot,xmid,ymid,zmid,dot124,dot215,
     *  dot1,dot2, dot4,dot5,dotmin,dotmax,damage,
     *  dist4,dist5,xa,ya,za,xb,yb,zb,
     *  xd,yd,zd,xn,yn,zn,xl,yl,zl,q,rout,
     *  xv,yv,zv,dist,d,vol,vol1,vol2,vol3,vol4,xtest,
     *  xvoltet,x1,x2,x3,y1,y2,y3,z1,z2,z3,cksign1,
     *  cksign2,cksign3,volitk,volitx,volit

      real*8 a,b,c,e,f,xone,xfix3tst,
     *  xfix1tst,xfix2tst,xc,yc,zc,em,t1,
     *  coordmax,t2,test1,distsq,voljt

      logical itsttp
      logical flip, noicontab
 
      character*132 logmess
      character*32 isubname
      character*32  blkout, prtout
      character*32 cout
      character*8 cglobal, cdefault
C
C ######################################################################
C
C     Define some statement functions.
C
      crosx(a,b,c,d,e,f)=b*f-c*e
      crosy(a,b,c,d,e,f)=c*d-a*f
      crosz(a,b,c,d,e,f)=a*e-b*d
      crosx1(i,j,k)=(yic(j)-yic(i))*(zic(k)-zic(i))-
     *              (yic(k)-yic(i))*(zic(j)-zic(i))
      crosy1(i,j,k)=(xic(k)-xic(i))*(zic(j)-zic(i))-
     *              (xic(j)-xic(i))*(zic(k)-zic(i))
      crosz1(i,j,k)=(xic(j)-xic(i))*(yic(k)-yic(i))-
     *              (xic(k)-xic(i))*(yic(j)-yic(i))
      volume(i1,i2,i3,i4)=(xic(i4)-xic(i1))*crosx1(i1,i2,i3)+
     *                    (yic(i4)-yic(i1))*crosy1(i1,i2,i3)+
     *                    (zic(i4)-zic(i1))*crosz1(i1,i2,i3)
c      jtet30(m1,m2)=jtet(m1,m2).and.jmsk30
c      jtet130(m)=jtet1(m).and.jmsk30
C
C ######################################################################
C
      isubname='recon'
      ioptinv=0
      idebug=0
      cglobal='global'
      cdefault='default'
      call cmo_get_name(cmo,ierror)
C
      call cmo_get_info('ivoronoi',cmo,
     *                ivoronoi,ilen,itype,icscode)
         if (icscode .ne. 0) call x3d_error(isubname,'get_info_i')
      call get_global('monitor',
     *                monitor,rout,cout,itype,icscode)
         if (icscode .ne. 0) call x3d_error(isubname,'get_info_i')
      call cmo_get_info('nnfreq',cmo,
     *                nnfreq,ilen,itype,icscode)
         if (icscode .ne. 0) call x3d_error(isubname,'get_info_i')
      call cmo_get_info('iopt2to2',cmo,
     *                iopt2to2,ilen,itype,icscode)
         if (icscode .ne. 0) call x3d_error(isubname,'get_info_i')
      call cmo_get_info('idebug',cmo,
     *                idebug,ilen,itype,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'get_info_i')
C
C
C     ******************************************************************
C     FETCH MESH OBJECT NAME AND POINTER INFORMATION.
C
      icmouse=2
      icmoget=0
      icmoset=1
C
C
C
C     ******************************************************************
C     FETCH MESH OBJECT NAME AND POINTER INFORMATION.
C
      call cmo_get_name(cmo,ierror)
C
C
C     ******************************************************************
C     FETCH MESH OBJECT INFORMATION FOR THE DATA THAT THIS ROUTINE NEEDS
C
      call cmo_get_info('mbndry',cmo,mbndry,length,icmotype,ierror)
      call cmo_get_info('nconbnd',cmo,nconbnd,length,icmotype,ierror)
      call cmo_get_info('isetwd',cmo,ipisetwd,length,icmotype,ierror)
      call cmo_get_info('imt1',cmo,ipimt1,length,icmotype,ierror)
      call cmo_get_info('itp1',cmo,ipitp1,length,icmotype,ierror)
      call cmo_get_info('icr1',cmo,ipicr1,length,icmotype,ierror)
      call cmo_get_info('isn1',cmo,ipisn1,length,icmotype,ierror)
      call cmo_get_info('xic',cmo,ipxic,length,icmotype,ierror)
      call cmo_get_info('yic',cmo,ipyic,length,icmotype,ierror)
      call cmo_get_info('zic',cmo,ipzic,length,icmotype,ierror)
      call cmo_get_info('itetclr',cmo,ipitetclr,length,icmotype,ier)
      call cmo_get_info('itettyp',cmo,ipitettyp,length,icmotype,ier)
      call cmo_get_info('itet',cmo,ipitet,length,icmotype,ierror)
      call cmo_get_info('jtet',cmo,ipjtet,length,icmotype,ierror)
      call cmo_get_info('icontab',cmo,ipicontab,length,icmotype,ierror)
      noicontab=.false.
      if(ierror.ne.0) noicontab=.true.
C
C
C     ******************************************************************
C
C
C
      call mmfindbk('merglst2',cglobal,ipmerglst2,length,icscode)
      length=6*ntets
      if(icscode.eq.0) then
         call mmnewlen('merglst2',cglobal,ipmerglst2,length,icscode)
      else
         call mmgetblk('merglst2',cglobal,ipmerglst2,length,2,icscode)
      endif
 
C
C     ******************************************************************
C
C
Cdcg   timing call not implemented
Cdcg   set t1 to zero
      t1=0.
C
c     ipid=loc(id(1))
c     ipitets=loc(itets(1))
C
C     ******************************************************************
C
      i2ndtime=0
      ione=1
      itwo=2
      ithree=3
      xxlarge=alargenumber
      xxsmall=asmallnumber
      xst=atolerance
      xst2=atolerance
      nnegvol=0
      npairs2=0
      niters=0
      n22=0
      n32=0
      n23=0
      n23i=0
      n32i=0
      n32x=0
      n44=0
      n44i=0
      n20=0
      nface=0
      nvacnt=0
      nbfaces=0
C
C     ******************************************************************
C
C     CHECK IF ALL MATERIALS ARE BEING RECONNECTED; IF NOT, COMPRESS
C     OUT A LIST OF TETS WHICH LIE IN THE RECONNECTING MATERIAL REGIONS.
C
      if (irecnall.eq.0 .and. nrecon.eq.ntets) then
         indx=nxtilt(matrecon,nmats,1,1)
         if (indx .eq. nmats+1) then
            irecnall=1
         else
            do 3 i=1,ntets
C*****         itest=ifittet(i).and.msktrec
               kfix1(i)=cvmgn(i,0,itest)
 3          continue
            call kmprsn(ntets,kfix1,1,kfix1,1,kfix1,1,nrecon)
            if (lenrclst .lt. nrecon) then
               lenrclst=nrecon+100
               call mflip(ione,lenrclst,'irclst')
            endif
            do 4 i=1,nrecon
               irclst(i)=kfix1(i)
 4          continue
         endif
      elseif (abs(ivoronoi).eq.2) then
         call mflip(ione,ntets,'ivacnt')
         call mflip(ione,ntets,'iopen')
      endif
C
C     ******************************************************************
C
C     TEMPORARY DEBUG SECTION TO TEST THE MATERIAL-RECONNECTION BIT
C     FLAG.
C
C     if(mod(ihcycle,monitor).eq.0 .and. irecnall.eq.0) then
C        do 223 i=1,ntets
C*****      itest=ifittet(i).and.msktrec
C           kfix1(i)=cvmgn(i,0,itest)
C           imt=imt1(itet(1,i))
C           if (kfix1(i) .gt. 0) then
C              kfix1(i)=cvmgt(i,0,matrecon(imt).eq.0)
C           else
C              kfix1(i)=cvmgt(i,0,matrecon(imt).eq.1)
C           endif
C223     continue
C        call kmprsn(ntets,kfix1,1,kfix1,1,kfix1,1,nwrong)
C        if (nwrong .gt. 0) call killcode('nwrong .gt. 0')
C     endif
C
C     ******************************************************************
C
 1    continue
      if(idebug.gt.1) then
         write(logdan,9000) niters
         call writloga("bat",0,logdan,0,ierrdum)
 9000    format('  niters=',i10)
      endif
C
      n2to2=0
      n3to2=0
      n2to3=0
      n2to3i=0
      n3to2i=0
      n3to2x=0
      n4to4=0
      n4to4i=0
      n2to0=0
      n1to0=0
      nvacnt=0
      nface=0
      nface2=0
C
      n=max(1000,nint(1.7*max(nrecon,ntets)*4))
      call mmgetlen(ipkfix,len,ier)
      if (len.lt.n) then
         call mmgetnam(ipkfix,blkout,prtout,ier)
         call mmnewlen('kfix',prtout,ipkfix,n,ier)
      endif
      n=max(1000,nint(1.7*max(nrecon,ntets)*4))
      call mmgetlen(ipxfix,len,ier)
      if (len.lt.n) then
         call mmgetnam(ipxfix,blkout,prtout,ier)
         call mmnewlen('xfix',prtout,ipxfix,n,ier)
      endif
C
      do 5 i=1,nrecon*4
         xfix1(i)=xxlarge
 5    continue
C
      if(abs(ivoronoi).eq.2) then
C
C
C        ***************************************************************
C        ADD SOME SPACE ONTO THE ELEMENT SIZE ARRAY AS WORK SPACE FOR
C           THE GRADIENT WEIGHTED RECONNECTION ALGORITHM.
C
         ntetsmax=ntets*2
         ntetsmax=max(ntetsmax,100)
         call cmo_set_info('nelements',cmo,ntetsmax,1,1,icmoerr)
         call cmo_newlen(cmo,icmoerr)
         call cmo_set_info('nelements',cmo,ntets,1,1,icmoerr)
C
         call cmo_get_intinfo('mbndry',cmo,mbndry,length,icmotype,
     *      ierror)
         call cmo_get_info('isetwd',cmo,ipisetwd,length,icmotype,
     *        ierror)
         call cmo_get_info('imt1',cmo,ipimt1,length,icmotype,ierror)
         call cmo_get_info('itp1',cmo,ipitp1,length,icmotype,ierror)
         call cmo_get_info('icr1',cmo,ipicr1,length,icmotype,ierror)
         call cmo_get_info('isn1',cmo,ipisn1,length,icmotype,ierror)
         call cmo_get_info('xic',cmo,ipxic,length,icmotype,ierror)
         call cmo_get_info('yic',cmo,ipyic,length,icmotype,ierror)
         call cmo_get_info('zic',cmo,ipzic,length,icmotype,ierror)
         call cmo_get_info('icontab',cmo,ipicontab,length,icmotype,
     *        ierror)
         call cmo_get_info('itetclr',cmo,
     *                     ipitetclr,length,icmotype,ier)
         call cmo_get_info('itettyp',cmo,
     *                     ipitettyp,length,icmotype,ier)
         call cmo_get_info('itet',cmo,ipitet,length,icmotype,ierror)
         call cmo_get_info('jtet',cmo,ipjtet,length,icmotype,ierror)
 
         nsd=3
         kpe=nsd+1
         call mega_hessian()
         call mega_error()
         call mmfindbk('megah',cmo,ipxmegah,lenout,icscode)
         call mmfindbk('megadet',cmo,ipxmegadet,lenout,icscode)
         call mmfindbk('megaerr',cmo,ipxmegaerr,lenout,icscode)
C
         do it=1,ntets
            do i=1,nelmnef(ifelmtet)
               kfix(i,it)=0
            enddo
         enddo
c
C  first do 3 to 2 flips
C
         nflips=0
         do it=1,ntets
            do i=1,nelmnef(ifelmtet)
               if(jtet(i,it).lt.mbndry.and.itet(1,it).gt.0) then
                  jt=1+(jtet(i,it)-1)/4
                  jf=jtet(i,it)-4*(jt-1)
                  iface=nelmnef(ifelmtet)*(it-1)+i
                  call face(iface,ifpos,it,n1,n2,n3)
                  if(ifpos.eq.1) then
                     ipos(1)=4
                     ipos(2)=5
                     ipos(3)=6
                  elseif(ifpos.eq.2) then
                     ipos(1)=2
                     ipos(2)=3
                     ipos(3)=6
                  elseif(ifpos.eq.3) then
                     ipos(1)=1
                     ipos(2)=3
                     ipos(3)=5
                  elseif(ifpos.eq.4) then
                     ipos(1)=1
                     ipos(2)=2
                     ipos(3)=4
                  endif
                  do j=1,3
                     k1=ielist(4*ipos(j)-1)
                     k2=ielist(4*ipos(j)  )
                     if(jtet(k1,it).lt.mbndry .and.
     *                  jtet(k2,it).lt.mbndry) then
                        if(itet1(jtet(k1,it)) .eq.
     *                     itet1(jtet(k2,it))) then
                           ip=ipos(j)
                           call find3to2(it,ip,it2,it3,id(1),jd(1),flag,
     *                                   npoints,ntets)
                           if(flag.eq.1) then
                              i1=itet(k1,it)
                              i2=itet(k2,it)
                              n=3
                              ntet(1)=it
                              ntet(2)=it2
                              ntet(3)=it3
                              do k=1,n
                                 xmegadet(ntet(k))=-1.0
                                 xmegaerr(ntet(k))=-1.0
                              enddo
                              m=2
                              mtet(1)=ntets+1
                              mtet(2)=ntets+2
                              do k=1,m
                                 xmegadet(mtet(k))=-1.0
                                 xmegaerr(mtet(k))=-1.0
                              enddo
                              itet(1,mtet(1))=id(1)
                              itet(2,mtet(1))=id(2)
                              itet(3,mtet(1))=id(3)
                              itet(4,mtet(1))=id(4)
                              itet(1,mtet(2))=id(5)
                              itet(2,mtet(2))=id(6)
                              itet(3,mtet(2))=id(7)
                              itet(4,mtet(2))=id(8)
                              vol1=volume(id(1),id(2),id(3),id(4))
                              vol2=volume(id(5),id(6),id(7),id(8))
                              if(vol1.gt.0.0.and.vol2.gt.0.0) then
                                 call b3dnxm (n,ntet,en,m,mtet,em,
     *                                     npoints,nsd,xic,yic,zic,
     *                                     xmegah,
     *                                     ntets,kpe,itet,
     *                                     xmegadet,xmegaerr,
     *                                     flip)
                                 if(flip.and.(it.lt.jt)) then
                                    call flip3to2(it,it2,it3,
     *                                id(1),jd(1),npoints,ntets)
                                    nflips=nflips+1
                                    goto 17
                                 endif
                              endif
                           endif
                        endif
                     endif
                  enddo
               endif
            enddo
 17         continue
         enddo
C
C   compress out dudded elements
C
         nholes=0
         do it=1,ntets
           if(itet(1,it).lt.0) then
              nholes=nholes+1
              ivacnt(nholes)=it
           endif
         enddo
         if (nholes.gt.0)
     *   call filholes(ipivacnt,nholes,ipiopen,npoints,ntets)
         nvacnt=0
         write(logmess,18) nflips
 18      format (' number of 3to2 flips with ivoronoi=2 is: ',i9)
         call writloga('default',0,logmess,0,ier)
C
C  2 to 3 flips with ivoronoi=2 or -2
c
         nflips=0
         do it=1,ntets
            do i=1,4
               if(jtet(i,it).gt.0.and.jtet(i,it).lt.mbndry.and.
     *            itet(1,it).gt.0) then
                  i1=itet(1,it)
                  i2=itet(2,it)
                  i3=itet(3,it)
                  i4=itet(4,it)
                  isum=i1+i2+i3+i4
                  i1=itet(ielmface1(1,i,ifelmtet),it)
                  i2=itet(ielmface1(2,i,ifelmtet),it)
                  i3=itet(ielmface1(3,i,ifelmtet),it)
                  i4=isum-(i1+i2+i3)
                  jt=1+(jtet(i,it)-1)/4
                  jf=jtet(i,it)-4*(jt-1)
                  i5=itet(jf,jt)
                  n=2
                  ntet(1)=it
                  ntet(2)=jt
                  do j=1,n
                     xmegadet(ntet(j))=-1.0
                     xmegaerr(ntet(j))=-1.0
                  enddo
                  m=3
                  mtet(1)=ntets+1
                  mtet(2)=ntets+2
                  mtet(3)=ntets+3
                  do j=1,m
                     xmegadet(mtet(j))=-1.0
                     xmegaerr(mtet(j))=-1.0
                  enddo
                  itet(1,mtet(1))=i1
                  itet(2,mtet(1))=i5
                  itet(3,mtet(1))=i2
                  itet(4,mtet(1))=i4
                  itet(1,mtet(2))=i2
                  itet(2,mtet(2))=i5
                  itet(3,mtet(2))=i3
                  itet(4,mtet(2))=i4
                  itet(1,mtet(3))=i3
                  itet(2,mtet(3))=i5
                  itet(3,mtet(3))=i1
                  itet(4,mtet(3))=i4
                  vol1=volume(i1,i5,i2,i4)
                  vol2=volume(i2,i5,i3,i4)
                  vol3=volume(i3,i5,i1,i4)
                  if(vol1.gt.0.0.and.vol2.gt.0.0.and.vol3.gt.0.0) then
                     call b3dnxm (n,ntet,en,m,mtet,em,
     *                         npoints,nsd,xic,yic,zic,xmegah,
     *                         ntets,kpe,itet,xmegadet,xmegaerr,
     *                         flip)
                     if(flip.and.(it.lt.jt)) then
                        it2=jt
                        ifpos=i
                        call find2to3(it,ifpos,it2,ifpos2,it3,
     *                     id(1),jd(1),npoints,ntets,ierflg)
                        call flip2to3(it,it2,it3,id(1),jd(1),
     *                     npoints,ntets)
                        nflips=nflips+1
                     endif
                  endif
               endif
            enddo
         enddo
         write(logmess,28) nflips
 28      format (' number of 2to3 flips with ivoronoi=2 is: ',i9)
         call writloga('default',0,logmess,0,ier)
C
C        Try the 4-to-4 flip
C
         nflips=0
         do it=1,ntets
            do i=1,4
               if(kfix(1,it).ne.0.or.kfix(2,it).ne.0.or.
     *            kfix(3,it).ne.0.or.kfix(4,it).ne.0.or.
     *            itet(1,it).lt.0) go to 34
               n1=itet(iflist(3*i-2),it)
               n2=itet(iflist(3*i-1),it)
               n3=itet(iflist(3*i  ),it)
               do  j=1,3
                  i1=n1
                  i2=n2
                  if(j.eq.2) then
                     i1=n2
                     i2=n3
                  elseif(j.eq.3) then
                     i1=n3
                     i2=n1
                  endif
                  do  m=1,6
                     ip1=ielist(4*(m-1)+1)
                     ip2=ielist(4*(m-1)+2)
                     if( (itet(ip1,it).eq.i1.and.itet(ip2,it).eq.i2).or.
     *                (itet(ip1,it).eq.i2.and.itet(ip2,it).eq.i1)) then
                            iepos=m
                            goto 33
                     endif
                  enddo
 33               continue
                  call try4to4xv(iepos,it,iflag,ntet,mtet,
     *                    xmegah,xmegadet,xmegaerr,
     *                    npoints,ntets,toldamage,iopt2to2)
                  if (iflag.eq.1) then
                     nflips=nflips+1
                     go to 36
                  endif
               enddo
 34            continue
            enddo
 36         continue
         enddo
         write(logmess,38) nflips
 38      format (' number of 4to4 flips with ivoronoi=2 is: ',i9)
         call writloga('default',0,logmess,0,ier)
C  test now for 2to2 boundary flips
         nflips=0
         do it=1,ntets
            do i=1,4
               if(jtet(i,it).gt.0.and.jtet(i,it).lt.mbndry.and.
     *            kfix(i,it).eq.0.and.itet(1,it).gt.0) then
                  i1=itet(1,it)
                  i2=itet(2,it)
                  i3=itet(3,it)
                  i4=itet(4,it)
                  isum=i1+i2+i3+i4
                  i1=itet(ielmface1(1,i,ifelmtet),it)
                  i2=itet(ielmface1(2,i,ifelmtet),it)
                  i3=itet(ielmface1(3,i,ifelmtet),it)
                  i4=isum-(i1+i2+i3)
                  jt=1+(jtet(i,it)-1)/4
                  jf=jtet(i,it)-4*(jt-1)
                  if(jf.le.0.or.jf.gt.mbndry) go to 220
                  i5=itet(jf,jt)
C
C  points i4 and i5 must be boundary points
C  if so identify which two face points are on the boundayr
C  and make sure they are not interface points
C
                  if (.not.(itsttp('boundary',itp1(i4))).or.
     *                .not.(itsttp('boundary',itp1(i5)))) go to 220
                  do  mm=1,3
                     if(mm.eq.1) then
                        n1=i1
                        n2=i2
                        n3=i3
                     elseif(mm.eq.2) then
                        n1=i3
                        n2=i2
                        n3=i1
                     elseif(mm.eq.3) then
                        n1=i3
                        n2=i1
                        n3=i2
                     endif
                     if (itsttp('intrface',itp1(n1)).or.
     *               itsttp('intrface',itp1(n2)))  go to 250
C
C           ENSURE THAT FACES OPPOSITE n3 ARE EXTERNAL BOUNDARY FACES.
C
                     ipos1=4
                     if(itet(1,it).eq.n3) then
                         ipos1=1
                     elseif(itet(2,it).eq.n3) then
                         ipos1=2
                     elseif(itet(3,it).eq.n3) then
                        ipos1=3
                     endif
                     if(jtet(ipos1,it).ne.mbndry) goto 250
                     ipos2=4
                     if(itet(1,jt).eq.n3) then
                         ipos2=1
                     elseif(itet(2,jt).eq.n3) then
                         ipos2=2
                     elseif(itet(3,jt).eq.n3) then
                         ipos2=3
                     endif
                     if(jtet(ipos2,jt).ne.mbndry) goto 250
C
C           ENSURE THAT POINTS n1, n2, n4, AND n5 LIE IN SAME
C           REFLECTING BOUNDARY PLANE. Use constraint values
c           (icr1) to determine if all nodes are in the same
c           constraint class.  If any node is not constrained
c           it belongs to all classes.  If icontab does not exist
c           then all flips are allowed.
C
                     icrnbr1=icr1(n1)
                     icrnbr2=icr1(n2)
                     icrnbr4=icr1(i4)
                     icrnbr5=icr1(i5)
                     irb1=-1
                     irb2=-1
                     irb4=-1
                     irb5=-1
                     if (noicontab) go to 240
                     if(icrnbr1+icrnbr2+icrnbr4+icrnbr5.eq.0)
     *                 go to 240
                     if(icrnbr1.eq.icrnbr2.and.
     *                  icrnbr4.eq.icrnbr1.or.
     *                  icrnbr5.eq.icrnbr1) go to 240
                     if(icrnbr1.eq.0) then
                         j1max=3
                         irb1=0
                     else
                         j1max=2+icontab(1,icrnbr1)
                     endif
                     if(icrnbr2.eq.0) then
                         j2max=3
                         irb2=0
                     else
                         j2max=2+icontab(1,icrnbr2)
                     endif
                     if(icrnbr4.eq.0) then
                         j4max=3
                         irb4=0
                     else
                         j4max=2+icontab(1,icrnbr4)
                     endif
                     if(icrnbr5.eq.0) then
                         j5max=3
                         irb5=0
                     else
                         j5max=2+icontab(1,icrnbr5)
                     endif
                     do  j1=3,j1max
                        if(irb1.eq.-1)irb1=icontab(j1,icrnbr1)
                        do  j2=3,j2max
                           if(irb2.eq.-1)irb2=icontab(j2,icrnbr2)
                           if(irb1.eq.0) irb1=irb2
                           if(irb2.eq.irb1.or.irb2.eq.0) then
                              do  j4=3,j4max
                                 if(irb4.eq.-1)
     *                             irb4=icontab(j4,icrnbr4)
                                 if(irb4.eq.irb1.or.irb4.eq.0) then
                                    do  j5=3,j5max
                                       if(irb5.eq.-1)
     *                                  irb5=icontab(j5,icrnbr5)
                                       if(irb5.eq.irb1.or.irb5.eq.0)
     *                                  goto 240
                                    enddo
                                 endif
                              enddo
                           endif
                        enddo
                     enddo
c.... Check that the 'damage' of performing a flip is less than
c.... TOLDAMAGE.  The damage will be nonzero if the points n1, n2,
c.... i4, i5 do not all lie in the same plane.  The damage is defined
c.... in a similar fashion as in the subroutine AGD3D.
c.... Form aggregate normal:  Area-weighted normal formed from the
c.... two boundary triangles [triangle (n1,n2,i4) and triangle (n2,n1,i5)].
 
 240                  a124x=crosx1(n1,n2,i4)
                      a124y=crosy1(n1,n2,i4)
                      a124z=crosz1(n1,n2,i4)
                      a215x=crosx1(n2,n1,i5)
                      a215y=crosy1(n2,n1,i5)
                      a215z=crosz1(n2,n1,i5)
                      atotx=a124x+a215x
                      atoty=a124y+a215y
                      atotz=a124z+a215z
                      atot=sqrt(atotx**2+atoty**2+atotz**2)
                      atotx=atotx/atot
                      atoty=atoty/atot
                      atotz=atotz/atot
c
c.... Calculate midpoint of edge (i1,i2)
c
                      xmid=half*(xic(i1)+xic(i2))
                      ymid=half*(yic(i1)+yic(i2))
                      zmid=half*(zic(i1)+zic(i2))
c.... If both boundary triangle normals are in the same direction as
c.... aggregate normal, damage is defined to be separation between two
c.... planes (normal to agg. normal) that sandwich the points i1, i2,
c.... i3, i4.  If one of the boundary triangle normals points contrary
c.... to the agg. normal, damage is defined as minimum of 'merge
c.... distances' from the midpoint of edge (i1,i2) to the points i4, i5.
                      dot124=a124x*atotx+a124y*atoty+a124z*atotz
                      dot215=a215x*atotx+a215y*atoty+a215z*atotz
                      if (dot124.gt.zero.and.dot215.gt.zero) then
                         dot1=(xic(n1)-xmid)*atotx+(yic(n1)-ymid)*atoty+
     &                      (zic(n1)-zmid)*atotz
                         dot2=(xic(n2)-xmid)*atotx+(yic(n2)-ymid)*atoty+
     &                      (zic(n2)-zmid)*atotz
                         dot4=(xic(i4)-xmid)*atotx+(yic(i4)-ymid)*atoty+
     &                      (zic(i4)-zmid)*atotz
                         dot5=(xic(i5)-xmid)*atotx+(yic(i5)-ymid)*atoty+
     &                      (zic(i5)-zmid)*atotz
                         dotmin=min(dot1,dot2,dot4,dot5)
                         dotmax=max(dot1,dot2,dot4,dot5)
                         damage=dotmax-dotmin
                         if (damage.gt.toldamage) goto 250
                      else
                        dist4=sqrt((xic(i4)-xmid)**2+(yic(i4)-ymid)**2+
     &                     (zic(i4)-zmid)**2)
                        dist5=sqrt((xic(i5)-xmid)**2+(yic(i5)-ymid)**2+
     &                     (zic(i5)-zmid)**2)
                        damage=min(dist4,dist5)
                        if (damage.gt.toldamage) goto 250
                     endif
C
                     n=2
                     ntet(1)=it
                     ntet(2)=jt
                     do j=1,n
                       xmegadet(ntet(j))=-1.0
                       xmegaerr(ntet(j))=-1.0
                     enddo
                     m=2
                     mtet(1)=ntets+1
                     mtet(2)=ntets+2
                     do j=1,m
                       xmegadet(mtet(j))=-1.0
                       xmegaerr(mtet(j))=-1.0
                     enddo
                     itet(1,mtet(1))=n1
                     itet(2,mtet(1))=i4
                     itet(3,mtet(1))=i5
                     itet(4,mtet(1))=n3
                     itet(1,mtet(2))=n2
                     itet(2,mtet(2))=i5
                     itet(3,mtet(2))=i4
                     itet(4,mtet(2))=n3
                     vol1=volume(n1,i4,i5,n3)
                     vol2=volume(n2,i5,i4,n3)
                     if(vol1.gt.0.0.and.vol2.gt.0.0) then
                       call b3dnxm (n,ntet,en,m,mtet,em,
     *                         npoints,nsd,xic,yic,zic,xmegah,
     *                         ntets,kpe,itet,xmegadet,xmegaerr,
     *                         flip)
                       if(flip.and.(it.lt.jt)) then
C
C do the flip now
C
                         call find2to2(it,jt,n1,n2,n3,i4,i5,idnew,
     *                       jdnew,npoints,ntets)
                         call flip2to2(it,jt,idnew,jdnew,
     *                      npoints,ntets)
                         nflips=nflips+1
                         go to 220
                       endif
                    endif
 250                continue
                  enddo
               endif
 220           continue
            enddo
         enddo
         write(logmess,221) nflips
 221     format (' number of 2to2 flips with ivoronoi=2 is: ',i9)
         call writloga('default',0,logmess,0,ier)
C
C  go to return
c
         go to 9999
      elseif(nrecon.le.ntets) then
         do 10 i=1,ntets
            xa=xic(itet(1,i))
            ya=yic(itet(1,i))
            za=zic(itet(1,i))
            xb=xic(itet(2,i))-xa
            yb=yic(itet(2,i))-ya
            zb=zic(itet(2,i))-za
            xc=xic(itet(3,i))-xa
            yc=yic(itet(3,i))-ya
            zc=zic(itet(3,i))-za
            xd=xic(itet(4,i))-xa
            yd=yic(itet(4,i))-ya
            zd=zic(itet(4,i))-za
            xn=crosx(xb,yb,zb,xc,yc,zc)
            yn=crosy(xb,yb,zb,xc,yc,zc)
            zn=crosz(xb,yb,zb,xc,yc,zc)
            x2=crosx(xn,yn,zn,xb,yb,zb)
            y2=crosy(xn,yn,zn,xb,yb,zb)
            z2=crosz(xn,yn,zn,xb,yb,zb)
            q=-0.5*(xc*xb+yc*yb+zc*zb-xc*xc-yc*yc-zc*zc)/
     *             (x2*xc+y2*yc+z2*zc+xxsmall)
            xl=q*x2+0.5*xb
            yl=q*y2+0.5*yb
            zl=q*z2+0.5*zb
            d=-0.5*(xd*xd+yd*yd+zd*zd)
            q=-(xd*xl+yd*yl+zd*zl+d)/(xd*xn+yd*yn+zd*zn+xxsmall)
            vol=xn*xd+yn*yd+zn*zd
            xv=q*xn+xl+xa
            yv=q*yn+yl+ya
            zv=q*zn+zl+za
            dist=(xv-xa)**2+(yv-ya)**2+(zv-za)**2
            j1=cvmgn(jtet(1,i),1,jtet(1,i)-mbndry)
            j1=cvmgm(j1,j1-mbndry,j1-mbndry)
            xfix(1,i)=(xic(itet1(j1))-xv)**2+
     *                (yic(itet1(j1))-yv)**2+
     *                (zic(itet1(j1))-zv)**2
            j1=cvmgn(jtet(2,i),1,jtet(2,i)-mbndry)
            j1=cvmgm(j1,j1-mbndry,j1-mbndry)
            xfix(2,i)=(xic(itet1(j1))-xv)**2+
     *                (yic(itet1(j1))-yv)**2+
     *                (zic(itet1(j1))-zv)**2
            j1=cvmgn(jtet(3,i),1,jtet(3,i)-mbndry)
            j1=cvmgm(j1,j1-mbndry,j1-mbndry)
            xfix(3,i)=(xic(itet1(j1))-xv)**2+
     *                (yic(itet1(j1))-yv)**2+
     *                (zic(itet1(j1))-zv)**2
            j1=cvmgn(jtet(4,i),1,jtet(4,i)-mbndry)
            j1=cvmgm(j1,j1-mbndry,j1-mbndry)
            xfix(4,i)=(xic(itet1(j1))-xv)**2+
     *                (yic(itet1(j1))-yv)**2+
     *                (zic(itet1(j1))-zv)**2
            ifc=4*(i-1)
            kfix(1,i)=cvmgpr(ifc+1,0,dist-xfix(1,i)-xst2*dist)
            kfix(2,i)=cvmgpr(ifc+2,0,dist-xfix(2,i)-xst2*dist)
            kfix(3,i)=cvmgpr(ifc+3,0,dist-xfix(3,i)-xst2*dist)
            kfix(4,i)=cvmgpr(ifc+4,0,dist-xfix(4,i)-xst2*dist)
            kfix(1,i)=cvmgmr(kfix(1,i),-i,-vol)
            do j=1,4
               if(jtet(j,i).gt.0.and.jtet(j,i).lt.mbndry) then
                  j1=jtet(j,i)
               elseif(jtet(j,i).gt.mbndry) then
                  j1=jtet(j,i)-mbndry
               else
                  j1=1
               endif
               if(j1.gt.0) then
                  xfix(j,i)=(xic(itet1(j1))-xv)**2+
     *                      (yic(itet1(j1))-yv)**2+
     *                      (zic(itet1(j1))-zv)**2
               endif
               kfix(j,i)=cvmgpr(4*(i-1)+j,0,dist-xfix(j,i)-xst2*dist)
            enddo
            kfix(1,i)=cvmgmr(kfix(1,i),-i,-vol)
 10      continue
      else
         do 11 i=1,nrecon
            it=min0(irclst(i),ntets)
            xa=xic(itet(1,it))
            ya=yic(itet(1,it))
            za=zic(itet(1,it))
            xb=xic(itet(2,it))-xa
            yb=yic(itet(2,it))-ya
            zb=zic(itet(2,it))-za
            xc=xic(itet(3,it))-xa
            yc=yic(itet(3,it))-ya
            zc=zic(itet(3,it))-za
            xd=xic(itet(4,it))-xa
            yd=yic(itet(4,it))-ya
            zd=zic(itet(4,it))-za
            xn=crosx(xb,yb,zb,xc,yc,zc)
            yn=crosy(xb,yb,zb,xc,yc,zc)
            zn=crosz(xb,yb,zb,xc,yc,zc)
            x2=crosx(xn,yn,zn,xb,yb,zb)
            y2=crosy(xn,yn,zn,xb,yb,zb)
            z2=crosz(xn,yn,zn,xb,yb,zb)
            q=-0.5*(xc*xb+yc*yb+zc*zb-xc*xc-yc*yc-zc*zc)/
     *             (x2*xc+y2*yc+z2*zc+xxsmall)
            xl=q*x2+0.5*xb
            yl=q*y2+0.5*yb
            zl=q*z2+0.5*zb
            d=-0.5*(xd*xd+yd*yd+zd*zd)
            q=-(xd*xl+yd*yl+zd*zl+d)/(xd*xn+yd*yn+zd*zn+xxsmall)
            vol=xn*xd+yn*yd+zn*zd
            xv=q*xn+xl+xa
            yv=q*yn+yl+ya
            zv=q*zn+zl+za
            dist=(xv-xa)**2+(yv-ya)**2+(zv-za)**2
            do j=1,4
               if(jtet(j,it).gt.0.and.jtet(j,it).lt.mbndry) then
                  j1=jtet(j,it)
               elseif(jtet(j,it).gt.mbndry) then
                  j1=jtet(j,it)-mbndry
               else
                  j1=1
               endif
               if(j1.gt.0) then
                  xfix(j,i)=(xic(itet1(j1))-xv)**2+
     *                      (yic(itet1(j1))-yv)**2+
     *                      (zic(itet1(j1))-zv)**2
               endif
               kfix(j,i)=cvmgpr(4*(it-1)+j,0,dist-xfix(j,i)-xst2*dist)
            enddo
            kfix(1,i)=cvmgmr(kfix(1,i),-it,-vol)
 11      continue
      endif
      call kmprsn(ntets*4,kfix1,1,kfix1,1,kfix1,1,nface1)
      if(nface1.eq.0.and.niters.eq.0) goto 9998
C
C     ******************************************************************
C
C     MAKE SURE MEMORY IS ADEQUATE
C
      if(lenremov.lt.4) then

C        check to see if code ever gets here
C        original code would do nothing if this was called
         lenremov=4
         call mflip(ione,lenremov,'iremov')
         write (logmess,'(a)') 
     *   'REPORT RECON2: MFLIP NEWLEN FOR iremov'
         call writloga('default', 0, logdan, 0, ierr)
      endif
      if(leniopen.lt.nface1) then
         leniopen=nface1+100
         call mflip(ione,leniopen,'iopen')
      endif
      do 15 i=1,nface1
         lst(i)=kfix1(i)
 15   continue
C
C     ******************************************************************
C
C     EXTRACT THE NEGATIVE VOLUME TETS FROM THE LIST AND:
C      1) PERFORM ALL POSSIBLE 1-TO-0 FLIPS.
C      2) ASSIGN THE FACES OPPOSITE THE NEGATIVE VOLUME TETS
C          TO THE RECONNECTION LIST IFF THE OPPOSITE TETS HAVE + VOLUME.
C
      call kmprsm(nface1,lst,1,lst,1,kfix(1,1),4,nvts)
      if(nvts.gt.0) then
         nntet2=nvts
         if(iopt2to2.eq.3) then
            do 1065 i=1,nvts
               it=-kfix(1,i)
               ict=0
               if(jtet(1,it).ge.mbndry) ict=ict+1
               if(jtet(2,it).ge.mbndry) ict=ict+1
               if(jtet(3,it).ge.mbndry) ict=ict+1
               if(jtet(4,it).ge.mbndry) ict=ict+1
               if(ict.eq.2) then
                  call flip1to0(it,
     *                          npoints,ntets)
                  n1to0=n1to0+1
                  kfix(1,i)=0
               endif
 1065       continue
            call kmprsn(nvts,kfix(1,1),4,kfix(1,1),4,kfix(1,1),4,nntet2)
         endif
         do 1070 i=1,nntet2
            it=-kfix(1,i)
            j1=cvmgm(jtet(1,it),1,jtet(1,it)-mbndry)
            j2=cvmgm(jtet(2,it),1,jtet(2,it)-mbndry)
            j3=cvmgm(jtet(3,it),1,jtet(3,it)-mbndry)
            j4=cvmgm(jtet(4,it),1,jtet(4,it)-mbndry)
            it1=0.25*dble(j1)+0.9
            it2=0.25*dble(j2)+0.9
            it3=0.25*dble(j3)+0.9
            it4=0.25*dble(j4)+0.9
            vol1=volume(itet(1,it1),itet(2,it1),itet(3,it1),itet(4,it1))
            vol2=volume(itet(1,it2),itet(2,it2),itet(3,it2),itet(4,it2))
            vol3=volume(itet(1,it3),itet(2,it3),itet(3,it3),itet(4,it3))
            vol4=volume(itet(1,it4),itet(2,it4),itet(3,it4),itet(4,it4))
            xtest=max(dble(jtet(1,it)-mbndry),-(vol1))
            kfix(1,i)=cvmgmr(j1,0,xtest)
            xtest=max(dble(jtet(2,it)-mbndry),-(vol2))
            kfix(2,i)=cvmgmr(j2,0,xtest)
            xtest=max(dble(jtet(3,it)-mbndry),-(vol3))
            kfix(3,i)=cvmgmr(j3,0,xtest)
            xtest=max(dble(jtet(4,it)-mbndry),-(vol4))
            kfix(4,i)=cvmgmr(j4,0,xtest)
 1070    continue
         num=nntet2*4
         call kmprsn(num,kfix1,1,kfix1,1,kfix1,1,nface2)
         call kmprsp(nface1,lst,1,lst,1,lst,1,nface0)
         if(leniopen.lt.nface0+nface2) then
            leniopen=nface0+nface2+100
            call mflip(ione,leniopen,'iopen')
         endif
         do 1080 i=1,nface0
            kfix1(i+nface2)=lst(i)
 1080    continue
         nface1=nface0+nface2
         do 1090 i=1,nface1
            lst(i)=kfix1(i)
 1090    continue
      endif
C
C     ******************************************************************
C
C     REMOVE FACES IN THE LIST THAT WOULD OTHERWISE PRODUCE
C     DUPLICATE TET PAIRS.  PUT BOUNDARY FACES IN A SEPARATE LIST.
C
      do 20 i=1,nface1
         xfix1(lst(i))=0.0
 20   continue
      do 25 i=1,nvacnt
         ifc=4*(ivacnt(i)-1)
         xfix1(ifc+1)=1.0
         xfix1(ifc+2)=1.0
         xfix1(ifc+3)=1.0
         xfix1(ifc+4)=1.0
 25   continue
      nface=0
      nbdyfc=0
      do 30 i=1,nface1
         ifc=lst(i)
         it=0.25*dble(ifc)+0.9
         if(jtet1(ifc).eq.mbndry.or.itet(1,it).le.0) goto 30
         if(xfix1(ifc).eq.0.0) then
            if (jtet1(ifc) .lt. mbndry) then
               nface=nface+1
               lst(nface)=ifc
               xfix1(ifc)=1.0
               xfix1(jtet1(ifc))=1.0
            else
               jtemp=jtet1(ifc)-mbndry
               nbdyfc=nbdyfc+1
               if(lentmp2.lt.nbdyfc) then
                  lentmp2=nbdyfc+200
                  call mflip(ione,lentmp2,'tmp2')
               endif
               itmp2(nbdyfc)=ifc
               xfix1(ifc)=1.0
               xfix1(jtet1(ifc)-mbndry)=1.0
            endif
         endif
 30   continue
      if(nface.eq.0.and.niters.eq.0.and.nvacnt.eq.0) goto 9998
C
C     ******************************************************************
C
C     LOAD ALL FACES THAT CONTAIN ONE EDGE ON A BOUNDARY INTO THE
C     "irclst" ARRAY.
C
      do 35 i=1,nface
         xfix1tst=-1.0
         xfix2tst=-1.0
         xfix3tst=-1.0
         ip=iand((lst(i)-1),maskface)+1
         it=0.25*dble(lst(i))+0.9
         i1=itet(iflist(3*ip-2),it)
         i2=itet(iflist(3*ip-1),it)
         i3=itet(iflist(3*ip  ),it)
         if(itp1(i1).ge.1) xfix1tst=1.0
         if(itp1(i2).ge.1) xfix2tst=1.0
         if(itp1(i3).ge.1) xfix3tst=1.0
         xone=1.0
         kfix1(i)=dble(lst(i))*sign(xone,xfix1tst+xfix2tst+xfix3tst)
 35   continue
      call kmprsp(nface,kfix1(1),1,kfix1(1),1,kfix1,1,nbfaces)
      if(lenrclst.le.nbfaces) then
         lenrclst=nbfaces+100
         call mflip(ione,lenrclst,'irclst')
      endif
      do 37 i=1,nbfaces
         irclst(i)=kfix1(i)
 37   continue
C
C     ******************************************************************
C
C     BEGIN THE RECONNECTION PROCESS
C
      do 40 i=1,ntets
         kfix(1,i)=0
 40   continue
C
C     __________________________________________________________________
C
C     FLIP BOUNDARY CONNECTIONS
C
 
      if (nbfaces.ge.1) then
         if (iopt2to2.eq.0.or.iopt2to2.eq.2) then
            if (nconbnd.gt.0) then
               call try2to2r(nbfaces,n2to2,
     *                       npoints,ntets,toldamage)
            endif
         elseif (iopt2to2 .eq. 3) then
            call try2to2b(nbfaces,n2to2,
     *                    npoints,ntets)
         endif
      endif
C
 
      if (nbdyfc.ge.1) then
C        isumo=n3to2i+n4to4i+n2to3i+n2to3+n2to0
         if (iopt2to2.eq.1.or.iopt2to2.eq.2) then
            call flipbdyc(nbdyfc,n3to2i,n4to4i,n2to3i,n2to3,n2to0,
     *                    npoints,ntets,toldamage)
            if(idebug.ge.1) then
               write(logdan,"(' nbdyfc ',i10)")nbdyfc
               call writloga('default', 0, logdan, 0, ierr)
            endif
         endif
C
C     TEMPORARY DEBUG SECTION
C
C           isumn=n3to2i+n4to4i+n2to3i+n2to3+n2to0
C           if (isumo.ne.isumn) then
C              write (logdan,3542) ihcycle
C3542          format ('FLIPBDYC FLIPPED!  cycle=',i8)
C              call writloga('default', 0, logdan, 0, ierr)
C           endif
C
C
      endif
 
C
C     __________________________________________________________________
C
C     FLIP INTERIOR CONNECTIONS
C
      do  i=1,nface
         iface=lst(i)
         if(jtet1(iface).ge.mbndry) goto 100
         it=0.25*dble(iface)+0.9
         itx=0.25*dble(jtet1(iface))+0.9
         if (kfix(1,it).ne.0.or.kfix(1,itx).ne.0) go to 100
         if(itet(1,it).le.0.or.itet(1,itx).le.0) goto 100
C
C        ...............................................................
C        TRY THE 2-TO-0 FLIP.
C
         itets(1)=it
         itets(2)=itx
         call try2to0(itets,itwo,nflips,itv,
     *                npoints,ntets)
         if(nflips.ge.1) then
 
            n2to0=n2to0+nflips
            goto 100
         endif
C
         if(kfix(1,it).gt.0.or.kfix(1,itx).gt.0) goto 100
         volit=volume(itet(1,it),itet(2,it),itet(3,it),itet(4,it))
         if(volit.le.0.0) goto 100
C
C        ...............................................................
C        Remove any edges with exactly three adjacent tetrahedra
C
         call face(iface,ifpos,it,n1,n2,n3)
         if(ifpos.eq.1) then
            ipos(1)=4
            ipos(2)=5
            ipos(3)=6
         elseif(ifpos.eq.2) then
            ipos(1)=2
            ipos(2)=3
            ipos(3)=6
         elseif(ifpos.eq.3) then
            ipos(1)=1
            ipos(2)=3
            ipos(3)=5
         elseif(ifpos.eq.4) then
            ipos(1)=1
            ipos(2)=2
            ipos(3)=4
         endif
         do 50 j=1,3
            k1=ielist(4*ipos(j)-1)
            k2=ielist(4*ipos(j)  )
            if(jtet(k1,it).ge.mbndry.or.jtet(k2,it).ge.mbndry) goto 50
            if(itet1(jtet(k1,it)).eq.itet1(jtet(k2,it))) then
               ip=ipos(j)
               call find3to2(it,ip,it2,it3,id(1),jd(1),flag,
     *                       npoints,ntets)
C
               if(kfix(1,it2).ne.0.or.kfix(1,it3).ne.0) go to 50
               itets(1)=it3
               call try2to0(itets,ione,nflips,ivt,
     *                      npoints,ntets)
               if(nflips.eq.1) then
                  n2to0=n2to0+1
                  goto 100
               endif
C
               if(flag.eq.0.0) goto 50
C
               np1=itet(ielist(4*ipos(j)-3),it)
               np2=itet(ielist(4*ipos(j)-2),it)
               do 910 j0=1,6
                  np3=itet(ielist(4*j0-3),it2)
                  np4=itet(ielist(4*j0-2),it2)
                  if((np3.eq.np1.and.np4.eq.np2).or.
     *               (np4.eq.np1.and.np3.eq.np2)) nppos=j0
 910           continue
               k10=ielist(4*nppos-1)
               k20=ielist(4*nppos  )
               if(jtet(k10,it2).ge.mbndry.or.
     *            jtet(k20,it2).ge.mbndry) goto 50
               if(itet1(jtet(k10,it2)).ne.itet1(jtet(k20,it2))) goto 100
C*****                  write(logdan,1020)
C*****                  call writloga('default',2,logdan,2,messerr)
C*****                  goto 100
C*****               endif
               do 920 j0=1,6
                  np3=itet(ielist(4*j0-3),it3)
                  np4=itet(ielist(4*j0-2),it3)
                  if((np3.eq.np1.and.np4.eq.np2).or.
     *               (np4.eq.np1.and.np3.eq.np2)) nppos=j0
 920           continue
               k10=ielist(4*nppos-1)
               k20=ielist(4*nppos  )
               if(jtet(k10,it3).ge.mbndry.or.
     *            jtet(k20,it3).ge.mbndry) goto 50
               if(itet1(jtet(k10,it3)).ne.itet1(jtet(k20,it3))) goto 100
C*****                  write(logdan,1020)
C***** 1020             format(' Interesting 3-to-2 flip')
C*****                  call writloga('default',0,logdan,1,messerr)
C*****                  goto 100
C*****               endif
               kfix(1,it)=it
               kfix(1,it2)=it2
               kfix(1,it3)=it3
               call flip3to2(it,it2,it3,id(1),jd(1),
     *                       npoints,ntets)
               n3to2=n3to2+1
               if(idebug.ge.1) then
                  write(logdan,'(a6,3i10)')'n3to2 ',it,it2,it3
                  call writloga('default',0,logdan,1,ierr)
               endif
               call mmfindbk('kfix',isubname,ipkfix,lenfix,icscode)
               n=max(1000,nint(1.7*ntets))
               if (lenfix.lt.n) then
                  call mmnewlen(
     *            'kfix',isubname,ipkfix,n,icscode)
                  call mmnewlen(
     *            'xfix',isubname,ipxfix,n,icscode)
               endif
               goto 100
            endif
 50      continue
C
C        ...............................................................
C        No edges were removed so now we attempt the 2-to-3 flip
C
         n4=itet1(iface)
         if (jtet1(iface).ge.mbndry) then
            igo=0
         else
            n5=itet1(jtet1(iface))
            call face(iface,ifpos,it,n1,n2,n3)
            call test2to3(n1,n2,n3,n4,n5,igo,
     *                    npoints,ntets)
         endif
         if(igo.eq.1) then
             if(i.gt.nface2) then
               do 750 j=1,nface2
                  jf1=lst(j)
                  if(jtet1(jf1).ge.mbndry) goto 750
                  jf2=jtet1(jf1)
                  call face(jf2,idum,itt,np1,np2,np3)
                  if(itet(1,itt).le.0) goto 750
                  if((np1.eq.n4.and.np2.eq.n5).or.
     *               (np1.eq.n5.and.np2.eq.n4).or.
     *               (np2.eq.n4.and.np3.eq.n5).or.
     *               (np2.eq.n5.and.np3.eq.n4).or.
     *               (np3.eq.n4.and.np1.eq.n5).or.
     *               (np3.eq.n5.and.np1.eq.n4)) goto 100
 750           continue
            endif
            if (iopt2to2.eq.1.or.iopt2to2.eq.2) then
C
C              ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
C              Check if there is an interface connection which needs
C              to be flipped with a 2-to-3i flip.
C
               itpgsum=itp1(n4)+itp1(n5)
               if (itpgsum.eq.4 .or. itpgsum.eq.14) then
                  volitx=volume(itet(1,itx),itet(2,itx),itet(3,itx),
     *                          itet(4,itx))
                  if (volitx .gt. 0) then
                     call try2to3i(it,ifpos,it2,ibdytet,iflip,
     *                             npoints,ntets)
                     if (ibdytet.eq.1) then
                        call testdamage(n3,n4,n1,n5,iflg,toldamage)
                     elseif (ibdytet.eq.2) then
                        call testdamage(n3,n4,n2,n5,iflg,toldamage)
                     elseif (ibdytet.eq.3) then
                        call testdamage(n2,n4,n1,n5,iflg,toldamage)
                     endif
 
                     if ((iflip .eq. 1).and.(iflg.eq.1)) then
                        call fnd2to3i(it,ifpos,it2,it3,ibdytet,id,jd,
     *                                npoints,ntets)
                        iofs=4*(ibdytet-1)
C
C                       ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
C                       CONSIDER THE 2-D VORONOI TEST.
C
                        call voron2d(id(iofs+1),id(iofs+4),id(iofs+2),
     *                               id(iofs+3),ichoice1)
                        if (ichoice1 .eq. 1) goto 60
                        call voron2d(id(iofs+2),id(iofs+3),id(iofs+1),
     *                               id(iofs+4),ichoice2)
                        if (ichoice2 .eq. 1) goto 60
                        call flp2to3i(it,it2,it3,ibdytet,id,jd,
     *                                npoints,ntets)
                        if(idebug.ge.1) then
                           write(logdan,'(a6,3i10)')'n2to3i ',
     *                                   it,it2,it3
                           call writloga('default',0,logdan,1,ierr)
                        endif
                        n2to3i=n2to3i+1
                        kfix(1,it)=it
                        kfix(1,it2)=it2
                        kfix(1,it3)=it3
                        goto 70
                     endif
                  endif
               endif
            endif
 60         continue
            call find2to3(it,ifpos,it2,ifpos2,it3,id(1),jd(1),
     *                    npoints,ntets,ierflg)
            if(ierflg.eq.0.and.kfix(1,it2).eq.0) then
               call flip2to3(it,it2,it3,id(1),jd(1),
     *                    npoints,ntets)
               kfix(1,it)=it
               kfix(1,it2)=it2
               kfix(1,it3)=it3
               n2to3=n2to3+1
               if(idebug.ge.1) then
                  write(logdan,'(a6,3i10)')'n2to3 ',it,it2,it3
                  call writloga('default',0,logdan,1,ierr)
               endif
            else
               igo=0
            endif
         endif
C
C        ...............................................................
C        If a 2-to-3 flip was performed then we test for a 3-to-2 flip
C        on the three edges that previously made up the now-removed face
C
 70      if(igo.eq.1) then
            itets(1)=it
            itets(2)=it2
            itets(3)=ntets
            call try2to0(itets,ithree,nflips,itv,
     *                   npoints,ntets)
            if(nflips.ge.1) then
 
               n2to0=n2to0+nflips
               goto 100
            endif
            do 80 j=1,3
               jt=itets(j)
               if(jtet(3,jt).ge.mbndry.or.jtet(4,jt).ge.mbndry) goto 80
               voljt=volume(itet(1,jt),itet(2,jt),itet(3,jt),itet(4,jt))
               if(voljt.le.0) goto 80
               if(itet1(jtet(3,jt)).eq.itet1(jtet(4,jt))) then
                  jpos=1
                  call find3to2(jt,jpos,it2,it3,id(1),jd(1),flag,
     *                          npoints,ntets)
                  if(flag.eq.0) go to 81
                  itets(1)=it2
                  itets(2)=it3
                  call try2to0(itets,itwo,nflips,itv,
     *                         npoints,ntets)
                  if(nflips.ge.1) then
                     n2to0=n2to0+nflips
                  if(idebug.ge.1) then
                     write(logdan,'(a6,3i10)')'n2to0 ',it,it2,it3
                     call writloga('default',0,logdan,1,ierr)
                  endif
                     goto 100
                  endif
C
                  if(kfix(1,it2).ge.0.or.kfix(1,it3).ge.0) goto 80
                  if(flag.eq.0.0) goto 80
                  call vorpoint(id(1),id(2),id(3),id(4),xv,yv,zv,distsq)
                  test1=distsq-(xic(id(7))-xv)**2-(yic(id(7))-yv)**2-
     *                                            (zic(id(7))-zv)**2
                  if(test1.lt.0) then
                     kfix(1,jt)=jt
                     kfix(1,it2)=it2
                     kfix(1,it3)=it3
                     call flip3to2(jt,it2,it3,id(1),jd(1),
     *                             npoints,ntets)
                     n3to2x=n3to2x+1
                     if(idebug.ge.1) then
                        write(logdan,'(a6,3i10)')'n3to2x ',jt,it2,it3
                        call writloga('default',0,logdan,1,ierr)
                     endif
                     goto 100
                  endif
               endif
 80         continue
            goto 100
         endif
 81      continue
C
C        ...............................................................
C        Try the 4-to-4 flip
C
         do 85 j=1,3
            i1=n1
            i2=n2
            if(j.eq.2) then
               i1=n2
               i2=n3
            elseif(j.eq.3) then
               i1=n3
               i2=n1
            endif
            do 83 m=1,6
               ip1=ielist(4*(m-1)+1)
               ip2=ielist(4*(m-1)+2)
               if( (itet(ip1,it).eq.i1.and.itet(ip2,it).eq.i2).or.
     *             (itet(ip1,it).eq.i2.and.itet(ip2,it).eq.i1)) then
                      iepos=m
                      goto 84
               endif
 83         continue
 84         continue
            call try4to4x(iepos,it,iflag,nface2,i,
     *                    npoints,ntets)
            if(iflag.eq.1) then
               n4to4=n4to4+1
               if(idebug.ge.1) then
                  write(logdan,'(a6,3i10)')'n4to4 ',it,it2,it3
                  call writloga('default',0,logdan,1,ierr)
               endif
               goto 100
            endif
 85      continue
  100    continue
         if (idebug.ge.2) then
            do m=1,ntets
              if(itet(1,m).gt.0) then
              xvoltet=volume(itet(1,m),itet(2,m),itet(3,m),itet(4,m))
              if (xvoltet.le.0.0) then
                 write(logmess,86) m,(itet(j,m),j=1,4),xvoltet
 86              format(5i10,e14.8)
              endif
              endif
            enddo
          endif
 
      enddo
C
C     *****************************************************************
C     COMPRESS THE "itet" AND "jtet" LISTS IF "nvacnt".GE.1
C
      if(nvacnt.ge.1) then
         ipiholes=ipivacnt
         nholes=nvacnt
         if(leniopen.lt.nvacnt) then
            leniopen=nvacnt
            call mflip(ione,leniopen,'iopen')
         endif
         call filholes(ipiholes,nholes,ipiopen,npoints,ntets)
         nvacnt=0
      endif
C
C     ******************************************************************
C
C     DO ANOTHER ITERATION IF THE MESH HAS NOT CONVERGED
C
      isum=n1to0+n2to2+n3to2+n2to3+n2to3i+n3to2x+n3to2i+n4to4+n4to4i
C
      if(idebug.ge.1) then
         write(logmess,'(a,i10)') 'niters: ',niters
            call writloga('default',0,logmess,0,ierrwrt)
         write(logmess,'(a,i10)') 'n22: ',n2to2
            call writloga('default',0,logmess,0,ierrwrt)
         write(logmess,'(a,i10)') 'n32: ',n3to2
            call writloga('default',0,logmess,0,ierrwrt)
         write(logmess,'(a,i10)') 'n23: ',n2to3
            call writloga('default',0,logmess,0,ierrwrt)
         write(logmess,'(a,i10)') 'n23i: ',n2to3i
            call writloga('default',0,logmess,0,ierrwrt)
         write(logmess,'(a,i10)') 'n32i: ',n3to2i
            call writloga('default',0,logmess,0,ierrwrt)
         write(logmess,'(a,i10)') 'n32x: ',n3to2x
            call writloga('default',0,logmess,0,ierrwrt)
         write(logmess,'(a,i10)') 'n44: ',n4to4
            call writloga('default',0,logmess,0,ierrwrt)
         write(logmess,'(a,i10)') 'n44i: ',n4to4i
            call writloga('default',0,logmess,0,ierrwrt)
         write(logmess,'(a,i10)') 'n20r: ',n2to0r
            call writloga('default',0,logmess,0,ierrwrt)
         write(logmess,'(a,i10)') 'n20b: ',n2to0b
            call writloga('default',0,logmess,0,ierrwrt)
         write(logmess,'(a,i10)') 'n10: ',n1to0
            call writloga('default',0,logmess,0,ierrwrt)
         write(logmess,1002) ntets,nface,isum,niters,
     *                       nnegvol,(t2-t1)/dble(npoints)
 1002       format('   recon2 - ',5i10,'# ',1pe10.3)
         call writloga('default',0,logmess,0,ierrwrt)
      endif
C
      if (isum. ge. 1) then
         n22=n22+n2to2
         n32=n32+n3to2
         n23=n23+n2to3
         n23i=n23i+n2to3i
         n32i=n32i+n3to2i
         n32x=n32x+n3to2x
         n44=n44+n4to4
         n44i=n44i+n4to4i
         n20=n20+n2to0
C        do 110 i=1,nface
C           it=0.25*dble(lst(i))+0.9
C           kfix(1,it)=it
C110     continue
         call kmprsn(ntets,kfix(1,1),4,kfix(1,1),4,
     *               kfix(1,1),4,nrecon)
         if(lenrclst.le.nrecon) then
            lenrclst=nrecon+1000
            call mflip(ione,lenrclst,'irclst')
         endif
         do 120 i=1,nrecon
            irclst(i)=kfix(1,i)
 120     continue
         do 122 i=1,ntets
            kfix(1,i)=0
 122     continue
C
C        ...............................................................
C        ADD FACE NEIGHBORS TO THE RECONNECTION LIST.
C
         do 124 i=1,nrecon
            it=irclst(i)
            kfix(1,it)=it
            if (jtet(1,it) .lt. mbndry) then
               itt=0.25*dble(jtet(1,it))+0.9
               kfix(1,itt)=itt
            endif
            if (jtet(2,it) .lt. mbndry) then
               itt=0.25*dble(jtet(2,it))+0.9
               kfix(1,itt)=itt
            endif
            if (jtet(3,it) .lt. mbndry) then
               itt=0.25*dble(jtet(3,it))+0.9
               kfix(1,itt)=itt
            endif
            if (jtet(4,it) .lt. mbndry) then
               itt=0.25*dble(jtet(4,it))+0.9
               kfix(1,itt)=itt
            endif
 124     continue
         call kmprsn(ntets,kfix(1,1),4,kfix(1,1),4,
     *               kfix(1,1),4,nrecon)
         if(lenrclst.le.nrecon) then
            lenrclst=nrecon+1000
            call mflip(ione,lenrclst,'irclst')
         endif
         do 126 i=1,nrecon
            irclst(i)=kfix(1,i)
 126     continue
         niters=niters+1
         if(niters.lt.31) goto 1
      endif
C
C     ******************************************************************
C
C     MAKE SURE THE jtet ARRAY IS CONSISTENT
C
c     if (mod(ihcycle,monitor) .eq. 0) then
         call cmo_set_info('nelements',cmo,ntets,1,1,ierror)
         call tettestd()
 
c     endif
C
C     ******************************************************************
C
 9998 continue
C
c$$$      iunit=-1
c$$$      call hassign(iunit,'reconlist',ierror)
c$$$      write(iunit,*) nface
c$$$      if(nface.gt.0) then
c$$$         do j=1,nface
c$$$            ifc=lst(j)
c$$$            it=0.25*dble(ifc)+0.9
c$$$            i=ifc-4*(it-1)
c$$$            i1=itet(ielmface1(1,i,ifelmtet),it)
c$$$            i2=itet(ielmface1(2,i,ifelmtet),it)
c$$$            i3=itet(ielmface1(3,i,ifelmtet),it)
c$$$            write(iunit,*) j,it,i,i1,i2,i3
c$$$         enddo
c$$$      endif
c$$$      close(iunit)
C
C     ******************************************************************
C     TEST FOR NEGATIVE AND ZERO-VOLUME TETS.
C
      nzerovol=0
      if (nconbnd .gt. 0) then
         do 150 i=1,ntets
            xvoltet=volume(itet(1,i),itet(2,i),itet(3,i),itet(4,i))
            kfix(2,i)=cvmgmr(i,0,xvoltet)
            kfix(3,i)=cvmgzr(i,0,xvoltet)
 150     continue
         call kmprsn(ntets,kfix(2,1),4,kfix(2,1),4,merglst2(1,1),2,
     *               npairs2)
         call kmprsn(ntets,kfix(3,1),4,kfix(3,1),4,kfix(3,1),4,
     *               nzerovol)
      else
         do 152 i=1,ntets
            xvoltet=volume(itet(1,i),itet(2,i),itet(3,i),itet(4,i))
            kfix(2,i)=cvmgmr(i,0,xvoltet)
 152     continue
         call kmprsn(ntets,kfix(2,1),4,kfix(2,1),4,merglst2(1,1),2,
     *               npairs2)
      endif
      nnegvol=npairs2
      if (npairs2.ge.maxmerg) call termcode(1)
C
C     ..................................................................
C     PEEL OFF ZERO-VOLUME TETS ON THE EXTERNAL BOUNDARIES.
C
      if (nzerovol .ge. 1) then
         npeeloff=0
         ntry=0
 160     continue
         ntry=ntry+1
         nvacnto=nvacnt
         do 165 i=1,nzerovol
            it=kfix(3,i)
            if (itet(1,it) .le. 0) goto 165
            ict=0
            do 155 j=1,4
               itets(j)=0
               if (jtet(j,it) .eq. mbndry) then
                  ict=ict+1
                  itets(j)=1
               endif
 155        continue
            if (ict .ge. 2) then
C
C              ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
C              PEEL OFF ZERO-VOLUME TETS WITH TWO OR MORE EXTERNAL
C              BOUNDARY FACES.
C
               do 157 j=1,4
                  if (itets(j) .eq. 0) then
                     jtemp=jtet(j,it)
                     if (jtemp .gt. mbndry) jtemp=jtemp-mbndry
                     jtet1(jtemp)=mbndry
                  endif
 157           continue
               kfix(3,i)=0
               nvacnt=nvacnt+1
               if (nvacnt .ge. lenvacnt) then
                  lenvacnt=nvacnt+100
                  call mflip(ione,lenvacnt,'ivacnt')
               endif
               ivacnt(nvacnt)=it
               itet(1,it)=-itet(1,it)
               npeeloff=npeeloff+1
            elseif (ict .eq. 1) then
C
C              ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
C              PEEL OFF A ZERO-VOLUME TET WITH ONLY ONE BOUNDARY FACE
C              ONLY IF THE POINT OPPOSITE THAT FACE LIES WITHIN THE
C              TRIANGULAR BOUNDARY FACE.
C
               index=1
               if (itets(2) .eq. 1) then
                  index=2
               elseif (itets(3) .eq. 1) then
                  index=3
               elseif (itets(4) .eq. 1) then
                  index=4
               endif
               n1=itet(iflist(3*index-2),it)
               n2=itet(iflist(3*index-1),it)
               n3=itet(iflist(3*index  ),it)
               n4=itet(index,it)
               xn=crosx1(n1,n3,n2)
               yn=crosy1(n1,n3,n2)
               zn=crosz1(n1,n3,n2)
               coordmax=max(abs(xn),abs(yn),abs(zn))
               if (abs(xn) .eq. coordmax) then
                  x1=crosx1(n1,n4,n2)
                  cksign1=x1*xn
                  x2=crosx1(n3,n4,n1)
                  cksign2=x2*xn
                  x3=crosx1(n2,n4,n3)
                  cksign3=x3*xn
               elseif (abs(yn) .eq. coordmax) then
                  y1=crosy1(n1,n4,n2)
                  cksign1=y1*yn
                  y2=crosy1(n3,n4,n1)
                  cksign2=y2*yn
                  y3=crosy1(n2,n4,n3)
                  cksign3=y3*yn
               else
                  z1=crosz1(n1,n4,n2)
                  cksign1=z1*zn
                  z2=crosz1(n3,n4,n1)
                  cksign2=z2*zn
                  z3=crosz1(n2,n4,n3)
                  cksign3=z3*zn
               endif
               xone=1.0d+00
               icksign1=sign(xone,cksign1)
               icksign2=sign(xone,cksign2)
               icksign3=sign(xone,cksign3)
               icksign=ior(ior(icksign1,icksign2),icksign3)
               if (icksign .gt. 0) then
                  do 159 j=1,4
                     if (itets(j) .eq. 0) then
                        jtemp=jtet(j,it)
                        if (jtemp .gt. mbndry) jtemp=jtemp-mbndry
                        jtet1(jtemp)=mbndry
                     endif
 159              continue
                  kfix(3,i)=0
                  nvacnt=nvacnt+1
                  if (nvacnt .ge. lenvacnt) then
                     lenvacnt=nvacnt+100
                     call mflip(ione,lenvacnt,'ivacnt')
                  endif
                  ivacnt(nvacnt)=it
                  itet(1,it)=-itet(1,it)
                  npeeloff=npeeloff+1
               endif
            endif
 165     continue
         if (nvacnt .gt. nvacnto) then
            call kmprsn(nzerovol,kfix(3,1),4,kfix(3,1),4,kfix(3,1),4,
     *                  nzerovol)
            if (nzerovol.gt.0.and.ntry.lt.5) goto 160
         endif
         if (npeeloff .ge. 1) then
            write (logdan,3675) npeeloff
 3675       format('    zero-volume tets peeled off',i10)
            call writloga('default',0,logdan,0,ierr)
         endif
C
C        ..............................................................
C        ADD ANY REMAINING ZERO-VOLUME TETS TO MERGLST.
C
         if (nzerovol .gt. 0) then
            npairs2=npairs2+nzerovol
            if (npairs2 .ge. maxmerg) call termcode(1)
            do 170 i=1,nzerovol
               nnegvol=nnegvol+1
               merglst2(1,nnegvol)=kfix(3,i)
 170        continue
            if (npairs2 .ne. nnegvol) call termcode(1)
         endif
      endif
C
      do 175 i=1,npairs2
         merglst2(2,i)=-1
 175  continue
C
C     *****************************************************************
C     COMPRESS THE "itet" AND "jtet" LISTS IF "nvacnt".GE.1
C
      if(nvacnt.ge.1) then
         ipiholes=ipivacnt
         nholes=nvacnt
         if(leniopen.lt.nvacnt) then
            leniopen=nvacnt
            call mflip(ione,leniopen,'iopen')
         endif
         call filholes(ipiholes,nholes,ipiopen,npoints,ntets)
         nvacnt=0
      endif
C
C     ******************************************************************
C
Cdcg   timing call not implemented
Cdcg   set t2 to zero
      t2=0.
      isum=n22+n32+n23+n23i+n32x+n44+n20+n32i+n44i
         write(logdan,1003) ntets,nface,isum
         call writloga('default',0,logdan,0,ierr)
         write(logdan,1004) niters,nnegvol,nbfaces
 1003    format(' RECON:ntets= ',i8,' nface= ',i8,' nflips= ',i8)
 1004    format(  ' niter= ',i8,' negvol= ',i8,' nbface= ',i8)
         call writloga('default',0,logdan,0,ierr)
C
C
      if (nnegvol.eq.0 .or. ioptinv.eq.0) goto 9999
      if ((i2ndtime.gt.0.and.isum.eq.0).or.i2ndtime.eq.3) goto 9999
C
C
C     ******************************************************************
C
C     PROCESS NEGATIVE VOLUME TETS.
C
C
      maxiter=130
      maxpairs=25
      n23r=0
      n23b=0
      n20r=0
      n20b=0
      ictinvrt=0
      do 555 k=1,nnegvol
         invert(k)=merglst2(1,k)
 555  continue
      do 575 k=1,ntets
         kfix(1,k)=0
 575  continue
C
 600  continue
C
C
      if (ictinvrt.gt.0) then
         isum=n2to3r+n2to3b+n2to0r+n2to0b
         if (isum .gt. 0) then
            n23r=n23r+n2to3r
            n23b=n23b+n2to3b
            n20r=n20r+n2to0r
            n20b=n20b+n2to0b
C           write (logdan,1212) invert(ictinvrt),n2to3r,n2to3b,
C    *                           n2to0r,n2to0b
C1212       format('    it:',i6,' - n2to3r,n2to3b,n2to0r,n2to0b:',4i5)
C           call writloga('default', 0, logdan, 0, ierr)
         endif
      endif
      ictinvrt=ictinvrt+1
      if (ictinvrt .gt. nnegvol) goto 800
      ictmrg=0
      npairs2=0
      n2to3r=0
      n2to3b=0
      n2to0r=0
      n2to0b=0
      niter=0
C
      it=invert(ictinvrt)
      if (itet(1,it) .lt. 0) goto 600
      itets(1)=it
      call try2to0b(itets,ione,nflips,nflipsb,itv,
     *              npoints,ntets)
      ntest=ior(nflips,nflipsb)
      if (ntest .gt. 0) then
         n2to0r=n2to0r+nflips
         n2to0b=n2to0b+nflipsb
         goto 600
      endif
      volit=volume(itet(1,it),itet(2,it),itet(3,it),itet(4,it))
      if (volit .ge. 0) goto 600
C
C     .................................................................
C     FIND A GOOD STARTING FACE.
C
      call findface(it,ifpos,
     *              npoints,ntets)
      if (ifpos .eq. 0) goto 600
C
C     .................................................................
C     BEGIN THE ITERATIONS.
C
      npairs2=0
 700  continue
C
      ito=it
      niter=niter+1
      if (niter .gt. maxiter) then
         goto 600
      endif
      if (jtet(ifpos,it) .lt. mbndry) then
         itx=0.25*dble(jtet(ifpos,it))+0.9
C        volitx=volume(itet(1,itx),itet(2,itx),itet(3,itx),itet(4,itx))
C        if (volitx .le. 0) then
C           write (logdan,5454)  ihcycle,it
C5454       format('   FLIPPING WITH 2 INVERTED TETS: cycle,it - ',2i8)
C           call writloga('default', 0, logdan, 0, ierr)
C        endif
         ifposx=iand((jtet(ifpos,it)-1),maskface) + 1
         call find2to3(itx,ifposx,it2,ifpos2,it3,id(1),jd(1),
     *                 npoints,ntets,ierflg)
         if (it2.ne.it) call termcode(1)
         call flip2to3(itx,it2,it3,id(1),jd(1),
     *                 npoints,ntets)
         n2to3r=n2to3r+1
      elseif (jtet(ifpos,it) .gt. mbndry) then
         jtemp=jtet(ifpos,it)-mbndry
         itx=0.25*dble(jtemp)+0.9
C        volitx=volume(itet(1,itx),itet(2,itx),itet(3,itx),itet(4,itx))
C        if (volitx .le. 0) then
C           write (logdan,5454)  ihcycle,it
C           call writloga('default', 0, logdan, 0, ierr)
C        endif
         ifposx=iand((jtemp-1),maskface) + 1
         call fnd2to3b(itx,ifposx,it2,ifpos2,it3,id(1),jd(1),
     *                 npoints,ntets)
         if (it2.ne.it) call termcode(1)
         call flp2to3b(itx,it2,it3,id(1),jd(1),
     *                 npoints,ntets)
         n2to3b=n2to3b+1
      else
         goto 725
      endif
C              volit1=volume(id(1),id(2),id(3),id(4))
C              volit2=volume(id(5),id(6),id(7),id(8))
C              volit3=volume(id(9),id(10),id(11),id(12))
C     .................................................................
C     TRY THE 2-TO-0 FLIP.
C
      itets(1)=itx
      itets(2)=it2
      itets(3)=it3
      call try2to0b(itets,ithree,nflips,nflipsb,itv,
     *              npoints,ntets)
C
C     call tettestd
C
C
C     do 705 i=1,3
C        nt=itets(i)
C        if (nt.eq.0) goto 705
C        if (itet(1,nt).lt.0) goto 705
C        do 702 k=1,4
C           if (jtet(k,nt) .lt. mbndry) then
C              nt2=0.25*dble(jtet(k,nt))+0.9
C              ifpos2=((jtet(k,nt)-1).and.maskface)+1
C              i1=itet(iflist(3*k-2),nt)
C              i2=itet(iflist(3*k-1),nt)
C              i3=itet(iflist(3*k),nt)
C              isum1=i1+i2+i3
C              i1=itet(iflist(ifpos2*3-2),nt2)
C              i2=itet(iflist(ifpos2*3-1),nt2)
C              i3=itet(iflist(ifpos2*3),nt2)
C              isum2=i1+i2+i3
C              if (isum1.ne.isum2) then
C                 write (logdan,1515) nt,nt2,niter
C1515          format('JTET PROB: it1,it2,niter: ',3i6)
C              call writloga('default', 0, logdan, 0, ierr)
C              endif
C           elseif (jtet(k,nt) .gt. mbndry) then
C              jtemp=jtet(k,nt)-mbndry
C              nt2=0.25*dble(jtemp)+0.9
C              ifpos2=((jtemp-1).and.maskface)+1
C              i1=itet(iflist(3*k-2),nt)
C              i2=itet(iflist(3*k-1),nt)
C              i3=itet(iflist(3*k),nt)
C              isum1=i1+i2+i3
C              i1=itet(iflist(ifpos2*3-2),nt2)
C              i2=itet(iflist(ifpos2*3-1),nt2)
C              i3=itet(iflist(ifpos2*3),nt2)
C              isum2=i1+i2+i3
C              if (isum1.eq.isum2) then
C                 write (logdan,1515) nt,nt2,niter
C                 call writloga('default', 0, logdan, 0, ierr)
C              endif
C           endif
C702     continue
C705  continue
      if (nflips .ge. 1) n2to0r=n2to0r+nflips
      if (nflipsb .ge. 1) n2to0b=n2to0b+nflipsb
C
C     ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
C     FIND THE NEXT INVERTED TET TO WORK WITH AND STORE ADDITIONAL
C     INVERTED TETS TO WORK WITH LATER.
C
      ninvrt=0
      do 710 k=1,3
         itk=itets(k)
         if (itk .eq. 0) goto 710
         if (itet(1,itk) .le. 0) goto 710
         kfix(1,itk)=itk
         volitk=volume(itet(1,itk),itet(2,itk),itet(3,itk),
     *                 itet(4,itk))
         if (volitk .lt. 0) then
            ninvrt=ninvrt+1
            if (ninvrt .eq. 1) then
               itnext=itk
            else
               npairs2=npairs2+1
C              if (npairs2 .gt. maxpairs) then
C                 write (logdan,2346) ihcycle,it
C2346             format('   NPAIRS2.GT.MAXPAIRS: cycle,it - ',2i8)
C                 call writloga('default', 0, logdan, 0, ierr)
C                 goto 600
C              endif
               if (npairs2 .gt. maxpairs) goto 600
               merglst2(1,npairs2)=itk
            endif
         endif
 710  continue
      ifpos=3
      if (ninvrt .eq. 0) then
 715     if (npairs2 .eq. 0) goto 600
         it=merglst2(1,npairs2)
         npairs2=npairs2-1
         if (itet(1,it).lt.0) goto 715
         volit=volume(itet(1,it),itet(2,it),itet(3,it),itet(4,it))
         if (volit .ge. 0) goto 715
         goto 700
      elseif (ninvrt .eq. 1) then
         it=itnext
         goto 700
      elseif (ninvrt .eq. 2) then
         it=itnext
         goto 700
      elseif (ninvrt .eq. 3) then
C        write (logdan,2345) ihcycle,it
C2345    format('   INVERT 3 TETS!! - cycle,it: ',2i8)
C        call writloga('default', 0, logdan, 0, ierr)
         it=itnext
         goto 700
      endif
C
 725  continue
C
C     ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
C     IF AN EXTERNAL BOUNDARY IS ENCOUNTERED, WORK ON THE NEXT
C     INVERTED TET.
C
      write (logdan,2348) it
 2348 format('    inverted tet at external bdy: it - ',i10)
      call writloga('default', 0, logdan, 0, ierr)
      kfix(1,it)=it
 730  if (npairs2 .eq. 0) goto 600
      it=merglst2(1,npairs2)
      npairs2=npairs2-1
      if (itet(1,it).lt.0) goto 730
      volit=volume(itet(1,it),itet(2,it),itet(3,it),itet(4,it))
      if (volit .ge. 0) goto 730
      ifpos=3
      goto 700
C
 800  continue
C
C     *****************************************************************
C     COMPRESS THE "itet" AND "jtet" LISTS IF "nvacnt".GE.1
C
      if(nvacnt.ge.1) then
         ipiholes=ipivacnt
         nholes=nvacnt
         if(leniopen.lt.nvacnt) then
            leniopen=nvacnt
            call mflip(ione,leniopen,'iopen')
         endif
         call filholes(ipiholes,nholes,ipiopen,npoints,ntets)
         nvacnt=0
      endif
C
C     *****************************************************************
C     CONSTRUCT THE RECONNECTION LIST.
C
      call kmprsn(ntets,kfix(1,1),4,kfix(1,1),4,kfix(1,1),4,nrecon)
      if (lenrclst .le. nrecon) then
         lenrclst=nrecon+1000
         call mflip(ione,lenrclst,'irclst')
      endif
      do 810 i=1,nrecon
         irclst(i)=kfix(1,i)
 810  continue
      do 815 i=1,ntets
         kfix(1,i)=0
 815  continue
C
C     ...............................................................
C     ADD FACE NEIGHBORS TO THE RECONNECTION LIST.
C
      do 820 i=1,nrecon
         it=irclst(i)
         kfix(1,it)=it
         if (jtet(1,it) .lt. mbndry) then
            itt=0.25*dble(jtet(1,it))+0.9
            kfix(1,itt)=itt
         endif
         if (jtet(2,it) .lt. mbndry) then
            itt=0.25*dble(jtet(2,it))+0.9
            kfix(1,itt)=itt
         endif
         if (jtet(3,it) .lt. mbndry) then
            itt=0.25*dble(jtet(3,it))+0.9
            kfix(1,itt)=itt
         endif
         if (jtet(4,it) .lt. mbndry) then
            itt=0.25*dble(jtet(4,it))+0.9
            kfix(1,itt)=itt
         endif
 820  continue
      call kmprsn(ntets,kfix(1,1),4,kfix(1,1),4,kfix(1,1),4,nrecon)
      if(lenrclst.le.nrecon) then
         lenrclst=nrecon+1000
         call mflip(ione,lenrclst,'irclst')
      endif
      do 825 i=1,nrecon
         irclst(i)=kfix(1,i)
 825  continue
C
      do  850 i=1,ntets
         xvoltet=volume(itet(1,i),itet(2,i),itet(3,i),itet(4,i))
         kfix1(i)=cvmgmr(i,0,xvoltet)
 850  continue
      call kmprsn(ntets,kfix1,1,kfix1,1,kfix1,1,nnegvol)
C
      nregflps=n23r+n20r+n20b
      nbdyflps=n23b
      if (nbdyflps .eq. 0) then
         write (logdan,4545) nregflps,nnegvol
 4545    format('    inversion march,     nflips= ',i4,22x,i4,'#')
      else
         write (logdan,4546) nregflps,nbdyflps,nnegvol
 4546    format('    inversion march, nflips:  interior=',i4,
     *            ' boundary=',i4,2x,i4,'#')
      endif
      call writloga('default', 0, logdan, 0, ierr)
C
C     *****************************************************************
C
C     RECONNECT MESH.
C
C     call tettestd
      i2ndtime=i2ndtime+1
      niters=0
      n22=0
      n32=0
      n23=0
      n23i=0
      n32i=0
      n32x=0
      n44=0
      n44i=0
      n20=0
      goto 1
C
c      goto 9999
 9999 continue
C
         call cmo_set_info('nnodes',cmo,npoints,1,1,ierror)
         call cmo_set_info('nelements',cmo,ntets,1,1,ierror)
C
         call cmo_get_name(cmo,ierror)
         call cmo_get_info('mbndry',cmo,
     *                      mbndry,length,icmotype,ierror)
         call cmo_get_info('nodes_per_element',cmo,
     *                     nen,length,icmotype,ierror)
         call cmo_get_info('faces_per_element',cmo,
     *                     nef,length,icmotype,ierror)
         call cmo_get_info('itetclr',cmo,ipitetclr,ilen,itype,ierror)
         call cmo_get_info('itettyp',cmo,ipitettyp,ilen,itype,ierror)
         call cmo_get_info('itetoff',cmo,ipitetoff,ilen,itype,ierror)
         call cmo_get_info('jtetoff',cmo,ipjtetoff,ilen,itype,ierror)
         call cmo_get_info('jtet',cmo,ipjtet,length,icmotype,ierror)
C
         do it=1,ntets
            itettyp(it)=ifelmtet
            itetoff(it)=nen*(it-1)
            jtetoff(it)=nef*(it-1)
            do i=1,nef
               if(jtet1(jtetoff(it)+i).le.0) then
                  jtet1(jtetoff(it)+i)=mbndry
               endif
            enddo
         enddo
C
      call mmrelblk('merglst2',cglobal,ipmerglst2,icscode)
      if(abs(ivoronoi).eq.2) then
         write(logmess,9990)'cmo/modatt/'//cmo//
     *        'megadet/persistence/temporary;finish'
        call dotaskx3d(logmess,ierror)
         write(logmess,9990)'cmo/delatt/'//cmo//'megadet;finish'
 9990    format (a,a,a)
         call dotaskx3d(logmess,ierror)
         write(logmess,9990)'cmo/modatt/'//cmo//
     *        'megaerr/persistence/temporary;finish'
         call dotaskx3d(logmess,ierror)
         write(logmess,9990)'cmo/delatt/'//cmo//'megaerr;finish'
         call dotaskx3d(logmess,ierror)
         write(logmess,9990)'cmo/modatt/'//cmo//
     *        'megah/persistence/temporary;finish'
         call dotaskx3d(logmess,ierror)
         write(logmess,9990)'cmo/delatt/'//cmo//'megah;finish'
         call dotaskx3d(logmess,ierror)
         write(logmess,9990)'cmo/modatt/'//cmo//
     *        'mega3d/persistence/temporary;finish'
         call dotaskx3d(logmess,ierror)
         write(logmess,9990)'cmo/delatt/'//cmo//'mega3d;finish'
         call dotaskx3d(logmess,ierror)
      endif
      return
      end
