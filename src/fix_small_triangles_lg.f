      subroutine fix_small_triangles_lg(npoints,ntets,
     *   epsilon,nlsttts)
C
C#######################################################################
C
C     PURPOSE -
C
C   This routine finds elements with small volumes and tries to fix them
C   by inserting a point into the mesh at the center of the element
C
C     INPUT ARGUMENTS -
C
C   npoints -- number of nodes in the mesh
C   ntets -- number of elements in the mesh
C
C     OUTPUT ARGUMENTS -
C
C   nlsttts will contain the number of nodes to be inserted
C
C
C     CHANGE HISTORY -
C
C        $Log: fix_small_triangles_lg.f,v $
C        Revision 2.00  2007/11/05 19:45:55  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.3   08 Feb 2006 14:38:16   dcg
CPVCS     "enforce lower case - add external statements for shift routines
CPVCS    these changes needed to compile with absoft pro fortran"
CPVCS    

CPVCS       Rev 1.2   30 Sep 2004 10:42:36   dcg

CPVCS    use ior in place of .or. with integer variables

CPVCS    

CPVCS       Rev 1.1   13 Jan 2000 14:47:56   dcg

CPVCS    

CPVCS       Rev 1.0   04 Jan 2000 16:47:36   dcg

CPVCS     

CPVCS    
CPVCS       Rev 1.1   Mon Mar 29 10:33:40 1999   dcg
CPVCS    update list of possible materials for nodes (matlst) correctly.
CPVCS
CPVCS       Rev 1.0   Fri Mar 19 13:14:48 1999   dcg
CPVCS    Initial revision.
C
C#######################################################################
C
      implicit none
      include 'search.h'
      include 'chydro.h'
      include 'geom_lg.h'
C
      integer shiftl, ior
      external shiftl
      logical itsttp
      real*8 cvmgt
      integer icscode,i,matindex,itetl1,itetl2,itetl3,iit,nnodes,
     *  nmatfal1,nmatfal2,it,npoints,ntets,leni,icmotype,incpts,
     *  ier,iimt,lenvvbar,ii,n,lenmatn,nlsttts,leng,iface,imtval
      pointer (ipvvbar,vvbar)
      pointer (ipvolic,volic)
      pointer (ipmatlst,matlst)
      pointer (ipimts1,imts1)
      pointer (iplstptl,lstptl)
      pointer (ipnimts,nimts)
      pointer (iprgnum,irgnum)
      pointer (ipsrfnum,isrfnum)
      pointer (ipitet,itet)
      pointer (ipitet,itet1)
      integer itet(3,*),itet1(*)
      pointer (ipitp1,itp1)
      integer itp1(*)
      pointer (ipimt1,imt1)
      integer imt1(*)
      pointer (ipxic,xic),(ipyic,yic),(ipzic,zic)
      real*8 xic(*),yic(*),zic(*)
      character*132 logmess
      integer irgnum(*),isrfnum(*),
     * nimts(*),
     * imts1(*),matlst(*),lstptl(*)
 
      real*8 vvbarmin,vvbar(*),volic(*),
     *  epsilon
      character*8 cmregion
      character*16 isubname
      character*32 cmo
      data cmregion,isubname/'mregion','fix_sm_tet'/
      data vvbarmin/1.0e-07/
C
C  If bigtet exists ibigtet > 0 then need to save
C  room for extra 3 points
      incpts=0
      if(ibigtet.gt.0) incpts=3
C
C  Refresh pointers
C
      call mmfindbk('matlst',nname,ipmatlst,leni,ier)
      call mmfindbk('lstptl',nname,iplstptl,leni,ier)
      call mmfindbk('lstfail',nname,iplstfal,leni,ier)
      call mmfindbk('lsttts',nname,iplsttts,leni,ier)
      call mmfindbk('vol',nname,ipvol,leni,ier)
      call cmo_get_name(cmo,ier)
      call cmo_get_info('itet',cmo,ipitet,leni,icmotype,ier)
C
C   GET memory
C
      lenvvbar=npoints+incpts
      call mmgetblk('vvbar',isubname,ipvvbar,lenvvbar,2,icscode)
      call mmgetblk('volic',isubname,ipvolic,lenvvbar,2,icscode)
C
C    ZERO-OUT THE DESIRED ARRAYS.
C
      do  i=1,npoints+incpts
         volic(i)=0.0
         vvbar(i)=0.0
      enddo
C
C        COMPUTE  VOLUMES BY ACCUMULATING triangle VOLUMES
C        ASSOCIATED WITH EACH NODE.  ALSO, ACCUMULATE NUMBER OF
C        TETRAHEDRA ASSOCIATED WITH EACH MASS POINT.
C
      do  it=1,ntets
         if(itet(1,it).le.npoints.and.itet(2,it).le.npoints.and.
     $      itet(3,it).le.npoints) then
            volic(itet(1,it))=volic(itet(1,it))+vol(it)
            volic(itet(2,it))=volic(itet(2,it))+vol(it)
            volic(itet(3,it))=volic(itet(3,it))+vol(it)
C                 *** THESE VOLUMES ARE 24 TIMES THE TRUE MASS-POINT
C                 *** VOLUMES.
            vvbar(itet(1,it))=vvbar(itet(1,it))+1.0
            vvbar(itet(2,it))=vvbar(itet(2,it))+1.0
            vvbar(itet(3,it))=vvbar(itet(3,it))+1.0
C                 *** NUMBER OF TETRAHEDRA ASSOCIATED WITH A MASS POINT.
         endif
      enddo
C
C        _______________________________________________________________
C
C        CALCULATE THE AVERAGE TETRAHEDRON VOLUME FOR EACH MASS POINT,
C        MULTIPLY IT BY A MINIMUM ACCEPTABLE VELUE OF VTET/VBAR, AND
C        STORE IT IN THE vvbar ARRAY.
C
      do  i=1,npoints+incpts
         vvbar(i)=vvbarmin*volic(i)/(vvbar(i)+epsilon)
C              *** SIX TIMES THE AVERAGE TETRAHEDRON VOLUME ASSOCIATED
C              *** WITH A MASS POINT, MULTIPLIED BY A MINIMUM ACCEPTABLE
C              *** RATIO OF VTET/VBAR.
      enddo
C
C        _______________________________________________________________
C
C        MAKE A LIST OF TETRAHEDRA THAT FAIL THE MINIMUM V/VBAR TEST.
C
      do  it=1,ntetexcl
         lsttts(it)=0
      enddo
      do it=itetstrt,ntets
         lsttts(it)=cvmgt(it,0,
     $     vol(it).lt.max(vvbar(itet(1,it)),vvbar(itet(2,it)),
     $                    vvbar(itet(3,it)))
     $       .and.
     $    itet(1,it).le.npoints.and.itet(2,it).le.npoints.and.
     $    itet(3,it).le.npoints)
      enddo
      call kmprsn(ntets,lsttts(1),1,lsttts(1),1,lsttts(1),1,
     $               nlsttts)
 
      if(nlsttts.ne.0) then
C
C           INCREASE MEMORY, IF NECESSARY.
C
         nnodes=npoints+incpts+nlsttts
         call cmo_set_info('nnodes',cmo,nnodes,1,1,ier)
         call cmo_newlen(cmo,ier)
         call cmo_get_info('imt1',cmo,ipimt1,leni,icmotype,ier)
         call cmo_get_info('itp1',cmo,ipitp1,leni,icmotype,ier)
         call cmo_get_info('xic',cmo,ipxic,leni,icmotype,ier)
         call cmo_get_info('yic',cmo,ipyic,leni,icmotype,ier)
         call cmo_get_info('zic',cmo,ipzic,leni,icmotype,ier)
         call cmo_get_info('itet',cmo,ipitet,leni,icmotype,ier)
         call cmo_set_info('nnodes',cmo,npoints,1,1,ier)
c
         if(nlsttts.gt.lstptlen) then
            lstptlen=nlsttts
            call mmnewlen('lstptl',nname,iplstptl,lstptlen,icscode)
            call mmnewlen('lstfail',nname,iplstfal,lstptlen,icscode)
         endif
         if(npoints+incpts+nlsttts.gt.lenmatmx) then
            lenmatn=(6*(npoints+incpts+nlsttts))/5
            call mmnewlen('matlst',nname,ipmatlst,lenmatn,
     $                       icscode)
            do  n=matblks,2,-1
               do  ii=(n-1)*lenmatn+npoints,(n-1)*lenmatn+1,-1
                  matlst(ii)=matlst(ii-(n-1)*(lenmatn-lenmatmx))
               enddo
            enddo
            do n=1,matblks
               do  ii=(n-1)*lenmatn+npoints+1,n*lenmatn
                  matlst(ii)=0
               enddo
            enddo
            lenmatmx=lenmatn
         else
            do  n=1,matblks
               matindex=(n-1)*lenmatmx
               matlst(matindex+npoints+1)=0
               matlst(matindex+npoints+2)=0
               matlst(matindex+npoints+3)=0
            enddo
         endif
C
C           ____________________________________________________________
C
C           ADD POINTS AT SMALL-Triangle CENTROIDS AND UPDATE THE
C           lstptl ARRAY.
C
         do  iit=1,nlsttts
            itetl1=itet(1,lsttts(iit))
            itetl2=itet(2,lsttts(iit))
            itetl3=itet(3,lsttts(iit))
            xic(npoints+iit)=(xic(itetl1)+xic(itetl2)
     $                            +xic(itetl3))/3.
            yic(npoints+iit)=(yic(itetl1)+yic(itetl2)
     $                            +yic(itetl3))/3.
            zic(npoints+iit)=(zic(itetl1)+zic(itetl2)
     $                            +zic(itetl3))/3.
            itp1(npoints+iit)=ifitpint
            imt1(npoints+iit)=imt1(itetl1)
C                 *** PROBABLE POINT TYPE.  THE POINT TYPE MAY GET
C                 *** CHANGED LATER ON IF THE POINT TURNS OUT TO BE
C                 *** AN INTERFACE POINT, OR IF THE POINT IS
C                 *** CONSTRAINED.  SET POINT MATERIAL FROM POINT
C                 *** ON CONTAINING TET.
            lstptl(iit)=npoints+iit
            write(logmess,8390) lstptl(iit),itetl1,itetl2,itetl3
 8390       format(' Adding point',i10,' in tet',3i10,'.')
            call writloga('default',0,logmess,0,ier)
         enddo
         nlstptl=nlsttts
C
C           IF NO REGIONS PRINT WARNING AND RETURN
C
         if (nmregs.eq.0) then
            write(logmess,8400) npoints+1,npoints+nlstptl
 8400       format(' No regions - material set from enclosing tet ',
     *        'for points ',i10,' to ',i10)
            call writloga('bat',0,logmess,0,ier)
            go to 9000
         endif
C
C
C           DETERMINE THE MATERIAL TYPES OF NEW POINTS, AND IF A POINT
C           TURNS OUT TO BE AN INTERFACE POINT, ALSO MODIFY ITS POINT
C           TYPE.
C
         nmatfal1=0
         call mmfindbk('irgnum',nname,iprgnum,leni,ier)
         if(ier.eq.0) then
            if(leni.lt.nlstptl)
     *        call mmnewlen('irgnum',nname,iprgnum,nlstptl,ier)
         else
            call mmgetblk('irgnum',nname,iprgnum,nlstptl,1,ier)
         endif
         call mmfindbk('isrfnum',nname,ipsrfnum,leni,ier)
         if(ier.eq.0) then
            if(leni.lt.nlstptl)
     *           call mmnewlen('isrfnum',nname,ipsrfnum,nlstptl,ier)
         else
            call mmgetblk('isrfnum',nname,ipsrfnum,nlstptl,1,ier)
         endif
         call getregv(xic(npoints+1),yic(npoints+1),
     $                     zic(npoints+1),nlstptl,
     &                     smldistp,cmregion,0,
     $                     irgnum,isrfnum,ier)
         do  i=1,nlstptl
            if(irgnum(i).gt.0) then
               imt1(npoints+i)=irgnum(i)
            elseif(isrfnum(i).gt.0) then
               imt1(npoints+i)=imatint
               itp1(npoints+i)=ifitpini
C                    *** PROBABLE POINT TYPE.  THE POINT TYPE MAY GET
C                    *** CHANGED LATER ON IF THE POINT TURNS OUT TO BE
C                    *** CONSTRAINED.
            else
               nmatfal1=nmatfal1+1
               imt1(npoints+i)=0
            endif
         enddo
         if(nmatfal1.ne.0) then
            write(logmess,8510) nlstptl,nmatfal1
 8510       format(' Out of',i10,
     $                ' new points, cannot find material for',i10,
     $                ' points.')
            call writloga('default',1,logmess,0,ier)
         endif
C
         call ifacerfl(npoints+1,nlstptl,smldistp)
C                 *** CONSTRAINTS WILL BE PACKED INTO THE FIT WORD IN
C                 *** THIS ROUTINE.
C
C           WRITE A MESSAGE ABOUT THE ADDITION OF POINTS.
C
         write(logmess,8600) lstptl(1),lstptl(nlstptl)
 8600    format(' Points',i7,' -',i7,
     *     ' are being added to fix small tets.')
         call writloga('default',1,logmess,0,ier)
C
C           MODIFY THE MATERIAL-LIST ARRAY.
C
         nmatfal1=0
         nmatfal2=0
         call mmfindbk('nimts',nname,ipnimts,leni,ier)
         if(ier.eq.0) then
            if(leni.lt.nlstptl)
     *          call mmnewlen('nimts',nname,ipnimts,nlstptl,ier)
         else
            call mmgetblk('nimts',nname,ipnimts,nlstptl,1,ier)
         endif
         call mmfindbk('imts1',nname,ipimts1,leni,ier)
         leng=nlstptl*max(imtmax,nmregs)
         if(ier.eq.0) then
            if(leni.lt.leng)
     *            call mmnewlen('imts1',nname,ipimts1,leng,ier)
         else
            call mmgetblk('imts1',nname,ipimts1,leng,1,ier)
         endif
         call ifaceregv(xic(npoints+1),yic(npoints+1),
     $                          zic(npoints+1),nlstptl,smldistp,
     *                          imts1,nimts,
     *                          ier)
          do  i=1,nlstptl
            if(itsttp('intrface',itp1(npoints+i))) then
                if(nimts(i).ne.0) then
                  do  iimt=1,nimts(i)
                     imtval=imts1(iimt+nmregs*(i-1))
                     matindex=((imtval-1)/32)*
     *                               lenmatmx
                     matlst(matindex+npoints+i)=ior(
     *                       matlst(matindex+npoints+i),
     $                       shiftl(1,imtval-((imtval-1)/32)*32-1))
                  enddo
               else
                  nmatfal1=nmatfal1+1
               endif
            else
               if(imt1(i+npoints).ne.0) then
                  matindex=((imt1(i+npoints)-1)/32)*lenmatmx
                  matlst(matindex+npoints+i)=ior
     $                (matlst(matindex+npoints+i)
     $                ,shiftl(1,
     $                imt1(i+npoints)-((imt1(i+npoints)-1)/32)*32-1))
               else
                  nmatfal2=nmatfal2+1
               endif
            endif
         enddo
         if(nmatfal1.ne.0) then
            write(logmess,8690) nmatfal1
 8690       format(' Cannot find materials for',i10,
     $                ' new interface points.')
            call writloga('default',1,logmess,0,ier)
         endif
         if(nmatfal2.ne.0) then
            write(logmess,8700) nmatfal2
 8700       format(' No materials associated with',i10,
     $                ' new noninterface points.')
            call writloga('default',0,logmess,0,ier)
         endif
C
C           CHANGE THE MASS-POINT NUMBERS OF POINTS ASSOCIATED WITH THE
C           BIG Triangle.
9000     if(ibigtet.gt.0) then
C
            do  iface=ittstrt1,3*ntets
               itet1(iface)=cvmgt(itet1(iface),itet1(iface)+nlstptl,
     $                            itet1(iface).lt.ibigtet)
            enddo
            ibigtet=npoints+nlstptl+1
            xic (ibigtet  )=xbigtet(1)
            yic (ibigtet  )=ybigtet(1)
            zic (ibigtet  )=zbigtet(1)
            itp1(ibigtet  )=ifitpini
            imt1(ibigtet  )=0
            xic (ibigtet+1)=xbigtet(2)
            yic (ibigtet+1)=ybigtet(2)
            zic (ibigtet+1)=zbigtet(2)
            itp1(ibigtet+1)=ifitpini
            imt1(ibigtet+1)=0
            xic (ibigtet+2)=xbigtet(3)
            yic (ibigtet+2)=ybigtet(3)
            zic (ibigtet+2)=zbigtet(3)
            itp1(ibigtet+2)=ifitpini
            imt1(ibigtet+2)=0
            do n=1,matblks
               matindex=(n-1)*lenmatmx
               matlst(matindex+ibigtet  )=-1
               matlst(matindex+ibigtet+1)=-1
               matlst(matindex+ibigtet+2)=-1
            enddo
        endif
C              *** THE BIG-Triangle POINT TYPES ARE SET TO INTERFACE
C              *** TYPES BECAUSE THIS ALLOWS US TO EXCLUDE CONNECTIONS
C              *** ORIGINATING FROM THESE POINTS DURING THE FIRST
C              *** INTERFACE PASS.  ALSO, ALL matlst BITS FOR THE
C              *** BIG-TETRAHEDRON POINTS ARE SET, SO THAT A CONNECTION
C              *** ORIGINATING FROM THESE POINTS WILL NEVER BE
C              *** CONSIDERED A MULTIMATERIAL CONNECTION DURING THE
C              *** SECOND INTERFACE PASS, AND ALL SUBSEQUENT PASSES.
C           INCREMENT npoints.
C
         npoints=npoints+nlstptl
         call cmo_set_info('nnodes',cmo,npoints,1,1,ier)
C
C           ____________________________________________________________
C
C           RETURN  AND TRY TO CONNECT THESE POINTS.
C
         istep=1
      endif
      call mmrelprt(isubname,ier)
      return
      end
