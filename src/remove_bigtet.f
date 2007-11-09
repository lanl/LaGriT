      subroutine remove_bigtet()
C
C#######################################################################
C
C     PURPOSE -  remove all tets associated with the enclosing 'bigtet'
C
C
C    CHANGE CONTROL
C
C  $Log: remove_bigtet.f,v $
C  Revision 2.00  2007/11/09 20:04:01  spchu
C  Import to CVS
C
CPVCS    
CPVCS       Rev 1.1   Mon Oct 06 10:05:14 1997   dcg
CPVCS    add cmo_newlen call
CPVCS
CPVCS       Rev 1.0   Tue Aug 19 14:35:52 1997   dcg
CPVCS    Initial revision.
C
C#######################################################################
C
      implicit none
      include 'search.h'
      include 'cmo.h'
C
      real*8 cvmgt,cvmgm
      integer leni,ierr,it,ntets,nremtet,jtetlcl,ii,imove,nmovtet,
     *  icmotype
      character*32 isubname
      pointer (ipiremtet,iremtet)
      pointer (ipimovtet,imovtet)
      integer iremtet(1000000),imovtet(1000000)
      data isubname/'remove_bigtet'/
C
      call cmo_get_name(cmo,ierr)
C
C  Access mesh object
C
      call cmo_get_info('nelements',cmo,ntets,leni,icmotype,ierr)
      call cmo_get_info('mbndry',cmo,mbndry,leni,icmotype,ierr)
      call cmo_get_info('itet',cmo,ipitet,leni,icmotype,ierr)
      call cmo_get_info('jtet',cmo,ipjtet,leni,icmotype,ierr)
C
C     REMOVE ALL TETRAHEDRA ASSOCIATED WITH THE BIG TETRAHEDRON AND FILL
C     HOLES.
C
      iremove=0
      idud=1
      iaddpts=1
C
C        _______________________________________________________________
C        MAKE A LIST OF TETRAHEDRA TO BE REMOVED.
C
      call mmfindbk('xvor',nname,ipxvor,leni,ierr)
      call mmfindbk('yvor',nname,ipyvor,leni,ierr)
      call mmfindbk('zvor',nname,ipzvor,leni,ierr)
      call mmfindbk('vol',nname,ipvol,leni,ierr)
      call mmgetblk('iremtet',isubname,ipiremtet,ntets,1,ierr)
      call mmgetblk('imovtet',isubname,ipimovtet,ntets,1,ierr)
      do  it=1,ntets
            iremtet(it)=cvmgt(it,0,    itet(1,it).ge.ibigtet
     $                             .or.itet(2,it).ge.ibigtet
     $                             .or.itet(3,it).ge.ibigtet
     $                             .or.itet(4,it).ge.ibigtet)
      enddo
      call kmprsn(ntets,iremtet(1),1,iremtet(1),1,iremtet(1),1,
     $               nremtet)
C
C        _______________________________________________________________
C        SET jtet FOR TETRAHEDRA ADJOINING THOSE TO BE REMOVED TO
C        mbndry.
C
      do  ii=1,nremtet
            jtetlcl=cvmgt(jtet(1,iremtet(ii)),
     $                    jtet(1,iremtet(ii))-mbndry,
     $                    jtet(1,iremtet(ii)).lt.mbndry)
            jtet1(jtetlcl)=cvmgt(jtet1(jtetlcl),mbndry,jtetlcl.le.0)
            jtetlcl=cvmgt(jtet(2,iremtet(ii)),
     $                    jtet(2,iremtet(ii))-mbndry,
     $                    jtet(2,iremtet(ii)).lt.mbndry)
            jtet1(jtetlcl)=cvmgt(jtet1(jtetlcl),mbndry,jtetlcl.le.0)
            jtetlcl=cvmgt(jtet(3,iremtet(ii)),
     $                    jtet(3,iremtet(ii))-mbndry,
     $                    jtet(3,iremtet(ii)).lt.mbndry)
            jtet1(jtetlcl)=cvmgt(jtet1(jtetlcl),mbndry,jtetlcl.le.0)
            jtetlcl=cvmgt(jtet(4,iremtet(ii)),
     $                    jtet(4,iremtet(ii))-mbndry,
     $                    jtet(4,iremtet(ii)).lt.mbndry)
            jtet1(jtetlcl)=cvmgt(jtet1(jtetlcl),mbndry,jtetlcl.le.0)
      enddo
C
C        _______________________________________________________________
C        SET THE FIRST VALUE OF itet TO NEGATIVE FOR TETRAHEDRA TO BE
C        REMOVED.
C
      do  ii=1,nremtet
            itet(1,iremtet(ii))=-itet(1,iremtet(ii))
      enddo
C
C        _______________________________________________________________
C        MAKE A LIST OF TETRAHEDRA TO BE MOVED INTO VACANT SPOTS.
C
      imove=ntets+1
      do  it=ntets,ntets-nremtet+1,-1
            imovtet(imove-it)=cvmgm(0,it,itet(1,it))
      enddo
      call kmprsn(nremtet,imovtet(1),1,imovtet(1),1,imovtet(1),1,
     $               nmovtet)
C
C        _______________________________________________________________
C
C        MOVE TETRAHEDRA IN THE MOVE LIST TO THE REMOVE LIST, ONE AT A
C        TIME, IN SCALAR MODE.  ALSO MOVE VORONOI CENTERS AND VOLUMES
C
      do  ii=1,nmovtet
            itet(1,iremtet(ii))=itet(1,imovtet(ii))
            itet(2,iremtet(ii))=itet(2,imovtet(ii))
            itet(3,iremtet(ii))=itet(3,imovtet(ii))
            itet(4,iremtet(ii))=itet(4,imovtet(ii))
            jtet(1,iremtet(ii))=jtet(1,imovtet(ii))
            jtet(2,iremtet(ii))=jtet(2,imovtet(ii))
            jtet(3,iremtet(ii))=jtet(3,imovtet(ii))
            jtet(4,iremtet(ii))=jtet(4,imovtet(ii))
            xvor(iremtet(ii))=xvor(imovtet(ii))
            yvor(iremtet(ii))=yvor(imovtet(ii))
            zvor(iremtet(ii))=zvor(imovtet(ii))
            vol(iremtet(ii))=vol(imovtet(ii))
            jtetlcl=cvmgt(jtet(1,iremtet(ii)),
     $                    jtet(1,iremtet(ii))-mbndry,
     $                    jtet(1,iremtet(ii)).lt.mbndry)
            if(jtetlcl.gt.0) jtet1(jtetlcl)=4*iremtet(ii)-3
            jtetlcl=cvmgt(jtet(2,iremtet(ii)),
     $                    jtet(2,iremtet(ii))-mbndry,
     $                    jtet(2,iremtet(ii)).lt.mbndry)
            if(jtetlcl.gt.0) jtet1(jtetlcl)=4*iremtet(ii)-2
            jtetlcl=cvmgt(jtet(3,iremtet(ii)),
     $                    jtet(3,iremtet(ii))-mbndry,
     $                    jtet(3,iremtet(ii)).lt.mbndry)
            if(jtetlcl.gt.0) jtet1(jtetlcl)=4*iremtet(ii)-1
            jtetlcl=cvmgt(jtet(4,iremtet(ii)),
     $                    jtet(4,iremtet(ii))-mbndry,
     $                    jtet(4,iremtet(ii)).lt.mbndry)
            if(jtetlcl.gt.0) jtet1(jtetlcl)=4*iremtet(ii)
      enddo
C
C        _______________________________________________________________
C        ADJUST ntets.
C
      ntets=ntets-nremtet
      call cmo_set_info('nelements',cmo,ntets,1,1,ierr)
      call cmo_newlen(cmo,ierr)
C
      call mmrelprt(isubname,ierr)
      return
      end
