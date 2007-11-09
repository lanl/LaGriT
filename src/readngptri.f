*DK readngptri
      subroutine readngptri(ifile,ierror)
C
C#######################################################################
C
C      PURPOSE -
C
C         THIS ROUTINE READS AN "NGP.fro" FILE.
C
C     INPUT ARGUMENTS -
C
C        ifile - INPUT NGP FRO UNFORMATTED FILE.
C
C     OUTPUT ARGUMENTS -
C
C        ierror - RETURN ERROR CODE (== 0 ==> OK, <> 0 ==> AN ERROR)
C
C      CHANGE HISTORY -
C
C         $Log:   /pvcs.config/t3d/src/readngptri.f_a  $
CPVCS    
CPVCS       Rev 1.11   Mon Apr 14 16:57:56 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.10   11/07/95 17:23:24   dcg
CPVCS    change flag to 2 in mmgetblk calls
CPVCS    
CPVCS       Rev 1.9   09/27/95 10:18:26   dcg
CPVCS    read point number into dummy variable - don't change
CPVCS    loop index
CPVCS
CPVCS       Rev 1.8   08/31/95 08:35:00   ahmed
CPVCS    Fix a bug and clean up the routine
CPVCS
CPVCS       Rev 1.7   08/28/95 12:16:18   dcg
CPVCS    remove extra cmo_get_info('nnodes'... call
CPVCS
CPVCS       Rev 1.6   06/05/95 10:36:24   het
CPVCS    Make changes for hybrid_grids
CPVCS
CPVCS       Rev 1.5   05/26/95 13:17:44   het
CPVCS    Replace subroutine parameter list with subroutine calles.
CPVCS
CPVCS       Rev 1.4   05/01/95 08:35:50   het
CPVCS    Modifiy all the storage block calles for long names
CPVCS
CPVCS       Rev 1.3   02/17/95 19:19:10   het
CPVCS    Fix a bug in the NGP triangle file
CPVCS
CPVCS       Rev 1.2   01/23/95 12:45:30   het
CPVCS    Delete the call to cmo_increment with cmo_newlen.
CPVCS
CPVCS
CPVCS       Rev 1.1   01/04/95 22:04:36   llt
CPVCS    unicos changes (made by het)
CPVCS
CPVCS       Rev 1.0   12/11/94 09:28:04   het
CPVCS    Original version.
CPVCS
C
C#######################################################################
C
      implicit real*8 (a-h, o-z)
C
C ######################################################################
C
C     DEFINE THE MESH_OBJECT POINTERS.
C
      include 'chydro.h'
      include 'local_element.h'
C
      parameter (lenptr=10000000)
C
C ######################################################################
C
      pointer (ipimt1, imt1)
      pointer (ipitp1, itp1)
      pointer (ipitetclr, itetclr)
      pointer (ipitettyp, itettyp)
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      pointer (ipitet, itet1)
      pointer (ipjtet, jtet1)
      pointer (ipitetoff, itetoff)
      pointer (ipjtetoff, jtetoff)
      pointer (ipjtet2, jtet2)
C
      integer imt1(lenptr), itp1(lenptr)
      integer itetclr(lenptr), itettyp(lenptr)
      integer itet1(lenptr), jtet1(lenptr)
      integer itetoff(lenptr), jtetoff(lenptr),jtet2(lenptr)
      REAL*8  xic(lenptr), yic(lenptr), zic(lenptr)
C
      character ifile*(*)
      character*32 isubname, cmo
      character*132 logmess
C
C#######################################################################
C
C     ******************************************************************
C     SET THE NAME TO USED AS THE PARTITION FOR THE TEMPORARY MEMORY
C     THAT WILL BE ALLOCATED FOR THIS SUBROUTINE.
C
      isubname='readngptri'
C
      iunit=-1
      call hassign(iunit,ifile,ierror)
C
      read(iunit,*) ntets, npoints
      nsd=2
      nsg=3
      nen=3
      nef=3
C
C     ******************************************************************
C     ALLOCATE SOME TEMPORARY MEMORY.
C
      length=nef*ntets
      call mmgetblk('jtet2',isubname,ipjtet2,length,2,icscode)
C
      call cmo_get_name(cmo,ierror)
      call cmo_set_info('nnodes',cmo,npoints,1,1,ierror)
      call cmo_set_info('nelements',cmo,ntets,1,1,ierror)
      call cmo_set_info('ndimensions_topo',cmo,nsd,1,1,ierror)
      call cmo_set_info('ndimensions_geom',cmo,nsg,1,1,ierror)
      call cmo_set_info('nodes_per_element',cmo,nen,1,1,ierror)
      call cmo_set_info('faces_per_element',cmo,nef,1,1,ierror)
C
      call cmo_newlen(cmo,ierror)
      call cmo_get_info('mbndry',cmo,mbndry,1,1,ierror)
C
      call cmo_get_info('imt1',cmo,ipimt1,lenimt1,icmotype,ierror)
      call cmo_get_info('itp1',cmo,ipitp1,lenitp1,icmotype,ierror)
      call cmo_get_info('xic',cmo,ipxic,lenxic,icmotype,ierror)
      call cmo_get_info('yic',cmo,ipyic,lenyic,icmotype,ierror)
      call cmo_get_info('zic',cmo,ipzic,lenzic,icmotype,ierror)
      call cmo_get_info('itetclr',cmo,ipitetclr,lenitetclr,icmotype,ier)
      call cmo_get_info('itettyp',cmo,ipitettyp,lenitettyp,icmotype,ier)
      call cmo_get_info('itetoff',cmo,ipitetoff,lenitetoff,icmotype,ier)
      call cmo_get_info('jtetoff',cmo,ipjtetoff,lenjtetoff,icmotype,ier)
      call cmo_get_info('itet',cmo,ipitet,lenitet,icmotype,ierror)
      call cmo_get_info('jtet',cmo,ipjtet,lenjtet,icmotype,ierror)
C
      write(logmess,'(a)')
     &              'Read NGP-fro mesh file and convert it to X3D cmo.'
      call writloga('default',0,logmess,0,ierror)
 
      do ind=1,npoints
         read(iunit,*) itno,xic(ind),yic(ind),zic(ind)
         itp1(ind)=0
         imt1(ind)=0
      enddo
C
      do itri=1,ntets
         read(iunit,*) itno,i1,i2,i3,itetclr(itri),idum
C
         itettyp(itri)=ifelmtri
         itetoff(itri)=nelmnen(ifelmtri)*(itri-1)
         jtetoff(itri)=nelmnef(ifelmtri)*(itri-1)
         index=nen*(itri-1)
         itet1(1+index)=i1
         itet1(2+index)=i2
         itet1(3+index)=i3
C
         index=nef*(itri-1)
         jtet1(1+index)=-1
         jtet1(2+index)=-1
         jtet1(3+index)=-1
         jtet2(1+index)=-1
         jtet2(2+index)=-1
         jtet2(3+index)=-1
      enddo
C
C     ******************************************************************
C     CLOSE THE DUMP FILE.
C
      close(iunit)
C
      do itri=1,ntets
         index=1+nef*(itri-1)
         imt1(itet1(index))= itetclr(itri)
      enddo
      do ind=1,npoints
         imt1(ind)=max(1,imt1(ind))
      enddo
C
      call cmo_get_name(cmo,ierror)
      call cmo_set_info('nnodes',cmo,npoints,1,1,ier)
      call cmo_set_info('nelements',cmo,ntets,1,1,ier)
C
C     ******************************************************************
C     GENERATE CONNECTIVITY
C
      call geniee(itet1,jtet1,jtet2,nen,nef,
     *     ntets,npoints,nsd,npoints,ntets)
      do it=1,ntets
         do i=1,nelmnef(itettyp(it))
            index=i+nef*(it-1)
            jt=jtet1(index)
            jf=jtet2(index)
            if(jt.gt.0.and.jt.le.ntets) then
               jtet1(index)=nelmnef(itettyp(it))*(jtet1(index)-1)+
     *                      jtet2(index)
            else
               jtet1(index)=mbndry
            endif
         enddo
      enddo
C
      goto 9999
 9999 continue
C
C     *******************************************************************
C     RELEASE TEMPORARY MEMORY.
C
      call mmrelprt(isubname,ierror)
C
      return
      end
