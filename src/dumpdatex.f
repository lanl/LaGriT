      subroutine dumpdatex(ifile)
C
C #####################################################################
C
C     PURPOSE -
C
C        None
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
C        $Log: dumpdatex.f,v $
C        Revision 2.00  2007/11/05 19:45:53  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.0   08 Feb 2000 09:42:40   dcg
CPVCS    Initial revision.
CPVCS    
CPVCS       Rev 1.12   Tue Mar 31 13:08:58 1998   dcg
CPVCS    fix format statements for newer version of datex standard
CPVCS
CPVCS       Rev 1.11   Wed Jul 16 10:23:44 1997   dcg
CPVCS    declare logmess
CPVCS
CPVCS       Rev 1.10   Mon Apr 14 16:44:02 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.9   Tue Mar 05 12:49:14 1996   dcg
CPVCS    remove icn1, int1
CPVCS
CPVCS       Rev 1.8   Fri Feb 23 16:36:02 1996   dcg
CPVCS    remove explicit references to uic,vic,wic,pic,ric,eic
CPVCS
CPVCS       Rev 1.7   11/07/95 17:16:18   dcg
CPVCS    change flag to 2 in mmgetblk calls
CPVCS
CPVCS       Rev 1.6   06/20/95 15:43:10   dcg
CPVCS    remove character literals from arguments list to hgetprt
CPVCS
CPVCS       Rev 1.5   05/26/95 13:13:24   het
CPVCS    Replace subroutine parameter list with subroutine calles.
CPVCS
CPVCS       Rev 1.4   05/01/95 08:35:54   het
CPVCS    Modifiy all the storage block calles for long names
CPVCS
CPVCS       Rev 1.3   03/03/95 12:09:20   dcg
CPVCS     Remove hard-wired 2dmesh mesh object in decimate, dump routines
CPVCS
CPVCS       Rev 1.2   01/04/95 22:02:06   llt
CPVCS    unicos changes (made by het)
CPVCS
CPVCS       Rev 1.1   12/19/94 08:20:44   het
CPVCS    Add the "comdict.h" include file.
CPVCS
CPVCS
CPVCS       Rev 1.0   11/13/94 11:44:18   pvcs
CPVCS    Orginal Version
C
C ######################################################################
C
      implicit none
      include 'geom_lg.h'
c
C ######################################################################
C
      character ifile*(*)
      pointer (ipimt1, imt1)
      pointer (ipitp1, itp1)
      pointer (ipicr1, icr1)
      pointer (ipisn1, isn1)
      pointer (ipicn1, icn1)
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      pointer (ipvels,vels)
      pointer (ippres, pres)
      pointer (ipdens, dens)
      pointer (ipener, ener)
      pointer (ipitetclr, itetclr)
      pointer (ipitet, itet)
      pointer (ipjtet, jtet)
      pointer (ipout,out)
      integer nsd, nen, nef
      integer imt1(*), itp1(*),
     *        icr1(*), isn1(*), icn1(*)
      real*8 xic(*), yic(*), zic(*)
      real*8 vels(3,*)
      real*8 dens(*), pres(*)
      real*8 ener(*),rout,out(*)
      integer itetclr(*)
      integer itet(*), jtet(*)
C
      character*32 isubname, iprtname, jfile, cmo, cvelnm,
     *        cpresnm, cdensnm, cenernm,geom_name
 
      character*132 logmess
      logical iscontact
      integer icscode,ir,iclr,i1,icontact,index,ishape,i,it,
     *  nregions,iere,ierd,ierp,ier,itype,lout,iout,ierror,
     *  iunit,nnodes,length,icmotype,nelements,mbndry,
     *  nsdtopo,nsdgeom,icharlnf,lenfile,ilen,ityp,ierr
C
      pointer (iplocation, location)
      integer location(10000000)
C
      integer iconn(100)
C
      integer jshape(6)
      data jshape / 3, 4, 4, 5, 6, 6 /
      integer x3d_to_simul_hex(8), simul_to_x3d_hex(8)
C                             1  2  3  4  5  6  7  8
      data x3d_to_simul_hex / 1, 2, 4, 5, 3, 6, 8, 7 /
      data simul_to_x3d_hex / 1, 2, 5, 3, 4, 6, 8, 7 /

C
      lenfile=icharlnf(ifile)
      jfile=ifile(1:lenfile) // '.geo'
      iunit=-1
      call hassign(iunit,jfile,ierror)
C
C get current mesh info
C
      isubname='dumpdatex'
C
      call cmo_get_name(cmo,ierror)
C
      call cmo_get_info('nnodes',cmo,
     *                  nnodes,length,icmotype,ierror)
      call cmo_get_info('nelements',cmo,
     *                  nelements,length,icmotype,ierror)
      call cmo_get_info('mbndry',cmo,
     *                  mbndry,length,icmotype,ierror)
      call cmo_get_info('ndimensions_topo',cmo,
     *                  nsdtopo,length,icmotype,ierror)
      call cmo_get_info('ndimensions_geom',cmo,
     *                  nsdgeom,length,icmotype,ierror)
      call cmo_get_info('nodes_per_element',cmo,
     *                  nen,length,icmotype,ierror)
      call cmo_get_info('faces_per_element',cmo,
     *                  nef,length,icmotype,ierror)
      call cmo_get_info('imt1',cmo,ipimt1,ilen,ityp,ierr)
      call cmo_get_info('itp1',cmo,ipitp1,ilen,ityp,ierr)
      call cmo_get_info('icr1',cmo,ipicr1,ilen,ityp,ierr)
      call cmo_get_info('isn1',cmo,ipisn1,ilen,ityp,ierr)
      call cmo_get_info('icn1',cmo,ipicn1,ilen,ityp,ierr)
      if( ierr.ne.0) then
         write(logmess,'("No contact attribute in dumpdatex")')
         call writloga('default',0,logmess,0,ierr)
         iscontact=.false.
      else
         iscontact=.true.
      endif
      call cmo_get_info('xic',cmo,ipxic,ilen,ityp,ierr)
      call cmo_get_info('yic',cmo,ipyic,ilen,ityp,ierr)
      call cmo_get_info('zic',cmo,ipzic,ilen,ityp,ierr)
      call cmo_get_attinfo('presname',cmo,iout,rout,cpresnm,
     *   ipout,lout,itype,ier)
      call cmo_get_info(cpresnm,cmo,ippres,ilen,ityp,ierp)
      call cmo_get_attinfo('densname',cmo,iout,rout,cdensnm,
     *   ipout,lout,itype,ier)
      call cmo_get_info(cdensnm,cmo,ipdens,ilen,ityp,ierd)
      call cmo_get_attinfo('enername',cmo,iout,rout,cenernm,
     *   ipout,lout,itype,ier)
      call cmo_get_info(cenernm,cmo,ipener,ilen,ityp,iere)
      call cmo_get_info('itetclr',cmo,
     *                        ipitetclr,ilen,ityp,ierr)
      call cmo_get_info('itet',cmo,ipitet,ilen,ityp,ierr)
      call cmo_get_info('jtet',cmo,ipjtet,ilen,ityp,ierr)
C
      nsd=nsdtopo
C
      nregions=0
      do it=1,nelements
         nregions=max(nregions,itetclr(it))
      enddo
      write(iunit,"('DATEX2.1')")
      write(iunit,"('Geometry')")
      write(iunit,"(a9)") '"LaGriT1.0"'
      write(iunit,"(a28)") '"SIMUL file written by LaGriT"'
      write(iunit,*) nsd
      write(iunit,*) nnodes
      write(iunit,*) nelements
      write(iunit,*) nregions
      write(iunit,"('Node')")
      write(iunit,*)
      if(nsd.eq.3) then
         do i=1,nnodes
            write(iunit,9000) xic(i),yic(i),zic(i)
9000        format(3(1pe15.7))
         enddo
      elseif(nsd.eq.2) then
         do i=1,nnodes
            write(iunit,9000) xic(i),yic(i)
         enddo
      endif
      if(nsd.eq.2.and.nen.eq.3.and.nef.eq.3) ishape=1
      if(nsd.eq.3.and.nen.eq.3.and.nef.eq.3) ishape=1
      if(nsd.eq.2.and.nen.eq.4.and.nef.eq.4) ishape=2
      if(nsd.eq.3.and.nen.eq.4.and.nef.eq.4) ishape=3
      if(nsd.eq.3.and.nen.eq.5.and.nef.eq.5) ishape=4
      if(nsd.eq.3.and.nen.eq.6.and.nef.eq.5) ishape=5
      if(nsd.eq.3.and.nen.eq.8.and.nef.eq.6) ishape=6
      write(iunit,"('Element')")
      write(iunit,*)
      do it=1,nelements
         index=nen*(it-1)
         if(ishape.eq.6) then
            do i=1,nen
               iconn(i)=itet(index+x3d_to_simul_hex(i))-1
            enddo
         else
            do i=1,nen
               iconn(i)=itet(index+i)-1
            enddo
         endif
         icontact=0
         do i=1,nen
            i1=itet(index+i)
            if(iscontact)icontact=max(icontact,icn1(i1))
         enddo
         icontact=icontact
         iclr=itetclr(it)-1
         write(iunit,*) ishape,icontact,iclr
         write(iunit,*) (iconn(i),i=1,nen)
      enddo
      write(iunit,"('Region')")
      write(iunit,*)
      call cmo_get_attinfo('geom_name',cmo,iout,rout,geom_name,
     *   ipout,lout,itype,ierr)      
      call mmfindbk('cmregs',geom_name,ipcmregs,lout,ierr)
      do ir=1,nregions
         iprtname=cmregs(ir)
      enddo
      length=nnodes
      call mmgetblk("location",isubname,iplocation,length,2,icscode)
      write(iunit,"('Location')")
      do i=1,nnodes
         if (iscontact) then
            location(i)=-2
            if(itp1(i).eq.0) then
               location(i)=-2
            elseif(itp1(i).eq.2.or.itp1(i).eq.12) then
               location(i)=-1
            elseif(itp1(i).ge.10.and.itp1(i).le.19) then
               location(i)=icn1(i)
            endif

            if(icn1(i).eq.0) then
               location(i)=-2
            else
               location(i)=icn1(i)
            endif
         else
            location(i)=0
         endif
      enddo
      write(iunit,"(20i5)") (location(i),i=1,nnodes)
      close(iunit)
      iunit=-1
      lenfile=icharlnf(ifile)
      jfile=ifile(1:lenfile) // '.dat'
      call hassign(iunit,jfile,ierror)
      write(iunit,"('DATEX2.1')")
      write(iunit,"('Simulation')")
      write(iunit,"(a8)") '"X3D1.0"'
      write(iunit,"(a27)") '"SIMUL file written by X3D"'
      write(iunit,*) 4
      write(iunit,"('DopingConcentration scalar node')")
      write(iunit,"('BoronConcentration scalar node')")
      write(iunit,"('TotalConcentration scalar node')")
      write(iunit,"('Velocity vector node')")
      if(ierd.eq.0) then
      write(iunit,"('Dataset')")
      do i=1,nnodes
         write(iunit,9000) dens(i)
      enddo
      write(iunit,"('0')")
      endif
      if(ierp.eq.0) then
      write(iunit,"('Dataset')")
      do i=1,nnodes
         write(iunit,9000) pres(i)
      enddo
      write(iunit,"('0')")
      endif
      if(iere.eq.0) then
      write(iunit,"('Dataset')")
      do i=1,nnodes
         write(iunit,9000) ener(i)
      enddo
      write(iunit,"('0')")
      endif
      call cmo_get_attinfo('velname',cmo,iout,rout,cvelnm,
     *   ipout,lout,itype,ier)
      call cmo_get_info(cvelnm,cmo,ipvels,length,itype,ier)
      if(ier.eq.0) then
      write(iunit,"('Dataset')")
      do i=1,nnodes
         write(iunit,9000) vels(1,i),vels(2,i),vels(3,i)
      enddo
      write(iunit,"('0')")
      endif
      close(iunit)
      goto 9999
 9999 continue
      call mmrelprt(isubname,icscode)
      return
      end
