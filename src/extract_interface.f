*dk,extract_interface
      subroutine extract_interface(cmoout,cmoin,npoints,ntets,
     *                             ipjalias,
     *                             ipartindx,jpartindx,interf,ierr1)
C
C #####################################################################
C
C     PURPOSE -
C
C        Extracts a surface along a material interface, possibly
C        just that part that is across from another designated
C        interface.
C
C     INPUT ARGUMENTS -
C
C        cmoout    - THE MESH OBJECT TO HOLD THE RESULT
C        cmoin     - THE INCOMING MESH OBJECT
C        npoints   - NUMBER OF POINTS
C        ntets     - NUMBER OF TETS
C        ipjalias   - POINTER TO ARRAY OF POINTS TO BE CONSIDERED
C        ipartindx - MATERIAL NUMBER FOR THE FIRST REGION
C        jpartindx - MATERIAL NUMBER FOR THE SECOND REGION
C        interf    - 1 IF THERE IS ONE REGION, 2 IF TWO REGIONS
C
C     OUTPUT ARGUMENTS -
C
C        ierr1   - ERROR RETURNED (ZERO IF NO ERRORS)
C
C     CHANGE HISTORY -
C
C$Log:   /pvcs.config/t3d/src/extract_interface.f_a  $
CPVCS    
CPVCS       Rev 1.23   17 Jun 2003 16:15:42   dcg
CPVCS    do not release cmo if it exists - doing this means that added attributes will be
CPVCS    lost when doing the extraction
CPVCS    
CPVCS       Rev 1.22   03 Oct 2000 10:03:58   dcg
CPVCS    use temporary array ialiastmp in place of ialias
CPVCS    mesh object attribute
CPVCS
CPVCS       Rev 1.21   21 Apr 2000 07:05:08   gable
CPVCS    Made setting and getting of mbndry value dynamic and problem size dependent.
CPVCS
CPVCS       Rev 1.20   Mon Apr 14 16:45:02 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.19   Mon Apr 07 14:54:22 1997   het
CPVCS    Correct an error in the 1D extract option.
CPVCS
CPVCS       Rev 1.18   Wed Jul 24 17:32:12 1996   dcg
CPVCS    use mesh object 'nef' attribute to pack element and
CPVCS    face number into jtet array
CPVCS
CPVCS       Rev 1.17   Mon Mar 04 11:11:10 1996   dcg
CPVCS    remove icn1, int1 unused in this routine
CPVCS
CPVCS       Rev 1.16   Fri Dec 22 14:23:00 1995   het
CPVCS    Correct an error
CPVCS    .
CPVCS
CPVCS       Rev 1.15   12/15/95 11:36:44   het
CPVCS    Correct some extract_interface problems for hexes
CPVCS
CPVCS       Rev 1.14   12/14/95 10:39:14   jxf
CPVCS    Fixed to allow mixed or non-tet grids.  Also cleaned up.
CPVCS
CPVCS       Rev 1.13   11/07/95 17:17:08   dcg
CPVCS    change flag to 2 in mmgetblk calls
CPVCS
CPVCS       Rev 1.12   10/03/95 10:39:54   dcg
CPVCS    IBM changes declare character variables
CPVCS
CPVCS       Rev 1.11   10/02/95 09:24:10   het
CPVCS    Correct point type and constraint selection for derived MO
CPVCS
CPVCS       Rev 1.10   09/18/95 19:43:50   dcg
CPVCS    look for mesh object added attributes
CPVCS
CPVCS       Rev 1.9   08/23/95 13:52:14   jxf
CPVCS    added -all- option to INTRFAC2
CPVCS
CPVCS       Rev 1.8   07/14/95 10:16:12   het
CPVCS    Correct errors with point types
CPVCS
CPVCS       Rev 1.7   06/21/95 13:19:42   het
CPVCS    Replace the character literal by a character variable in the call to hgetprt.f and add the hybrid grid arrays
CPVCS
CPVCS       Rev 1.6   06/02/95 08:43:34   jxf
CPVCS    Changed multiple returns to one return
CPVCS
CPVCS       Rev 1.5   05/17/95 09:34:56   dcg
CPVCS    remove cmo_get_name for old cmo
CPVCS
CPVCS       Rev 1.4   05/16/95 13:40:56   ejl
CPVCS    Fixed uninitialized ierror=0
CPVCS
CPVCS       Rev 1.3   04/02/95 11:28:18   jxf
CPVCS
CPVCS
CPVCS       Rev 1.2   02/27/95 11:26:42   jxf
CPVCS    Added cmoin.  Changed 2-region intrface to intrfac2.
CPVCS
CPVCS       Rev 1.1   02/13/95 09:18:56   jxf
CPVCS    New routine.
CPVCS
CPVCS       Rev 1.0   02/10/95 14:23:46   dcg
CPVCS
CPVCS
CPVCS       Rev 1.0   02/10/95 13:58:44   dcg
CPVCS
C
C ######################################################################
C
      implicit real*8 (a-h, o-z)
C
      include "local_element.h"
      include "chydro.h"
C
      parameter (nplen=1000000)
C
C   Definitions for incoming (existing) cmo
C
      pointer (ipialias, ialiastmp)
      integer ialiastmp(nplen)
      pointer (ipitetclr, itetclr)
      pointer (ipitettyp, itettyp)
      pointer (ipitetoff, itetoff)
      pointer (ipjtetoff, jtetoff)
      pointer (ipitet1, itet1)
      pointer (ipjtet1, jtet1)
      integer itetclr(nplen), itettyp(nplen),
     *        itetoff(nplen), jtetoff(nplen)
      integer itet1(nplen) , jtet1(nplen)
C
C   Definitions for cmo that is to be created
C
      pointer (ipitp1a, itp1a)
      pointer (ipicr1a, icr1a)
      integer itp1a(nplen), icr1a(nplen)
      pointer (ipitetclra, itetclra)
      pointer (ipitettypa, itettypa)
      pointer (ipitetoffa, itetoffa)
      pointer (ipjtetoffa, jtetoffa)
      pointer (ipiteta1, iteta1)
      pointer (ipjteta1, jteta1)
      integer itetclra(nplen), itettypa(nplen),
     *        itetoffa(nplen), jtetoffa(nplen)
      integer iteta1(nplen) , jteta1(nplen)
C
      pointer (ipidone, idone)
      integer idone(nplen)
C
      pointer (ipjtet2, jtet2)
      integer jtet2(3,nplen)
      pointer(ipjalias, jalias(nplen))
 
      pointer(iplist,list)
      pointer(ipilist,ilist)
      pointer(ipxweight,xweight)
      integer ilist(npoints), list(npoints)
      real*8 xweight(npoints)
      character*132 logmess
      character*(*) cmoin, cmoout
      character*32 isubname
C
      dimension inod(4), jpnt1(4)
C
      isubname = 'extract_interface'
      ierr1 = 0
C
C   Get the existing cmo
C
      call cmo_get_info('mbndry',cmoin,mbndry,length,icmotype,
     *			ierror)
      call cmo_get_info('nodes_per_element',cmoin,nen,lenf,iftyp,
     * 			ierror)
      call cmo_get_info('faces_per_element',cmoin,nef,lenf,iftyp,
     * 			ierror)
      call cmo_get_info('ndimensions_topo',cmoin,nsdtopo,lenf,iftyp,
     * 			ierror)
      call cmo_get_info('itetclr',cmoin,ipitetclr,lenitetclr,icmotype,
     *			ierror)
      call cmo_get_info('itettyp',cmoin,ipitettyp,lenitettyp,icmotype,
     *			ierror)
      call cmo_get_info('itetoff',cmoin,ipitetoff,lenitetoff,icmotype,
     *			ierror)
      call cmo_get_info('jtetoff',cmoin,ipjtetoff,lenjtetoff,icmotype,
     *			ierror)
      call cmo_get_info('itet',cmoin,ipitet1,lenitet1,icmotype,ierror)
      call cmo_get_info('jtet',cmoin,ipjtet1,lenjtet1,icmotype,ierror)
C
C   Create the new cmo.
C
      call cmo_exist(cmoout,ierror)
C
C   ierror.eq.0 means that the cmo already exists.
C
      nface_max=nef*ntets
C
C     Assign the nodes-per-element and faces-per-element to
C        indicate a "hybrid" grid.
C
      if(nsdtopo.eq.2.and.nen.eq.3.and.nef.eq.3) then
         nenout=nelmnen(ifelmlin)
         nefout=nelmnef(ifelmlin)
      elseif(nsdtopo.eq.2.and.nen.eq.4.and.nef.eq.4) then
         nenout=nelmnen(ifelmlin)
         nefout=nelmnef(ifelmlin)
      elseif(nsdtopo.eq.3.and.nen.eq.4.and.nef.eq.4) then
         nenout=nelmnen(ifelmtri)
         nefout=nelmnef(ifelmtri)
      elseif(nen.eq.8.and.nef.eq.6) then
         nenout=nelmnen(ifelmqud)
         nefout=nelmnef(ifelmqud)
      else
         nenout=nelmnen(ifelmhyb)
         nefout=nelmnef(ifelmhyb)
      endif
C
C     if(ierror.eq.0) call cmo_release(cmoout,idelete)
      call cmo_set_name(cmoout,ierror)
      call cmo_set_info('nnodes',cmoout,npoints,1,1,ierror)
      call cmo_set_info('nelements',cmoout,nface_max,1,1,ierror)
      call cmo_set_info('ndimensions_topo',cmoout,2,1,1,ierror)
      call cmo_set_info('ndimensions_geom',cmoout,3,1,1,ierror)
      call cmo_set_info('nodes_per_element',cmoout,nenout,1,1,ierror)
      call cmo_set_info('faces_per_element',cmoout,nefout,1,1,ierror)
C
      call cmo_newlen(cmoout,ierror)
C
      call cmo_get_info('itp1',cmoout,ipitp1a,lenitp1a,icmotype,ierror)
      call cmo_get_info('icr1',cmoout,ipicr1a,lenicr1a,icmotype,ierror)
      call cmo_get_info('isn1',cmoout,ipisn1a,lenisn1a,icmotype,ierror)
      call cmo_get_info('itetclr',cmoout,ipitetclra,lenitetclra,
     *			icmotype,ierror)
      call cmo_get_info('itettyp',cmoout,ipitettypa,lenitettypa,
     *			icmotype,ierror)
      call cmo_get_info('itetoff',cmoout,ipitetoffa,lenitetoffa,
     *			icmotype,ierror)
      call cmo_get_info('jtetoff',cmoout,ipjtetoffa,lenitetoffa,
     *			icmotype,ierror)
      call cmo_get_info('itet',cmoout,ipiteta1,leniteta1,icmotype,
     *			ierror)
      call cmo_get_info('jtet',cmoout,ipjteta1,lenjteta1,icmotype,
     *			ierror)
C
C  get temporary work space
      call mmgetblk('list',isubname,iplist,npoints,2,ierror)
      call mmgetblk('ialiastmp',isubname,ipialias,npoints,2,ierror)
      call mmgetblk('ilist',isubname,ipilist,npoints,2,ierror)
      call mmgetblk('xweight',isubname,ipxweight,npoints,2,ierror)
      do i=1,npoints
         ialiastmp(i) = 0
      enddo
C
      npointsn = 0
      ntetsn = 0
      mbndry2 = mbndry
      itoff = 0
      jtoff = 0
C
C   Loop through the tets of the incoming cmo and find those with
C   the sought-for material number.  ipartindx.eq. -1 means that
C   the input line has '-all-' for the partname.
C
      do it=1,ntets
         if( (itetclr(it).eq.ipartindx) .or.
     *       (ipartindx.eq. -1) ) then
            do i=1,nelmnef(itettyp(it))
               if(jtet1(jtetoff(it)+i).ge.mbndry) then
C
C   Is this tet on a boundary (interface)?  If so, then if this is
C   a dual-interface request, look at a point in the tet across the
C   interface to assure that it is of the other material type.
C
                  if((interf.eq.1) .or.
     *               ((interf.eq.2) .and.
     *                (jtet1(jtetoff(it)+i).gt.mbndry2) .and.
     *		      ( (itetclr(1 + (jtet1(jtetoff(it)+i) - mbndry
     *			- 1)/nef)
     *                   .eq. jpartindx) .or. (jpartindx .eq. -1) )
     *               )
     *              ) then
C
C   If we get here, we have an appropriate interface tet.  Find the
C   nodes of the face on the interface.
C
                     isgood = 1
		     do j1=1,ielmface0(i,itettyp(it))
			inod(j1) = itet1(itetoff(it)+
     *			           ielmface1(j1,i,itettyp(it)))
			if (jalias(inod(j1)) .ne. 1) isgood = 0
		     enddo
C
                     if (isgood .eq. 1) then
		        do j1=1,ielmface0(i,itettyp(it))
C
C   If (ialiastmp(inod(j1)).eq.0) we haven't seen this point before, so
C   assign it the next sequence number.
C
			   if (ialiastmp(inod(j1)) .eq. 0) then
                              npointsn = npointsn+1
                              list(npointsn)=npointsn
                              ilist(npointsn)=inod(j1)
                              xweight(npointsn)=1.
                              ialiastmp(inod(j1))= npointsn
                              jpnt1(j1) = npointsn
                           else
C
C   ialiastmp contained the point number in the new cmo for this point;
C   i.e.:  it was previously found.  Use it now for jpnt1.
C
                              jpnt1(j1) = ialiastmp(inod(j1))
                           endif
			enddo
C
C   Make a new tet (triangle) in the new cmo.
C
                     ntetsn = ntetsn + 1
                     itetclra(ntetsn) = itetclr(it)
                     if(ielmface0(i,itettyp(it)).eq.2) then
                        itettypa(ntetsn) = ifelmlin
                     elseif(ielmface0(i,itettyp(it)).eq.3) then
                        itettypa(ntetsn) = ifelmtri
                     elseif(ielmface0(i,itettyp(it)).eq.4) then
                        itettypa(ntetsn) = ifelmqud
                     endif
                     itetoffa(ntetsn) = itoff
                     itoff = itoff + nelmnen(itettypa(ntetsn))
                     jtetoffa(ntetsn) = jtoff
                     jtoff = jtoff + nelmnef(itettypa(ntetsn))
		     do j1=1,ielmface0(i,itettyp(it))
                        iteta1(itetoffa(ntetsn)+j1) = jpnt1(j1)
		     enddo
                  endif
C
C   end "if (isgood .eq. 1)"
C
             endif
               endif
C
C   end "do i=1,nelmnef(itettyp(it))"
C
            enddo
C
C   end "if( (itetclr(it).eq.ipartindx) then"
C
         endif
C
C    end "do it=1, ntets"
C
      enddo
C
C   Update the new cmo.
C
      call cmo_set_info('nnodes',cmoout,npointsn,1,1,ierror)
      call cmo_set_info('nelements',cmoout,ntetsn,1,1,ierror)
      call cmo_newlen(cmoout,ierror)
      call cmo_get_intinfo('mbndry',cmoout,mbndry,
     *                  lenitp1a,icmotype,ierror)
 
 
      call cmo_interpolate(cmoout,cmoin,'nnodes',npointsn,1,
     *                     list,ilist,xweight,ierror)
C
C   If no new tets were found, this is an empty part.
C
      if(ntetsn.eq.0) then
         ierr1 = 1
         write(logmess,'(a)')
     *     'Nothing found for new mesh object.'
          call writloga('default',0,logmess,0,ierr1)
         goto 9999
      endif
C
C   Figure out the new jtet array.
C
C
      length = 3*ntetsn
      call mmgetblk('jtet2',isubname,ipjtet2,length,2,
     *		       icscode)
C
C   Refresh some pointers
C
      call cmo_get_info('itp1',cmoout,ipitp1a,lenitp1a,icmotype,ierror)
      call cmo_get_info('itet',cmoout,ipiteta1,leniteta1,icmotype,
     *			ierror)
      call cmo_get_info('jtet',cmoout,ipjteta1,lenjteta1,icmotype,
     *			ierror)
C
      do it=1,ntetsn
         do i=1,nelmnef(itettypa(it))
            jteta1(jtetoffa(it)+i) = -1
            jtet2(i,it) = -1
         enddo
      enddo
C
      call geniee_cmo(cmoout)
C
C
C
C     ...............................................................
C     SET THE EXTERNAL BOUNDARY NODE TYPE BASED ON BOUNDARY FACES.
C
      call cmo_get_info('nnodes',cmoout,npointsn,length,icmotype,ier)
      call cmo_get_info('nelements',cmoout,ntetsn,length,icmotype,ier)
      call cmo_get_info('itp1',cmoout,ipitp1a,ilen,ityp,ier)
      call cmo_get_info('icr1',cmoout,ipicr1a,ilen,ityp,ier)
      call cmo_get_info('itetclr',cmoout,ipitetclra,ilen,ityp,ier)
      call cmo_get_info('itettyp',cmoout,ipitettypa,ilen,ityp,ier)
      call cmo_get_info('itetoff',cmoout,ipitetoffa,ilen,ityp,ier)
      call cmo_get_info('jtetoff',cmoout,ipjtetoffa,ilen,ityp,ier)
      call cmo_get_info('itet',cmoout,ipiteta1,ilen,ityp,ier)
      call cmo_get_info('jtet',cmoout,ipjteta1,ilen,ityp,ier)
C
      length=npointsn
      call mmgetblk('idone',isubname,ipidone,length,2,icscode)
C
      do i=1,npointsn
         idone(i)=0
         itp1a(i)=0
      enddo
C
      do it=1,ntetsn
         do i=1,nelmnef(itettypa(it))
            if (jteta1(jtetoffa(it)+i).ge.mbndry) then
               do j=1,ielmface0(i,itettypa(it))
                  node1 = iteta1(itetoffa(it)+
     *                           ielmface1(j,i,itettypa(it)))
                  itp1a(node1)=ifitprfl
               enddo
            endif
         enddo
      enddo
C
      do it=1,ntetsn
         do i=1,nelmnef(itettypa(it))
            if (jteta1(jtetoffa(it)+i).gt.0.and.
     *          jteta1(jtetoffa(it)+i).lt.mbndry) then
               jt=1+(jteta1(jtetoffa(it)+i)-1)/nefout
               jf=jteta1(jtetoffa(it)+i)-nefout*(it-1)
               if(itetclra(it).ne.itetclra(jt)) then
                  do j=1,ielmface0(i,itettypa(it))
                     node1=iteta1(itetoffa(it)+
     *                            ielmface1(j,i,itettypa(it)))
                     if(idone(node1).eq.0) then
                        idone(node1)=1
                        if(itp1a(node1).eq.ifitprfl) then
                           itp1a(node1)=ifitpinb
                        else
                           itp1a(node1)=ifitpini
                        endif
                     endif
                  enddo
               endif
            endif
         enddo
      enddo
C
      do i=1,npointsn
         if(itp1a(i).eq.ifitpint.or.itp1a(i).eq.ifitpini) icr1a(i)=0
      enddo
C
C
C     ******************************************************************
C     Set the mbndry flag at interface faces.
C
      do it=1,ntetsn
         do i=1,nelmnef(itettypa(it))
            index=jtetoffa(it)+i
            if(jteta1(index).gt.0.and.jteta1(index).lt.mbndry) then
               jt=1+(jteta1(index)-1)/nelmnef(itettypa(it))
               jf=jteta1(index)-nelmnef(itettypa(it))*(jt-1)
               if(itetclra(it).ne.itetclra(jt)) then
                  jteta1(index)=mbndry+nelmnef(itettypa(it))*(jt-1)+jf
                  jteta1(jtetoffa(jt)+jf)=mbndry+index
               endif
            endif
         enddo
      enddo
C
      goto 9999
 9999 continue
C
      call mmrelprt(isubname,icscode)
C
      return
      end
