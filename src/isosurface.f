      subroutine isosurface(ipfield,value,npoints,ntets,
     *                              cmoin,cmoout,ierr1)
C
C #####################################################################
C
C     PURPOSE -
C
C        Extracts an isosurface.                                              
C
C     INPUT ARGUMENTS -
C
C        ipfield   - POINTER TO FIELD TO BE ISOSURFACED
C        value     - VALUE OF FIELD ON ISOSURFACE
C        npoints   - NUMBER OF POINTS
C        ntets     - NUMBER OF TETS
C        cmoin     - NAME OF THE EXISTING MESH OBJECT
C        cmoout    - NAME OF THE NEW MESH OBJECT
C
C     OUTPUT ARGUMENTS -
C
C        ierr1   - ERROR RETURNED (ZERO IF NO ERRORS)
C
C     CHANGE HISTORY -
C  $Log:   /pvcs.config/t3d/src/isosurface.f_a  $
CPVCS    
CPVCS       Rev 1.30   03 Mar 2006 09:16:22   gable
CPVCS    Fixed problem with the case where no elements were created. For
CPVCS    example in the case where a plane is extracted from a 3D object
CPVCS    but the plane does not intersect the 3D object anywhere. In that
CPVCS    case a new mesh object was created that was 2*nnodes and 2*nelements
CPVCS    of the input mesh object but there was nothing in the output mesh
CPVCS    object. Now the output mesh object is created but nnodes=nelements=0.
CPVCS    
CPVCS       Rev 1.29   27 Jul 2001 10:19:24   dcg
CPVCS    set edges_per_element
CPVCS    make code implicit none
C
C
C ######################################################################
C
      implicit none
      real*8 value
      integer npoints,ntets,ierr1
C
      include "local_element.h"
      include "chydro.h"
      common /mcube/ isotyp(368), isodat(8,368)
      integer isotyp,isodat
C
      integer nplen
      parameter (nplen=1000000)
      integer itetbgn, ihexbgn, ipyrbgn, iprsmbgn
      parameter(itetbgn=0)
      parameter(ihexbgn=16)
      parameter(ipyrbgn=272)
      parameter(iprsmbgn=304)
C
C   Definitions for incoming (existing) cmo
C
      pointer (ipfield,field)
      real*8 field(nplen)
      pointer (ipimt1, imt1)
      pointer (ipitp1, itp1)
      pointer (ipicr1, icr1)
      pointer (ipisn1, isn1)
      integer imt1(nplen), itp1(nplen),
     *        icr1(nplen), isn1(nplen)
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      real*8 xic(nplen), yic(nplen), zic(nplen)
      pointer (ipitetclr, itetclr)
      pointer (ipitettyp, itettyp)
      pointer (ipitetoff, itetoff)
      pointer (ipjtetoff, jtetoff)
      pointer (ipitet1, itet1)
      pointer (ipjtet1, jtet1)
      integer itetclr(nplen), itettyp(nplen),
     *     itetoff(nplen), jtetoff(nplen)
      integer itet1(nplen) , jtet1(nplen)
C
C   Definitions for cmo that is to be created
C
      pointer (ipimt1a, imt1a)
      pointer (ipitp1a, itp1a)
      pointer (ipicr1a, icr1a)
      pointer (ipisn1a, isn1a)
      integer imt1a(nplen), itp1a(nplen),
     *        icr1a(nplen), isn1a(nplen)
      pointer (ipxica, xica)
      pointer (ipyica, yica)
      pointer (ipzica, zica)
      real*8 xica(nplen), yica(nplen), zica(nplen)
      pointer (ipitetclra, itetclra)
      pointer (ipitettypa, itettypa)
      pointer (ipitetoffa, itetoffa)
      pointer (ipjtetoffa, jtetoffa)
      pointer (ipiteta1, iteta1)
      pointer (ipjteta1, jteta1)
      integer itetclra(nplen), itettypa(nplen),
     *      itetoffa(nplen), jtetoffa(nplen)
      integer iteta1(nplen) , jteta1(nplen)
C
      pointer (ipint1a, int1a)
      integer int1a(nplen)
C
      pointer (ipidone, idone)
      integer idone(nplen)
C
      real*8 xc(6),yc(6),zc(6),fv(8), cdist(6)
      integer newpt(6),ipn(6),nsides(3)

C
C  Pointers for temporary storage for lists to pass to cmo_interpolate
       pointer(iplist,list)
       pointer(ipilist,ilist)
       pointer(ipxweight,xweight)
       integer list(2*npoints), ilist(2,2*npoints)
       real*8 xweight(2,2*npoints)
C
      character*132 logmess
      character*(*) cmoin, cmoout
      character*32 isubname
      integer iedg(3,6,2)
      integer i,ieltyp,igflag, ieflag, neq, icase, iaoffset
      real*8 x1,y1,z1,x2,fvmin,sdistsign,xmax,ymax,zmax,
     * xmin,ymin,zmin,epsilone,sdist,vol,xpt,ypt,zpt,a,b,c,d,
     * x3,y3,z3,y2,z2
      integer ioff,m,iskipit,itri,ichk,ifoundit,ip1,iv2,iv1,is,ipoly,
     * icorner,npolys,in,it,numquads,numtris,jtindex,itindex,icscode,
     * leni,idelete,nefout,nenout,neeout,nface_max,mbndry,length,
     * icmotype,ierror,nen,nef,nsdtopo,ntetsn,npointsn,nvalue,nlist,
     * i1,icount,ierrdum,index,jf,jt,node1,j,ityp,ilen,ier,n4,n3,n2,
     * ip2,jchk
c
c
c
C  initialize
      nlist=0
      nvalue=2
      isubname = 'isosurface'
      epsilone = 1.0e-10
      ierr1 = 0
      npointsn = 0
      ntetsn = 0
C
C   Get the existing cmo
C
      call cmo_get_info('mbndry',cmoin,mbndry,leni,icmotype,
     *			ierror)
      call cmo_get_info('nodes_per_element',cmoin,nen,leni,
     * 			icmotype,ierror)
      call cmo_get_info('faces_per_element',cmoin,nef,leni,
     * 			icmotype,ierror)
      call cmo_get_info('ndimensions_topo',cmoin,nsdtopo,leni,
     * 			icmotype,ierror)
      call cmo_get_info('imt1',cmoin,ipimt1,leni,icmotype,ierror)
      call cmo_get_info('itp1',cmoin,ipitp1,leni,icmotype,ierror)
      call cmo_get_info('icr1',cmoin,ipicr1,leni,icmotype,ierror)
      call cmo_get_info('isn1',cmoin,ipisn1,leni,icmotype,ierror)
      call cmo_get_info('xic',cmoin,ipxic,leni,icmotype,ierror)
      call cmo_get_info('yic',cmoin,ipyic,leni,icmotype,ierror)
      call cmo_get_info('zic',cmoin,ipzic,leni,icmotype,ierror)
      call cmo_get_info('itetclr',cmoin,ipitetclr,leni,icmotype,
     *			ierror)
      call cmo_get_info('itettyp',cmoin,ipitettyp,leni,icmotype,
     *			ierror)
      call cmo_get_info('itetoff',cmoin,ipitetoff,leni,icmotype,
     *			ierror)
      call cmo_get_info('jtetoff',cmoin,ipjtetoff,leni,icmotype,
     *			ierror)
      call cmo_get_info('itet',cmoin,ipitet1,leni,icmotype,
     *			ierror)
      call cmo_get_info('jtet',cmoin,ipjtet1,leni,icmotype,
     *			ierror)
C
C
C   Get the problem size.
C
      xmin = 1.0e30
      ymin = xmin
      zmin = xmin
      xmax = -xmin
      ymax = -xmin
      zmax = -xmin
      do i=1,npoints
         if (itp1(i).eq.10) then
            if ( xic(i).lt.xmin ) xmin = xic(i)
            if ( xic(i).gt.xmax ) xmax = xic(i)
            if ( yic(i).lt.ymin ) ymin = yic(i)
            if ( yic(i).gt.ymax ) ymax = yic(i)
            if ( zic(i).lt.zmin ) zmin = zic(i)
            if ( zic(i).gt.zmax ) zmax = zic(i)
         endif
      enddo
C
C   Just in case there are no type 10 points ....
C
      if ( (xmax+1.) .lt. xmin ) then
         do i=1,npoints
            if ( xic(i).lt.xmin ) xmin = xic(i)
            if ( xic(i).gt.xmax ) xmax = xic(i)
            if ( yic(i).lt.ymin ) ymin = yic(i)
            if ( yic(i).gt.ymax ) ymax = yic(i)
            if ( zic(i).lt.zmin ) zmin = zic(i)
            if ( zic(i).gt.zmax ) zmax = zic(i)
         enddo
      endif
C
          epsilone = 1.0e-6*sqrt((xmax-xmin)**2 + (ymax-ymin)**2 +
     *                       (zmax-zmin)**2)
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
         nenout=nelmnen(ifelmhyb)
         nefout=nelmnef(ifelmhyb)
         neeout=nelmnee(ifelmhyb)

C
      if(ierror.eq.0) call cmo_release(cmoout,idelete)
      call cmo_derive(cmoout,cmoin,ierror)
C
C   Make room for twice the existing number of points and tets.
C   This should be enough.
C
      call cmo_set_info('nnodes',cmoout,2*npoints,1,1,ierror)
      call cmo_set_info('nelements',cmoout,2*ntets,1,1,ierror)
      call cmo_set_info('ndimensions_topo',cmoout,2,1,1,ierror)
      call cmo_set_info('ndimensions_geom',cmoout,3,1,1,ierror)
      call cmo_set_info('nodes_per_element',cmoout,nenout,1,1,ierror)
      call cmo_set_info('faces_per_element',cmoout,nefout,1,1,ierror)
      call cmo_set_info('edges_per_element',cmoout,neeout,1,1,ierror)
C
      call cmo_newlen(cmoout,ierror)
C
      call cmo_get_info('imt1',cmoout,ipimt1a,leni,icmotype,ierror)
      call cmo_get_info('itp1',cmoout,ipitp1a,leni,icmotype,ierror)
      call cmo_get_info('icr1',cmoout,ipicr1a,leni,icmotype,ierror)
      call cmo_get_info('isn1',cmoout,ipisn1a,leni,icmotype,ierror)
      call cmo_get_info('xic',cmoout,ipxica,leni,icmotype,ierror)
      call cmo_get_info('yic',cmoout,ipyica,leni,icmotype,ierror)
      call cmo_get_info('zic',cmoout,ipzica,leni,icmotype,ierror)
      call cmo_get_info('itetclr',cmoout,ipitetclra,leni,
     *			icmotype,ierror)
      call cmo_get_info('itettyp',cmoout,ipitettypa,leni,
     *			icmotype,ierror)
      call cmo_get_info('itetoff',cmoout,ipitetoffa,leni,
     *			icmotype,ierror)
      call cmo_get_info('jtetoff',cmoout,ipjtetoffa,leni,
     *			icmotype,ierror)
      call cmo_get_info('itet',cmoout,ipiteta1,leni,icmotype,
     *			ierror)
      call cmo_get_info('jtet',cmoout,ipjteta1,leni,icmotype,
     *			ierror)
C
C  get working space for interpolation
C
      call mmgetblk('list',isubname,iplist,2*npoints,2,icscode)
      call mmgetblk('ilist',isubname,ipilist,4*npoints,2,icscode)
      call mmgetblk('xweight',isubname,ipxweight,4*npoints,2,icscode)
C
C   Go through the elements and compute the isosurface.
C
C   The algorithm is "marching cubes".  First an 8-bit (or less)
C   quantity, igflag, is computed.  Each bit corresponds to one of the
C   nodes of the cell.  It is set to one if the field value at that
C   node is greater than or equal to the cnotour value.  There is a
C   similar variable, ieflag, that is set if the contour value just
C   equals the field value at the node.  The array value isotyp(igflag)
C   determines the type of polygon to be created.  For tets there are
C   two types.  For hexes there are 20 types.  The isodat(n,igflag)
C   array holds the starting and ending vertices for each cell edge
C   that is cut by the isosurface.  These are placed in the iedg array,
C   which is triply-dimensioned.  (dim1 = polygon number.  There can be
C   more than one polygon created by the intersection of an isosurface
C   with a cell.  dim2 = vertex number for this polygon.  dim3 = 1 or 2,
C   corresponding to the two nodes at the endpoints of this cell edge.
C
C   After the iedg array is filled up, points are added to the new cmo
C   if they aren't already there and new cells are added to the itet1
C   array for the new cmo.
C
      itindex = 0
      jtindex = 0
      numtris = 0
      numquads = 0
      do it=1,ntets
         do in=1,nelmnen(itettyp(it))
            fv(in) = field(itet1(itetoff(it)+in))
         enddo
         ieltyp = nelmnen(itettyp(it))
         iaoffset = 0
         if (ieltyp .eq. 8) then
            iaoffset = ihexbgn
         elseif (ieltyp .eq. 5) then
            iaoffset = ipyrbgn
         elseif (ieltyp .eq. 6) then
            iaoffset = iprsmbgn
         endif
         ieflag = 0
         igflag = 0
         neq = 0
         npolys = 0
         icorner = 0
         sdistsign = 1.0
         fvmin = 1.0d30
C
C   Put the vertices of this element in the fv array and compute
C   the offset into the table.
C
         do i=1,ieltyp
           if (fv(i) .ge. value) then
              igflag = igflag + 2**(i-1)
           else if (fv(i) .lt. fvmin) then
              fvmin = fv(i)
              icorner = i
           endif
           if (abs(fv(i)-value) .lt. 1.e-10) then
             neq = neq + 1
             ieflag = ieflag + 2**(i-1)
           endif
         enddo
         igflag = igflag + 1
         ieflag = ieflag + 1
C
C   If icorner .eq. 0, there were no nodes in this element with fv .lt.
C   value.  So we need to find a node with fv .gt. value and also reverse
C   the sign of sdist, when determining the handedness of the cmoout
C   element.
C
         if (icorner .eq. 0) then
            do i=1,ieltyp
               if (fv(i) .gt. value) then
                  icorner = i
                  sdistsign = -1.0
               endif
            enddo
         endif

C
C   Take care of the case where the isosurface just touches a face.
C
         if ( (ieltyp .eq. 4) .and. (neq .eq. 3) ) then
            igflag = ieflag
         else if ( (ieltyp .eq. 8) .and. (neq .eq. 4) ) then
            if ( (ieflag .eq. 16) .or. (ieflag .eq. 52) .or.
     *           (ieflag .eq. 241) .or. (ieflag .eq. 205) .or.
     *           (ieflag .eq. 103) .or. (ieflag .eq. 154) )
     *         igflag = ieflag
         else if ( (ieltyp .eq. 5) .and. (neq .eq. 4) ) then
            if (ieflag .eq. 31) igflag = ieflag
         else if ( (ieltyp .eq. 5) .and. (neq .eq. 3) ) then
            if ( (ieflag .eq. 8) .or. (ieflag .eq. 14) .or.
     *           (ieflag .eq. 20) .or. (ieflag .eq. 26) )
     *         igflag = ieflag
         else if ( (ieltyp .eq. 6) .and. (neq .eq. 4) ) then
            if ( (ieflag .eq. 28) .or. (ieflag .eq. 46) .or.
     *           (ieflag .eq. 55) )
     *         igflag = ieflag
         else if ( (ieltyp .eq. 6) .and. (neq .eq. 3) ) then
            if ( (ieflag .eq. 8) .or. (ieflag .eq. 57) )
     *         igflag = ieflag
         endif
         igflag = igflag + iaoffset
         icase = isotyp(igflag)
         if (ieltyp .eq. 4) then
C
C   tets
C
           if (icase .eq. 1) then
             nsides(1) = 3
             npolys = 1
             iedg(1,1,1) = isodat(1,igflag)
             iedg(1,1,2) = isodat(2,igflag)
             iedg(1,2,1) = isodat(1,igflag)
             iedg(1,2,2) = isodat(4,igflag)
             iedg(1,3,1) = isodat(1,igflag)
             iedg(1,3,2) = isodat(3,igflag)
           else if (icase .eq. 2) then
             nsides(1) = 4
             npolys = 1
             iedg(1,1,1) = isodat(2,igflag)
             iedg(1,1,2) = isodat(4,igflag)
             iedg(1,2,1) = isodat(2,igflag)
             iedg(1,2,2) = isodat(3,igflag)
             iedg(1,3,1) = isodat(1,igflag)
             iedg(1,3,2) = isodat(3,igflag)
             iedg(1,4,1) = isodat(1,igflag)
             iedg(1,4,2) = isodat(4,igflag)
           endif
         elseif (ieltyp .eq. 8) then
C
C   hexes
C
           if (icase .eq. 1) then
             nsides(1) = 3
             npolys = 1
             iedg(1,1,1) = isodat(1,igflag)
             iedg(1,1,2) = isodat(5,igflag)
             iedg(1,2,1) = isodat(1,igflag)
             iedg(1,2,2) = isodat(3,igflag)
             iedg(1,3,1) = isodat(1,igflag)
             iedg(1,3,2) = isodat(2,igflag)
           elseif (icase .eq. 3) then
             nsides(1) = 4
             npolys = 1
             iedg(1,1,1) = isodat(1,igflag)
             iedg(1,1,2) = isodat(5,igflag)
             iedg(1,2,1) = isodat(1,igflag)
             iedg(1,2,2) = isodat(3,igflag)
             iedg(1,3,1) = isodat(2,igflag)
             iedg(1,3,2) = isodat(4,igflag)
             iedg(1,4,1) = isodat(2,igflag)
             iedg(1,4,2) = isodat(6,igflag)
           elseif (icase .eq. 6) then
C                  /*  Something of a special case here:   */
C                  /*  If the average of the vertices      */
C                  /*  is greater than the isovalue,       */
C                  /*  we create two separated polygons    */
C                  /*  at the vertices.  If it is less,    */
C                  /* then we make a little valley shape.  */
C
              if ( (fv(isodat(1,igflag)) + fv(isodat(2,igflag)) +
     *              fv(isodat(3,igflag)) + fv(isodat(4,igflag)))
     *           .gt. 4.0*value ) then
                nsides(1) = 6
                npolys = 1
                iedg(1,1,1) = isodat(3,igflag)
                iedg(1,1,2) = isodat(1,igflag)
                iedg(1,2,1) = isodat(3,igflag)
                iedg(1,2,2) = isodat(7,igflag)
                iedg(1,3,1) = isodat(3,igflag)
                iedg(1,3,2) = isodat(4,igflag)
                iedg(1,4,1) = isodat(2,igflag)
                iedg(1,4,2) = isodat(4,igflag)
                iedg(1,5,1) = isodat(2,igflag)
                iedg(1,5,2) = isodat(6,igflag)
                iedg(1,6,1) = isodat(2,igflag)
                iedg(1,6,2) = isodat(1,igflag)
              else
                nsides(1) = 3
                npolys = 2
                iedg(1,1,1) = isodat(2,igflag)
                iedg(1,1,2) = isodat(4,igflag)
                iedg(1,2,1) = isodat(2,igflag)
                iedg(1,2,2) = isodat(6,igflag)
                iedg(1,3,1) = isodat(2,igflag)
                iedg(1,3,2) = isodat(1,igflag)
                nsides(2) = 3
                iedg(2,1,1) = isodat(3,igflag)
                iedg(2,1,2) = isodat(1,igflag)
                iedg(2,2,1) = isodat(3,igflag)
                iedg(2,2,2) = isodat(7,igflag)
                iedg(2,3,1) = isodat(3,igflag)
                iedg(2,3,2) = isodat(4,igflag)
              endif
           elseif (icase .eq. 7) then
             nsides(1) = 5
             npolys = 1
             iedg(1,1,1) = isodat(1,igflag)
             iedg(1,1,2) = isodat(5,igflag)
             iedg(1,2,1) = isodat(3,igflag)
             iedg(1,2,2) = isodat(7,igflag)
             iedg(1,3,1) = isodat(3,igflag)
             iedg(1,3,2) = isodat(4,igflag)
             iedg(1,4,1) = isodat(2,igflag)
             iedg(1,4,2) = isodat(4,igflag)
             iedg(1,5,1) = isodat(2,igflag)
             iedg(1,5,2) = isodat(6,igflag)
           elseif (icase .eq. 15) then
             nsides(1) = 4
             npolys = 1
             iedg(1,1,1) = isodat(1,igflag)
             iedg(1,1,2) = isodat(5,igflag)
             iedg(1,2,1) = isodat(3,igflag)
             iedg(1,2,2) = isodat(7,igflag)
             iedg(1,3,1) = isodat(4,igflag)
             iedg(1,3,2) = isodat(8,igflag)
             iedg(1,4,1) = isodat(2,igflag)
             iedg(1,4,2) = isodat(6,igflag)
           elseif (icase .eq. 22) then
             nsides(1) = 3
             npolys = 3
             iedg(1,1,1) = isodat(5,igflag)
             iedg(1,1,2) = isodat(6,igflag)
             iedg(1,2,1) = isodat(5,igflag)
             iedg(1,2,2) = isodat(7,igflag)
             iedg(1,3,1) = isodat(5,igflag)
             iedg(1,3,2) = isodat(1,igflag)
             nsides(2) = 3
             iedg(2,1,1) = isodat(2,igflag)
             iedg(2,1,2) = isodat(1,igflag)
             iedg(2,2,1) = isodat(2,igflag)
             iedg(2,2,2) = isodat(4,igflag)
             iedg(2,3,1) = isodat(2,igflag)
             iedg(2,3,2) = isodat(6,igflag)
             nsides(3) = 3
             iedg(3,1,1) = isodat(3,igflag)
             iedg(3,1,2) = isodat(1,igflag)
             iedg(3,2,1) = isodat(3,igflag)
             iedg(3,2,2) = isodat(7,igflag)
             iedg(3,3,1) = isodat(3,igflag)
             iedg(3,3,2) = isodat(4,igflag)
           elseif (icase .eq. 23) then
             nsides(1) = 6
             npolys = 1
             iedg(1,1,1) = isodat(5,igflag)
             iedg(1,1,2) = isodat(6,igflag)
             iedg(1,2,1) = isodat(5,igflag)
             iedg(1,2,2) = isodat(7,igflag)
             iedg(1,3,1) = isodat(3,igflag)
             iedg(1,3,2) = isodat(7,igflag)
             iedg(1,4,1) = isodat(3,igflag)
             iedg(1,4,2) = isodat(4,igflag)
             iedg(1,5,1) = isodat(2,igflag)
             iedg(1,5,2) = isodat(4,igflag)
             iedg(1,6,1) = isodat(2,igflag)
             iedg(1,6,2) = isodat(6,igflag)
           elseif (icase .eq. 24) then
             nsides(1) = 3
             npolys = 2
             iedg(1,1,1) = isodat(5,igflag)
             iedg(1,1,2) = isodat(6,igflag)
             iedg(1,2,1) = isodat(5,igflag)
             iedg(1,2,2) = isodat(7,igflag)
             iedg(1,3,1) = isodat(5,igflag)
             iedg(1,3,2) = isodat(1,igflag)
             nsides(2) = 3
             iedg(2,1,1) = isodat(4,igflag)
             iedg(2,1,2) = isodat(3,igflag)
             iedg(2,2,1) = isodat(4,igflag)
             iedg(2,2,2) = isodat(8,igflag)
             iedg(2,3,1) = isodat(4,igflag)
             iedg(2,3,2) = isodat(2,igflag)
           elseif (icase .eq. 25) then
             nsides(1) = 4
             npolys = 2
             iedg(1,1,1) = isodat(1,igflag)
             iedg(1,1,2) = isodat(2,igflag)
             iedg(1,2,1) = isodat(5,igflag)
             iedg(1,2,2) = isodat(6,igflag)
             iedg(1,3,1) = isodat(5,igflag)
             iedg(1,3,2) = isodat(7,igflag)
             iedg(1,4,1) = isodat(1,igflag)
             iedg(1,4,2) = isodat(3,igflag)
             nsides(2) = 3
             iedg(2,1,1) = isodat(4,igflag)
             iedg(2,1,2) = isodat(3,igflag)
             iedg(2,2,1) = isodat(4,igflag)
             iedg(2,2,2) = isodat(8,igflag)
             iedg(2,3,1) = isodat(4,igflag)
             iedg(2,3,2) = isodat(2,igflag)
           elseif (icase .eq. 27) then
             nsides(1) = 6
             npolys = 1
             iedg(1,1,1) = isodat(4,igflag)
             iedg(1,1,2) = isodat(3,igflag)
             iedg(1,2,1) = isodat(4,igflag)
             iedg(1,2,2) = isodat(8,igflag)
             iedg(1,3,1) = isodat(2,igflag)
             iedg(1,3,2) = isodat(6,igflag)
             iedg(1,4,1) = isodat(5,igflag)
             iedg(1,4,2) = isodat(6,igflag)
             iedg(1,5,1) = isodat(5,igflag)
             iedg(1,5,2) = isodat(7,igflag)
             iedg(1,6,1) = isodat(1,igflag)
             iedg(1,6,2) = isodat(3,igflag)
           elseif (icase .eq. 30) then
             nsides(1) = 3
             npolys = 2
             iedg(1,1,1) = isodat(5,igflag)
             iedg(1,1,2) = isodat(6,igflag)
             iedg(1,2,1) = isodat(5,igflag)
             iedg(1,2,2) = isodat(7,igflag)
             iedg(1,3,1) = isodat(5,igflag)
             iedg(1,3,2) = isodat(1,igflag)
             nsides(2) = 5
             iedg(2,1,1) = isodat(2,igflag)
             iedg(2,1,2) = isodat(6,igflag)
             iedg(2,2,1) = isodat(2,igflag)
             iedg(2,2,2) = isodat(1,igflag)
             iedg(2,3,1) = isodat(3,igflag)
             iedg(2,3,2) = isodat(1,igflag)
             iedg(2,4,1) = isodat(3,igflag)
             iedg(2,4,2) = isodat(7,igflag)
             iedg(2,5,1) = isodat(4,igflag)
             iedg(2,5,2) = isodat(8,igflag)
           elseif (icase .eq. 31) then
             nsides(1) = 5
             npolys = 1
             iedg(1,1,1) = isodat(2,igflag)
             iedg(1,1,2) = isodat(6,igflag)
             iedg(1,2,1) = isodat(5,igflag)
             iedg(1,2,2) = isodat(6,igflag)
             iedg(1,3,1) = isodat(5,igflag)
             iedg(1,3,2) = isodat(7,igflag)
             iedg(1,4,1) = isodat(3,igflag)
             iedg(1,4,2) = isodat(7,igflag)
             iedg(1,5,1) = isodat(4,igflag)
             iedg(1,5,2) = isodat(8,igflag)
           elseif (icase .eq. 39) then
             nsides(1) = 6
             npolys = 1
             iedg(1,1,1) = isodat(1,igflag)
             iedg(1,1,2) = isodat(3,igflag)
             iedg(1,2,1) = isodat(1,igflag)
             iedg(1,2,2) = isodat(7,igflag)
             iedg(1,3,1) = isodat(5,igflag)
             iedg(1,3,2) = isodat(6,igflag)
             iedg(1,4,1) = isodat(2,igflag)
             iedg(1,4,2) = isodat(6,igflag)
             iedg(1,5,1) = isodat(4,igflag)
             iedg(1,5,2) = isodat(8,igflag)
             iedg(1,6,1) = isodat(4,igflag)
             iedg(1,6,2) = isodat(3,igflag)
           elseif (icase .eq. 60) then
             nsides(1) = 4
             npolys = 2
             iedg(1,1,1) = isodat(5,igflag)
             iedg(1,1,2) = isodat(1,igflag)
             iedg(1,2,1) = isodat(6,igflag)
             iedg(1,2,2) = isodat(2,igflag)
             iedg(1,3,1) = isodat(6,igflag)
             iedg(1,3,2) = isodat(8,igflag)
             iedg(1,4,1) = isodat(5,igflag)
             iedg(1,4,2) = isodat(7,igflag)
             nsides(2) = 4
             iedg(2,1,1) = isodat(3,igflag)
             iedg(2,1,2) = isodat(7,igflag)
             iedg(2,2,1) = isodat(4,igflag)
             iedg(2,2,2) = isodat(8,igflag)
             iedg(2,3,1) = isodat(4,igflag)
             iedg(2,3,2) = isodat(2,igflag)
             iedg(2,4,1) = isodat(3,igflag)
             iedg(2,4,2) = isodat(1,igflag)
           elseif (icase .eq. 61) then
             nsides(1) = 3
             npolys = 2
             iedg(1,1,1) = isodat(1,igflag)
             iedg(1,1,2) = isodat(2,igflag)
             iedg(1,2,1) = isodat(6,igflag)
             iedg(1,2,2) = isodat(2,igflag)
             iedg(1,3,1) = isodat(4,igflag)
             iedg(1,3,2) = isodat(2,igflag)
             iedg(2,1,1) = isodat(5,igflag)
             nsides(2) = 4
             iedg(2,1,2) = isodat(7,igflag)
             iedg(2,2,1) = isodat(3,igflag)
             iedg(2,2,2) = isodat(7,igflag)
             iedg(2,3,1) = isodat(4,igflag)
             iedg(2,3,2) = isodat(8,igflag)
             iedg(2,4,1) = isodat(6,igflag)
             iedg(2,4,2) = isodat(8,igflag)
           elseif (icase .eq. 63) then
             nsides(1) = 4
             npolys = 1
             iedg(1,1,1) = isodat(4,igflag)
             iedg(1,1,2) = isodat(8,igflag)
             iedg(1,2,1) = isodat(6,igflag)
             iedg(1,2,2) = isodat(8,igflag)
             iedg(1,3,1) = isodat(5,igflag)
             iedg(1,3,2) = isodat(7,igflag)
             iedg(1,4,1) = isodat(3,igflag)
             iedg(1,4,2) = isodat(7,igflag)
           elseif (icase .eq. 107) then
             nsides(1) = 3
             npolys = 2
             iedg(1,1,1) = isodat(4,igflag)
             iedg(1,1,2) = isodat(3,igflag)
             iedg(1,2,1) = isodat(7,igflag)
             iedg(1,2,2) = isodat(3,igflag)
             iedg(1,3,1) = isodat(1,igflag)
             iedg(1,3,2) = isodat(3,igflag)
             nsides(2) = 5
             iedg(2,1,1) = isodat(4,igflag)
             iedg(2,1,2) = isodat(3,igflag)
             iedg(2,2,1) = isodat(4,igflag)
             iedg(2,2,2) = isodat(8,igflag)
             iedg(2,3,1) = isodat(7,igflag)
             iedg(2,3,2) = isodat(8,igflag)
             iedg(2,4,1) = isodat(7,igflag)
             iedg(2,4,2) = isodat(5,igflag)
             iedg(2,5,1) = isodat(1,igflag)
             iedg(2,5,2) = isodat(5,igflag)
           elseif (icase .eq. 111) then
C                  /*  Something of a special case here:    */
C                  /*  If the average of the vertices       */
C                  /*  is greater than the isovalue,        */
C                  /*  we create two separated polygons     */
C                  /*  at the vertices.  If it is less,     */
C                  /*  then we make a little valley shape.  */
C
              if ( (fv(isodat(5,igflag)) + fv(isodat(6,igflag)) +
     *              fv(isodat(7,igflag)) + fv(isodat(8,igflag)))
     *           .gt. 4.0*value ) then
 
                nsides(1) = 6
                npolys = 1
                iedg(1,1,1) = isodat(1,igflag)
                iedg(1,1,2) = isodat(5,igflag)
                iedg(1,2,1) = isodat(7,igflag)
                iedg(1,2,2) = isodat(5,igflag)
                iedg(1,3,1) = isodat(7,igflag)
                iedg(1,3,2) = isodat(8,igflag)
                iedg(1,4,1) = isodat(4,igflag)
                iedg(1,4,2) = isodat(8,igflag)
                iedg(1,5,1) = isodat(6,igflag)
                iedg(1,5,2) = isodat(8,igflag)
                iedg(1,6,1) = isodat(6,igflag)
                iedg(1,6,2) = isodat(5,igflag)
              else
                nsides(1) = 3
                npolys = 2
                iedg(1,1,1) = isodat(1,igflag)
                iedg(1,1,2) = isodat(5,igflag)
                iedg(1,2,1) = isodat(7,igflag)
                iedg(1,2,2) = isodat(5,igflag)
                iedg(1,3,1) = isodat(6,igflag)
                iedg(1,3,2) = isodat(5,igflag)
                nsides(2) = 3
                iedg(2,1,1) = isodat(7,igflag)
                iedg(2,1,2) = isodat(8,igflag)
                iedg(2,2,1) = isodat(4,igflag)
                iedg(2,2,2) = isodat(8,igflag)
                iedg(2,3,1) = isodat(6,igflag)
                iedg(2,3,2) = isodat(8,igflag)
              endif
           elseif (icase .eq. 126) then
             nsides(1) = 3
             npolys = 2
             iedg(1,1,1) = isodat(2,igflag)
             iedg(1,1,2) = isodat(1,igflag)
             iedg(1,2,1) = isodat(3,igflag)
             iedg(1,2,2) = isodat(1,igflag)
             iedg(1,3,1) = isodat(5,igflag)
             iedg(1,3,2) = isodat(1,igflag)
             nsides(2) = 3
             iedg(2,1,1) = isodat(4,igflag)
             iedg(2,1,2) = isodat(8,igflag)
             iedg(2,2,1) = isodat(6,igflag)
             iedg(2,2,2) = isodat(8,igflag)
             iedg(2,3,1) = isodat(7,igflag)
             iedg(2,3,2) = isodat(8,igflag)
           elseif (icase .eq. 127) then
             nsides(1) = 3
             npolys = 1
             iedg(1,1,1) = isodat(4,igflag)
             iedg(1,1,2) = isodat(8,igflag)
             iedg(1,2,1) = isodat(6,igflag)
             iedg(1,2,2) = isodat(8,igflag)
             iedg(1,3,1) = isodat(7,igflag)
             iedg(1,3,2) = isodat(8,igflag)
           endif
C
C   pyramids
C
         elseif (ieltyp .eq. 5) then
           if (icase .eq. 1) then
             nsides(1) = 3
             npolys = 1
             iedg(1,1,1) = isodat(1,igflag)
             iedg(1,1,2) = isodat(2,igflag)
             iedg(1,2,1) = isodat(1,igflag)
             iedg(1,2,2) = isodat(3,igflag)
             iedg(1,3,1) = isodat(1,igflag)
             iedg(1,3,2) = isodat(4,igflag)
           elseif (icase .eq. 2) then
             nsides(1) = 5
             npolys = 1
             iedg(1,1,1) = isodat(1,igflag)
             iedg(1,1,2) = isodat(3,igflag)
             iedg(1,2,1) = isodat(1,igflag)
             iedg(1,2,2) = isodat(4,igflag)
             iedg(1,3,1) = isodat(1,igflag)
             iedg(1,3,2) = isodat(5,igflag)
             iedg(1,4,1) = isodat(2,igflag)
             iedg(1,4,2) = isodat(5,igflag)
             iedg(1,5,1) = isodat(2,igflag)
             iedg(1,5,2) = isodat(3,igflag)
           elseif (icase .eq. 3) then
             nsides(1) = 4
             npolys = 1
             iedg(1,1,1) = isodat(3,igflag)
             iedg(1,1,2) = isodat(1,igflag)
             iedg(1,2,1) = isodat(3,igflag)
             iedg(1,2,2) = isodat(4,igflag)
             iedg(1,3,1) = isodat(2,igflag)
             iedg(1,3,2) = isodat(5,igflag)
             iedg(1,4,1) = isodat(2,igflag)
             iedg(1,4,2) = isodat(1,igflag)
           elseif (icase .eq. 4) then
 
             if ( (fv(2) + fv(3) + fv(4) + fv(5))
     *          .gt. 4.0*value ) then
 
                nsides(1) = 6
                npolys = 1
 
                iedg(1,1,1) = isodat(2,igflag)
                iedg(1,1,2) = isodat(3,igflag)
                iedg(1,2,1) = isodat(2,igflag)
                iedg(1,2,2) = isodat(1,igflag)
                iedg(1,3,1) = isodat(2,igflag)
                iedg(1,3,2) = isodat(5,igflag)
                iedg(1,4,1) = isodat(4,igflag)
                iedg(1,4,2) = isodat(5,igflag)
                iedg(1,5,1) = isodat(4,igflag)
                iedg(1,5,2) = isodat(1,igflag)
                iedg(1,6,1) = isodat(4,igflag)
                iedg(1,6,2) = isodat(3,igflag)
 
             else
                nsides(1) = 3
                npolys = 2
 
                iedg(1,1,1) = isodat(2,igflag)
                iedg(1,1,2) = isodat(1,igflag)
                iedg(1,2,1) = isodat(2,igflag)
                iedg(1,2,2) = isodat(3,igflag)
                iedg(1,3,1) = isodat(2,igflag)
                iedg(1,3,2) = isodat(5,igflag)
                nsides(2) = 3
                iedg(2,1,1) = isodat(4,igflag)
                iedg(2,1,2) = isodat(1,igflag)
                iedg(2,2,1) = isodat(4,igflag)
                iedg(2,2,2) = isodat(3,igflag)
                iedg(2,3,1) = isodat(4,igflag)
               iedg(2,3,2) = isodat(5,igflag)
            endif
 
          elseif (icase .eq. 5) then
             nsides(1) = 4
             npolys = 1
             iedg(1,1,1) = isodat(1,igflag)
             iedg(1,1,2) = isodat(2,igflag)
             iedg(1,2,1) = isodat(1,igflag)
             iedg(1,2,2) = isodat(3,igflag)
             iedg(1,3,1) = isodat(1,igflag)
             iedg(1,3,2) = isodat(4,igflag)
             iedg(1,4,1) = isodat(1,igflag)
             iedg(1,4,2) = isodat(5,igflag)
           endif
         elseif (ieltyp .eq. 6) then
C
C   prisms
C
           if (icase .eq. 1) then
             nsides(1) = 3
             npolys = 1
             iedg(1,1,1) = isodat(1,igflag)
             iedg(1,1,2) = isodat(2,igflag)
             iedg(1,2,1) = isodat(1,igflag)
             iedg(1,2,2) = isodat(3,igflag)
             iedg(1,3,1) = isodat(1,igflag)
             iedg(1,3,2) = isodat(4,igflag)
           elseif (icase .eq. 2) then
             nsides(1) = 3
             npolys = 1
             iedg(1,1,1) = isodat(1,igflag)
             iedg(1,1,2) = isodat(4,igflag)
             iedg(1,2,1) = isodat(2,igflag)
             iedg(1,2,2) = isodat(5,igflag)
             iedg(1,3,1) = isodat(3,igflag)
             iedg(1,3,2) = isodat(6,igflag)
           elseif (icase .eq. 3) then
             nsides(1) = 4
             npolys = 1
             iedg(1,1,1) = isodat(1,igflag)
             iedg(1,1,2) = isodat(4,igflag)
             iedg(1,2,1) = isodat(2,igflag)
             iedg(1,2,2) = isodat(5,igflag)
             iedg(1,3,1) = isodat(2,igflag)
             iedg(1,3,2) = isodat(3,igflag)
             iedg(1,4,1) = isodat(1,igflag)
             iedg(1,4,2) = isodat(3,igflag)
           elseif (icase .eq. 4) then
             nsides(1) = 3
             npolys = 2
             iedg(1,1,1) = isodat(2,igflag)
             iedg(1,1,2) = isodat(1,igflag)
             iedg(1,2,1) = isodat(2,igflag)
             iedg(1,2,2) = isodat(3,igflag)
             iedg(1,3,1) = isodat(2,igflag)
             iedg(1,3,2) = isodat(5,igflag)
             nsides(2) = 3
             iedg(2,1,1) = isodat(4,igflag)
             iedg(2,1,2) = isodat(1,igflag)
             iedg(2,2,1) = isodat(4,igflag)
             iedg(2,2,2) = isodat(5,igflag)
             iedg(2,3,1) = isodat(4,igflag)
             iedg(2,3,2) = isodat(6,igflag)
           elseif (icase .eq. 5) then
             nsides(1) = 5
             npolys = 1
             iedg(1,1,1) = isodat(1,igflag)
             iedg(1,1,2) = isodat(3,igflag)
             iedg(1,2,1) = isodat(3,igflag)
             iedg(1,2,2) = isodat(2,igflag)
             iedg(1,3,1) = isodat(2,igflag)
             iedg(1,3,2) = isodat(5,igflag)
             iedg(1,4,1) = isodat(5,igflag)
             iedg(1,4,2) = isodat(4,igflag)
             iedg(1,5,1) = isodat(4,igflag)
             iedg(1,5,2) = isodat(6,igflag)
           elseif (icase .eq. 6) then
             nsides(1) = 4
             npolys = 2
             iedg(1,1,1) = isodat(1,igflag)
             iedg(1,1,2) = isodat(2,igflag)
             iedg(1,2,1) = isodat(2,igflag)
             iedg(1,2,2) = isodat(5,igflag)
             iedg(1,3,1) = isodat(6,igflag)
             iedg(1,3,2) = isodat(3,igflag)
             iedg(1,4,1) = isodat(3,igflag)
             iedg(1,4,2) = isodat(1,igflag)
             nsides(2) = 3
             iedg(2,1,1) = isodat(4,igflag)
             iedg(2,1,2) = isodat(5,igflag)
             iedg(2,2,1) = isodat(4,igflag)
             iedg(2,2,2) = isodat(6,igflag)
             iedg(2,3,1) = isodat(4,igflag)
             iedg(2,3,2) = isodat(1,igflag)
           elseif (icase .eq. 7) then
             nsides(1) = 4
             npolys = 1
             iedg(1,1,1) = isodat(1,igflag)
             iedg(1,1,2) = isodat(2,igflag)
             iedg(1,2,1) = isodat(1,igflag)
             iedg(1,2,2) = isodat(3,igflag)
             iedg(1,3,1) = isodat(4,igflag)
             iedg(1,3,2) = isodat(6,igflag)
             iedg(1,4,1) = isodat(4,igflag)
             iedg(1,4,2) = isodat(5,igflag)
           endif
C
         endif
C
C   Loop over polygons.
C
         if (npolys .gt. 0) then
         do ipoly=1,npolys
C
C   Now update the node quantities.  First, find the set of points
C   that represents the intersection of the contour surface with the
C   element edges.
C
            do is=1,nsides(ipoly)
               iv1 = iedg(ipoly,is,1)
               iv2 = iedg(ipoly,is,2)
               ip1 = itet1(itetoff(it)+iv1)
               ip2 = itet1(itetoff(it)+iv2)
               cdist(is) = (value - fv(iv1))/(fv(iv2) - fv(iv1))
               xc(is) = xic(ip1) + cdist(is)*(xic(ip2) - xic(ip1))
               yc(is) = yic(ip1) + cdist(is)*(yic(ip2) - yic(ip1))
               zc(is) = zic(ip1) + cdist(is)*(zic(ip2) - zic(ip1))
            enddo
C
C   Now check and see if these points are already in the list.  If
C   not, add them.
C
            do jchk=1,nsides(ipoly)
               newpt(jchk) = 0
               ifoundit = 0
               ichk = 1
               do while ( (ifoundit.eq.0) .and. (ichk.le.npointsn) )
                  if ( (abs(xica(ichk) - xc(jchk)).lt.epsilone) .and.
     *                 (abs(yica(ichk) - yc(jchk)).lt.epsilone) .and.
     *                 (abs(zica(ichk) - zc(jchk)).lt.epsilone) ) then
C
C   Point already is in the list.
                     ipn(jchk) = ichk
                     ifoundit = 1
                  endif
                  ichk = ichk + 1
               enddo
               if (ifoundit.eq.0) then
                  npointsn = npointsn + 1
                  ipn(jchk) = npointsn
                  newpt(jchk) = 1
                  xica(ipn(jchk)) = xc(jchk)
                  yica(ipn(jchk)) = yc(jchk)
                  zica(ipn(jchk)) = zc(jchk)
                  ip1 = itet1(itetoff(it)+iedg(ipoly,jchk,1))
                  ip2 = itet1(itetoff(it)+iedg(ipoly,jchk,2))
                  imt1a(ipn(jchk)) = imt1(ip1)
C
C   Point is on an interface if and only if both ends of the edge
C   are.  If this is the case, set the point type here.  We will
C   find the other boundary points (type 10) later.
C
                  itp1a(ipn(jchk)) = max(itp1(ip1),itp1(ip2))
                  icr1a(ipn(jchk)) = 0
C
C   We will set the isn1a points later.
C
                  isn1a(ipn(jchk)) = 0
C
C  Put newpoints and neighbors and weights into arrays
C  for later call to cmo_interpolate
C
                  nlist=nlist+1
                  list(nlist)=ipn(jchk)
                  ilist(1,nlist)=ip1
                  ilist(2,nlist)=ip2
                  xweight(1,nlist)=1.-cdist(jchk)
                  xweight(2,nlist)=cdist(jchk)
               endif
            enddo
C
C   Update the tets, making sure not to draw any zero-volume
C   triangles.  Handle quads separately so that the cmo will have
C   quads if possible.  Otherwise, break pentagons and hexagons into
C   triangles.
C
         if ( (nsides(ipoly) .ne. 4) .or. ((nsides(ipoly) .eq. 4)
     *  .and. ( (ipn(1) .eq. ipn(2)) .or. (ipn(1) .eq. ipn(3)) .or.
     *         (ipn(1) .eq. ipn(4)) .or. (ipn(2) .eq. ipn(3)) .or.
     *         (ipn(2) .eq. ipn(4)) .or. (ipn(3) .eq. ipn(4))) )
     *      ) then
C
         do itri=1,nsides(ipoly)-2
C
            if ( (ipn(1).ne.ipn(itri+1)).and.(ipn(1).ne.ipn(itri+2))
     *           .and.(ipn(itri+1).ne.ipn(itri+2)) ) then
C
C   If no new points were found, we must check to see if this
C   triangle already exists in the tet list.
C
               iskipit = 0
               if ( (newpt(1)+newpt(itri+1)+newpt(itri+2)) .eq. 0 )
     *          then
                  m = 1
                   do while ( (iskipit .eq. 0) .and.
     *                       (m .lt. (ntetsn+1)) )
                     ioff = itetoffa(m)
                     if ( ( (iteta1(ioff+1) .eq. ipn(1) ) .or.
     *                      (iteta1(ioff+1) .eq. ipn(itri+1) ) .or.
     *                      (iteta1(ioff+1) .eq. ipn(itri+2) ) ) .and.
     *                    ( (iteta1(ioff+2) .eq. ipn(1) ) .or.
     *                      (iteta1(ioff+2) .eq. ipn(itri+1) ) .or.
     *                      (iteta1(ioff+2) .eq. ipn(itri+2) ) ) .and.
     *                    ( (iteta1(ioff+3) .eq. ipn(1) ) .or.
     *                      (iteta1(ioff+3) .eq. ipn(itri+1) ) .or.
     *                      (iteta1(ioff+3) .eq. ipn(itri+2) ) ) )
     *                   iskipit = 1
                     m = m + 1
                  enddo
               endif



C
C   Make sure the vertices are connected clockwise as seen from the
C   outside (the positive side).  Do this by constructing the plane
C   equation for the triangle, then finding the signed distance from
C   the plane of a vertex having field value less than the isosurface
C   value.  If this distance is negative, do 1-3-2 else do 1-2-3.
C
            if (iskipit .eq. 0) then
               x1 = xica(ipn(1))
               y1 = yica(ipn(1))
               z1 = zica(ipn(1))
               x2 =  xica(ipn(itri+1))
               y2 =  yica(ipn(itri+1))
               z2 =  zica(ipn(itri+1))
               x3 = xica(ipn(itri+2))
               y3 = yica(ipn(itri+2))
               z3 = zica(ipn(itri+2))

               a = (y2-y1)*(z3-z1) - (y3-y1)*(z2-z1)
               b = (z2-z1)*(x3-x1) - (z3-z1)*(x2-x1)
               c = (x2-x1)*(y3-y1) - (x3-x1)*(y2-y1)
               d = -a*x1 - b*y1 - c*z1
               xpt = xic(itet1(itetoff(it)+icorner))
               ypt = yic(itet1(itetoff(it)+icorner))
               zpt = zic(itet1(itetoff(it)+icorner))

               vol = -(x2*y3*zpt)+(x1*y3*zpt)+(y2*x3*zpt)- 
     x                (y1*x3*zpt)-(x1*y2*zpt)+(y1*x2*zpt)+ 
     x                (x2*z3*ypt)-(x1*z3*ypt)-(z2*x3*ypt)+ 
     x                (z1*x3*ypt)+(x1*z2*ypt)-(z1*x2*ypt)- 
     x                (y2*z3*xpt)+(y1*z3*xpt)+(z2*y3*xpt)- 
     x                (z1*y3*xpt)-(y1*z2*xpt)+(z1*y2*xpt)+ 
     x                (x1*y2*z3)-(y1*x2*z3)-(x1*z2*y3)+ 
     x                (z1*x2*y3)+(y1*z2*x3)-(z1*y2*x3)
               sdist = sdistsign*(a*xpt + b*ypt + c*zpt + d)

               if (sdist.lt.0.0) then
                  n2 = 3
                  n3 = 2
               else
                  n2 = 2
                  n3 = 3
               endif
               ntetsn = ntetsn + 1
               itetoffa(ntetsn) = itindex
               jtetoffa(ntetsn) = jtindex
               itindex = itindex + nelmnen(ifelmtri)
               jtindex = jtindex + nelmnef(ifelmtri)


               iteta1(itetoffa(ntetsn)+1) = ipn(1)
               iteta1(itetoffa(ntetsn)+n2) = ipn(itri+1)
               iteta1(itetoffa(ntetsn)+n3) = ipn(itri+2)
               itetclra(ntetsn) = itetclr(it)
               itettypa(ntetsn) = ifelmtri
               numtris = numtris + 1
            endif
            endif
C
C   end "do itri=1,nsides(ipoly)-2"
C
         enddo
C
C   Do quads here.
C
C
         else
C
C   If no new points were found, we must check to see if this
C   triangle already exists in the tet list.
C
               iskipit = 0
               if ( (newpt(1)+newpt(2)+newpt(3)+newpt(4)) .eq. 0 )
     *          then
                  m = 1
                   do while ( (iskipit .eq. 0) .and.
     *                       (m .lt. (ntetsn+1)) )
                     ioff = itetoffa(m)
                     if ( ( (iteta1(ioff+1) .eq. ipn(1) ) .or.
     *                      (iteta1(ioff+1) .eq. ipn(2) ) .or.
     *                      (iteta1(ioff+1) .eq. ipn(3) ) .or.
     *                      (iteta1(ioff+1) .eq. ipn(4) ) ) .and.
     *                    ( (iteta1(ioff+2) .eq. ipn(1) ) .or.
     *                      (iteta1(ioff+2) .eq. ipn(2) ) .or.
     *                      (iteta1(ioff+2) .eq. ipn(3) ) .or.
     *                      (iteta1(ioff+2) .eq. ipn(4) ) ) .and.
     *                    ( (iteta1(ioff+3) .eq. ipn(1) ) .or.
     *                      (iteta1(ioff+3) .eq. ipn(2) ) .or.
     *                      (iteta1(ioff+3) .eq. ipn(3) ) .or.
     *                      (iteta1(ioff+3) .eq. ipn(4) ) ) .and.
     *                    ( (iteta1(ioff+4) .eq. ipn(1) ) .or.
     *                      (iteta1(ioff+4) .eq. ipn(2) ) .or.
     *                      (iteta1(ioff+4) .eq. ipn(3) ) .or.
     *                      (iteta1(ioff+4) .eq. ipn(4) ) ) )
     *                   iskipit = 1
                     m = m + 1
                  enddo
               endif
C
C   Make sure the vertices are connected clockwise as seen from the
C   outside (the positive side).  Do this by constructing the plane
C   equation for the triangle, then finding the signed distance from
C   the plane of a vertex having field value less than the isosurface
C   value.  If this distance is negative, do 1-3-2 else do 1-2-3.
C
            if (iskipit .eq. 0) then
               x1 = xc(1)
               y1 = yc(1)
               z1 = zc(1)
               x2 = xc(2)
               y2 = yc(2)
               z2 = zc(2)
               x3 = xc(3)
               y3 = yc(3)
               z3 = zc(3)

               a = (y2-y1)*(z3-z1) - (y3-y1)*(z2-z1)
               b = (z2-z1)*(x3-x1) - (z3-z1)*(x2-x1)
               c = (x2-x1)*(y3-y1) - (x3-x1)*(y2-y1)
               d = -a*x1 - b*y1 - c*z1

               xpt = xic(itet1(itetoff(it)+icorner))

               ypt = yic(itet1(itetoff(it)+icorner))

               zpt = zic(itet1(itetoff(it)+icorner))

               sdist = sdistsign*(a*xpt + b*ypt + c*zpt + d)

               if (sdist.lt.0.0) then
                  n2 = 4
                  n3 = 3
                  n4 = 2
               else
                  n2 = 2
                  n3 = 3
                  n4 = 4
               endif
               ntetsn = ntetsn + 1
               itetoffa(ntetsn) = itindex
               jtetoffa(ntetsn) = jtindex
               itindex = itindex + nelmnen(ifelmqud)
               jtindex = jtindex + nelmnef(ifelmqud)
               iteta1(itetoffa(ntetsn)+1) = ipn(1)
               iteta1(itetoffa(ntetsn)+n2) = ipn(2)
               iteta1(itetoffa(ntetsn)+n3) = ipn(3)
               iteta1(itetoffa(ntetsn)+n4) = ipn(4)
               itetclra(ntetsn) = itetclr(it)
               itettypa(ntetsn) = ifelmqud
               numquads = numquads + 1
            endif
C
         endif
C
         enddo
C
C   end "if (npolys .gt. 0)"
C
         endif
C
C   end "do it=1,ntets"
C
      enddo
C
      if(numquads .eq. 0) then
         nenout=nelmnen(ifelmtri)
         nefout=nelmnef(ifelmtri)
         neeout=nelmnee(ifelmtri)
      elseif(numtris .eq. 0) then
         nenout=nelmnen(ifelmqud)
         nefout=nelmnef(ifelmqud)
         neeout=nelmnee(ifelmqud)
      endif
      call cmo_set_info('nodes_per_element',cmoout,nenout,1,1,ierror)
      call cmo_set_info('faces_per_element',cmoout,nefout,1,1,ierror)
      call cmo_set_info('edges_per_element',cmoout,neeout,1,1,ierror)
C
C   If no new tets were found, this is an empty part.
C
C
C       In this case the result is cmoout will exist but it
C       will be empty, nnodes = 0, nelements = 0.
C
      if(ntetsn.eq.0) then
        ierr1 = 1
        write(logmess,'(a)')
     *     'ISOSURFACE: Nothing found for new mesh object.'
        call writloga('default',0,logmess,0,ierr1)
        call cmo_set_info('nnodes',cmoout,npointsn,1,1,ierror)
        call cmo_set_info('nelements',cmoout,ntetsn,1,1,ierror)
        call cmo_newlen(cmoout,ierror)
        goto 9999
      endif
C
C   Figure out the new jtet array.
C
C
C   Update the new cmo.
C
      call cmo_set_info('nnodes',cmoout,npointsn,1,1,ierror)
      call cmo_set_info('nelements',cmoout,ntetsn,1,1,ierror)
      call cmo_newlen(cmoout,ierror)
C
C
C   Figure out the new jtet array.
C
      call geniee_cmo(cmoout)
 
C  Use cmo_interpolate to fill output cmo
      call cmo_interpolate(cmoout,cmoin,'nnodes',nlist,nvalue,
     x     list,ilist,xweight,ierror)
      if(ierror.ne.0) call x3d_error(isubname,'cmo_interpolate')
C
C
C     ...............................................................
C     SET THE EXTERNAL BOUNDARY NODE TYPE BASED ON BOUNDARY FACES.
C
C
      call cmo_get_info('nnodes',cmoout,npointsn,length,icmotype,ier)
      call cmo_get_info('nelements',cmoout,ntetsn,length,icmotype,ier)
      call cmo_get_info('mbndry',cmoout,mbndry,length,icmotype,ier)
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
         index=nelmnef(itettypa(it))*(it-1)
         do i=1,nelmnef(itettypa(it))
            index=jtetoffa(it)+i
            if(jteta1(index).gt.0.and.jteta1(index).lt.mbndry) then
               jt=1+(jteta1(index)-1)/nefout
               jf=jteta1(index)-nefout*(jt-1)
               if(itetclra(it).ne.itetclra(jt)) then
                  jteta1(index)=mbndry+nefout*(jt-1)+jf
                  jteta1(jtetoffa(jt)+jf)=nefout*(it-1)+i+mbndry
               endif
            endif
         enddo
      enddo
C
C
      call cmo_get_info('nnodes',cmoout,npointsn,length,icmotype,ier)
      call cmo_get_info('nelements',cmoout,ntetsn,length,icmotype,ier)
      call cmo_get_info('itp1',cmoout,ipitp1a,ilen,ityp,ier)
C
C     ******************************************************************
C     SET UP AN ARRARY THAT IDENTIFIES THE INTERFACE NODES.
C          INT1 = 1  -> Interface Node.
C          INT1 = 0  -> Interior Node.
C
      length=npointsn
      call mmgetblk('int1a',isubname,ipint1a,length,2,icscode)
      call unpacktp('intrface','set',npointsn,ipitp1a,ipint1a,ierrdum)
      if(ierrdum.ne.0) call x3d_error(isubname,'unpacktp')
C
      icount=0
      do i1=1,npointsn
         if(int1a(i1).eq.1) icount=icount+1
      enddo
      if(icount.gt.0) then
         call dotaskx3d('settets/parents ; finish',ierror)
         call cmo_get_info('nnodes',cmoout,npointsn,length,icmotype,ier)
         call cmo_get_info('nelements',cmoout,
     *                     ntetsn,length,icmotype,ier)
      endif
C
      goto 9999
 9999 continue
C
      call mmrelprt(isubname,icscode)
C
      return
      end
