*DK offsetpara
      subroutine offsetpara(imsgin,xmsgin,cmsgin,msgtype,nwds,ierror)
C
C########################################################################
C
C     PURPOSE -
C
C        This subroutine (offsetpara) offsets a surface or a polyline
C        (1D or 2D topologically) such that the faces in the new and old
C        surfaces are a constant distance away from each other at any
C        point (draw a normal from each surface).
C
C     NOTES -
C
C        Currently only for xyz coordinate system.
C        Currently only for surfaces
C        Syntax for this command:
C          offsetpara/sink_mesh_object/source_mesh_object/
C          real|integer/[+|-]
C
C     argument 4 is the offset for the surface
C
C     argument 5 is optional. It specifies whether the offset is with
C     or against the defined normal. Default is with the normal.
C
C     INPUT ARGUMENTS -
C
C        xmsgin()  - REAL ARRAY OF COMMAND INPUT VALUES
C        cmsgin()  - CHARACTER ARRAY OF COMMAND INPUT VALUES
C        imsgin()  - INTEGER ARRAY OF COMMAND INPUT VALUES
C        msgtype() - INTEGER ARRAY OF COMMAND INPUT TYPE
C        nwds      - NO. OF WORDS OF COMMAND INPUT VALUES
C
C     CHANGE HISTORY -
C$Log:   /pvcs.config/t3d/src/offsetpara.f_a  $
CPVCS    
CPVCS       Rev 1.7   08 Feb 2006 14:35:36   dcg
CPVCS    "enforce lower case - add external statements for shift routines
CPVCS    these changes needed to compile with absoft pro fortran"
CPVCS    
CPVCS       Rev 1.6   30 Sep 2004 10:59:14   dcg
CPVCS    use value of pie from chydro include file set variable pi to pie
CPVCS
CPVCS       Rev 1.5   10 Apr 2001 13:41:52   dcg
CPVCS    make implicit none
CPVCS
CPVCS       Rev 1.4   21 Apr 2000 07:06:32   gable
CPVCS    Made setting and getting of mbndry value dynamic and problem size dependent.
CPVCS
CPVCS       Rev 1.3   03 Apr 2000 19:09:10   gable
CPVCS    Set mbndry according to problem size instead of old hardwired 16,000,000 value.
CPVCS
CPVCS       Rev 1.2   08 Feb 2000 08:39:30   dcg
CPVCS    remove comdict
CPVCS
CPVCS       Rev 1.1   Thu Aug 13 15:08:34 1998   dcg
CPVCS    IBM changes
CPVCS
CPVCS       Rev 1.0   Fri Aug 07 13:27:40 1998   dcg
CPVCS    Initial revision.
C
C
C########################################################################
C
      implicit none
C
      include "machine.h"
      include "chydro.h"
      include "local_element.h"
      include "consts.h"
C
      integer lenptr
      real*8 epsln,pi
      parameter (lenptr=1000000)
      parameter (epsln=1.0e-10)
 
C
C########################################################################
C
C Variable Declarations
C
C########################################################################
C
C     Subroutine Input Variables
C
      integer nwds
      character*(*) cmsgin(nwds)
      integer       imsgin(nwds), msgtype(nwds)
      real*8        xmsgin(nwds)
C
C     Name Variables and Message Variables
C
      character*32  isubname, cmoin, cmoout, cmotmp
      character*132 logmess, cmdmess
C
C     Pointers used to store node info for various reasons
C
      pointer (ipnodeidx, nodeidx)
      pointer (ipnodex, nodex)
      pointer (ipnodey, nodey)
      pointer (ipnodez, nodez)
      pointer (ipelts, elts)
      pointer (ipiparent, iparent)
C
      integer   nodeidx(lenptr), elts(lenptr), iparent(lenptr)
      integer nodex(lenptr), nodey(lenptr), nodez(lenptr)
C
C     Integers uded to store info for surface and endnode calculations
C
C     Surface Stuff
      integer   refnode, elmidx
C     Polyline Stuff
      integer   prevelmidx, endnode, temp, tempctr, endnodectr
C
C     Pointers used to store vector info: direction and mag factor
C
      pointer (ipxvect, xvect)
      pointer (ipyvect, yvect)
      pointer (ipzvect, zvect)
C
      real*8 xvect(lenptr), yvect(lenptr), zvect(lenptr)
      real*8 avg_deflection
      real*8 max_deflection
      real*8 deflection
 
C
C     Pointers for incoming CMO
C
C     Node Based Attributes
      pointer (ipimt1, imt1)
      pointer (ipitp1, itp1)
      pointer (ipisn1, isn1)
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
C
C     Element Based Attributes
      pointer (ipitetclr, itetclr)
      pointer (ipitettyp, itettyp)
      pointer (ipitetoff, itetoff)
      pointer (ipjtetoff, jtetoff)
C
C     Array of No. of Elements*No. of Nodes per Element
      pointer (ipitet, itet)
      pointer (ipjtet, jtet)
C
      real*8 xic(lenptr), yic(lenptr), zic(lenptr)
      integer   imt1(lenptr), itp1(lenptr), isn1(lenptr)
      integer   itetclr(lenptr), itettyp(lenptr)
      integer   itet(4*lenptr), jtet(4*lenptr)
      integer   itetoff(lenptr), jtetoff(lenptr)
C     The 4 is used to ensure that the pointer is large enough to handle
C     any surface.
C
C     Pointers for outgoing CMO
C
C     Node Based Attributes
      pointer (ipimt1o, imt1o)
      pointer (ipitp1o, itp1o)
      pointer (ipisn1o, isn1o)
      pointer (ipxico, xico)
      pointer (ipyico, yico)
      pointer (ipzico, zico)
C
C     Element Based Attributes
      pointer (ipitetclro, itetclro)
      pointer (ipitettypo, itettypo)
      pointer (ipitetoffo, itetoffo)
      pointer (ipjtetoffo, jtetoffo)
C
C     Array of No. of Elements*No. of Nodes per Element
      pointer (ipiteto, iteto)
      pointer (ipjteto, jteto)
C
      real*8 xico(lenptr), yico(lenptr), zico(lenptr)
      integer   imt1o(lenptr), itp1o(lenptr), isn1o(lenptr)
      integer   itetclro(lenptr), itettypo(lenptr)
      integer   iteto(4*lenptr), jteto(4*lenptr)
      integer   itetoffo(lenptr), jtetoffo(lenptr)
c
      integer id1,id2,id3,ilen,icmotype,ierror,idelete,k,
     * curr_elt,nelts,itri,n,length,icscode,isdel,ics,inode,
     * test,nnodes,nsdgeom,nef,nee,mbndry,i,nsdtopo,j,
     * nelements,nen,sign,refnode1
      real*8 ax,ay,az,bx,by,bz,crossmag,dbarea,d,
     *  u1,v1,u2,v2,u3,v3,delta,d1,d2,d3,dot12,dot23,dot31,
     *  xvar,yvar,zvar,p3z,p3y,p3x,p2z,p2y,p2x,p1z,p1y,p1x,
     *  xbisect,ybisect,zbisect,xrefvect,yrefvect,zrefvect,
     *  A,B,C,anorm,trueendidx,trueendnode,true2ndnode,
     *  xcheck,ycheck,zcheck,dfact
C     The 4 is used to ensure that the pointer is large enough to handle
C     any surface.
C
C########################################################################
C
C Inline Function crossmag:
C
C     Returns the area of the triangle made up of the two vectors A & B
C     the vectors are designated as: <ax,ay,az> and <bx,by,bz>.
C
C#######################################################################
C
      crossmag(ax,ay,az,bx,by,bz)=sqrt((ax*by-bx*ay)**2+
     &     (ax*bz-bz*ax)**2+(ay*bz-by*az)**2)
C
C
C########################################################################
C
C Inline Function DBArea:
C
C     Returns double the area of a triangle ordered (counterclockwise)
C     1,2,3 in the u-v plane. This means that for a triangle ordered
C     1,2,3 in x-y-z space, the (r.h. rule) vector normal to this triangle
C     with magnitude equal to double the area is given by:
C                    < dbarea(y1,z1,y2,z2,y3,z3),
C                      dbarea(z1,x1,z2,x2,z3,x3),
C                      dbarea(x1,y1,x2,y2,x3,y3) >.
C
C########################################################################
C
      dbarea(u1,v1,u2,v2,u3,v3)=(u2-u1)*(v3-v1)-(v2-v1)*(u3-u1)
C
C#######################################################################
C
C Initialize error flag and other assorted goodies
C
      pi=pie
      ierror = 0
      cmoin = '-cmo-'
      cmoout = '-none-'
      cmotmp = 'cmotmp'
      isubname = 'offsetpara'
C
C#######################################################################
C
C     Check the gross syntax of the command entered
C
      if((nwds.ne.4).AND.(nwds.ne.5)) then
         write(logmess,'(a)')
     &        'Error in subroutine offsetpara: The proper Syntax is:'
         call writloga('default',0,logmess,0,ierror)
         write(logmess,'(a)')
     &       'offsetpara/cmoout/cmoin/offset/[+|-]'
         call writloga('default',0,logmess,0,ierror)
         ierror = 1
         go to 9999
      endif
C
C#######################################################################
C
C     Get the offset distance
C
      if(msgtype(4).eq.1) then
         d=imsgin(4)
      elseif(msgtype(4).eq.2) then
         d=xmsgin(4)
      else
         write(logmess,'(a)')
     &        'Error in subroutine offsetpara: offset is not a number!'
         call writloga('default',0,logmess,0,ierror)
         ierror = 1
         go to 9999
      endif
C
C#######################################################################
C
C     Get the direction of the offset
C
      if((nwds.eq.5).AND.(cmsgin(5).eq.'-')) then
         sign = -1
      elseif((nwds.eq.5).AND.(cmsgin(5).ne.'+')) then
         write(logmess,'(a)')
     &        'Warning: direction is not + or - assuming +!'
         call writloga('default',0,logmess,0,ierror)
         sign=1
      else
         sign=1
      endif
C
C#######################################################################
C
C     Initialize the Mesh Objects (Harder than it sounds)
C
C     ******************************************************************
C     Check if the incoming MO exists
C
      cmoin=cmsgin(3)
      call cmo_exist(cmoin,ierror)
      if(ierror.ne.0) then
         write(logmess,'(a)')
     &        'Error in subroutine extrude: input MO does not exist'
         call writloga('default',0,logmess,0,ierror)
         ierror = 1
         go to 9999
      endif
C
C     ******************************************************************
C     Check to see if the incoming MO is eligible for this transformation
C     (i.e., it is topologically <= 2D and is made up of lines, tris,
C     or quads)
C
      call cmo_get_info('ndimensions_topo',cmoin,
     &     nsdtopo,ilen,icmotype,ierror)
      call cmo_get_info('nodes_per_element',cmoin,
     &     nen,ilen,icmotype,ierror)
 
      if(nsdtopo.gt.2) then
         write(logmess,'(a)')
     &        'Error in subroutine offsetpara: cmoin is not <= 2D!'
         call writloga('default',0,logmess,0,ierror)
         ierror = 1
         go to 9999
      endif
      if((nen.ne.4).AND.(nen.ne.3).AND.(nen.ne.2)) then
         write(logmess,5)
 5       format('Error in subroutine offsetpara: Only quads, lines,',
     &        'and tris are supported!')
         call writloga('default',0,logmess,0,ierror)
         ierror = 1
         go to 9999
      elseif(nen.eq.4) then
         write(logmess,'(a)')
     &    'Message: quads detected; transforming to tris before offset!'
         call writloga('default',0,logmess,0,ierror)
C
C        Create the command line for the transformation
C
C     ******************************************************************
C     Ensure Temporary MO names are not in use... if they are, modify
C     them. Now... if someone decides to use cmotmp2%%%%%%%,
C     We are not responsible.
         do i=7,32
            call cmo_exist(cmotmp,ierror)
            if(ierror.eq.0) then
               do j=7,32
                  if(cmotmp(j:j).eq.' ') then
                     cmotmp(j:j)='%'
                     goto 7
                  endif
               enddo
 7             continue
            else
               goto 10
            endif
         enddo
 10      continue
C
C     Now that we have a valid temporary MO name, call hextotet
         write(cmdmess,15) cmotmp,cmoin
 15      format('hextotet/2/',A,'/',A,'; finish')
         call dotaskx3d(cmdmess,ierror)
         if(ierror.ne.0) goto 9999
         cmoin=cmotmp
      endif
C
C
C     ******************************************************************
C     Get incoming MO information
C
      call cmo_get_info('nnodes',cmoin,nnodes,ilen,icmotype,ierror)
      call cmo_get_info('nelements',cmoin,
     &     nelements,ilen,icmotype,ierror)
      call cmo_get_info('ndimensions_geom',cmoin,
     &     nsdgeom,ilen,icmotype,ierror)
      call cmo_get_info('ndimensions_topo',cmoin,
     &     nsdtopo,ilen,icmotype,ierror)
      call cmo_get_info('nodes_per_element',cmoin,
     &     nen,ilen,icmotype,ierror)
      call cmo_get_info('faces_per_element',cmoin,
     &     nef,ilen,icmotype,ierror)
      call cmo_get_info('edges_per_element',cmoin,
     &     nee,ilen,icmotype,ierror)
      call cmo_get_info('mbndry',cmoin,mbndry,ilen,icmotype,ierror)
      call cmo_get_info('itp1',cmoin,ipitp1,ilen,icmotype,ierror)
      call cmo_get_info('imt1',cmoin,ipimt1,ilen,icmotype,ierror)
      call cmo_get_info('isn1',cmoin,ipisn1,ilen,icmotype,ierror)
      call cmo_get_info('xic',cmoin,ipxic,ilen,icmotype,ierror)
      call cmo_get_info('yic',cmoin,ipyic,ilen,icmotype,ierror)
      call cmo_get_info('zic',cmoin,ipzic,ilen,icmotype,ierror)
      call cmo_get_info('itetclr',cmoin,ipitetclr,ilen,icmotype
     &     ,ierror)
      call cmo_get_info('itettyp',cmoin,ipitettyp,ilen,icmotype
     &     ,ierror)
      call cmo_get_info('itet',cmoin,ipitet,ilen,icmotype,ierror)
      call cmo_get_info('itetoff',cmoin,ipitetoff,ilen,icmotype
     &     ,ierror)
      call cmo_get_info('jtet',cmoin,ipjtet,ilen,icmotype,ierror)
      call cmo_get_info('jtetoff',cmoin,ipjtetoff,ilen,icmotype
     &                      ,ierror)
C
C
C     ******************************************************************
C     Create the output MO
C
C     Check if the output MO exists, if it does, remove it.
      cmoout = cmsgin(2)
      call cmo_exist(cmoout,ierror)
      if(ierror.eq.0) call cmo_release(cmoout,idelete)
C
      call cmo_create(cmoout,ierror)
C
C     Set the information for the type of mesh object this happens to be
C
      call cmo_set_info('nnodes',cmoout,nnodes,1,1,ierror)
      call cmo_set_info('nelements',cmoout,nelements,1,1,ierror)
      call cmo_set_info('ndimensions_topo',cmoout,nsdtopo,1,1,ierror)
      call cmo_set_info('ndimensions_geom',cmoout,nsdgeom,1,1,ierror)
      call cmo_set_info('nodes_per_element',cmoout,nen,1,1,ierror)
      call cmo_set_info('faces_per_element',cmoout,nef,1,1,ierror)
      call cmo_set_info('edges_per_element',cmoout,nee,1,1,ierror)
C
C     Reallocate memory.
      call cmo_newlen(cmoout,ierror)
C
C     ******************************************************************
C     Get output MO information
C
      call cmo_get_intinfo('mbndry',cmoout,mbndry,
     *                  ilen,icmotype,ierror)
 
      call cmo_get_info('imt1',cmoout,ipimt1o,ilen,icmotype,ierror)
      call cmo_get_info('itp1',cmoout,ipitp1o,ilen,icmotype,ierror)
      call cmo_get_info('isn1',cmoout,ipisn1o,ilen,icmotype,ierror)
      call cmo_get_info('xic',cmoout,ipxico,ilen,icmotype,ierror)
      call cmo_get_info('yic',cmoout,ipyico,ilen,icmotype,ierror)
      call cmo_get_info('zic',cmoout,ipzico,ilen,icmotype,ierror)
      call cmo_get_info('itetclr',cmoout,ipitetclro,ilen,
     &     icmotype,ierror)
      call cmo_get_info('itettyp',cmoout,ipitettypo,ilen,
     &     icmotype,ierror)
      call cmo_get_info('itet',cmoout,ipiteto,ilen,icmotype,ierror)
      call cmo_get_info('itetoff',cmoout,ipitetoffo,ilen,
     &     icmotype,ierror)
      call cmo_get_info('jtet',cmoout,ipjteto,ilen,icmotype,ierror)
      call cmo_get_info('jtetoff',cmoout,ipjtetoffo,ilen,
     &                   icmotype,ierror)
C
C#######################################################################
C
C     Initialize Local Arrays
C
C     ******************************************************************
C     Allocate memory for the temporary node arrays.
C
      call mmgetblk('nodeidx',isubname,ipnodeidx,nen,1,icscode)
      if(icscode.ne.0) call x3d_error(isubname,'mmgetblk')
      call mmgetblk('nodex',isubname,ipnodex,nen,2,icscode)
      if(icscode.ne.0) call x3d_error(isubname,'mmgetblk')
      call mmgetblk('nodey',isubname,ipnodey,nen,2,icscode)
      if(icscode.ne.0) call x3d_error(isubname,'mmgetblk')
      call mmgetblk('nodez',isubname,ipnodez,nen,2,icscode)
      if(icscode.ne.0) call x3d_error(isubname,'mmgetblk')
C
      do i=1,nen
         nodeidx(i)=0
         nodex(i)=0
         nodey(i)=0
         nodez(i)=0
      enddo
C
C     ******************************************************************
C     Allocate memory for the direction vectors
C
      call mmgetblk('xvect',isubname,ipxvect,nelements,2,icscode)
      if(icscode.ne.0) call x3d_error(isubname,'mmgetblk')
      call mmgetblk('yvect',isubname,ipyvect,nelements,2,icscode)
      if(icscode.ne.0) call x3d_error(isubname,'mmgetblk')
      call mmgetblk('zvect',isubname,ipzvect,nelements,2,icscode)
      if(icscode.ne.0) call x3d_error(isubname,'mmgetblk')
C
      do i=1,nelements
         xvect(i)=0
         yvect(i)=0
         zvect(i)=0
      enddo
C
C     ******************************************************************
C     Allocate memory for element retrieval arrays
C
      call mmgetblk('iparent',isubname,ipiparent,nnodes,1,icscode)
      length=100
      call mmgetblk('elts',isubname,ipelts,length,1,icscode)
C
C     Initialize the iparent array
      call unpackpc(nnodes,itp1,isn1,iparent,icscode)
      if(icscode.ne.0) call x3d_error(isubname,'unpackpc')
C
C########################################################################
C     Figure out if we have a polyline or a surface, if it is a polyline,
C     do different things ;-)
C
      if(nen.eq.2) then
C        Find the end of the line...
         prevelmidx = 0
         elmidx = 1
 16      if((jtet(jtetoff(elmidx)+1).eq.mbndry).OR.
     &        (jtet(jtetoff(elmidx)+2).eq.mbndry)) then
            endnode = itet(itetoff(elmidx)+1)
            temp = itet(itetoff(elmidx)+2)
            tempctr = 0
            endnodectr = 0
            do n=1,nelements
               if((itet(itetoff(n)+1).eq.endnode).OR.
     &              (itet(itetoff(n)+2).eq.endnode)) then
                  endnodectr = endnodectr+1
               endif
               if((itet(itetoff(n)+1).eq.temp).OR.
     &              (itet(itetoff(n)+2).eq.temp)) then
                  tempctr = tempctr+1
               endif
            enddo
            if(tempctr.eq.1) then
               refnode=endnode
               endnode=temp
               temp=refnode
            endif
            trueendnode=endnode
            trueendidx=elmidx
            true2ndnode=temp
         else
            if(mod(jtet(jtetoff(elmidx)+1),mbndry).ne.prevelmidx) then
               prevelmidx=elmidx
               elmidx=mod(jtet(jtetoff(elmidx)+1),mbndry)
               goto 16
            else
               prevelmidx=elmidx
               elmidx=mod(jtet(jtetoff(elmidx)+2),mbndry)
               goto 16
            endif
         endif
 
C        Now that we have the end, ensure the polyline lies in a plane.
C        [xyz]vect(1) and [xyz]vect(2) are used to store vectors that lie
C        in the alleged plane.
C        General setup for the vectors: one vector is (temp->endnode), the
C        other is (temp->refnode)
         if(jtet(jtetoff(elmidx)+1).eq.mbndry) then
            if(itet(itetoff(mod(jtet(jtetoff(elmidx)+2),mbndry))+1).eq.
     &           temp) then
               refnode = itet(itetoff(mod(jtet(jtetoff(elmidx)+2),
     &              mbndry))+2)
            else
               refnode = itet(itetoff(mod(jtet(jtetoff(elmidx)+2),
     &              mbndry))+1)
            endif
            prevelmidx = elmidx
            elmidx = mod(jtet(jtetoff(elmidx)+2),mbndry)
         else
            if(itet(itetoff(mod(jtet(jtetoff(elmidx)+1),mbndry))+1).eq.
     &           temp) then
               refnode = itet(itetoff(mod(jtet(jtetoff(elmidx)+1),
     &              mbndry))+2)
            else
               refnode = itet(itetoff(mod(jtet(jtetoff(elmidx)+1),
     &              mbndry))+1)
            endif
            prevelmidx = elmidx
            elmidx = mod(jtet(jtetoff(elmidx)+1),mbndry)
         endif
C
C        Make the actual vectors
 17      xvect(1)=xic(endnode)-xic(temp)
         xvect(2)=xic(refnode)-xic(temp)
         yvect(1)=yic(endnode)-yic(temp)
         yvect(2)=yic(refnode)-yic(temp)
         zvect(1)=zic(endnode)-zic(temp)
         zvect(2)=zic(refnode)-zic(temp)
C
C        cross them
         
         A=dbarea(yvect(1),zvect(1),yvect(2),zvect(2),yvect(3),zvect(3))
         B=dbarea(zvect(1),xvect(1),zvect(2),xvect(2),zvect(3),xvect(3))
         C=dbarea(xvect(1),yvect(1),xvect(2),yvect(2),xvect(3),yvect(3))
C
         anorm=sqrt(A**2+B**2+C**2)
         if(anorm.le.epsln) then
            endnode=temp
            temp=refnode
            if(mod(jtet(jtetoff(elmidx)+1),mbndry).eq.prevelmidx) then
               if(itet(itetoff(mod(jtet(jtetoff(elmidx)+2),mbndry))+1)
     &              .eq.temp) then
                  refnode=itet(itetoff(mod(jtet(jtetoff(elmidx)+2),
     &                 mbndry))+2)
               else
                  refnode=itet(itetoff(mod(jtet(jtetoff(elmidx)+2),
     &                 mbndry))+1)
               endif
               prevelmidx=elmidx
               elmidx=mod(jtet(jtetoff(elmidx)+2),mbndry)
            else
               if(itet(itetoff(mod(jtet(jtetoff(elmidx)+1),mbndry))+1)
     &              .eq.temp) then
                  refnode=itet(itetoff(mod(jtet(jtetoff(elmidx)+1),
     &                 mbndry))+2)
               else
                  refnode=itet(itetoff(mod(jtet(jtetoff(elmidx)+1),
     &                 mbndry))+1)
               endif
               prevelmidx=elmidx
               elmidx=mod(jtet(jtetoff(elmidx)+1),mbndry)
            endif
            goto 17
         endif
C
C        Figure out if the polyline is planar.
         do i=1,nnodes
            if(A*(xic(temp)-xic(i))+B*(yic(temp)-yic(i))
     &           +C*(zic(temp)-zic(i)).gt.epsln) then
               write(logmess,'(a,a)')
     &              'Error in subroutine offsetpara: ',
     &              'cmoin is a non-planar polyline!'
               call writloga('default',0,logmess,0,ierror)
               ierror = 1
               go to 9999
            endif
         enddo
C
C
C        begin by figuring out the proper direction
C        to move the lines.
         anorm=sqrt(xvect(1)**2+yvect(1)**2+zvect(1)**2)
         xvect(1)=xvect(1)/anorm
         yvect(1)=yvect(1)/anorm
         zvect(1)=zvect(1)/anorm
         anorm=sqrt(xvect(2)**2+yvect(2)**2+zvect(2)**2)
         xvect(2)=xvect(2)/anorm
         yvect(2)=yvect(2)/anorm
         zvect(2)=zvect(2)/anorm
C
         xrefvect=(xvect(1)+xvect(2))/2
         yrefvect=(yvect(1)+yvect(2))/2
         zrefvect=(zvect(1)+zvect(2))/2
C
         endnode=trueendnode
         elmidx = trueendidx
         temp=true2ndnode
         prevelmidx = 0
 
 18      if(mod(jtet(jtetoff(elmidx)+1),mbndry).eq.prevelmidx) then
            if(itet(itetoff(mod(jtet(jtetoff(elmidx)+2),mbndry))+1).eq.
     &           temp) then
               refnode = itet(itetoff(mod(jtet(jtetoff(elmidx)+2),
     &              mbndry))+2)
            else
               refnode = itet(itetoff(mod(jtet(jtetoff(elmidx)+2),
     &              mbndry))+1)
            endif
            prevelmidx = elmidx
            elmidx = mod(jtet(jtetoff(elmidx)+2),mbndry)
         else
            if(itet(itetoff(mod(jtet(jtetoff(elmidx)+1),mbndry))+1).eq.
     &           temp) then
               refnode = itet(itetoff(mod(jtet(jtetoff(elmidx)+1),
     &              mbndry))+2)
            else
               refnode = itet(itetoff(mod(jtet(jtetoff(elmidx)+1),
     &              mbndry))+1)
            endif
            prevelmidx = elmidx
            elmidx = mod(jtet(jtetoff(elmidx)+1),mbndry)
         endif
               write(logmess,'(a,a)')
     &              'The polyline implementation is still not ',
     &              'finished, will be completed soon!'
               call writloga('default',0,logmess,0,ierror)
               ierror = 1
               go to 9999
      endif
 
 
 
C########################################################################
C
C     We have a surface... begin the process of finding the direction
C     vector
C
C     *******************************************************************
C     Find the normal vectors to each of the elements (this is easy) and
C     normalize them.
C
      do itri=1,nelements
C
         do i=1,nelmnen(itettyp(itri))
            nodeidx(i)=itet(itetoff(itri)+i)
         enddo
C
         id1=nodeidx(mod(i,nelmnen(itettyp(itri)))+1)
         id2=nodeidx(mod((i+1),nelmnen(itettyp(itri)))+1)
         id3=nodeidx(mod((i+2),nelmnen(itettyp(itri)))+1)
C
C        Calculate out the normal vectors.
         xvect(itri)=dbarea(yic(id2),zic(id2),yic(id1),
     &        zic(id1),yic(id3),zic(id3))
         yvect(itri)=dbarea(zic(id2),xic(id2),zic(id1),
     &        xic(id1),zic(id3),xic(id3))
         zvect(itri)=dbarea(xic(id2),yic(id2),xic(id1),
     &        yic(id1),xic(id3),yic(id3))
         anorm=sqrt((xvect(itri)**2)+(yvect(itri)**2)
     &        +(zvect(itri)**2))
C
C     Normalize all the normal vectors
         xvect(itri)=xvect(itri)/anorm
         yvect(itri)=yvect(itri)/anorm
         zvect(itri)=zvect(itri)/anorm
      enddo
C
C########################################################################
C
C     Ok, we now have the planes (defined by point-normal form) that make
C     up the elements defined. Now, for each node, figure out how it is
C     supposed to move.
C
C     *******************************************************************
C     1) Get all the surrounding elements.
      do itri=1,nelements
         do i=1,nelmnen(itettyp(itri))
            refnode=itet(itetoff(itri)+i)
 20         if((isn1(refnode).ne.0).AND.(itp1(refnode).ne.41)) then
               refnode=isn1(refnode)
               goto 20
            endif
            nelts=0
            call get_elements_around_node(itri,i,nelts,ipelts,
     &           itetoff,jtetoff,itet,jtet,itettyp,iparent,
     &           nef,mbndry)
 
C
C     ******************************************************************
C     2) Modify elts to ensure that only non-coplanar elements are left
C
            elmidx=1
            do j=1,nelts
               do k=1,j-1
                  if((sqrt((xvect(elts(j))-xvect(elts(k)))**2+
     &                 (yvect(elts(j))-yvect(elts(k)))**2+
     &                 (zvect(elts(j))-zvect(elts(k)))**2).lt.epsln).OR.
     &                 (sqrt((xvect(elts(j))+xvect(elts(k)))**2+
     &                 (yvect(elts(j))+yvect(elts(k)))**2+
     &                 (zvect(elts(j))+zvect(elts(k)))**2).lt.epsln))
     &                 then
                     goto 25
                  endif
               enddo
               if(j.gt.elmidx) then
                  elts(elmidx)=elts(j)
               endif
               elmidx=elmidx+1
 25         enddo
            nelts=elmidx-1
 
C
C     ******************************************************************
C     3) Based on the number of non-coplanar elements, figure out how
C        to move the node in question
            if((xico(refnode).eq.0).AND.(yico(refnode).eq.0).AND.
     &           (zico(refnode).eq.0)) then
               curr_elt = 4
 30            if(nelts.eq.1) then
                  xico(refnode)=xic(refnode)+xvect(elts(1))*d*sign
                  yico(refnode)=yic(refnode)+yvect(elts(1))*d*sign
                  zico(refnode)=zic(refnode)+zvect(elts(1))*d*sign
C
               elseif(nelts.eq.2) then
                  xbisect=(xvect(elts(1))+xvect(elts(2)))/2.0
                  ybisect=(yvect(elts(1))+yvect(elts(2)))/2.0
                  zbisect=(zvect(elts(1))+zvect(elts(2)))/2.0
                  anorm=sqrt((xbisect**2)+(ybisect**2)+(zbisect**2))
                  xbisect=xbisect/anorm
                  ybisect=ybisect/anorm
                  zbisect=zbisect/anorm
C
C                 Find the distance offset factor...
C
                  dfact=xvect(elts(1))*xvect(elts(2))+
     &                 yvect(elts(1))*yvect(elts(2))+
     &                 zvect(elts(1))*zvect(elts(2))
                  dfact=acos(dfact)
                  dfact=(pi-dfact)/2.0
                  dfact=1.0/sin(dfact)
C
C                 Offset the node by the distance
                  xico(refnode)=xic(refnode)+xbisect*d*dfact*sign
                  yico(refnode)=yic(refnode)+ybisect*d*dfact*sign
                  zico(refnode)=zic(refnode)+zbisect*d*dfact*sign
C
               elseif(nelts.ge.3) then
C                 1) find the new points for the translated planes
C                 Point 1
                  p1x=xic(refnode)+(xvect(elts(1))*d*sign)
                  p1y=yic(refnode)+(yvect(elts(1))*d*sign)
                  p1z=zic(refnode)+(zvect(elts(1))*d*sign)
C                 Point 2
                  p2x=xic(refnode)+(xvect(elts(2))*d*sign)
                  p2y=yic(refnode)+(yvect(elts(2))*d*sign)
                  p2z=zic(refnode)+(zvect(elts(2))*d*sign)
C                 Point 3
                  p3x=xic(refnode)+(xvect(elts(3))*d*sign)
                  p3y=yic(refnode)+(yvect(elts(3))*d*sign)
                  p3z=zic(refnode)+(zvect(elts(3))*d*sign)
C
C                 2) Find -D so we can use it to find the intersection
                  d1=xvect(elts(1))*p1x+yvect(elts(1))*p1y+
     &                 zvect(elts(1))*p1z
                  d2=xvect(elts(2))*p2x+yvect(elts(2))*p2y+
     &                 zvect(elts(2))*p2z
                  d3=xvect(elts(3))*p3x+yvect(elts(3))*p3y+
     &                 zvect(elts(3))*p3z
C
C                 3) Find delta (denominator of the determinant)
                  delta=xvect(elts(1))*(yvect(elts(2))*zvect(elts(3))-
     &                 zvect(elts(2))*yvect(elts(3)))-
     &                 yvect(elts(1))*(xvect(elts(2))*zvect(elts(3))-
     &                 zvect(elts(2))*xvect(elts(3)))+
     &                 zvect(elts(1))*(xvect(elts(2))*yvect(elts(3))-
     &                 yvect(elts(2))*xvect(elts(3)))
C
C     If Delta is zero, the vectors are identical, so somehow the original
C     co-planar check failed. Find the vectors that are causing the
C     problem and attempt to correct it.
C
 
                  if(delta**2.lt.epsln) then
                     dot12=abs(xvect(elts(1))*xvect(elts(2))+
     &                    yvect(elts(1))*yvect(elts(2))+
     &                    zvect(elts(1))*zvect(elts(2)))
                     dot23=abs(xvect(elts(3))*xvect(elts(2))+
     &                    yvect(elts(3))*yvect(elts(2))+
     &                    zvect(elts(3))*zvect(elts(2)))
                     dot31=abs(xvect(elts(1))*xvect(elts(3))+
     &                    yvect(elts(1))*yvect(elts(3))+
     &                    zvect(elts(1))*zvect(elts(3)))
                     dot12=abs(1.0-dot12)
                     dot23=abs(1.0-dot23)
                     dot31=abs(1.0-dot31)
                     if(curr_elt.le.nelts) then
C
                        if((dot12.lt.dot23).AND.(dot12.lt.dot31)) then
                           elts(1) = elts(curr_elt)
                        else
                           elts(3) = elts(curr_elt)
                        endif
 
                        curr_elt=curr_elt+1
                        goto 30
                     else
                        if((dot12.lt.dot23).AND.(dot12.lt.dot31)) then
                           elts(2)=elts(3)
                        endif
                        nelts=2
                        goto 30
                     endif
                     print*,delta,xvar,yvar,zvar
                     do test=1,3
                        print*,'Vector',test,': <',xvect(elts(test)),
     &                       ',',yvect(elts(test)),',',
     &                       zvect(elts(test)),'>'
                     enddo
                  endif
C
C                 4) Find the X variant on the determinant
                  xvar=d1*(yvect(elts(2))*zvect(elts(3))-
     &                 zvect(elts(2))*yvect(elts(3)))-
     &                 yvect(elts(1))*(d2*zvect(elts(3))-
     &                 zvect(elts(2))*d3)+
     &                 zvect(elts(1))*(d2*yvect(elts(3))-
     &                 yvect(elts(2))*d3)
C
C                 5) Find the Y variant on the determinant
                  yvar=xvect(elts(1))*(d2*zvect(elts(3))-
     &                 zvect(elts(2))*d3)-
     &                 d1*(xvect(elts(2))*zvect(elts(3))-
     &                 zvect(elts(2))*xvect(elts(3)))+
     &                 zvect(elts(1))*(xvect(elts(2))*d3-
     &                 d2*xvect(elts(3)))
C
C                 6) Find the Z variant on the determinant
                  zvar=xvect(elts(1))*(yvect(elts(2))*d3-
     &                 d2*yvect(elts(3)))-
     &                 yvect(elts(1))*(xvect(elts(2))*d3-
     &                 d2*xvect(elts(3)))+
     &                 d1*(xvect(elts(2))*yvect(elts(3))-
     &                 yvect(elts(2))*xvect(elts(3)))
C
C                 7) Finalize the output point
                  xico(refnode)=xvar/delta
                  yico(refnode)=yvar/delta
                  zico(refnode)=zvar/delta
               endif
C              Propagate the node coordinates througout the parent/child
C              chain
               refnode1=refnode
 35            if(isn1(refnode1).ne.0) then
                  refnode1=isn1(refnode1)
                  xico(refnode1)=xico(refnode)
                  yico(refnode1)=yico(refnode)
                  zico(refnode1)=zico(refnode)
                  if(refnode.eq.refnode1) goto 40
               endif
 40            continue
            endif
         enddo
      enddo
C
C     Debugging Time...
C
C
      avg_deflection = 0.0
      max_deflection = 0.0
C
      do itri=1,nelements
C
         do i=1,nelmnen(itettyp(itri))
            nodeidx(i)=itet(itetoff(itri)+i)
         enddo
C
         id1=nodeidx(mod(i,nelmnen(itettyp(itri)))+1)
         id2=nodeidx(mod((i+1),nelmnen(itettyp(itri)))+1)
         id3=nodeidx(mod((i+2),nelmnen(itettyp(itri)))+1)
C
C        Calculate out the normal vectors.
         xcheck=dbarea(yico(id2),zico(id2),yico(id1),
     &        zico(id1),yico(id3),zico(id3))
         ycheck=dbarea(zico(id2),xico(id2),zico(id1),
     &        xico(id1),zico(id3),xico(id3))
         zcheck=dbarea(xico(id2),yico(id2),xico(id1),
     &        yico(id1),xico(id3),yico(id3))
         anorm=sqrt((xcheck**2)+(ycheck**2)
     &        +(zcheck**2))
C
C     Normalize all the normal vectors
         xcheck=xcheck/anorm
         ycheck=ycheck/anorm
         zcheck=zcheck/anorm
C
C     Sanity Check
         deflection = abs(acos(xcheck*xvect(itri)+ycheck*yvect(itri)+
     &        zcheck*zvect(itri)))
         if(deflection.gt.max_deflection) then
            max_deflection = deflection
         endif
         avg_deflection = avg_deflection + deflection
      enddo
      avg_deflection = (avg_deflection*180.0)/(nelements*pi)
      max_deflection = (max_deflection*180.0)/pi
      print *,'Average Deflection (degrees): ',avg_deflection
      print *,'Maximum Deflection (degrees): ',max_deflection
 
C
C     ******************************************************************
C     Copy the MO's attributes from the old MO to the new one.
C
C     Node-Based
      do i=1,nnodes
         imt1o(i)=imt1(i)
         isn1o(i)=isn1(i)
         itp1o(i)=isn1(i)
      enddo
C     Element-Based
      do itri=1,nelements
         do inode = 1,nelmnen(itettyp(itri))
            iteto(itetoff(itri)+inode)=itet(itetoff(itri)+inode)
            jteto(jtetoff(itri)+inode)=jtet(jtetoff(itri)+inode)
         enddo
C
         itetclro(itri)=itetclr(itri)
         itettypo(itri)=itettyp(itri)
         itetoffo(itri)=itetoff(itri)
         jtetoffo(itri)=jtetoff(itri)
C
      enddo
C
C     *******************************************************************
C     Release temporary memory and be done with it
C
 9999 continue
 9995 call mmrelprt(isubname,ics)
      call cmo_exist(cmotmp,isdel)
      if(isdel.eq.0) call cmo_release(cmotmp,isdel)
      return
      end
 
 
 
 
