      subroutine tangent_plane(xic,yic,zic,it,inode,int1,itetoff,
     *   jtetoff,itet,jtet,itettyp,itetclr,iparent,nef,mbndry,epsln,
     *   a,b,c,d)
C
C #####################################################################
C
C     PURPOSE -
C
C        This routine computes the (angle-weighted) synthetic normal
C        around INODE in a triangular mesh (a,b,c).  It then computes
C        the equation of the tangent plane at this node (a,b,c,d)
C
C     INPUT ARGUMENTS -
C
C       XIC,YIC,ZIC -- x,y,z node coordinate arrays.
C       IT -- tet number one of whose vertices is inode.
C       INODE --  local central node number wrt. it.
C       INT1 -- node indexed array =1 if node is on query surface
C                                  =0 if node is not on surface.
C       ITETOFF,JTETOFF,ITET1,JTET1 - connectivity of mesh object
C       ITETTYP -- array of element types
C       IPARENT -- parent node array.
C       NEF -- number of faces for this mesh object - must be 4.
C       MBNDRY -- interface/boundary flag value
C       EPSLN -- mesh object epsilon
C
C     OUTPUT ARGUMENTS -
C
C        A,B,C -- synthetic normal.
C        A,B,C,D -- equation of plane is Ax+By+Cz+D=0.
C
C     CHANGE HISTORY -
C
C        $Log:   /pvcs.config/t3d/src/tangent_plane.f_a  $
CPVCS    
CPVCS       Rev 1.5   Tue Feb 23 15:20:52 1999   dcg
CPVCS    check for matching colors of elements when accepting
CPVCS    contributions for the tangent plane
CPVCS    
CPVCS       Rev 1.4   Thu Dec 24 13:55:26 1998   dcg
CPVCS    allow for parent/child chains
CPVCS    
CPVCS       Rev 1.3   Wed Dec 23 13:59:12 1998   jtg
CPVCS    removed duplicate declaration of a
CPVCS    
CPVCS       Rev 1.2   Thu Dec 03 13:00:46 1998   dcg
CPVCS    fix complicated nested if
CPVCS
CPVCS       Rev 1.1   Thu Dec 03 12:39:22 1998   dcg
CPVCS    fix tests for inclusion of boundary faces
C
      implicit none
      include 'local_element.h'
      include 'consts.h'
 
      integer inode,nelts,ielts(*),iparent(*),itet(*),itetoff(*),
     *  jtet(*),jtetoff(*),int1(*),it,itettyp(*),nef,ityp,mbndry,
     *  nf,itetclr(*),iclref
      real*8 xic(*),yic(*),zic(*),a,b,c,d
 
      pointer (ipielts,ielts)
      real*8 ax,ay,az
 
      real*8 epsln,asumx,asumy,asumz,top,bot,ang,atot
      integer ind1,ind2,ind3,ielt,i1,i2,i3,j,itest,
     *   ipari1,ipari2,ipari3,ipartest
      character*32 isubname
      character*132 logmess
      integer icscode,len_nelts
      save len_nelts
 
      include 'statementfunctions.h'
 
      data len_nelts/0/
 
      isubname='tangent_plane'
      if(nef.ne.4) then
        write(logmess,10) nef
 10     format ('tangent_plane calculation valid only for tet meshes ',
     *     'number of faces not = 4 ',i5)
        call writloga('default',0,logmess,0,icscode)
      endif
C
C   get all elements that surround inode
C
      nelts=100
      call mmgetblk('ielts',isubname,ipielts,nelts,1,icscode)
      call get_elements_around_node(it,inode,nelts,ipielts,
     *  itetoff,jtetoff,itet,jtet,itettyp,iparent,nef,mbndry)
C
C   restrict calculation to faces on surface or faces containing
C   requested edges on external boundaries
C
      asumx=0.
      asumy=0.
      asumz=0.
      iclref=itetclr(it)
      itest=itet(itetoff(it)+inode)
      ipartest= iparent(itest)
      do j=1,nelts
         ielt=ielts(j)
         ityp = itettyp(ielt)
         if(ityp.ne.ifelmtet) then
           write(logmess,12) ityp
 12        format(
     *       'tangent_plane calculation valid only for tet meshes ',
     *       'element type not = 5 ',i5)
           call writloga('default',0,logmess,0,icscode)
         endif
         if(itetclr(ielt).eq.iclref) then
            do nf = 1,4
               i1=itet(itetoff(ielt)+ielmface1(1,nf,ityp))
               i2=itet(itetoff(ielt)+ielmface1(2,nf,ityp))
               i3=itet(itetoff(ielt)+ielmface1(3,nf,ityp))
               ipari1=iparent(i1)
               ipari2=iparent(i2)
               ipari3=iparent(i3)
               if (((int1(i1)+int1(i2)+int1(i3).eq.3).or.
     *          ((int1(i1)+int1(i2)+int1(i3).ge.1).and.
     *            (jtet(jtetoff(ielt)+nf).eq.mbndry)))
     *           .and.
     *          (ipari1.eq.ipartest.or.ipari2.eq.ipartest.or.
     *           ipari3.eq.ipartest))
     *          then
                  if(ipari1.eq.ipartest) then
                   ind1=i1
                   ind2=i2
                   ind3=i3
                  elseif(ipari2.eq.ipartest) then
                   ind1=i2
                   ind2=i3
                   ind3=i1
                  elseif(ipari3.eq.ipartest) then
                   ind1=i3
                   ind2=i1
                   ind3=i2
                  endif
C
C  this face is a contributor
C
                  ax=dcrosx(xic(ind1),yic(ind1),zic(ind1),xic(ind2),
     &             yic(ind2),zic(ind2),xic(ind3),yic(ind3),zic(ind3))
                  ay=dcrosy(xic(ind1),yic(ind1),zic(ind1),xic(ind2),
     &             yic(ind2),zic(ind2),xic(ind3),yic(ind3),zic(ind3))
                  az=dcrosz(xic(ind1),yic(ind1),zic(ind1),xic(ind2),
     &             yic(ind2),zic(ind2),xic(ind3),yic(ind3),zic(ind3))
                  top=(xic(ind2)-xic(ind1))*(xic(ind3)-xic(ind1))+
     *             (yic(ind2)-yic(ind1))*(yic(ind3)-yic(ind1))+
     &             (zic(ind2)-zic(ind1))*(zic(ind3)-zic(ind1))
                  bot=sqrt((xic(ind2)-xic(ind1))**2+
     *                  (yic(ind2)-yic(ind1))**2+
     &                  (zic(ind2)-zic(ind1))**2)*
     *             sqrt((xic(ind3)-xic(ind1))**2+
     &                  (yic(ind3)-yic(ind1))**2+
     *                  (zic(ind3)-zic(ind1))**2)
                  if (bot.lt.epsln**2) then
                     ang=1.5707963267948966
                  else
                     ang=acos(top/bot)
                  endif
                  a=max(epsln,sqrt(ax**2+ay**2+az**2))
                  asumx=asumx+ax*ang/a
                  asumy=asumy+ay*ang/a
                  asumz=asumz+az*ang/a
               endif
            enddo
         endif
      enddo
      atot=max(epsln,sqrt(asumx**2+asumy**2+asumz**2))
      a=asumx/atot
      b=asumy/atot
      c=asumz/atot
      i1=itet(itetoff(it)+inode)
      d=-a*xic(i1)-b*yic(i1)-c*zic(i1)
      call mmrelprt(isubname,icscode)
      return
      end
