      subroutine synthnormal(node,nelts,ielts,iparent,itet,itetoff,
     &   xic,yic,zic,epsln,synthx,synthy,synthz,lsomereversed)
C
C #####################################################################
C
C     PURPOSE -
C
C        This routine computes the (angle-weighted) synthetic normal
C        around NODE in a triangular mesh.  It then checks if some
C        of the triangles have a reversed orientation with respect 
C        to this normal.
C
C     INPUT ARGUMENTS -
C
C       NODE -- central node.
C       NELTS -- number of surrounding triangles.
C       IELTS -- array of triangle numbers.
C       IPARENT -- parent node array.
C       ITET -- triangle-node relation.
C       ITETOFF -- offset array for triangle node relation.
C       XIC,YIC,ZIC -- x,y,z node coordinate arrays.
C
C     OUTPUT ARGUMENTS -
C
C        SYNTHX,SYNTHY,SYNTHZ -- synthetic normal.
C        LSOMEREVERSED -- .TRUE. if there are some reversed triangles.
C
C     CHANGE HISTORY -
C
C        $Log:   /pvcs.config/t3d/src/synthnormal.f_a  $
CPVCS    
CPVCS       Rev 1.2   Wed Nov 25 16:13:14 1998   dcg
CPVCS    fix confusion between angle and cos of angle if cos = 0
CPVCS    
CPVCS       Rev 1.1   Mon Oct 26 13:00:24 1998   dcg
CPVCS    refresh pointers
CPVCS    
CPVCS       Rev 1.0   Fri Oct 23 16:28:54 1998   kuprat
CPVCS    Initial revision.

      implicit none

      include 'consts.h'

      integer node,nelts,ielts(*),iparent(*),itet(*),itetoff(*)
      real*8 xic(*),yic(*),zic(*),synthx,synthy,synthz
      logical lsomereversed

      pointer (ipax,ax),(ipay,ay),(ipaz,az)
      real*8 ax(*),ay(*),az(*)

      real*8 epsln,asumx,asumy,asumz,top,bot,ang,a,dot,atot
      integer ind1,ind2,ind3,ielt,i1,i2,i3,j,k
      character*32 isubname
      integer icscode,len_nelts,inc
      save len_nelts

      include 'statementfunctions.h'

      data len_nelts/0/

      isubname='synthnormal'

      if (len_nelts.eq.0) then
         len_nelts=nelts+100
         call mmgetblk('ax',isubname,ipax,len_nelts,2,icscode)
         call mmgetblk('ay',isubname,ipay,len_nelts,2,icscode)
         call mmgetblk('az',isubname,ipaz,len_nelts,2,icscode)
      else
         call mmfindbk('ax',isubname,ipax,len_nelts,icscode)
         call mmfindbk('ay',isubname,ipay,len_nelts,icscode)
         call mmfindbk('az',isubname,ipaz,len_nelts,icscode)
      endif
      if (len_nelts.lt.nelts) then
         inc=nelts-len_nelts+100
         len_nelts=len_nelts+inc
         call mmincblk('ax',isubname,ipax,inc,icscode)
         call mmincblk('ay',isubname,ipay,inc,icscode)
         call mmincblk('az',isubname,ipaz,inc,icscode)
      endif

      asumx=0.
      asumy=0.
      asumz=0.
      do j=1,nelts
         ielt=ielts(j)
         ind1=0
         do k=1,3
            if (iparent(itet(k+itetoff(ielt))).eq.node) then
               ind1=k
            endif
         enddo
         if (ind1.eq.0) then
            print*,'Synthnormal: Node ',node,' not found.'
            stop
         endif
         ind2=mod(ind1,3)+1
         ind3=mod(ind2,3)+1

         i1=itet(ind1+itetoff(ielt))
         i2=itet(ind2+itetoff(ielt))
         i3=itet(ind3+itetoff(ielt))
         ax(j)=dcrosx(xic(i1),yic(i1),zic(i1),xic(i2),
     &      yic(i2),zic(i2),xic(i3),yic(i3),zic(i3))
         ay(j)=dcrosy(xic(i1),yic(i1),zic(i1),xic(i2),
     &      yic(i2),zic(i2),xic(i3),yic(i3),zic(i3))
         az(j)=dcrosz(xic(i1),yic(i1),zic(i1),xic(i2),
     &      yic(i2),zic(i2),xic(i3),yic(i3),zic(i3))
         top=(xic(i2)-xic(i1))*(xic(i3)-xic(i1))+(yic(i2)-yic(i1))
     &      *(yic(i3)-yic(i1))+(zic(i2)-zic(i1))*(zic(i3)-zic(i1))
         bot=sqrt((xic(i2)-xic(i1))**2+(yic(i2)-yic(i1))**2+(zic(i2)
     &      -zic(i1))**2)*sqrt((xic(i3)-xic(i1))**2+(yic(i3)-yic(i1))**2
     &      +(zic(i3)-zic(i1))**2)
         if (bot.lt.epsln**2) then
            ang=1.5707963267948966
         else
            ang=acos(top/bot)
         endif

         a=max(epsln,sqrt(ax(j)**2+ay(j)**2+az(j)**2))
         
         asumx=asumx+ax(j)*ang/a
         asumy=asumy+ay(j)*ang/a
         asumz=asumz+az(j)*ang/a
      enddo
      atot=max(epsln,sqrt(asumx**2+asumy**2+asumz**2))
      synthx=asumx/atot
      synthy=asumy/atot
      synthz=asumz/atot
      lsomereversed=.false.

C....  Check old triangles against average normal

      do j=1,nelts
         dot=ax(j)*synthx+ay(j)*synthy+az(j)*synthz
         if(dot.lt.zero) lsomereversed=.true.
      enddo

      return
      end
