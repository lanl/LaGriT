      subroutine getgsynth(eps,cprtname,ieltary,ieltno,iparent,itet
     &   ,itetoff,invmpary,mpno,xic,yic,zic,ipgsynth,len_gsynth)
C
C #####################################################################
C
C     PURPOSE -
C
C        This routine computes the (angle-weighted) synthetic normal
C        around each vertex in a triangular mesh.
C
C     INPUT ARGUMENTS -
C
C       EPS -- Length epsilon.
C       IELTNO -- number of triangles.
C       IELTARY -- array of triangle numbers.
C       IPARENT -- parent node array.
C       ITET -- triangle-node relation.
C       ITETOFF -- offset array for triangle node relation.
C       INVMPARY -- inverse pset mapping.
C       MPNO -- size of pset.
C       XIC,YIC,ZIC -- x,y,z node coordinate arrays.
C
C     OUTPUT ARGUMENTS -
C
C        IPGSYNTH(3,*) -- (pointer to) components of synthetic normal.
C        LEN_GSYNTH -- length of output array.
C
C     CHANGE HISTORY -
C
C        $Log:   /pvcs.config/t3d/src/getgsynth.f_a  $
CPVCS    
CPVCS       Rev 1.2   30 Sep 2004 10:54:08   dcg
CPVCS    make constants double precision
CPVCS    
CPVCS       Rev 1.1   31 May 2001 11:41:30   dcg
CPVCS    remove duplicate declaration
CPVCS    
CPVCS       Rev 1.0   29 May 2001 17:59:12   kuprat
CPVCS    Initial revision.
 
      implicit none
 
      include 'consts.h'
 
      integer ieltno,ieltary(*),iparent(*),itet(*),itetoff(*)
     &   ,invmpary(*),mpno
      real*8 xic(*),yic(*),zic(*),eps
      character*32 cprtname
 
      real*8 ac(3),top,bot,a,atot,rl1,rl2,rl3,ang(3)
      integer i,ielt,i1,i2,i3,j,k,mp(3)
      integer icscode
 
      pointer (ipgsynth,gsynth)
      real*8 gsynth(3,*)
      integer len_gsynth
 
      include 'statementfunctions.h'
 
      if (3*mpno.gt.len_gsynth) call mm_ovall('gsynth',cprtname,
     &   ipgsynth,3*mpno,100,len_gsynth,2,icscode)
      do k=1,3
         do i=1,mpno
            gsynth(k,i)=0.d0
         enddo
      enddo
 
      do j=1,ieltno
         ielt=ieltary(j)
         i1=iparent(itet(1+itetoff(ielt)))
         i2=iparent(itet(2+itetoff(ielt)))
         i3=iparent(itet(3+itetoff(ielt)))
         ac(1)=dcrosx(xic(i1),yic(i1),zic(i1),xic(i2),
     &      yic(i2),zic(i2),xic(i3),yic(i3),zic(i3))
         ac(2)=dcrosy(xic(i1),yic(i1),zic(i1),xic(i2),
     &      yic(i2),zic(i2),xic(i3),yic(i3),zic(i3))
         ac(3)=dcrosz(xic(i1),yic(i1),zic(i1),xic(i2),
     &      yic(i2),zic(i2),xic(i3),yic(i3),zic(i3))
         rl1=sqrt((xic(i3)-xic(i2))**2+(yic(i3)-yic(i2))**2
     &      +(zic(i3)-zic(i2))**2)
         rl2=sqrt((xic(i3)-xic(i1))**2+(yic(i3)-yic(i1))**2
     &      +(zic(i3)-zic(i1))**2)
         rl3=sqrt((xic(i2)-xic(i1))**2+(yic(i2)-yic(i1))**2
     &      +(zic(i2)-zic(i1))**2)
         top=(xic(i2)-xic(i1))*(xic(i3)-xic(i1))+(yic(i2)-yic(i1))
     &      *(yic(i3)-yic(i1))+(zic(i2)-zic(i1))*(zic(i3)-zic(i1))
         bot=rl3*rl2
         if (bot.lt.eps**2) then
            ang(1)=1.5707963267948966d0
         else
            ang(1)=acos(top/bot)
         endif
         top=(xic(i3)-xic(i2))*(xic(i1)-xic(i2))+(yic(i3)-yic(i2))
     &      *(yic(i1)-yic(i2))+(zic(i3)-zic(i2))*(zic(i1)-zic(i2))
         bot=rl1*rl3
         if (bot.lt.eps**2) then
            ang(2)=1.5707963267948966d0
         else
            ang(2)=acos(top/bot)
         endif
         top=(xic(i1)-xic(i3))*(xic(i2)-xic(i3))+(yic(i1)-yic(i3))
     &      *(yic(i2)-yic(i3))+(zic(i1)-zic(i3))*(zic(i2)-zic(i3))
         bot=rl2*rl1
         if (bot.lt.eps**2) then
            ang(3)=1.5707963267948966d0
         else
            ang(3)=acos(top/bot)
         endif
 
         a=max(eps,sqrt(ac(1)**2+ac(2)**2+ac(3)**2))
 
         mp(1)=invmpary(i1)
         mp(2)=invmpary(i2)
         mp(3)=invmpary(i3)
 
         do k=1,3
            do i=1,3
               gsynth(k,mp(i))=gsynth(k,mp(i))+ac(k)*ang(i)/a
            enddo
         enddo
      enddo
      do i=1,mpno
         atot=max(eps,sqrt(gsynth(1,i)**2+gsynth(2,i)**2+gsynth(3,i)**2)
     &      )
         do k=1,3
            gsynth(k,i)=gsynth(k,i)/atot
         enddo
      enddo
 
      return
      end
