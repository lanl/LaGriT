      subroutine edgefun_lg(nelts,ipelts,ipedges,
     &   itettyp,itet,itetoff,xic,yic,zic,
     &   hxx,hxy,hxz,hyy,hyz,hzz,edge_error)
C     #####################################################################
C
C     PURPOSE -
C
C     edgefun_lg returns the value of the edge error.
c     The ``edge'' refers to the fact that the
C     the evaluation is over the all elements
C     sharing the edge.
C
C     INPUT ARGUMENTS -
C
C  
C     OUTPUT ARGUMENTS -
C
C     EDGE_ERROR   the error for the input edge
C
C     CHANGE HISTORY -
C     $Log: edgefun_lg.f,v $
C     Revision 2.00  2007/11/05 19:45:53  spchu
C     Import to CVS
C
CPVCS    
CPVCS       Rev 1.1   10 Sep 2002 13:19:14   kuprat
CPVCS    Divided error estimator by 8, since error=xHx/8.
CPVCS    
CPVCS       Rev 1.0   31 Jan 2002 13:12:22   dcg
CPVCS    Initial revision.

C     ######################################################################
      implicit none
      include 'consts.h'
      include 'local_element.h'
      
      pointer (ipelts,elts),(ipedges,edges)
      integer elts(*),edges(*),nelts
      real*8 hxx(*),hxy(*),hxz(*),hyy(*),hyz(*),hzz(*)
      real*8 ex,ey,ez,ee,edge_error,xic(*),yic(*),zic(*)
      
      integer itet(*),itetoff(*),itettyp(*)      
      integer i,i1,i2,ielt,iedge,ityp
      
      character*32 isubname

      isubname='edgefun_lg'
            
      edge_error=-one  
c...  Loop over elements around edge and compute max error
      do i=1,nelts
         ielt=elts(i)
         iedge=edges(i)
         ityp=itettyp(elts(i))
         i1=itet(ielmedge1(1,iedge,ityp)+
     *                  itetoff(elts(i)))
         i2=itet(ielmedge1(2,iedge,ityp)+
     *                  itetoff(elts(i)))
                  
         ex=xic(i1)-xic(i2)
         ey=yic(i1)-yic(i2)
         ez=zic(i1)-zic(i2)
         ee=(ex*(hxx(ielt)*ex+2.*hxy(ielt)*ey+2.*hxz(ielt)*ez)+
     &      ey*(hyy(ielt)*ey+2.*hyz(ielt)*ez)+ez*hzz(ielt)*ez)/8.0d0
         if(abs(ee).gt.edge_error) edge_error=abs(ee)
 
       enddo
            

 9999 continue
      return
      end
