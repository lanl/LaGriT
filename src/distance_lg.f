      function distance_lg(x1,y1,z1,x2,y2,z2,ierror)
c
c calculate the distance between two points
c
c  Inputs
c
c   x1,y1,z1 coordinates of first point
c   x1,y2,z2 coordinates of second point
c  
c  Outputs
c
c    distance_lg  - distance from point 1 to point 2
c    ierror  -- error flag - 0 means no error
c
c  Change log
c
c  $Log: distance_lg.f,v $
c  Revision 2.00  2007/11/05 19:45:52  spchu
c  Import to CVS
c
CPVCS    
CPVCS       Rev 1.0   Wed Mar 01 11:15:20 2000   dcg
CPVCS    Initial revision.
C
      implicit  none
      real*8 x1,y1,z1,x2,y2,z2,distance_lg
      integer ierror
      ierror=0
      distance_lg=sqrt((x1-x2)**2+(y1-y2)**2+(z1-z2)**2)
      return
      end
c
      function sqdistance_lg(x1,y1,z1,x2,y2,z2,ierror)
c
c calculate the square of the distance between two points
c
      implicit  none
      real*8 x1,y1,z1,x2,y2,z2,sqdistance_lg
      integer ierror
      ierror=0
      sqdistance_lg=(x1-x2)**2+(y1-y2)**2+(z1-z2)**2
      return
      end
c
      function dotproduct_lg(x1,y1,z1,x2,y2,z2,x3,y3,z3,ierror)
c
c calculate the dot product of the two vectors 12 and 13
c
      implicit  none
      real*8 x1,y1,z1,x2,y2,z2,x3,y3,z3,dotproduct_lg,
     *   xa,ya,za,xb,yb,zb
      integer ierror
      ierror=0
      xa=x2-x1
      ya=y2-y1
      za=z2-z1
      xb=x3-x1
      yb=y3-y1
      zb=z3-z1
      dotproduct_lg=xa*xb+ya*yb+za*zb
      return
      end
c
      function cos_lg(x1,y1,z1,x2,y2,z2,x3,y3,z3,eps,ierror)
c
c calculate the cosine between the two vectors 12 and 13
c
      implicit  none
      real*8 x1,y1,z1,x2,y2,z2,x3,y3,z3,dot,cos_lg,
     *   xa,ya,za,xb,yb,zb,eps,d1,d2,d
      integer ierror
      ierror=0
      xa=x2-x1
      ya=y2-y1
      za=z2-z1
      xb=x3-x1
      yb=y3-y1
      zb=z3-z1
      dot=xa*xb+ya*yb+za*zb
      d1=sqrt((x1-x2)**2+(y1-y2)**2+(z1-z2)**2)
      d2=sqrt((x1-x3)**2+(y1-y3)**2+(z1-z3)**2)
      d=d1*d2
      if(d.gt.eps) then
        cos_lg=dot/d
        ierror=0
      else
        ierror=1
      endif
      return
      end



