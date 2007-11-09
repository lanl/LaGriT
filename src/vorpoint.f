*dk,vorpoint
      subroutine vorpoint(n1,n2,n3,n4,xv,yv,zv,distsq)
       implicit real*8 (a-h,o-z)
C
C #####################################################################
C
C     PURPOSE -
C
C        This routine calculates the 3-D voronoi point
C
C     INPUT ARGUMENTS -
C
C        n1-n4  - the points in the tetrahedron
C
C
C     OUTPUT ARGUMENTS -
C
C        xv     - the x-coordinate of the Voronoi point
C        yv     - the y-coordinate of the Voronoi point
C        zv     - the z-coordinate of the Voronoi point
C        distsq - the Voronoi radius squared
C
C     CHANGE HISTORY -
C
C        $Log:   /pvcs.config/t3d/src/vorpoint.f_a  $
CPVCS    
CPVCS       Rev 1.4   Mon Apr 14 17:05:54 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.3   12/02/94 15:07:22   het
CPVCS    Added an option for the "cmo" access functions
CPVCS    
CPVCS    
CPVCS       Rev 1.2   12/01/94 18:50:00   het
CPVCS    Added a data type to the "cmo" calles 
CPVCS       and added the "cmo.h" include file.
CPVCS    
CPVCS       Rev 1.1   11/17/94 21:56:00   het
CPVCS    Added include files for chydro, neibor, cmerge, comdict. Added calles and
CPVCS    pointer statements for current_mesh_object database access.
CPVCS
CPVCS       Rev 1.0   11/10/94 12:20:20   pvcs
CPVCS    Original version.
C
C ######################################################################
C
      include "cmo.h"
      include "chydro.h"
      include "neibor.h"
C
C ######################################################################
C
      crosx(a,b,c,d,e,f)=b*f-c*e
      crosy(a,b,c,d,e,f)=c*d-a*f
      crosz(a,b,c,d,e,f)=a*e-b*d
C
C ######################################################################
C
C
C
C     ******************************************************************
C     FETCH MESH OBJECT NAME AND POINTER INFORMATION.
C
      if(icmoget.eq.1) then
C
      call cmo_get_name(cmo,ierror)
C
      call cmo_get_info('xic',cmo,ipxic,lenxic,icmotype,ierror)
      call cmo_get_info('yic',cmo,ipyic,lenyic,icmotype,ierror)
      call cmo_get_info('zic',cmo,ipzic,lenzic,icmotype,ierror)
C
      endif
C
C     ******************************************************************
C
C
C
      xxsmall=1.0e-30
      xa=xic(n1)
      ya=yic(n1)
      za=zic(n1)
      xb=xic(n2)-xa
      yb=yic(n2)-ya
      zb=zic(n2)-za
      xc=xic(n3)-xa
      yc=yic(n3)-ya
      zc=zic(n3)-za
      xd=xic(n4)-xa
      yd=yic(n4)-ya
      zd=zic(n4)-za
      xn=crosx(xb,yb,zb,xc,yc,zc)
      yn=crosy(xb,yb,zb,xc,yc,zc)
      zn=crosz(xb,yb,zb,xc,yc,zc)
      x2=crosx(xn,yn,zn,xb,yb,zb)
      y2=crosy(xn,yn,zn,xb,yb,zb)
      z2=crosz(xn,yn,zn,xb,yb,zb)
      q=-0.5*(xc*xb+yc*yb+zc*zb-xc*xc-yc*yc-zc*zc)/
     *       (x2*xc+y2*yc+z2*zc+xxsmall)
      xl=q*x2+0.5*xb
      yl=q*y2+0.5*yb
      zl=q*z2+0.5*zb
      d=-0.5*(xd*xd+yd*yd+zd*zd)
      q=-(xd*xl+yd*yl+zd*zl+d)/(xd*xn+yd*yn+zd*zn+xxsmall)
      xv=q*xn+xl+xa
      yv=q*yn+yl+ya
      zv=q*zn+zl+za
      distsq=(xv-xa)**2+(yv-ya)**2+(zv-za)**2
C
      goto 9999
 9999 continue
      return
      end
