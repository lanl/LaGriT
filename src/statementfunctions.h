C statementfunctions.h
C
C #####################################################################
C
C     PURPOSE -
C
C        This include file contains useful statement functions.  This
C        file should be included just ahead of any other 
C        statement functions in the subroutine.
C
C     CHANGE HISTORY -
C
C        $Log: statementfunctions.h,v $
C        Revision 2.00  2007/11/05 19:46:03  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.4   14 May 2001 09:00:28   kuprat
CPVCS    Put in DSZAREA function which returns signed area of triangle.
CPVCS    The sign is determined by the z-component of the area vector.
CPVCS    Also put in DSZIRTRI which uses DSZAREA to give a signed
CPVCS    inscribed area for a triangle.
CPVCS    
CPVCS       Rev 1.3   01 Mar 2000 14:56:04   kuprat
CPVCS    Used 'safe' statement function in inscribed radii denominators.
CPVCS    
CPVCS       Rev 1.2   Tue Oct 26 14:08:26 1999   dcg
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.1   Fri Jan 08 16:58:10 1999   kuprat
CPVCS    New SAFE statement function.
CPVCS    
CPVCS       Rev 1.0   Fri Oct 23 16:21:28 1998   kuprat
CPVCS    Initial revision.

c.... Statement function and dummy variable declarations.

      real*8 dcrosx,dcrosy,dcrosz,dlen,darea,dszarea,dvol,dperimtri,
     &   dareatet,dirtri,dszirtri,dirtet,safe
      real*8 x_a1,y_a1,z_a1,x_a2,y_a2,z_a2,x_a3,y_a3,z_a3,x_a4,y_a4,z_a4
     &   ,xsafe

c.... statement function which adjusts XSAFE to have a minimum absolute
c.... value so that it is suitable for division.  Sign of XSAFE is 
c.... preserved. In the case of XSAFE=0, sign is taken to be positive.

      safe(xsafe)=max(abs(xsafe),1.d-50)*sign(1.d0,xsafe)

c.... statement functions for the components of the cross product
c.... (vec2 - vec1) x (vec3 - vec1) .

      dcrosx(x_a1,y_a1,z_a1,x_a2,y_a2,z_a2,x_a3,y_a3,z_a3)=(y_a2-y_a1)
     &   *(z_a3-z_a1)-(z_a2-z_a1)*(y_a3-y_a1)
      dcrosy(x_a1,y_a1,z_a1,x_a2,y_a2,z_a2,x_a3,y_a3,z_a3)=(z_a2-z_a1)
     &   *(x_a3-x_a1)-(x_a2-x_a1)*(z_a3-z_a1)
      dcrosz(x_a1,y_a1,z_a1,x_a2,y_a2,z_a2,x_a3,y_a3,z_a3)=(x_a2-x_a1)
     &   *(y_a3-y_a1)-(y_a2-y_a1)*(x_a3-x_a1)
   
c.... statement function for the length of a segment.

      dlen(x_a1,y_a1,z_a1,x_a2,y_a2,z_a2)=
     &   sqrt((x_a1-x_a2)**2+(y_a1-y_a2)**2+(z_a1-z_a2)**2)

c.... statement function for the area of a triangle.

      darea(x_a1,y_a1,z_a1,x_a2,y_a2,z_a2,x_a3,y_a3,z_a3)=0.5d0*sqrt(
     &   dcrosx(x_a1,y_a1,z_a1,x_a2,y_a2,z_a2,x_a3,y_a3,z_a3)**2+
     &   dcrosy(x_a1,y_a1,z_a1,x_a2,y_a2,z_a2,x_a3,y_a3,z_a3)**2+
     &   dcrosz(x_a1,y_a1,z_a1,x_a2,y_a2,z_a2,x_a3,y_a3,z_a3)**2  )

c.... Statement function for the signed area of a triangle.  The
c.... sign is determined by the z-component of the area vector.

      dszarea(x_a1,y_a1,z_a1,x_a2,y_a2,z_a2,x_a3,y_a3,z_a3)=0.5d0*sqrt(
     &   dcrosx(x_a1,y_a1,z_a1,x_a2,y_a2,z_a2,x_a3,y_a3,z_a3)**2+
     &   dcrosy(x_a1,y_a1,z_a1,x_a2,y_a2,z_a2,x_a3,y_a3,z_a3)**2+
     &   dcrosz(x_a1,y_a1,z_a1,x_a2,y_a2,z_a2,x_a3,y_a3,z_a3)**2  ) *
     &   sign(1.d0,dcrosz(x_a1,y_a1,z_a1,x_a2,y_a2,z_a2,x_a3,y_a3,z_a3))

c.... statement function for the volume of a tet.

      dvol(x_a1,y_a1,z_a1,x_a2,y_a2,z_a2,x_a3,y_a3,z_a3,x_a4,y_a4,z_a4)
     &   =one6th*((x_a4-x_a1)*dcrosx(x_a1,y_a1,z_a1,x_a2,y_a2,z_a2,x_a3
     &   ,y_a3,z_a3)+(y_a4-y_a1)*dcrosy(x_a1,y_a1,z_a1,x_a2,y_a2,z_a2
     &   ,x_a3,y_a3,z_a3)+(z_a4-z_a1)*dcrosz(x_a1,y_a1,z_a1,x_a2,y_a2
     &   ,z_a2,x_a3,y_a3,z_a3)  )

c.... statement function for the perimeter of a triangle.

      dperimtri(x_a1,y_a1,z_a1,x_a2,y_a2,z_a2,x_a3,y_a3,z_a3)=
     &   dlen(x_a1,y_a1,z_a1,x_a2,y_a2,z_a2)+
     &   dlen(x_a2,y_a2,z_a2,x_a3,y_a3,z_a3)+
     &   dlen(x_a3,y_a3,z_a3,x_a1,y_a1,z_a1)

c.... statement function for the surface area of a tet.

      dareatet(x_a1,y_a1,z_a1,x_a2,y_a2,z_a2,x_a3,y_a3,z_a3,x_a4,y_a4
     &   ,z_a4)=darea(x_a1,y_a1,z_a1,x_a2,y_a2,z_a2,x_a3,y_a3,z_a3)
     &   +darea(x_a2,y_a2,z_a2,x_a3,y_a3,z_a3,x_a4,y_a4,z_a4)
     &   +darea(x_a3,y_a3,z_a3,x_a4,y_a4,z_a4,x_a1,y_a1,z_a1)
     &   +darea(x_a4,y_a4,z_a4,x_a1,y_a1,z_a1,x_a2,y_a2,z_a2)

c.... statement function for the inscribed radius of a triangle.

      dirtri(x_a1,y_a1,z_a1,x_a2,y_a2,z_a2,x_a3,y_a3,z_a3)=
     &   2.d0*darea(x_a1,y_a1,z_a1,x_a2,y_a2,z_a2,x_a3,y_a3,z_a3)/
     &   safe(dperimtri(x_a1,y_a1,z_a1,x_a2,y_a2,z_a2,x_a3,y_a3,z_a3))

c.... Statement function for the signed inscribed radius of a triangle.
c.... The sign is determined by the signed area, which is determined
c.... by the z-component of the area vector of the triangle.

      dszirtri(x_a1,y_a1,z_a1,x_a2,y_a2,z_a2,x_a3,y_a3,z_a3)=
     &   2.d0*dszarea(x_a1,y_a1,z_a1,x_a2,y_a2,z_a2,x_a3,y_a3,z_a3)/
     &   safe(dperimtri(x_a1,y_a1,z_a1,x_a2,y_a2,z_a2,x_a3,y_a3,z_a3))

c.... statement function for the inscribed radius of a tet.

      dirtet(x_a1,y_a1,z_a1,x_a2,y_a2,z_a2,x_a3,y_a3,z_a3,x_a4,y_a4,z_a4
     &   )=3.d0*dvol(x_a1,y_a1,z_a1,x_a2,y_a2,z_a2,x_a3,y_a3,z_a3,x_a4
     &   ,y_a4,z_a4)/safe(dareatet(x_a1,y_a1,z_a1,x_a2,y_a2,z_a2,x_a3
     &   ,y_a3,z_a3,x_a4,y_a4,z_a4))


