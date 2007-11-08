*DK conset
      subroutine conset(iopt,cgeom,isym,jsym,ksym,nx,ny,nz,
     *                       ipitp1,ipicr1)
C
C#######################################################################
C
C      PURPOSE -
C
C        THIS ROUTINE SETS THE POINT TYPES AND THE CONSTRAINT INDICES
C           FOR A LOGICAL BOX OF DIMENSIONS [(1,NX),(1,NY),(1,NZ)].
C           CORNERS ARE DONE FIRST, THEN EDGES ARE DONE AND THEN THE
C           FACES ARE FLAGGED.
C
C
C
C      INPUT ARGUMENTS -
C
C        igeom    - INDICATES THE GEOMETRY IN WHICH THE LOGICAL BOX IS
C                      BEING DEFINED.
C        nx,ny,nz - LOGICAL DIMENSIONS OF THE BOX.
C        ipitp1   - POINTER TO THE POINT TYPE ARRAY.
C        ipicr1   - POINTER TO THE POINT CONSTRAINT_ARRAY.
C
C
C      OUTPUT ARGUMENTS -
C
C
C
C      CHANGE HISTORY -
C
C         HT1023AA-87, HT1109AA-87, HT1112AA-87
C
C
C#######################################################################
C
      implicit real*8 (a-h, o-z)
C
      include "chydro.h"
C
      character*132 logmess
C
      character*32 cgeom
C
C#######################################################################
C
      pointer (ipitp1, itp1)
      pointer (ipicr1, icr1)
      integer itp1(1000000), icr1(1000000)
C
C#######################################################################
C
C     DEFINE THE STATEMENT FUNCTIONS FOR THIS ROUTINE.
C
      icc(i,j,k)=i+(j-1+(k-1)*(ny-1))*(nx-1)
      ivc(i,j,k)=i+(j-1+(k-1)*ny)*nx
      ifc(i,j,k,l)=i+(j-1)*3+(k-1)*(nx+1)*3+(l-1)*(ny+1)*(nx+1)*3
C
C#######################################################################
C
C
C
      if(cgeom(1:3).eq."xyz") then
         icorn1=1
         icorn2=2
         icorn3=3
         icorn4=4
         icorn5=5
         icorn6=6
         icorn7=7
         icorn8=8
         iedge1=9
         iedge2=10
         iedge3=11
         iedge4=12
         iedge5=13
         iedge6=14
         iedge7=15
         iedge8=16
         iedge9=17
         iedge10=18
         iedge11=19
         iedge12=20
         iface1=21
         iface2=22
         iface3=23
         iface4=24
         iface5=25
         iface6=26
         icr1(ivc( 1, 1, 1))=icorn1
         icr1(ivc(nx, 1, 1))=icorn2
         icr1(ivc(nx,ny, 1))=icorn3
         icr1(ivc( 1,ny, 1))=icorn4
         icr1(ivc( 1, 1,nz))=icorn5
         icr1(ivc(nx, 1,nz))=icorn6
         icr1(ivc(nx,ny,nz))=icorn7
         icr1(ivc( 1,ny,nz))=icorn8
         itp1(ivc( 1, 1, 1))=ifitprfl
         itp1(ivc(nx, 1, 1))=ifitprfl
         itp1(ivc(nx,ny, 1))=ifitprfl
         itp1(ivc( 1,ny, 1))=ifitprfl
         itp1(ivc( 1, 1,nz))=ifitprfl
         itp1(ivc(nx, 1,nz))=ifitprfl
         itp1(ivc(nx,ny,nz))=ifitprfl
         itp1(ivc( 1,ny,nz))=ifitprfl
         do 100 ix=2,nx-1
            itp1(ivc(ix, 1, 1))=ifitprfl
            itp1(ivc(ix,ny, 1))=ifitprfl
            itp1(ivc(ix, 1,nz))=ifitprfl
            itp1(ivc(ix,ny,nz))=ifitprfl
            icr1(ivc(ix, 1, 1))=iedge1
            icr1(ivc(ix,ny, 1))=iedge3
            icr1(ivc(ix, 1,nz))=iedge5
            icr1(ivc(ix,ny,nz))=iedge7
 100     continue
         do 110 iy=2,ny-1
            itp1(ivc( 1,iy, 1))=ifitprfl
            itp1(ivc( 1,iy,nz))=ifitprfl
            itp1(ivc(nx,iy, 1))=ifitprfl
            itp1(ivc(nx,iy,nz))=ifitprfl
            icr1(ivc( 1,iy, 1))=iedge4
            icr1(ivc( 1,iy,nz))=iedge8
            icr1(ivc(nx,iy, 1))=iedge2
            icr1(ivc(nx,iy,nz))=iedge6
 110     continue
         do 120 iz=2,nz-1
            itp1(ivc( 1, 1,iz))=ifitprfl
            itp1(ivc(nx, 1,iz))=ifitprfl
            itp1(ivc(nx,ny,iz))=ifitprfl
            itp1(ivc( 1,ny,iz))=ifitprfl
            icr1(ivc( 1, 1,iz))=iedge9
            icr1(ivc(nx, 1,iz))=iedge10
            icr1(ivc(nx,ny,iz))=iedge11
            icr1(ivc( 1,ny,iz))=iedge12
 120     continue
         do 200 iy=2,ny-1
            do 210 iz=2,nz-1
               itp1(ivc(nx,iy,iz))=ifitprfl
               itp1(ivc( 1,iy,iz))=ifitprfl
               icr1(ivc(nx,iy,iz))=iface1
               icr1(ivc( 1,iy,iz))=iface4
 210        continue
 200     continue
         do 220 ix=2,nx-1
            do 230 iz=2,nz-1
               itp1(ivc(ix,ny,iz))=ifitprfl
               itp1(ivc(ix, 1,iz))=ifitprfl
               icr1(ivc(ix,ny,iz))=iface2
               icr1(ivc(ix, 1,iz))=iface5
 230        continue
 220     continue
         do 240 ix=2,nx-1
            do 250 iy=2,ny-1
               itp1(ivc(ix,iy,nz))=ifitprfl
               itp1(ivc(ix,iy, 1))=ifitprfl
               icr1(ivc(ix,iy,nz))=iface3
               icr1(ivc(ix,iy, 1))=iface6
 250        continue
 240     continue
         if(iopt.eq.2) then
            nptsv=nx*ny*nz
            nptsc=(nx-1)*(ny-1)*(nz-1)
            nptsf=(nx+1)*(ny+1)*(nz+1)
            nptsvc =nptsv+nptsc
            nptsvcf=nptsv+nptsc+nptsf
            do 300 iy=1,ny-1
               do 310 iz=1,nz-1
                  itp1(ifc(1, 1,iy,iz)+nptsvc)=ifitprfl
                  itp1(ifc(1,nx,iy,iz)+nptsvc)=ifitprfl
                  icr1(ifc(1, 1,iy,iz)+nptsvc)=iface4
                  icr1(ifc(1,nx,iy,iz)+nptsvc)=iface1
 310           continue
 300        continue
            do 320 ix=1,nx-1
               do 330 iz=1,nz-1
                  itp1(ifc(2,ix, 1,iz)+nptsvc)=ifitprfl
                  itp1(ifc(2,ix,ny,iz)+nptsvc)=ifitprfl
                  icr1(ifc(2,ix, 1,iz)+nptsvc)=iface5
                  icr1(ifc(2,ix,ny,iz)+nptsvc)=iface2
 330           continue
 320        continue
            do 340 ix=1,nx-1
               do 350 iy=1,ny-1
                  itp1(ifc(3,ix,iy, 1)+nptsvc)=ifitprfl
                  itp1(ifc(3,ix,iy,nz)+nptsvc)=ifitprfl
                  icr1(ifc(3,ix,iy, 1)+nptsvc)=iface6
                  icr1(ifc(3,ix,iy,nz)+nptsvc)=iface3
 350           continue
 340        continue
         endif
      elseif(cgeom.eq."rtz") then
         iface3=3
         iface4=4
         iface6=6
         do 420 iy=1,ny
            do 430 iz=1,nz
               if(isym.ne.2) then
                  itp1(ivc( 1,iy,iz))=ifitprfl
                  icr1(ivc( 1,iy,iz))=iface4
               endif
               itp1(ivc(nx,iy,iz))=ifitprfl
               icr1(ivc(nx,iy,iz))=0
 430        continue
 420     continue
         do 400 ix=1,nx
            do 410 iy=1,ny
               itp1(ivc(ix,iy, 1))=ifitprfl
               icr1(ivc(ix,iy, 1))=iface6
               itp1(ivc(ix,iy,nz))=ifitprfl
               icr1(ivc(ix,iy,nz))=iface3
 410        continue
 400     continue
         if(iopt.eq.2) then
            nptsv=nx*ny*nz
            nptsc=(nx-1)*(ny-1)*(nz-1)
            nptsf=(nx+1)*(ny+1)*(nz+1)
            nptsvc =nptsv+nptsc
            nptsvcf=nptsv+nptsc+nptsf
            do 460 iy=1,ny-1
               do 470 iz=1,nz-1
                  if(isym.eq.2) then
                     itp1(ifc(1, 1,iy,iz)+nptsvc)=ifitpdud
                     icr1(ifc(1, 1,iy,iz)+nptsvc)=0
                  else
                     itp1(ifc(1, 1,iy,iz)+nptsvc)=ifitprfl
                     icr1(ifc(1, 1,iy,iz)+nptsvc)=iface4
                  endif
                  itp1(ifc(1,nx,iy,iz)+nptsvc)=ifitprfl
                  icr1(ifc(1,nx,iy,iz)+nptsvc)=0
 470           continue
 460        continue
            do 440 ix=1,nx-1
               do 450 iy=1,ny-1
                  itp1(ifc(3,ix,iy, 1)+nptsvc)=ifitprfl
                  itp1(ifc(3,ix,iy,nz)+nptsvc)=ifitprfl
                  icr1(ifc(3,ix,iy, 1)+nptsvc)=iface3
                  icr1(ifc(3,ix,iy,nz)+nptsvc)=iface6
 450           continue
 440        continue
         endif
      endif
      goto 9999
 9999 continue
      return
      end
