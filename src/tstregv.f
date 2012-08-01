      subroutine tstregv(x,y,z,npts,epsln,regname,
     &                   iregloc,ierr)
C
C#######################################################################
C
C     PURPOSE -
C
C
C     THIS ROUTINE CHECK WHETHER THE POINT (x,y,z) LIES INSIDE, ON
C     THE SURFACE OR OUTSIDE THE REGION iregck.
C
C
C     INPUT ARGUMENTS -
C
C        x - X COORDINATE ARRAY OF THE POINTS TO CHECK
C        y - Y COORDINATE ARRAY OF THE POINTS TO CHECK
C        z - Z COORDINATE ARRAY OF THE POINTS TO CHECK
C        npts - NO. OF POINTS TO CHECK
C        epsln - EPSILON FOR SURFACE CHECKS
C        regname - name of region to test
C
C     OUTPUT ARGUMENTS -
C
C        iregloc - RETURNS 1=IN, 2=ON OR 3=OUT
C        ierr - ERROR FLAG
C
C     CHANGE HISTORY -
C
C        $Log: tstregv.f,v $
C        Revision 2.00  2007/11/09 20:04:05  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.1   Wed Apr 05 13:35:34 2000   nnc
CPVCS    Minor source modifications required by the Absoft compiler.
CPVCS    
CPVCS       Rev 1.0   Tue Mar 07 10:28:12 2000   dcg
CPVCS    Initial revision.
C
C#######################################################################
C
      implicit none
C
      include 'geom_lg.h'
C
C#######################################################################
C
C arguments (x,y,z,npts,epsln,regname,iregloc,ierr)
      integer npts
      real*8 x(npts),y(npts),z(npts),epsln
      character*(*) regname
      integer iregloc(npts)
      integer ierr

C variables
      integer iout,lout,itype,ierror,length,i

      pointer(ipout,out)
      real*8 out(*),rout

      character*32 cmo,isubname,geomn,irtype
C
C#######################################################################
C BEGIN begin
C
      isubname='tstregv'
      irtype='region'
      ierr=1
      call cmo_get_name(cmo,ierror)
      call cmo_get_attinfo('geom_name',cmo,iout,rout,geomn,
     *                        ipout,lout,itype,ierror)
c
c  get region information
c
      call mmfindbk('cregs',geomn,ipcregs,length,ierror)
      call mmfindbk('offregdef',geomn,ipoffregdef,length,ierror)
      call mmfindbk('ndefregs',geomn,ipndefregs,length,ierror)
      call mmfindbk('regdef',geomn,ipregdef,length,ierror)
c
c  find matching region
c
      do i=1,nregs
        if(cregs(i).eq.regname) then
           call chkregliteral(x,y,z,npts,epsln,irtype,
     &                   regdef(offregdef(i)+1),ndefregs(i),
     &                   iregloc,ierr)
           go to 9999
        endif
      enddo
 9999 continue
      return
      end
