      subroutine calc_rdist(imsgin,xmsgin,cmsgin,msgtype,nwds,ierror)
C
C######################################################################
C
C     PURPOSE -
C
C        The purpose of this subroutine is to calculate the radial
C        distance between a specified point and a set of points.
C        The set of points can be specified using the standard
C        LaGriT techniques of psets, the whole grid, or ranges.
C
C     NOTES -
C
C        Syntax for this command:
C          calc_rdist/
C          x0, y0, z0/
C          [radius_index]/
C          [RANGE]
C
C        We define RANGE as:
C          ifirst,ilast,istride    OR
C          pset,get,<pset_name>
C
C        Because we only allow this operation on sets of nodes the
C        eltset,get,<eltset_name> syntax is not allowed, and will
C        result in an error. If RANGE is not specified, the entire
C        grid is assumed.
C
C        x0, y0, z0 define a point from which the radial distance
C        will be calculated.
C
C        The radius index (must be an integer) is used to delineate
C        from what point the distance was calculated. This is
C        optional, and if not specified, will leave the current
C        indices alone. If it is specified, it will take and modify
C        the attribute cntrpt to the integer specified. (Within the
C        range defined by RANGE.)
C
C
C     INPUT ARGUMENTS -
C
C        xmsgin()  - REAL ARRAY OF COMMAND INPUT VALUES
C        cmsgin()  - CHARACTER ARRAY OF COMMAND INPUT VALUES
C        imsgin()  - INTEGER ARRAY OF COMMAND INPUT VALUES
C        msgtype() - INTEGER ARRAY OF COMMAND INPUT TYPE
C        nwds      - NO. OF WORDS OF COMMAND INPUT VALUES
C
C
C     $Log: calc_rdist.f,v $
C     Revision 2.00  2007/11/05 19:45:47  spchu
C     Import to CVS
C
CPVCS    
CPVCS       Rev 1.4   30 Sep 2004 11:12:00   dcg
CPVCS    make epsln double precision
CPVCS
CPVCS       Rev 1.3   07 Feb 2000 17:38:06   dcg
CPVCS    remove unused comdict.h
CPVCS
CPVCS       Rev 1.2   Tue Aug 31 14:22:24 1999   dcg
CPVCS    make file implicit none
CPVCS
CPVCS       Rev 1.1   Fri Aug 06 13:41:52 1999   bap
CPVCS    This revision incorporates the syntax changes, eliminating the source and sink
CPVCS    mesh objects.
CPVCS
CPVCS       Rev 1.0   Fri Aug 06 13:21:14 1999   bap
CPVCS    Initial revision.
C
C######################################################################
C
      implicit none
C
C     include "machine.h"
      include "local_element.h"
C
      integer lenptr
      real*8 epsln
      parameter (lenptr=1000000)
      parameter (epsln=1.0d-10)
C
C
C######################################################################
C
C Variable Declarations
C
C######################################################################
c
      integer ilen,icmotype,i,j,ioff,ierror,nwds
      real*8 x0,y0,z0
C
C     Subroutine Input Variables
C
      character*(*) cmsgin(nwds)
      integer       imsgin(nwds), msgtype(nwds)
      real*8        xmsgin(nwds)
C
C     Name Variables and Message Variables
C
      character*32  isubname, cmoin, cmoout
      character*64  range
      character*132 logmess,errmess
      character*256 cmdmess
C
C     Variables used to store temporary info and indices
C
      integer radius_idx
      integer range_len
      integer icharlnf,strlen
C
C     Temporary Pointers used to validate MOs
C
      pointer(iptestptr, testptr)
      real*8 testptr(lenptr)
C
C     Initialize Error Flag and other assorted goodies
C
      ierror = 0
      cmoin = '-cmo-'
      cmoout = '-none-'
      isubname = 'calc_rdist'
      range_len = 0
C
C
C######################################################################
C
C     Check the gross syntax of the command entered
C
      if((nwds.ne.4).AND.(nwds.ne.5).AND.(nwds.ne.7).AND.
     &     (nwds.ne.8)) then
         write(logmess,'(a)')
     &        'Error in subroutine calc_rdist: The proper syntax is:'
         call writloga('default',0,logmess,0,ierror)
         write(logmess,'(a)')
     &       'calc_rdist/x0,y0,z0/[rad_idx]/[range]'
         call writloga('default',0,logmess,0,ierror)
         ierror = 1
         go to 9999
      endif
C
C
C######################################################################
C
C     Initialize the Mesh Objects and corresponding attributes
C
C     *****************************************************************
C     Check if a MO exists (kinda important)
C
      call cmo_get_name(cmoin,ierror)
      call cmo_exist(cmoin,ierror)
      if(ierror.ne.0) then
         write(logmess,'(a)')
     &       'Error in subroutine calc_rdist: input MO does not exist.'
         call writloga('default',0,logmess,0,ierror)
         write(logmess,'(a)')
     &        'Please either use cmo/select or creat a MO.'
         call writloga('default',0,logmess,0,ierror)
         ierror = 1
         go to 9999
      endif
 
C######################################################################
C
C     Now that we have all the proper variables build the command lines
C
C     *****************************************************************
C     Figure out whether or not we need to supply default values and
C     whether the types of arguments are right.
C
C     Figure out the range of the command.
C     If there is no range provided, create the default.
      if((nwds.eq.4).OR.(nwds.eq.5)) then
         range = '1,0,0'
      else
         ioff = 0
         if(nwds.eq.8) then
            ioff = 1
         endif
         do i = 5+ioff, 7+ioff
            if(msgtype(i).eq.1) then
               write(range(range_len+1:),'(I7)') imsgin(i)
                range_len=range_len+7
            elseif(msgtype(i).eq.2) then
               write(range(range_len+1:),'(E13.6)') xmsgin(i)
               range_len=range_len+13
            elseif(msgtype(i).eq.3) then
               write(*,*) cmsgin(i)
               strlen=icharlnf(cmsgin(i))
               write(range(range_len+1:),'(a)') cmsgin(i)(1:strlen)
               range_len=range_len+strlen
            endif
            if(i.ne.7+ioff) then
               write(range(range_len+1:),'(a)') ','
               range_len=range_len+1
            endif
         enddo
C        Clean up the range line by deleting all the whitespace
         j = 1
         do i = 1, range_len
            if(range(i:i) .ne.' ') then
               range(j:j) = range(i:i)
               j = j+1
            endif
         enddo
         if(j.lt.i) then
            do j = j, range_len
               range(j:j) = ' '
            enddo
         endif
      endif
C     Now we have the range properly specified... let's go on
C
C     Convert the input arguments to reals for x0, y0, and z0
C     x0:
      if(msgtype(2).eq.1) then
         x0 = REAL(imsgin(2))
      elseif(msgtype(2).eq.2) then
         x0 = xmsgin(2)
      else
         write(errmess,'(a,a)')
     &        'Error in calc_rdist.f: x0 must be a number!'
         call writloga('default',0,logmess,0,ierror)
         goto 9999
      endif
C     y0:
      if(msgtype(3).eq.1) then
         y0 = REAL(imsgin(3))
      elseif(msgtype(3).eq.2) then
         y0 = xmsgin(3)
      else
         write(errmess,'(a,a)')
     &        'Error in calc_rdist.f: y0 must be a number!'
         call writloga('default',0,logmess,0,ierror)
         goto 9999
      endif
C     z0:
      if(msgtype(4).eq.1) then
         z0 = REAL(imsgin(4))
      elseif(msgtype(4).eq.2) then
         z0 = xmsgin(4)
      else
         write(errmess,'(a,a)')
     &        'Error in calc_rdist.f: z0 must be a number!'
         call writloga('default',0,logmess,0,ierror)
         goto 9999
      endif
 
C     Cases where we have a radius index.
      if((nwds.eq.5).OR.(nwds.eq.8)) then
         if(msgtype(5).ne.1) then
C     Houston, we have a problem...
            write(errmess,'(a,a)')
     &           'Warning: the radius index must be an integer, ',
     &           'trying to figure out what happened.'
            call writloga('default',0,logmess,0,ierror)
            if(msgtype(5).eq.2) then
               write(errmess,'(a,a)')
     &           'Attempting to recover by changing a float to an int.'
               call writloga('default',0,logmess,0,ierror)
               radius_idx=xmsgin(5)
            endif
         else
            radius_idx=imsgin(5)
         endif
      endif
C
C     *****************************************************************
C     Check to see if the necessary attributes exist in the output
C     MO, if they don't, create them.
C
C     rdist, the radial distance from the test point
      call cmo_get_info('rdist',cmoin,iptestptr,ilen,icmotype,ierror)
      if(ierror.ne.0) then
         write(cmdmess,10) cmoin,
     &        'VDOUBLE/scalar/nnodes/linear/permanent',
     &        '/   /-1.0; finish'
 10      format('cmo/addatt/',A,'/rdist/',A,A)
         ierror = 0
         call dotaskx3d(cmdmess,ierror)
         if (ierror.ne.0) goto 9999
      endif
C
C     temp, the temporary variable that will be deleted at the end
C     of this mess.
      call cmo_get_info('temp',cmoin,iptestptr,ilen,icmotype,ierror)
      if(ierror.ne.0) then
         write(cmdmess,20) cmoin,
     &        'VDOUBLE/scalar/nnodes/linear/temporary',
     &        '/   /-1.0; finish'
 20      format('cmo/addatt/',A,'/temp/',A,A)
         ierror = 0
         call dotaskx3d(cmdmess,ierror)
         if (ierror.ne.0) goto 9999
      endif
C
C     ctrpt, the index of the center point we are referencing
      call cmo_get_info('ictrpt',cmoin,iptestptr,ilen,icmotype,ierror)
      if(ierror.ne.0) then
         write(cmdmess,15) cmoin,
     &        'VINT/scalar/nnodes/linear/permanent/   /0; finish'
 15      format('cmo/addatt/',A,'/ictrpt/',A)
         ierror = 0
         call dotaskx3d(cmdmess,ierror)
         if (ierror.ne.0) goto 9999
      endif
C
 
C     Now we have the proper radius index... let's create the line
C     to set the attribute.
      write(cmdmess,25) cmoin,range,radius_idx
 25   format('cmo/setatt/',A,'/ictrpt/',A,'/',I7,'; finish')
      ierror = 0
      call dotaskx3d(cmdmess,ierror)
      if(ierror.ne.0) goto 9999
C
C     *****************************************************************
C     Now we have either set or ignored the radius index and we have
C     the range properly set up, so we can get down to doing the
C     math for the radial distance calculation
C
C
C     Subtract the x-coordinate origin from xic
      write(cmdmess,30) cmoin,range,cmoin,x0
 30   format('math/sub/',A,'/temp/',A,'/',A,'/xic/',E20.12,'; finish')
      ierror = 0
      call dotaskx3d(cmdmess,ierror)
      if(ierror.ne.0) goto 9999
C
C     square the difference, store x_diff^2 in rdist
      write(cmdmess,35) cmoin,range,cmoin
 35   format('math/power/',A,'/rdist/',A,'/',A,'/temp/2.0; finish')
      ierror = 0
      call dotaskx3d(cmdmess,ierror)
      if(ierror.ne.0) goto 9999
C
C     Subtract the y-coordinate origin from yic
      write(cmdmess,40) cmoin,range,cmoin,y0
 40   format('math/sub/',A,'/temp/',A,'/',A,'/yic/',E20.12,'; finish')
      ierror = 0
      call dotaskx3d(cmdmess,ierror)
      if(ierror.ne.0) goto 9999
C
C     square the difference, store y_diff^2 in temp
      write(cmdmess,45) cmoin,range,cmoin
 45   format('math/power/',A,'/temp/',A,'/',A,'/temp/2.0; finish')
      ierror = 0
      call dotaskx3d(cmdmess,ierror)
      if(ierror.ne.0) goto 9999
C
C     add y_diff^2 to rdist
      write(cmdmess,50) cmoin,range,cmoin,cmoin,'; finish'
 50   format('math/add/',A,'/rdist/',A,'/',A,'/rdist/',A,'/temp',A)
      ierror = 0
      call dotaskx3d(cmdmess,ierror)
      if(ierror.ne.0) goto 9999
C
C     Subtract the z-coordinate origin from zic
      write(cmdmess,55) cmoin,range,cmoin,z0
 55   format('math/sub/',A,'/temp/',A,'/',A,'/zic/',E20.12,'; finish')
      ierror = 0
      call dotaskx3d(cmdmess,ierror)
      if(ierror.ne.0) goto 9999
C
C     square the difference, store z_diff^2 in temp
      write(cmdmess,60) cmoin,range,cmoin
 60   format('math/power/',A,'/temp/',A,'/',A,'/temp/2.0; finish')
      ierror = 0
      call dotaskx3d(cmdmess,ierror)
      if(ierror.ne.0) goto 9999
C
C     add z_diff^2 to rdist
      write(cmdmess,65) cmoin,range,cmoin,cmoin,'; finish'
 65   format('math/add/',A,'/rdist/',A,'/',A,'/rdist/',A,'/temp',A)
      ierror = 0
      call dotaskx3d(cmdmess,ierror)
      if(ierror.ne.0) goto 9999
C
C     Take the square root of rdist, which is now
C     x_diff^2 + y_diff^2 + z_diff^2
      write(cmdmess,70) cmoin,range,cmoin
 70   format('math/power/',A,'/rdist/',A,'/',A,'/rdist/0.5; finish')
      ierror = 0
      call dotaskx3d(cmdmess,ierror)
      if(ierror.ne.0) goto 9999
C
C######################################################################
C
C     We're done... let's finish it off. Remove temporary variables.
C
         write(cmdmess,75) cmoin
 75      format('cmo/delatt/',A,'/temp; finish')
         ierror = 0
         call dotaskx3d(cmdmess,ierror)
         if (ierror.ne.0) goto 9999
C
C######################################################################
C
C     Clean house.
 
 9999    continue
         return
         end
