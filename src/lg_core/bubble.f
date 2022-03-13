
      subroutine bubble(imsgin,xmsgin,cmsgin,msgtype,nwds,ierror)
C
C########################################################################
C
C     PURPOSE -
C
C        This subroutine extrudes a pseudo-2D polygon made up of quads,
C        triangles, or hybrid polygons (normals of the curve pointing
C        in more or less the same direction) into three dimensions along
C        either the normal to the curve (default) or a user entered
C        value. It then takes the volume and makes it a closed surface.
C        (i.e., instead of being made up of volume elements, it is now a
C        shell)
C
C     NOTES -
C
C        Currently only for xyz coordinate system.
C        Syntax for this command:
C          bubble/sink_mesh_object/source_mesh_object/
C          const|min/
C          real|integer/
C          [norm|x1,y1,z1]
C
C        if argument 4 is const, argument 5 is considered to be a constant
C        offset from the surface; min, it is the minimum distance
C        from the surface to a reference plane. The extruding vector is normal
C        to the reference plane.
C
C        argument 6 states whether or not the extruding vector will be the
C        average normal to the surface, or a user specified vector. This
C        argument is optional, norm is the default. If the user specifies a
C        vector, the vector will be normalized (i.e., only the directionality
C        will be used)
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
C     CHANGE HISTORY -
C$Log: bubble.f,v $
CRevision 2.00  2007/11/05 19:45:47  spchu
CImport to CVS
C
CPVCS    
CPVCS       Rev 1.2   07 Feb 2000 17:37:14   dcg
CPVCS    remove unused comdict.h
CPVCS
CPVCS       Rev 1.1   Thu Aug 13 15:08:50 1998   dcg
CPVCS    IBM changes
CPVCS
CPVCS       Rev 1.0   Fri Aug 07 13:27:30 1998   dcg
CPVCS    Initial revision.
C
C
C########################################################################
C
      implicit none

C tam commented unused machine.h which now needs preprocessor
C     include "machine.h"

      include "chydro.h"
      include "local_element.h"
C
C########################################################################
C
C Variable Declarations
C
C########################################################################
C
C     Subroutine Input Variables
C
      character*(*) cmsgin(nwds)
      real*8        xmsgin(nwds)
      integer       imsgin(nwds), msgtype(nwds)
      integer nwds, ierror
C
      integer httopt
      integer i,j,nen,ilen,icmotype,idelete
C
C     Subroutine Input Variables save pts.
C
      real*8 xvect
      real*8 yvect
      real*8 zvect
      real*8 offset

C     Name Variables and Message Variables

      character*32  cmoin, cmoout
      character*16  cmotmp1, cmotmp2
      character*132 logmess
      character*256 cmdmess
C
      character*6 type
      character*5  vtype
C
C
C
C########################################################################
C
C     Initialize Error Flag and other assorted goodies
C
      ierror = 0
      cmoin = '-cmo-'
      cmoout = '-none-'
      cmotmp1 = 'cmotmp1'
      cmotmp2 = 'cmotmp2'
C
C#######################################################################
C
C     Check the gross syntax of the command entered & get type info
C
      if((nwds.ne.5).AND.(nwds.ne.6).AND.(nwds.ne.8)) then
         write(logmess,'(a)')
     &        'Error in subroutine bubble: The proper Syntax is:'
         call writloga('default',0,logmess,0,ierror)
         write(logmess,'(a)')
     &       'bubble/cmoout/cmoin/min|const/offset/[norm|x1,y1,z1]'
         call writloga('default',0,logmess,0,ierror)
         ierror = 1
         go to 9999
      endif
C
C     Get the Type of extrusion
      type=cmsgin(4)
      if((type.ne.'min').AND.(type.ne.'const')) then
         write(logmess,'(a)')
     &        'Warning in bubble: Extrusion type is neither min nor'
         call writloga('default',0,logmess,0,ierror)
         write(logmess,'(a)')
     &        'const. Assuming const!'
         call writloga('default',0,logmess,0,ierror)
         type='const'
      endif
C
C     Get the offset
      if(msgtype(5).eq.1) then
         offset=imsgin(5)
      elseif(msgtype(5).eq.2) then
         offset=xmsgin(5)
      else
         write(logmess,'(a)')
     &        'Error in subroutine bubble: offset is not a number!'
         call writloga('default',0,logmess,0,ierror)
         ierror = 1
         go to 9999
      endif
C
C     Based on the number of arguments, do the needful...
      if((nwds.ne.5).AND.(cmsgin(6).ne.'norm').AND.(nwds.ne.8)) then
         write(logmess,'(a)')
     &        'Error in subroutine bubble: invalid extruding vector!'
         call writloga('default',0,logmess,0,ierror)
         ierror = 1
         go to 9999
      elseif(nwds.ne.8) then
         vtype='norm'
      else
         vtype='user'
C     GET THE X PART OF THE EXTRUDING VECTOR
         if(msgtype(6).eq.1) then
            xvect=imsgin(6)
         elseif(msgtype(6).eq.2) then
            xvect=xmsgin(6)
         else
            write(logmess,'(a)')
     &          'Error in subroutine bubble: x vector is not a number!'
            call writloga('default',0,logmess,0,ierror)
            ierror = 1
            go to 9999
         endif
C     GET THE Y PART OF THE EXTRUDING VECTOR
         if(msgtype(7).eq.1) then
            yvect=imsgin(7)
         elseif(msgtype(7).eq.2) then
            yvect=xmsgin(7)
         else
            write(logmess,'(a)')
     &          'Error in subroutine bubble: y vector is not a number!'
            call writloga('default',0,logmess,0,ierror)
            ierror = 1
            go to 9999
         endif
C     GET THE Z PART OF THE EXTRUDING VECTOR
         if(msgtype(8).eq.1) then
            zvect=imsgin(8)
         elseif(msgtype(8).eq.2) then
            zvect=xmsgin(8)
         else
            write(logmess,'(a)')
     &          'Error in subroutine bubble: z vector is not a number!'
            call writloga('default',0,logmess,0,ierror)
            ierror = 1
            go to 9999
         endif
      endif
C
C
C#######################################################################
C
C     All the pre-processing is done...Initialize the Mesh objects
C
C     ******************************************************************
C     Check if the input MO exists
C
      cmoin=cmsgin(3)
      call cmo_exist(cmoin,ierror)
      if(ierror.ne.0) then
         write(logmess,'(a)')
     &       'Error in subroutine extract_line: input MO does not exist'
         call writloga('default',0,logmess,0,ierror)
         ierror = 1
         go to 9999
      endif
C
C     ******************************************************************
C     Get incoming MO information & Check if it's valid
C
      call cmo_get_info('nodes_per_element',cmoin,
     &     nen,ilen,icmotype,ierror)
      if((nen.ne.3).AND.(nen.ne.4).AND.(nen.lt.9)) then
         write(logmess,'(a)')
     &        'Error in subroutine bubble: Input MO can only be made up'
         call writloga('default',0,logmess,0,ierror)
         write(logmess,'(a)')
     &        ' of triangles, quads, or hybrid elements!'
         call writloga('default',0,logmess,0,ierror)
         ierror = 1
         go to 9999
      endif
C
C     Figure out the proper hextotet conversion option.
      if(nen.eq.3) then
         httopt=3
      elseif(nen.eq.4) then
         httopt=5
      elseif(nen.ge.9) then
         write(logmess,'(a)')
     &       'Warning: hextotet may get confused, output may be garbled'
         call writloga('default',0,logmess,0,ierror)
         httopt=5
      endif
C
C     ******************************************************************
C     Save the output MO name. Check if it exists, if it does, clear it
C     so we can create it.
C
      cmoout=cmsgin(2)
      call cmo_exist(cmoout,ierror)
      if(ierror.eq.0) call cmo_release(cmoout,idelete)
C
C     ******************************************************************
C     Ensure Temporary MO names are not in use... if they are, modify
C     them. Now... if someone decides to use cmotmp2%%%%%%%,
C     We are not responsible.
      do i=1,9
         call cmo_exist(cmotmp1,ierror)
         if(ierror.eq.0) then
            do j=7,16
               if(cmotmp1(j:j).eq.' ') then
                  cmotmp1(j:j)='%'
                  goto 5
               endif
            enddo
 5          continue
         else
            goto 10
         endif
      enddo
 10   continue
      do i=1,9
         call cmo_exist(cmotmp2,ierror)
         if(ierror.eq.0) then
            do j=7,16
               if(cmotmp2(j:j).eq.' ') then
                  cmotmp2(j:j)='%'
                  goto 15
               endif
            enddo
 15         continue
         else
            goto 20
         endif
      enddo
 20   continue
C
C#######################################################################
C
C     The first command: extrude
C
C     ******************************************************************
C     Build the command line
C
      if(vtype.eq.'norm') then
         write(cmdmess,25) cmotmp1,cmoin,type,offset
 25      format('extrude/',A,'/',A,'/',A,'/',F13.6,'/s/norm; finish')
         call dotaskx3d(cmdmess,ierror)
         if(ierror.ne.0) then
            goto 9999
         endif
      else
         write(cmdmess,30) cmotmp1,cmoin,type,offset,xvect,yvect,zvect
 30      format('extrude/',A,'/',A,'/',A,'/',F13.6,'/s/',
     &        F13.6,'/',F13.6,'/',F13.6,'; finish')
         call dotaskx3d(cmdmess,ierror)
         if(ierror.ne.0) then
            goto 9999
         endif
      endif
C
C#######################################################################
C
C     HextoTet and Extract commands create the bubble
C
      write(cmdmess,35) httopt,cmotmp2,cmotmp1
 35   format('hextotet/',I1,'/',A,'/',A,'; finish')
      call dotaskx3d(cmdmess,ierror)
      if(ierror.ne.0) then
         goto 9999
      endif
C
      write(cmdmess,40) cmoout,cmotmp2
 40   format('extract/intrface/-all-/1 0 0/',A,'/',A,'; finish')
      call dotaskx3d(cmdmess,ierror)
      if(ierror.ne.0) then
         goto 9999
      endif
C
C#######################################################################
C
C     Release Temporary Memory
C
 9999 continue
 
      call cmo_exist(cmotmp1,ierror)
      if(ierror.eq.0) call cmo_release(cmotmp1,idelete)
      call cmo_exist(cmotmp2,ierror)
      if(ierror.eq.0) call cmo_release(cmotmp2,idelete)
      return
      end
 
 
 
 
 
 
 
 
