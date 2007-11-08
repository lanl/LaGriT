*DK extract_line
      subroutine extract_line(imsgin,xmsgin,cmsgin,msgtype,nwds,ierror)
C
C########################################################################
C
C     PURPOSE -
C
C        Extract a line of intersection given two points that define it
C        from a volume
C
C     NOTES -
C
C        Currently only for xyz coordinate system.
C        Syntax for this command:
C          extrude/sink_mesh_object/source_mesh_object/
C          x1,y1,z1/x2,y2,z2/
C          To be worked on: [inf|lim]
C
C        Argument 6 specifies whether the line will be infinite (will
C        reach to the bounding box of the volume) or will be limited
C        by the two points.
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
C$Log:   /pvcs.config/t3d/src/extract_line.f_a  $
CPVCS    
CPVCS       Rev 1.3   07 Feb 2000 17:42:40   dcg
CPVCS    remove comdict.h
CPVCS
CPVCS       Rev 1.2   Tue Sep 22 14:07:40 1998   dcg
CPVCS    supply missing pointer statement
CPVCS
CPVCS       Rev 1.1   Thu Aug 13 15:08:42 1998   dcg
CPVCS    IBM changes
CPVCS
CPVCS       Rev 1.0   Fri Aug 07 13:27:36 1998   dcg
CPVCS    Initial revision.
C
C
C########################################################################
C
      implicit real*8 (a-h,o-z)
C
      include "machine.h"
      include "chydro.h"
      include "local_element.h"
C
      parameter (lenptr=1000000)
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
      integer       imsgin(nwds), msgtype(nwds)
      real*8        xmsgin(nwds)
C
C     Name Variables and Message Variables
C
      character*32  cmoin, cmoout
      character*16  cmotmp1, cmotmp2
      character*132 logmess
      character*256 cmdmess
C
C     Subroutine Input Variables save pts.
C
      real*8 x_pt(4)
      real*8 y_pt(4)
      real*8 z_pt(4)
C
      character*32  type
C
C     Variables used to store temporary data for distance calculations
C
      integer p1close, p2close, itptmp
      real*8  dist2p1_ref, dist2p1_curr
      real*8  dist2p2_ref, dist2p2_curr
C
C     Pointers for outgoing CMO
C
C     Node Based Attributes
      pointer (ipitp1o, itp1o)
      pointer (ipisn1o, isn1o)
      pointer (ipxico, xico)
      pointer (ipyico, yico)
      pointer (ipzico, zico)
C
C     Element Based Attributes
      pointer (ipitettypo, itettypo)
      pointer (ipitetoffo, itetoffo)
C
C     Array of No. of Elements*No. of Nodes per Element
      pointer (ipiteto, iteto)
C
      dimension xico(lenptr), yico(lenptr), zico(lenptr)
      integer   itp1o(lenptr), isn1o(lenptr)
      integer   itettypo(lenptr), itetoffo(lenptr)
      integer   iteto(2*lenptr)
C
C########################################################################
C
C Inline Function DBArea:
C
C     Returns double the area of a triangle ordered (counterclockwise)
c     1,2,3 in the u-v plane. This means that for a triangle ordered
c     1,2,3 in x-y-z space, the (r.h. rule) vector normal to this triangle
c     with magnitude equal to double the area is given by:
C                    < dbarea(y1,z1,y2,z2,y3,z3),
C                      dbarea(z1,x1,z2,x2,z3,x3),
C                      dbarea(x1,y1,x2,y2,x3,y3) >.
C
C########################################################################
C
      dbarea(u1,v1,u2,v2,u3,v3)=(u2-u1)*(v3-v1)-(v2-v1)*(u3-u1)
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
      if((nwds.ne.10)) then
         write(logmess,'(a)')
     &        'Error in subroutine extract_line: The proper Syntax is:'
         call writloga('default',0,logmess,0,ierror)
         write(logmess,'(a)')
     &       'extract_line/cmoout/cmoin/x1,y1,z1/x2,y2,z2/inf|lim'
         call writloga('default',0,logmess,0,ierror)
         ierror = 1
         go to 9999
      endif
C
      type=cmsgin(10)
      if((type.ne.'lim').AND.(type.ne.'inf')) then
         write(logmess,'(a)')
     &        'Warning in extract_line: Line type is neither inf nor'
         call writloga('default',0,logmess,0,ierror)
         write(logmess,'(a)')
     &        'lim. Assuming inf!'
         call writloga('default',0,logmess,0,ierror)
         type='inf'
      endif
C
C
C
C#######################################################################
C
C     Initialize the Mesh Objects
C
C     ******************************************************************
C     Check if the incoming MO exists
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
C     Get incoming MO information
C
      call cmo_get_info('nodes_per_element',cmoin,
     &     nen,ilen,icmotype,ierror)
C
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
C     Copy the points into their more "permanent" locations
C
      do i=1,2
C     Get the x part of the points
         if(msgtype(1+3*i).eq.1) then
            x_pt(i)=imsgin(1+3*i)
         elseif(msgtype(1+3*i).eq.2) then
            x_pt(i)=xmsgin(1+3*i)
         else
            write(logmess,35) i
 35      format('Error in subroutine extract_line: x',I1,' is invalid!')
            call writloga('default',0,logmess,0,ierror)
            ierror = 1
            go to 9999
         endif
C     Get the y part of the points
         if(msgtype(2+3*i).eq.1) then
            y_pt(i)=imsgin(2+3*i)
         elseif(msgtype(1+3*i).eq.2) then
            y_pt(i)=xmsgin(2+3*i)
         else
            write(logmess,36) i
 36      format('Error in subroutine extract_line: y',I1,' is invalid!')
            call writloga('default',0,logmess,0,ierror)
            ierror = 1
            go to 9999
         endif
C     Get the z part of the points
         if(msgtype(3+3*i).eq.1) then
            z_pt(i)=imsgin(3+3*i)
         elseif(msgtype(1+3*i).eq.2) then
            z_pt(i)=xmsgin(3+3*i)
         else
            write(logmess,37) i
 37      format('Error in subroutine extract_line: z',I1,' is invalid!')
            call writloga('default',0,logmess,0,ierror)
            ierror = 1
            go to 9999
         endif
      enddo
C
C#######################################################################
C
C     The first extract
C
C     ******************************************************************
C     Find a non-colinear point for the first extract
C
      deltax=x_pt(2)-x_pt(1)
      deltay=y_pt(2)-y_pt(1)
      deltaz=z_pt(2)-z_pt(1)
 
      if((deltax.eq.0).AND.(deltay.eq.0).AND.(deltaz.eq.0)) then
         write(logmess,38) i
 38      format('Error in subroutine extract_line: duplicate points',
     &        ' entered!')
         call writloga('default',0,logmess,0,ierror)
         ierror=1
         goto 9999
      endif
C
      if(deltaz.eq.0) then
         x_pt(3)=x_pt(2)
         y_pt(3)=y_pt(2)
         z_pt(3)=z_pt(2)+1
      elseif(deltay.eq.0) then
         x_pt(3)=x_pt(2)
         y_pt(3)=y_pt(2)+1
         z_pt(3)=z_pt(2)
      elseif(deltax.eq.0) then
         x_pt(3)=x_pt(2)+1
         y_pt(3)=y_pt(2)
         z_pt(3)=z_pt(2)
      else
         x_pt(3)=x_pt(2)+1
         y_pt(3)=y_pt(2)
         z_pt(3)=z_pt(2)
      endif
C
      write(cmdmess, 40) x_pt(1),y_pt(1),z_pt(1),
     &     x_pt(2),y_pt(2),z_pt(2),x_pt(3),y_pt(3),z_pt(3),
     &     cmotmp1,cmoin
 40   format('extract/plane/threepts/',
     &     E13.6,',',E13.6,',',E13.6,'/',
     &     E13.6,',',E13.6,',',E13.6,'/',
     &     E13.6,',',E13.6,',',E13.6,'/ 1 0 0/',
     &     A,'/',A,'; finish')
      call dotaskx3d(cmdmess,ierror)
      if(ierror.ne.0) then
         goto 9999
      endif
C
C     ******************************************************************
C     Post-Processing
C
      call dotaskx3d('resetpts/parent;rmpoint/compress; finish',ierror)
      if(ierror.ne.0) then
         goto 9999
      endif
C
C#######################################################################
C
C     The second Extract...
C     ******************************************************************
C     Find a non-colinear point for the first extract (using the normal)
C
      x_pt(4) = dbarea(y_pt(1),z_pt(1),y_pt(2),z_pt(2),y_pt(3),z_pt(3))
      y_pt(4) = dbarea(z_pt(1),x_pt(1),z_pt(2),x_pt(2),z_pt(3),x_pt(3))
      z_pt(4) = dbarea(x_pt(1),y_pt(1),x_pt(2),y_pt(2),x_pt(3),y_pt(3))
      anorm = sqrt(x_pt(4)*x_pt(4)+
     &              y_pt(4)*y_pt(4)+
     &              z_pt(4)*z_pt(4))
      x_pt(4) = x_pt(1)+x_pt(4)/anorm
      y_pt(4) = y_pt(1)+y_pt(4)/anorm
      z_pt(4) = z_pt(1)+z_pt(4)/anorm
C
      write(cmdmess, 42) x_pt(1),y_pt(1),z_pt(1),
     &     x_pt(2),y_pt(2),z_pt(2),x_pt(4),y_pt(4),z_pt(4),
     &     cmotmp2,cmoin
 42   format('extract/plane/threepts/',
     &     E13.6,',',E13.6,',',E13.6,'/',
     &     E13.6,',',E13.6,',',E13.6,'/',
     &     E13.6,',',E13.6,',',E13.6,'/ 1 0 0/',
     &     A,'/',A,'; finish')
      call dotaskx3d(cmdmess,ierror)
      if(ierror.ne.0) then
         goto 9999
      endif
C
C     ******************************************************************
C     Post-Processing
C
      call dotaskx3d('resetpts/parent;rmpoint/compress; finish',ierror)
      if(ierror.ne.0) then
         goto 9999
      endif
C
C#######################################################################
C
C     Do the intersection of the two planes
C
      write(cmdmess, 43) cmoout,cmotmp1,cmotmp2
 43   format('intersect/',A,'/',A,'/',A,'; finish')
      call dotaskx3d(cmdmess,ierror)
      if(ierror.ne.0) then
         goto 9999
      endif
C
C#######################################################################
C
C     If the type was defined as limited, find which nodes to keep, and
C     toss the others.
C
C     ******************************************************************
C     Get the necessary info about the output object
      if(type.eq.'lim') then
         call cmo_get_info('itp1',cmoout,ipitp1o,
     &        lenitp1o,icmotype,ierror)
         call cmo_get_info('isn1',cmoout,ipisn1o,ilen,icmotype,ierror)
         call cmo_get_info('xic',cmoout,ipxico,lenxico,icmotype,ierror)
         call cmo_get_info('yic',cmoout,ipyico,lenyico,icmotype,ierror)
         call cmo_get_info('zic',cmoout,ipzico,lenzico,icmotype,ierror)
         call cmo_get_info('nnodes',cmoout,nnodes,ilen,icmotype,ierror)
         call cmo_get_info('nelements',cmoout,nelements,
     &        ilen,icmotype,ierror)
         call cmo_get_info('itettyp',cmoout,ipitettypo,lenitettypo,
     &        icmotype,ierror)
         call cmo_get_info('itet',cmoout,ipiteto,leniteto,
     &        icmotype,ierror)
         call cmo_get_info('itetoff',cmoout,ipitetoffo,lenitetoffo,
     &        icmotype,ierror)
C
C     ******************************************************************
C     Find the nodes that are outside the endpoints of the line, and
C     dud them. Also, find the points closest to the end points of the
C     line that are outside (or on) the bounding box
C
         p1close = 0
         p2close = 0
C
         do i=1,nnodes
C        X coodinate...
            if(.NOT.(((xico(i).ge.x_pt(1)).AND.(xico(i).le.x_pt(2))).OR.
     &           ((xico(i).le.x_pt(1)).AND.(xico(i).ge.x_pt(2))))) then
               itptmp=itp1o(i)
               itp1o(i)=21
               dist2p1_curr=sqrt((xico(i)-x_pt(1))**2+
     &              (yico(i)-y_pt(1))**2+(zico(i)-z_pt(1))**2)
               dist2p2_curr=sqrt((xico(i)-x_pt(2))**2+
     &              (yico(i)-y_pt(2))**2+(zico(i)-z_pt(2))**2)
               if(dist2p1_curr.lt.dist2p2_curr) then
                  if(p1close.eq.0) then
                     p1close=i
                     dist2p1_ref=dist2p1_curr
                     itp1o(i)=itptmp
                  elseif(dist2p1_curr.lt.dist2p1_ref) then
                     itp1o(p1close)=21
                     itp1o(i)=itptmp
                     p1close=i
                     dist2p1_ref=dist2p1_curr
                  elseif(dist2p1_curr.eq.dist2p1_ref) then
                     itp1o(i)=itptmp
                   endif
               endif
               if(dist2p2_curr.lt.dist2p1_curr) then
                  if(p2close.eq.0) then
                     p2close=i
                     dist2p2_ref=dist2p2_curr
                     itp1o(i)=itptmp
                  elseif(dist2p2_curr.lt.dist2p2_ref) then
                     itp1o(p2close)=21
                     itp1o(i)=itptmp
                     p1close=i
                     dist2p2_ref=dist2p2_curr
                  elseif(dist2p2_curr.eq.dist2p2_ref) then
                     itp1o(i)=itptmp
                  endif
               endif
C        Y coodinate...
            elseif(.NOT.(((yico(i).ge.y_pt(1)).AND.(yico(i).le.y_pt(2)))
     &         .OR.((yico(i).le.y_pt(1)).AND.(yico(i).ge.y_pt(2)))))then
               itptmp=itp1o(i)
               itp1o(i)=21
               dist2p1_curr=sqrt((xico(i)-x_pt(1))**2+
     &              (yico(i)-y_pt(1))**2+(zico(i)-z_pt(1))**2)
               dist2p2_curr=sqrt((xico(i)-x_pt(2))**2+
     &              (yico(i)-y_pt(2))**2+(zico(i)-z_pt(2))**2)
               if(dist2p1_curr.lt.dist2p2_curr) then
                  if(p1close.eq.0) then
                     p1close=i
                     dist2p1_ref=dist2p1_curr
                     itp1o(i)=itptmp
                  elseif(dist2p1_curr.lt.dist2p1_ref) then
                     itp1o(p1close)=21
                     itp1o(i)=itptmp
                     p1close=i
                     dist2p1_ref=dist2p1_curr
                  elseif(dist2p1_curr.eq.dist2p1_ref) then
                     itp1o(i)=itptmp
                  endif
               endif
               if(dist2p2_curr.lt.dist2p1_curr) then
                  if(p2close.eq.0) then
                     p2close=i
                     dist2p2_ref=dist2p2_curr
                     itp1o(i)=itptmp
                  elseif(dist2p2_curr.lt.dist2p2_ref) then
                     itp1o(p2close)=21
                     itp1o(i)=itptmp
                     p1close=i
                     dist2p2_ref=dist2p2_curr
                  elseif(dist2p2_curr.eq.dist2p2_ref) then
                     itp1o(i)=itptmp
                  endif
               endif
C        Z coodinate...
            elseif(.NOT.(((zico(i).ge.z_pt(1)).AND.(zico(i).le.z_pt(2)))
     &         .OR.((zico(i).le.z_pt(1)).AND.(zico(i).ge.z_pt(2)))))then
               itptmp=itp1o(i)
               itp1o(i)=21
               dist2p1_curr=sqrt((xico(i)-x_pt(1))**2+
     &              (yico(i)-y_pt(1))**2+(zico(i)-z_pt(1))**2)
               dist2p2_curr=sqrt((xico(i)-x_pt(2))**2+
     &              (yico(i)-y_pt(2))**2+(zico(i)-z_pt(2))**2)
               if(dist2p1_curr.lt.dist2p2_curr) then
                  if(p1close.eq.0) then
                     p1close=i
                     dist2p1_ref=dist2p1_curr
                     itp1o(i)=itptmp
                  elseif(dist2p1_curr.lt.dist2p1_ref) then
                     itp1o(p1close)=21
                     itp1o(i)=itptmp
                     p1close=i
                     dist2p1_ref=dist2p1_curr
                  elseif(dist2p1_curr.eq.dist2p1_ref) then
                     itp1o(i)=itptmp
                  endif
               endif
               if(dist2p2_curr.lt.dist2p1_curr) then
                  if(p2close.eq.0) then
                     p2close=i
                     dist2p2_ref=dist2p2_curr
                     itp1o(i)=itptmp
                  elseif(dist2p2_curr.lt.dist2p2_ref) then
                     itp1o(p2close)=21
                     itp1o(i)=itptmp
                     p1close=i
                     dist2p2_ref=dist2p2_curr
                  elseif(dist2p2_curr.eq.dist2p2_ref) then
                     itp1o(i)=itptmp
                  endif
               endif
C     Boundary Conditions
            elseif((xico(i).eq.x_pt(1)).AND.(yico(i).eq.y_pt(1)).AND.
     &             (zico(i).eq.z_pt(1))) then
               if(p1close.ne.0) then
                  itp1o(p1close)=21
               endif
               p1close=i
               dist2p1_ref=0
            elseif((xico(i).eq.x_pt(2)).AND.(yico(i).eq.y_pt(2)).AND.
     &              (zico(i).eq.z_pt(2))) then
               if(p2close.ne.0) then
                  itp1o(p2close)=21
               endif
               p2close=i
               dist2p2_ref=0
            endif
         enddo
C
C     ******************************************************************
C     For the points that are closest to the endpoints, bring them into
C     the end points.
C
         if(p1close.ne.0) then
            xico(p1close)=x_pt(1)
            yico(p1close)=y_pt(1)
            zico(p1close)=z_pt(1)
            l = p1close
            if(isn1o(l).ne.0) then
 50            l=isn1o(l)
               if(l.ne.p1close) then
                  xico(l)=x_pt(1)
                  yico(l)=y_pt(1)
                  zico(l)=z_pt(1)
                  goto 50
               else
                  goto 55
               endif
            endif
         endif
 55      continue
         if(p2close.ne.0) then
            xico(p2close)=x_pt(2)
            yico(p2close)=y_pt(2)
            zico(p2close)=z_pt(2)
            l = p2close
            if(isn1o(l).ne.0) then
 60            l=isn1o(l)
               if(l.ne.p2close) then
                  xico(l)=x_pt(2)
                  yico(l)=y_pt(2)
                  zico(l)=z_pt(2)
                  goto 60
               else
                  goto 65
               endif
            endif
        endif
 65     continue
 
C
C#######################################################################
C
C     Mark the elements that have dudded nodes as endpoints for deletion
C
        do i=1,nelements
           do j=1,nelmnen(itettypo(i))
              if(itp1o(iteto(itetoffo(i)+j)).eq.21) then
                 iteto(itetoffo(i)+1)=-1
              endif
           enddo
        enddo
C
C#######################################################################
C
C     Remove the dead elements, points, and reset the parent/child
C     structure
C
        call dotaskx3d(
     &       "resetpts/parent;resetpts/itp;rmpoint/compress; finish",
     &       ierror)
        if(ierror.ne.0) then
           goto 9999
        endif
      endif
 
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
 
