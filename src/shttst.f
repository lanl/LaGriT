      subroutine shttst(qx,qy,qz,epsln,cmoin,ickout)
C
C########################################################################
C
C     PURPOSE -
C
C     THIS ROUTINE TESTS WHETHER A POINT IS LT, GT, OR EQ (IN, OUT OR
C     ON) A SHEET SURFACE.
C
C
C     INPUT ARGUMENTS -
C
C        qx - X COORDINATE OF THE POINT TO CHECK
C        qy - Y COORDINATE OF THE POINT TO CHECK
C        qz - Z COORDINATE OF THE POINT TO CHECK
C        epsln - EPSILON FOR SURFACE CHECKS
C        cmoin - name of sheet mesh object
C
C     OUTPUT ARGUMENTS -
C
C        ickout - POINT-SURFACE REFERENCE (lt, gt OR eq)
C
C
C     CHANGE HISTORY -
C
C        $Log: shttst.f,v $
C        Revision 2.00  2007/11/09 20:04:03  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.4   Thu Mar 09 09:03:42 2000   dcg
CPVCS    pass sheet name correctly to sheet test routine
CPVCS    
CPVCS       Rev 1.3   Thu Feb 03 09:00:32 2000   dcg
CPVCS    
CPVCS       Rev 1.2   13 Jan 2000 14:49:22   dcg
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.1   04 Jan 2000 16:48:06   dcg
CPVCS     
CPVCS
CPVCS       Rev 1.14   Mon Apr 14 17:01:42 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.13   Wed Feb 28 18:11:48 1996   ahmed
CPVCS    fix epsilon
CPVCS
CPVCS       Rev 1.12   Tue Feb 13 14:43:26 1996   ahmed
CPVCS    base the algorithm on k-D search
CPVCS
CPVCS       Rev 1.11   12/05/95 08:21:18   het
CPVCS    Make changes for UNICOS
CPVCS
CPVCS       Rev 1.10   11/07/95 17:26:32   dcg
CPVCS    change flag to 2 in mmgetblk calls
CPVCS
CPVCS       Rev 1.9   10/18/95 12:17:40   het
CPVCS    Allow greater that 8 character names for sheets in the surface command.
CPVCS
CPVCS       Rev 1.8   10/11/95 10:29:40   ahmed
CPVCS    implement new definitions of inside outside wrt. sheets
CPVCS
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
      pointer (ipdist, dist)
      pointer (ipcpx, cpx)
      pointer (ipcpy, cpy)
      pointer (ipcpz, cpz)
      pointer (ipcptype, cptype)
      pointer (ipdmin, dmin)
      pointer (ipitfound, itfound)
      pointer (ipinfound, infound)
      pointer (ipwork, work)
      pointer (iptn, tn)
C
      dimension dist(lenptr),cpx(lenptr),cpy(lenptr),cpz(lenptr),
     &          dmin(lenptr),work(lenptr),tn(3,lenptr)
      integer itfound(lenptr),infound(lenptr),node1
      character*8 cptype(lenptr)
C
C     SET POINTERS FOR THE SHEET cmo
C
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      pointer (iplinkt, linkt)
      pointer (ipsbox, sbox)
      pointer (ipitet, itet)
C
      dimension xic(lenptr),yic(lenptr),zic(lenptr),sbox(lenptr)
      integer   linkt(lenptr),itet(3,lenptr)
C
      character*8 ickout
      character*32 cmoin
      character*32 isubname
      character*132 logmess
C
C########################################################################
C
C     STATEMENT FUNCTION DBAREA GIVES DOUBLE THE AREA OF A TRIANGLE
C     ORDERED (COUNTERCLOCKWISE) 1,2,3 IN THE u-v PLANE. THIS MEANS
C     THAT FOR A TRIANGLE ORDERED 1,2,3 IN x-y-z SPACE, THE (R.H. RULE)
C     VECTOR NORMAL TO THIS TRIANGLE WITH MAGNITUDE EQUAL TO DOUBLE THE
C     AREA IS GIVEN BY ( dbarea(y1,z1,y2,z2,y3,z3),
C                        dbarea(z1,x1,z2,x2,z3,x3),
C                        dbarea(x1,y1,x2,y2,x3,y3) ).
 
      dbarea(u1,v1,u2,v2,u3,v3)=(u2-u1)*(v3-v1)-(v2-v1)*(u3-u1)
C
      next3(ii)=mod(ii,3)+1
      iprev3(ii)=mod(ii+1,3)+1
C
C########################################################################
C     ierror - ERROR FLAG RETURNS (0 IF THERE IS NO ERROR,
C                                  1 IF THERE IS AN ERROR)
      ierror = 0
      pi=3.14159265385
      ickout='  '
C
C     *******************************************************************
C     SET THE MEMORY MANAGED PARTITION NAME.
C
      isubname='shttst'
C
C     *******************************************************************
C     GET THE SHEET DATA FROM THE cmo.
C
      call cmo_get_info('nnodes',cmoin,nnodes,ilen,icmotype,ierror)
      call cmo_get_info('nelements',cmoin,
     &                   nelements,ilen,icmotype,ierror)
      call cmo_get_info('xic',cmoin,ipxic,ilen,icmotype,ierror)
      call cmo_get_info('yic',cmoin,ipyic,ilen,icmotype,ierror)
      call cmo_get_info('zic',cmoin,ipzic,ilen,icmotype,ierror)
      call cmo_get_info('sbox',cmoin,ipsbox,ilen,icmotype,ierror)
      call cmo_get_info('linkt',cmoin,iplinkt,ilen,icmotype,ierror)
      call cmo_get_info('itet',cmoin,ipitet,ilen,icmotype,ierror)
C
C     ******************************************************************
C     GET MEMORY FOR LOCAL ARRAYS.
C
      lenmm1=nnodes
      lenmm2=nelements
      lenmm3=3*nelements
C
      call mmgetblk('dist',isubname,ipdist,lenmm1,2,icscode)
      call mmgetblk('cpx',isubname,ipcpx,lenmm2,2,icscode)
      call mmgetblk('cpy',isubname,ipcpy,lenmm2,2,icscode)
      call mmgetblk('cpz',isubname,ipcpz,lenmm2,2,icscode)
      call mmgetblk('cptype',isubname,ipcptype,lenmm2,2,icscode)
      call mmgetblk('dmin',isubname,ipdmin,lenmm2,2,icscode)
      call mmgetblk('itfound',isubname,ipitfound,lenmm2,2,icscode)
      call mmgetblk('infound',isubname,ipinfound,lenmm2,2,icscode)
      call mmgetblk('work',isubname,ipwork,lenmm2,2,icscode)
      call mmgetblk('tn',isubname,iptn,lenmm3,2,icscode)
C
C     ******************************************************************
C     GET A FEASIBLE SUBSET OF CLOSEST TRIANGLES (USING THE k-D TREE)
C     GUARANTEED TO CONTAIN THE NEAREST POINT TO THE QUERY POINT.
C
      node1=itet(1,1)
      cpx1=xic(node1)
      cpy1=yic(node1)
      cpz1=zic(node1)
C
      call nearestpoint(qx,qy,qz,xic,yic,zic, itet,
     &                  cpx1,cpy1,cpz1,linkt,sbox,
     &                  epsln,work,nfound,itfound,ierror)
C
      if (nfound .eq. 0) then
         write(logmess,'(a)') 'Error in subroutine shttst:
     &   kd tree returns no triangles'
         call writloga('default',0,logmess,0,ierror)
      endif
C
C     ******************************************************************
C     COMPUTE THE MIN DISTANCE FROM THE QUERY POINT TO EACH SELECTED
C     ELEMENT AND FIND THE CORRESPONDING CLOSEST POINT ON THAT ELEMENT.
C
         do it=1,nfound
            itri=itfound(it)
C
C           ------------------------------------------------------------
C           FIRST CHECK ELEMENT VERTICES.
C
            dmin(itri)=1.0e32
            do i=1,3
               ii=itet(i,itri)
               dist(ii)=sqrt( (qx-xic(ii))*(qx-xic(ii))+
     &                        (qy-yic(ii))*(qy-yic(ii))+
     &                        (qz-zic(ii))*(qz-zic(ii)) )
C
C           -------------------------------------------------------------
C           TEST IF THE QUERY POINT LIES ON A VERTEX.
C
               if (dist(ii) .lt. epsln) then
                  ickout='eq'
                  goto 9999
               endif
C
               if (dist(ii) .lt. dmin(itri)) then
                 dmin(itri)=dist(ii)
                 cpx(itri)=xic(ii)
                 cpy(itri)=yic(ii)
                 cpz(itri)=zic(ii)
                 infound(itri)=i
               endif
            enddo
            cptype(itri)='vertex'
C
C           ------------------------------------------------------------
C           SECOND CHECK ELEMENT EDGES.
C
            nedges=0
            do i=1,3
               i1=itet(i       ,itri)
               i2=itet(next3(i),itri)
               ax=xic(i2)-xic(i1)
               ay=yic(i2)-yic(i1)
               az=zic(i2)-zic(i1)
               bx=qx-xic(i1)
               by=qy-yic(i1)
               bz=qz-zic(i1)
               denom=ax*ax+ay*ay+az*az
               t=(ax*bx+ay*by+az*bz)/denom
               eps=2.0*epsln/sqrt(denom)
               if (t .gt. eps .and. t .lt. 1.0-eps) then
                  nedges=nedges+1
                  xs=t*xic(i2)+(1.0-t)*xic(i1)
                  ys=t*yic(i2)+(1.0-t)*yic(i1)
                  zs=t*zic(i2)+(1.0-t)*zic(i1)
                  dedge= sqrt((qx-xs)*(qx-xs)+
     &                        (qy-ys)*(qy-ys)+
     &                        (qz-zs)*(qz-zs))
C
C              ----------------------------------------------------------
C              TEST IF THE QUERY POINT LIES ON A EDGE.
C
               if (dedge .lt. epsln) then
                  ickout='eq'
                  goto 9999
               endif
C
                  if (dedge .lt. dmin(itri)) then
                     dmin(itri)=dedge
                     cpx(itri)=xs
                     cpy(itri)=ys
                     cpz(itri)=zs
                     cptype(itri)='edge'
                  endif
               endif
            enddo
C
C           ------------------------------------------------------------
C           NOW WE STORE THE NORMALS TO nfound ELEMENTS IN THE ARRAY tn.
C
            i1=itet(1,itri)
            i2=itet(2,itri)
            i3=itet(3,itri)
            x1=xic(i1)
            y1=yic(i1)
            z1=zic(i1)
            x2=xic(i2)
            y2=yic(i2)
            z2=zic(i2)
            x3=xic(i3)
            y3=yic(i3)
            z3=zic(i3)
C
            tn(1,itri)=dbarea(y1,z1,y2,z2,y3,z3)
            tn(2,itri)=dbarea(z1,x1,z2,x2,z3,x3)
            tn(3,itri)=dbarea(x1,y1,x2,y2,x3,y3)
            dcross=sqrt( tn(1,itri)*tn(1,itri)+
     &                   tn(2,itri)*tn(2,itri)+
     &                   tn(3,itri)*tn(3,itri) )
            tn(1,itri)=tn(1,itri)/dcross
            tn(2,itri)=tn(2,itri)/dcross
            tn(3,itri)=tn(3,itri)/dcross
C
C           ------------------------------------------------------------
C           FINALLY CHECK THE FACES OF ELEMENTS IF NEEDED.
C
            if (nedges .gt. 1) then
C
               iface=1
               dface=(qx-x1)*tn(1,itri)+(qy-y1)*tn(2,itri)+
     &               (qz-z1)*tn(3,itri)
C
               if (abs(dface) .lt. dmin(itri)) then
                  xs=qx-tn(1,itri)*dface
                  ys=qy-tn(2,itri)*dface
                  zs=qz-tn(3,itri)*dface
C
                  do i=1,3
                     i1=itet(i       ,itri)
                     i2=itet(next3(i),itri)
                     x1=xic(i1)
                     y1=yic(i1)
                     z1=zic(i1)
                     x2=xic(i2)
                     y2=yic(i2)
                     z2=zic(i2)
                     el=sqrt( (x2-x1)*(x2-x1)+
     &                        (y2-y1)*(y2-y1)+
     &                        (z2-z1)*(z2-z1) )
                     area=dbarea(y1,z1,y2,z2,ys,zs)*tn(1,itri)+
     &                    dbarea(z1,x1,z2,x2,zs,xs)*tn(2,itri)+
     &                    dbarea(x1,y1,x2,y2,xs,ys)*tn(3,itri)
                     if (area .le. 2.0*el*epsln) iface=0
                  enddo
                  if (iface .eq. 1) then
C
C                 ------------------------------------------------------
C                 TEST IF THE QUERY POINT LIES ON A TRIANGLE'S FACE.
C
                  if (abs(dface) .lt. epsln) then
                     ickout='eq'
                     goto 9999
                  endif
C
                     dmin(itri)=abs(dface)
                     cpx(itri)=xs
                     cpy(itri)=ys
                     cpz(itri)=zs
                     cptype(itri)='face'
                  endif
               endif
            endif
         enddo
C
C        ***************************************************************
C        FIND THE MIN DISTANCE dmintri.
C
         dmintri=1.0e32
         do it=1,nfound
            itri=itfound(it)
            if (dmin(itri) .lt. dmintri) dmintri=dmin(itri)
         enddo
C
C        ***************************************************************
C        TEST THE QUERY POINT FOR INSIDE, ON, AND OUTSIDE.
C
         if (dmintri .lt. epsln) ickout='eq'
         if (ickout .eq. '  ') then
C
C           ------------------------------------------------------------
C           PICK UP THE ELEMENTS THAT HAVE MIN DISTANCE EQUAL TO dmintri
C
            icnt=0
            do it=1,nfound
               itri=itfound(it)
               if (abs(dmin(itri)-dmintri) .lt. epsln) then
                  icnt=icnt+1
                  itfound(icnt)=itri
               endif
            enddo
C
C           ************************************************************
C           CHOOSE THE FIRST CLOSEST POINT AND LOOP THROUGHT THE REST TO
C             PICK THE ONES THAT ARE THE SAME.
C
            ncp=1
            itri1=itfound(1)
            if (icnt .ge. 2) then
               do it=2,icnt
                  itri=itfound(it)
                  dp1pi=sqrt( (cpx(itri)-cpx(itri1))*
     &                        (cpx(itri)-cpx(itri1)) +
     &                        (cpy(itri)-cpy(itri1))*
     &                        (cpy(itri)-cpy(itri1)) +
     &                        (cpz(itri)-cpz(itri1))*
     &                        (cpz(itri)-cpz(itri1)) )
                  if (dp1pi .lt. epsln) then
                     ncp=ncp+1
                     itfound(ncp)=itri
                  endif
               enddo
            endif
C
C           ************************************************************
C           COMPUTE THE (SYNTHETIC) NORMAL AT THE NEAREST POINT. NEAREST
C           POINT COULD LIE ON A 'VERTEX', 'EDGE', OR A 'FACE'.
C           INITIALIZE THE OUTWARD NORMAL THAT WILL BE USED FOR TESTING.
C
            vx=0.0
            vy=0.0
            vz=0.0
C
            do it=1,ncp
               itri=itfound(it)
               if( cptype(itri) .eq. 'vertex') then
                   ind=infound(itri)
                   i1=itet(ind,        itri)
                   i2=itet(iprev3(ind),itri)
                   i3=itet(next3(ind), itri)
                   x1=xic(i2)-xic(i1)
                   y1=yic(i2)-yic(i1)
                   z1=zic(i2)-zic(i1)
                   x2=xic(i3)-xic(i1)
                   y2=yic(i3)-yic(i1)
                   z2=zic(i3)-zic(i1)
                   top=x1*x2+y1*y2+z1*z2
                   bot=sqrt(x1*x1+y1*y1+z1*z1)*sqrt(x2*x2+y2*y2+z2*z2)
                   bot=max(epsln,bot)
                   ang=acos(top/bot)
                   vx=vx+ang*tn(1,itri)
                   vy=vy+ang*tn(2,itri)
                   vz=vz+ang*tn(3,itri)
               elseif(cptype(itri) .eq. 'edge') then
                    ang=pi
                    vx=vx+ang*tn(1,itri)
                    vy=vy+ang*tn(2,itri)
                    vz=vz+ang*tn(3,itri)
               elseif(cptype(itri) .eq. 'face') then
                   if(ncp .eq. 1) then
                     vx=tn(1,itri)
                     vy=tn(2,itri)
                     vz=tn(3,itri)
                   else
                     ang=1.0e32
                     vx=vx+ang*tn(1,itri)
                     vy=vy+ang*tn(2,itri)
                     vz=vz+ang*tn(3,itri)
C
                     write(logmess,'(a)')
     &                     'Warning in subroutine shttst:
     &                      Shared face nearest point'
                     call writloga('default',0,logmess,0,ierror)
                   endif
 
               else
                     write(logmess,'(a)')
     &                     'Error in subroutine shttst:
     &                      Nearest point type not defined'
                     call writloga('default',0,logmess,0,ierror)
                     ierror = 1
                     go to 9999
               endif
            enddo
C
            anorm=max(epsln,sqrt(vx*vx+vy*vy+vz*vz))
            vx=vx/anorm
            vy=vy/anorm
            vz=vz/anorm
            a=qx-cpx(itri1)
            b=qy-cpy(itri1)
            c=qz-cpz(itri1)
            test=a*vx+b*vy+c*vz
            if (test .lt. 0.0) then
               ickout='lt'
            else
               ickout='gt'
            endif
         endif
C
      goto 9999
 9999 continue
C
C     *******************************************************************
C     RELEASE TEMPORARY MEMORY.
C
 9995 call mmrelprt(isubname,ics)
C
C     *******************************************************************
C     SET UP THE CFT IMMUNE STATEMENT FOR DDT.
C
      return
      end
