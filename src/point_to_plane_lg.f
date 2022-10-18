subroutine point_to_plane_lg (xq,yq,zq,xplane,yplane,zplane,
                              *    xproj,yproj,zproj,dist,ierror)
c#######################################################################
c
c     purpose -
c
C  This routine computes the projection of the query point
c  to a plane and determines if the projection is inside
c  the triangle of nodes defining the plane
c
c  input:
c
c  xq,yq,zq  coordinates of query point
c  xplane,yplane,zplane   coordinates of nodes defining the plane
c
c  output:
c
c  xproj,yproj,zproj coordinates of projected point
c  dist  distance from query point to projected point
c  ierror = 0 normal completion
            c         = 1 plane ill defined (probably nodes are collinear)
                            c         = 2 projected node outside triangle
                                        C
                                    C $Log:
                                        point_to_plane_lg.f,v $
                                        C Revision 2.00  2007/11/09 20:03:58  spchu
                                        C Import to CVS
                                        C
                                        CPVCS
                                        CPVCS       Rev 1.1   01 Feb 2002 10:40:02   dcg
                                        CPVCS    add subroutine project_point_to_line
                                        CPVCS
                                        CPVCS       Rev 1.0   28 Jan 2002 13:14:54   dcg
                                        CPVCS    Initial revision.
                                        c#######################################################################
                                        c
                                        implicit none
                                        real*8 xq,yq,zq,xplane(*),yplane(*),zplane(*),
                                        *   xproj,yproj,zproj,a,b,c,d,au,bu,cu,sf,ss,
                                        *   sarea,dist,sarea0
                                        integer ierror
                                        include 'consts.h'
                                        include 'statementfunctions.h'
                                        C
                                        C  SET UP THE EQUATION OF THE PLANE FROM THE 3 POINTS.
                                        C
                                        ierror=0
                                                a=dcrosx(xplane(1),yplane(1),zplane(1),
                                                        *   xplane(2),yplane(2),zplane(2),
                                                        *   xplane(3),yplane(3),zplane(3))
                                                        b=dcrosy(xplane(1),yplane(1),zplane(1),
                                                                *   xplane(2),yplane(2),zplane(2),
                                                                *   xplane(3),yplane(3),zplane(3))
                                                                c=dcrosz(xplane(1),yplane(1),zplane(1),
                                                                        *   xplane(2),yplane(2),zplane(2),
                                                                        *   xplane(3),yplane(3),zplane(3))
                                                                        d=a*xplane(1)+b*yplane(1)+c*zplane(1)
                                                                                ss=sqrt(a*a + b*b + c*c)
                                                                                        if (ss.le.zero) then
                                                                                            ierror=1
                                                                                                    xproj=0.
                                                                                                            yproj=0.
                                                                                                                    zproj=0.
                                                                                                                            else
                                                                                                                                c  get unit normal to plane
                                                                                                                                C  and the distance to the plane (sf)
                                                                                                                                au=a/ss
                                                                                                                                        bu=b/ss
                                                                                                                                                cu=c/ss
                                                                                                                                                        sf=(a*xq + b*yq + c*zq - d)/ss
                                                                                                                                                                c  get projection of query point onto plane
                                                                                                                                                                xproj=xq-au*sf
                                                                                                                                                                        yproj=yq-bu*sf
                                                                                                                                                                                zproj=zq-cu*sf
                                                                                                                                                                                        dist=dlen(xq,yq,zq,xproj,yproj,zproj)
                                                                                                                                                                                                sarea0=dszarea(xplane(1),yplane(1),zplane(1),
                                                                                                                                                                                                        *   xplane(2),yplane(2),zplane(2),
                                                                                                                                                                                                        *   xplane(3),yplane(3),zplane(3))
                                                                                                                                                                                                        sarea0=dsign(one,sarea0)
                                                                                                                                                                                                                c  test inside
                                                                                                                                                                                                                sarea=dszarea(xplane(2),yplane(2),zplane(2),
                                                                                                                                                                                                                        *   xplane(3),yplane(3),zplane(3),
                                                                                                                                                                                                                        *   xproj,yproj,zproj)
                                                                                                                                                                                                                        if(sarea0.ne.dsign(one,sarea)) then
                                                                                                                                                                                                                            ierror=2
                                                                                                                                                                                                                                    go to 9999
                                                                                                                                                                                                                                    endif
                                                                                                                                                                                                                                    sarea=dszarea(xplane(1),yplane(1),zplane(1),
                                                                                                                                                                                                                                            *   xplane(2),yplane(2),zplane(2),
                                                                                                                                                                                                                                            *   xproj,yproj,zproj)
                                                                                                                                                                                                                                            if(sarea0.ne.dsign(one,sarea)) then
                                                                                                                                                                                                                                                ierror=2
                                                                                                                                                                                                                                                        go to 9999
                                                                                                                                                                                                                                                        endif
                                                                                                                                                                                                                                                        sarea=dszarea(xplane(1),yplane(1),zplane(1),
                                                                                                                                                                                                                                                                *   xproj,yproj,zproj,
                                                                                                                                                                                                                                                                *   xplane(3),yplane(3),zplane(3))
                                                                                                                                                                                                                                                                if(sarea0.ne.dsign(one,sarea)) then
                                                                                                                                                                                                                                                                    ierror=2
                                                                                                                                                                                                                                                                            go to 9999
                                                                                                                                                                                                                                                                            endif
                                                                                                                                                                                                                                                                            endif
                                                                                                                                                                                                                                                                            9999 continue
                                                                                                                                                                                                                                                                            return
                                                                                                                                                                                                                                                                                    end
                                                                                                                                                                                                                                                                                    C
                                                                                                                                                                                                                                                                                    C
                                                                                                                                                                                                                                                                                    C
                                                                                                                                                                                                                                                                                    subroutine project_point_to_line(x1,y1,z1,x2,y2,z2,
                                                                                                                                                                                                                                                                                            *   x3,y3,z3,xproj,yproj,zproj)
                                                                                                                                                                                                                                                                                    implicit none
                                                                                                                                                                                                                                                                                    include 'consts.h'
                                                                                                                                                                                                                                                                                    c
                                                                                                                                                                                                                                                                                    c  project x3,y3,z3 onto line defined by
                                                                                                                                                                                                                                                                                    c  x1,y1,z1,x2,y2,z2
                                                                                                                                                                                                                                                                                    c  return coordinates in xproj,yproj,zproj
                                                                                                                                                                                                                                                                                            c
                                                                                                                                                                                                                                                                                            real*8 x1,y1,z1,x2,y2,z2,x3,y3,z3,xproj,yproj,zproj,frac
                                                                                                                                                                                                                                                                                            c
                                                                                                                                                                                                                                                                                            frac=((x2-x1)*(x3-x1)+
                                                                                                                                                                                                                                                                                                    &      (y2-y1)*(y3-y1)+
                                                                                                                                                                                                                                                                                                    &      (z2-z1)*(z3-z1))/
                                                                                                                                                                                                                                                                                                    &     ((x2-x1)*(x2-x1)+
                                                                                                                                                                                                                                                                                                            &      (y2-y1)*(y2-y1)+
                                                                                                                                                                                                                                                                                                            &      (z2-z1)*(z2-z1))
                                                                                                                                                                                                                                                                                                    xproj=frac*x2+(one-frac)*x1
                                                                                                                                                                                                                                                                                                            yproj=frac*y2+(one-frac)*y1
                                                                                                                                                                                                                                                                                                                    zproj=frac*z2+(one-frac)*z1
                                                                                                                                                                                                                                                                                                                            return
                                                                                                                                                                                                                                                                                                                                    end
