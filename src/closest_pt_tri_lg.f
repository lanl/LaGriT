      subroutine closest_pt_tri_lg(px,py,pz,ax,ay,az,bx,by,bz,
     &   cx,cy,cz,qx,qy,qz,dist2)
C
C #####################################################################
C
C     PURPOSE -
C
C     Exact closest point Q on the triangle (A,B,C) to the point P,
C     and the squared distance DIST2 = |P-Q|**2.  Q may lie inside
C     the face, on one of the three edges or at one of the three
C     vertices (barycentric region test, Ericson, Real-Time
C     Collision Detection, 2005, sec. 5.1.5).
C
C     A line segment (A,B) is passed as the degenerate triangle
C     (A,B,B).  A triangle with no area (collinear vertices) is
C     measured as its three edges.
C
C     INPUT  - px,py,pz   the point
C              ax..cz     the triangle vertices
C     OUTPUT - qx,qy,qz   the closest point on the triangle
C              dist2      squared distance from P to Q
C
C #####################################################################
C
      implicit none
      real*8 px,py,pz,ax,ay,az,bx,by,bz,cx,cy,cz,qx,qy,qz,dist2
C
      real*8 abx,aby,abz,acx,acy,acz,apx,apy,apz,bpx,bpy,bpz,
     &   cpx,cpy,cpz,d1,d2,d3,d4,d5,d6,va,vb,vc,v,w,denom,
     &   crx,cry,crz,area2,sab,sac,rx,ry,rz,r2
C
      abx=bx-ax
      aby=by-ay
      abz=bz-az
      acx=cx-ax
      acy=cy-ay
      acz=cz-az
C
C.... A triangle with (almost) no area: take the best of its edges.
C.... This also covers a segment (A,B,B) and a point (A,A,A).
C
      crx=aby*acz-abz*acy
      cry=abz*acx-abx*acz
      crz=abx*acy-aby*acx
      area2=crx*crx+cry*cry+crz*crz
      sab=abx*abx+aby*aby+abz*abz
      sac=acx*acx+acy*acy+acz*acz
      if (area2.le.1.0d-24*sab*sac) then
         call closest_pt_seg_lg(px,py,pz,ax,ay,az,bx,by,bz,
     &      qx,qy,qz,dist2)
         call closest_pt_seg_lg(px,py,pz,ax,ay,az,cx,cy,cz,
     &      rx,ry,rz,r2)
         if (r2.lt.dist2) then
            qx=rx
            qy=ry
            qz=rz
            dist2=r2
         endif
         call closest_pt_seg_lg(px,py,pz,bx,by,bz,cx,cy,cz,
     &      rx,ry,rz,r2)
         if (r2.lt.dist2) then
            qx=rx
            qy=ry
            qz=rz
            dist2=r2
         endif
         return
      endif
C
C.... Vertex region A
      apx=px-ax
      apy=py-ay
      apz=pz-az
      d1=abx*apx+aby*apy+abz*apz
      d2=acx*apx+acy*apy+acz*apz
      if (d1.le.0.0d0 .and. d2.le.0.0d0) then
         qx=ax
         qy=ay
         qz=az
         goto 900
      endif
C
C.... Vertex region B
      bpx=px-bx
      bpy=py-by
      bpz=pz-bz
      d3=abx*bpx+aby*bpy+abz*bpz
      d4=acx*bpx+acy*bpy+acz*bpz
      if (d3.ge.0.0d0 .and. d4.le.d3) then
         qx=bx
         qy=by
         qz=bz
         goto 900
      endif
C
C.... Edge region AB
      vc=d1*d4-d3*d2
      if (vc.le.0.0d0 .and. d1.ge.0.0d0 .and. d3.le.0.0d0) then
         v=d1/(d1-d3)
         qx=ax+v*abx
         qy=ay+v*aby
         qz=az+v*abz
         goto 900
      endif
C
C.... Vertex region C
      cpx=px-cx
      cpy=py-cy
      cpz=pz-cz
      d5=abx*cpx+aby*cpy+abz*cpz
      d6=acx*cpx+acy*cpy+acz*cpz
      if (d6.ge.0.0d0 .and. d5.le.d6) then
         qx=cx
         qy=cy
         qz=cz
         goto 900
      endif
C
C.... Edge region AC
      vb=d5*d2-d1*d6
      if (vb.le.0.0d0 .and. d2.ge.0.0d0 .and. d6.le.0.0d0) then
         w=d2/(d2-d6)
         qx=ax+w*acx
         qy=ay+w*acy
         qz=az+w*acz
         goto 900
      endif
C
C.... Edge region BC
      va=d3*d6-d5*d4
      if (va.le.0.0d0 .and. (d4-d3).ge.0.0d0
     &    .and. (d5-d6).ge.0.0d0) then
         w=(d4-d3)/((d4-d3)+(d5-d6))
         qx=bx+w*(cx-bx)
         qy=by+w*(cy-by)
         qz=bz+w*(cz-bz)
         goto 900
      endif
C
C.... Face region
      denom=1.0d0/(va+vb+vc)
      v=vb*denom
      w=vc*denom
      qx=ax+abx*v+acx*w
      qy=ay+aby*v+acy*w
      qz=az+abz*v+acz*w
C
 900  continue
      dist2=(px-qx)**2+(py-qy)**2+(pz-qz)**2
      return
      end
C
C #####################################################################
C
      subroutine closest_pt_seg_lg(px,py,pz,ax,ay,az,bx,by,bz,
     &   qx,qy,qz,dist2)
C
C     Closest point Q on the segment (A,B) to P; DIST2 = |P-Q|**2.
C
      implicit none
      real*8 px,py,pz,ax,ay,az,bx,by,bz,qx,qy,qz,dist2
      real*8 abx,aby,abz,t,s
C
      abx=bx-ax
      aby=by-ay
      abz=bz-az
      s=abx*abx+aby*aby+abz*abz
      if (s.le.0.0d0) then
         t=0.0d0
      else
         t=((px-ax)*abx+(py-ay)*aby+(pz-az)*abz)/s
         t=max(0.0d0,min(1.0d0,t))
      endif
      qx=ax+t*abx
      qy=ay+t*aby
      qz=az+t*abz
      dist2=(px-qx)**2+(py-qy)**2+(pz-qz)**2
      return
      end
