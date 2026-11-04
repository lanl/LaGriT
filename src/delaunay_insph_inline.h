C     delaunay_insph_inline.h -- included in delaunay.f (connect /
C     exact) where a tet is tested: sets rtv (the quick test, the same
C     value as rtest) and isgn (1 inside, 0 on, -1 outside, exact) for
C     the tet idx, its vertex ivertex and the point mpnt.
C     The quick test uses the tet's stored circumcentre (twice it in
C     xvor, yvor, zvor) and 6 x volume (vol). It has the sign of the
C     exact test unless it is within the error of the circumcentre
C     (about eps R**4/|6V| with R the circumradius, plus eps |X| for
C     the final addition) and of its own evaluation; the bound used is
C     4 times what that analysis gives. Inside it, or for a nearly flat
C     tet, lg_predicates3d.c decides; for a tet of positive volume its
C     orientation is passed (LaGriT's positive tets have orient3d = -1;
C     iom2 = -2 also says the quick test was too close to call).
C     iwalkq = 1 (the walk, its first 50 steps): a near-tie counts as
C     outside without the exact test; the walk only needs some tet that
C     holds mpnt strictly inside its sphere, and the tet that contains
C     mpnt does.
      dxv=xic(ivertex)-xic(mpnt)
      dyv=yic(ivertex)-yic(mpnt)
      dzv=zic(ivertex)-zic(mpnt)
      sxv=xic(ivertex)+xic(mpnt)-xvor(idx)
      syv=yic(ivertex)+yic(mpnt)-yvor(idx)
      szv=zic(ivertex)+zic(mpnt)-zvor(idx)
      rtv=dxv*sxv+dyv*syv+dzv*szv
      r2v=(xic(ivertex)-0.5d0*xvor(idx))**2
     *   +(yic(ivertex)-0.5d0*yvor(idx))**2
     *   +(zic(ivertex)-0.5d0*zvor(idx))**2
      v6v=vol(idx)
      if (v6v.gt.0.0d0 .and. v6v*v6v.gt.1.0d-12*r2v*r2v*r2v) then
         errv=4.0d0*((abs(dxv)+abs(dyv)+abs(dzv))*
     *      (384.0d0*epsr*r2v*r2v/v6v
     *      +4.0d0*epsr*(abs(xvor(idx))+abs(yvor(idx))+abs(zvor(idx))))
     *      +8.0d0*epsr*(abs(dxv*sxv)+abs(dyv*syv)+abs(dzv*szv)))
         if (rtv.gt.errv) then
            isgn=1
         elseif (rtv.lt.-errv .or. iwalkq.ne.0) then
            isgn=-1
         else
            call lg_insphere3d_or(xic(itet(1,idx)),yic(itet(1,idx)),
     *         zic(itet(1,idx)),xic(itet(2,idx)),yic(itet(2,idx)),
     *         zic(itet(2,idx)),xic(itet(3,idx)),yic(itet(3,idx)),
     *         zic(itet(3,idx)),xic(itet(4,idx)),yic(itet(4,idx)),
     *         zic(itet(4,idx)),xic(mpnt),yic(mpnt),zic(mpnt),iom2,isgn)
         endif
      else
         call delaunay_insph(itet(1,idx),itet(2,idx),itet(3,idx),
     *      itet(4,idx),mpnt,xic,yic,zic,isgn)
      endif
