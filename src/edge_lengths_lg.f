      subroutine edge_lengths_lg(cmode,nnode,iedges,iedges_first,
     &           iparent,xic,yic,zic,value)
C
C #####################################################################
C
C     PURPOSE -
C
C        Fill a vertex attribute with a statistic of the lengths of
C        the edges at each vertex, for
C           cmo / addatt / mo / edge_lengths_min  / att_name
C           cmo / addatt / mo / edge_lengths_max  / att_name
C           cmo / addatt / mo / edge_lengths_mean / att_name
C        Edges are those of the cells (any cell type); an edge shared
C        by several cells counts once. The edge lists come from
C        getedges_d, as for edge_connections, between parent vertices:
C        a child vertex (interface or doubled) takes its parent's
C        value. A vertex with no edge gets 0.
C
C     INPUT ARGUMENTS -
C
C        cmode        - 'min', 'max' or 'mean'
C        nnode        - number of vertices
C        iedges       - the neighbours of each vertex (getedges_d)
C        iedges_first - where the neighbours of vertex i start in
C                       iedges; they end before iedges_first(i+1)
C        iparent      - parent of each vertex
C        xic,yic,zic  - vertex coordinates
C
C     OUTPUT ARGUMENTS -
C
C        value        - the statistic at each vertex
C
C #####################################################################
C
      implicit none
C
      character*(*) cmode
      integer nnode
      integer iedges(*),iedges_first(*),iparent(*)
      real*8 xic(*),yic(*),zic(*),value(*)
C
      integer i,j,jn,k,n,ip
      real*8 d,dmin,dmax,dsum
C
      do i = 1, nnode
         k = iedges_first(i)
         n = iedges_first(i+1) - iedges_first(i)
         if(n .le. 0) then
            value(i) = 0.0d0
         else
            dmin = huge(1.0d0)
            dmax = 0.0d0
            dsum = 0.0d0
            do j = 1, n
               jn = iedges(k+j-1)
               d = sqrt((xic(jn)-xic(i))**2 + (yic(jn)-yic(i))**2 +
     &                  (zic(jn)-zic(i))**2)
               dmin = min(dmin,d)
               dmax = max(dmax,d)
               dsum = dsum + d
            enddo
            if(cmode(1:3) .eq. 'min') then
               value(i) = dmin
            elseif(cmode(1:3) .eq. 'max') then
               value(i) = dmax
            else
               value(i) = dsum/dble(n)
            endif
         endif
      enddo
C
C     a child vertex has no edges of its own: give it its parent's
C
      do i = 1, nnode
         ip = iparent(i)
         if(ip.ne.i .and. ip.ge.1 .and. ip.le.nnode) then
            if(iedges_first(i+1)-iedges_first(i) .le. 0)
     &         value(i) = value(ip)
         endif
      enddo
C
      return
      end
