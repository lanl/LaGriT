      subroutine avs_cell_table(k, cname, lname, ifelm, nvert, njtet,
     *                          lperm)
C
C #####################################################################
C
C     PURPOSE -
C
C        The AVS UCD cell types that read / avs and dump / avs know,
C        in one place for both: the AVS name, the LaGriT cell type,
C        and the vertex order.
C
C     INPUT ARGUMENTS -
C
C        k      - 1 to 8: pt, line, tri, quad, tet, pyr, prism, hex
C                 (any other k: nvert = 0)
C
C     OUTPUT ARGUMENTS -
C
C        cname  - the AVS cell name (character*8, blank padded)
C        lname  - its length
C        ifelm  - the LaGriT cell type (local_element.h)
C        nvert  - the number of vertices
C        njtet  - the number of jtet entries read / avs sets to -1
C                 (as it always has)
C        lperm  - lperm(j) = the LaGriT vertex (1..nvert) at AVS
C                 position j: AVS vertex j of a cell is itet(lperm(j))
C
C     CHANGE HISTORY -
C
C        Nov 2026: new, from the cell branches of readavs.f and
C        dumpavs.f (the orders are those they used).
C
C ######################################################################
C
      implicit none
      include "local_element.h"
      integer k, lname, ifelm, nvert, njtet, lperm(8)
      character*8 cname
      integer j
      integer perm(8,8), nv(8), nj(8), lnam(8)
      character*8 cnam(8)
      data cnam / 'pt', 'line', 'tri', 'quad', 'tet', 'pyr',
     *            'prism', 'hex' /
      data lnam / 2, 4, 3, 4, 3, 3, 5, 3 /
      data nv   / 1, 2, 3, 4, 4, 5, 6, 8 /
      data nj   / 1, 2, 3, 4, 4, 5, 5, 6 /
      data perm / 1, 0, 0, 0, 0, 0, 0, 0,
     *            1, 2, 0, 0, 0, 0, 0, 0,
     *            1, 2, 3, 0, 0, 0, 0, 0,
     *            1, 2, 3, 4, 0, 0, 0, 0,
     *            1, 2, 4, 3, 0, 0, 0, 0,
     *            5, 1, 2, 3, 4, 0, 0, 0,
     *            4, 5, 6, 1, 2, 3, 0, 0,
     *            5, 6, 7, 8, 1, 2, 3, 4 /
C
      nvert = 0
      njtet = 0
      lname = 0
      ifelm = 0
      cname = ' '
      if (k.lt.1 .or. k.gt.8) return
      cname = cnam(k)
      lname = lnam(k)
      nvert = nv(k)
      njtet = nj(k)
      do j = 1, 8
         lperm(j) = perm(j,k)
      enddo
      if (k.eq.1) ifelm = ifelmpnt
      if (k.eq.2) ifelm = ifelmlin
      if (k.eq.3) ifelm = ifelmtri
      if (k.eq.4) ifelm = ifelmqud
      if (k.eq.5) ifelm = ifelmtet
      if (k.eq.6) ifelm = ifelmpyr
      if (k.eq.7) ifelm = ifelmpri
      if (k.eq.8) ifelm = ifelmhex
      return
      end
C
      integer function avs_width(n)
C
C     The width dump / avs gives a vertex or cell number when there
C     are n of them: 3 below 99, 4 below 999, ..., at most 11 (the
C     steps of the format ladders it replaces).
C
      implicit none
      integer n, m
      avs_width = 1
      m = n + 1
      do while (m .ge. 10)
         m = m / 10
         avs_width = avs_width + 1
      enddo
      avs_width = max(3, min(11, avs_width + 1))
      return
      end
