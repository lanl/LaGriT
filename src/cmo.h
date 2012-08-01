*cd,cmo
C
C
C#######################################################################
C
C
C     INCLUDE THE CMO.h file
C
C
C#######################################################################
C
C
C      CHANGE HISTORY:
C
C         $Log: cmo.h,v $
C         Revision 2.00  2007/11/05 19:45:47  spchu
C         Import to CVS
C
CPVCS    
CPVCS       Rev 1.18   Tue Mar 07 08:25:06 2000   dcg
CPVCS    remove references to ign1
CPVCS
CPVCS       Rev 1.17   Tue Oct 26 14:07:36 1999   dcg
CPVCS    No change.
CPVCS
CPVCS       Rev 1.16   Thu Jan 21 20:54:48 1999   jtg
CPVCS    common blocks moved after declarations and/or saves added
CPVCS
CPVCS       Rev 1.15   Wed Dec 23 13:12:20 1998   jtg
CPVCS    common blocks moved to after declarations
CPVCS
CPVCS       Rev 1.14   Mon Apr 14 16:37:16 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.13   Tue Mar 05 12:50:58 1996   dcg
CPVCS    remove int1, icn1 from mesh object description
CPVCS
CPVCS       Rev 1.12   Fri Feb 23 09:29:48 1996   dcg
CPVCS    remove uic,vic,wic,pic,ric,eic
CPVCS
CPVCS       Rev 1.11   02/10/95 09:13:04   ejl
CPVCS    Split cmo.h into cmo.h and mesh_object.h
CPVCS    Cleaned up and implicit none.
CPVCS
CPVCS       Rev 1.10   01/31/95 13:19:26   het
CPVCS    Make cmo a common block variable
CPVCS
CPVCS       Rev 1.9   01/23/95 12:42:10   het
CPVCS    Put in the default cmo_attribute table variables.
CPVCS
CPVCS
CPVCS       Rev 1.8   01/04/95 22:06:56   llt
CPVCS    unicos changes (made by het)
CPVCS
CPVCS       Rev 1.7   12/09/94 22:45:50   het
CPVCS    Made changes to support the new cmo_ routines.
CPVCS
CPVCS
CPVCS       Rev 1.6   12/06/94 19:08:18   het
CPVCS    Add the icmonum, nnodes_cmo, nelements_cmo variables
CPVCS    to the current mesh object definition.
CPVCS
CPVCS
CPVCS       Rev 1.5   12/02/94 16:18:58   dcg
CPVCS     changes required to compile on IBM RS6000
CPVCS
CPVCS       Rev 1.4   12/02/94 15:08:28   het
CPVCS    Added the icmouse, icmoget, icmoset variables.
CPVCS
CPVCS
CPVCS       Rev 1.3   12/01/94 18:52:08   het
CPVCS    Modified the "cmo" support data structures.
CPVCS
CPVCS
CPVCS       Rev 1.2   11/28/94 14:18:42   het
CPVCS    Add the "mbndry" scalar that indicates a boundary.
CPVCS
CPVCS       Rev 1.1   11/14/94 15:23:02   het
CPVCS
CPVCS
CPVCS       Rev 1.1   11/14/94 15:22:00   het
CPVCS
CPVCS
CPVCS       Rev 1.0   11/14/94 12:05:22   het
CPVCS    Original Version
C
C#######################################################################
C
C
C     COMMON BLOCK DEFINITION FOR CURRENT_MESH_OBJECT USE
C
      integer icmouse, icmoget, icmoset
      common / icmo_use / icmouse, icmoget, icmoset
      save / icmo_use /
C
      character*32 cmo
      common / name_cmo / cmo
      save / name_cmo /
C
C
C     *****************************************************************
C
C     DEFINE COMMON AND POINTERS TO BE IMPORTED FROM DBMS.
C
C     *****************************************************************
C
      integer nnodes,
     *        nelements,
     *        nfaces,
     *        nedges,
     *        mbndry,
     *        ndimensions_geom,
     *        ndimensions_topo,
     *        maximum_nodes_per_element,
     *        maximum_faces_per_element,
     *        maximum_edges_per_element,
     *        itotal_node_connections,
     *        itotal_face_connections,
     *        itotal_edge_connections
C
      common / icmo_integer_scalars / nnodes,
     *                                nelements,
     *                                nfaces,
     *                                nedges,
     *                                mbndry,
     *                                ndimensions_geom,
     *                                ndimensions_topo,
     *                                maximum_nodes_per_element,
     *                                maximum_faces_per_element,
     *                                maximum_edges_per_element,
     *                                itotal_node_connections,
     *                                itotal_face_connections,
     *                                itotal_edge_connections
      save / icmo_integer_scalars /
C
C
C     *****************************************************************
C
      pointer (ipisetwd, isetwd)
      pointer (ipialias, ialias)
      pointer (ipimt1, imt1)
      pointer (ipitp1, itp1)
      pointer (ipicr1, icr1)
      pointer (ipisn1, isn1)
      integer isetwd(*), ialias(*)
      integer imt1(*), itp1(*),
     *        icr1(*), isn1(*)
C
      common / icmo_integer_pointers / ipisetwd,
     *                                 ipialias,
     *                                 ipimt1,
     *                                 ipitp1,
     *                                 ipicr1,
     *                                 ipisn1
      save / icmo_integer_pointers /
C
C
C     *****************************************************************
C
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      REAL*8 xic(*), yic(*), zic(*)
C
      common / cmo_real_pointers / ipxic, ipyic, ipzic
      save / cmo_real_pointers /
C
C
C     *****************************************************************
C
      pointer (ipitetclr, itetclr)
      pointer (ipitettyp, itettyp)
      pointer (ipitet, itet)
      pointer (ipitet, itet1)
      pointer (ipjtet, jtet)
      pointer (ipjtet, jtet1)
      integer itetclr(*), itettyp(*)
      integer itet(4,*), itet1(*)
      integer jtet(4,*), jtet1(*)
C
      common / icmo_conn_pointers / ipitetclr,
     *                              ipitettyp,
     *                              ipitet,
     *                              ipjtet
      save / icmo_conn_pointers /
C
C *****************************************************************
C
 
