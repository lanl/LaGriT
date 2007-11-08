*CD smooth
C
C
C ######################################################################
C
C     PURPOSE -
C
C        Topological Relations for hybrid smoothing.
C
C     INPUT ARGUMENTS -
C
C        None
C
C     OUTPUT ARGUMENTS -
C
C        None
C
C     CHANGE HISTORY -
C
C        $Log: smooth.h,v $
C        Revision 2.00  2007/11/05 19:46:03  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.3   20 Feb 2002 16:59:38   kuprat
CPVCS    Added MAXEL parameter.
CPVCS    
CPVCS       Rev 1.2   Tue Oct 26 14:08:20 1999   dcg
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.1   Mon Apr 14 16:37:34 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.0   Thu Oct 31 13:30:36 1996   kuprat
CPVCS    Initial revision.
C
C ######################################################################
C

      integer i_,j_

c.... IHYBNUMTETV gives the number of 'virtual tetrahedra' for each
c.... kind of element.

      integer ihybnumtetv(nelmtypes)
      data ihybnumtetv/0,0,1,2,1,4,12,10,0,0/
      save ihybnumtetv

c.... MAXHYBNUMTETV gives the maximum number of virtual tetrahedra that
c.... any type of element can have.

      integer maxhybnumtetv
      parameter (maxhybnumtetv=12)

c.... For each supported element type, ITETV gives the 'ITET' relation
c.... for the virtual tetrahedra in terms of the local node numbers.
c.... The supported element types are tetrahedra, pyramids, prisms,
c.... and hexahedra.

      integer itetv(4,maxhybnumtetv,nelmtypes)
      data ((itetv(i_,j_,5),i_=1,4),
     &	    j_=1,maxhybnumtetv)
     &   /1,2,3,4, 0,0,0,0, 0,0,0,0, 0,0,0,0,
     &    0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0,
     &    0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0 /
      data ((itetv(i_,j_,6),i_=1,4),
     &	    j_=1,maxhybnumtetv)
     &   /5,1,4,3, 5,1,3,2, 5,3,2,4, 5,2,1,4,
     &    0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0,
     &    0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0 /
      data ((itetv(i_,j_,7),i_=1,4),
     &	    j_=1,maxhybnumtetv)
     &   /1,4,5,6, 2,4,5,6, 3,4,5,6, 4,1,3,2,
     &    5,1,3,2, 6,1,3,2, 1,2,6,5, 6,1,2,4,
     &    1,3,4,5, 1,3,6,5, 2,3,4,5, 2,3,4,6 /
      data ((itetv(i_,j_,8),i_=1,4),
     &	    j_=1,maxhybnumtetv)
     &   /1,2,4,5, 8,5,4,7, 3,4,2,7, 6,2,5,7,
     &    2,4,5,7, 2,1,6,3, 4,1,3,8, 5,1,8,6, 
     &    7,8,3,6, 8,3,6,1, 0,0,0,0, 0,0,0,0 /
      save itetv

c.... For each supported element type, WTTETV gives the weights of each
c.... virtual tetrahedron.

      real*8 wttetv(maxhybnumtetv,nelmtypes)
      data (wttetv(i_,5),i_=1,maxhybnumtetv)
     &   /one,     zero,    zero,    zero,
     &    zero,    zero,    zero,    zero,
     &    zero,    zero,    zero,    zero /
      data (wttetv(i_,6),i_=1,maxhybnumtetv)
     &   /half,    half,    half,    half,
     &    zero,    zero,    zero,    zero,
     &    zero,    zero,    zero,    zero /
      data (wttetv(i_,7),i_=1,maxhybnumtetv)
     &   /one3rd,  one3rd,  one3rd,  one3rd,
     &    one3rd,  one3rd,  one6th,  one6th,
     &    one6th,  one6th,  one6th,  one6th /
      data (wttetv(i_,8),i_=1,maxhybnumtetv)
     &   /half,    half,    half,    half,
     &    half,    half,    half,    half,
     &    half,    half,    zero,    zero /
      save wttetv

c.... MAXNODNUMTETV gives the maximum number of virtual tetrahedra that
c.... any local node in any kind of supported element can be a member of.

      integer maxnodnumtetv
      parameter (maxnodnumtetv=8)

c.... NODNUMTETV gives for each local node in each supported element type
c.... the actual number of virtual tetrahedra that the node is a member of.
c.... This array is derived at run time from the ITETV relation.

      integer nodnumtetv(maxnen,nelmtypes)
      common /smooth01/ nodnumtetv
      save /smooth01/

c.... JTETV relates local element node
c.... numbers to virtual tet/ local virtual tet node numbers. More
c.... precisely, if the i'th local node in a given element happens
c.... to be the j'th node in the k'th virtual tet, then there is a jtetv
c.... value equal to (k-1)*4+j, corresponding to node i.
c.... This array is derived at run time from the ITETV relation.

      integer jtetv(maxnodnumtetv,maxnen,nelmtypes)
      common /smooth02/ jtetv
      save /smooth02/

      integer maxel
      parameter (MAXEL=100)
      
