/*
Purpose:  To give a better, stronger, faster, and more memory efficient
implementation of matbld3d_stor.f.
 
(See anothermatbld_wrapper.f for more.)
 
$Log: anothermatbld3d.c,v $
Revision 2.00  2007/11/05 19:45:46  spchu
Import to CVS

*PVCS    
*PVCS       Rev 1.10   04 Jun 2001 07:49:04   dcg
*PVCS    get rid of embedded underscore
*PVCS    
*PVCS       Rev 1.10   01 Jun 2001 16:30:44   dcg
*PVCS    get rid on embedded underscore in inititialize3ddiffusionmat
*PVCS
*PVCS       Rev 1.9   10 May 2001 13:43:14   jan
*PVCS    delete embedded unscores
*PVCS
*PVCS
*PVCS       Rev 1.8   08 May 2001 11:05:58   jan
*PVCS    took out embedded underscores in subroutine names and calls to subroutines
*PVCS
*PVCS       Rev 1.6   Mon Dec 20 19:18:52 1999   murphy
*PVCS    No change.
*PVCS
*PVCS       Rev 1.5   Fri Jul 16 14:09:18 1999   murphy
*PVCS    Can't remember what I did.
*PVCS
*PVCS       Rev 1.4   Thu Jul 15 16:22:26 1999   murphy
*PVCS    No change.
*PVCS
*PVCS       Rev 1.3   Tue Jun 15 15:52:42 1999   murphy
*PVCS    No change.
*PVCS
*PVCS       Rev 1.2   Mon Jun 14 11:01:08 1999   dcg
*PVCS    shorten name
*PVCS
*PVCS       Rev 1.1   Fri Jun 11 13:50:54 1999   murphy
*PVCS    Some DEC machines are too wimpy to handle long subroutine names.
*PVCS    So I shortened the names.
*PVCS
*PVCS       Rev 1.0   Tue May 18 14:46:44 1999   murphy
*PVCS    Initial revision.
*/

#include <assert.h>
#include <stdlib.h> 
#include <stdio.h>
#include <math.h>

#include "type_sizes.h"
#include "sparseMatrix.h"
 
#define MAX(a,b) (a > b ? a : b)
#define MIN(a,b) (a < b ? a : b)
 
#define det3(a11,a12,a13,a21,a22,a23,a31,a32,a33) \
 ((a31*((a12*a23)-(a22*a13)))-(a32*((a11*a23)-(a13*a21)))+(a33*((a11*a22)-\
 (a21*a12))))
 
#define det2(a11,a12,a21,a22) (a11*a22 - a21*a12)

/* Declare some Fortran functions we're going to use so that the compiler can do
 * type checking. */
void inside_tet_(
        double *x1, double *y1, double *z1,
        double *x2, double *y2, double *z2,
        double *x3, double *y3, double *z3,
        double *x4, double *y4, double *z4,
        double *xa, double *ya, double *za,
        int_ptrsize *flag);

void lineseg_tri_(
        double *x1, double *y1, double *z1,
        double *x2, double *y2, double *z2,
        double *x3, double *y3, double *z3,
        double *xa, double *ya, double *za,
        double *xb, double *yb, double *zb,
        double *x, double *y, double *z,
        int_ptrsize *flag);
 
typedef struct x3dMeshStructure {
  int_ptrsize nnodes;
  double *xic;
  double *yic;
  double *zic;
  int_ptrsize ntets;
  int_ptrsize *itet;
  int_ptrsize *jtet;
  int_ptrsize mbndry;
  int_ptrsize ifhybrid;     /* flag indicating whether we will use hybrid
                               median-Voronoi control volumes */
  double *hybrid_factor;    /* a value from 0 to 1 indicating the relative
                               position of the hybrid point between the Voronoi
                               point and the median point */
} x3dMesh;   /* a bunch of values... */
 
static x3dMesh *Mesh;
 
static int_ptrsize num_area_coefs;

/**************************************************************************/

static double dot(double x1, double y1, double z1,
    double x2, double y2, double z2)

/**************************************************************************/

/* Compute the dot product of two vectors. */

{
  return x1 * x2 + y1 * y2 + z1 * z2;
}

static double distance(double x1, double y1, double z1,
        double x2, double y2, double z2)
{
    return sqrt((x2 - x1)*(x2 - x1) + (y2 - y1)*(y2 - y1) +
            (z2 - z1)*(z2 - z1));
}
 
/**************************************************************************/
 
static void areaOf3dTriangle(double a0, double a1, double a2,
			double b0, double b1, double b2,
			double c0, double c1, double c2,
            double *xa, double *ya, double *za)
 
/**************************************************************************/

/*
 * Compute the vector area of a triangle. The result is stored in (xa, ya, za).
 */
 
{
  /* area (a,b,c) = 0.5 * length of AxB, A = b-a, B = c-a */
  double A0, A1, A2, B0, B1, B2;
 
  A0 = b0 - a0;
  A1 = b1 - a1;
  A2 = b2 - a2;
 
  B0 = c0 - a0;
  B1 = c1 - a1;
  B2 = c2 - a2;

  *xa = (A1*B2 - A2*B1)/2.0;
  *ya = (A2*B0 - A0*B2)/2.0;
  *za = (A0*B1 - A1*B0)/2.0;
}
 
 
/**************************************************************************/
 
static double volumeOfTet(double a0, double a1, double a2,
		   double b0, double b1, double b2,
		   double c0, double c1, double c2,
		   double d0, double d1, double d2)
 
/**************************************************************************/
 
     /*
	Computes the Volume of the tetrahedron with vertices a,b,c,d
	*/
 
{
  return((
 -b0 * c1 * d2 + a0 * c1 * d2 + b1 * c0 *d2 - a1 * c0 * d2 - a0 * b1 * d2
 +a1 * b0 * d2 + b0 * c2 * d1 - a0 * c2 *d1 - b2 * c0 * d1 + a2 * c0 * d1
 +a0 * b2 * d1 - a2 * b0 * d1 - b1 * c2 *d0 + a1 * c2 * d0 + b2 * c1 * d0
 -a2 * c1 * d0 - a1 * b2 * d0 + a2 * b1 *d0 + a0 * b1 * c2 - a1 * b0 * c2
 -a0 * b2 * c1 + a2 * b0 * c1 + a1 * b2 *c0 - a2 * b1 * c0)/6.0);
}
 
 
/*************************************************************************/
 
static void getCircumcenterOfTetrahedron(double a0, double a1, double a2,
				  double b0, double b1, double b2,
				  double c0, double c1, double c2,
				  double d0, double d1, double d2,
				  double *cenx, double *ceny, double *cenz)
 
/*************************************************************************/
 
 
/*
  Computes the center of the tetrahedron formed by
  a, b, c, and d.  It returns the center in the
  variables and cenx, ceny, cenz.
 
  Used to construct the Voronoi point associated with a Delaunay Tetrahedron.
*/
 
 
{
  double A[3][3],B[3];
  double deta;
 
  A[0][0] = d0 - a0;
  A[0][1] = d1 - a1;
  A[0][2] = d2 - a2;
 
 
  A[1][0] = d0 - b0;
  A[1][1] = d1 - b1;
  A[1][2] = d2 - b2;
 
 
  A[2][0] = d0 - c0;
  A[2][1] = d1 - c1;
  A[2][2] = d2 - c2;
 
 
  B[0] = 0.5*(A[0][0]*(a0+d0)+
	      A[0][1]*(a1+d1)+
	      A[0][2]*(a2+d2));
 
 
  B[1] = 0.5*(A[1][0]*(b0+d0)+
	      A[1][1]*(b1+d1)+
	      A[1][2]*(b2+d2));
 
 
  B[2] = 0.5*(A[2][0]*(c0+d0)+
	      A[2][1]*(c1+d1)+
	      A[2][2]*(c2+d2));
 
  /* Now solve the system */
  deta = det3(A[0][0],A[0][1],A[0][2],
	      A[1][0],A[1][1],A[1][2],
	      A[2][0],A[2][1],A[2][2]);
 
  if (deta==0) {
    printf("ERROR: Singular matrix\n");
  }
 
  *cenx = det3(B[0],A[0][1],A[0][2],
	       B[1],A[1][1],A[1][2],
	       B[2],A[2][1],A[2][2])/deta;
 
  *ceny = det3(A[0][0],B[0],A[0][2],
	       A[1][0],B[1],A[1][2],
	       A[2][0],B[2],A[2][2])/deta;
 
  *cenz = det3(A[0][0],A[0][1],B[0],
	       A[1][0],A[1][1],B[1],
	       A[2][0],A[2][1],B[2])/deta;
}

/**************************************************************************/
 
static void TranslateTetToZero (double a0, double a1, double a2,
			 double b0, double b1, double b2,
			 double c0, double c1, double c2,
      	    	         double d0, double d1, double d2,
                         double *xv1_save, double *yv1_save, double *zv1_save,
                         double *xv2_save, double *yv2_save, double *zv2_save,
                         double *xk1_save, double *yk1_save, double *zk1_save,
                         double *xk2_save, double *yk2_save, double *zk2_save)
 
/**************************************************************************/
 
/*
 Translate a tet so that the local node 0 is at coordinate (0,0,0)
 Return the original coordinates as arguments.
 */
{
  *xv1_save = a0;
  *yv1_save = a1;
  *zv1_save = a2;

  *xv2_save = b0;
  *yv2_save = b1;
  *zv2_save = b2;

  *xk1_save = c0;
  *yk1_save = c1;
  *zk1_save = c2;

  *xk2_save = d0;
  *yk2_save = d1;
  *zk2_save = d2;

  b0 = b0 - a0;
  b1 = b1 - a1;
  b2 = b2 - a2;

  c0 = c0 - a0;
  c1 = c1 - a1;
  c2 = c2 - a2;

  d0 = d0 - a0;
  d1 = d1 - a1;
  d2 = d2 - a2;

  a0 = a0 - a0;
  a1 = a1 - a1;
  a2 = a2 - a2;
}
/* end TranslateTetToZero() */

/**************************************************************************/

static void getCircumcenterOfTriangle(double a0, double a1, double a2,
                               double b0, double b1, double b2,
                               double c0, double c1, double c2,
                               double *cenx, double *ceny, double *cenz)

/**************************************************************************/

/*

  Computes the circumcenter of the (three dimensional triangle)
  formed by a, b, and c.  It returns the center in the variables and
  cx, cy, cz.

  Used to construct the Voronoi point associated with a Delaunay Triangle.  */

{
  double A[3][3],B[3];
  double deta;


  A[0][0] =  c0 - a0;
  A[0][1] =  c1 - a1;
  A[0][2] =  c2 - a2;

  A[1][0] =  b0 - a0;
  A[1][1] =  b1 - a1;
  A[1][2] =  b2 - a2;

  A[2][0] =  det2(A[0][1],A[0][2],A[1][1],A[1][2]);
  A[2][1] = -det2(A[0][0],A[0][2],A[1][0],A[1][2]);
  A[2][2] =  det2(A[0][0],A[0][1],A[1][0],A[1][1]);


  B[0] = 0.5*(A[0][0]*(a0+c0)+
              A[0][1]*(a1+c1)+
              A[0][2]*(a2+c2));


  B[1] = 0.5*(A[1][0]*(a0+b0)+
              A[1][1]*(a1+b1)+
              A[1][2]*(a2+b2));


  B[2] = (A[2][0]*(a0) +
          A[2][1]*(a1) +
          A[2][2]*(a2));


  /* Now solve the system */
  deta = det3(A[0][0],A[0][1],A[0][2],
              A[1][0],A[1][1],A[1][2],
              A[2][0],A[2][1],A[2][2])+1e-30;

  if (fabs(deta) ==0.0) {
    /* Are we ever setting ourselves up for epsilon problems
       with this test, baby! */
    printf("ERROR: Singular matrix--Compute Triangle center--%lf.\n",deta);
  }


  *cenx = det3(B[0],A[0][1],A[0][2],
               B[1],A[1][1],A[1][2],
               B[2],A[2][1],A[2][2])/deta;


  *ceny = det3(A[0][0],B[0],A[0][2],
               A[1][0],B[1],A[1][2],
               A[2][0],B[2],A[2][2])/deta;

  *cenz = det3(A[0][0],A[0][1],B[0],
               A[1][0],A[1][1],B[1],
               A[2][0],A[2][1],B[2])/deta;

}
/* end getCircumcenterOfTriangle() */
 

 
/**************************************************************************/
 
static void getTetVertices(int_ptrsize tet, int localEdgeNum,
		    int_ptrsize *v1, int_ptrsize *v2, int_ptrsize *k1, int_ptrsize *k2)
 
/**************************************************************************/
 
     /* Uses intimate knowledge of x3d data structure (See section on
	Mesh Object Connectivity in the x3d user manual for more
	details) to assign vertices of tetrahedron in a well-defined
	order with respect to which of the 6 edges of the tet we are
	visiting.  As long as we are consistent about it, I think we
	should be OK! */
 
{
  int_ptrsize a,b,c,d;
 
  a = Mesh->itet[(4*(tet-1))]   - 1 ;
  b = Mesh->itet[(4*(tet-1))+1] - 1 ;
  c = Mesh->itet[(4*(tet-1))+2] - 1 ;
  d = Mesh->itet[(4*(tet-1))+3] - 1 ;
 
  switch (localEdgeNum)
    {
    case 1:
      *v1 = a;
      *v2 = b;
      *k1 = d;
      *k2 = c;
      break;
    case 2:
      *v1 = a;
      *v2 = c;
      *k1 = b;
      *k2 = d;
      break;
    case 3:
      *v1 = a;
      *v2 = d;
      *k1 = c;
      *k2 = b;
      break;
    case 4:
      *v1 = b;
      *v2 = c;
      *k1 = d;
      *k2 = a;
      break;
    case 5:
      *v1 = b;
      *v2 = d;
      *k1 = a;
      *k2 = c;
      break;
    case 6:
      *v1 = c;
      *v2 = d;
      *k1 = b;
      *k2 = a;
      break;
    default:
      printf("Error: getTetVertices has unexpected set of vertices.\n");

      /* Ugly C preprocessor junk to make sure we use the right format string
       * depending on the size of int_ptrsize. */
#if SIZEOF_INT == SIZEOF_VOIDP
      printf("tet: %d with a b c d: %d %d %d %d\n",tet,a,b,c,d);
#else
      printf("tet: %ld with a b c d: %ld %ld %ld %ld\n",tet,a,b,c,d);
#endif
    }
}
/* end getTetVertices() */ 
 
/**************************************************************************/
 
int entryprocessed_(int_ptrsize *i, int_ptrsize *j)
 
/**************************************************************************/
 
/* returns TRUE if local edge j on tet i has been processed.  FALSE
   otherwise.  References passed for FORTRAN's sake */
 
 
{
  return(entryExists(*i,*j));
}
 
/**************************************************************************/
 
int_ptrsize entryprocessed(int_ptrsize *i, int_ptrsize *j)
 
/**************************************************************************/
 
/* returns TRUE if local edge j on tet i has been processed.  FALSE
   otherwise.  References passed for FORTRAN's sake */
 
 
{
  return(entryExists(*i,*j));
}
/* end entryprocessed_() and  entryprocessed() */ 

/**************************************************************************/

static int_ptrsize tetIsOnBoundary(int_ptrsize tet)

/**************************************************************************/

/*
 * Return 1 or 0 to indicate whether the tetrahedron indexed by tet is on a
 * boundary. Note that we assume that all elements in itet are tetrahedra; if
 * not then the results are undefined. We also assume that the tet index uses
 * the C convention of starting from 0. The Mesh structure declared at the top
 * of this file must be initialized.
 */
 
{
    size_t i;
    int_ptrsize *jtet;

    /* Adjust jtet so that it points to the tet we're interested in. */
    jtet = Mesh->jtet + 4 * tet;

    for (i = 0; i < 4; i++) {
        if (jtet[i] >= Mesh->mbndry) {
            return 1;
        }
    }

    return 0;
}

/**************************************************************************/

static int_ptrsize intersectSegmentWithFace(int_ptrsize *itet,
        double xm, double ym, double zm,
        double xv, double yv, double zv,
        int_ptrsize p1, int_ptrsize p2, int_ptrsize p3,
        double *x, double *y, double *z)

/**************************************************************************/

/*
 * Test for intersection between the line segment ((xm, ym, zm), (xv, yv, zv))
 * and the triangle defined by points p1, p2, and p3, which are indices into the
 * itet argument. Return 1 if an intersection exists and 0 otherwise. If there
 * is an intersection, the function will return it by placing it in the
 * arguments x, y, and z.
 */

{
    int_ptrsize flag;

    lineseg_tri_(
            &Mesh->xic[itet[p1] - 1], &Mesh->yic[itet[p1] - 1],
            &Mesh->zic[itet[p1] - 1], &Mesh->xic[itet[p2] - 1],
            &Mesh->yic[itet[p2] - 1], &Mesh->zic[itet[p2] - 1],
            &Mesh->xic[itet[p3] - 1], &Mesh->yic[itet[p3] - 1],
            &Mesh->zic[itet[p3] - 1],
            &xm, &ym, &zm, &xv, &yv, &zv, x, y, z, &flag);

    if (flag != -1)
        return 1;
    else
        return 0;
}

/**************************************************************************/

static int_ptrsize getHybridPoint(int_ptrsize tet,
        double xv, double yv, double zv,
        double *x, double *y, double *z,
        double *hybrid_factor)

/**************************************************************************/

/*
 * Given the index of a tet which does not contain its Voronoi center, compute
 * the point that represents the "hybrid median-Voronoi" center. We define this
 * to be the point where the line segment connecting the median point and
 * Voronoi point intersects a face of the tet. The triple (xv, yv, zv) should be
 * the coordinates of the Voronoi center. The function returns the result by
 * storing it in the arguments x, y, and z. It also computes the hybrid_factor
 * which is a measure of how far the hybrid point is relative to the Voronoi and
 * median points. A 0 indicates that the hybrid point is on the Voronoi point
 * whereas a 1 indicates that it is on the hybrid point.
 *
 * NOTE: This function will only compute the hybrid point if the Voronoi center
 * lies outside the boundary of the mesh. If the Voronoi center is outside the
 * tet but still within the mesh, we don't need to do anything. The function
 * will return 1 if it finds an intersection and 0 otherwise. If it returns 0
 * then (assuming nothing went wrong) we don't need to use a hybrid point
 * because the Voronoi center is still within the boundaries of the mesh.
 */

{
    size_t i;
    int_ptrsize *itet;
    int_ptrsize *jtet;
    double xm = 0, ym = 0, zm = 0;
    double dist_v_to_h;     /* distance from Voronoi to hybrid */
    double dist_v_to_m;     /* distance from Voronoi to median */

    /* Set itet and jtet to point to the tet we're interested in. */
    itet = Mesh->itet + 4 * tet;
    jtet = Mesh->jtet + 4 * tet;

    /* Compute the median point of the tetrahedron by taking the arithmetic mean
     * of each coordinate across the four vertices of the tet. */
    for (i = 0; i < 4; i++) {
        xm += Mesh->xic[itet[i] - 1];
    }
    xm /= 4.0;

    for (i = 0; i < 4; i++) {
        ym += Mesh->yic[itet[i] - 1];
    }
    ym /= 4.0;

    for (i = 0; i < 4; i++) {
        zm += Mesh->zic[itet[i] - 1];
    }
    zm /= 4.0;

    /* It's not obvious which face the median-Voronoi segment will intersect, so
     * we just try all boundary faces of this tet. */
    if (jtet[0] >= Mesh->mbndry && intersectSegmentWithFace(itet,
                xm, ym, zm, xv, yv, zv, 1, 2, 3, x, y, z))
        ;
    else if (jtet[1] >= Mesh->mbndry && intersectSegmentWithFace(itet,
                xm, ym, zm, xv, yv, zv, 0, 2, 3, x, y, z))
        ;
    else if (jtet[2] >= Mesh->mbndry && intersectSegmentWithFace(itet,
                xm, ym, zm, xv, yv, zv, 0, 1, 3, x, y, z))
        ;
    else if (jtet[3] >= Mesh->mbndry && intersectSegmentWithFace(itet,
                xm, ym, zm, xv, yv, zv, 0, 1, 2, x, y, z))
        ;
    else
        return 0;

    dist_v_to_h = distance(xv, yv, zv, *x, *y, *z);
    dist_v_to_m = distance(xv, yv, zv, xm, ym, zm);
    if (dist_v_to_m != 0.0) /* make sure we don't divide by zero */
        *hybrid_factor = dist_v_to_h / dist_v_to_m;
    else
        *hybrid_factor = 0;

    return 1;
}
 
/**************************************************************************/
 
static void computescalarVoronoientry(int_ptrsize index_i, int_ptrsize index_j,
				  int_ptrsize numIncidentTets, int_ptrsize *incidentTets,
				  int_ptrsize *localEdges)
 
/**************************************************************************/
 
     /* Computes the information for the ijth entry. */
     /* We specify index_i, index_j explicitly rather than deriving it
	from the incident tets because of "parential issues." So deal
	with it.*/
 
     /* incidentTets --- array of indices to the incident tetrahedra */
     /* localEdges   --- local edge number (ala x3d)corresponding to the
                         local edge number of the edge of interest on
                         each tet in incident tets*/
 
{
  int_ptrsize tetIndex;
  int_ptrsize v1,v2,k1,k2;
  int_ptrsize flag = 0;
 
  double area, edgelength, value, volume, vol1, vol2;
  double edge_unit_x, edge_unit_y, edge_unit_z; /* Unit edge vector */
  double area1, area2;
  double xa, ya, za;                            /* Triangle area vector */

  double bisectorx,   bisectory,   bisectorz,
         triCenter1x, triCenter1y, triCenter1z,
         triCenter2x, triCenter2y, triCenter2z,
         tetCenterx,  tetCentery,  tetCenterz;
  double xv1_save, yv1_save, zv1_save,
         xv2_save, yv2_save, zv2_save,
         xk1_save, yk1_save, zk1_save,
         xk2_save, yk2_save, zk2_save;
  double xhybrid, yhybrid, zhybrid;
 
  area=0.0;
  volume = 0.0;

  /* Compute unit vector in the direction of the edge from index_i to index_j.
   */
  edge_unit_x = Mesh->xic[index_j - 1] - Mesh->xic[index_i - 1];
  edge_unit_y = Mesh->yic[index_j - 1] - Mesh->yic[index_i - 1];
  edge_unit_z = Mesh->zic[index_j - 1] - Mesh->zic[index_i - 1];
  edgelength = sqrt(edge_unit_x*edge_unit_x +
      edge_unit_y*edge_unit_y +
      edge_unit_z*edge_unit_z);
  edge_unit_x /= edgelength;
  edge_unit_y /= edgelength;
  edge_unit_z /= edgelength;
 
  /* for every incidentTet.*/
  for (tetIndex=0; tetIndex < numIncidentTets; tetIndex++) {

    /* get indices of vertices of edge. */
    getTetVertices(incidentTets[tetIndex],localEdges[tetIndex],
		   &v1,&v2,&k1,&k2);

    /* Translate each tet to the origin (0,0,0) before
       all the geometric calculations.
       The function below translates so that node v1 has coordinates (0,0,0) 
    */
    TranslateTetToZero(   Mesh->xic[v1],Mesh->yic[v1],Mesh->zic[v1],
        	       	  Mesh->xic[v2],Mesh->yic[v2],Mesh->zic[v2],
			  Mesh->xic[k1],Mesh->yic[k1],Mesh->zic[k1],
			  Mesh->xic[k2],Mesh->yic[k2],Mesh->zic[k2],
                          &xv1_save, &yv1_save, &zv1_save,
                          &xv2_save, &yv2_save, &zv2_save,
                          &xk1_save, &yk1_save, &zk1_save,
                          &xk2_save, &yk2_save, &zk2_save);

    if ((MIN((v1+1),(v2+1)) != MIN(index_i,index_j)) ||
	(MAX((v1+1),(v2+1)) != MAX(index_i,index_j))) {
      printf("Error:  There are parential issues involved!!!!!!!\n");
    }
 
      /* Compute bisector, and the relevant circumcenters. */
    bisectorx = (Mesh->xic[v1] + Mesh->xic[v2])/2.0;
    bisectory = (Mesh->yic[v1] + Mesh->yic[v2])/2.0;
    bisectorz = (Mesh->zic[v1] + Mesh->zic[v2])/2.0;
 
    getCircumcenterOfTetrahedron(Mesh->xic[v1],Mesh->yic[v1],Mesh->zic[v1],
				 Mesh->xic[v2],Mesh->yic[v2],Mesh->zic[v2],
				 Mesh->xic[k1],Mesh->yic[k1],Mesh->zic[k1],
				 Mesh->xic[k2],Mesh->yic[k2],Mesh->zic[k2],
				 &tetCenterx, &tetCentery, &tetCenterz);
 
    /* If the tet is on the boundary and it does not contain its circumcenter,
     * then we might need to move the center back inside the tet. I say "might
     * need to" because it's possible that the Voronoi center is outside the tet
     * but still within the mesh, in which case we're ok already. We only
     * attempt to do this stuff if we're using the "hybrid" mode. */
    if (Mesh->ifhybrid && tetIsOnBoundary(incidentTets[tetIndex] - 1)) {
        /* This call might not be necessary. getHybridPoint will find out later
         * whether or not the circumcenter is inside the tet. */
        inside_tet_(&Mesh->xic[v1],&Mesh->yic[v1],&Mesh->zic[v1],
                     &Mesh->xic[v2],&Mesh->yic[v2],&Mesh->zic[v2],
                     &Mesh->xic[k1],&Mesh->yic[k1],&Mesh->zic[k1],
                     &Mesh->xic[k2],&Mesh->yic[k2],&Mesh->zic[k2],
                     &tetCenterx, &tetCentery, &tetCenterz, &flag);
        if (flag == -1 && getHybridPoint(incidentTets[tetIndex] - 1,
                    tetCenterx, tetCentery, tetCenterz,
                    &xhybrid, &yhybrid, &zhybrid,
                    &Mesh->hybrid_factor[incidentTets[tetIndex] - 1])) {
            tetCenterx = xhybrid;
            tetCentery = yhybrid;
            tetCenterz = zhybrid;
        }
    }
 
    getCircumcenterOfTriangle(Mesh->xic[v1], Mesh->yic[v1],Mesh->zic[v1],
			      Mesh->xic[v2], Mesh->yic[v2],Mesh->zic[v2],
			      Mesh->xic[k1], Mesh->yic[k1],Mesh->zic[k1],
			      &triCenter1x, &triCenter1y, &triCenter1z);
 
    getCircumcenterOfTriangle(Mesh->xic[v1], Mesh->yic[v1], Mesh->zic[v1],
			      Mesh->xic[v2], Mesh->yic[v2], Mesh->zic[v2],
			      Mesh->xic[k2], Mesh->yic[k2], Mesh->zic[k2],
			      &triCenter2x, &triCenter2y, &triCenter2z);
 
 
    /* Compute the signed contribution of area and volume. */
    areaOf3dTriangle(bisectorx,bisectory,bisectorz,
		       triCenter1x,triCenter1y,triCenter1z,
		       tetCenterx, tetCentery, tetCenterz,
               &xa, &ya, &za);
    /* Dot the area with the unit edge vector so that we count only the
     * component of the area vector that is in line with the edge. We take the
     * absolute value in case the area vector and edge vector are pointing in
     * opposite directions. */
    area1 = fabs(dot(xa, ya, za, edge_unit_x, edge_unit_y, edge_unit_z));
 
    vol1 =  volumeOfTet(Mesh->xic[v1],Mesh->yic[v1],Mesh->zic[v1],
			   bisectorx,bisectory,bisectorz,
			   triCenter1x,triCenter1y,triCenter1z,
			   tetCenterx, tetCentery,tetCenterz);
 
    if (vol1 < 0.0) {
      area1 = -area1;
    }
 
    areaOf3dTriangle(bisectorx,bisectory,bisectorz,
		     tetCenterx, tetCentery,tetCenterz,
		     triCenter2x,triCenter2y,triCenter2z,
             &xa, &ya, &za);
    area2 = fabs(dot(xa, ya, za, edge_unit_x, edge_unit_y, edge_unit_z));
	
    vol2 = volumeOfTet(Mesh->xic[v1],Mesh->yic[v1],Mesh->zic[v1],
			    triCenter2x,triCenter2y,triCenter2z,
			    bisectorx,bisectory,bisectorz,
			    tetCenterx, tetCentery,tetCenterz);
 
    if (vol2 < 0.0) {
      area2 = - area2;
    }
 
    area += area1+area2;
    volume += vol1 + vol2;
  }
 
  value = -area/edgelength;
 
  setEntry(index_i,index_j,volume,&value);
 /* Undo the translation of node v1 to (0,0,0)  */
       Mesh->xic[v1] = xv1_save;
       Mesh->yic[v1] = yv1_save;
       Mesh->zic[v1] = zv1_save;

       Mesh->xic[v2] = xv2_save;
       Mesh->yic[v2] = yv2_save;
       Mesh->zic[v2] = zv2_save;

       Mesh->xic[k1] = xk1_save;
       Mesh->yic[k1] = yk1_save;
       Mesh->zic[k1] = zk1_save;

       Mesh->xic[k2] = xk2_save;
       Mesh->yic[k2] = yk2_save;
       Mesh->zic[k2] = zk2_save;
}
 
 
/**************************************************************************/
 
void computeentry_(int_ptrsize *Pindex_i, int_ptrsize *Pindex_j, int_ptrsize
        *PnumIncidentTets, int_ptrsize *incidentTets, int_ptrsize *localEdges)
 
/**************************************************************************/
 
{
  /* This is where we place hooks for the good stuff.. */
  computescalarVoronoientry(*Pindex_i, *Pindex_j, *PnumIncidentTets,
			       incidentTets, localEdges);
 
}
 
 
/**************************************************************************/
 
void computeentry(int_ptrsize *Pindex_i, int_ptrsize *Pindex_j, int_ptrsize
        *PnumIncidentTets, int_ptrsize *incidentTets, int_ptrsize *localEdges)
 
/**************************************************************************/
 
{
  /* This is where we place hooks for the good stuff.. But for now...*/
  computescalarVoronoientry(*Pindex_i, *Pindex_j, *PnumIncidentTets,
			       incidentTets, localEdges);
 
}
 
 
 
/**************************************************************************/
 
void initialize3ddiffusionmat_(int_ptrsize *pentrysize, int_ptrsize *pcompress,
        int_ptrsize *pnnodes, double *xic, double *yic, double *zic, int_ptrsize
        *pntets, int_ptrsize *itet, int_ptrsize *jtet, int_ptrsize *pmbndry,
        int_ptrsize *ifhybrid, double *hybrid_factor, double *eps)
 
/**************************************************************************/
 
    /*  The input variable names are slightly funny because FORTRAN
	passes by reference.  We dereference below when we populate the
	x3dMesh structure.  */
 
{

  double eps_val;

/*
  TAM
  double eps = 1e-8;
  changed to passed in value in eps
  extend range of ccoef values allow eps to be set by user */

  eps_val = *eps;
  printf("\nSparseMatrix initialize epsilon to %e\n",eps_val) ;

  /* populate mesh structure. */
  Mesh=(x3dMesh*)  malloc(sizeof(x3dMesh));

  Mesh->xic     = xic;
  Mesh->yic     = yic;
  Mesh->zic     = zic;
  Mesh->nnodes  = *pnnodes;
  Mesh->ntets   = *pntets;
  Mesh->itet    = itet;
  Mesh->jtet    = jtet;
  Mesh->mbndry  = *pmbndry;
  Mesh->ifhybrid= *ifhybrid;
  Mesh->hybrid_factor = hybrid_factor;

  num_area_coefs = *pentrysize;

  createSparseMatrix(Mesh->nnodes,num_area_coefs,*pcompress,eps_val);
 
}
 
 
/**************************************************************************/
 
void initialize3ddiffusionmat(int_ptrsize *pentrysize, int_ptrsize *pcompress,
        int_ptrsize *pnnodes, double *xic, double *yic, double *zic, int_ptrsize
        *pntets, int_ptrsize *itet, int_ptrsize *jtet, int_ptrsize *pmbndry,
        int_ptrsize *ifhybrid, double *hybrid_factor,double *eps)
 
/**************************************************************************/
 
 
{
  initialize3ddiffusionmat_(pentrysize, pcompress, pnnodes, xic, yic, zic,
          pntets, itet, jtet, pmbndry, ifhybrid, hybrid_factor, eps);
 
}
 
 
/***************************************************************************/
 
void finalscalar3ddiffusionmat_()
 
/***************************************************************************/
 
{
  free(Mesh);
  setDiagonalEntries();
}
 
/***************************************************************************/
 
void finalscalar3ddiffusionmat()
 
/***************************************************************************/
 
{
  free(Mesh);
  setDiagonalEntries();
}
