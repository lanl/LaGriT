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
 
#include <stdio.h>
#include <math.h>
 
#define MAX(a,b) (a > b ? a : b)
#define MIN(a,b) (a < b ? a : b)
 
#define det3(a11,a12,a13,a21,a22,a23,a31,a32,a33) \
 ((a31*((a12*a23)-(a22*a13)))-(a32*((a11*a23)-(a13*a21)))+(a33*((a11*a22)-\
 (a21*a12))))
 
#define det2(a11,a12,a21,a22) (a11*a22 - a21*a12)
 
extern double sqrt();
 
typedef struct x3dMeshStructure {
  int nnodes;
  double *xic;
  double *yic;
  double *zic;
  int ntets;
  int *itet;
} x3dMesh;   /* a bunch of values... */
 
x3dMesh *Mesh;
 
static int num_area_coefs;
 
/**************************************************************************/
 
double areaOf3dTriangle(double a0, double a1, double a2,
			double b0, double b1, double b2,
			double c0, double c1, double c2)
 
/**************************************************************************/
 
{
     /* area (a,b,c) = length of AxB, A= b-a, B = c-a */
  double A0, A1, A2, B0,B1,B2;
  double D1,D2,D3;
 
  A0 = b0 - a0;
  A1 = b1 - a1;
  A2 = b2 - a2;
 
  B0 = c0 - a0;
  B1 = c1 - a1;
  B2 = c2 - a2;
 
  D1 = A1*B2-A2*B1;
  D1 = D1*D1;
 
  D2 = A2*B0 - A0*B2;
  D2 = D2*D2;
 
  D3 = A0*B1 - A1*B0;
  D3 = D3*D3;
 
  return (sqrt(D1+D2+D3)/2.0);
 
}
 
 
/**************************************************************************/
 
double volumeOfTet(double a0, double a1, double a2,
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
 
void getCircumcenterOfTetrahedron(double a0, double a1, double a2,
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
 
void getCircumcenterOfTriangle(double a0, double a1, double a2,
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
 
 
/**************************************************************************/
 
void getTetVertices(int tet, int localEdgeNum,
		    int *v1, int *v2, int *k1, int *k2)
 
/**************************************************************************/
 
     /* Uses intimate knowledge of x3d data structure (See section on
	Mesh Object Connectivity in the x3d user manual for more
	details to assign vertices of tetrahedron in a well-defined
	order with respect to which of the 6 edges of the tet we are
	visiting.  As long as we are consistent about it, I think we
	should be OK! */
 
{
  int a,b,c,d;
 
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
      printf("That's not supposed to happen.\n");
    }
}
 
 
/**************************************************************************/
 
int entryprocessed_(int *i, int *j)
 
/**************************************************************************/
 
/* returns TRUE if local edge j on tet i has been processed.  FALSE
   otherwise.  References passed for FORTRAN's sake */
 
 
{
  int v1,v2,k1,k2;
 
  return(entryExists(*i,*j));
}
 
/**************************************************************************/
 
int entryprocessed(int *i, int *j)
 
/**************************************************************************/
 
/* returns TRUE if local edge j on tet i has been processed.  FALSE
   otherwise.  References passed for FORTRAN's sake */
 
 
{
  int v1,v2,k1,k2;
 
  return(entryExists(*i,*j));
}
 
 
/**************************************************************************/
 
void computescalarVoronoientry(int index_i, int index_j,
				  int numIncidentTets, int *incidentTets,
				  int *localEdges)
 
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
  int tetIndex;
  int v1,v2,k1,k2;
 
  double area, edgelength, value, volume, area1, area2, vol1,vol2;
  double bisectorx,   bisectory,   bisectorz,
         triCenter1x, triCenter1y, triCenter1z,
         triCenter2x, triCenter2y, triCenter2z,
         tetCenterx,  tetCentery,  tetCenterz;
 
 
 
  area=0.0; volume = 0.0;
 
  /* for every incidentTet.*/
  for (tetIndex=0; tetIndex < numIncidentTets; tetIndex++) {
    /* get indices of vertices of edge. */
    getTetVertices(incidentTets[tetIndex],localEdges[tetIndex],
		   &v1,&v2,&k1,&k2);
 
 
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
 
    getCircumcenterOfTriangle(Mesh->xic[v1], Mesh->yic[v1],Mesh->zic[v1],
			      Mesh->xic[v2], Mesh->yic[v2],Mesh->zic[v2],
			      Mesh->xic[k1], Mesh->yic[k1],Mesh->zic[k1],
			      &triCenter1x, &triCenter1y, &triCenter1z);
 
    getCircumcenterOfTriangle(Mesh->xic[v1], Mesh->yic[v1], Mesh->zic[v1],
			      Mesh->xic[v2], Mesh->yic[v2], Mesh->zic[v2],
			      Mesh->xic[k2], Mesh->yic[k2], Mesh->zic[k2],
			      &triCenter2x, &triCenter2y, &triCenter2z);
 
 
    /* Compute the signed contribution of area and volume. */
    area1 = areaOf3dTriangle(bisectorx,bisectory,bisectorz,
		       triCenter1x,triCenter1y,triCenter1z,
		       tetCenterx, tetCentery,tetCenterz) ;
 
 
    vol1 =  volumeOfTet(Mesh->xic[v1],Mesh->yic[v1],Mesh->zic[v1],
			   bisectorx,bisectory,bisectorz,
			   triCenter1x,triCenter1y,triCenter1z,
			   tetCenterx, tetCentery,tetCenterz);
 
    if (vol1 < 0.0) {
      area1 = -area1;
    }
 
 
    area2 = areaOf3dTriangle(bisectorx,bisectory,bisectorz,
		     tetCenterx, tetCentery,tetCenterz,
		     triCenter2x,triCenter2y,triCenter2z);
	
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
 
  edgelength = sqrt((Mesh->xic[v1]-Mesh->xic[v2])*
		    (Mesh->xic[v1]-Mesh->xic[v2])+
		    (Mesh->yic[v1]-Mesh->yic[v2])*
		    (Mesh->yic[v1]-Mesh->yic[v2])+
		    (Mesh->zic[v1]-Mesh->zic[v2])*
		    (Mesh->zic[v1]-Mesh->zic[v2]));
 
  value = -area/edgelength;
 
  setEntry(index_i,index_j,volume,&value);
 
}
 
 
/**************************************************************************/
 
void computeentry_(int *Pindex_i, int *Pindex_j, int *PnumIncidentTets,
		    int *incidentTets, int *localEdges)
 
/**************************************************************************/
 
{
  /* This is where we place hooks for the good stuff.. */
  computescalarVoronoientry(*Pindex_i, *Pindex_j, *PnumIncidentTets,
			       incidentTets, localEdges);
 
}
 
 
/**************************************************************************/
 
void computeentry(int *Pindex_i, int *Pindex_j, int *PnumIncidentTets,
		    int *incidentTets, int *localEdges)
 
/**************************************************************************/
 
{
  /* This is where we place hooks for the good stuff.. But for now...*/
  computescalarVoronoientry(*Pindex_i, *Pindex_j, *PnumIncidentTets,
			       incidentTets, localEdges);
 
}
 
 
 
/**************************************************************************/
 
void initialize3ddiffusionmat_(int *pentrysize, int
*pcompress, int *pnnodes, double *xic, double *yic, double *zic,
int *pntets, int *itet)
 
/**************************************************************************/
 
    /*  The input variable names are slightly funny because FORTRAN
	passes by reference.  We dereference below when we populate the
	x3dMesh structure.  */
 
{
  int i,j;  /* loop indices. */
 
  int v1,v2,k1,k2;  /* Refer to tet indices.  Used to index xic,yic,
		       and zic arrays.  (v1,v2) is the edge currently
		       under consideration.  k1 and k2 are the two
		       other vertices on the tet.  Respecting
		       orientation is the KEY! */
 
  double eps = 1e-8;
 
  /* populate mesh structure. */
  Mesh=(x3dMesh*)  malloc(sizeof(x3dMesh));
 
  Mesh->xic     = xic;
  Mesh->yic     = yic;
  Mesh->zic     = zic;
  Mesh->nnodes  = *pnnodes;
  Mesh->ntets   = *pntets;
  Mesh->itet    = itet;
 
  num_area_coefs = *pentrysize;
  createSparseMatrix(Mesh->nnodes,num_area_coefs,*pcompress,eps);
 
}
 
 
/**************************************************************************/
 
void initialize3ddiffusionmat(int *pentrysize, int
*pcompress, int *pnnodes, double *xic, double *yic, double *zic,
int *pntets, int *itet)
 
/**************************************************************************/
 
 
{
  initialize3ddiffusionmat_(pentrysize, pcompress, pnnodes,
				       xic, yic, zic, pntets, itet);
 
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
