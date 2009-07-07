#include <stdio.h>
#include <math.h>
#include "skiplist.ch"
 
extern double fabs();
 
/************************************************************************
Purpose:  This .c file implements the symmetric sparse matrix abstract data
type, with a compression option.  It contains routines that will
create the proper output arrays for a FEHM .stor file.  [See "The FEHM
.stor format" in matbld3d.f for a description.}
 
This implementation uses skiplists.  A skiplist is an efficient data
structure for maintaining a sorted linked list of elements.  With high
probablity an insertion of, a deletion of, and a search for an item in
a skiplist takes O(log n) time.  This is a striking improvement over
the naive implementation which takes O(n) time for each operation.
(Consider n=1,000,000).  The skiplist implementation is currently in
matrix_values_compress.c.
 
The sparse matrix is an array of skiplists from 1..n.  Each skiplist
corresponds to a row in the matrix.  The index of an element is its
column.
 
This routine is used in conjunction with anothermatbld3d.c and
anothermatbld3d_wrapper.f.
 
 
$Log: sparseMatrix.c,v $
Revision 2.00  2007/11/05 19:46:03  spchu
Import to CVS

*PVCS    
*PVCS       Rev 1.7   10 Jan 2007 16:21:10   tam
*PVCS    added missing underscore to called routines
*PVCS    getoccupiedcolumns_ getmatrixpointers_ which caused
*PVCS    a segmentation fault on the mac platform
*PVCS    
*PVCS       Rev 1.6   19 Feb 2001 13:18:20   dcg
*PVCS    remove embedded underscores in procedure names - alpha compiler
*PVCS    wants to have double underscore after the names - this causes
*PVCS    incompatibility among platforms
*PVCS
*PVCS       Rev 1.5   Tue Nov 16 11:59:44 1999   murphy
*PVCS    Changed routines to add more information output.
*PVCS
*PVCS       Rev 1.4   Mon Sep 20 15:02:16 1999   murphy
*PVCS    Took out annoying printf statements.
*PVCS
*PVCS       Rev 1.3   Tue Aug 17 11:47:08 1999   jan
*PVCS    changed 1084 void free_neg_coeffs() to 1084 void freenegcoefs()
*PVCS
*PVCS       Rev 1.2   Fri Jul 16 14:08:30 1999   murphy
*PVCS    Added code to report "negative" coefficients.  Also, added code to not create
*PVCS    off-diagonal elements of zero-value.
*PVCS>
*PVCS
*PVCS       Rev 1.1   Fri Jun 11 12:26:20 1999   murphy
*PVCS    Use "skiplist.ch" rather than "skiplist.h"
*PVCS
*PVCS       Rev 1.0   Tue May 18 14:46:12 1999   murphy
*PVCS    Initial revision.
 
*************************************************************************/
 
 
/* Variables for internal representation. */
 
static SkipList *sparseMatrix;
 
/* Anything (function, variable, etc.) involving "entry" (in
   lowercase) is internal to this module.*/
 
/* Variables for the .stor format. */
static int neq;  /* Dimension of the sparse matrix.  Also known as
		    number of equations, number of rows, number of
		    columns, number of points, or simply n. */
 
static int ncon;  /* Number of connections in the grid.  (Number of
		     non-zero entries in the matrix.)  */
 
static int *ncon_row;  /* An array of size neq containing the total
			  number of connections in a given row of a
			  matrix.  */
 
static int ncon_max = 0;  /* Maximum number of connections to a single
			     node.  I.e., the maximum degree of a
			     vertex. */
 
static double *voronoiVolume;  /* an array of 1..eq containing the
				  volume of the Voronoi volume of each
				  node. */
 
static int matrixEntrySize;   /* num_area_coeff */
 
static int *occupiedColumns;  /* an array containing the numbers of
				 the occupied columns.  Used to
				 interface with the FEHM .stor
				 format. */
 
/* Compression option variables. */
 
static int compressionEnabled;   /* a boolean flag indicating the selection
				    of the compression option. */
 
static double epsilon;   /* User supplied value for defining the
			    tolerance of equality when comparing two
			    double numbers.  Its use in this module is
			    explained below. */
 
 
static SkipList compressList;  /* If the compression option is chosen,
				  the compressList contains the value
				  of each component of each entry in
				  sorted order.  When an entry is
				  created or modified in the sparse
				  matrix data structure, this list is
				  searched for anything resembling the
				  entry (i.e., within the user
				  supplied epsilon.)  If so, we do not
				  create another entryComponent for
				  this value.  Rather, we place a
				  pointer to the already existing
				  component in the entry.*/
 
 
/* The Entry data structure. */
 
typedef struct entryComponentStruct {
  double *value;  /* The value of a component of an entry. */
 
  int entryNum;  /* Suppose that all of the double values in the
		    matrix are represented in a sequential array (as
		    occurs in the .stor format).  entryNum contains
		    the index of this value in that array.  This gets
		    assigned when the .stor file is dumped. */
 
  int refCount;  /* Counts the number of entries with references to
		    this structure (i.e., the number of matrix entries
		    with this value.)  This quantity is only used when
		    the compression option is selected.  The idea is
		    to delete this record if only one entry points to
		    it when the value of that entry is modified.  If
		    other entries share this value, then upon change
		    of an entry that points to it, we decrement the
		    reference count and insert the new value of the
		    entry under modification into the list of values
		    (again checking to see if the new value is already
		    represented).  */
} entryComponent;
 
 
typedef struct entryKeyStruct {
  int column;        /* column number.  The row number is assumed known. */
 
  entryComponent *info;  /* A pointer to the info record. */
 
} entryKey;
 
 
static int entryNumber;  /* global variable used to assign entry
			    numbers to component entries.  Needed
			    because of the DoForSL() callback. */
 
static int *entryNumbers;      /* array for FEHM output. */
static int *diagonalIndices;   /* array for FEHM output. */
static double *MatrixValues;  /* an array for interfacing with FORTRAN */
 
 
static int columnCounter;     /* global variable used to populate an
				 array for FEHM output.  Global because
				 I use callbacks.  */
 
static int entryCounter;  /* global variable used to populate an array
			     for FEHM output.  Global because I use
			     callbacks.  */
 
static double maximum[4];     /* an array containing the maximum of
				 each entry. */
 
static double rowsum[4];      /* an array containing the component sum
				  of each row.*/
 
static int num_written_coefs; /* number of unique matrix values in the
				  matrix */
 
static int component_of_interest;  /* another bloody global variable
				      needed because I use a main-memory
				      efficient algorithm. */
 
/* variables used in reporting the of 'negative' coeffs
   (i.e., positive off-diagonal coeffs).*/
static int num_neg_coefs;
static int num_suspect_coefs;
static int num_zero_coefs;
static int *row_neg_coefs;
static int *col_neg_coefs;
static double *neg_coefs;
 
 
/************************************************************************/
 
int zeroVector(double *value)
 
/************************************************************************/
     /* Compares the key (column number) on two entries. */
     /* Used for skiplist implementation. */
 
{
  int k;
 
  for (k=0; k<matrixEntrySize; k++) {
    if (fabs((value[k])) > (maximum[k]*epsilon)) {
      /*      printf("Value[k] = %e, maximum[k] = %e   epsilon = %e\n",fabs(value[k]),
			       maximum[k],epsilon); */
      return 0;
    }  /* not zero-vector */
  }
  return 1;   /* zero-vector */
}
 
/************************************************************************/
 
int entryKeyCompare(entryKey *i, entryKey *j)
 
/************************************************************************/
     /* Compares the key (column number) on two entries. */
     /* Used for skiplist implementation. */
 
{
  if (i->column == j->column) {
    return 0;
  } else {
    if (i->column < j->column) {
      return -1;
    } else {
      return 1;
    }
  }
}
 
 
/************************************************************************/
 
entryComponent *entryComponentCreate (double *v, int count)
				
/************************************************************************/
 
{
  int i;
 
  entryComponent *ec = (entryComponent *)(malloc(sizeof(entryComponent)));
  ec->value = (double *)malloc(matrixEntrySize*sizeof(double));
 
  for (i=0; i<matrixEntrySize; i++) {
    ec->value[i] = v[i];
  }
 
  ec->refCount = count;
  return(ec);
}
 
 
/************************************************************************/
 
void entryComponentFree (entryComponent *ec)
 
/************************************************************************/
 
{
 
  /* do nothing! */
  free(ec->value);
  free(ec);
}
 
 
/************************************************************************/
 
void entryKeyFree(entryKey *ek)
 
/************************************************************************/
 
/* Frees a key. */
 
{
  /* Do nothing. */
  if(!compressionEnabled) {
    if (ek->info->value != 0) {
      free(ek->info->value);
      ek->info->value = 0;
    } else {
      free(ek->info);
    }
  }
  free(ek);
}
 
 
/************************************************************************/
 
int entryComponentCompare (entryComponent *i, entryComponent *j)
 
/************************************************************************/
 
     /* used in the skiplist of compressed values */
 
{
  int k;
 
  for (k=0; k<matrixEntrySize; k++) {
    if (fabs((i->value[k] - j->value[k])) > (maximum[k]*epsilon)) {
      if (i->value[k] < j->value[k]) {
	 return -1;
      } else {
	return 1;
      }
    }
  }
  return 0;   /* equal */
}
 
 
/************************************************************************/
 
entryComponent *entryKeyCreateInfo(double *value)
 
/************************************************************************/
 
{
  int i;
 
  entryComponent* ec;
  entryComponent temp;

  if (compressionEnabled) {
    temp.value = value;
    ec = SearchSL(compressList,&temp);
    if (ec) {
      (ec->refCount)++;
    } else {
      ec = entryComponentCreate(value,1);
      InsertSL(compressList,ec);
    }
  } else {
    ec = entryComponentCreate(value,1);
  }
  return(ec);
}
 
 
/*!!!!!!!!!!!!!!!!!!!!!!!!!!!! Callbacks !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*/
 
/************************************************************************/
 
int sumRow(entryKey *ek)
 
/************************************************************************/
 
     /* used to set diagonal elements. */
 
{
  int j;
 
  for (j=0; j<matrixEntrySize; j++) {
    rowsum[j] += ek->info->value[j];
  }
  return 1;
}
 
 
/************************************************************************/
 
int printRow(entryKey *ek, char *rowc)
 
/************************************************************************/
 
{
  int j;
  printf("\t Row=%d, Column=%d, Value = %e\n", (int)rowc,
	 ek->column,ek->info->value[0]);
  return 1;
}
 
 
 
/************************************************************************/
 
int getColumnNumber(entryKey *ek, char *rowc)
 
/************************************************************************/
 
{
  int j;
 
  occupiedColumns[columnCounter] =  ek->column;
  if ((int)rowc == ek->column) {
    diagonalIndices[((int)rowc)-1] = columnCounter;
  }
 
  columnCounter++;
  return 1;
}
 
 
 
/************************************************************************/
 
int getEntryNumbers(entryKey *ek, char *rowc)
 
/************************************************************************/
 
 
{
  int j;
 
  entryNumbers[entryCounter] =  ek->info->entryNum;
  entryCounter++;
  return 1;
}
 
 
/************************************************************************/
 
int assignEntryNumCompression(entryComponent *ec)
 
/************************************************************************/
 
{
 
    ec->entryNum = entryNumber;
    entryNumber++;
    return 1;
}
 
 
/************************************************************************/
 
int assignEntryNumNoCompression(entryKey *ek, char *rowc)
 
/************************************************************************/
 
{
  int i;
  int row;
 
  row = (int) rowc;
  if (ek->column >= row) {
    ek->info->entryNum = entryNumber;
    entryNumber++;
  }
  return 1;
}
 
 
/************************************************************************/
 
int populateCompressedValuesArray(entryComponent *ec)
 
/************************************************************************/
 
{
 
  MatrixValues[entryCounter] = ec->value[component_of_interest];
  entryCounter++;
  return 1;
}
 
 
/************************************************************************/
 
int populateUncompressedValuesArray(entryKey *ek, char *rowc)
 
/************************************************************************/
 
{
  /*  printf("Value is %e\n",ek->info->value[component_of_interest]); */
 
  if (ek->column >= (int) rowc) {
    MatrixValues[entryCounter] = ek->info->value[component_of_interest];
    entryCounter++;
  }
  return 1;
}
 
 
/************************************************************************/
 
int countNegCoeffs(entryKey *ek, char *rowc)
 
/************************************************************************/
 
     /*
        counts the following quantities:
            a) total number of "negative" coefficients (num_neg_coefs)
	    b) total number of "significant negative coefficents"  (i.e,
	       non-zero.)  (num_suspect_coefs)
	    c) total number of "zero" coefficents.
               (negative or not) num_zero_coefs
 
         Global variables involved:
	       static int num_neg_coefs;
	       static int num_suspect_coefs;
	       static int num_zero_coefs;
      */
 
{
  if ((int)rowc < ek->column) {
    /* Test the value to see if it is "positive."  (positive is negative
       in this crazy world of geoanalysis).  */
 
    if (fabs((ek->info->value[component_of_interest])) >
             (maximum[component_of_interest]*epsilon)) {
 
      if ((ek->info->value[component_of_interest]) > 0.0) {
	/*
          printf("Row %d Column %d Value %lf\n",(int)rowc,ek->column,
		 ek->info->value[component_of_interest]);
		 */
	  num_suspect_coefs++;
	  num_neg_coefs++;
      }
    } else {
      num_zero_coefs++;
 
      if ((ek->info->value[component_of_interest]) > 0.0) {
	  num_neg_coefs++;
      }
    }
  }
  return 1;
}
 
 
/************************************************************************/
 
int retrieveNegCoeffs(entryKey *ek, char *rowc)
 
/************************************************************************/
 
{
 
  if ((int)rowc < ek->column) {
    /* Test the value to see if it is "positive" and "significantly
       positive" (positive is negative in this crazy world of
       geoanalysis).  */
 
    if (fabs((ek->info->value[component_of_interest])) >
             (maximum[component_of_interest]*epsilon)) {
 
 
      if ((ek->info->value[component_of_interest]) > 0.0) {
	row_neg_coefs[num_suspect_coefs] = (int)rowc;
	col_neg_coefs[num_suspect_coefs] = ek->column;
	neg_coefs[num_suspect_coefs]  =
	  -ek->info->value[component_of_interest];
	num_suspect_coefs++;
      }
    }
  }
  return 1;
}
 
 
 
/***************************USER CALLABLE ROUTINES***********************/
 
/************************************************************************/
 
void createSparseMatrix(int numberOfEquations, int sparseMatrixEntrySize,
			int Compression, double Epsilon)
 
/************************************************************************/
 
 
{
  /* this routine is ok */
  int i,j;
 
  compressionEnabled = 0;
  compressList = NULL;
  voronoiVolume = NULL;

  if (sparseMatrixEntrySize < 1) {
    printf("createSparseMatrix Error: Matrix Entry Data Size must be >= 1\n");
  } else {
    matrixEntrySize = sparseMatrixEntrySize;
  }
 
  neq=numberOfEquations;
  ncon_row =  (int *)malloc((1+neq)*sizeof(int));
  voronoiVolume = (double *)malloc((1+neq)*sizeof(double));
 
  sparseMatrix = (SkipList *) malloc((neq+1)*sizeof(SkipList));
 
  for (i=1; i<=neq; i++) {
    sparseMatrix[i] = (SkipList) NewSL(entryKeyCompare,
				       entryKeyFree,NO_DUPLICATES);
 
    ncon_row[i]=0;
    voronoiVolume[i] = 0.0;
  }
 
 
  for (i=0; i<4; i++) {
    maximum[i] = +1e-30;
  }
 
  epsilon = Epsilon;
 
  if(Compression) {
    compressionEnabled = 1;
    compressList = (SkipList) NewSL(entryComponentCompare,
				    entryComponentFree,NO_DUPLICATES);
  }
}
 
 
/************************************************************************/
 
int entryExists(int index_i, int index_j)
 
/************************************************************************/
 
     /* returns TRUE if i,j exists, FALSE otherwise. */
     /* Assumes matrix has been initialized */
{
  /* Search for the entry first.  If it is there, update it.
     Otherwise, create a new entry.  */
 
 
  entryKey  searchEntry;  /* used for searching Skiplist */
  entryKey *entryMat;     /* pointer to entry in the skiplist.
			     NULL if not there . */
 
 
  searchEntry.column = index_i;
  entryMat = SearchSL(sparseMatrix[index_j],&searchEntry);
 
  if (entryMat) {
    return 1;
  } else {
    return 0;
  }
}
 
 
/************************************************************************/
 
void setEntry(int index_i, int index_j,  double volContrib, double *value)
 
/************************************************************************/
 
     /* Assumes matrix has been initialized */
 
{
  /* Search for the entry first.  If it is there, update it.
     Otherwise, create a new entry.  */
 
  int i;
 
  entryKey  searchEntry;  /* used for searching Skiplist */
  entryKey *entryMat;     /* pointer to entry in the skiplist.
			     NULL if not there . */
 
  entryKey *entryMat2;    /* pointer to other entry (j,i) in matrix */
 
  entryComponent *ec,       /* makes dereferincing easier. */
                 *newec;
 
 
  /* Update the Voronoi volumes. */
  voronoiVolume[index_i-1] += volContrib;
  voronoiVolume[index_j-1] += volContrib;
 
  /* Update the maximum values. */
  for (i=0; i<matrixEntrySize; i++) {
    if (fabs(value[i]) > maximum[i]) {
      maximum[i] = fabs(value[i]);
    }
  }
 
  searchEntry.column = index_i;
  entryMat = SearchSL(sparseMatrix[index_j],&searchEntry);
 
  if (entryMat) {
    /* update the value of the entry.  ec.*/
    entryMat2 = SearchSL(sparseMatrix[index_i],&searchEntry);
 
    ec=entryMat->info;
 
    if (compressionEnabled) {
      if (ec->refCount == 1) {  /* this is the only entry pointing to it. */
	DeleteSL(compressList, ec);
      } else {
	(ec->refCount)--;
      }
    } else {
      free(entryMat->info->value);
      free(entryMat->info);
    }
    newec = entryComponentCreate(value,1);
    entryMat->info = newec;
    entryMat2->info = newec;
  } else {
    /* update the number of connections per row. */
     if (index_i != index_j) {
       /* is it the "zero-vector"?  If so, don't bother adding it.  */
       if (!zeroVector(value)) {
	 ncon_row[index_i]++;
	 ncon_row[index_j]++;
 
	 /* Create two new entries (i,j) and (j,i) and insert them into the
	    matrix. */
	 entryMat = (entryKey *)malloc(sizeof(entryKey));
	 entryMat->column = index_i;
	 entryMat->info = entryKeyCreateInfo(value);
	 InsertSL(sparseMatrix[index_j],entryMat);
	
	 entryMat2 = (entryKey *)malloc(sizeof(entryKey));
	 entryMat2->column = index_j;
	 entryMat2->info = entryMat->info;
	 InsertSL(sparseMatrix[index_i],entryMat2);
       }
     } else {
       /* Create just one entry because we are on a diagonal. */
       ncon_row[index_i]++;
       entryMat = (entryKey *)malloc(sizeof(entryKey));
       entryMat->column = index_i;
       entryMat->info = entryKeyCreateInfo(value);
       InsertSL(sparseMatrix[index_j],entryMat);
     }
  }
}
 
/************************************************************************/
 
void setDiagonalEntries()
 
/************************************************************************/
 
{
  int i,j;
 
  for (i=1; i<=neq; i++) {
    for (j=0; j<matrixEntrySize; j++) {
      rowsum[j] = 0.0;
    }
    /*  We don't need to compute the diagonals!
    DoForSL(sparseMatrix[i], sumRow, NULL);
    for (j=0; j<matrixEntrySize; j++) {
      rowsum[j] = -rowsum[j];
    }
    */
    setEntry(i, i, 0.0, rowsum);
    /*    DoForSL(sparseMatrix[i], printRow, (char *)i); */
 
  }
}
 
 
 
/*********************FORTRAN USER CALLABLE ROUTINES ********************/
 
/*These are used to get the information from the SparseMatrix */
/*There must be routine names with and without the appended underscore */
 
/************************************************************************/
 
void getmatrixsizes_(int *Pnum_written_coefs, int *ncoefs, int *ncon_max)
 
/************************************************************************/
 
{
   int i, accumulatedDegree;

   /* compute ncon and ncon_max */
   ncon=0;
   *ncon_max = 0;
   for (i=1; i<=neq; i++) {
     if (*ncon_max < ncon_row[i]) {
       *ncon_max = ncon_row[i];
     }
     ncon+=ncon_row[i];
   }
 
   *ncoefs = ncon;
 
   /* Assign the entry numbers. */
   /* In this way, we compute num_written_coefs */

   entryNumber=1;
   if (compressionEnabled) {

     DoForSL(compressList,assignEntryNumCompression,NULL);
   } else {

     for (i=1; i<=neq; i++) {
       DoForSL(sparseMatrix[i],assignEntryNumNoCompression,(char *)i);
     }
   }
 
   num_written_coefs = entryNumber - 1;
   *Pnum_written_coefs = num_written_coefs;
}
 
 
/************************************************************************/
 
void getmatrixsizes(int *Pnum_written_coefs, int *ncoefs, int *ncon_max)
 
/************************************************************************/
 
{
  getmatrixsizes_(Pnum_written_coefs,  ncoefs,  ncon_max);
}
 
 
/************************************************************************/
 
void getvoronoivolumes_(double **volic)
 
/************************************************************************/
 
{
  int i;
 
  *volic = voronoiVolume;
}
 
 
/************************************************************************/
 
void getvoronoivolumes(double **volic)
 
/************************************************************************/
 
{
  int i;
 
  *volic = voronoiVolume;
}
 
 
 
/************************************************************************/
 
void freevoronoivolumes_()
 
/************************************************************************/
 
{
  free(voronoiVolume);
  voronoiVolume = NULL;
}
 
/************************************************************************/
 
void freevoronoivolumes()
 
/************************************************************************/
 
{
  free(voronoiVolume);
  voronoiVolume = NULL;
}
 
 
/************************************************************************/
 
void getentriesperrow_(int **epr)
 
/************************************************************************/
 
{
  int i;
  /* put it in Funky-George Format */
  ncon_row[0] = neq+1;
  for (i=1; i<=neq; i++) {
    ncon_row[i] = ncon_row[i] + ncon_row[i-1];
  }
 
  /* return it */
  *epr = ncon_row;
}
 
/************************************************************************/
 
void getentriesperrow(int **epr)
 
/************************************************************************/
 
{
  int i;
  /* put it in Funky-George Format */
  ncon_row[0] = neq+1;
  for (i=1; i<=neq; i++) {
    ncon_row[i] = ncon_row[i] + ncon_row[i-1];
  }
 
  /* return it */
  *epr = ncon_row;
}
 
 
/************************************************************************/
 
void freeentriesperrow_()
 
/************************************************************************/
 
{
  free (ncon_row);
  ncon_row = NULL;
}
 
/************************************************************************/
 
void freeentriesperrow()
 
/************************************************************************/
 
{
  free (ncon_row);
  ncon_row = NULL;
}
 
 
/************************************************************************/
 
void getoccupiedcolumns_(int **columns)
 
/************************************************************************/
 
{
  int i;
 
  columnCounter = 0;
  occupiedColumns = (int*)malloc(ncon*sizeof(int));
  diagonalIndices = (int*)malloc(neq*sizeof(int));
 
  for (i=1; i<=neq; i++) {
    DoForSL(sparseMatrix[i], getColumnNumber ,(char *)i);
  }
 
  *columns = occupiedColumns;
}
 
 
/************************************************************************/
 
void getoccupiedcolumns(int **columns)
 
/************************************************************************/
 
{
  getoccupiedcolumns_(columns);
}
 
 
/************************************************************************/
 
void freeoccupiedcolumns_()
 
/************************************************************************/
 
{
  free(occupiedColumns);
  occupiedColumns = NULL;
}
 
/************************************************************************/
 
void freeoccupiedcolumns()
 
/************************************************************************/
 
{
  free(occupiedColumns);
  occupiedColumns = NULL;
}
 
 
/************************************************************************/
 
void getmatrixpointers_(int **MatPointers, int **diagonals)
 
/************************************************************************/
 
{
  int i;
  /* Get the entry numbers into matPointers */
  entryNumbers = (int *)malloc(ncon*sizeof(int));
  entryCounter = 0;
  for (i=1; i<=neq; i++) {
    DoForSL(sparseMatrix[i],getEntryNumbers,(char *)i);
  }
 
  *MatPointers = entryNumbers;
  *diagonals = diagonalIndices;
}
 
 
 
/************************************************************************/
 
void getmatrixpointers(int **MatPointers, int **diagonals)
 
/************************************************************************/
 
{
  getmatrixpointers_(MatPointers, diagonals);
}
 
 
/************************************************************************/
 
void freematrixpointers_()
 
/************************************************************************/
 
{
  free (entryNumbers);
  free (diagonalIndices);
  entryNumbers = NULL;
  diagonalIndices = NULL;
}
 
/************************************************************************/
 
void freematrixpointers()
 
/************************************************************************/
 
{
  free (entryNumbers);
  free (diagonalIndices);
  entryNumbers = NULL;
  diagonalIndices = NULL;
}
 
 
/************************************************************************/
 
void getcomponentmatrixvalues_(int *component, double **values)
 
/************************************************************************/
 
     /* This is a real pisser because FORTRAN is column-major and C is
	row-major. */
 
{
  int i;
 
  /* Get the entry numbers into MatrixValues */
  MatrixValues = (double*) malloc(num_written_coefs*sizeof(double));
  component_of_interest = *component;

  if(compressionEnabled) {
    entryCounter = 0;
    DoForSL(compressList,populateCompressedValuesArray,NULL);
  } else {
    entryCounter = 0;
    for (i=1; i<=neq; i++) {
      DoForSL(sparseMatrix[i],populateUncompressedValuesArray,(char *)i);
    }
  }
 
 
  *values=MatrixValues;
}
 
 
 
/************************************************************************/
 
void getcomponentmatrixvalues(int *component, double **values)
 
/************************************************************************/
 
{
  getcomponentmatrixvalues_(component, values);
}
 
 
/***************************************************************************/
 
void extractnegativecoefs(int *component, int *numnegs, int *numsuspectnegs,
 			    int *numzeronegs, int **negrows, int **negcols,
			    double **negs)
 
/***************************************************************************/
 
     /* Helpful in identifying the bane of Carl's existence. */
 
{
  int i;
  component_of_interest = *component;
 
  num_neg_coefs = 0;
  num_suspect_coefs = 0;
  num_zero_coefs = 0;
 
 
  for (i=1; i<=neq; i++) {
    DoForSL(sparseMatrix[i],countNegCoeffs,(char *)i);
  }
 
   /* allocate space. */
  row_neg_coefs = (int *)malloc(num_suspect_coefs*sizeof(int));
  col_neg_coefs = (int *)malloc(num_suspect_coefs*sizeof(int));
  neg_coefs     = (double *)malloc(num_suspect_coefs*sizeof(double));
 
  /* assign fortran pointers */
  *numnegs = num_neg_coefs;
  *numsuspectnegs = num_suspect_coefs;
  *numzeronegs = num_zero_coefs;
 
  *negrows = row_neg_coefs;
  *negcols = col_neg_coefs;
  *negs    = neg_coefs;
 
  num_suspect_coefs = 0;  /* practicing bad programming by using this
			     as counter*/
  for (i=1; i<=neq; i++) {
    DoForSL(sparseMatrix[i],retrieveNegCoeffs,(char *)i);
  }
}
 
 
/***************************************************************************/
 
void extractnegativecoefs_(int *component, int *numnegs, int *numsuspectnegs,
 			    int *numzeronegs, int **negrows, int **negcols,
			    double **negs)
 
/***************************************************************************/
 
{
  extractnegativecoefs(component,numnegs,numsuspectnegs,numzeronegs,
			 negrows,negcols,negs);
}
 
 
/***************************************************************************/
 
void freenegcoefs_()
 
/***************************************************************************/
 
{
  free(row_neg_coefs);
  free(col_neg_coefs);
  free(neg_coefs);
  row_neg_coefs = NULL;
  col_neg_coefs = NULL;
  neg_coefs = NULL;
}
 
/***************************************************************************/
 
void freenegcoefs()
 
/***************************************************************************/
 
{
  free(row_neg_coefs);
  free(col_neg_coefs);
  free(neg_coefs);
  row_neg_coefs = NULL;
  col_neg_coefs = NULL;
  neg_coefs = NULL;
}
 
 
/************************************************************************/
 
void freematrixvalues_()
 
/************************************************************************/
 
{
  free(MatrixValues);
  MatrixValues = NULL;
}
 
/************************************************************************/
 
void freematrixvalues()
 
/************************************************************************/
 
{
  free(MatrixValues);
  MatrixValues = NULL;
}
 
/************************************************************************/
 
void killsparsematrix_()
 
/************************************************************************/
 
{
  /* deallocate the entire matrix! */
 
  int i;
 
  for (i=1; i<=neq; i++) {
    FreeSL(sparseMatrix[i]);
  }

  if (compressionEnabled) {
    if (compressList){
      FreeSL(compressList);
    } else {
      printf("killsparsematrix: compressList expected but does not exist. No action taken.\n"); }
  }
  free(sparseMatrix);
  sparseMatrix = NULL;
  compressList = NULL;

  if(compressList){
    printf("Warning: killsparsematrix: compressList not free: %d\n",compressList); }
  if (sparseMatrix){
    printf("Warning: killsparsematrix: sparseMatrix not free: %d\n",sparseMatrix[1]); }
}
 
/************************************************************************/
 
void killsparsematrix()
 
/************************************************************************/
 
{
  killsparsematrix_();
}
 
 
