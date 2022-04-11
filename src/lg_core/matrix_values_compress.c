/*
  ######################################################################

         $Log: matrix_values_compress.c,v $
         Revision 2.00  2007/11/05 19:46:01  spchu
         Import to CVS

 PVCS
 PVCS       Rev 1.21   02 Oct 2007 12:40:28   spchu
 PVCS    original version

  ######################################################################
*/

/***********************************************************************
                         Copyright, 1996                              
 Parts of this program was prepared by the Regents of the University of        
 California at Los Alamos National Laboratory (the University) under  
 Contract No. W-7405-ENG-36 with the U.S. Department of Energy (DOE). 
 The University has certain rights in the program pursuant to the     
 contract and the program should not be copied or distributed outside 
 your organization. All rights in the program are reserved by the DOE 
 and the University. Neither the U.S. Government nor the University   
 makes any warranty, express or implied, or assumes and liability or  
 responsibility for the use of this software.                         

 Other parts, specifically the programs labeled skiplist.h and sl.c
 are under the GNU copyleft and were created by Dominic Giampaolo while
 he was a student at WPI.  (Hey, if it's a good implementation and
 it's free, USE IT!)  The university cannot copyright that code.

 For more on this copyright see skiplist.h and sl.c in this file

***********************************************************************/

#include <stdlib.h>
#include <stdio.h>
#include <math.h>

/********************************skiplist.h******************************/


/* This header file contains the definitions for use with the generic
 * SkipList package.
 *
 *             -- THIS CODE IS UNDER THE GNU COPYLEFT --
 */

#ifndef SKIPLIST_H
#define SKIPLIST_H


/* RAND_MAX should be defined if you are using an ANSI compiler system,
 * but alas it isn't always.  You should define it to be the correct
 * value for whatever your library rand() function returns.
 *
 * Under unix (mach, bsd, etc), that's 2^31 - 1.  On my Amiga at home 
 * it's 2^15 - 1.  It would be wise to verify what your compiler uses
 * for RAND_MAX (the maximum value returned from rand()) because otherwise
 * the code will _not_ work.
 */
#ifndef RAND_MAX 
#define RAND_MAX (2147483648 - 1)
#endif  

#define MAX(a,b) (a > b ? a : b)


#define ALLOW_DUPLICATES  1   /* allow or disallow duplicates in a list */
#define NO_DUPLICATES	  0
#define DUPLICATE_ITEM	 -1   /* ret val from InsertSL if dups not allowed */

/*  LaGriT assumes that the size of an integer is the same size as a
 *  pointer.  Use the preprocessor and configure settings to select
 *  the integer type so that it matches the size of a pointer.
 */

/**** linux 32 ****/
#ifdef lin
#define FCV_UNDERSCORE
#define SIZEOF_INT 4
#define SIZEOF_LONG 4
#define SIZEOF_VOIDP 4
#endif

/**** linux x64 ****/
#ifdef linx64
#define FCV_UNDERSCORE
#define SIZEOF_INT 4
#define SIZEOF_LONG 8
#define SIZEOF_VOIDP 8
#endif

#if SIZEOF_INT == SIZEOF_VOIDP
#define int_ptrsize int
#elif SIZEOF_LONG == SIZEOF_VOIDP
#define int_ptrsize long
#else
 "Unknown case for size of pointer."
#endif



/* typedef's */
typedef struct SLNodeStruct *SLNode;

struct SLNodeStruct
{
  void	 *key;
  SLNode  forward[1]; /* variable sized array of forward pointers */
};

typedef struct _SkipList
{
  struct SLNodeStruct *header;	   /* pointer to header */

  int_ptrsize  (*compare)();
  void (*freeitem)();

  int_ptrsize flags;
  int_ptrsize level;			   /* max index+1 of the forward array */

} *SkipList;



/* protos */
SkipList   NewSL(int_ptrsize (*compare)(), void (*freeitem)(), int_ptrsize flags);
void	   FreeSL(SkipList l);
int_ptrsize	   InsertSL(SkipList l, void *key);
int_ptrsize	   DeleteSL(SkipList l, void *key);
void	  *SearchSL(SkipList l, void *key);
void	   DoForSL(SkipList  l, int_ptrsize (*function)(), void *arg);

#endif	/* SKIPLIST_H */

/******************************sl.c****************************************/

/* This file contains a heavily hacked and generalized version of the
   Example skiplist code distributed on mimsy.cs.umd.edu.

 Here is a short excerpt from the original comment :

	 Example of Skip List source code for C :

	   Skip Lists are a probabilistic alternative to balanced trees,
	 as described in the June 1990 issue of CACM and were invented by
	 William Pugh in 1987.

 These are my additions :

     This file contains my (Dominic Giampaolo's) heavily hacked version
   of skip lists.  These work on any arbitrary data by using callback
   functions which you supply (at list creation time) to do the data
   comparisons.  You could instantly use this package to implement a
   symbol table for a compiler which would be blazingly fast and
   require zippo effort on your part.

     I've changed the function names (not to protect the innocent, but
   to make them easier to read :) and changed the data structures a bit.
   I've ansi'fied the code, added prototypes, and changed all those ugly
   do/while's to for loops.  I also removed the dependance on those silly
   NIL items at the end of the list (it just checks for regular NULL
   pointers instead).  Additionally, the code is more easily reentrant now,
   and doesn't depend on any global variables.  The code is quite a bit
   different looking than it originally was, but the underlying algorithims
   (of course) remain unchanged.

              -- THIS CODE IS UNDER THE GNU COPYLEFT --


      Dominic Giampaolo (nick@maxine.wpi.edu)
      Aug, 1991
*/
#include <stdio.h>
#include <stdlib.h>
/* #include "skiplist.h"*/

/* define's */
#ifndef TRUE
#define TRUE  1
#endif
#ifndef FALSE
#define FALSE 0
#endif

#define MaxNumberOfLevels 20
#define MaxLevel (MaxNumberOfLevels-1)
#define NewNodeOfLevel(x) (SLNode)malloc(sizeof(struct SLNodeStruct)+(x)*sizeof(SLNode *))


/* private proto */
static int_ptrsize RandomLevelSL(SkipList l);


/* functions */
SkipList  NewSL(int_ptrsize (*compare)(), void (*freeitem)(), int_ptrsize flags)
{
  SkipList l;
  int_ptrsize i;

  if (compare == NULL)    /* need at least a compare function... */
    return NULL;

  l = (SkipList)malloc(sizeof(struct _SkipList));
  if (l == NULL)
    return NULL;

  l->level = 1;
  l->header = NewNodeOfLevel(MaxNumberOfLevels);
  if (l->header == NULL)
    { free(l); return NULL; }

  for(i=0; i < MaxNumberOfLevels; i++)
    l->header->forward[i] = NULL;
  l->header->key = NULL;		 /* just to be sure */

  srand(time(NULL) | 0x01);   /* seed with an odd number */

  l->compare	 = compare;
  l->freeitem	 = freeitem;
  l->flags	 = flags;

  return(l);
}


void FreeSL(SkipList l)
{
  register SLNode p,q;
  void (*freeitem)() = l->freeitem;

  if (l == NULL || l->header == NULL)
    return;

  p = l->header;	   /* free header node first, because it doesn't */
  q = p->forward[0];	   /* have a real key to it			 */
  free(p);
  p = q;

  while (p != NULL)
   {
     q = p->forward[0];
     if (freeitem)
       (*freeitem)(p->key);
     free(p);
     p = q;
   }

  free(l);
}




/*
 *   This RandomLevelSL function generates a very good representation of
 *   p=.25 (or p=.5, etc).  The number of nodes of each level works out
 *   to be very very close to what they should be.  I didn't check it
 *   statistically,  but on large data sets, I imagine it's +/- 5% of what
 *   it should be.  This P value is good for lists of up to 64K elements.
 *   
 *   For more info about the P value, see the papers by Mr. Pugh (available
 *   in postscript from mimsy.umd.edu).
 */
#define P_50   (RAND_MAX / 2)     /* p value of .50   */
#define P_25   (RAND_MAX / 4)     /* p value of .25   */
#define P_125  (RAND_MAX / 8)     /* p value of .125  */

static int_ptrsize RandomLevelSL(SkipList l)
{
  register int_ptrsize level = 0;

  while(rand() < P_25)
    {
      level++;
    }

  return (level > MaxLevel ? MaxLevel : level);
}


int_ptrsize InsertSL(SkipList l, void *key)
{
  register int_ptrsize i,k;
  SLNode update[MaxNumberOfLevels];
  register SLNode p,q;
  int_ptrsize (*compare)() = l->compare;

  p = l->header;
  for(k = l->level-1; k >= 0; k--)
   {
     while((q = p->forward[k]) && (*compare)(q->key, key) < 0)
	p = q;

     update[k] = p;
   }

  if ((l->flags & ALLOW_DUPLICATES) == FALSE) /* if no duplicates allowed */
   if (q && (*compare)(q->key, key) == 0)     /* item is a duplicate */
     {
       return DUPLICATE_ITEM;
     }


  k = RandomLevelSL(l);
  if (k >= l->level)
    {
      l->level++;
      k = l->level - 1;
      update[k] = l->header;
    }

  q = NewNodeOfLevel(k+1);

  if (q == NULL)
    return FALSE;

  q->key = key;
  for(i=0; i < k; i++)
    q->forward[i] = NULL;

  for(; k >= 0; k--)
    {
      p = update[k];
      q->forward[k] = p->forward[k];
      p->forward[k] = q;
    }

  return TRUE;
}



int_ptrsize DeleteSL(SkipList l, void *key)
{
  register int_ptrsize k,m;
  SLNode update[MaxNumberOfLevels];
  register SLNode p,q;
  int_ptrsize  (*compare)()  = l->compare;
  void (*freeitem)() = l->freeitem;

  p = l->header;

  for(k=l->level-1; k >= 0; k--)
   {
     while((q = p->forward[k]) && (*compare)(q->key, key) < 0)
	p = q;

     update[k] = p;
   }
  q = p->forward[0];

  if (q && (*compare)(q->key, key) == 0)
    {
      m = l->level - 1;
      for(k=0; k < m; k++)
	{
	  p = update[k];
	  if (p == NULL || p->forward[k] != q)
	    break;
	  p->forward[k] = q->forward[k];
	}

      if (freeitem)
	(*freeitem)(q->key);

      free(q);

      m = l->level - 1;
      while(l->header->forward[m] == NULL && m > 0)
	m--;

      l->level = m + 1;
      return TRUE;
    }
  else
    return FALSE;
}



void *SearchSL(SkipList l, void *key)
{
  register int_ptrsize k;
  register SLNode p,q;
  int_ptrsize (*compare)() = l->compare;

  p = l->header;
  for(k=l->level-1; k >= 0; k--)
   {
     while((q = p->forward[k]) && (*compare)(q->key, key) < 0)
	p = q;
   }

  if (q == NULL || (*compare)(q->key, key) != 0)
    return NULL;

  return q->key;
}


void DoForSL(SkipList l, int_ptrsize (*function)(), void *arg)
{
  register SLNode p,q;

  if (l == NULL || l->header == NULL || function == NULL)
    return;

  p = l->header;	   /* skip header node because it isn't a real node */
  p = p->forward[0];

  while (p != NULL)
   {
     q = p->forward[0];
     if ((*function)(p->key, arg) == FALSE)
       break;
     p = q;
   }
}



/*********************matrix_values_compress.c****************************** 

This file contains routines for compressing a sparse matrix.
The Univeristy can copyright this.

***************************************************************************/ 


extern double fabs(double x);
extern double log10(double x);
extern double drand48();
extern double pow(double x, double y);
extern double sqrt(double x);



double epsilon;
int_ptrsize matrixEntrySize;
int_ptrsize entryNumber = 0; 

typedef struct entryStruct {
  double *value;  /* The value of an entry. */

  int_ptrsize entryNum;  /* Suppose that all of the double values in the
		    matrix are represented in a sequential array (as
		    occurs in the .STOR format).  entryNum contains
		    the index of this value in that array.  This gets
		    assigned when the STOR file is dumped. */
} entry;


static SkipList compressList;

typedef struct x3dMatrixInfoStruct {
  double *xmat;
  double *ymat;
  double *zmat;
  double *mat;
} x3dMatrixInfo;

x3dMatrixInfo* MatrixInfo;

double maximum[4];

/*------------------------------------------------------------------------*/

void computeMaximums(double *xic, double *yic, double* zic, 
		     double *mat, int_ptrsize n)

/*------------------------------------------------------------------------*/


{
  int_ptrsize i,j;
  int_ptrsize  nexp[4];

  for (j=0; j<4; j++) {
    maximum[j] = -1;
  }

  for (i=0; i<n; i++) {
    if (fabs(xic[i]) > maximum[0]) {
      maximum[0] = fabs(xic[i]);
    }

    if (fabs(yic[i]) > maximum[1]) {
      maximum[1] = fabs(yic[i]);
    }

    if (fabs(zic[i]) > maximum[2]) {
      maximum[2] = fabs(zic[i]);
    }

    if (fabs(mat[i]) > maximum[3]) {
      maximum[3] = fabs(mat[i]);
    }
  }

}

/************************************************************************/ 

int_ptrsize entryCompare(entry *i, entry *j) 

/************************************************************************/ 

     /* used in the skiplist of compressed values  */

 { 
   int_ptrsize i1,i2;
   int_ptrsize k;

  switch(matrixEntrySize)
    {
    case 1:
      i1=3;i2=4;  /* look at last entry only. */
      break;
    case 3: 
      i1=0; i2=3;
      break;
    case 4: 
      i1=0; i2=4;
      break;
    }


   for (k=i1; k<i2; k++) {
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

void entryFree(entry *i)

/************************************************************************/ 


 { 
   free(i->value);
   free(i);
 }


/************************************************************************/ 

entry *entryCreate(double *v)  

/************************************************************************/ 


{
  int_ptrsize i;
  entry *ec =  (entry*)malloc(sizeof(entry));
  ec->value = (double *)malloc(4*sizeof(double));

  for (i=0; i<4; i++) {
    ec->value[i] = v[i];
  }

  return ec;

}

/************************************************************************/

int_ptrsize assignEntryNum(entry *ec)

/************************************************************************/

{
  entryNumber++;
  ec->entryNum = entryNumber;
  return 1;
}

/************************************************************************/

int_ptrsize populateX3dMatrixInfo(entry *ec)

/************************************************************************/

{
  int_ptrsize k;
  x3dMatrixInfo *mi = MatrixInfo;

  mi->xmat[(ec->entryNum)-1]=ec->value[0];
  mi->ymat[(ec->entryNum)-1]=ec->value[1];
  mi->zmat[(ec->entryNum)-1]=ec->value[2];  
  mi-> mat[(ec->entryNum)-1]=ec->value[3];  

  return 1;
}


/************************************************************************/

void compressmatrixvalues_(int_ptrsize *Ncon, int_ptrsize *Neq, int_ptrsize *MEntrySize,
			   int_ptrsize **MatPointers, double **Xmat, 
			   double  **Ymat, double  **Zmat, double  **Mat,
			   double **CXmat, double **CYmat, double **CZmat, 
			   double **CMat, int_ptrsize *numwrittenentries,
			   double *Epsilon)

/************************************************************************/

     /* Every parameter is a pointer because fortran passes by 
	reference.  We dereference these pointers below. */


{
  int_ptrsize i,j,k, dummyInt1, dummyInt2, deg1, deg2, numvalues;
  int_ptrsize ncon         = (int_ptrsize)(*Ncon);
  int_ptrsize neq          = (int_ptrsize)(*Neq);
  int_ptrsize *matPointers = (int_ptrsize*)(*MatPointers);

  double *xmat = (double*)(*Xmat);
  double *ymat = (double*)(*Ymat);
  double *zmat = (double*)(*Zmat);
  double *mat  = (double*)(*Mat);

  double *doubleEntry;
  entry **compressMat, *ec, temp;
  epsilon = *Epsilon;
  matrixEntrySize  = (int_ptrsize)(*MEntrySize);
  


  /* Compute numvalues,the number of double precision matrix entries offered.*/
  numvalues = 0;
  for (i=0; i< ncon; i++) {
    if (numvalues < matPointers[i]) {
      numvalues = matPointers[i];
    }
  }


  /* compute the maximum */
  computeMaximums(xmat,ymat,zmat,mat,numvalues);



  *numwrittenentries = 0;
  
  /* Establish a new "value pointer matrix" */
  /* 0 is never used!  This is because matPointers points into it
     and matPointers indexes a FORTRAN array!*/
  compressMat = (entry **)malloc((1+ncon)*sizeof(entry*));

  /* create compressList */
  compressList = (SkipList) NewSL(entryCompare, entryFree,NO_DUPLICATES);


  /* put the current floatingPoint matrix into the compress list. */
  for (i=1; i<=ncon; i++) {
    doubleEntry = (double *)malloc(4*sizeof(double));

    /* set the entry.  This is tailored for x3dStuff.  An entry size
       of 2 or 5 or more isn't gonna fly in this routine!!! */
    doubleEntry[0] = xmat[matPointers[i-1]-1];
    doubleEntry[1] = ymat[matPointers[i-1]-1];
    doubleEntry[2] = zmat[matPointers[i-1]-1];  
    doubleEntry[3] =  mat[matPointers[i-1]-1];

    /* decide if the entry exists in the skiplist, by comparing the appropriate
       values. */  
    temp.value = doubleEntry;
    ec = SearchSL(compressList,&temp);
      
    if (ec) {      /* it's in there */
      compressMat[i]=ec;
      free(doubleEntry);
    } else {  /* it's not there, so add it. */
      compressMat[i] = entryCreate(doubleEntry);
      InsertSL(compressList,compressMat[i]);
      *numwrittenentries= *numwrittenentries + 1;
      /*      printf("Inserting %lf\n",compressMat[i]->value[3]);*/
    }
  }

  /* assign numbers to entries */
  DoForSL(compressList, assignEntryNum,NULL);

  numvalues = entryNumber;

  /* create "value pointer matrix" */
  for (i=0; i<ncon; i++) {
    if (matPointers[i]!=0) {
      /*
      printf("Before matptris[%d] = %d compares with
              %lf \n",i,matPointers[i],compressMat[matPointers[i]]->value[3]);
	      */
      matPointers[i] = (compressMat[i+1])->entryNum;
      /*
      printf("Value %d is %d which compares with %lf \n",i,matPointers[i],
	     mat[i]);
	     */
    }
  }


  /* populate the X3dMatrixInfo. */
  MatrixInfo = (x3dMatrixInfo *)malloc(sizeof(x3dMatrixInfo)); 
  MatrixInfo->xmat = (double *)malloc(sizeof(double)*numvalues);
  MatrixInfo->ymat = (double *)malloc(sizeof(double)*numvalues);
  MatrixInfo->zmat = (double *)malloc(sizeof(double)*numvalues);
  MatrixInfo->mat  = (double *)malloc(sizeof(double)*numvalues);

  DoForSL(compressList,populateX3dMatrixInfo,NULL);

  *CXmat = MatrixInfo->xmat;
  *CYmat = MatrixInfo->ymat;
  *CZmat = MatrixInfo->zmat;
  *CMat  = MatrixInfo->mat;

}



/************************************************************************/

void compressmatrixvalues(int_ptrsize *Ncon, int_ptrsize *Neq, int_ptrsize *MEntrySize,
			   int_ptrsize **MatPointers, double **Xmat, 
			   double  **Ymat, double  **Zmat, double  **Mat,
			   double **CXmat, double **CYmat, double **CZmat, 
			   double **CMat, int_ptrsize *numwrittenentries,
			   double *Epsilon)

/************************************************************************/

{

compressmatrixvalues_(Ncon, Neq, MEntrySize,MatPointers,Xmat, 
		      Ymat, Zmat,Mat,CXmat, CYmat, CZmat, 
	   	      CMat, numwrittenentries,Epsilon);


}


/************************************************************************/

void freecompressedvaluesarrays_()

/************************************************************************/

     /* Deallocates MatrixInfo structure*/

{
  free(MatrixInfo->xmat);
  free(MatrixInfo->ymat);
  free(MatrixInfo->zmat);
  free(MatrixInfo->mat );
}

/************************************************************************/

void freecompressedvaluesarrays()

/************************************************************************/

     /* Deallocates MatrixInfo structure*/

{
  free(MatrixInfo->xmat);
  free(MatrixInfo->ymat);
  free(MatrixInfo->zmat);
  free(MatrixInfo->mat );
}
