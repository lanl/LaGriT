/*  Modified for 64 bit version
 *  add opsys.h for preprocess definitions
 *
 *  LaGriT assumes that the size of an integer is the same size as a
 *  pointer.  Use the preprocessor and compile settings to select
 *  the integer type so that it matches the size of a pointer.
 *
 *  New wrappers for memory allocation:
 *
 *   void* util_malloc(int_ptrsize* nwords)
 *   void* util_realloc(void** iptr, int_ptrsize* nwords)
 *   int_ptrsize util_free(void** iptr)
 *   int_ptrsize util_sum_addr(int_ptrsize* a, int_ptrsize* b)
 *
 *  New routines useful for checking platform dependent sizes
 *   int util_check_ptrsize(void)
 *   void util_print_sizes(void)
 *   int util_test_malloc(void)
 *
 * Note that malloc will use the argument as unsigned int
 *  max unsigned int 2^32 =              4,294,967,295 
 *  max unsigned int 2^64 = 18,446,744,073,709,551,615 
 *  the %d for printf will print the unsigned int value
 */ 

#include "opsys.h"

/*
  ######################################################################
 
         $Log: mmsc.c,v $
         Revision 2.00  2007/11/03 00:49:12  spchu
         Import to CVS

 PVCS
 PVCS       Rev 1.21   02 Oct 2007 12:40:28   spchu
 PVCS    original version
 
  ######################################################################
*/


#include <stdlib.h>
#include <stdio.h>

/* ----------------------------------------------------------------------
 * util_malloc: Wrapper for C malloc().
 */
void* util_malloc(int_ptrsize* nwords)
{ /* util_malloc */
  void* iptout = NULL;
  int_ptrsize nbytes = 0;
 
  nbytes = (*nwords);

  if(!(iptout= malloc(nbytes))) {
    printf("util_malloc: Out of memory, malloc return: %p \n",iptout);
    printf("   Requested value: %f = %4d bit unsigned int \n",(float)nbytes,sizeof(nbytes));
    return NULL;

  } /* if */
 
  return iptout;
} /* util_malloc */

void* util_malloc_(int_ptrsize* nwords)
{ /* util_malloc_ */
  void* iptout = NULL;
  int_ptrsize nbytes = 0;
  
  nbytes = (*nwords);

  if(!(iptout= malloc(nbytes))) {
    printf("util_malloc_: Out of memory, malloc return: %p \n",iptout);
    printf("   Requested value: %f = %4d bit unsigned int \n",(float)nbytes,sizeof(nbytes));
    return NULL;
      
  } /* if */ 
            
  return iptout;
} /* util_malloc_ */

void* UTIL_MALLOC(int_ptrsize* nwords)
{ /* util_malloc  */
  void* iptout = NULL;
  int_ptrsize nbytes = 0;

  nbytes = (*nwords);

  if(!(iptout= malloc(nbytes))) {
    printf("UTIL_MALLOC: Out of memory, malloc return: %p \n",iptout);
    printf("   Requested value: %f = %4d bit unsigned int \n",(float)nbytes,sizeof(nbytes));
    return NULL;

  } /* if */

  return iptout;
} /* util_malloc */


/* ----------------------------------------------------------------------
 * util_realloc: Wrapper for C realloc().
 */
void* util_realloc(void** iptr, int_ptrsize* nwords)
{ /* util_realloc */
  void* iptout = NULL;
  int_ptrsize nbytes = 0;
 
  nbytes = (*nwords);

  if (!(iptout= realloc(*iptr,nbytes))) {
    printf("util_realloc: Reallocation error - aborting\n");
    printf("   Requested value: %f = %4d bit unsigned int \n",(float)nbytes,sizeof(nbytes));

    /*  exit(1); */
    return NULL;

  } /* if */
 
  return iptout;
} /* util_realloc */

void* util_realloc_(void** iptr, int_ptrsize* nwords)
{ /* util_realloc */
  void* iptout = NULL;
  int_ptrsize nbytes = 0;

  nbytes = (*nwords);

  if (!(iptout= realloc(*iptr,nbytes))) {
    printf("util_realloc_: Reallocation error - aborting\n");
    printf("   Requested value: %f = %4d bit unsigned int \n",(float)nbytes,sizeof(nbytes));

    /*  exit(1); */
    return NULL;

  } /* if */

  return iptout;
} /* util_realloc */

void* UTIL_REALLOC(void** iptr, int_ptrsize* nwords)
{ /* util_realloc */
  void* iptout = NULL;
  int_ptrsize nbytes = 0;

  nbytes = (*nwords);

  if (!(iptout= realloc(*iptr,nbytes))) {
    printf("UTIL_REALLOC: Reallocation error - aborting\n");
    printf("   Requested value: %f = %4d bit unsigned int \n",(float)nbytes,sizeof(nbytes));

    /*  exit(1); */
    return NULL;

  } /* if */

  return iptout;
} /* util_realloc */

 
/* ----------------------------------------------------------------------
 * util_free: Wrapper for C free().
 */
int_ptrsize util_free(void** iptr)
{ /* free */
  free(*iptr);
  *iptr = NULL;
  return 0;
} /* free */

int_ptrsize util_free_(void** iptr)
{ /* free */
  free(*iptr);
  *iptr = NULL;
  return 0;
} /* free */

int_ptrsize UTIL_FREE(void** iptr)
{ /* free */
  free(*iptr);
  *iptr = NULL;
  return 0;
} /* free */

  
/* ----------------------------------------------------------------------
 * util_sum_addr: Add two integers/pointers.
 */
int_ptrsize util_sum_addr(int_ptrsize* a, int_ptrsize* b)
{ /* util_sum_addr */
  return *a + *b;
} /* util_sum_addr */

int_ptrsize util_sum_addr_(int_ptrsize* a, int_ptrsize* b)
{ /* util_sum_addr */
  return *a + *b;
} /* util_sum_addr */

int_ptrsize UTIL_SUM_ADDR(int_ptrsize* a, int_ptrsize* b)
{ /* util_sum_addr */
  return *a + *b;
} /* util_sum_addr */

/*
* Original allocation routines removed
*  warning: cast to pointer from integer of different size
*  warning: assignment makes integer from pointer without a cast
*/
 
unsigned int unsigned_sum_ (a, b)
    unsigned int *a, *b;
{
    return *a+*b;
}  
unsigned int unsigned_sum (a, b)
    unsigned int *a, *b;
{
    return *a+*b;
}  
unsigned int UNSIGNED_SUM (a, b)
    unsigned int *a, *b;
{
    return *a+*b;
}  

/* ====================================================================== */
/*  The following are routines handy for platform dependent checking      */

/* ----------------------------------------------------------------------
   util_check_ptrsize - lagrit assumes size of pointer and integer
   are the same,this returns 0 if equal, difference if not 
*/ 
int util_check_ptrsize(void)
{
   void* ipt = NULL;
   return ( sizeof(ipt) - sizeof(int_ptrsize)) ;  
}
int util_check_ptrsize_(void)
{
   void* ipt = NULL;
   return ( sizeof(ipt) - sizeof(int_ptrsize)) ;          
}
int UTIL_CHECK_PTRSIZE(void)
{
   void* ipt = NULL;
   return ( sizeof(ipt) - sizeof(int_ptrsize)) ;          
}

/* ----------------------------------------------------------------------
   util_get_ptrsize - returns size of pointer (4 or 8) 
*/ 
int util_get_ptrsize(void)
{
  void* ipt = NULL;
  return( sizeof(ipt));
}
int util_get_ptrsize_(void)
{
  void* ipt = NULL;
  return( sizeof(ipt));
}
int UTIL_GET_PTRSIZE(void)
{
  void* ipt = NULL;
  return( sizeof(ipt));
}


/* ----------------------------------------------------------------------
   util_print_sizes - write to screen information on sizeof for each type
*/
void util_print_sizes(void)
{
   int i;
   long l;
   void* ipt = NULL;
   int_ptrsize* nwords;

   printf("\nMEMORY SIZES : \n");
   printf(" Sizeof char    (type 3) =  %u bytes      Sizeof long        =   %u bytes\n",sizeof(char),sizeof(l));
   printf(" Sizeof real*8  (type 2) =  %u bytes      Sizeof pointer     =   %u bytes\n",sizeof(double),sizeof(ipt));
   printf(" Sizeof integer (type 1) =  %u bytes      Sizeof INT_PTRSIZE =   %u bytes\n",sizeof(i),sizeof(int_ptrsize));
   if (sizeof(ipt) != sizeof(int_ptrsize)) {
     printf("MALLOC DANGER! Size of INT_PTRSIZE pointer must equal pointer size!\n\n"); 
   } 

   printf("\n");
} 
void util_print_sizes_(void)
{
   int i;
   long l;
   void* ipt = NULL;
   int_ptrsize* nwords;

   printf("\nMEMORY SIZES : \n");
   printf(" Sizeof char    (type 3) =  %u bytes      Sizeof long        =   %u bytes\n",sizeof(char),sizeof(l));
   printf(" Sizeof real*8  (type 2) =  %u bytes      Sizeof pointer     =   %u bytes\n",sizeof(double),sizeof(ipt));
   printf(" Sizeof integer (type 1) =  %u bytes      Sizeof INT_PTRSIZE =   %u bytes\n",sizeof(i),sizeof(int_ptrsize));
   if (sizeof(ipt) != sizeof(int_ptrsize)) {
     printf("MALLOC DANGER! Size of INT_PTRSIZE pointer must equal pointer size!\n\n");
   }

   printf("\n");
}
void UTIL_PRINT_SIZES(void)
{
   int i;
   long l;
   void* ipt = NULL;
   int_ptrsize* nwords;

   printf("\nMEMORY SIZES : \n");
   printf(" Sizeof char    (type 3) =  %u bytes      Sizeof long        =   %u bytes\n",sizeof(char),sizeof(l));
   printf(" Sizeof real*8  (type 2) =  %u bytes      Sizeof pointer     =   %u bytes\n",sizeof(double),sizeof(ipt));
   printf(" Sizeof integer (type 1) =  %u bytes      Sizeof INT_PTRSIZE =   %u bytes\n",sizeof(i),sizeof(int_ptrsize));
   if (sizeof(ipt) != sizeof(int_ptrsize)) {
     printf("MALLOC DANGER! Size of INT_PTRSIZE pointer must equal pointer size!\n\n");
   }

   printf("\n");
}
 

/* ----------------------------------------------------------------------
 * Simple routine to check platform dependant sizes 
 */


int util_test_malloc(void)
  // void* malloc(size_t size);
  //    malloc returns a pointer to a block of memory of size bytes, returns 0d0 if memory unavailable
  //    the pointer must be cast into the appropriate variable type to be used
  // void free(void *s);
  //    given a pointer to memory allocated with malloc, the memory is freed
{

  long length;
  long depth;
  int_ptrsize ** list = NULL;
  int_ptrsize nbytes;
  long i,j;
  long count = 0;
  long itotal = 0;
  int idone = 0;
  int ishow = 6;

  length = 10;
  length = 1000000;
  depth = length;


  printf("Calling util_test_malloc:\n");
  printf("The size of an integer in bytes is %d\n",sizeof(int));
  printf("The size of a  long in bytes is %d\n",sizeof(long));
  printf("The size of a  pointer in bytes is %d\n",sizeof(NULL));
  if(list == NULL)
    printf("initial value of pointer properly set to null pointer\n");

  list = (int_ptrsize **) malloc(length * sizeof(int_ptrsize*));
  if(list == NULL){
   printf("There is NOT enough space for %d pointers to long integers\n",length);
  }

/*  main work loop */
  else {
  printf("There is enough space for %d pointers to long integers\n",length);
  printf("LOOP with malloc sizes %d\n",depth * sizeof(int));
  for(i=0; i<length && idone==0; i++) {

      nbytes = depth * sizeof(int);
      list[i] = (int_ptrsize *) malloc(nbytes);
      if (i < ishow ) printf("%d is %p int %d long int %ld \n",i, list[i],list[i],list[i]);
      if(list[i] == NULL) {
          if (i > ishow ) {
            printf("%d is %p int %d long int %ld \n",i-1, list[i-1],list[i-1],list[i-1]);
            printf("%d is %p int %d long int %ld \n",i, list[i],list[i],list[i]);
          }
          itotal = i;
          printf("malloc failed at depth step %d\n",i+1);
          printf("malloc size total %d \n",i*(depth * sizeof(int)));
          printf("long nbytes= %ld int nbytes=%d \n",nbytes,nbytes);
          idone = 1;
      }
      if((i+1) == length) {
          if (i > ishow ) printf("%d is int %d \n",i, list[i]);
          itotal = length;
          printf("malloc succeeded at all steps %d\n",i+1);
/*          printf("malloc size total %"PRIu64" \n",i*(depth * sizeof(int)));
*/
          printf("malloc size total %d \n",i*(depth * sizeof(int)));
      }

  }
  /*   free loop */
  for(i=0;i< itotal;i++) {
      if (i < ishow || i==(itotal-1)) { printf("free %d at %p int %d long int %ld \n",i, list[i],list[i],list[i]); }
      free(list[i]);
   }}

/*  end main work loop */
  return 0;
}
int util_test_malloc_(void)
{

  long length;
  long depth;
  int_ptrsize ** list = NULL;
  int_ptrsize nbytes;
  long i,j;
  long count = 0;
  long itotal = 0;
  int idone = 0;
  int ishow = 6;

  length = 10;
  length = 1000000;
  depth = length;


  printf("Calling util_test_malloc mangled to util_test_malloc_:\n");
  printf("The size of an integer in bytes is %d\n",sizeof(int));
  printf("The size of a  long in bytes is %d\n",sizeof(long));
  printf("The size of a  pointer in bytes is %d\n",sizeof(NULL));
  if(list == NULL)
    printf("initial value of pointer properly set to null pointer\n");

  list = (int_ptrsize **) malloc(length * sizeof(int_ptrsize*));
  if(list == NULL){
   printf("There is NOT enough space for %d pointers to long integers\n",length);
  }

/*  main work loop */
  else {
  printf("There is enough space for %d pointers to long integers\n",length);
  printf("LOOP with malloc sizes %d\n",depth * sizeof(int));
  for(i=0; i<length && idone==0; i++) {

      nbytes = depth * sizeof(int);
      list[i] = (int_ptrsize *) malloc(nbytes);
      if (i < ishow ) printf("%d is %p int %d long int %ld \n",i, list[i],list[i],list[i]);
      if(list[i] == NULL) {
          if (i > ishow ) {
            printf("%d is %p int %d long int %ld \n",i-1, list[i-1],list[i-1],list[i-1]);
            printf("%d is %p int %d long int %ld \n",i, list[i],list[i],list[i]);
          }
          itotal = i;
          printf("malloc failed at depth step %d\n",i+1);
          printf("malloc size total %d \n",i*(depth * sizeof(int)));
          printf("long nbytes= %ld int nbytes=%d \n",nbytes,nbytes);
          idone = 1;
      }
      if((i+1) == length) {
          if (i > ishow ) printf("%d is int %d \n",i, list[i]);
          itotal = length;
          printf("malloc succeeded at all steps %d\n",i+1);
          printf("malloc size total %d \n",length*(depth * sizeof(int)));
          printf("long nbytes= %d int nbytes=%d \n",nbytes,nbytes);
      }

  }
  /*   free loop */
  for(i=0;i< itotal;i++) {
      if (i < ishow || i==(itotal-1)) { printf("free %d at %p int %d long int %ld \n",i, list[i],list[i],list[i]); }
      free(list[i]);
   }}

/*  end main work loop */
  return 0;
}





/* End of file */


