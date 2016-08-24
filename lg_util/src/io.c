/*  LaGriT assumes that the size of an integer is the same size as a
 *  pointer.  Use the preprocessor and configure settings to select
 *  the integer type so that it matches the size of a pointer.
 */ 
#include "opsys.h"

/*
  ######################################################################
 
         $Log: io.c,v $
         Revision 2.00  2007/11/03 00:49:11  spchu
         Import to CVS

 PVCS
 PVCS       Rev 1.21   02 Oct 2007 12:40:28   spchu
 PVCS    original version
 
  ######################################################################
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

FILE *fd[99];
 
/* ====================== */

/* Note unit should be int = 4 bytes
   all remaining integer can be long for 64 bit compile
*/

void cassignw (unit , fname ,ierr)
int  *unit;
char *fname;
int_ptrsize *ierr;
  {
        char name[132];
        int i,len;
 
/*  terminate fname with null character   */
        len = strlen(fname);
        i = len;
        while ( *(fname+ --i) == ' ') ;
        *(fname+i+1) = '\0';
        *ierr = 0;
	strcpy(name,fname);
	if((fd[*unit] = fopen(name,"w")) == NULL)
	{
	  printf("Error opening the input file \n");
          printf("File name was %s \n",name);
          *ierr = -1;
	  exit(0);
	}
  }

void cassignw_(unit , fname ,ierr)
int  *unit;
char *fname;
int_ptrsize *ierr;
  {
        char name[132];
        int_ptrsize i,len;

/*  terminate fname with null character   */
        len = strlen(fname);
        i = len;
        while ( *(fname+ --i) == ' ') ;
        *(fname+i+1) = '\0';
        *ierr = 0;
        strcpy(name,fname);
        if((fd[*unit] = fopen(name,"w")) == NULL)
        {
          printf("Error opening the input file \n");
          printf("File name was %s \n",name);
          *ierr = -1;
          exit(0);
        }
  }

 
/* ====================== */
void cassignr (unit , fname ,ierr)
int  *unit;
char *fname;
int_ptrsize *ierr;
  {
        char name[132];
        int_ptrsize i,len;
 
/*  terminate fname with null character   */
        len = strlen(fname);
        i = len;
        while ( *(fname+ --i) == ' ') ;
        *(fname+i+1) = '\0';
        *ierr = 0;
	strcpy(name,fname);
	if((fd[*unit] = fopen(name,"r")) == NULL)
	{
	  printf("Error opening the input file \n");
          printf("File name was %s \n",name);
          *ierr = -1;
	  exit(0);
	}
  }

void cassignr_(unit , fname ,ierr)
int  *unit;
char *fname;
int_ptrsize *ierr;
  {
        char name[132];
        int_ptrsize i,len;

/*  terminate fname with null character   */
        len = strlen(fname);
        i = len;
        while ( *(fname+ --i) == ' ') ;
        *(fname+i+1) = '\0';
        *ierr = 0;
        strcpy(name,fname);
        if((fd[*unit] = fopen(name,"r")) == NULL)
        {
          printf("Error opening the input file \n");
          printf("File name was %s \n",name);
          *ierr = -1;
          exit(0);
        }
  }

 
/* ====================== */
void cposition (unit,nbytes,ierr)
  int *unit;
  int_ptrsize *nbytes, *ierr;
  {
    	*ierr = fseek(fd[*unit],*nbytes,0);
  }

void cposition_(unit,nbytes,ierr)
  int *unit;
  int_ptrsize *nbytes, *ierr;
  {
        *ierr = fseek(fd[*unit],*nbytes,0);
  }
 
/* ====================== */
void cwrite (unit , array , ilen , ierr)
  int *unit;
  char *array;
  int_ptrsize *ilen , *ierr;
  {
        int_ptrsize  n_written , num_to_write;
 
	num_to_write = *ilen;
	n_written = fwrite(array,1,num_to_write,fd[*unit]);
	if(n_written == num_to_write)
	  *ierr = 0;
	else
	  *ierr = n_written;
  }

void cwrite_(unit , array , ilen , ierr)
  int *unit;
  char *array;
  int_ptrsize *ilen , *ierr;
  {
        int_ptrsize  n_written , num_to_write;

        num_to_write = *ilen;
        n_written = fwrite(array,1,num_to_write,fd[*unit]);
        if(n_written == num_to_write)
          *ierr = 0;
        else
          *ierr = n_written;
  }

 
/* ====================== */
void cread (unit , array , ilen , ierr)
  int *unit;
  char *array;
  int_ptrsize  *ilen , *ierr;
  {
       int_ptrsize  n_read , num_to_read;
 
	num_to_read = *ilen;
	n_read = fread(array,1,num_to_read,fd[*unit]);
	if(n_read == num_to_read)
	  *ierr = 0;
	else
	  *ierr = n_read;
  }

void cread_(unit , array , ilen , ierr)
  int *unit;
  char *array;
  int_ptrsize  *ilen , *ierr;
  {
       int_ptrsize  n_read , num_to_read;

        num_to_read = *ilen;
        n_read = fread(array,1,num_to_read,fd[*unit]);
        if(n_read == num_to_read)
          *ierr = 0;
        else
          *ierr = n_read;
  }

/* ====================== */
 
void cclose (unit)
  int *unit;
  {
      fclose(fd[*unit]);
  }

void cclose_(unit)
  int *unit;
  {
      fclose(fd[*unit]);
  }

void CCLOSE(unit)
  int *unit;
  {
      fclose(fd[*unit]);
  }

