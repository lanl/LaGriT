/*  LaGriT assumes that the size of an integer is the same size as a
 *  pointer.  Use the preprocessor and configure settings to select
 *  the integer type so that it matches the size of a pointer.
 */ 
#include "opsys.h" 

/*
  ######################################################################
 
         $Log: gmvwritef.c,v $
         Revision 2.00  2007/11/03 00:49:10  spchu
         Import to CVS

 PVCS
 PVCS       Rev 1.21   02 Oct 2007 12:40:28   spchu
 PVCS    original version
 
  ######################################################################
*/

/*
    Copyright, 1991, The Regents of the University of
    California.  This software was produced under a U. S.
    Government contract (W-7405-ENG-36) by the Los Alamos
    National Laboratory, which is operated by the
    University of California for the U. S. Department of
    Energy.  The U. S. Government is licensed to use,
    reproduce, and distribute this software.  Permission
    is granted to the public to copy and use this software
    without charge, provided that this Notice and any statement
    of authorship are reproduced on all copies.  Neither the
    Government nor the University makes any warranty, express
    or implied, or assumes any liability or responsibility for
    the use of this software.
 
 
     Software Author: Kevin L. Bolling
*/
/* For name mangling on different platforms we need a version
   of routines with an appended underscore_
   Use a wrapper method for the routines
   Use copy of code section for one or two line wrappers
*/

 
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

/************************************************************************/
void fgmvwriteopenfile_(char filnam[])
{
 char *name;
 int i,len;

 name = (char*)malloc(sizeof(char)*90);

 if(!name){
   printf("fgmvwriteopenfile: malloc failed for name pointer.\n");
   printf("Attempted %d bytes.\n",sizeof(char)*90);
   return;
  }

  for (i=0;i<89;i++) name[i]=filnam[i];
 /*  teminate fname with null character */
 i = 0;
 while ( *(name+i) != ' ')
  {
   i++;
  }
 *(name+i) = '\0';
 gmvwriteopenfile(name);
 free(name);
}

void fgmvwriteopenfile(char filnam[])
{
  fgmvwriteopenfile_(filnam);
}

/************************************************************************/

void fgmvwriteclosefile()
 {
  gmvwriteclosefile();
 }

void fgmvwriteclosefile_()
 {
  gmvwriteclosefile();
 }


/************************************************************************/

void fgmvwritenodedata(int *nndes, float x[], float y[], float z[])
 {
  gmvwritenodedata(*nndes, x, y, z);
 }

void fgmvwritenodedata_(int *nndes, float x[], float y[], float z[])
 {
  gmvwritenodedata(*nndes, x, y, z);
 }


/************************************************************************/


void fgmvwritenodedatastruct(int *nxv, int *nyv, int *nzv, float x[], float y[], float z[])
 {
  gmvwritenodedatastruct(*nxv, *nyv, *nzv, x, y, z);
 }

void fgmvwritenodedatastruct_(int *nxv, int *nyv, int *nzv, float x[], float y[], float z[])
 {
  gmvwritenodedatastruct(*nxv, *nyv, *nzv, x, y, z);
 }

/************************************************************************/


void fgmvwritenodedatalstruct(int *nxv, int *nyv, int *nzv, float x[], float y[], float z[])
{ 
 gmvwritenodedatalstruct(*nxv, *nyv, *nzv, x, y, z);
}

void fgmvwritenodedatalstruct_(int *nxv, int *nyv, int *nzv, float x[], float y[], float z[])
{ 
 gmvwritenodedatalstruct(*nxv, *nyv, *nzv, x, y, z);
}

/************************************************************************/ 

void fgmvwritecellheader(int *ncells)
 {
  gmvwritecellheader(*ncells);
 }

void fgmvwritecellheader_(int *ncells)
 {
  gmvwritecellheader(*ncells);
 }

/************************************************************************/
 

void fgmvwritecelldata_(char cell_data[], int *nverts, int nodes[])
 {
  char *type;
  int i, len;
  type = (char*)malloc(sizeof(char)*10);

  if(!type){
     printf("fgmvwritecelldata: malloc failed for type pointer.\n");
     printf("Attempted %d bytes.\n",sizeof(char)*10);
     return;
   }

  for (i=0;i<8;i++) type[i]=cell_data[i];
  /* terminate type with null character */
  *(type+8) = '\0';
  gmvwritecelldata(type, *nverts, nodes);
  free(type);
 }
 
void fgmvwritecelldata(char cell_data[], int *nverts, int nodes[])
 {
  fgmvwritecelldata_(cell_data, nverts, nodes) ;
 }

/************************************************************************/

void fgmvwritegeneralcelldata_(char cell_data[], int nverts[], int *nfaces, int nodeids[])
 {
  char *type;
  int i, len;
  type = (char*)malloc(sizeof(char)*10);

  if(!type){
     printf("fgmvwritegeneralcelldata: malloc failed for type pointer.\n");
     printf("Attempted %d bytes.\n",sizeof(char)*10);
     return;
   }

  for (i=0;i<8;i++) type[i]=cell_data[i];
  /* terminate type with null character */
  *(type+8) = '\0';
  gmvwritegeneralcelldata(type, nverts, *nfaces, nodeids);
  free(type);
 }

void fgmvwritegeneralcelldata(char cell_data[], int nverts[], int *nfaces, int nodeids[])
 {
  fgmvwritegeneralcelldata_(cell_data, nverts, nfaces, nodeids) ;
}
/************************************************************************/
 
void fgmvwritematerialheader(int *nmats, int *data_type)
 {
  gmvwritematerialheader(*nmats, *data_type);
 }

void fgmvwritematerialheader_(int *nmats, int *data_type)
 {
  gmvwritematerialheader(*nmats, *data_type);
 }

/************************************************************************/
 
void fgmvwritematerialname_(char matname[])
 {
  char *name;
  int i;

  name = (char*)malloc(sizeof(char)*10);

  if(!name){
    printf("fgmvwritematerialname: malloc failed for name pointer.\n");
    printf("Attempted %d bytes.\n",sizeof(char)*10);
    return;
   }


  /* teminate matname with null character */
  for (i=0;i<8;i++) name[i]=matname[i];
  *(name + 8) = '\0';
  gmvwritematerialname(name);
  free(name);
 }

void fgmvwritematerialname(char matname[])
 {
   fgmvwritematerialname_(matname) ;
 }

/************************************************************************/
 
void fgmvwritematerialids(int matids[], int *data_type)
{
  gmvwritematerialids(matids, *data_type);
 }

void fgmvwritematerialids_(int matids[], int *data_type)
{
  gmvwritematerialids(matids, *data_type);
 }

 
/************************************************************************/
void fgmvwritevelocitydata(int *data_type, float u[], float v[], float w[])
 {
  gmvwritevelocitydata(*data_type, u, v, w);
 }

void fgmvwritevelocitydata_(int *data_type, float u[], float v[], float w[])
 {
  gmvwritevelocitydata(*data_type, u, v, w);
 }
 
/************************************************************************/

void fgmvwritevariableheader()
 {
  gmvwritevariableheader();
 }

void fgmvwritevariableheader_()
 {
  gmvwritevariableheader();
 }

 
/************************************************************************/
void fgmvwritevariablenamedata_(char varname[], int *data_type, float vids[])
 {
  char *name;
  int i;

  name = (char*)malloc(sizeof(char)*10);

  if(!name){
   printf("fgmvwritevariablenamedata: malloc failed for name pointer.\n");
   printf("Attempted %d bytes.\n",sizeof(char)*10);
   return;
  }

  /*  terminate varname with null character  */
  for (i=0;i<8;i++) name[i]=varname[i];
  *(name+8) = '\0';
  gmvwritevariablenamedata(name, *data_type, vids);
  free(name);
 }

void fgmvwritevariablenamedata(char varname[], int *data_type, float vids[])
 {
  fgmvwritevariablenamedata_(varname, data_type, vids) ;
}

 
/************************************************************************/
void fgmvwritevariableendvars()
 {
  gmvwritevariableendvars();
 }
 
void fgmvwritevariableendvars_()
 {
  gmvwritevariableendvars();
 }

/************************************************************************/
void fgmvwriteflagheader()
 {
  gmvwriteflagheader();
 }

void fgmvwriteflagheader_()
 {
  gmvwriteflagheader();
 }

/************************************************************************/
void fgmvwriteflagname_(char flagname[], int *data_type, int *ntypes)
{
  char *name;
  int i;

  name = (char*)malloc(sizeof(char)*10);

  if(!name){
   printf("fgmvwriteflagname: malloc failed for name pointer.\n");
   printf("Attempted %d bytes.\n",sizeof(char)*10);
   return;
  }

  for (i=0;i<8;i++) name[i]=flagname[i];
  /*  terminate flagname with null character  */
  *(name+8) = '\0';
  gmvwriteflagname(name, *data_type, *ntypes);
  free(name);
 }

void fgmvwriteflagname(char flagname[], int *data_type, int *ntypes)
{
  fgmvwriteflagname_(flagname, data_type, ntypes) ;
 }

 
/************************************************************************/
void fgmvwriteflagtype_(char flagtype[])
{
  char *type;
  int i;

  type = (char*)malloc(sizeof(char)*10);

  if(!type){
   printf("fgmvwriteflagtype: malloc failed for type pointer.\n");
   printf("Attempted %d bytes.\n",sizeof(char)*10);
   return;
  }

  for (i=0;i<8;i++) type[i]=flagtype[i];
  /*  terminate flagtype with null character  */
  *(type+8) = '\0';
  gmvwriteflagtype(type);
  free(type);
 }

void fgmvwriteflagtype(char flagtype[])
{
 fgmvwriteflagtype_(flagtype);
 }

 
/************************************************************************/
void fgmvwriteflagdata(int *data_type,int iflag[])
{
  gmvwriteflagdata(*data_type, iflag);
 }

void fgmvwriteflagdata_(int *data_type,int iflag[])
{
  gmvwriteflagdata(*data_type, iflag);
 }

 
/************************************************************************/
void fgmvwriteflagendflag(int *data_type,int iflag[])
 {
  gmvwriteflagendflag();
 }

void fgmvwriteflagendflag_(int *data_type,int iflag[])
 {
  gmvwriteflagendflag();
 }
 
/************************************************************************/
void fgmvwritepolygonsheader()
{
  gmvwritepolygonsheader();
 }

void fgmvwritepolygonsheader_()
{
  gmvwritepolygonsheader();
 }
 
/************************************************************************/
void fgmvwritepolygonsdata(int *nverts, int *matnum, float x[], float y[], float z[])
 {
  gmvwritepolygonsdata(*nverts, *matnum, x, y, z);
 } 

void fgmvwritepolygonsdata_(int *nverts, int *matnum, float x[], float y[], float z[])
 {
  gmvwritepolygonsdata(*nverts, *matnum, x, y, z);
 } 
 
/************************************************************************/
void fgmvwritepolygonsendpoly()
 {
  gmvwritepolygonsendpoly();
 }

void fgmvwritepolygonsendpoly_()
 {
  gmvwritepolygonsendpoly();
 }

/************************************************************************/
void fgmvwritetracersheader(int *ntracers, float u[], float y[], float z[])
 {
  gmvwritetracersheader(*ntracers, u, y, z);
  printf("\ntracersmain\n");
 }

void fgmvwritetracersheader_(int *ntracers, float u[], float y[], float z[])
 {
  gmvwritetracersheader(*ntracers, u, y, z);
  printf("\ntracersmain\n");
 }

 
/************************************************************************/
void fgmvwritetracersnamedata_(char tracername[], int *ntracers, float data[])
{
  char *name;
  int i;

  name = (char*)malloc(sizeof(char)*10);

  if(!name){
    printf("fgmvwritetracersnamedata: malloc failed for name pointer.\n");
    printf("Attempted %d bytes.\n",sizeof(char)*10);
    return;
  }

  for (i=0;i<8;i++) name[i]=tracername[i];
  /*  terminate tracername with null character  */
  *(name+8) = '\0';
  gmvwritetracersnamedata(name, *ntracers, data);
  free(name);
 }

void fgmvwritetracersnamedata(char tracername[], int *ntracers, float data[])
{
   fgmvwritetracersnamedata_(tracername, ntracers, data);
 }
 
/************************************************************************/
void fgmvwritetracersendtrace()
 {
  gmvwritetracersendtrace();
 }

void fgmvwritetracersendtrace_()
 {
  gmvwritetracersendtrace();
 }

 
/************************************************************************/
void fgmvwriteprobtime(float *ptime)
 {
  gmvwriteprobtime(*ptime);
 }

void fgmvwriteprobtime_(float *ptime)
 {
  gmvwriteprobtime(*ptime);
 }

 
/************************************************************************/
void fgmvwritecycleno(int *number)
 {
  gmvwritecycleno(*number);
 }

void fgmvwritecycleno_(int *number)
 {
  gmvwritecycleno(*number);
 }

/*  gmvwritef.c */ 
