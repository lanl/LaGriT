/*  LaGriT assumes that the size of an integer is the same size as a
 *  pointer.  Use the preprocessor and configure settings to select
 *  the integer type so that it matches the size of a pointer.
 */ 
#include "opsys.h"

/*
  ######################################################################
 
         $Log: gmvwrite.c,v $
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
 
 
     Software Author:  Kevin L. Bolling
 
*/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <math.h>

#define FLOAT32 4
#define INT32 4
int n_nodes, n_cells;
FILE *fp;
char tmpname[9];
int ieerror;
int typei = 1;
int typef = 2;
int num0 = 0;
int num1 = 1;


void gmv_print_sizes(void)
{
   int i;
   long l;
   void* ipt = NULL;
   int_ptrsize* nwords;

   printf("gmv_print_sizes : \n");
   printf("    sizeof int =           %u\n",sizeof(i));
   printf("    sizeof long =          %u\n",sizeof(l));     
   printf("    sizeof int_ptrsize =   %u\n",sizeof(int_ptrsize));
   printf("    FLOAT32 =              %u\n",FLOAT32);
   printf("    INT32 =                %u\n",INT32);


   printf("GMV FLOAT and INT sizes must be 4.\n");
   printf("\n");

} 
void gmv_print_sizes_(void)
{
   int i;
   long l;
   void* ipt = NULL;
   int* nwords;

   printf("gmv_print_sizes : \n");
   printf("    sizeof int =           %u\n",sizeof(i));
   printf("    sizeof long =          %u\n",sizeof(l));
   printf("    sizeof int_ptrsize =   %u\n",sizeof(int_ptrsize));
   printf("    FLOAT32 =              %u\n",FLOAT32);
   printf("    INT32 =                %u\n",INT32);


   printf("GMV FLOAT and INT sizes must be 4.\n");
   printf("\n");

}



void gmvwriteopenfile(char filenam[])
{
 fp = fopen(filenam, "w");
 /*  Write header.  */
 strcpy(tmpname,"gmvinput");
 fwrite(tmpname,sizeof(char),8,fp);
 strcpy(tmpname, "ieee    ");
 fwrite(tmpname,sizeof(char),8,fp);
 if (!fp) printf("gmvwriteopenfile: OPEN FAILED for file: %s\n",filenam);
 
}
 
void gmvwriteclosefile()
 {
  strcpy(tmpname, "endgmv  ");
  fwrite(tmpname,sizeof(char),8,fp);
  fclose(fp);
 }
 
void gmvwritenodedata(int nndes, float x[], float y[], float z[])
 {
   int count, tmpnndes, e;
   float *tempx;
   float *tempy;
   float *tempz;
   strcpy(tmpname, "nodes   ");
 
#ifdef unicos
     {
      ieerror = CRAY2IEG(&typei, &num1, &tmpnndes, &num0, &nndes, &num1);
     }
#else
     {
      tmpnndes = nndes;
     }
#endif
 
   tempx = (float*)malloc(sizeof(float)*(nndes));
   tempy = (float*)malloc(sizeof(float)*(nndes));
   tempz = (float*)malloc(sizeof(float)*(nndes));

   if(!tempx){
     printf("gmvwritenodedata: malloc failed for tempx pointer.\n");
     printf("Requested number of floats = %f \n",(float)nndes);
     printf("sizeof malloc float = %d, sizeof GMV float = %d \n",sizeof(float),FLOAT32);
     gmv_print_sizes();
     nndes = 0;
     return;
   }
   if(!tempy){
     printf("gmvwritenodedata: malloc failed for tempy pointer.\n");
     printf("Requested number of floats = %f \n",(float)nndes);
     printf("sizeof malloc float = %d, sizeof GMV float = %d \n",sizeof(float),FLOAT32);
     nndes = 0;
     return;
   }
   if(!tempz){
     printf("gmvwritenodedata: malloc failed for tempz pointer.\n");
     printf("Requested number of floats = %f \n",(float)nndes);
     printf("sizeof malloc float = %d, sizeof GMV float = %d \n",sizeof(float),FLOAT32);
     nndes = 0;
     return;
   }

 
#ifdef unicos
     {
        ieerror = CRAY2IEG(&typef, &nndes, tempx, &num0, x, &num1);
        ieerror = CRAY2IEG(&typef, &nndes, tempy, &num0, y, &num1);
        ieerror = CRAY2IEG(&typef, &nndes, tempz, &num0, z, &num1);
     }
#else
     {
      for(count = 0; count < nndes; count++)
       {
        tempx[count] = x[count];
        tempy[count] = y[count];
        tempz[count] = z[count];
       }
     }
#endif
   fwrite(tmpname,sizeof(char),8,fp);
   fwrite(&tmpnndes,INT32,1,fp);
   /*  Write node x,y,z's.  */
   fwrite(tempx,FLOAT32,nndes,fp);
   fwrite(tempy,FLOAT32,nndes,fp);
   fwrite(tempz,FLOAT32,nndes,fp);
   n_nodes = nndes;
   free(tempx);
   free(tempy);
   free(tempz);
 }

 
void gmvwritenodedatastruct(int nxv, int nyv, int nzv, float x[], float y[], float z[])
 {
   int nndes, alt, count;
   int tmpnxv, tmpnyv, tmpnzv;
   float *tempx, *tempy, *tempz;
   strcpy(tmpname, "nodes   ");
   alt =-1;
 
#ifdef unicos
     {
      ieerror = CRAY2IEG(&typei, &num1, &tmpnxv, &num0, nxv, &num1);
      ieerror = CRAY2IEG(&typei, &num1, &tmpnyv, &num0, nyv, &num1);
      ieerror = CRAY2IEG(&typei, &num1, &tmpnzv, &num0, nzv, &num1);
      ieerror = CRAY2IEG(&typei, &num1, &alt, &num0, &alt, &num1);
     }
#else
     {
      tmpnxv = nxv;
      tmpnyv = nyv;
      tmpnzv = nzv;
     }
#endif
 
   tempx = (float*)malloc(sizeof(float)*nxv);
   tempy = (float*)malloc(sizeof(float)*nyv);
   tempz = (float*)malloc(sizeof(float)*nzv);
   if(!tempx){
     printf("gmvwritenodedatastruct: malloc failed for tempx pointer.\n");
     printf("Requested number of floats = %f \n",(float)nxv);
     printf("sizeof float = %d \n",sizeof(float));
     nxv = 0;
     return;
   }
   if(!tempy){
     printf("gmvwritenodedatastruct: malloc failed for tempy pointer.\n");
     printf("Requested number of floats = %f \n",(float)nyv);
     printf("sizeof float = %d \n",sizeof(float));
     nyv = 0;
     return;
   }
   if(!tempz){
     printf("gmvwritenodedatastruct: malloc failed for tempz pointer.\n");
     printf("Requested number of floats = %f \n",(float)nzv);
     printf("sizeof float = %d \n",sizeof(float));
     nzv = 0;
     return;
   }

 
#ifdef unicos
     {
      ieerror = CRAY2IEG(&typef, &nxv, tempx, &num0, x, &num1);
      ieerror = CRAY2IEG(&typef, &nyv, tempy, &num0, y, &num1);
      ieerror = CRAY2IEG(&typef, &nzv, tempz, &num0, z, &num1);
     }
#else
     {
      for(count = 0; count < nxv; count ++)
       {
        tempx[count] = x[count];
       }
      for(count = 0; count < nyv; count ++)
       {
        tempy[count] = y[count];
       }
      for(count = 0; count < nzv; count ++)
       {
        tempz[count] = z[count];
       }
     }
#endif
 
   nndes = nxv * nyv * nzv;
   /*  Write node header.  */
   fwrite(tmpname,sizeof(char),8,fp);
   fwrite(&alt,INT32,1,fp);
   fwrite(&tmpnxv,INT32,1,fp);
   fwrite(&tmpnyv,INT32,1,fp);
   fwrite(&tmpnzv,INT32,1,fp);
   fwrite(tempx,FLOAT32,nxv,fp);
   fwrite(tempy,FLOAT32,nyv,fp);
   fwrite(tempz,FLOAT32,nzv,fp);
   n_nodes =nndes;
   free(tempx);
   free(tempy);
   free(tempz);
 }
 
void gmvwritenodedatalstruct(int nxv, int nyv, int nzv, float x[], float y[], float z[])
{
  int nndes, alt, tmpnxv, tmpnyv, tmpnzv, count;
  float *tempx, *tempy, *tempz;
  strcpy(tmpname, "nodes   ");
  alt = -2;
#ifdef unicos
    {
     ieerror = CRAY2IEG(&typei, &num1, &tmpnxv, &num0, &nxv, &num1);
     ieerror = CRAY2IEG(&typei, &num1, &tmpnyv, &num0, &nyv, &num1);
     ieerror = CRAY2IEG(&typei, &num1, &tmpnzv, &num0, &nzv, &num1);
     ieerror = CRAY2IEG(&typei, &num1, &alt, &num0, &alt, &num1);
    }
#else
    {
     tmpnxv = nxv;
     tmpnyv = nyv;
     tmpnzv = nzv;
    }
#endif
 
  nndes = nxv * nyv * nzv;
  tempx = (float*)malloc(sizeof(float)*nndes);
  tempy = (float*)malloc(sizeof(float)*nndes);
  tempz = (float*)malloc(sizeof(float)*nndes);

   if(!tempx){
     printf("gmvwritenodedatalstruct: malloc failed for tempx pointer.\n");
     printf("Requested number of floats = %f \n",(float)nndes);
     printf("sizeof float = %d \n",sizeof(float));
     nxv = 0;
     return;
   }
   if(!tempy){
     printf("gmvwritenodedatalstruct: malloc failed for tempy pointer.\n");
     printf("Requested number of floats = %f \n",(float)nndes);
     printf("sizeof float = %d \n",sizeof(float));
     nyv = 0;
     return;
   }
   if(!tempz){
     printf("gmvwritenodedatalstruct: malloc failed for tempz pointer.\n");
     printf("Requested number of floats = %f \n",(float)nndes);
     printf("sizeof float = %d \n",sizeof(float));
     nzv = 0;
     return;
   }

 
#ifdef unicos
    {
     ieerror = CRAY2IEG(&typef, &nndes, tempx, &num0, x, &num1);
     ieerror = CRAY2IEG(&typef, &nndes, tempy, &num0, y, &num1);
     ieerror = CRAY2IEG(&typef, &nndes, tempz, &num0, z, &num1);
    }
#else
    {
     for(count = 0; count < nndes; count++)
      {
       tempx[count] = x[count];
       tempy[count] = y[count];
       tempz[count] = z[count];
      }
    }
#endif
 
 /*  Write node x,y,z's for logicaly structured grids.  */
   n_nodes = nndes;
   fwrite(tmpname,sizeof(char),8,fp);
   fwrite(&alt,INT32,1,fp);
   fwrite(&tmpnxv,INT32,1,fp);
   fwrite(&tmpnyv,INT32,1,fp);
   fwrite(&tmpnzv,INT32,1,fp);
   fwrite(tempx,FLOAT32,nndes,fp);
   fwrite(tempy,FLOAT32,nndes,fp);
   fwrite(tempz,FLOAT32,nndes,fp);
   free(tempx);
   free(tempy);
   free(tempz);
}
 
 
void gmvwritecellheader(int ncells)
 {
  int tmpncells;
  strcpy(tmpname, "cells   ");
#ifdef unicos
    {
     ieerror = CRAY2IEG(&typei, &num1, &tmpncells, &num0, &ncells, &num1);
    }
#else
    {
     tmpncells = ncells;
    }
#endif
 
  n_cells = ncells;
  fwrite(tmpname,sizeof(char),8,fp);
  fwrite(&tmpncells,INT32,1,fp);
 
 }
 
void gmvwritecelldata(char cell_data[], int nverts, int nodes[])
 {
   int tmpnverts, count;
   int *tempnodes;
#ifdef unicos
     {
      ieerror = CRAY2IEG(&typei, &num1, &tmpnverts, &num0, &nverts, &num1);
     }
#else
     {
      tmpnverts = nverts;
     }
#endif
 
   tempnodes = (int*)malloc(sizeof(int)*nverts);
   if(!tempnodes){
     printf("gmvwritecelldata: malloc failed for tempnodes pointer.\n");
     printf("Requested number of ints = %f \n",(float)nverts);
     printf("sizeof int = %d \n",sizeof(int));
     nverts = 0;
     return;
   }

 
#ifdef unicos
     {
      ieerror = CRAY2IEG(&typei, &nverts, tempnodes, &num0, nodes, &num1);
     }
#else
     {
      for(count = 0;count < nverts;count++)
       {
        tempnodes[count] = nodes[count];
       }
      }
#endif
 
   fwrite(cell_data,sizeof(char),8,fp);
   fwrite(&tmpnverts,INT32,1,fp);
   fwrite(tempnodes,INT32,nverts,fp);
   free(tempnodes);
 }
 
 
void gmvwritegeneralcelldata(char cell_data[], int nverts[], int nfaces, int nodeids[])
 
 {
   int sumverts, count, i,  *tempnverts, tmpnfaces, *tempnodeids;
#ifdef unicos
     {
      ieerror = CRAY2IEG(&typei, &num1, &tmpnfaces, &num0, &nfaces, &num1);
     }
#else
     {
      tmpnfaces = nfaces;
     }
#endif
 
   tempnverts = (int*)malloc(sizeof(int)*nfaces);
   if(!tempnverts){
     printf("gmvwritegeneralcelldata: malloc failed for tempnverts pointer.\n");
     printf("Requested number of ints = %f \n",(float)nfaces);
     printf("sizeof int = %d \n",sizeof(int));
     nfaces = 0;
     return;
   }

 
#ifdef unicos
     {
      ieerror = CRAY2IEG(&typei, &nfaces, tempnverts, &num0, nverts, &num1);
     }
#else
     {
      for(count = 0;count < nfaces;count++)
       {
        tempnverts[count] = nverts[count];
       }
     }
#endif
 
   for(i=0; i < nfaces; i++)
    {
     sumverts +=nverts[i];
    }
   tempnodeids = (int*)malloc(sizeof(int)*sumverts);
   if(!tempnodeids){
     printf("gmvwritegeneralcelldata: malloc failed for tempnodeids pointer.\n");
     printf("Requested number of ints = %f \n",(float)sumverts);
     printf("sizeof int = %d \n",sizeof(int));
     nverts = 0;
     return;
   }

 
#ifdef unicos
     {
      ieerror = CRAY2IEG(&typei, &sumverts, tempnodeids, &num0, nodeids, &num1);
     }
#else
     {
      for(count = 0;count < sumverts;count++)
       {
        tempnodeids[count] = nodeids[count];
       }
     }
#endif
 
   fwrite(cell_data,sizeof(char),8,fp);
   fwrite(&tmpnfaces,INT32,1,fp);
   fwrite(tempnverts,INT32,nfaces,fp);
   fwrite(tempnodeids,INT32,sumverts,fp);
   free(tempnodeids);
   free(tempnverts);
 }
 
 
void gmvwritematerialheader(int nmats, int data_type)
 {
  int tmpnmats, tmpdata_type;
  strcpy(tmpname, "material");
#ifdef unicos
    {
     ieerror = CRAY2IEG(&typei, &num1, &tmpnmats, &num0, &nmats, &num1);
     ieerror = CRAY2IEG(&typei, &num1, &tmpdata_type, &num0, &data_type, &num1);
    }
#else
    {
     tmpnmats = nmats;
     tmpdata_type = data_type;
    }
#endif
 
  fwrite(tmpname,sizeof(char),8,fp);
  fwrite(&tmpnmats,INT32,1,fp);
  fwrite(&tmpdata_type,INT32,1,fp);
 }
 
void gmvwritematerialname(char matname[])
 {
    fwrite(matname,sizeof(char),8,fp);
 }
 
void gmvwritematerialids(int matids[], int data_type)
 {
  int *tempmatids, npts, count;
 
  if(data_type==0)
   {
    npts = n_cells;
   }
  else if(data_type == 1)
   {
    npts = n_nodes;
   }
  tempmatids = (int*)malloc(sizeof(int)*npts);
  if(!tempmatids){
     printf("gmvwritematerialids: malloc failed for tempmatids pointer.\n");
     printf("Requested number of ints = %f \n",(float)npts);
     printf("sizeof int = %d \n",sizeof(int));
     return;
  }

 
#ifdef unicos
    {
     ieerror = CRAY2IEG(&typei, &npts, tempmatids, &num0, matids, &num1);
    }
#else
    {
     for(count = 0; count <npts; count++)
      {
       tempmatids[count] = matids[count];
      }
    }
#endif
 
  fwrite(tempmatids,INT32,npts,fp);
  free(tempmatids);
 }
void gmvwritevelocitydata(int data_type, float u[], float v[], float w[])
 {
  int tmpdata_type, count, npts;
  float *tempu, *tempv, *tempw;
  strcpy(tmpname, "velocity");
 
#ifdef unicos
    {
     ieerror = CRAY2IEG(&typei, &num1, &tmpdata_type, &num0, &data_type, &num1);
    }
#else
    {
     tmpdata_type = data_type;
    }
#endif
 
  if(data_type == 0)
   {
    npts = n_cells;
   }
  else if(data_type == 1)
   {
    npts = n_nodes;
   }
  tempu = (float*)malloc(sizeof(float)*npts);
  tempv = (float*)malloc(sizeof(float)*npts);
  tempw = (float*)malloc(sizeof(float)*npts);

   if(!tempu){
     printf("gmvwritevelocitydata: malloc failed for tempu pointer.\n");
     printf("Requested number of floats = %f \n",(float)npts);
     printf("sizeof float = %d \n",sizeof(float));
     return;
   }
   if(!tempv){
     printf("gmvwritevelocitydata: malloc failed for tempv pointer.\n");
     printf("Requested number of floats = %f \n",(float)npts);
     printf("sizeof float = %d \n",sizeof(float));
     return;
   }
   if(!tempw){
     printf("gmvwritevelocitydata: malloc failed for tempw pointer.\n");
     printf("Requested number of floats = %f \n",(float)npts);
     printf("sizeof float = %d \n",sizeof(float));
     return;
   }

 
#ifdef unicos
    {
     ieerror = CRAY2IEG(&typef, &npts, tempu, &num0, u, &num1);
     ieerror = CRAY2IEG(&typef, &npts, tempv, &num0, v, &num1);
     ieerror = CRAY2IEG(&typef, &npts, tempw, &num0, w, &num1);
    }
#else
    {
      for(count = 0; count < npts; count++)
       {
        tempu[count] = u[count];
        tempv[count] = v[count];
        tempw[count] = w[count];
       }
     }
#endif
 
  fwrite(tmpname,sizeof(char),8,fp);
  fwrite(&tmpdata_type,INT32,1,fp);
  fwrite(tempu,FLOAT32,npts,fp);
  fwrite(tempv,FLOAT32,npts,fp);
  fwrite(tempw,FLOAT32,npts,fp);
  free(tempu);
  free(tempv);
  free(tempw);
 }
 
 
void gmvwritevariableheader()
 {
  strcpy(tmpname, "variable");
  fwrite(tmpname,sizeof(char),8,fp);
 }
 
void gmvwritevariablenamedata(char varname[], int data_type, float vids[])
 {
  int tmpdata_type, count, npts;
  float *tempvids;
#ifdef unicos
    {
     ieerror = CRAY2IEG(&typei, &num1, &tmpdata_type, &num0, &data_type, &num1);
    }
#else
    {
     tmpdata_type = data_type;
    }
#endif
 
  if(data_type == 0)
   {
    npts = n_cells;
   }
  else if(data_type == 1)
   {
    npts = n_nodes;
   }
  tempvids = (float*)malloc(sizeof(float)*npts);
  if(!tempvids){
     printf("gmvwritevariablenamedata: malloc failed for tempvids pointer.\n");
     printf("Requested number of floats = %f \n",(float)npts);
     printf("sizeof float = %d \n",sizeof(float));
     return;
   }

 
#ifdef unicos
    {
     ieerror = CRAY2IEG(&typef, &npts, tempvids, &num0, vids, &num1);
    }
#else
    {
     for(count = 0; count < npts; count++)
      {
       tempvids[count] = vids[count];
      }
    }
#endif
 
  fwrite(varname, sizeof(char), 8, fp);
  fwrite(&tmpdata_type, INT32, 1, fp);
  fwrite(tempvids, FLOAT32, npts, fp);
  free(tempvids);
 }
 
void gmvwritevariableendvars()
 {
  strcpy(tmpname, "endvars ");
  fwrite(tmpname,sizeof(char),8,fp);
 }
 
 
void gmvwriteflagheader()
 {
  strcpy(tmpname, "flags   ");
  fwrite(tmpname, sizeof(char),8,fp);
 }
void gmvwriteflagname(char flagname[], int data_type, int ntypes)
 {
  int tmpdata_type, tmpntypes;
#ifdef unicos
    {
     ieerror = CRAY2IEG(&typei, &num1, &tmpdata_type, &num0, &data_type, &num1);
     ieerror = CRAY2IEG(&typei, &num1, &tmpntypes, &num0, &ntypes, &num1);
    }
#else
    {
     tmpdata_type = data_type;
     tmpntypes = ntypes;
    }
#endif
 
  fwrite(flagname,sizeof(char),8,fp);
  fwrite(&tmpntypes,INT32,1,fp);
  fwrite(&tmpdata_type,INT32,1,fp);
 }
 
void gmvwriteflagtype(char flagtype[])
 {
   fwrite(flagtype,sizeof(char),8,fp);
 }
 
void gmvwriteflagdata(int data_type,int iflag[])
 {
  int *tempiflag, npts, count;
 
  if(data_type == 0)
   {
    npts = n_cells;
   }
  else if(data_type == 1)
   {
    npts = n_nodes;
   }
  tempiflag = (int*)malloc(sizeof(int)*npts);
  if(!tempiflag){
     printf("gmvwriteflagdata: malloc failed for tempiflag pointer.\n");
     printf("Requested number of ints = %f \n",(float)npts);
     printf("sizeof int = %d \n",sizeof(int));
     return;
   }

 
#ifdef unicos
    {
     ieerror = CRAY2IEG(&typei, &npts, tempiflag, &num0, iflag, &num1);
    }
#else
    {
     for(count = 0; count<npts; count++)
      {
       tempiflag[count] = iflag[count];
      }
    }
#endif
 
  fwrite(tempiflag,INT32,npts,fp);
  free(tempiflag);
 }
 
void gmvwriteflagendflag()
 {
  strcpy(tmpname, "endflag ");
  fwrite(tmpname,sizeof(char),8,fp);
 }
 
 
void gmvwritepolygonsheader()
 {
  strcpy(tmpname, "polygons");
  fwrite(tmpname,sizeof(char),8,fp);
 }
 
void gmvwritepolygonsdata(int nverts, int matnum, float x[], float y[], float z[])
 {
   float tx[50];
   float ty[50];
   float tz[50];
   int tmpnverts, tmpmatnum, count;
#ifdef unicos
     {
      ieerror = CRAY2IEG(&typei, &num1, &tmpnverts, &num0, &nverts, &num1);
      ieerror = CRAY2IEG(&typei, &num1, &tmpmatnum, &num0, &matnum, &num1);
      ieerror = CRAY2IEG(&typef, &nverts, tx, &num0, x, &num1);
      ieerror = CRAY2IEG(&typef, &nverts, ty, &num0, y, &num1);
      ieerror = CRAY2IEG(&typef, &nverts, tz, &num0, z, &num1);
     }
#else
     {
      tmpnverts = nverts;
      tmpmatnum = matnum;
      for(count = 0; count <  nverts; count++)
       {
        tx[count] = x[count];
        ty[count] = y[count];
        tz[count] = z[count];
       }
     }
#endif
 
   fwrite(&tmpmatnum, INT32, 1, fp);
   fwrite(&tmpnverts, INT32, 1, fp);
   fwrite(tx,FLOAT32,nverts,fp);
   fwrite(ty,FLOAT32,nverts,fp);
   fwrite(tz,FLOAT32,nverts,fp);
 }
 
void gmvwritepolygonsendpoly()
 {
  strcpy(tmpname, "endpoly ");
  fwrite(tmpname,sizeof(char),8,fp);
 }
 
void gmvwritetracersheader(int ntracers, float w[], float y[], float z[])
 {
  int tmpntracers, count;
  float *tempx;
  float *tempy;
  float *tempz;
 
  tempx = (float*)malloc(sizeof(float)*ntracers);
  tempy = (float*)malloc(sizeof(float)*ntracers);
  tempz = (float*)malloc(sizeof(float)*ntracers);

   if(!tempx){
     printf("gmvwritetracersheader: malloc failed for tempx pointer.\n");
     printf("Requested number of floats = %f \n",(float)ntracers);
     printf("sizeof float = %d \n",sizeof(float));
     return;
   }
   if(!tempy){
     printf("gmvwritetracersheader: malloc failed for tempy pointer.\n");
     printf("Requested number of floats = %f \n",(float)ntracers);
     printf("sizeof float = %d \n",sizeof(float));
     return;
   }
   if(!tempz){
     printf("gmvwritetracersheader: malloc failed for tempz pointer.\n");
     printf("Requested number of floats = %f \n",(float)ntracers);
     printf("sizeof float = %d \n",sizeof(float));
     return;
   }

 
  strcpy(tmpname, "tracers ");
 
#ifdef unicos
    {
     ieerror = CRAY2IEG(&typei, &num1, &tmpntracers, &num0, &ntracers, &num1);
     ieerror = CRAY2IEG(&typef, &ntracers, tempx, &num0, w, &num1);
     ieerror = CRAY2IEG(&typef, &ntracers, tempy, &num0, y, &num1);
     ieerror = CRAY2IEG(&typef, &ntracers, tempz, &num0, z, &num1);
    }
#else
    {
     tmpntracers = ntracers;
     for(count = 0; count< ntracers; count++)
      {
        tempx[count] = w[count];
        tempy[count] = y[count];
        tempz[count] = z[count];
      }
    }
#endif
 
  fwrite(tmpname, sizeof(char), 8, fp);
  fwrite(&tmpntracers, INT32, 1, fp);
  fwrite(tempx, FLOAT32, ntracers, fp);
  fwrite(tempy, FLOAT32, ntracers, fp);
  fwrite(tempz, FLOAT32, ntracers, fp);
  free(tempx);
  free(tempy);
  free(tempz);
 }
void gmvwritetracersnamedata(char tracername[], int ntracers, float data[])
 {
  float *tmpdata;
  int count;
 
  tmpdata = (float*)malloc(sizeof(float) * ntracers);
  if(!tmpdata){
     printf("gmvwritetracersnamedata: malloc failed for tmpdata pointer.\n");
     printf("Requested number of floats = %f \n",(float)ntracers);
     printf("sizeof float = %d \n",sizeof(float));
     return;
   }

 
#ifdef unicos
    {
     ieerror = CRAY2IEG(&typef, &ntracers, tmpdata, &num0, data, &num1);
    }
#else
    {
     for(count = 0; count < ntracers; count++)
      {
       tmpdata[count] = data[count];
      }
    }
#endif
 
  fwrite(tracername,sizeof(char),8,fp);
  fwrite(tmpdata,FLOAT32,ntracers,fp);
  free(tmpdata);
 }
 
void gmvwritetracersendtrace()
 {
  strcpy(tmpname, "endtrace");
  fwrite(tmpname, sizeof(char), 8, fp);
 }
 
void gmvwriteprobtime(float ptime)
 {
  float tmpptime;
  strcpy(tmpname, "probtime");
 
#ifdef unicos
    {
     ieerror = CRAY2IEG(&typef, &num1, &tmpptime, &num0, &ptime, &num1);
    }
#else
    {
     tmpptime = ptime;
    }
#endif
 
  fwrite(tmpname,sizeof(char),8,fp);
  fwrite(&tmpptime,FLOAT32,1,fp);
 }
 
void gmvwritecycleno(int cyclenum)
 {
  int tmpcyclenum;
  strcpy(tmpname, "cycleno ");
#ifdef unicos
    {
     CRAY2IEG(&typei, &num1, &tmpcyclenum, &num0, &cyclenum, &num1);
    }
#else
    {
     tmpcyclenum = cyclenum;
    }
#endif
 
  fwrite(tmpname,sizeof(char),8,fp);
  fwrite(&tmpcyclenum,INT32,1,fp);
 }
