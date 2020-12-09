C#######################################################################
C
C  PURPOSE
C     definitions for default mesh object, its attributes and
c     attribute parameters
c
C#######################################################################
c
c  CHANGE HISTORY
C  $Log: cmo_lg.h,v $
C  Revision 2.00  2007/11/05 19:45:49  spchu
C  Import to CVS
C
CPVCS    
CPVCS       Rev 1.3   10 Apr 2001 10:56:36   dcg
CPVCS    change number_of_default_attparam_names to number_of_default_attparam_name
CPVCS    so number of characters is less than 32
CPVCS
CPVCS       Rev 1.2   Fri Feb 04 16:35:24 2000   dcg
CPVCS
CPVCS       Rev 1.1   Mon Jan 31 17:21:14 2000   dcg
C
C#######################################################################
c
c  names of attribute parameters (i.e. definition of an attribute
      pointer(ipdefcmo_attparam_names,defcmo_attparam_names)
      character*32 defcmo_attparam_names(*)
c  names of mesh objects
      pointer(ipcmo_names,cmo_names)
      character*32 cmo_names(*)
c  number of attributes for mesh object
      pointer(ipcmo_natts,cmo_natts)
      integer cmo_natts(*)
c  list of attribute parameter values for a mesh object
      pointer (ipcmo_attlist,cmo_attlist)
      character*32 cmo_attlist(*)
c  list of values for the 'default' paramater for each attribute
c  for a mesh object - divided into integer,real and character types
      pointer(ipcmo_attparam_idefault,cmo_attparam_idefault)
      integer cmo_attparam_idefault(*)
      pointer(ipcmo_attparam_rdefault,cmo_attparam_rdefault)
      real*8 cmo_attparam_rdefault(*)
      pointer(ipcmo_attparam_cdefault,cmo_attparam_cdefault)
      character*32 cmo_attparam_cdefault(*)
      common/cmo_lg/ number_of_mesh_objects,number_of_params_per_att,
     *   current_cmo_index,number_of_default_attributes,
     *   number_of_default_attparam_name
      integer number_of_mesh_objects,number_of_params_per_att,
     *   current_cmo_index,number_of_default_attributes,
     *   number_of_default_attparam_name
 
