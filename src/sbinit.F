      subroutine sbinit
C#############################################
C
C     PURPOSE -
C
C        INITIALIZE (AND READ) ALL STORAGE BLOCKS THAT ARE USED IN THE
C           CODE.  EACH STORAGE IS GIVEN A UNIQUE NAME THAT CORRESPONDS
C           TO THE STORAGE BLOCK PATH AND THE MEMORY MANAGEMENT PATH.
C           FOR STORAGE BLOCK THAT ARE LOADED FROM EXTERNAL
C           DICTIONARIES A TWO PASS SCHEME IS USED.  FIRST, THE
C           DICTIONARY IS READ TO FIND HOW MUCH DATA (# OF WORDS)
C           ARE IN THE DICTIONARY.  THEN THE MEMORY IS SET UP
C           SO THAT WE CAN READ THE DICTIONARY INTO THE STORAGE
C           BLOCK ON THE SECOND PASS.
C
C
C     INPUT ARGUMENTS -
C
C        isbname - INTEGER ARRAY THAT CONTAINS THE NAMES OF STORAGE
C                  BLOCKS THAT WILL BE INITIALIZED.
C
C        nsbname - NUMBER OF STORAGE BLOCKS THAT WILL INITIALIZED
C
C
C     OUTPUT ARGUMENTS -
C
C        THE OUTPUT PARAMETERS FROM THIS ROUTINE ARE IN THE DICTIONARY
C        AND STORAGE BLOCK COMDECK.  MAINLY FLAGS ARE SET FOR VARIOUS
C        STORAGE BLOCKS TO INDICATE THAT THEY HAVE BEEN INITIALIZED.
C
C
C     CHANGE HISTORY -
C
C        $Log: sbinit.f,v $
C        Revision 2.00  2007/11/09 20:04:02  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.17   09 Nov 2005 09:25:06   dcg
CPVCS    return mbndry value to 16000000
CPVCS    
CPVCS       Rev 1.16   09 Nov 2005 09:21:56   dcg
CPVCS    add attributes to default mesh object for face sets
CPVCS    
CPVCS       Rev 1.15   10 Apr 2001 11:04:26   dcg
CPVCS    shorten too long name
CPVCS    
CPVCS       Rev 1.14   22 Jan 2001 12:57:34   dcg
CPVCS    set number of psetnames based on NBYTES_INT
CPVCS    
CPVCS       Rev 1.13   19 Apr 2000 10:10:16   dcg
CPVCS    change names of some attributes
CPVCS    
CPVCS       Rev 1.12   16 Mar 2000 15:16:38   dcg
CPVCS    use type=3 for cmo_names array
CPVCS    
CPVCS       Rev 1.11   Wed Mar 01 08:44:38 2000   dcg
CPVCS    use 'vels' as default velocity name
CPVCS    
CPVCS       Rev 1.10   Mon Feb 07 11:13:32 2000   dcg
C
C#######################################################################
C
      implicit none
      include 'global_lg.h'
      include 'cmo_lg.h'
      include 'geom_lg.h'
      include 'machine.h'
C
C#######################################################################
C
 
      integer len,ierror,inc,length,i
      character*32 isubname,partname
 
C
C#######################################################################
C begin
      isubname='sbinit'
c
c  initialize global common
c
      partname='global_lg'
      number_of_globals=7
      call mmfindbk('global_name',partname,ipglobal_name,len,ierror)
      if(ierror.ne.0) then
        call mmgetblk('global_name',partname,ipglobal_name,10,3,ierror)
        call mmgetblk('global_type',partname,ipglobal_type,10,1,ierror)
        call mmgetblk('global_index',partname,ipglobal_index,10,1,
     *     ierror)
        call mmgetblk('global_integer',partname,ipglobal_integer,20,1,
     *     ierror)
        call mmgetblk('global_real',partname,ipglobal_real,20,2,
     *     ierror)
        call mmgetblk('global_character',partname,ipglobal_character,20,
     *     3,ierror)
       endif
       global_name(1)='ihcycle'
       global_type(1)=1
       global_index(1)=1
       global_integer(1,1)=0
       global_integer(2,1)=0
       global_name(2)='time'
       global_type(2)=2
       global_index(2)=1
       global_real(1,1)=0
       global_real(2,1)=0
       global_name(3)='monitor'
       global_type(3)=1
       global_index(3)=2
       global_integer(1,2)=0
       global_integer(2,2)=0
       global_name(4)='hextotet_remove_volume'
       global_type(4)=3
       global_index(4)=1
       global_character(1,1)='no'
       global_character(2,1)='no'
       global_name(5)='hextotet_check_imt'
       global_type(5)=3
       global_index(5)=2
       global_character(1,2)='yes'
       global_character(2,2)='yes'
       global_name(6)='hextotet_radavg'
       global_type(6)=3
       global_index(6)=3
       global_character(1,3)='no'
       global_character(2,3)='no'
       global_name(7)='hextotet_remove_duplicates'
       global_type(7)=3
       global_index(7)=4
       global_character(1,4)='no'
       global_character(2,4)='no'
c
c  initialize geometry arrays
c
      number_of_geometries=1
      current_geom_name='-defaultgeom-'
      partname='geom_lg'
      call mmfindbk ('geom_names',partname,
     *  ipgeom_names,len,ierror)
      if(ierror.ne.0)
     *    call mmgetblk('geom_names',partname,
     *  ipgeom_names,10,3,ierror)
      call mmfindbk ('geom_info',partname,
     *  ipgeom_info,len,ierror)
      if(ierror.ne.0)
     *    call mmgetblk('geom_info',partname,
     *  ipgeom_info,80,1,ierror)
      geom_names(1)='-defaultgeom-'
      do i=1,8
        geom_info(i,1)=0
      enddo
 
C
c  initialize default mesh object
c
      partname='define_cmo_lg'
      call mmfindbk ('defcmo_attparam_names',partname,
     *  ipdefcmo_attparam_names,len,ierror)
      if(ierror.ne.0)
     *    call mmgetblk('defcmo_attparam_names',partname,
     *  ipdefcmo_attparam_names,8,3,ierror)
      number_of_default_attparam_name=8
      defcmo_attparam_names(1)='name'
      defcmo_attparam_names(2)='type'
      defcmo_attparam_names(3)='rank'
      defcmo_attparam_names(4)='length'
      defcmo_attparam_names(5)='interpolation'
      defcmo_attparam_names(6)='persistence'
      defcmo_attparam_names(7)='ioflag'
      defcmo_attparam_names(8)='default'
      call mmfindbk ('cmo_names',partname,
     *  ipcmo_names,len,ierror)
      if(ierror.ne.0)
     *    call mmgetblk('cmo_names',partname,
     *  ipcmo_names,10,3,ierror)
      cmo_names(1)='-default-'
      call mmfindbk ('cmo_natts',partname,
     *  ipcmo_natts,len,ierror)
      if(ierror.ne.0)
     *    call mmgetblk('cmo_natts',partname,
     *  ipcmo_natts,10,1,ierror)
      cmo_natts(1)=65
      number_of_default_attributes=65
      number_of_mesh_objects=1
      number_of_params_per_att=7
      partname='default_cmo_lg'
      call mmfindbk ('cmo_attlist',partname,
     *  ipcmo_attlist,len,ierror)
      if(ierror.ne.0) then
         length=number_of_default_attributes*number_of_params_per_att
         call mmgetblk('cmo_attlist',partname,
     *  ipcmo_attlist,length,3,ierror)
      endif
      call mmfindbk ('cmo_attparam_cdefault',partname,
     *  ipcmo_attparam_cdefault,len,ierror)
      if(ierror.ne.0) then
         length=number_of_default_attributes
         call mmgetblk('cmo_attparam_cdefault',partname,
     *   ipcmo_attparam_cdefault,length,3,ierror)
      endif
      call mmfindbk ('cmo_attparam_idefault',partname,
     *  ipcmo_attparam_idefault,len,ierror)
      if(ierror.ne.0) then
         length=number_of_default_attributes
         call mmgetblk('cmo_attparam_idefault',partname,
     *   ipcmo_attparam_idefault,length,1,ierror)
      endif
      call mmfindbk ('cmo_attparam_rdefault',partname,
     *  ipcmo_attparam_rdefault,len,ierror)
      if(ierror.ne.0) then
         length=number_of_default_attributes
         call mmgetblk('cmo_attparam_rdefault',partname,
     *   ipcmo_attparam_rdefault,length,2,ierror)
      endif
c  fill in parameter values for default mesh object attribute
c  for default attribute
      cmo_attlist(1)='-def-'
      cmo_attlist(2)='VDOUBLE'
      cmo_attlist(3)='scalar'
      cmo_attlist(4)='nnodes'
      cmo_attlist(5)='linear'
      cmo_attlist(6)='temporary'
      cmo_attlist(7)='agltn'
      cmo_attparam_cdefault(1)=' '
      cmo_attparam_rdefault(1)=0.
      cmo_attparam_idefault(1)=0
      inc=number_of_params_per_att
      cmo_attlist(1+inc)='scalar'
      cmo_attlist(2+inc)='INT'
      cmo_attlist(3+inc)='scalar'
      cmo_attlist(4+inc)='scalar'
      cmo_attlist(5+inc)='constant'
      cmo_attlist(6+inc)='permanent'
      cmo_attlist(7+inc)='l'
      cmo_attparam_cdefault(2)=' '
      cmo_attparam_rdefault(2)=1.
      cmo_attparam_idefault(2)=1
      inc=inc+number_of_params_per_att
      cmo_attlist(1+inc)='vector'
      cmo_attlist(2+inc)='INT'
      cmo_attlist(3+inc)='scalar'
      cmo_attlist(4+inc)='scalar'
      cmo_attlist(5+inc)='constant'
      cmo_attlist(6+inc)='permanent'
      cmo_attlist(7+inc)='l'
      cmo_attparam_cdefault(3)=' '
      cmo_attparam_rdefault(3)=3.
      cmo_attparam_idefault(3)=3
      inc=inc+number_of_params_per_att
      cmo_attlist(1+inc)='nnodes'
      cmo_attlist(2+inc)='INT'
      cmo_attlist(3+inc)='scalar'
      cmo_attlist(4+inc)='scalar'
      cmo_attlist(5+inc)='constant'
      cmo_attlist(6+inc)='permanent'
      cmo_attlist(7+inc)='l'
      cmo_attparam_cdefault(4)=' '
      cmo_attparam_rdefault(4)=0.
      cmo_attparam_idefault(4)=0
      inc=inc+number_of_params_per_att
      cmo_attlist(1+inc)='nedges'
      cmo_attlist(2+inc)='INT'
      cmo_attlist(3+inc)='scalar'
      cmo_attlist(4+inc)='scalar'
      cmo_attlist(5+inc)='constant'
      cmo_attlist(6+inc)='permanent'
      cmo_attlist(7+inc)='l'
      cmo_attparam_cdefault(5)=' '
      cmo_attparam_rdefault(5)=0.
      cmo_attparam_idefault(5)=0
      inc=inc+number_of_params_per_att
      cmo_attlist(1+inc)='nfaces'
      cmo_attlist(2+inc)='INT'
      cmo_attlist(3+inc)='scalar'
      cmo_attlist(4+inc)='scalar'
      cmo_attlist(5+inc)='constant'
      cmo_attlist(6+inc)='permanent'
      cmo_attlist(7+inc)='l'
      cmo_attparam_cdefault(6)=' '
      cmo_attparam_rdefault(6)=0.
      cmo_attparam_idefault(6)=0
      inc=inc+number_of_params_per_att
      cmo_attlist(1+inc)='nelements'
      cmo_attlist(2+inc)='INT'
      cmo_attlist(3+inc)='scalar'
      cmo_attlist(4+inc)='scalar'
      cmo_attlist(5+inc)='constant'
      cmo_attlist(6+inc)='permanent'
      cmo_attlist(7+inc)='l'
      cmo_attparam_cdefault(7)=' '
      cmo_attparam_rdefault(7)=0.
      cmo_attparam_idefault(7)=0
      inc=inc+number_of_params_per_att
      cmo_attlist(1+inc)='mbndry'
      cmo_attlist(2+inc)='INT'
      cmo_attlist(3+inc)='scalar'
      cmo_attlist(4+inc)='scalar'
      cmo_attlist(5+inc)='constant'
      cmo_attlist(6+inc)='permanent'
      cmo_attlist(7+inc)='l'
      cmo_attparam_cdefault(8)=' '
      cmo_attparam_rdefault(8)=16000000.
      cmo_attparam_idefault(8)=16000000
      inc=inc+number_of_params_per_att
      cmo_attlist(1+inc)='ndimensions_topo'
      cmo_attlist(2+inc)='INT'
      cmo_attlist(3+inc)='scalar'
      cmo_attlist(4+inc)='scalar'
      cmo_attlist(5+inc)='constant'
      cmo_attlist(6+inc)='permanent'
      cmo_attlist(7+inc)='l'
      cmo_attparam_cdefault(9)=' '
      cmo_attparam_rdefault(9)=3.
      cmo_attparam_idefault(9)=3
      inc=inc+number_of_params_per_att
      cmo_attlist(1+inc)='ndimensions_geom'
      cmo_attlist(2+inc)='INT'
      cmo_attlist(3+inc)='scalar'
      cmo_attlist(4+inc)='scalar'
      cmo_attlist(5+inc)='constant'
      cmo_attlist(6+inc)='permanent'
      cmo_attlist(7+inc)='l'
      cmo_attparam_cdefault(10)=' '
      cmo_attparam_rdefault(10)=3.
      cmo_attparam_idefault(10)=3
      inc=inc+number_of_params_per_att
      cmo_attlist(1+inc)='nodes_per_element'
      cmo_attlist(2+inc)='INT'
      cmo_attlist(3+inc)='scalar'
      cmo_attlist(4+inc)='scalar'
      cmo_attlist(5+inc)='constant'
      cmo_attlist(6+inc)='permanent'
      cmo_attlist(7+inc)='l'
      cmo_attparam_cdefault(11)=' '
      cmo_attparam_rdefault(11)=4.
      cmo_attparam_idefault(11)=4
      inc=inc+number_of_params_per_att
      cmo_attlist(1+inc)='edges_per_element'
      cmo_attlist(2+inc)='INT'
      cmo_attlist(3+inc)='scalar'
      cmo_attlist(4+inc)='scalar'
      cmo_attlist(5+inc)='constant'
      cmo_attlist(6+inc)='permanent'
      cmo_attlist(7+inc)='l'
      cmo_attparam_cdefault(12)=' '
      cmo_attparam_rdefault(12)=6.
      cmo_attparam_idefault(12)=6
      inc=inc+number_of_params_per_att
      cmo_attlist(1+inc)='faces_per_element'
      cmo_attlist(2+inc)='INT'
      cmo_attlist(3+inc)='scalar'
      cmo_attlist(4+inc)='scalar'
      cmo_attlist(5+inc)='constant'
      cmo_attlist(6+inc)='permanent'
      cmo_attlist(7+inc)='l'
      cmo_attparam_cdefault(13)=' '
      cmo_attparam_rdefault(13)=4.
      cmo_attparam_idefault(13)=4
      inc=inc+number_of_params_per_att
      cmo_attlist(1+inc)='isetwd'
      cmo_attlist(2+inc)='VDOUBLE'
      cmo_attlist(3+inc)='scalar'
      cmo_attlist(4+inc)='nnodes'
      cmo_attlist(5+inc)='or'
      cmo_attlist(6+inc)='permanent'
      cmo_attlist(7+inc)='l'
      cmo_attparam_cdefault(14)=' '
      cmo_attparam_rdefault(14)=0.
      cmo_attparam_idefault(14)=0
      inc=inc+number_of_params_per_att
      cmo_attlist(1+inc)='ialias'
      cmo_attlist(2+inc)='VINT'
      cmo_attlist(3+inc)='scalar'
      cmo_attlist(4+inc)='nnodes'
      cmo_attlist(5+inc)='sequence'
      cmo_attlist(6+inc)='permanent'
      cmo_attlist(7+inc)='l'
      cmo_attparam_cdefault(15)=' '
      cmo_attparam_rdefault(15)=0.
      cmo_attparam_idefault(15)=0
      inc=inc+number_of_params_per_att
      cmo_attlist(1+inc)='imt1'
      cmo_attlist(2+inc)='VINT'
      cmo_attlist(3+inc)='scalar'
      cmo_attlist(4+inc)='nnodes'
      cmo_attlist(5+inc)='max'
      cmo_attlist(6+inc)='permanent'
      cmo_attlist(7+inc)='gal'
      cmo_attparam_cdefault(16)=' '
      cmo_attparam_rdefault(16)=0.
      cmo_attparam_idefault(16)=0
      inc=inc+number_of_params_per_att
      cmo_attlist(1+inc)='itp1'
      cmo_attlist(2+inc)='VINT'
      cmo_attlist(3+inc)='scalar'
      cmo_attlist(4+inc)='nnodes'
      cmo_attlist(5+inc)='min'
      cmo_attlist(6+inc)='permanent'
      cmo_attlist(7+inc)='gal'
      cmo_attparam_cdefault(17)=' '
      cmo_attparam_rdefault(17)=0.
      cmo_attparam_idefault(17)=0
      inc=inc+number_of_params_per_att
      cmo_attlist(1+inc)='icr1'
      cmo_attlist(2+inc)='VINT'
      cmo_attlist(3+inc)='scalar'
      cmo_attlist(4+inc)='nnodes'
      cmo_attlist(5+inc)='min'
      cmo_attlist(6+inc)='permanent'
      cmo_attlist(7+inc)='gal'
      cmo_attparam_cdefault(18)=' '
      cmo_attparam_rdefault(18)=0.
      cmo_attparam_idefault(18)=0
      inc=inc+number_of_params_per_att
      cmo_attlist(1+inc)='isn1'
      cmo_attlist(2+inc)='VINT'
      cmo_attlist(3+inc)='scalar'
      cmo_attlist(4+inc)='nnodes'
      cmo_attlist(5+inc)='user'
      cmo_attlist(6+inc)='permanent'
      cmo_attlist(7+inc)='gal'
      cmo_attparam_cdefault(19)=' '
      cmo_attparam_rdefault(19)=0.
      cmo_attparam_idefault(19)=0
      inc=inc+number_of_params_per_att
      cmo_attlist(1+inc)='xic'
      cmo_attlist(2+inc)='VDOUBLE'
      cmo_attlist(3+inc)='scalar'
      cmo_attlist(4+inc)='nnodes'
      cmo_attlist(5+inc)='linear'
      cmo_attlist(6+inc)='permanent'
      cmo_attlist(7+inc)='l'
      cmo_attparam_cdefault(20)=' '
      cmo_attparam_rdefault(20)=0.
      cmo_attparam_idefault(20)=0
      inc=inc+number_of_params_per_att
      cmo_attlist(1+inc)='yic'
      cmo_attlist(2+inc)='VDOUBLE'
      cmo_attlist(3+inc)='scalar'
      cmo_attlist(4+inc)='nnodes'
      cmo_attlist(5+inc)='linear'
      cmo_attlist(6+inc)='permanent'
      cmo_attlist(7+inc)='l'
      cmo_attparam_cdefault(21)=' '
      cmo_attparam_rdefault(21)=0.
      cmo_attparam_idefault(21)=0
      inc=inc+number_of_params_per_att
      cmo_attlist(1+inc)='zic'
      cmo_attlist(2+inc)='VDOUBLE'
      cmo_attlist(3+inc)='scalar'
      cmo_attlist(4+inc)='nnodes'
      cmo_attlist(5+inc)='linear'
      cmo_attlist(6+inc)='permanent'
      cmo_attlist(7+inc)='l'
      cmo_attparam_cdefault(22)=' '
      cmo_attparam_rdefault(22)=0.
      cmo_attparam_idefault(22)=0
      inc=inc+number_of_params_per_att
      cmo_attlist(1+inc)='xtetwd'
      cmo_attlist(2+inc)='VDOUBLE'
      cmo_attlist(3+inc)='scalar'
      cmo_attlist(4+inc)='nelements'
      cmo_attlist(5+inc)='or'
      cmo_attlist(6+inc)='permanent'
      cmo_attlist(7+inc)='l'
      cmo_attparam_cdefault(23)=' '
      cmo_attparam_rdefault(23)=0.
      cmo_attparam_idefault(23)=0
      inc=inc+number_of_params_per_att
      cmo_attlist(1+inc)='itetclr'
      cmo_attlist(2+inc)='VINT'
      cmo_attlist(3+inc)='scalar'
      cmo_attlist(4+inc)='nelements'
      cmo_attlist(5+inc)='user'
      cmo_attlist(6+inc)='permanent'
      cmo_attlist(7+inc)='l'
      cmo_attparam_cdefault(24)=' '
      cmo_attparam_rdefault(24)=0.
      cmo_attparam_idefault(24)=0
      inc=inc+number_of_params_per_att
      cmo_attlist(1+inc)='itettyp'
      cmo_attlist(2+inc)='VINT'
      cmo_attlist(3+inc)='scalar'
      cmo_attlist(4+inc)='nelements'
      cmo_attlist(5+inc)='user'
      cmo_attlist(6+inc)='permanent'
      cmo_attlist(7+inc)='l'
      cmo_attparam_cdefault(25)=' '
      cmo_attparam_rdefault(25)=0.
      cmo_attparam_idefault(25)=0
      inc=inc+number_of_params_per_att
      cmo_attlist(1+inc)='itetoff'
      cmo_attlist(2+inc)='VINT'
      cmo_attlist(3+inc)='scalar'
      cmo_attlist(4+inc)='nelements'
      cmo_attlist(5+inc)='user'
      cmo_attlist(6+inc)='permanent'
      cmo_attlist(7+inc)='l'
      cmo_attparam_cdefault(26)=' '
      cmo_attparam_rdefault(26)=0.
      cmo_attparam_idefault(26)=0
      inc=inc+number_of_params_per_att
      cmo_attlist(1+inc)='jtetoff'
      cmo_attlist(2+inc)='VINT'
      cmo_attlist(3+inc)='scalar'
      cmo_attlist(4+inc)='nelements'
      cmo_attlist(5+inc)='user'
      cmo_attlist(6+inc)='permanent'
      cmo_attlist(7+inc)='l'
      cmo_attparam_cdefault(27)=' '
      cmo_attparam_rdefault(27)=0.
      cmo_attparam_idefault(27)=0
      inc=inc+number_of_params_per_att
      cmo_attlist(1+inc)='itet'
      cmo_attlist(2+inc)='VINT'
      cmo_attlist(3+inc)='nodes_per_element'
      cmo_attlist(4+inc)='nelements'
      cmo_attlist(5+inc)='user'
      cmo_attlist(6+inc)='permanent'
      cmo_attlist(7+inc)='l'
      cmo_attparam_cdefault(28)=' '
      cmo_attparam_rdefault(28)=0.
      cmo_attparam_idefault(28)=0
      inc=inc+number_of_params_per_att
      cmo_attlist(1+inc)='jtet'
      cmo_attlist(2+inc)='VINT'
      cmo_attlist(3+inc)='faces_per_element'
      cmo_attlist(4+inc)='nelements'
      cmo_attlist(5+inc)='user'
      cmo_attlist(6+inc)='permanent'
      cmo_attlist(7+inc)='l'
      cmo_attparam_cdefault(29)=' '
      cmo_attparam_rdefault(29)=0.
      cmo_attparam_idefault(29)=0
      inc=inc+number_of_params_per_att
      cmo_attlist(1+inc)='ipolydat'
      cmo_attlist(2+inc)='CHARACTER'
      cmo_attlist(3+inc)='scalar'
      cmo_attlist(4+inc)='scalar'
      cmo_attlist(5+inc)='constant'
      cmo_attlist(6+inc)='permanent'
      cmo_attlist(7+inc)='l'
      cmo_attparam_cdefault(30)='no  '
      cmo_attparam_rdefault(30)=0.
      cmo_attparam_idefault(30)=0
      inc=inc+number_of_params_per_att
      cmo_attlist(1+inc)='vor2d'
      cmo_attlist(2+inc)='CHARACTER'
      cmo_attlist(3+inc)='scalar'
      cmo_attlist(4+inc)='scalar'
      cmo_attlist(5+inc)='constant'
      cmo_attlist(6+inc)='permanent'
      cmo_attlist(7+inc)='l'
      cmo_attparam_cdefault(31)='yes '
      cmo_attparam_rdefault(31)=0.
      cmo_attparam_idefault(31)=0
      inc=inc+number_of_params_per_att
      cmo_attlist(1+inc)='vor3d'
      cmo_attlist(2+inc)='CHARACTER'
      cmo_attlist(3+inc)='scalar'
      cmo_attlist(4+inc)='scalar'
      cmo_attlist(5+inc)='constant'
      cmo_attlist(6+inc)='permanent'
      cmo_attlist(7+inc)='l'
      cmo_attparam_cdefault(32)='no '
      cmo_attparam_rdefault(32)=0.
      cmo_attparam_idefault(32)=0
      inc=inc+number_of_params_per_att
      cmo_attlist(1+inc)='epsilon'
      cmo_attlist(2+inc)='REAL'
      cmo_attlist(3+inc)='scalar'
      cmo_attlist(4+inc)='scalar'
      cmo_attlist(5+inc)='constant'
      cmo_attlist(6+inc)='permanent'
      cmo_attlist(7+inc)='l'
      cmo_attparam_cdefault(33)=' '
      cmo_attparam_rdefault(33)=1.e-15
      cmo_attparam_idefault(33)=0
      inc=inc+number_of_params_per_att
      cmo_attlist(1+inc)='epsilonl'
      cmo_attlist(2+inc)='REAL'
      cmo_attlist(3+inc)='scalar'
      cmo_attlist(4+inc)='scalar'
      cmo_attlist(5+inc)='constant'
      cmo_attlist(6+inc)='permanent'
      cmo_attlist(7+inc)='l'
      cmo_attparam_cdefault(34)=' '
      cmo_attparam_rdefault(34)=1.e-8
      cmo_attparam_idefault(34)=0
      inc=inc+number_of_params_per_att
      cmo_attlist(1+inc)='epsilona'
      cmo_attlist(2+inc)='REAL'
      cmo_attlist(3+inc)='scalar'
      cmo_attlist(4+inc)='scalar'
      cmo_attlist(5+inc)='constant'
      cmo_attlist(6+inc)='permanent'
      cmo_attlist(7+inc)='l'
      cmo_attparam_cdefault(35)=' '
      cmo_attparam_rdefault(35)=1.e-8
      cmo_attparam_idefault(35)=0
      inc=inc+number_of_params_per_att
      cmo_attlist(1+inc)='epsilonv'
      cmo_attlist(2+inc)='REAL'
      cmo_attlist(3+inc)='scalar'
      cmo_attlist(4+inc)='scalar'
      cmo_attlist(5+inc)='constant'
      cmo_attlist(6+inc)='permanent'
      cmo_attlist(7+inc)='l'
      cmo_attparam_cdefault(36)=' '
      cmo_attparam_rdefault(36)=1.e-8
      cmo_attparam_idefault(36)=0
      inc=inc+number_of_params_per_att
      cmo_attlist(1+inc)='ipointi'
      cmo_attlist(2+inc)='INT'
      cmo_attlist(3+inc)='scalar'
      cmo_attlist(4+inc)='scalar'
      cmo_attlist(5+inc)='constant'
      cmo_attlist(6+inc)='permanent'
      cmo_attlist(7+inc)='l'
      cmo_attparam_cdefault(37)=' '
      cmo_attparam_rdefault(37)=0.
      cmo_attparam_idefault(37)=0
      inc=inc+number_of_params_per_att
      cmo_attlist(1+inc)='ipointj'
      cmo_attlist(2+inc)='INT'
      cmo_attlist(3+inc)='scalar'
      cmo_attlist(4+inc)='scalar'
      cmo_attlist(5+inc)='constant'
      cmo_attlist(6+inc)='permanent'
      cmo_attlist(7+inc)='l'
      cmo_attparam_cdefault(38)=' '
      cmo_attparam_rdefault(38)=0.
      cmo_attparam_idefault(38)=0
      inc=inc+number_of_params_per_att
      cmo_attlist(1+inc)='idebug'
      cmo_attlist(2+inc)='INT'
      cmo_attlist(3+inc)='scalar'
      cmo_attlist(4+inc)='scalar'
      cmo_attlist(5+inc)='constant'
      cmo_attlist(6+inc)='permanent'
      cmo_attlist(7+inc)='l'
      cmo_attparam_cdefault(39)=' '
      cmo_attparam_rdefault(39)=0.
      cmo_attparam_idefault(39)=0
      inc=inc+number_of_params_per_att
      cmo_attlist(1+inc)='itypconv_sm'
      cmo_attlist(2+inc)='INT'
      cmo_attlist(3+inc)='scalar'
      cmo_attlist(4+inc)='scalar'
      cmo_attlist(5+inc)='constant'
      cmo_attlist(6+inc)='permanent'
      cmo_attlist(7+inc)='l'
      cmo_attparam_cdefault(40)=' '
      cmo_attparam_rdefault(40)=1.
      cmo_attparam_idefault(40)=1
      inc=inc+number_of_params_per_att
      cmo_attlist(1+inc)='maxiter_sm'
      cmo_attlist(2+inc)='INT'
      cmo_attlist(3+inc)='scalar'
      cmo_attlist(4+inc)='scalar'
      cmo_attlist(5+inc)='constant'
      cmo_attlist(6+inc)='permanent'
      cmo_attlist(7+inc)='l'
      cmo_attparam_cdefault(41)=' '
      cmo_attparam_rdefault(41)=25.
      cmo_attparam_idefault(41)=25
      inc=inc+number_of_params_per_att
      cmo_attlist(1+inc)='tolconv_sm'
      cmo_attlist(2+inc)='REAL'
      cmo_attlist(3+inc)='scalar'
      cmo_attlist(4+inc)='scalar'
      cmo_attlist(5+inc)='constant'
      cmo_attlist(6+inc)='permanent'
      cmo_attlist(7+inc)='l'
      cmo_attparam_cdefault(42)=' '
      cmo_attparam_rdefault(42)=1.
      cmo_attparam_idefault(42)=1
      inc=inc+number_of_params_per_att
      cmo_attlist(1+inc)='nnfreq'
      cmo_attlist(2+inc)='INT'
      cmo_attlist(3+inc)='scalar'
      cmo_attlist(4+inc)='scalar'
      cmo_attlist(5+inc)='constant'
      cmo_attlist(6+inc)='permanent'
      cmo_attlist(7+inc)='l'
      cmo_attparam_cdefault(43)=' '
      cmo_attparam_rdefault(43)=1.
      cmo_attparam_idefault(43)=1
      inc=inc+number_of_params_per_att
      cmo_attlist(1+inc)='ivoronoi'
      cmo_attlist(2+inc)='INT'
      cmo_attlist(3+inc)='scalar'
      cmo_attlist(4+inc)='scalar'
      cmo_attlist(5+inc)='constant'
      cmo_attlist(6+inc)='permanent'
      cmo_attlist(7+inc)='l'
      cmo_attparam_cdefault(44)=' '
      cmo_attparam_rdefault(44)=1.
      cmo_attparam_idefault(44)=1
      inc=inc+number_of_params_per_att
      cmo_attlist(1+inc)='iopt2to2'
      cmo_attlist(2+inc)='INT'
      cmo_attlist(3+inc)='scalar'
      cmo_attlist(4+inc)='scalar'
      cmo_attlist(5+inc)='constant'
      cmo_attlist(6+inc)='permanent'
      cmo_attlist(7+inc)='l'
      cmo_attparam_cdefault(45)=' '
      cmo_attparam_rdefault(45)=2.
      cmo_attparam_idefault(45)=2
      inc=inc+number_of_params_per_att
      cmo_attlist(1+inc)='dumptype'
      cmo_attlist(2+inc)='CHARACTER'
      cmo_attlist(3+inc)='scalar'
      cmo_attlist(4+inc)='scalar'
      cmo_attlist(5+inc)='constant'
      cmo_attlist(6+inc)='permanent'
      cmo_attlist(7+inc)='l'
      cmo_attparam_cdefault(46)='binary '
      cmo_attparam_rdefault(46)=0.
      cmo_attparam_idefault(46)=0
      inc=inc+number_of_params_per_att
      cmo_attlist(1+inc)='velname'
      cmo_attlist(2+inc)='CHARACTER'
      cmo_attlist(3+inc)='scalar'
      cmo_attlist(4+inc)='scalar'
      cmo_attlist(5+inc)='constant'
      cmo_attlist(6+inc)='permanent'
      cmo_attlist(7+inc)='l'
      cmo_attparam_cdefault(47)='vels '
      cmo_attparam_rdefault(47)=0.
      cmo_attparam_idefault(47)=0
      inc=inc+number_of_params_per_att
      cmo_attlist(1+inc)='densname'
      cmo_attlist(2+inc)='CHARACTER'
      cmo_attlist(3+inc)='scalar'
      cmo_attlist(4+inc)='scalar'
      cmo_attlist(5+inc)='constant'
      cmo_attlist(6+inc)='permanent'
      cmo_attlist(7+inc)='l'
      cmo_attparam_cdefault(48)='ric'
      cmo_attparam_rdefault(48)=0.
      cmo_attparam_idefault(48)=0
      inc=inc+number_of_params_per_att
      cmo_attlist(1+inc)='presname'
      cmo_attlist(2+inc)='CHARACTER'
      cmo_attlist(3+inc)='scalar'
      cmo_attlist(4+inc)='scalar'
      cmo_attlist(5+inc)='constant'
      cmo_attlist(6+inc)='permanent'
      cmo_attlist(7+inc)='l'
      cmo_attparam_cdefault(49)='pic'
      cmo_attparam_rdefault(49)=0.
      cmo_attparam_idefault(49)=0
      inc=inc+number_of_params_per_att
      cmo_attlist(1+inc)='enername'
      cmo_attlist(2+inc)='CHARACTER'
      cmo_attlist(3+inc)='scalar'
      cmo_attlist(4+inc)='scalar'
      cmo_attlist(5+inc)='constant'
      cmo_attlist(6+inc)='permanent'
      cmo_attlist(7+inc)='l'
      cmo_attparam_cdefault(50)='eic'
      cmo_attparam_rdefault(50)=0.
      cmo_attparam_idefault(50)=0
      inc=inc+number_of_params_per_att
      cmo_attlist(1+inc)='xmin'
      cmo_attlist(2+inc)='REAL'
      cmo_attlist(3+inc)='scalar'
      cmo_attlist(4+inc)='scalar'
      cmo_attlist(5+inc)='constant'
      cmo_attlist(6+inc)='permanent'
      cmo_attlist(7+inc)='l'
      cmo_attparam_cdefault(51)=' '
      cmo_attparam_rdefault(51)=-1.
      cmo_attparam_idefault(51)=-1
      inc=inc+number_of_params_per_att
      cmo_attlist(1+inc)='ymin'
      cmo_attlist(2+inc)='REAL'
      cmo_attlist(3+inc)='scalar'
      cmo_attlist(4+inc)='scalar'
      cmo_attlist(5+inc)='constant'
      cmo_attlist(6+inc)='permanent'
      cmo_attlist(7+inc)='l'
      cmo_attparam_cdefault(52)=' '
      cmo_attparam_rdefault(52)=-1.
      cmo_attparam_idefault(52)=-1
      inc=inc+number_of_params_per_att
      cmo_attlist(1+inc)='zmin'
      cmo_attlist(2+inc)='REAL'
      cmo_attlist(3+inc)='scalar'
      cmo_attlist(4+inc)='scalar'
      cmo_attlist(5+inc)='constant'
      cmo_attlist(6+inc)='permanent'
      cmo_attlist(7+inc)='l'
      cmo_attparam_cdefault(53)=' '
      cmo_attparam_rdefault(53)=-1.
      cmo_attparam_idefault(53)=-1
      inc=inc+number_of_params_per_att
      cmo_attlist(1+inc)='xmax'
      cmo_attlist(2+inc)='REAL'
      cmo_attlist(3+inc)='scalar'
      cmo_attlist(4+inc)='scalar'
      cmo_attlist(5+inc)='constant'
      cmo_attlist(6+inc)='permanent'
      cmo_attlist(7+inc)='l'
      cmo_attparam_cdefault(54)=' '
      cmo_attparam_rdefault(54)=1.
      cmo_attparam_idefault(54)=1
      inc=inc+number_of_params_per_att
      cmo_attlist(1+inc)='ymax'
      cmo_attlist(2+inc)='REAL'
      cmo_attlist(3+inc)='scalar'
      cmo_attlist(4+inc)='scalar'
      cmo_attlist(5+inc)='constant'
      cmo_attlist(6+inc)='permanent'
      cmo_attlist(7+inc)='l'
      cmo_attparam_cdefault(55)=' '
      cmo_attparam_rdefault(55)=1.
      cmo_attparam_idefault(55)=1
      inc=inc+number_of_params_per_att
      cmo_attlist(1+inc)='zmax'
      cmo_attlist(2+inc)='REAL'
      cmo_attlist(3+inc)='scalar'
      cmo_attlist(4+inc)='scalar'
      cmo_attlist(5+inc)='constant'
      cmo_attlist(6+inc)='permanent'
      cmo_attlist(7+inc)='l'
      cmo_attparam_cdefault(56)=' '
      cmo_attparam_rdefault(56)=1.
      cmo_attparam_idefault(56)=1
      inc=inc+number_of_params_per_att
      cmo_attlist(1+inc)='kdtree_level'
      cmo_attlist(2+inc)='INT'
      cmo_attlist(3+inc)='scalar'
      cmo_attlist(4+inc)='scalar'
      cmo_attlist(5+inc)='constant'
      cmo_attlist(6+inc)='permanent'
      cmo_attlist(7+inc)='l'
      cmo_attparam_cdefault(57)=' '
      cmo_attparam_rdefault(57)=0.
      cmo_attparam_idefault(57)=0
      inc=inc+number_of_params_per_att
      cmo_attlist(1+inc)='max_number_sets'
      cmo_attlist(2+inc)='INT'
      cmo_attlist(3+inc)='scalar'
      cmo_attlist(4+inc)='scalar'
      cmo_attlist(5+inc)='constant'
      cmo_attlist(6+inc)='permanent'
      cmo_attlist(7+inc)='l'
      cmo_attparam_cdefault(58)=' '
      cmo_attparam_rdefault(58)=dble(8*NBYTES_INT)
      cmo_attparam_idefault(58)=8*NBYTES_INT
      inc=inc+number_of_params_per_att
      cmo_attlist(1+inc)='number_of_psets'
      cmo_attlist(2+inc)='INT'
      cmo_attlist(3+inc)='scalar'
      cmo_attlist(4+inc)='scalar'
      cmo_attlist(5+inc)='constant'
      cmo_attlist(6+inc)='permanent'
      cmo_attlist(7+inc)='l'
      cmo_attparam_cdefault(59)=' '
      cmo_attparam_rdefault(59)=0.
      cmo_attparam_idefault(59)=0
      inc=inc+number_of_params_per_att
      cmo_attlist(1+inc)='number_of_eltsets'
      cmo_attlist(2+inc)='INT'
      cmo_attlist(3+inc)='scalar'
      cmo_attlist(4+inc)='scalar'
      cmo_attlist(5+inc)='constant'
      cmo_attlist(6+inc)='permanent'
      cmo_attlist(7+inc)='l'
      cmo_attparam_cdefault(60)=' '
      cmo_attparam_rdefault(60)=0.
      cmo_attparam_idefault(60)=0
      inc=inc+number_of_params_per_att
      cmo_attlist(1+inc)='psetnames'
      cmo_attlist(2+inc)='VCHAR'
      cmo_attlist(3+inc)='scalar'
      cmo_attlist(4+inc)='max_number_sets'
      cmo_attlist(5+inc)='constant'
      cmo_attlist(6+inc)='permanent'
      cmo_attlist(7+inc)='l'
      cmo_attparam_cdefault(61)=' '
      cmo_attparam_rdefault(61)=0.
      cmo_attparam_idefault(61)=0
      inc=inc+number_of_params_per_att
      cmo_attlist(1+inc)='eltsetnames'
      cmo_attlist(2+inc)='VCHAR'
      cmo_attlist(3+inc)='scalar'
      cmo_attlist(4+inc)='max_number_sets'
      cmo_attlist(5+inc)='constant'
      cmo_attlist(6+inc)='permanent'
      cmo_attlist(7+inc)='l'
      cmo_attparam_cdefault(62)=' '
      cmo_attparam_rdefault(62)=0.
      cmo_attparam_idefault(62)=0
      inc=inc+number_of_params_per_att
      cmo_attlist(1+inc)='geom_name'
      cmo_attlist(2+inc)='CHARACTER'
      cmo_attlist(3+inc)='scalar'
      cmo_attlist(4+inc)='max_number_sets'
      cmo_attlist(5+inc)='constant'
      cmo_attlist(6+inc)='permanent'
      cmo_attlist(7+inc)='l'
      cmo_attparam_cdefault(63)='-defaultgeom-'
      cmo_attparam_rdefault(63)=0.
      cmo_attparam_idefault(63)=0
c
      inc=inc+number_of_params_per_att
      cmo_attlist(1+inc)='fsetnames'
      cmo_attlist(2+inc)='VCHAR'
      cmo_attlist(3+inc)='scalar'
      cmo_attlist(4+inc)='max_number_sets'
      cmo_attlist(5+inc)='constant'
      cmo_attlist(6+inc)='permanent'
      cmo_attlist(7+inc)='l'
      cmo_attparam_cdefault(64)=' '
      cmo_attparam_rdefault(64)=0.
      cmo_attparam_idefault(64)=0
c
      inc=inc+number_of_params_per_att
      cmo_attlist(1+inc)='number_of_fsets'
      cmo_attlist(2+inc)='INT'
      cmo_attlist(3+inc)='scalar'
      cmo_attlist(4+inc)='scalar'
      cmo_attlist(5+inc)='constant'
      cmo_attlist(6+inc)='permanent'
      cmo_attlist(7+inc)='l'
      cmo_attparam_cdefault(65)=' '
      cmo_attparam_rdefault(65)=0.
      cmo_attparam_idefault(65)=0
c
c      inc=inc+number_of_params_per_att
c      cmo_attlist(1+inc)='ftet'
c      cmo_attlist(2+inc)='VINT'
c      cmo_attlist(3+inc)='faces_per_element'
c      cmo_attlist(4+inc)='nelements'
c      cmo_attlist(5+inc)='copy'
c      cmo_attlist(6+inc)='permanent'
c      cmo_attlist(7+inc)='l'
c      cmo_attparam_cdefault(66)=' '
c      cmo_attparam_rdefault(66)=0.
c      cmo_attparam_idefault(66)=0
c
      return
      end
