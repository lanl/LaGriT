CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     This module is a collection of interfaces to C/C++ functions.
C     To use in a Fortran source file, add the following below the
C     subroutine declaration and above the `implicit none` line
C     (in other words, these should be the first lines above any
C     other lines in a subroutine/function):
C
C      use, intrinsic :: ISO_C_BINDING, only: C_INT, C_DOUBLE
C      use c2f_interface
C
C     where `C_INT, C_DOUBLE` should be changed according to the
C     C bindings that you will need to pass variables in/out.
C
C     While you can get away without doing this on a *nix OS with GCC
C     compilers, with other compilers and different OS linkers
C     (*cough* Windows) you may not be so lucky.
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     BINDING EXAMPLES:
C     int x  -> INTEGER(C_INT), VALUE :: x
C     int *y -> INTEGER(C_INT) :: y
C       (or) -> TYPE (C_PTR) :: y
C     void *sendbuf -> TYPE(C_PTR), VALUE :: sendbuf

          integer function dotask_c(command)
            implicit none
            character*(*), intent(in) :: command
            integer :: ierror

            ierror = 0
            print*,'working'
            call dotask(command, ierror)

            dotask_c = ierror
          end function

      module c2f_interface
        interface

          integer(kind=c_int) function dotask_c
     &    (command)
     &    bind(C, name="dotask_c")
            use, intrinsic :: iso_c_binding, only: c_char, 
     &      c_null_char, c_size_t, c_int
            character(kind=c_char), dimension(*), intent(in) :: 
     &      command
          end function 

          subroutine initlagrit(mode, log_file, batch_file) 
     &    bind(C, name="initlagrit")
            use, intrinsic :: iso_c_binding, only: c_char, 
     &      c_null_char, c_size_t
            character(kind=c_char), dimension(*), intent(in) :: 
     &      mode, log_file, batch_file
          end subroutine

C          subroutine dotask(task_buff, ierror)
C     &    bind(C, name="dotask_")
C            use, intrinsic :: iso_c_binding, only: c_char, 
C     &      c_null_char, c_size_t, c_int
C            character(kind=c_char), dimension(*), intent(in) :: 
C     &      task_buff
C            integer, intent(out) :: ierror
C          end subroutine

C          subroutine cmo_get_info(ioption,cmo_name,ipout,
C     &    lout,itype,ierror_return)
C     &    bind(C, name="cmo_get_info")
C            use, intrinsic :: iso_c_binding, only: c_char, 
C     &      c_null_char, c_size_t, c_int, c_ptr, C_DOUBLE
C            character(kind=c_char), dimension(*), intent(in) :: 
C     &      ioption, cmo_name
C
C           type(c_ptr), intent(out) :: ipout
C            real(C_DOUBLE), pointer :: ipout
C           integer(c_int), intent(inout) :: lout, itype, ierror_return
C         end subroutine

C          subroutine cmo_get_name(cmo_name,ierror)
C     &    bind(C, name="cmo_get_name")
C            use, intrinsic :: iso_c_binding, only: c_char, 
C     &      c_null_char, c_size_t, c_int
C            character(kind=c_char), dimension(*), intent(inout) :: 
C     &      cmo_name
C            integer(c_int), intent(in) :: ierror
C          end subroutine
C=========== BEGIN ANOTHERMATBLD3D DECLARATIONS ========================

C     void initialize3ddiffusionmat_(int_ptrsize *pentrysize,
C             int_ptrsize *pcompress, int_ptrsize *pnnodes, double *xic,
C             double *yic, double *zic, int_ptrsize *pntets,
C             int_ptrsize *itet, int_ptrsize *jtet,
C             int_ptrsize *pmbndry, int_ptrsize *ifhybrid,
C             double *hybrid_factor, double *eps)
          subroutine initialize3ddiffusionmat
     &    (pentrysize, pcompress, pnnodes, xic, yic, zic, pntets, itet,
     &     jtet, pmbndry, ifhybrid, hybrid_factor, eps)
     &    BIND(C, name="initialize3ddiffusionmat_")
            use ISO_C_BINDING, only : C_INT, C_FLOAT, C_DOUBLE
            implicit none

            integer(C_INT) :: pentrysize, pcompress, pnnodes, pntets
            integer(C_INT) :: itet, jtet, pmbndry, ifhybrid
            real(C_DOUBLE) :: xic, yic, zic, hybrid_factor, eps
          end subroutine initialize3ddiffusionmat

C     int entryprocessed_(int_ptrsize *i, int_ptrsize *j)
          integer(KIND=C_INT) function entryprocessed
     &    (i, j)
     &    BIND(C, name="entryprocessed_")
            use ISO_C_BINDING, only : C_INT
            implicit none

            integer(C_INT) :: i, j
          end function entryprocessed

C     void computeentry_(int_ptrsize *Pindex_i, int_ptrsize *Pindex_j,
C            int_ptrsize *PnumIncidentTets, int_ptrsize *incidentTets,
C            int_ptrsize *localEdges)
          subroutine computeentry
     &    (Pindex_i, Pindex_j, PnumIncidentTets, incidentTets,
     &     localEdges)
     &    BIND(C, name="computeentry_")
            use ISO_C_BINDING, only : C_INT
            implicit none

            integer(C_INT) :: Pindex_i, Pindex_j, PnumIncidentTets
            integer(C_INT) :: incidentTets, localEdges
          end subroutine computeentry


C     void finalscalar3ddiffusionmat_()
          subroutine finalscalar3ddiffusionmat
     &    ()
     &    BIND(C, name="finalscalar3ddiffusionmat_")
          end subroutine finalscalar3ddiffusionmat

C     void extractnegativecoefs(int_ptrsize *component, int_ptrsize
C               *numnegs, int_ptrsize *numsuspectnegs,
C               int_ptrsize *numzeronegs, int_ptrsize **negrows,
C               int_ptrsize **negcols, double **negs)
          subroutine extractnegativecoefs
     &    (component, numnegs, numsuspectnegs, numzeronegs,
     &     negrows, negcols, negs)
     &    BIND(C, name="extractnegativecoefs")
            use ISO_C_BINDING, only : C_INT, C_DOUBLE
            implicit none

            integer(C_INT) :: component, numnegs, numsuspectnegs
            integer(C_INT) :: numzeronegs, negrows, negcols
            !real(C_DOUBLE) :: negs
            integer(C_INT) :: negs
          end subroutine extractnegativecoefs

C     void freenegcoefs_()
          subroutine freenegcoefs
     &    ()
     &    BIND(C, name="freenegcoefs_")
          end subroutine freenegcoefs

C     void getmatrixsizes_(int_ptrsize *Pnum_written_coefs,
C               int_ptrsize *ncoefs, int_ptrsize *ncon_max)
          subroutine getmatrixsizes
     &    (Pnum_written_coefs, ncoefs, ncon_max)
     &    BIND(C, name="getmatrixsizes_")
            use ISO_C_BINDING, only : C_INT
            implicit none

            integer(C_INT) :: Pnum_written_coefs, ncoefs, ncon_max
          end subroutine getmatrixsizes

C     void getvoronoivolumes_(double **volic)
          subroutine getvoronoivolumes
     &    (volic)
     &    BIND(C, name="getvoronoivolumes_")
            use ISO_C_BINDING, only : C_DOUBLE, C_PTR, C_INT
            implicit none

            !type(C_PTR) :: volic
            integer(C_INT) :: volic
          end subroutine getvoronoivolumes

C     void freevoronoivolumes_()
          subroutine freevoronoivolumes
     &    ()
     &    BIND(C, name="freevoronoivolumes_")
          end subroutine freevoronoivolumes

C     void getentriesperrow_(int_ptrsize **epr)
C      pointer (ipepr, epr)
C      integer epr(*)
          subroutine getentriesperrow
     &    (epr)
     &    BIND(C, name="getentriesperrow_")
            use ISO_C_BINDING, only : C_INT
            implicit none

            integer(C_INT) :: epr
          end subroutine getentriesperrow

C     void getoccupiedcolumns_(int_ptrsize **columns)
          subroutine getoccupiedcolumns
     &    (columns)
     &    BIND(C, name="getoccupiedcolumns_")
            use ISO_C_BINDING, only : C_INT
            implicit none

            integer(C_INT) :: columns
          end subroutine getoccupiedcolumns

C     void freeentriesperrow_()
          subroutine freeentriesperrow
     &    ()
     &    BIND(C, name="freeentriesperrow_")
          end subroutine freeentriesperrow

C     void freeoccupiedcolumns_()
          subroutine freeoccupiedcolumns
     &    ()
     &    BIND(C, name="freeoccupiedcolumns_")
          end subroutine freeoccupiedcolumns

C     void getmatrixpointers_(int_ptrsize **MatPointers,
C                             int_ptrsize **diagonals)
          subroutine getmatrixpointers
     &    (MatPointers, diagonals)
     &    BIND(C, name="getmatrixpointers_")
            use ISO_C_BINDING, only : C_INT
            implicit none

            integer(C_INT) :: MatPointers, diagonals
          end subroutine getmatrixpointers

C     void freematrixpointers_()
          subroutine freematrixpointers
     &    ()
     &    BIND(C, name="freematrixpointers_")
          end subroutine freematrixpointers

C     void getcomponentmatrixvalues_(int_ptrsize *component,
C                                    double **values)
          subroutine getcomponentmatrixvalues
     &    (component, values)
     &    BIND(C, name="getcomponentmatrixvalues_")
            use ISO_C_BINDING, only : C_INT, C_DOUBLE
            implicit none

            integer(C_INT) :: component
            !real(C_DOUBLE) :: values
            integer(C_INT) :: values
          end subroutine getcomponentmatrixvalues

C     void freematrixvalues_()
          subroutine freematrixvalues
     &    ()
     &    BIND(C, name="freematrixvalues_")
          end subroutine freematrixvalues

C     void killsparsematrix_()
          subroutine killsparsematrix
     &    ()
     &    BIND(C, name="killsparsematrix_")
          end subroutine killsparsematrix

C     void compressmatrixvalues_(int_ptrsize *Ncon, int_ptrsize *Neq,
C              int_ptrsize *MEntrySize, int_ptrsize **MatPointers,
C              double **Xmat, double  **Ymat, double  **Zmat,
C              double **Mat, double **CXmat, double **CYmat,
C              double **CZmat, double **CMat,
C              int_ptrsize *numwrittenentries, double *Epsilon)
          subroutine compressmatrixvalues
     &    (Ncon, Neq, MEntrySize, MatPointers, Xmat, Ymat, Zmat, Mat,
     &     CXmat, CYmat, CZmat, CMat, numwrittenentries, Epsilon)
     &    BIND(C, name="compressmatrixvalues_")
            use ISO_C_BINDING, only : C_INT, C_DOUBLE
            implicit none

            integer(C_INT) :: Ncon, Neq, MEntrySize, MatPointers
C            real(C_DOUBLE) :: Xmat, Ymat, Zmat, Mat, CXmat, CYmat
C            real(C_DOUBLE) :: CZmat, CMat
            integer(C_INT) :: Xmat, Ymat, Zmat, Mat, CXmat, CYmat
            integer(C_INT) :: CZmat, CMat
            integer(C_INT) :: numwrittenentries
            real(C_DOUBLE) :: Epsilon
          end subroutine compressmatrixvalues

C     void freecompressedvaluesarrays_()
          subroutine freecompressedvaluesarrays
     &    ()
     &    BIND(C, name="freecompressedvaluesarrays_")
          end subroutine freecompressedvaluesarrays

C=========== END ANOTHERMATBLD3D DECLARATIONS ==========================
C=========== BEGIN GMV FILE I/O DECLARATIONS ===========================

C         void fgmvwriteopenfile_(char filnam[])
          subroutine fgmvwriteopenfile
     &    (filnam)
     &    BIND(C, name="fgmvwriteopenfile_")
            use ISO_C_BINDING, only : C_CHAR

            character(kind=C_CHAR) :: filnam(*)

          end subroutine fgmvwriteopenfile

C         void fgmvwritenodedata_(int *nndes, float x[], float y[], float z[])
          subroutine fgmvwritenodedata
     &    (nndes, x, y, z)
     &    BIND(C, name="fgmvwritenodedata_")
            use ISO_C_BINDING, only : C_INT, C_FLOAT
            implicit none

            integer(C_INT) :: nndes
            real(C_FLOAT) :: x(*), y(*), z(*)
          end subroutine fgmvwritenodedata

C         void fgmvwritecellheader_(int *ncells)
          subroutine fgmvwritecellheader
     &    (ncells)
     &    BIND(C, name="fgmvwritecellheader_")
            use ISO_C_BINDING, only : C_INT
            implicit none

            integer(C_INT) :: ncells
          end subroutine fgmvwritecellheader

C         void fgmvwritecelldata_(char cell_data[], int *nverts, int nodes[])
          subroutine fgmvwritecelldata
     &    (cell_data, nverts, nodes)
     &    BIND(C, name="fgmvwritecelldata_")
            use ISO_C_BINDING, only : C_INT, C_CHAR
            implicit none

            character(kind=C_CHAR) :: cell_data(*)
            integer(C_INT) :: nverts, nodes(*)
          end subroutine fgmvwritecelldata

C         void fgmvwritematerialheader_(int *nmats, int *data_type)
          subroutine fgmvwritematerialheader
     &    (nmats, data_type)
     &    BIND(C, name="fgmvwritematerialheader_")
            use ISO_C_BINDING, only : C_INT
            implicit none

            integer(C_INT) :: nmats, data_type
          end subroutine fgmvwritematerialheader

C         void fgmvwritematerialname_(char matname[])
          subroutine fgmvwritematerialname
     &    (matname)
     &    BIND(C, name="fgmvwritematerialname_")
            use ISO_C_BINDING, only : C_CHAR
            implicit none

            character(kind=C_CHAR) :: matname(*)
          end subroutine fgmvwritematerialname

C         void fgmvwritematerialids_(int matids[], int *data_type)
          subroutine fgmvwritematerialids
     &    (matids, data_type)
     &    BIND(C, name="fgmvwritematerialids_")
            use ISO_C_BINDING, only : C_INT
            implicit none

            integer(C_INT) :: matids(*), data_type
          end subroutine fgmvwritematerialids

C         void fgmvwritevelocitydata_(int *data_type, float u[], float v[], float w[])
          subroutine fgmvwritevelocitydata
     &    (data_type, u, v, w)
     &    BIND(C, name="fgmvwritevelocitydata_")
            use ISO_C_BINDING, only : C_INT, C_FLOAT
            implicit none

            integer(C_INT) :: data_type
            real(C_FLOAT) :: u(*), v(*), w(*)
          end subroutine fgmvwritevelocitydata

C         void fgmvwritevariableheader_()
          subroutine fgmvwritevariableheader
     &    ()
     &    BIND(C, name="fgmvwritevariableheader_")
          end subroutine fgmvwritevariableheader

C         void fgmvwritevariablenamedata_(char varname[], int *data_type, float vids[])
          subroutine fgmvwritevariablenamedata
     &    (varname, data_type, vids)
     &    BIND(C, name="fgmvwritevariablenamedata_")
            use ISO_C_BINDING, only : C_CHAR, C_INT, C_FLOAT
            implicit none

            character(kind=C_CHAR) :: varname(*)
            integer(C_INT) :: data_type
            real(C_FLOAT) :: vids(*)
          end subroutine fgmvwritevariablenamedata

C         void fgmvwritevariableendvars_()
          subroutine fgmvwritevariableendvars
     &    ()
     &    BIND(C, name="fgmvwritevariableendvars_")
          end subroutine fgmvwritevariableendvars

C         void fgmvwriteflagheader_()
          subroutine fgmvwriteflagheader
     &    ()
     &    BIND(C, name="fgmvwriteflagheader_")
          end subroutine fgmvwriteflagheader

C         void fgmvwriteflagname_(char flagname[], int *data_type, int *ntypes)
          subroutine fgmvwriteflagname
     &    (flagname, data_type, ntypes)
     &    BIND(C, name="fgmvwriteflagname_")
            use ISO_C_BINDING, only : C_CHAR, C_INT
            implicit none

            character(kind=C_CHAR) :: flagname(*)
            integer(C_INT) :: data_type, ntypes
          end subroutine fgmvwriteflagname

C         void fgmvwriteflagtype_(char flagtype[])
          subroutine fgmvwriteflagtype
     &    (flagtype)
     &    BIND(C, name="fgmvwriteflagtype_")
            use ISO_C_BINDING, only : C_CHAR
            implicit none

            character(kind=C_CHAR) :: flagtype(*)
          end subroutine fgmvwriteflagtype

C         void fgmvwriteflagdata_(int *data_type,int iflag[])
          subroutine fgmvwriteflagdata
     &    (data_type, iflag)
     &    BIND(C, name="fgmvwriteflagdata_")
            use ISO_C_BINDING, only : C_INT
            implicit none

            integer(C_INT) :: data_type, iflag(*)
          end subroutine fgmvwriteflagdata

C         void fgmvwriteflagendflag_(int *data_type,int iflag[])
C           --> but, calls directly: `gmvwriteflagendflag();`
C               --> (no args)
          subroutine fgmvwriteflagendflag
     &    ()
     &    BIND(C, name="fgmvwriteflagendflag_")
            use ISO_C_BINDING, only : C_INT
            implicit none
          end subroutine fgmvwriteflagendflag

C         void fgmvwritepolygonsheader_()
          subroutine fgmvwritepolygonsheader
     &    ()
     &    BIND(C, name="fgmvwritepolygonsheader_")
          end subroutine fgmvwritepolygonsheader

C         void fgmvwritepolygonsdata_(int *nverts, int *matnum,
C           float x[], float y[], float z[])
          subroutine fgmvwritepolygonsdata
     &    (nverts, matnum, x, y, z)
     &    BIND(C, name="fgmvwritepolygonsdata_")
            use ISO_C_BINDING, only : C_INT, C_FLOAT
            implicit none

            integer(C_INT) :: nverts, matnum
            real(C_FLOAT) :: x(*), y(*), z(*)
          end subroutine fgmvwritepolygonsdata

C         void fgmvwritepolygonsendpoly_()
          subroutine fgmvwritepolygonsendpoly
     &    ()
     &    BIND(C, name="fgmvwritepolygonsendpoly_")
          end subroutine fgmvwritepolygonsendpoly

C         void fgmvwritecycleno_(int *number)
          subroutine fgmvwritecycleno
     &    (number)
     &    BIND(C, name="fgmvwritecycleno_")
            use ISO_C_BINDING, only : C_INT
            implicit none

            integer(C_INT) :: number
          end subroutine fgmvwritecycleno

C         void fgmvwriteprobtime_(float *ptime)
          subroutine fgmvwriteprobtime
     &    (ptime)
     &    BIND(C, name="fgmvwriteprobtime_")
            use ISO_C_BINDING, only : C_FLOAT
            implicit none

            real(C_FLOAT) :: ptime
          end subroutine fgmvwriteprobtime

C         void fgmvwriteclosefile_()
          subroutine fgmvwriteclosefile
     &    ()
     &    BIND(C, name="fgmvwriteclosefile_")
          end subroutine fgmvwriteclosefile

C=========== END GMV FILE I/O DECLARATIONS =============================
C=========== BEGIN MESH MANIPULATION SUBROUTINE DECLARATIONS ===========

C     void quad_quality_(const int_ptrsize (*quads)[4],
C                        const int_ptrsize *nquads,
C                        const double *xic,
C                        const double *yic,
C                        const double *zic,
C                        double *quality,
C                        double *regularity,
C                        int_ptrsize *flag)
          subroutine quad_quality
     &    (quads, nquads, xic, yic, zic, quality, regularity, flag)
     &    BIND(C, name="quad_quality_")
            use ISO_C_BINDING, only : C_INT, C_DOUBLE
            implicit none

            integer(C_INT) :: quads
            integer(C_INT) :: nquads
            real(C_DOUBLE) :: xic, yic, zic
            real(C_DOUBLE) :: quality
            real(C_DOUBLE) :: regularity
            integer(C_INT) :: flag
          end subroutine quad_quality

C=========== END MESH MANIPULATION SUBROUTINE DECLARATIONS =============
C=========== BEGIN C READ/WRITE SUBROUTINE DECLARATIONS ================

C     Because Fortran does not (officially) support polymorphism with
C     these ISO_C_BINDING declarations, and methods like `cread` are
C     called with different types for the same arg, psuedo-polymorphism
C     is implemented via methods like `cread`, `cread4`, `cread8`.
C ----------------------------------------------------------------------

C     void cassignr_(int *unit, char *fname, int_ptrsize *ierr)
          subroutine cassignr
     &    (unit, fname, ierr)
     &    BIND(C, name="cassignr_")
            use ISO_C_BINDING, only : C_INT, C_CHAR
            implicit none

            integer(C_INT) :: unit, ierr
            character(kind=C_CHAR) :: fname(*)
          end subroutine cassignr

C     void cread_(int *unit, char *array, int_ptrsize *ilen,
C                 int_ptrsize *ierr)
C         ==> type(array) => character array(*)
          subroutine cread
     &    (unit, array, ilen, ierr)
     &    BIND(C, name="cread_")
            use ISO_C_BINDING, only : C_INT, C_CHAR
            implicit none

            integer(4) :: unit
            integer(C_INT) :: ilen, ierr
            character(kind=C_CHAR) :: array(*)
          end subroutine cread

C         ==> type(array) => real*4 array
          subroutine cread4
     &    (unit, array, ilen, ierr)
     &    BIND(C, name="cread_")
            use ISO_C_BINDING, only : C_INT, C_FLOAT
            implicit none

            integer(4) :: unit
            integer(C_INT) :: ilen, ierr
            real(C_FLOAT) :: array
          end subroutine cread4

C         ==> type(array) => real*4 array(*)
          subroutine cread4_ptr
     &    (unit, array, ilen, ierr)
     &    BIND(C, name="cread_")
            use ISO_C_BINDING, only : C_INT, C_FLOAT
            implicit none

            integer(4) :: unit
            integer(C_INT) :: ilen, ierr
            real(C_FLOAT) :: array(*)
          end subroutine cread4_ptr

C         ==> type(array) => int*4 array
          subroutine cread4_int
     &    (unit, array, ilen, ierr)
     &    BIND(C, name="cread_")
            use ISO_C_BINDING, only : C_INT
            implicit none

            integer(4) :: unit
            integer(C_INT) :: ilen, ierr
            integer(C_INT) :: array
          end subroutine cread4_int

C         ==> type(array) => int*4 array(*)
          subroutine cread4_int_ptr
     &    (unit, array, ilen, ierr)
     &    BIND(C, name="cread_")
            use ISO_C_BINDING, only : C_INT
            implicit none

            integer(4) :: unit
            integer(C_INT) :: ilen, ierr
            integer(C_INT) :: array(*)
          end subroutine cread4_int_ptr

C         ==> type(array) => real*8 array
          subroutine cread8
     &    (unit, array, ilen, ierr)
     &    BIND(C, name="cread_")
            use ISO_C_BINDING, only : C_INT, C_DOUBLE
            implicit none

            integer(4) :: unit
            integer(C_INT) :: ilen, ierr
            real(C_DOUBLE) :: array
          end subroutine cread8

C=========== END C READ/WRITE SUBROUTINE DECLARATIONS ==================

C     void line_graph_sort_(int_ptrsize edges[][2], int_ptrsize *cid,
C        int_ptrsize *ctype, int_ptrsize *lid, int_ptrsize *skey,
C        int_ptrsize *len)
          subroutine line_graph_sort
     &    (edges, cid, ctype, lid, skey, len)
     &    BIND(C, name="line_graph_sort_")
            use ISO_C_BINDING, only : C_INT, C_LONG_LONG
            implicit none

C           Uses LONG LONG here (size=8) because INT is failing
            integer :: edges(*), skey(*)
            integer(C_INT) :: cid, ctype, lid, len
          end subroutine line_graph_sort

C     void line_graph_nsort_(int_ptrsize edges[][2],
C         int_ptrsize *nskey, int_ptrsize *len)
          subroutine line_graph_nsort
     &    (edges, nskey, len)
     &    BIND(C, name="line_graph_nsort_")
            use ISO_C_BINDING, only : C_INT, C_LONG_LONG
            implicit none

C           Uses LONG LONG here (size=8) because INT is failing
            integer :: edges(*), nskey(*)
            integer(C_INT) :: len
          end subroutine line_graph_nsort

        end interface
C        contains
      end module c2f_interface