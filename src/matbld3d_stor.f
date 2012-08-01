*dk,matbld3d_stor
      subroutine matbld3d_stor
     x           (ifile,ijob,io_type,num_area_coef,ifcompress)
 
C
C#######################################################################
C
C      PURPOSE -
C
C         BUILD A SPARSE MATRIX THAT CAN BE FED TO AN ITERATIVE SOLVER.
C
C      INPUT ARGUMENTS -
C
C         ifile - Base file name (.stor is appended).
C         ijob  = 1 Calculate coefficients and output to files.
C                   Release all temporary memory allocated.
C               = 2 Calculate coefficients. Do not output anything.
C                   Do not release temporary memory.
C         io_type = 1 Output binary coefficient (stor) file.
C                 = 2 Output ascii coefficient (stor) file.
C                 = 3 Output unformatted coefficient (stor) file.
C         note: until binary form is written, 1 = 3 = unformatted
C
C         num_area_coef = 1 Output single component scalar area/distance
c                           coefficients.
C                       = 3 Output x,y,z  vector area/distance coefficients.
C                       = 4 Output scalar and vector area/distance
c                           coefficients.
C                       =-1 Output single component scalar area coefficients.
C                       =-3 Output x,y,z  vector area coefficients.
C                       =-4 Output scalar and vector area coefficients.
c
c          ifcompress   = 0 If no compression of the .stor file is desired.
C                       = 1 If compression is desired.
C      OUTPUT ARGUMENTS -
C
C         istatus - Error Return Code (==0 ==> OK, <>0 ==> Error)
C         There is no returned error, checking is confined to this routine
C
c         The output file is in the FEHM .stor format
c
c                           THE FEHM .stor FORMAT
c                           ---------------------
c
c
c Title Line
c ----------
c
c  The first line consists of a character string of 72 or less
c  characters.
c
c
c Date Line
c ---------
c
c  The second line contains a character string of 72 or less
c  characters. This line contains a time stamp of when the file was
c  written.  It also contains information stating whether
c  the file is binary or ASCII (text).  If it is ASCII, then
c  characters 10-16 should be 'asci.'  If it is binary, then
c  characters 10-16 should be 'ieee'.
c
c  Characters 14-17 should be labled as either:
c              r4i4  r4i8  r8i4  r8i8
c
c  This signifies the number of bytes used for reals 'r' and
c  integers 'i'.   E.g., r8i4 means 8 byte reals, 4 byte integers.
c
c
c Matrix Parameters Line
c ----------------------
c
c  Next comes 4 or 5 integer values: NUM_WRITTEN_COEFS, NEQ, NCOEF+NEQ+1,
c  NUM_AREA_COEF, and [NCON_MAX}.  [] denotes the optional parameter:
c
c    NUM_WRITTEN_COEFS:
c              NUM_WRITTEN_COEFS  is the number of  floating point
c              entries written in the .stor file.  If compression is not used,
c              then this is equal to the number of connections in the grid.
c              This value is contained in the variable NCOEF, which is
c              used to allocate integer pointers.
c
c    NEQ:
c              NEQ signifies the number of equations. This is equivalent to
c              the number of nodes in a triangular or tetrahedral grid.
c
c    NCOEF+NEQ+1:
c               A value used to allocate memory in FEHM for the integer
c               pointers.
c
c    NUM_AREA_COEF:
c              Can be (1,3, or 4) number of area coefficients. The
c              area/distance coefficients are the area of a Voronoi
c              face divided by the Delaunay edge length. This
c              coefficient is either written as a scalar (1), a vector
c              area (3), or both vector and scalar areas are written
c              (4).
c
c    NCON_MAX:
c              NCON_MAX is an optional parameter indicating the maximum
c              number of connections to a single node in the entire
c              mesh. This is used for memory allocation.  If omitted,
c              FEHM will default to a value that might cause problems in
c              highly unstructured meshes.
c              [GEORGE -- Could you please elaborate on this?]
c
c [A note on delimiters: In the ASCII version of the .stor format, the
c first three lines are parsed for end of line characters.  That is, the
c Title Line, Date Line, and Matrix Parameters Line must occur in the
c first three lines.  Afterwards, data is delimited by whitespace and
c an occur in free form.  Further, no attempt to find or report errors
c in .stor files is made in FEHM.]
c
c
c Voronoi Volumes
c ---------------
c
c  This block has NEQ floating point values which are the Voronoi
c  Volumes associated with each node.
c
c
c Entries Per Row
c ---------------
c
c  The purpose of this block is to indicate the number of specified
c  entries each row of the matrix contains.  (All unspecified entries
c  are assumed to be 0.0).
c
c  The representation is circuitous.  There are a total of NEQ+1
c  entries.  The first entry is the value NEQ+1.  The second entry is
c  the value NEQ+1+the number of connections the in the first row.  The
c  third entry is value of the second entry + the number of connections
c  in the second row.  In general, the ith entry (for i>1) is the i-1th
c  entry + the number of specified entries in the row i-1.  In this
c  way, the difference between the i+1st and the ith value indicates
c  how many entries row i has in the matrix.
c
c  For example, suppose we had a 3x3 matrix with 5 entries we wished to
c  explicitly represent.  The .stor file at representing the matrix
c  would contain 4 entries in this block (i.e., NEQ+1=4).  Suppose the
c  first row of this matrix has 3 entries, the second row contains 1
c  entry, and the third row contains one entry.  There would
c  be 4 integer values given to represent this information:
c
c                              4 7 8 9
c
c
c The Entries of Each Row
c -----------------------
c
c  The purpose of the next block is to state which entries of the
c  matrix are explicitly represented.  Since we know the number of
c  entries per row, (given above), we can do this by stating which
c  columns of each row are represented.  For example, suppose in our
c  3x3 matrix example, we saw the following entries in this block:
c
c     1 2 3
c     2 3
c
c   This would indicate: The first row has specified entries in
c   the first, second, and third columns (i.e, entries (1,1), (1,2) and (1,3)
c   are specified ; the second row has an entry specified in the second
c   column entry (i.e., (2,2)); the third row has an entry specified
c   in  the third column (i.e. (3,3)).
c
c   There are a total of NCOEF such values.
c
c
c Pointers to the Floating Point Values
c -------------------------------------
c
c  This block contains NCOEF integer pointers to entries to the
c  Floating Point Values Block (described below.)  The actual values of a
c  matrix entry can be repeated many times, particularly in a matrix
c  arising from the discretization of a structured mesh.  Because
c  floating point values require more storage than integers, we have
c  set up the following data structure to allow compression:
c
c    View the floating point values as two dimensional array of
c    dimensions NUM_WRITTEN_COEFS by NUM_AREA_COEF.
c
c  To represent a matrix entry, give the index of value contained in
c  the floating point values block here.  If a 0 is given, then
c  the writer of the .stor file has explicitly stated he wants the
c  value to be 0.0.  [George, can you explain why anyone would
c  want to do this?]
c
c  Following the NCOEF integer pointers are: 1) NEQ+1 0's for padding
c  and 2) a set of NEQ+1 integer pointers stating which pointers
c  correspond to diagonal elements.
c
c
c Floating Point Values Block
c ---------------------------
c
c  Next are NUM_WRITTEN_COEFS*NUM_AREA_COEF floating point values.
c  If NUM_AREA_COEF = 1, they are the scalar area for each connection,
c  a(i=1,k). If NUM_AREA_COEF=3, they are the vector area for each
c  connection in the order, a_x(i=1,k), a_y(i=1,k), a_z(i=1,k).
c  If NUM_AREA_COEF=4, they are the vector areas first followed by the
c  scalar area. Note that a(i) = sqrt(a_x**2 +  a_y**2 + a_z**2).
c
c A small example .stor file is included below.
c
c LaGriT Sparse Matrix, Voronoi Coefficients
c Wed Oct 15 12:48:45 1997 3-D Linear Diffusion Model (matbld3d_.stor)
c        44         8        53         4         7
c  1.250000000000E-01  1.250000000000E-01  1.250000000000E-01
c  1.250000000000E-01
c  1.250000000000E-01
c  1.250000000000E-01  1.250000000000E-01  1.250000000000E-01
c
c         9        16        20        24        31
c        35        42        49        53
c
c         1
c         2         3         4         5         6
c         7         1         2         4         6
c         1         3         4         7         1
c         2         3         4         6         7
c         8         1         5         6         7
c         1         2         4         5         6
c         7         8         1         3         4
c         5         6         7         8         4
c         6         7         8
c
c
c         1         2         3         4         5
c         6         7         8         9        10
c        11        12        13        14        15
c        16        17        18        19        20
c        21        22        23        24        25
c        26        27        28        29        30
c        31        32        33        34        35
c        36        37        38        39        40
c        41        42        43        44
c
c         0
c         0         0         0         0         0
c         0         0         0
c        10        18        22        28        33
c        40        48        53
 
c0.000000000000E+00 -2.500000000000E-01  0.000000000000E+00  2.081668171172E-17
c0.000000000000E+00
c2.775557561563E-17  0.000000000000E+00  2.500000000000E-01  0.000000000000E+00
c0.000000000000E+00
c1.387778780781E-17  0.000000000000E+00  0.000000000000E+00 -2.500000000000E-01
c0.000000000000E+00
c-2.081668171172E-17  0.000000000000E+00  2.500000000000E-01 0.000000000000E+00
c-3.271025955591E-18
c1.387778780781E-17  0.000000000000E+00  0.000000000000E+00  0.000000000000E+00
c-2.500000000000E-01
c0.000000000000E+00 -2.775557561563E-17 -1.387778780781E-17  3.271025955591E-18
c2.500000000000E-01
c0.000000000000E+00  2.081668171172E-17  0.000000000000E+00  0.000000000000E+00
c 0.000000000000E+00
c-1.387778780781E-17 0.000000000000E+00 -2.081668171172E-17  0.000000000000E+00
c-2.500000000000E-01
c0.000000000000E+00  0.000000000000E+00  2.500000000000E-01  0.000000000000E+00
c0.000000000000E+00  0.000000000000E+00 -2.500000000000E-01 -2.775557561563E-17
c 0.000000000000E+00
c-3.271025955591E-18  2.081668171172E-17  0.000000000000E+00 0.000000000000E+00
c-2.500000000000E-01
c-1.387778780781E-17  2.500000000000E-01  0.000000000000E+00 0.000000000000E+00
c 0.000000000000E+00
c2.775557561563E-17  2.500000000000E-01  0.000000000000E+00  0.000000000000E+00
c-6.938893903907E-18
c-6.542051911182E-18  0.000000000000E+00  0.000000000000E+00 0.000000000000E+00
c0.000000000000E+00
c-2.500000000000E-01  3.271025955591E-18 1.387778780781E-17 6.938893903907E-18
c0.000000000000E+00
c0.000000000000E+00  6.938893903907E-18 -2.500000000000E-01 -2.081668171172E-17
c0.000000000000E+00
c6.542051911182E-18  2.500000000000E-01 -6.938893903907E-18  0.000000000000E+00
c0.000000000000E+00
c0.000000000000E+00  2.500000000000E-01  0.000000000000E+00  0.000000000000E+00
c0.000000000000E+00  0.000000000000E+00  0.000000000000E+00 -6.542051911182E-18
c-2.500000000000E-01
c-3.469446951954E-17  2.775557561563E-17  0.000000000000E+00 0.000000000000E+00
c0.000000000000E+00
c-2.500000000000E-01  0.000000000000E+00  0.000000000000E+00 0.000000000000E+00
c-2.500000000000E-01
c6.542051911182E-18  0.000000000000E+00  0.000000000000E+00  0.000000000000E+00
c-2.081668171172E-17
c2.081668171172E-17 -2.500000000000E-01  2.500000000000E-01  0.000000000000E+00
c0.000000000000E+00
c0.000000000000E+00  3.469446951954E-17  2.500000000000E-01  2.081668171172E-17
c0.000000000000E+00
c0.000000000000E+00 -6.542051911182E-18  0.000000000000E+00 -2.775557561563E-17
c2.500000000000E-01
c-2.081668171172E-17  0.000000000000E+00  6.542051911182E-18 0.000000000000E+00
c0.000000000000E+00
c2.500000000000E-01  0.000000000000E+00  0.000000000000E+00  0.000000000000E+00
c0.000000000000E+00 -2.500000000000E-01 -2.500000000000E-01 -3.530587286063E-17
c-2.500000000000E-01
c-4.455084515247E-17 -3.469446951954E-17 -2.500000000000E-01 0.000000000000E+00
c-2.500000000000E-01
c-2.500000000000E-01 -2.500000000000E-01 0.000000000000E+00 -2.500000000000E-01
c-2.500000000000E-01
c-3.530587286063E-17 -2.500000000000E-01 -2.500000000000E-01 0.000000000000E+00
c-2.218517741419E-17
c-2.585973076299E-17 -2.500000000000E-01 -2.500000000000E-01 0.000000000000E+00
c-2.500000000000E-01
c-2.500000000000E-01 -4.455084515247E-17-2.500000000000E-01-2.218517741419E-17
c-2.500000000000E-01
c0.000000000000E+00 -2.289718168914E-17 -2.500000000000E-01 -3.469446951954E-17
c-2.500000000000E-01
c-2.585973076299E-17 -2.500000000000E-01 -2.289718168914E-17 0.000000000000E+00
c-2.500000000000E-01
c-2.500000000000E-01 -2.500000000000E-01 -2.500000000000E-01 0.000000000000E+00
C
C      CHANGE HISTORY -
C
C         $Log: matbld3d_stor.f,v $
C         Revision 2.00  2007/11/05 19:46:01  spchu
C         Import to CVS
C
CPVCS    
CPVCS       Rev 1.28   10 May 2006 16:54:08   gable
CPVCS    Modified format of negative coef. screen output so if fits on
CPVCS    one 80 character line.
CPVCS    
CPVCS       Rev 1.27   08 Feb 2006 14:37:44   dcg
CPVCS     "enforce lower case - add external statements for shift routines
CPVCS    these changes needed to compile with absoft pro fortran"
CPVCS    
CPVCS       Rev 1.26   17 Jun 2004 15:34:42   gable
CPVCS    Add output of xyz min/max bounding box of each Voronoi cell.
CPVCS    This makes very large files and adds to memory requirements.
CPVCS    To turn this feature on change,
CPVCS    data if_vor_io / 0 /  to data if_vor_io / 2 / 
CPVCS    
CPVCS       Rev 1.25   22 Apr 2003 13:07:04   gable
CPVCS    Slight change to log output.
CPVCS    
CPVCS       Rev 1.24   25 Mar 2003 16:04:00   gable
CPVCS    Change order in which work arrays are allocated and released so
CPVCS    that they only exist when they are really needed. Changed some
CPVCS    of the output to log files. Small modifications to algorithm
CPVCS    to make it faster.
CPVCS    
CPVCS       Rev 1.23   07 Aug 2001 13:51:42   dcg
CPVCS    fix line that was too long by making a continuation line
CPVCS
CPVCS       Rev 1.22   Thu Apr 06 09:15:50 2000   dcg
CPVCS    replace get_info_i call
CPVCS
CPVCS       Rev 1.21   Wed Apr 05 13:34:40 2000   nnc
CPVCS    Minor source modifications required by the Absoft compiler.
CPVCS
CPVCS       Rev 1.20   Mon Sep 20 15:03:12 1999   murphy
CPVCS
CPVCS       Rev 1.19   Tue Aug 31 14:23:52 1999   murphy
CPVCS    Fixed more unformatted i/o woes
CPVCS
CPVCS       Rev 1.18   Fri Jun 25 12:55:26 1999   murphy
CPVCS
CPVCS       Rev 1.17   Fri Jun 25 12:44:18 1999   murphy
CPVCS    fixed unformatted io problems
CPVCS
CPVCS       Rev 1.16   Thu Jun 03 14:11:08 1999   tam
CPVCS    set idebug, clarified screen output, removed print*
CPVCS
CPVCS       Rev 1.15   Wed Mar 17 15:33:10 1999   murphy
CPVCS    No change.
CPVCS
CPVCS       Rev 1.14   Tue Mar 16 09:49:12 1999   murphy
CPVCS    Removed print * statements.
CPVCS
CPVCS       Rev 1.13   Mon Mar 15 16:45:28 1999   murphy
CPVCS    Added compression code to compress the .stor file.
CPVCS
CPVCS       Rev 1.12   Fri Jan 22 14:09:34 1999   dcg
CPVCS    replace nnodes with npoints
CPVCS
CPVCS
CPVCS       Rev 1.11   Thu Mar 12 14:19:50 1998   gable
CPVCS    Added option for area coef without division by distance.
CPVCS    Changed screen output so that coef. is only printed to
CPVCS    the screen if it is negative and
CPVCS    value .lt. -abs(max_coef)*scale_factor with scale_factor=1.e-10.
CPVCS
CPVCS       Rev 1.10   Wed Feb 11 10:38:10 1998   tam
CPVCS    added unformatted option, for now binary and unformatted same
CPVCS    added header indicating data type in file, ascir8i4 and ieeer8i4
CPVCS
CPVCS       Rev 1.9   Wed Oct 15 13:38:30 1997   gable
CPVCS    Modified to allow ascii or binary stor file output.
CPVCS    Also added option to have scalar, vector or scalar and vector
CPVCS    area coefficients as part of output.
CPVCS
CPVCS       Rev 1.8   Wed Oct 08 16:55:36 1997   dcg
CPVCS    use 'one' not '1' in calls to sign
CPVCS
CPVCS       Rev 1.7   Thu Jul 03 15:19:18 1997   gable
CPVCS    Added parameter, ijob, to subroutine call.
CPVCS    Changed time stamp to use fdate()
CPVCS
CPVCS       Rev 1.6   Sat Jun 28 18:37:34 1997   gable
CPVCS    Include time stamp in stor file header.
CPVCS
CPVCS       Rev 1.5   Sun Jun 22 10:05:30 1997   gable
CPVCS    Change the test comparing two methods of calculating voronoi center.
CPVCS
CPVCS       Rev 1.4   Fri Jun 20 20:16:20 1997   gable
CPVCS    Changed calculation of num_conn_max (max connections to a node).
CPVCS
CPVCS       Rev 1.3   Thu Jun 19 17:28:04 1997   gable
CPVCS    This is not a final version. Checking in just to stor the changes.
CPVCS    Major changes to how sign of coefficients are computed.
CPVCS    Added i/o that indicates which connections have negative coefficients.
CPVCS    Option to output AVS files of voronoi faces and volumes.
CPVCS    Output both Aij/xij vector and Aij/xij scalar to .stor file.
CPVCS    **********This is not a final version.***************
CPVCS
CPVCS       Rev 1.2   Mon Apr 14 16:53:06 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.1   Fri Jan 24 13:52:58 1997   het
CPVCS    Use the Voronoi algorithm to create the .stor file.
CPVCS
CPVCS       Rev 1.0   Wed May 08 12:41:26 1996   gable
CPVCS    Initial revision.
C
C#######################################################################
C
C
C#######################################################################
C
c     implicit real*8 (a-h,o-z)
      implicit none
C
      include "chydro.h"
      include "consts.h"
C
c args
      character*(*) ifile
      character*72  title_string
      integer ijob, io_type, num_area_coef, ifcompress
C
c local ----------------------------------------------------------------
C
      integer nconn
      integer istatus, ierror, ierr, ierr1,ics,ierrw
      integer isort, icolmat, irowmat, isendnn,
     *        irowcnt, irowoff, irowdag, iamat, itet_vor,
     *        isign_dot, imedian, isolve
      integer if_vor_io
      integer itmax,icscode,npoints,length,icmotype,ntets,ier,
     *        mbndry,nsdtopo,nsdgeom,nen,nef,
     *        lenisetwd,lenimt1,lenitp1,lenicr1,lenisn1,
     *        lenxic,lenyic,lenzic,lenitetclr,lenitet,lenjtet,
     *        ipackopt,idiag,n12,it,index,i,irow,idsave,
     *        nnmax,ncoefs,len15,len24,len3_2_nn,
     *        i1,i2,i3,i4,i1p,i2p,i3p,i4p,
     *        iedge,neq,neqp1,ncont,iwtotl,ilenout,
     *        iatt_type,icount_neg,icount,numj,jnum,
     *        j,ii,jj,kk,ll,index_f,isign_meth,num_conn_max,
     *        iunit, iunit1, iunit2, iunit3
      integer jcount
      integer icharlnf
      integer nextlun
      integer num_written_coefs
 
      real*8 a,b,c,d,e,f, amat, xamat, yamat, zamat, camat,cxamat,
     *       cyamat,czamat,voltet,voltot,voltot_vor1,voltot_vor2,rn,
     *       dotb3,dot3,rb3,ql,ds11,ds21,ds31,zv1,ds12,ds22,ds32,
     *       xv2,ds13,ds23,ds33,ds14,ds24,ds34,three_i,q,dvor,qvor2,
     *       radiusv,distsqa,distsqb,distsqc,distsqd,vol11,vol13,vol14,
     *       xdot11,xdot12,xdot1,vol21,vol23,vol22,vol24,xdot21,xdot22,
     *       vol31,vol33,vol34,vol41,vol43,vol42,vol44,xdot41,xdot42,
     *       vol51,vol53,vol52,vol54,xdot51,xdot52,vol61,vol63,vol62,
     *       vol64,xdot61,xdot62,vol1a,vol1b,vol2a,vol2b,vol3a,vol3b,
     *       vol4a,vol4b,vol5a,vol5b,vol6a,vol6b,voltet_vor1,volvortet1,
     *       vol12,xdot2,vol32,xdot31,xdot32,xdot3,xdot4,xdot5,
     *       xdot6,volvortet2,volvortet3,volvortet4,
     *       voltet_vor2,ds12i,ds13i,ds14i,ds23i,ds24i,ds34i,
     *       sfac1,sfac2,amatmin,amatmax,volmin,volmax,
     *       amatsave,xijmag,amag,value,
     *       ave_con_node,xij_dot_aij
      real*8 xconst, yconst, zconst, xsendnn, ysendnn, zsendnn,
     *       xic, yic,zic,xv4,yv4,zv4,xc,yc,zc,ax12,ay12,az12,
     *       ax21,ay21,az21,ax22,ay22,az22,ax31,ay31,az31,
     *       ax32,ay32,az32,ax41,ay41,az41,ax42,ay42,az42,
     *       ax51,ay51,az51,ax52,ay52,az52,ax5,ay5,az5,ax61,ay61,az61,
     *       ax62,ay62,az62,ax6,ay6,az6,xij,yij,zij,
     *       x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4,dx,dy,dz,xa,ya,za,
     *       xfac,xb,yb,zb,xd,yd,zd,xn1,yn1,zn1,xn,yn,zn,
     *       crosx,crosy,crosz,xl,yl,zl,xv1,yv1,yv2,zv2,
     *       xv3,yv3,zv3,xl1,yl1,zl1,xl2,yl2,zl2,xl3,yl3,zl3,
     *       xl4,yl4,zl4,ax1,ay1,az1,ax2,ay2,az2,ax3,ay3,az3,
     *       ax4,ay4,az4,x234,y234,z234,x143,y143,z143,x124,y124,z124,
     *       x132,y132,z132,xvor,yvor,zvor,xcenv,ycenv,zcenv,
     *       xdifvor,ydifvor,zdifvor,xyzdif,xyzsiz,x12,y12,z12,
     *       x13,y13,z13,x14,y14,z14,x23,y23,z23,x24n,y24,z24,
     *       x34,y34,z34,xm,ym,zm,ax11,ay11,az11
      real*8 volic,vor_pts,ccoef,coef_neg,cpmult,
     * compress_epsilon, area_scale,scale_factor
      real*8 vor_xyz_min_max
      character*32 cmo, ifilename
      character*24 string, fdate
      character*132 logmess
C
      pointer (ipisetwd, isetwd)
      pointer (ipimt1, imt1)
      pointer (ipitp1, itp1)
      pointer (ipicr1, icr1)
      pointer (ipisn1, isn1)
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      pointer (ipitetclr, itetclr)
      pointer (ipitet, itet)
      pointer (ipitet, itet1)
      pointer (ipjtet, jtet)
      pointer (ipjtet, jtet1)
      integer isetwd(1000000)
      integer imt1(1000000), itp1(1000000),
     *        icr1(1000000), isn1(1000000),
     *        itet(4,1000000), jtet(4,1000000), itetclr(1000000)
      integer itet1(4*1000000), jtet1(4*1000000)
      dimension   xic(1000000), yic(1000000), zic(1000000)
C
      pointer (ipvolic, volic)
      dimension volic(1000000)
C
      pointer (ipitemp, itemp)
      integer itemp(10000000)
c
      pointer (ipimatptrs, imatptrs)
      integer imatptrs(10000000)
 
c
c
C
C#######################################################################
C
C
C
      pointer (ipiparent, iparent)
      integer iparent(1000000)
      pointer (ipitetp, itetp1)
      integer itetp1(4*1000000)
      pointer (ipitetp, itetp)
      integer itetp(4,1000000)
C
      pointer (ipisort, isort)
      pointer (ipicolmat, icolmat)
      pointer (ipirowmat, irowmat)
      pointer (ipisendnn, isendnn)
      pointer (ipxsendnn, xsendnn)
      pointer (ipysendnn, ysendnn)
      pointer (ipzsendnn, zsendnn)
      pointer (ipirowcnt, irowcnt)
      pointer (ipirowoff, irowoff)
      pointer (ipirowdag, irowdag)
      dimension isort(12*1000000+1000000)
      dimension icolmat(12*1000000+1000000), irowmat(12*1000000+1000000)
      dimension isendnn(12*1000000+1000000), xsendnn(12*1000000+1000000)
      dimension ysendnn(12*1000000+1000000), zsendnn(12*1000000+1000000)
      dimension irowcnt(1000000), irowoff(1000000), irowdag(1000000)
c
      pointer (ipxconst, xconst)
      pointer (ipyconst, yconst)
      pointer (ipzconst, zconst)
      dimension xconst(12*1000000+1000000)
      dimension yconst(12*1000000+1000000)
      dimension zconst(12*1000000+1000000)
C
      pointer (ipamat, amat)
      pointer (ipxamat, xamat)
      pointer (ipyamat, yamat)
      pointer (ipzamat, zamat)
      pointer (ipiamat, iamat)
 
      pointer (ipcamat,  camat(1000000))
      pointer (ipcxamat, cxamat(1000000))
      pointer (ipcyamat, cyamat(1000000))
      pointer (ipczamat, czamat(1000000))
 
 
      dimension amat(12*1000000+1000000)
      dimension xamat(12*1000000+1000000)
      dimension yamat(12*1000000+1000000)
      dimension zamat(12*1000000+1000000)
      dimension iamat(12*1000000+1000000)
C
C
C   These arrays are used to store voronoi faces
C
      pointer (ipisign_face, isign_face)
      integer isign_face(10000000)
      pointer (ipvor_pts, vor_pts)
      dimension vor_pts(3,15,1000000)
      pointer (ipvor_xyz_min_max, vor_xyz_min_max)
      dimension vor_xyz_min_max(3,2,10000000)
      dimension itet_vor(4,24)
      data itet_vor / 15, 6, 14, 1,
     2                15, 12, 6, 1,
     3                15,  5,13, 1,
     4                15, 14, 5, 1,
     5                15,  7,12, 1,
     6                15, 13, 7, 1,
     7                15,  5,14, 2,
     8                15, 13, 5, 2,
     9                15,  8,11, 2,
     1                15, 14, 8, 2,
     1                15,  9,13, 2,
     2                15, 11, 9, 2,
     3                15,  6,12, 3,
     4                15, 14, 6, 3,
     5                15,  8,14, 3,
     6                15, 11, 8, 3,
     7                15, 10,11, 3,
     8                15, 12,10, 3,
     9                15,  7,13, 4,
     1                15, 12, 7, 4,
     1                15,  9,11, 4,
     2                15, 13, 9, 4,
     3                15, 10,12, 4,
     4                15, 11,10, 4 /
      dimension isign_dot(24)
C
      character*32 isubname
      character*32 io_string, coef_string, comp_string
C
      parameter (nconn=12)
      integer lconn(2,nconn)
      data lconn / 1, 2,
     *             2, 1,
     *             1, 3,
     *             3, 1,
     *             1, 4,
     *             4, 1,
     *             2, 3,
     *             3, 2,
     *             2, 4,
     *             4, 2,
     *             3, 4,
     *             4, 3 /
      character*132 dotask_command
      character*15 icr_ccoef
      data icr_ccoef / 'ccoef'/
      pointer (ipccoef, ccoef)
      dimension   ccoef(1000000)
      pointer (ipcoef_neg, coef_neg)
      dimension coef_neg(10000000)
      pointer (ipindex_neg, index_neg)
      integer index_neg(10000000)
C
      data imedian / 0 /
      data cpmult / 0.0 /
      data itmax / 10 /
      data isolve / 0 /
      data if_vor_io / 0 /
c     data if_vor_io / 2 /
      data scale_factor / 1.e-10 /
C
      crosx(a,b,c,d,e,f)=b*f-c*e
      crosy(a,b,c,d,e,f)=c*d-a*f
      crosz(a,b,c,d,e,f)=a*e-b*d
C
C
C#######################################################################
C
      isubname='matbld3d'
      istatus=0
      icscode = 0 
C
      call mmrelprt(isubname,icscode)
C
      call cmo_get_name(cmo,ierror)
      if (ierror .ne. 0) then
        write(logmess,'(a,a15)')"matbld3d: error from get cmo: ",cmo
        call writloga('default',0,logmess,0,ierrw)
        istatus = -1
        goto 9999
      endif
 
C
C     Set up binning of edges data structure
C
      if(ijob .eq. 2)call bin_ij_edge(cmo,isubname)
C
C      call cwg_voronoi_stor(cmo,'all',ifile)
C
      call cmo_get_info('nnodes',cmo,npoints,length,icmotype,ierror)
      call cmo_get_info('nelements',cmo,ntets,length,icmotype,ier)

      if (npoints.le.0) then
        write(logmess,'(a,a)')
     *  "matbld3d: cmo has no points: ",cmo(1:icharlnf(cmo))
        call writloga('default',1,logmess,1,ierrw)
        istatus = -1
        goto 9999
      endif
      if (ntets.le.0) then
        write(logmess,'(a,a)')
     *  "matbld3d: cmo has no elements: ",cmo(1:icharlnf(cmo))
        call writloga('default',1,logmess,1,ierrw)
        istatus = -1
        goto 9999
      endif


      call cmo_get_info('mbndry',cmo,mbndry,length,icmotype,ierror)
      call cmo_get_intinfo('idebug',cmo,idebug,length,icmotype,ierror)

      call cmo_get_info('ndimensions_topo',cmo,
     *                  nsdtopo,length,icmotype,ierror)
      call cmo_get_info('ndimensions_geom',cmo,
     *                  nsdgeom,length,icmotype,ierror)
      call cmo_get_info('nodes_per_element',cmo,
     *                  nen,length,icmotype,ierror)
      call cmo_get_info('faces_per_element',cmo,
     *                  nef,length,icmotype,ierror)
      call cmo_get_info('isetwd',cmo,ipisetwd,lenisetwd,icmotype,ier)
      call cmo_get_info('imt1',cmo,ipimt1,lenimt1,icmotype,ierror)
      call cmo_get_info('itp1',cmo,ipitp1,lenitp1,icmotype,ierror)
      call cmo_get_info('icr1',cmo,ipicr1,lenicr1,icmotype,ierror)
      call cmo_get_info('isn1',cmo,ipisn1,lenisn1,icmotype,ierror)
      call cmo_get_info('xic',cmo,ipxic,lenxic,icmotype,ierror)
      call cmo_get_info('yic',cmo,ipyic,lenyic,icmotype,ierror)
      call cmo_get_info('zic',cmo,ipzic,lenzic,icmotype,ierror)
      call cmo_get_info('itetclr',cmo,ipitetclr,lenitetclr,icmotype,ier)
      call cmo_get_info('itet',cmo,ipitet,lenitet,icmotype,ierror)
      call cmo_get_info('jtet',cmo,ipjtet,lenjtet,icmotype,ierror)
 
      if(idebug.ne.0) then
        write(logmess,'(a,i5)')"debug option set to: ",idebug
        call writloga('default',0,logmess,0,ierrw)
      endif
C
C
C     ..................................................................
C     SET THE MATRIX PACKING FLAG:  =0 ==> THE SPARSE MATRIX PATTERN
C                                             IS PADDED TO A CONSTANT
C                                             COLUMN WIDTH.
C                                   =1 ==> THE SPARSE MATRIX PATTERN
C                                             IS FULLY COMPRESSED WITH
C                                             OFFSETS TO THE ROWS.
C
      ipackopt=1
C
C
C     ..................................................................
C     SET THE MATRIX DIAGONAL FLAG: =0 ==> DON'T INCLUDE THE DIAGONAL
C                                             ELEMENTS IN THE MATRIX.
C                                   =1 ==> INCLUDE THE DIAGONAL
C                                             ELEMENTS IN THE MATRIX.
C
      idiag=1
C
C
C     ..................................................................
C
      n12=nconn*ntets+idiag*npoints
C
C
C
C#######################################################################
C

C     add information so user knows which routine is being used.
      if (idebug.ne.0) then 
      write(logmess,'(a)') "Matbld3d_stor "
      call writloga('default',1,logmess,0,ierrw)
      write(logmess,'(a,a)') "  cmo name         : ",cmo
      call writloga('default',0,logmess,0,ierrw)
      write(logmess,'(a,a)') "  file name        : ",ifile
      call writloga('default',0,logmess,0,ierrw)
      write(logmess,'(a,i5)') "  file type option: ",io_type
      call writloga('default',0,logmess,0,ierrw)
      write(logmess,'(a,i5)') "  compress option : ",ifcompress
      call writloga('default',0,logmess,0,ierrw)
      write(logmess,'(a,i5)') "  coef option     : ",num_area_coef
      call writloga('default',0,logmess,1,ierrw)
      endif

      length=n12
      if (idebug.gt.1) then 
      write(logmess,'(a,i14)')
     *   "Matbld3d_stor: mmgetblk   isort: ",length
      call writloga('default',0,logmess,0,ierrw)
      endif
 
      call mmgetblk("isort",isubname,ipisort,length,1,icscode)
      if (icscode .ne. 0) then
        write(logmess,'(a,i5)')
     *   "Matbld3d_stor: mmgetblk failed:  isort ",icscode
        call writloga('default',0,logmess,0,ierrw)
        istatus = icscode
        goto 9999
      endif
 
      if (idebug.gt.1) then
      write(logmess,'(a,i14)')
     *   "Matbld3d_stor: mmgetblk icolmat: ",length
      call writloga('default',0,logmess,0,ierrw)
      endif
 
      call mmgetblk("icolmat",isubname,ipicolmat,length,1,icscode)
      if (icscode .ne. 0) then
        write(logmess,'(a,i5)')
     *   "Matbld3d_stor: mmgetblk failed: icolmat ",icscode
        call writloga('default',0,logmess,0,ierrw)
        istatus = icscode
        goto 9999
      endif

      if (idebug.gt.1) then
      write(logmess,'(a,i14)')
     *   "Matbld3d_stor: mmgetblk irowmat: ",length
      call writloga('default',0,logmess,0,ierrw)
      endif 
 
      call mmgetblk("irowmat",isubname,ipirowmat,length,1,icscode)
      if (icscode .ne. 0) then
        write(logmess,'(a,i5)')
     *   "Matbld3d_stor: mmgetblk failed: irowmat ",icscode
        call writloga('default',0,logmess,0,ierrw)
        istatus = icscode
        goto 9999
      endif

      if (idebug.gt.1) then 
      write(logmess,'(a,i14)')
     *   "Matbld3d_stor: mmgetblk isendnn: ",length
      call writloga('default',0,logmess,0,ierrw)
      endif
 
      call mmgetblk("isendnn",isubname,ipisendnn,length,1,icscode)
      if (icscode .ne. 0) then
        write(logmess,'(a,i5)')
     *   "Matbld3d_stor: mmgetblk failed: isendnn ",icscode
        call writloga('default',0,logmess,0,ierrw)
        istatus = icscode
        goto 9999
      endif

 
      length=npoints
      
      if (idebug.gt.1) then 
      write(logmess,'(a,i14)')
     *   "Matbld3d_stor: mmgetblk irowcnt: ",length
      call writloga('default',0,logmess,0,ierrw)
      endif
 
      call mmgetblk("irowcnt",isubname,ipirowcnt,length,1,icscode)
      if (icscode .ne. 0) then
        write(logmess,'(a,i5)')
     *   "Matbld3d_stor: mmgetblk failed: irowcnt ",icscode
        call writloga('default',0,logmess,0,ierrw)
        istatus = icscode
        goto 9999
      endif

      if (idebug.gt.1) then 
      write(logmess,'(a,i14)')
     *   "Matbld3d_stor: mmgetblk irowoff: ",length
      call writloga('default',0,logmess,0,ierrw)
      endif
 
      call mmgetblk("irowoff",isubname,ipirowoff,length,1,icscode)
      if (icscode .ne. 0) then
        write(logmess,'(a,i5)')
     *   "Matbld3d_stor: mmgetblk failed: irowoff ",icscode
        call writloga('default',0,logmess,0,ierrw)
        istatus = icscode
        goto 9999
      endif

      if (idebug.gt.1) then 
      write(logmess,'(a,i14)')
     *   "Matbld3d_stor: mmgetblk irowdag: ",length
      call writloga('default',0,logmess,0,ierrw)
      endif
 
      call mmgetblk("irowdag",isubname,ipirowdag,length,1,icscode)
      if (icscode .ne. 0) then
        write(logmess,'(a,i5)')
     *   "Matbld3d_stor: mmgetblk failed: irowdag ",icscode
        call writloga('default',0,logmess,0,ierrw)
        istatus = icscode
        goto 9999
      endif

      if (idebug.gt.1) then 
      write(logmess,'(a,i14)')
     *   "Matbld3d_stor: mmgetblk iparent: ",length
      call writloga('default',0,logmess,0,ierrw)
      endif
 
      call mmgetblk("iparent",isubname,ipiparent,length,1,icscode)
      if (icscode .ne. 0) then
        write(logmess,'(a,i5)')
     *   "Matbld3d_stor: mmgetblk failed: iparent ",icscode
        call writloga('default',0,logmess,0,ierrw)
        istatus = icscode
        goto 9999
      endif


      length=nen*ntets

      if (idebug.gt.1) then
      write(logmess,'(a,i14)')
     *   "Matbld3d_stor: mmgetblk   itetp: ",length
      call writloga('default',0,logmess,0,ierrw)
      endif
 
      call mmgetblk("itetp",isubname,ipitetp,length,1,icscode)
      if (icscode .ne. 0) then
        write(logmess,'(a,i5)')
     *   "Matbld3d_stor: mmgetblk failed:  itetp ",icscode
        call writloga('default',0,logmess,0,ierrw)
        istatus = icscode
        goto 9999
      endif

      
      if(idebug .gt. 0)call mmverify( )
C
C
C     ..................................................................
C     FIND THE PARENTS OF EACH NODE.
C
      call unpackpc(npoints,itp1,isn1,iparent)
C
      do it=1,ntets
         index=nen*(it-1)
         do i=1,nen
            itetp1(index+i)=iparent(itet1(index+i))
         enddo
      enddo
C
C
C     ..................................................................
C     BUILD SOME MATRIX ARRAYS FROM THE TET LIST.
c
      do i=1,n12
         irowmat(i)=0
         icolmat(i)=0
         isendnn(i)=0
      enddo
      do i=1,npoints
         irowcnt(i)=0
         irowoff(i)=0
         irowdag(i)=0
      enddo
c
      call matbld1(idiag,ipackopt,
     *             nsdtopo,nen,nef,
     *             nconn,lconn,
     *             npoints,ntets,itet1,
     *             irowmat,icolmat,isort,isendnn,
     *             irowcnt,irowoff)
      do irow=1,npoints
         do i=irowoff(irow)+1 , irowoff(irow)+irowcnt(irow)
            if(irowmat(i).eq.icolmat(i)) then
               idsave=i
            endif
         enddo
         irowdag(irow)=idsave
      enddo
 
C
C     ..................................................................
C     FORM THE SPARSE COEFFICIENT MATRIX AND THE SPARSE MATRIX PATTERN.
C
C     ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
C     ALLOCATE MEMORY FOR THE SPARSE MATRIX AND SPARSE MATRIX PATTERN.
C
      nnmax = 0
      do i=1,npoints
         nnmax=max(nnmax,irowcnt(i))
      enddo
      if(ipackopt.eq.0) then
         length=nnmax*npoints
      elseif(ipackopt.eq.1) then
         length=irowoff(npoints)+irowcnt(npoints)
      endif
C
      ncoefs=length

      if (idebug.gt.1) then
      write(logmess,'(a,i14)')
     *   "Matbld3d_stor: mmnewlen icolmat: ",length
      call writloga('default',0,logmess,0,icscode)
      endif
 
      call mmnewlen("icolmat",isubname,ipicolmat,length,icscode)
      if (icscode .ne. 0) then
        write(logmess,'(a,i5)')
     *   "Matbld3d_stor: mmnewlen failed: icolmat ",icscode
        call writloga('default',0,logmess,0,ierrw)
        istatus = icscode
      endif

      if (idebug.gt.1) then 
      write(logmess,'(a,i14)')
     *   "Matbld3d_stor: mmnewlen irowmat: ",length
      call writloga('default',0,logmess,0,ierrw)
      endif
 
      call mmnewlen("irowmat",isubname,ipirowmat,length,icscode)
      if (icscode .ne. 0) then
        write(logmess,'(a,i5)')
     *   "Matbld3d_stor: mmnewlen failed: irowmat ",icscode
        call writloga('default',0,logmess,0,ierrw)
        istatus = icscode
      endif

C
      ncoefs=length
 
      if (idebug.gt.1) then
      write(logmess,'(a,i14)')
     *   "Matbld3d_stor: mmgetblk    amat: ",length
      call writloga('default',0,logmess,0,ierrw)
      endif
 
      call mmgetblk("amat",isubname,ipamat,length,2,icscode)
      if (icscode .ne. 0) then
        write(logmess,'(a,i5)')
     *   "Matbld3d_stor: mmgetblk failed: amat ",icscode
        call writloga('default',0,logmess,0,ierrw)
        istatus = icscode
        goto 9999
      endif

      if (idebug.gt.1) then 
      write(logmess,'(a,i14)')
     *   "Matbld3d_stor: mmgetblk   xamat: ",length
      call writloga('default',0,logmess,0,ierrw)
      endif
 
      call mmgetblk("xamat",isubname,ipxamat,length,2,icscode)
      if (icscode .ne. 0) then
        write(logmess,'(a,i5)')
     *   "Matbld3d_stor: mmgetblk failed: xamat ",icscode
        call writloga('default',0,logmess,0,ierrw)
        istatus = icscode
        goto 9999
      endif

      if (idebug.gt.1) then 
      write(logmess,'(a,i14)')
     *   "Matbld3d_stor: mmgetblk   yamat: ",length
      call writloga('default',0,logmess,0,ierrw)
      endif 
 
      call mmgetblk("yamat",isubname,ipyamat,length,2,icscode)
      if (icscode .ne. 0) then
        write(logmess,'(a,i5)')
     *   "Matbld3d_stor: mmgetblk failed: yamat ",icscode
        call writloga('default',0,logmess,0,ierrw)
        istatus = icscode
        goto 9999
      endif

      if (idebug.gt.1) then 
      write(logmess,'(a,i14)')
     *   "Matbld3d_stor: mmgetblk   zamat: ",length
      call writloga('default',0,logmess,0,ierrw)
      endif
 
      call mmgetblk("zamat",isubname,ipzamat,length,2,icscode)
      if (icscode .ne. 0) then
        write(logmess,'(a,i5)')
     *   "Matbld3d_stor: mmgetblk failed: zamat ",icscode
        call writloga('default',0,logmess,0,ierrw)
        istatus = icscode
        goto 9999
      endif

      if (idebug.gt.1) then 
      write(logmess,'(a,i14)')
     *   "Matbld3d_stor: mmgetblk   iamat: ",length
      call writloga('default',0,logmess,0,ierrw)
      endif
 
      call mmgetblk("iamat",isubname,ipiamat,length,1,icscode)
      if (icscode .ne. 0) then
        write(logmess,'(a,i5)')
     *   "Matbld3d_stor: mmgetblk failed: iamat ",icscode
        call writloga('default',0,logmess,0,ierrw)
        istatus = icscode
        goto 9999
      endif

 
      if(if_vor_io .ne. 0)then
      len15 = 3*15*ntets

      if (idebug.gt.1) then 
      write(logmess,'(a,i14)')
     *   "Matbld3d_stor: mmgetblk vor_pts: ",length
      call writloga('default',0,logmess,0,ierrw)
      endif
 
      call mmgetblk("vor_pts",isubname,ipvor_pts,len15,2,icscode)
      if (icscode .ne. 0) then
        write(logmess,'(a,i5)')
     *   "Matbld3d_stor: mmgetblk failed: vor_pts ",icscode
        call writloga('default',0,logmess,0,ierrw)
        istatus = icscode
        goto 9999
      endif

      len24 = 24*ntets

      if (idebug.gt.1) then 
      write(logmess,'(a,i14)')
     *   "Matbld3d_stor: mmgetblk isign_face: ",length
      call writloga('default',0,logmess,0,ierrw)
      endif
 
      call mmgetblk
     1  ("isign_face",isubname,ipisign_face,len24,1,icscode)
      if (icscode .ne. 0) then
        write(logmess,'(a,i5)')
     *   "Matbld3d_stor: mmgetblk failed: isign_face ",icscode
        call writloga('default',0,logmess,0,ierrw)
        istatus = icscode
        goto 9999
      endif


      if(if_vor_io .eq. 2)then
C
C    Allocate array for storing bounding box of each voronoi polygon.
C
      len3_2_nn = 3*2*npoints
 
      if (idebug.gt.1) then
      write(logmess,'(a,i14)')
     *   "Matbld3d_stor: mmgetblk vor_xyz_min_max: ",len3_2_nn
      call writloga('default',0,logmess,0,ierrw)
      endif
 
      call mmgetblk
     *("vor_xyz_min_max",isubname,ipvor_xyz_min_max,len3_2_nn,2,icscode)
      if (icscode .ne. 0) then
        write(logmess,'(a,i5)')
     *   "Matbld3d_stor: mmgetblk failed: vor_xyz_min_max ",icscode
        call writloga('default',0,logmess,0,ierrw)
        istatus = icscode
        goto 9999
      endif

      endif
C
C     
      do j = 1, 3
      do i = 1, npoints
      vor_xyz_min_max(j,1,i) = 1.e20
      vor_xyz_min_max(j,2,i) = -1.e20
      
      enddo
      enddo
      
      endif
      
      if(idebug .gt. 0)call mmverify( )
C
C     ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
C     BUILD THE SPARSE MATRIX AND SPARSE MATRIX PATTERN.
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C    Check that all the tets have positive volume
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      if(idebug.ge.2) then
      do it=1,ntets
         i1=itet(1,it)
         i2=itet(2,it)
         i3=itet(3,it)
         i4=itet(4,it)
         x1=xic(i1)
         y1=yic(i1)
         z1=zic(i1)
         x2=xic(i2)
         y2=yic(i2)
         z2=zic(i2)
         x3=xic(i3)
         y3=yic(i3)
         z3=zic(i3)
         x4=xic(i4)
         y4=yic(i4)
         z4=zic(i4)
         dx=  (y2-y3)*(z4-z3)-(y4-y3)*(z2-z3)
         dy=-((x2-x3)*(z4-z3)-(x4-x3)*(z2-z3))
         dz=  (x2-x3)*(y4-y3)-(x4-x3)*(y2-y3)
         voltet=-((x3-x1)*dx+(y3-y1)*dy+(z3-y1)*dz) / 6.0
         if(voltet .le. 0.0)then
C***           print *, 'Negative Volume ', i1,i2,i3,i4,voltet
           write(logmess,'(a,4i10,1pe15.7)')
     *           "Negative Volume ",i1,i2,i3,i4,voltet
           call writloga('default',0,logmess,0,ierrw)
         endif
      enddo
      endif
C
C     ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
C     BUILD THE SPARSE MATRIX AND SPARSE MATRIX PATTERN.
C
C
      length=n12
 
      if (idebug.gt.1) then
      write(logmess,'(a,i14)')
     *   "Matbld3d_stor: mmgetblk  xconst: ",length
      call writloga('default',0,logmess,0,ierrw)
      endif
 
      call mmgetblk("xconst",isubname,ipxconst,length,2,icscode)
      if (icscode .ne. 0) then
        write(logmess,'(a,i5)')
     *   "Matbld3d_stor: mmgetblk failed: xconst ",icscode
        call writloga('default',0,logmess,0,ierrw)
        istatus = icscode
        goto 9999
      endif

      if (idebug.gt.1) then 
      write(logmess,'(a,i14)')
     *   "Matbld3d_stor: mmgetblk  yconst: ",length
      call writloga('default',0,logmess,0,ierrw)
      endif
 
      call mmgetblk("yconst",isubname,ipyconst,length,2,icscode)
      if (icscode .ne. 0) then
        write(logmess,'(a,i5)')
     *   "Matbld3d_stor: mmgetblk failed: yconst ",icscode
        call writloga('default',0,logmess,0,ierrw)
        istatus = icscode
        goto 9999
      endif

      if (idebug.gt.1) then 
      write(logmess,'(a,i14)')
     *   "Matbld3d_stor: mmgetblk  zconst: ",length
      call writloga('default',0,logmess,0,ierrw)
      endif
 
      call mmgetblk("zconst",isubname,ipzconst,length,2,icscode)
      if (icscode .ne. 0) then
        write(logmess,'(a,i5)')
     *   "Matbld3d_stor: mmgetblk failed: zconst ",icscode
        call writloga('default',0,logmess,0,ierrw)
        istatus = icscode
        goto 9999
      endif

 
      length=npoints

      if (idebug.gt.1) then 
      write(logmess,'(a,i14)')
     *   "Matbld3d_stor: mmgetblk   volic: ",length
      call writloga('default',0,logmess,0,ierrw)
      endif
 
      call mmgetblk("volic",isubname,ipvolic,length,2,icscode)
      if (icscode .ne. 0) then
        write(logmess,'(a,i5)')
     *   "Matbld3d_stor: mmgetblk failed: volic ",icscode
        call writloga('default',0,logmess,0,ierrw)
        istatus = icscode
        goto 9999
      endif

 
      do i=1,npoints
         volic(i)=0.0
      enddo
 
      voltot=0.0
      voltot_vor1=0.0
      voltot_vor2=0.0
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   Loop through all the tets and calculate the voronoi volume
C   associated with each node, and the voronoi area associate
C   with each edge.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      do it=1,ntets
 
         i1=itet(1,it)
         i2=itet(2,it)
         i3=itet(3,it)
         i4=itet(4,it)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C    Calculate the 2D voronoi point of each of the 4
C    triangular faces of the tet.
C
ccccccccccccccccccccccccccccccccccccccccccccccc
         xa=xic(i2)
         ya=yic(i2)
         za=zic(i2)
         xfac=1.0d+00
         xb=xfac*(xic(i3)-xa)
         yb=xfac*(yic(i3)-ya)
         zb=xfac*(zic(i3)-za)
         xd=xfac*(xic(i4)-xa)
         yd=xfac*(yic(i4)-ya)
         zd=xfac*(zic(i4)-za)
         xn1=crosx(xb,yb,zb,xd,yd,zd)
         yn1=crosy(xb,yb,zb,xd,yd,zd)
         zn1=crosz(xb,yb,zb,xd,yd,zd)
         xn=crosx(xb,yb,zb,xn1,yn1,zn1)
         yn=crosy(xb,yb,zb,xn1,yn1,zn1)
         zn=crosz(xb,yb,zb,xn1,yn1,zn1)
         rn=1.0/sqrt(xn*xn+yn*yn+zn*zn)
         xn=xn*rn
         yn=yn*rn
         zn=zn*rn
         dotb3=xb*xd+yb*yd+zb*zd
         dot3=dotb3/(xd*xd+yd*yd+zd*zd)
         rb3=1.0/(xb*xb+yb*yb+zb*zb)
         ql=(1.0-dot3)/(1.0-dot3*dotb3*rb3+1.0d-30)
         xl=0.5*(ql*(xd-dotb3*rb3*xb)+xb)
         yl=0.5*(ql*(yd-dotb3*rb3*yb)+yb)
         zl=0.5*(ql*(zd-dotb3*rb3*zb)+zb)
         ds11=sqrt((xl)**2+(yl)**2+(zl)**2)
         ds21=sqrt((xl-xb)**2+(yl-yb)**2+(zl-zb)**2)
         ds31=sqrt((xl-xd)**2+(yl-yd)**2+(zl-zd)**2)
         xv1=xl+xa
         yv1=yl+ya
         zv1=zl+za
 
         xa=xic(i1)
         ya=yic(i1)
         za=zic(i1)
         xfac=1.0d+00
         xb=xfac*(xic(i4)-xa)
         yb=xfac*(yic(i4)-ya)
         zb=xfac*(zic(i4)-za)
         xd=xfac*(xic(i3)-xa)
         yd=xfac*(yic(i3)-ya)
         zd=xfac*(zic(i3)-za)
         xn1=crosx(xb,yb,zb,xd,yd,zd)
         yn1=crosy(xb,yb,zb,xd,yd,zd)
         zn1=crosz(xb,yb,zb,xd,yd,zd)
         xn=crosx(xb,yb,zb,xn1,yn1,zn1)
         yn=crosy(xb,yb,zb,xn1,yn1,zn1)
         zn=crosz(xb,yb,zb,xn1,yn1,zn1)
         rn=1.0/sqrt(xn*xn+yn*yn+zn*zn)
         xn=xn*rn
         yn=yn*rn
         zn=zn*rn
         dotb3=xb*xd+yb*yd+zb*zd
         dot3=dotb3/(xd*xd+yd*yd+zd*zd)
         rb3=1.0/(xb*xb+yb*yb+zb*zb)
         ql=(1.0-dot3)/(1.0-dot3*dotb3*rb3+1.0d-30)
         xl=0.5*(ql*(xd-dotb3*rb3*xb)+xb)
         yl=0.5*(ql*(yd-dotb3*rb3*yb)+yb)
         zl=0.5*(ql*(zd-dotb3*rb3*zb)+zb)
         ds12=sqrt((xl)**2+(yl)**2+(zl)**2)
         ds22=sqrt((xl-xb)**2+(yl-yb)**2+(zl-zb)**2)
         ds32=sqrt((xl-xd)**2+(yl-yd)**2+(zl-zd)**2)
         xv2=xl+xa
         yv2=yl+ya
         zv2=zl+za
 
         xa=xic(i1)
         ya=yic(i1)
         za=zic(i1)
         xfac=1.0d+00
         xb=xfac*(xic(i2)-xa)
         yb=xfac*(yic(i2)-ya)
         zb=xfac*(zic(i2)-za)
         xd=xfac*(xic(i4)-xa)
         yd=xfac*(yic(i4)-ya)
         zd=xfac*(zic(i4)-za)
         xn1=crosx(xb,yb,zb,xd,yd,zd)
         yn1=crosy(xb,yb,zb,xd,yd,zd)
         zn1=crosz(xb,yb,zb,xd,yd,zd)
         xn=crosx(xb,yb,zb,xn1,yn1,zn1)
         yn=crosy(xb,yb,zb,xn1,yn1,zn1)
         zn=crosz(xb,yb,zb,xn1,yn1,zn1)
         rn=1.0/sqrt(xn*xn+yn*yn+zn*zn)
         xn=xn*rn
         yn=yn*rn
         zn=zn*rn
         dotb3=xb*xd+yb*yd+zb*zd
         dot3=dotb3/(xd*xd+yd*yd+zd*zd)
         rb3=1.0/(xb*xb+yb*yb+zb*zb)
         ql=(1.0-dot3)/(1.0-dot3*dotb3*rb3+1.0d-30)
         xl=0.5*(ql*(xd-dotb3*rb3*xb)+xb)
         yl=0.5*(ql*(yd-dotb3*rb3*yb)+yb)
         zl=0.5*(ql*(zd-dotb3*rb3*zb)+zb)
         ds13=sqrt((xl)**2+(yl)**2+(zl)**2)
         ds23=sqrt((xl-xb)**2+(yl-yb)**2+(zl-zb)**2)
         ds33=sqrt((xl-xd)**2+(yl-yd)**2+(zl-zd)**2)
         xv3=xl+xa
         yv3=yl+ya
         zv3=zl+za
 
         xa=xic(i1)
         ya=yic(i1)
         za=zic(i1)
         xfac=1.0d+00
         xb=xfac*(xic(i3)-xa)
         yb=xfac*(yic(i3)-ya)
         zb=xfac*(zic(i3)-za)
         xd=xfac*(xic(i2)-xa)
         yd=xfac*(yic(i2)-ya)
         zd=xfac*(zic(i2)-za)
         xn1=crosx(xb,yb,zb,xd,yd,zd)
         yn1=crosy(xb,yb,zb,xd,yd,zd)
         zn1=crosz(xb,yb,zb,xd,yd,zd)
         xn=crosx(xb,yb,zb,xn1,yn1,zn1)
         yn=crosy(xb,yb,zb,xn1,yn1,zn1)
         zn=crosz(xb,yb,zb,xn1,yn1,zn1)
         rn=1.0/sqrt(xn*xn+yn*yn+zn*zn)
         xn=xn*rn
         yn=yn*rn
         zn=zn*rn
         dotb3=xb*xd+yb*yd+zb*zd
         dot3=dotb3/(xd*xd+yd*yd+zd*zd)
         rb3=1.0/(xb*xb+yb*yb+zb*zb)
         ql=(1.0-dot3)/(1.0-dot3*dotb3*rb3+1.0d-30)
         xl=0.5*(ql*(xd-dotb3*rb3*xb)+xb)
         yl=0.5*(ql*(yd-dotb3*rb3*yb)+yb)
         zl=0.5*(ql*(zd-dotb3*rb3*zb)+zb)
         ds14=sqrt((xl)**2+(yl)**2+(zl)**2)
         ds24=sqrt((xl-xb)**2+(yl-yb)**2+(zl-zb)**2)
         ds34=sqrt((xl-xd)**2+(yl-yd)**2+(zl-zd)**2)
         xv4=xl+xa
         yv4=yl+ya
         zv4=zl+za
ccccccccccccccccccccccccccccccccccccccccccccccc
c
c    calculate the vector area of the tet faces
c
ccccccccccccccccccccccccccccccccccccccccccccccc
         x1=xic(i1)
         y1=yic(i1)
         z1=zic(i1)
         x2=xic(i2)
         y2=yic(i2)
         z2=zic(i2)
         x3=xic(i3)
         y3=yic(i3)
         z3=zic(i3)
         xl1=xic(i1)
         yl1=yic(i1)
         zl1=zic(i1)
         xl2=xic(i2)
         yl2=yic(i2)
         zl2=zic(i2)
         xl3=xic(i3)
         yl3=yic(i3)
         zl3=zic(i3)
         xl4=xic(i4)
         yl4=yic(i4)
         zl4=zic(i4)
         ax1=  (yl3-yl2)*(zl4-zl2)-(zl3-zl2)*(yl4-yl2)
         ay1=-((xl3-xl2)*(zl4-zl2)-(zl3-zl2)*(xl4-xl2))
         az1=  (xl3-xl2)*(yl4-yl2)-(yl3-yl2)*(xl4-xl2)
         ax2=  (yl4-yl1)*(zl3-zl1)-(zl4-zl1)*(yl3-yl1)
         ay2=-((xl4-xl1)*(zl3-zl1)-(zl4-zl1)*(xl3-xl1))
         az2=  (xl4-xl1)*(yl3-yl1)-(yl4-yl1)*(xl3-xl1)
         ax3=  (yl2-yl1)*(zl4-zl1)-(zl2-zl1)*(yl4-yl1)
         ay3=-((xl2-xl1)*(zl4-zl1)-(zl2-zl1)*(xl4-xl1))
         az3=  (xl2-xl1)*(yl4-yl1)-(yl2-yl1)*(xl4-xl1)
         ax4=  (yl3-yl1)*(zl2-zl1)-(zl3-zl1)*(yl2-yl1)
         ay4=-((xl3-xl1)*(zl2-zl1)-(zl3-zl1)*(xl2-xl1))
         az4=  (xl3-xl1)*(yl2-yl1)-(yl3-yl1)*(xl2-xl1)
         voltet=-((xl4-xl1)*ax4+(yl4-yl1)*ay4+(zl4-zl1)*az4)
         voltot=voltot+voltet
ccccccccccccccccccccccccccccccccccccccccccccccc
c
c    calculate the median point on each of the tet faces
c    (these do not get used anywhere. cwg)
c
ccccccccccccccccccccccccccccccccccccccccccccccc
         three_i = 1.0d0/3.0d0
         x234=(xl2+xl3+xl4)*three_i
         y234=(yl2+yl3+yl4)*three_i
         z234=(zl2+zl3+zl4)*three_i
         x143=(xl1+xl4+xl3)*three_i
         y143=(yl1+yl4+yl3)*three_i
         z143=(zl1+zl4+zl3)*three_i
         x124=(xl1+xl2+xl4)*three_i
         y124=(yl1+yl2+yl4)*three_i
         z124=(zl1+zl2+zl4)*three_i
         x132=(xl1+xl3+xl2)*three_i
         y132=(yl1+yl3+yl2)*three_i
         z132=(zl1+zl3+zl2)*three_i
ccccccccccccccccccccccccccccccccccccccccccccccc
c
c    calculate voronoi point (xvor, yvor, zvor) of the tet
c
ccccccccccccccccccccccccccccccccccccccccccccccc
         xa=xl2
         ya=yl2
         za=zl2
         xb=xl3-xa
         yb=yl3-ya
         zb=zl3-za
         xc=xl4-xa
         yc=yl4-ya
         zc=zl4-za
         xd=xl1-xa
         yd=yl1-ya
         zd=zl1-za
         xn=  yb*zc-yc*zb
         yn=-(xb*zc-xc*zb)
         zn=  xb*yc-xc*yb
         x2=  yn*zb-yb*zn
         y2=-(xn*zb-xb*zn)
         z2=  xn*yb-xb*yn
         q=-0.5*(xc*xb+yc*yb+zc*zb-xc*xc-yc*yc-zc*zc)/
     *          (x2*xc+y2*yc+z2*zc+1.0e-30)
         xl=q*x2+0.5*xb
         yl=q*y2+0.5*yb
         zl=q*z2+0.5*zb
         dvor=-0.5*(xd*xd+yd*yd+zd*zd)
         qvor2=-(xd*xl+yd*yl+zd*zl+dvor)/(xd*xn+yd*yn+zd*zn+1.0d-30)
         xvor=qvor2*xn+xl+xa
         yvor=qvor2*yn+yl+ya
         zvor=qvor2*zn+zl+za
ccccccccccccccccccccccccccccccccccccccccccccccc
c
c        calculate voronoi point a different way and compare result
c
ccccccccccccccccccccccccccccccccccccccccccccccc
         call voronoi_center(
     1        xic(i1),yic(i1),zic(i1),
     2        xic(i2),yic(i2),zic(i2),
     3        xic(i3),yic(i3),zic(i3),
     4        xic(i4),yic(i4),zic(i4),
     5        xcenv,ycenv,zcenv, radiusv)
 
         xdifvor = xvor - xcenv
         ydifvor = yvor - ycenv
         zdifvor = zvor - zcenv
         xyzdif = xdifvor**2 + ydifvor**2 + zdifvor**2
         xyzsiz = xvor   **2 + yvor   **2 + zvor   **2
 
         if(xyzdif/xyzsiz .gt. 1.e-8)then
         write(6,*)'Voronoi Point Calculation Error ',it
         write(6,*)xvor,yvor,zvor
         write(6,*)xcenv,ycenv,zcenv, radiusv
         endif
 
         distsqa=(xvor-xl2)**2+(yvor-yl2)**2+(zvor-zl2)**2
         distsqb=(xvor-xl3)**2+(yvor-yl3)**2+(zvor-zl3)**2
         distsqc=(xvor-xl4)**2+(yvor-yl4)**2+(zvor-zl4)**2
         distsqd=(xvor-xl1)**2+(yvor-yl1)**2+(zvor-zl1)**2
ccccccccccccccccccccccccccccccccccccccccccccccc
c
c
c
ccccccccccccccccccccccccccccccccccccccccccccccc
         x1=xic(i1)
         y1=yic(i1)
         z1=zic(i1)
         x2=xic(i2)
         y2=yic(i2)
         z2=zic(i2)
         x3=xic(i3)
         y3=yic(i3)
         z3=zic(i3)
         x4=xic(i4)
         y4=yic(i4)
         z4=zic(i4)
         x12=0.5*(xic(i1)+xic(i2))
         y12=0.5*(yic(i1)+yic(i2))
         z12=0.5*(zic(i1)+zic(i2))
         x13=0.5*(xic(i1)+xic(i3))
         y13=0.5*(yic(i1)+yic(i3))
         z13=0.5*(zic(i1)+zic(i3))
         x14=0.5*(xic(i1)+xic(i4))
         y14=0.5*(yic(i1)+yic(i4))
         z14=0.5*(zic(i1)+zic(i4))
         x23=0.5*(xic(i2)+xic(i3))
         y23=0.5*(yic(i2)+yic(i3))
         z23=0.5*(zic(i2)+zic(i3))
c         x24=0.5*(xic(i2)+xic(i4))
c   Nothing special about this variable. I was just
c   that when I used the above statement the compiler would have a problem.
c   Carl Gable
c
c  "matbld3d_stor.f", line 605: Error: assignment to a non-variable
c
         x24n=0.5*(xic(i2)+xic(i4))
         y24=0.5*(yic(i2)+yic(i4))
         z24=0.5*(zic(i2)+zic(i4))
         x34=0.5*(xic(i3)+xic(i4))
         y34=0.5*(yic(i3)+yic(i4))
         z34=0.5*(zic(i3)+zic(i4))
 
         if(imedian.eq.1) then
            xv1=(xic(i2)+xic(i3)+xic(i4))/3.0
            yv1=(yic(i2)+yic(i3)+yic(i4))/3.0
            zv1=(zic(i2)+zic(i3)+zic(i4))/3.0
            xv2=(xic(i1)+xic(i3)+xic(i4))/3.0
            yv2=(yic(i1)+yic(i3)+yic(i4))/3.0
            zv2=(zic(i1)+zic(i3)+zic(i4))/3.0
            xv3=(xic(i1)+xic(i2)+xic(i4))/3.0
            yv3=(yic(i1)+yic(i2)+yic(i4))/3.0
            zv3=(zic(i1)+zic(i2)+zic(i4))/3.0
            xv4=(xic(i1)+xic(i2)+xic(i3))/3.0
            yv4=(yic(i1)+yic(i2)+yic(i3))/3.0
            zv4=(zic(i1)+zic(i2)+zic(i3))/3.0
            xm=(xic(i1)+xic(i2)+xic(i3)+xic(i4))/4.0
            ym=(yic(i1)+yic(i2)+yic(i3)+yic(i4))/4.0
            zm=(zic(i1)+zic(i2)+zic(i3)+zic(i4))/4.0
            xvor=xm
            yvor=ym
            zvor=zm
         endif
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C    Calculate the face area and volume of the 12
C    tets used to calculate voronoi edge area.
C    Each tet is made from a vertex node, an edge bisector node,
C    a face 2D voronoi node and the 3D voronoi point.
C
C    Only 6 face areas are calculated since shared faces
C    have the same area, but the vector area equal but
C    of the opposite sign.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
         ax11=  (yv4-y12)*(zvor-z12)-(yvor-y12)*(zv4-z12)
         ay11=-((xv4-x12)*(zvor-z12)-(xvor-x12)*(zv4-z12))
         az11=  (xv4-x12)*(yvor-y12)-(xvor-x12)*(yv4-y12)
         vol11=-((x1-x12)*ax11+(y1-y12)*ay11+(z1-z12)*az11)
         vol13= ((x2-x12)*ax11+(y2-y12)*ay11+(z2-z12)*az11)
 
         ax12=-((yv3-y12)*(zvor-z12)-(yvor-y12)*(zv3-z12))
         ay12=+((xv3-x12)*(zvor-z12)-(xvor-x12)*(zv3-z12))
         az12=-((xv3-x12)*(yvor-y12)-(xvor-x12)*(yv3-y12))
         vol12=-((x1-x12)*ax12+(y1-y12)*ay12+(z1-z12)*az12)
         vol14= ((x2-x12)*ax12+(y2-y12)*ay12+(z2-z12)*az12)
 
         ax1=half*(ax11+ax12)
         ay1=half*(ay11+ay12)
         az1=half*(az11+az12)
         xdot11=(x2-x1)*ax11+(y2-y1)*ay11+(z2-z1)*az11
         xdot12=(x2-x1)*ax12+(y2-y1)*ay12+(z2-z1)*az12
         xdot1=(x2-x1)*ax1+(y2-y1)*ay1+(z2-z1)*az1
 
         ax21=  (yv2-y13)*(zvor-z13)-(yvor-y13)*(zv2-z13)
         ay21=-((xv2-x13)*(zvor-z13)-(xvor-x13)*(zv2-z13))
         az21=  (xv2-x13)*(yvor-y13)-(xvor-x13)*(yv2-y13)
         vol21=-((x1-x13)*ax21+(y1-y13)*ay21+(z1-z13)*az21)
         vol23= ((x3-x13)*ax21+(y3-y13)*ay21+(z3-z13)*az21)
 
         ax22=-((yv4-y13)*(zvor-z13)-(yvor-y13)*(zv4-z13))
         ay22=+((xv4-x13)*(zvor-z13)-(xvor-x13)*(zv4-z13))
         az22=-((xv4-x13)*(yvor-y13)-(xvor-x13)*(yv4-y13))
         vol22=-((x1-x13)*ax22+(y1-y13)*ay22+(z1-z13)*az22)
         vol24= ((x3-x13)*ax22+(y3-y13)*ay22+(z3-z13)*az22)
 
         ax2=half*(ax21+ax22)
         ay2=half*(ay21+ay22)
         az2=half*(az21+az22)
         xdot21=(x3-x1)*ax21+(y3-y1)*ay21+(z3-z1)*az21
         xdot22=(x3-x1)*ax22+(y3-y1)*ay22+(z3-z1)*az22
         xdot2=(x3-x1)*ax2+(y3-y1)*ay2+(z3-z1)*az2
 
         ax31=  (yv3-y14)*(zvor-z14)-(yvor-y14)*(zv3-z14)
         ay31=-((xv3-x14)*(zvor-z14)-(xvor-x14)*(zv3-z14))
         az31=  (xv3-x14)*(yvor-y14)-(xvor-x14)*(yv3-y14)
         vol31=-((x1-x14)*ax31+(y1-y14)*ay31+(z1-z14)*az31)
         vol33= ((x4-x14)*ax31+(y4-y14)*ay31+(z4-z14)*az31)
 
         ax32=-((yv2-y14)*(zvor-z14)-(yvor-y14)*(zv2-z14))
         ay32=+((xv2-x14)*(zvor-z14)-(xvor-x14)*(zv2-z14))
         az32=-((xv2-x14)*(yvor-y14)-(xvor-x14)*(yv2-y14))
         vol32=-((x1-x14)*ax32+(y1-y14)*ay32+(z1-z14)*az32)
         vol34= ((x4-x14)*ax32+(y4-y14)*ay32+(z4-z14)*az32)
 
         ax3=half*(ax31+ax32)
         ay3=half*(ay31+ay32)
         az3=half*(az31+az32)
         xdot31=(x4-x1)*ax31+(y4-y1)*ay31+(z4-z1)*az31
         xdot32=(x4-x1)*ax32+(y4-y1)*ay32+(z4-z1)*az32
         xdot3=(x4-x1)*ax3+(y4-y1)*ay3+(z4-z1)*az3
 
         ax41=  (yv4-y23)*(zvor-z23)-(yvor-y23)*(zv4-z23)
         ay41=-((xv4-x23)*(zvor-z23)-(xvor-x23)*(zv4-z23))
         az41=  (xv4-x23)*(yvor-y23)-(xvor-x23)*(yv4-y23)
         vol41=-((x2-x23)*ax41+(y2-y23)*ay41+(z2-z23)*az41)
         vol43= ((x3-x23)*ax41+(y3-y23)*ay41+(z3-z23)*az41)
 
         ax42=-((yv1-y23)*(zvor-z23)-(yvor-y23)*(zv1-z23))
         ay42=+((xv1-x23)*(zvor-z23)-(xvor-x23)*(zv1-z23))
         az42=-((xv1-x23)*(yvor-y23)-(xvor-x23)*(yv1-y23))
         vol42=-((x2-x23)*ax42+(y2-y23)*ay42+(z2-z23)*az42)
         vol44= ((x3-x23)*ax42+(y3-y23)*ay42+(z3-z23)*az42)
 
         ax4=half*(ax41+ax42)
         ay4=half*(ay41+ay42)
         az4=half*(az41+az42)
         xdot41=(x3-x2)*ax41+(y3-y2)*ay41+(z3-z2)*az41
         xdot42=(x3-x2)*ax42+(y3-y2)*ay42+(z3-z2)*az42
         xdot4=(x3-x2)*ax4+(y3-y2)*ay4+(z3-z2)*az4
 
         ax51=  (yv1-y24)*(zvor-z24)-(yvor-y24)*(zv1-z24)
         ay51=-((xv1-x24n)*(zvor-z24)-(xvor-x24n)*(zv1-z24))
         az51=  (xv1-x24n)*(yvor-y24)-(xvor-x24n)*(yv1-y24)
         vol51=-((x2-x24n)*ax51+(y2-y24)*ay51+(z2-z24)*az51)
         vol53= ((x4-x24n)*ax51+(y4-y24)*ay51+(z4-z24)*az51)
 
         ax52=-((yv3-y24)*(zvor-z24)-(yvor-y24)*(zv3-z24))
         ay52=+((xv3-x24n)*(zvor-z24)-(xvor-x24n)*(zv3-z24))
         az52=-((xv3-x24n)*(yvor-y24)-(xvor-x24n)*(yv3-y24))
         vol52=-((x2-x24n)*ax52+(y2-y24)*ay52+(z2-z24)*az52)
         vol54= ((x4-x24n)*ax52+(y4-y24)*ay52+(z4-z24)*az52)
 
         ax5=half*(ax51+ax52)
         ay5=half*(ay51+ay52)
         az5=half*(az51+az52)
         xdot51=(x4-x2)*ax51+(y4-y2)*ay51+(z4-z2)*az51
         xdot52=(x4-x2)*ax52+(y4-y2)*ay52+(z4-z2)*az52
         xdot5=(x4-x2)*ax5+(y4-y2)*ay5+(z4-z2)*az5
 
         ax61=  (yv2-y34)*(zvor-z34)-(yvor-y34)*(zv2-z34)
         ay61=-((xv2-x34)*(zvor-z34)-(xvor-x34)*(zv2-z34))
         az61=  (xv2-x34)*(yvor-y34)-(xvor-x34)*(yv2-y34)
         vol61=-((x3-x34)*ax61+(y3-y34)*ay61+(z3-z34)*az61)
         vol63= ((x4-x34)*ax61+(y4-y34)*ay61+(z4-z34)*az61)
 
         ax62=-((yv1-y34)*(zvor-z34)-(yvor-y34)*(zv1-z34))
         ay62=+((xv1-x34)*(zvor-z34)-(xvor-x34)*(zv1-z34))
         az62=-((xv1-x34)*(yvor-y34)-(xvor-x34)*(yv1-y34))
         vol62=-((x3-x34)*ax62+(y3-y34)*ay62+(z3-z34)*az62)
         vol64= ((x4-x34)*ax62+(y4-y34)*ay62+(z4-z34)*az62)
 
         ax6=half*(ax61+ax62)
         ay6=half*(ay61+ay62)
         az6=half*(az61+az62)
         xdot61=(x4-x3)*ax61+(y4-y3)*ay61+(z4-z3)*az61
         xdot62=(x4-x3)*ax62+(y4-y3)*ay62+(z4-z3)*az62
         xdot6=(x4-x3)*ax6+(y4-y3)*ay6+(z4-z3)*az6
 
C
        if(if_vor_io .ne. 0)then
        vor_pts(1,1,it)=x1
        vor_pts(2,1,it)=y1
        vor_pts(3,1,it)=z1
        vor_pts(1,2,it)=x2
        vor_pts(2,2,it)=y2
        vor_pts(3,2,it)=z2
        vor_pts(1,3,it)=x3
        vor_pts(2,3,it)=y3
        vor_pts(3,3,it)=z3
        vor_pts(1,4,it)=x4
        vor_pts(2,4,it)=y4
        vor_pts(3,4,it)=z4
        vor_pts(1,5,it)=x12
        vor_pts(2,5,it)=y12
        vor_pts(3,5,it)=z12
        vor_pts(1,6,it)=x13
        vor_pts(2,6,it)=y13
        vor_pts(3,6,it)=z13
        vor_pts(1,7,it)=x14
        vor_pts(2,7,it)=y14
        vor_pts(3,7,it)=z14
        vor_pts(1,8,it)=x23
        vor_pts(2,8,it)=y23
        vor_pts(3,8,it)=z23
        vor_pts(1,9,it)=x24n
        vor_pts(2,9,it)=y24
        vor_pts(3,9,it)=z24
        vor_pts(1,10,it)=x34
        vor_pts(2,10,it)=y34
        vor_pts(3,10,it)=z34
        vor_pts(1,11,it)=xv1
        vor_pts(2,11,it)=yv1
        vor_pts(3,11,it)=zv1
        vor_pts(1,12,it)=xv2
        vor_pts(2,12,it)=yv2
        vor_pts(3,12,it)=zv2
        vor_pts(1,13,it)=xv3
        vor_pts(2,13,it)=yv3
        vor_pts(3,13,it)=zv3
        vor_pts(1,14,it)=xv4
        vor_pts(2,14,it)=yv4
        vor_pts(3,14,it)=zv4
        vor_pts(1,15,it)=xvor
        vor_pts(2,15,it)=yvor
        vor_pts(3,15,it)=zvor
C
C       Update bounding box for each voronoi cell
C
        if(if_vor_io .eq. 2)then
          call vor_pts_minmax
     1      (vor_pts(1,1,it),i1,i2,i3,i4, vor_xyz_min_max, npoints)
        endif
C
        index_f = (it-1)*24
        isign_dot(1) = sign(one,xdot2)
        isign_dot(2) = sign(one,xdot2)
        isign_dot(3) = sign(one,xdot1)
        isign_dot(4) = sign(one,xdot1)
        isign_dot(5) = sign(one,xdot3)
        isign_dot(6) = sign(one,xdot3)
        isign_dot(7) = sign(one,xdot1)
        isign_dot(8) = sign(one,xdot1)
        isign_dot(9) = sign(one,xdot4)
        isign_dot(10) = sign(one,xdot4)
        isign_dot(11) = sign(one,xdot5)
        isign_dot(12) = sign(one,xdot5)
        isign_dot(13) = sign(one,xdot2)
        isign_dot(14) = sign(one,xdot2)
        isign_dot(15) = sign(one,xdot4)
        isign_dot(16) = sign(one,xdot4)
        isign_dot(17) = sign(one,xdot6)
        isign_dot(18) = sign(one,xdot6)
        isign_dot(19) = sign(one,xdot3)
        isign_dot(20) = sign(one,xdot3)
        isign_dot(21) = sign(one,xdot5)
        isign_dot(22) = sign(one,xdot5)
        isign_dot(23) = sign(one,xdot6)
        isign_dot(24) = sign(one,xdot6)
 
        index_f = (it-1)*24
        isign_face(index_f + 1) = 2 + sign(one,xdot2)
        isign_face(index_f + 2) = 2 +sign(one,xdot2)
        isign_face(index_f + 3) = 2 +sign(one,xdot1)
        isign_face(index_f + 4) = 2 +sign(one,xdot1)
        isign_face(index_f + 5) = 2 +sign(one,xdot3)
        isign_face(index_f + 6) = 2 +sign(one,xdot3)
        isign_face(index_f + 7) = 2 +sign(one,xdot1)
        isign_face(index_f + 8) = 2 +sign(one,xdot1)
        isign_face(index_f + 9) = 2 +sign(one,xdot4)
        isign_face(index_f + 10) = 2 +sign(one,xdot4)
        isign_face(index_f + 11) = 2 +sign(one,xdot5)
        isign_face(index_f + 12) = 2 +sign(one,xdot5)
        isign_face(index_f + 13) = 2 +sign(one,xdot2)
        isign_face(index_f + 14) = 2 +sign(one,xdot2)
        isign_face(index_f + 15) = 2 +sign(one,xdot4)
        isign_face(index_f + 16) = 2 +sign(one,xdot4)
        isign_face(index_f + 17) = 2 +sign(one,xdot6)
        isign_face(index_f + 18) = 2 +sign(one,xdot6)
        isign_face(index_f + 19) = 2 +sign(one,xdot3)
        isign_face(index_f + 20) = 2 +sign(one,xdot3)
        isign_face(index_f + 21) = 2 +sign(one,xdot5)
        isign_face(index_f + 22) = 2 +sign(one,xdot5)
        isign_face(index_f + 23) = 2 +sign(one,xdot6)
        isign_face(index_f + 24) = 2 +sign(one,xdot6)
       endif
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C    The sum of the sub-divided tets is equal
C    to the sum of the tet being considered.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
         vol1a=vol11+vol12
         vol1b=vol13+vol14
         vol2a=vol21+vol22
         vol2b=vol23+vol24
         vol3a=vol31+vol32
         vol3b=vol33+vol34
         vol4a=vol41+vol42
         vol4b=vol43+vol44
         vol5a=vol51+vol52
         vol5b=vol53+vol54
         vol6a=vol61+vol62
         vol6b=vol63+vol64
 
         voltet_vor1=vol1a+vol1b+
     *           vol2a+vol2b+
     *           vol3a+vol3b+
     *           vol4a+vol4b+
     *           vol5a+vol5b+
     *           vol6a+vol6b
C
C        ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
C        CALCULATE THE VOLUME OF THE TET USING THE VORONOI POINT AS THE
C           PIVOT POINT.
C
         x1=xvor
         y1=yvor
         z1=zvor
         x2=xic(i2)
         y2=yic(i2)
         z2=zic(i2)
         x3=xic(i3)
         y3=yic(i3)
         z3=zic(i3)
         x4=xic(i4)
         y4=yic(i4)
         z4=zic(i4)
         dx=  (y2-y3)*(z4-z3)-(y4-y3)*(z2-z3)
         dy=-((x2-x3)*(z4-z3)-(x4-x3)*(z2-z3))
         dz=  (x2-x3)*(y4-y3)-(x4-x3)*(y2-y3)
         volvortet1=-((x3-x1)*dx+(y3-y1)*dy+(z3-y1)*dz)
         x2=xic(i1)
         y2=yic(i1)
         z2=zic(i1)
         x3=xic(i4)
         y3=yic(i4)
         z3=zic(i4)
         x4=xic(i3)
         y4=yic(i3)
         z4=zic(i3)
         dx=  (y2-y3)*(z4-z3)-(y4-y3)*(z2-z3)
         dy=-((x2-x3)*(z4-z3)-(x4-x3)*(z2-z3))
         dz=  (x2-x3)*(y4-y3)-(x4-x3)*(y2-y3)
         volvortet2=-((x3-x1)*dx+(y3-y1)*dy+(z3-y1)*dz)
         x2=xic(i1)
         y2=yic(i1)
         z2=zic(i1)
         x3=xic(i2)
         y3=yic(i2)
         z3=zic(i2)
         x4=xic(i4)
         y4=yic(i4)
         z4=zic(i4)
         dx=  (y2-y3)*(z4-z3)-(y4-y3)*(z2-z3)
         dy=-((x2-x3)*(z4-z3)-(x4-x3)*(z2-z3))
         dz=  (x2-x3)*(y4-y3)-(x4-x3)*(y2-y3)
         volvortet3=-((x3-x1)*dx+(y3-y1)*dy+(z3-y1)*dz)
         x2=xic(i1)
         y2=yic(i1)
         z2=zic(i1)
         x3=xic(i3)
         y3=yic(i3)
         z3=zic(i3)
         x4=xic(i2)
         y4=yic(i2)
         z4=zic(i2)
         dx=  (y2-y3)*(z4-z3)-(y4-y3)*(z2-z3)
         dy=-((x2-x3)*(z4-z3)-(x4-x3)*(z2-z3))
         dz=  (x2-x3)*(y4-y3)-(x4-x3)*(y2-y3)
         volvortet4=-((x3-x1)*dx+(y3-y1)*dy+(z3-y1)*dz)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   Volume is being calculated and compared 3 different ways.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
         voltot_vor1=voltot_vor1+voltet_vor1
         voltet_vor2=volvortet1+volvortet2+volvortet3+volvortet4
         voltot_vor2=voltot_vor2+voltet_vor2
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   Add the calculated voronoi volume to the associated node.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
         i1p=iparent(i1)
         i2p=iparent(i2)
         i3p=iparent(i3)
         i4p=iparent(i4)
         volic(i1)=volic(i1)+((vol1a+vol2a+vol3a))/6.0
         volic(i2)=volic(i2)+((vol1b+vol4a+vol5a))/6.0
         volic(i3)=volic(i3)+((vol2b+vol4b+vol6a))/6.0
         volic(i4)=volic(i4)+((vol3b+vol5b+vol6b))/6.0
         if(idebug.ge.2) then
          write(6,555)'3vol ',voltet,voltet_vor1,voltet_vor2
          write(6,556)it,i1,i2,i3,i4,voltet_vor1,voltot_vor1
          write(6,555)'vol1 ',vol1a,vol1b,vol11,vol12,vol13,vol14
          write(6,555)'vol2 ',vol2a,vol2b,vol21,vol22,vol23,vol24
          write(6,555)'vol3 ',vol3a,vol3b,vol31,vol32,vol33,vol34
          write(6,555)'vol4 ',vol4a,vol4b,vol41,vol42,vol43,vol44
          write(6,555)'vol5 ',vol5a,vol5b,vol51,vol52,vol53,vol54
          write(6,555)'vol6 ',vol6a,vol6b,vol61,vol62,vol63,vol64
  555     format(a5,6e14.6)
  556     format(5i6,6e14.6)
         endif
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C  Divide area by distance if num_area_coef is gt zero
C
C  Otherwise calculate actual area vector.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
       if(num_area_coef .gt. 0)then
         ds12i=1.0d0/sqrt((xic(i2)-xic(i1))**2+
     *             (yic(i2)-yic(i1))**2+
     *             (zic(i2)-zic(i1))**2)
         ds13i=1.0d0/sqrt((xic(i3)-xic(i1))**2+
     *             (yic(i3)-yic(i1))**2+
     *             (zic(i3)-zic(i1))**2)
         ds14i=1.0d0/sqrt((xic(i4)-xic(i1))**2+
     *             (yic(i4)-yic(i1))**2+
     *             (zic(i4)-zic(i1))**2)
         ds23i=1.0d0/sqrt((xic(i3)-xic(i2))**2+
     *             (yic(i3)-yic(i2))**2+
     *             (zic(i3)-zic(i2))**2)
         ds24i=1.0d0/sqrt((xic(i4)-xic(i2))**2+
     *             (yic(i4)-yic(i2))**2+
     *             (zic(i4)-zic(i2))**2)
         ds34i=1.0d0/sqrt((xic(i4)-xic(i3))**2+
     *             (yic(i4)-yic(i3))**2+
     *             (zic(i4)-zic(i3))**2)
        else
          ds12i=1.0d0
          ds13i=1.0d0
          ds14i=1.0d0
          ds23i=1.0d0
          ds24i=1.0d0
          ds34i=1.0d0
        endif
 
       isign_meth = 2
       if(isign_meth .eq. 1)then
            xconst(it         )=sign(one, xdot1)*ax1*ds12i
            yconst(it         )=sign(one, xdot1)*ay1*ds12i
            zconst(it         )=sign(one, xdot1)*az1*ds12i
            xconst(it+   ntets)=sign(one,-xdot1)*ax1*ds12i
            yconst(it+   ntets)=sign(one,-xdot1)*ay1*ds12i
            zconst(it+   ntets)=sign(one,-xdot1)*az1*ds12i
 
            xconst(it+ 2*ntets)=sign(one, xdot2)*ax2*ds13i
            yconst(it+ 2*ntets)=sign(one, xdot2)*ay2*ds13i
            zconst(it+ 2*ntets)=sign(one, xdot2)*az2*ds13i
            xconst(it+ 3*ntets)=sign(one,-xdot2)*ax2*ds13i
            yconst(it+ 3*ntets)=sign(one,-xdot2)*ay2*ds13i
            zconst(it+ 3*ntets)=sign(one,-xdot2)*az2*ds13i
 
            xconst(it+ 4*ntets)=sign(one, xdot3)*ax3*ds14i
            yconst(it+ 4*ntets)=sign(one, xdot3)*ay3*ds14i
            zconst(it+ 4*ntets)=sign(one, xdot3)*az3*ds14i
            xconst(it+ 5*ntets)=sign(one,-xdot3)*ax3*ds14i
            yconst(it+ 5*ntets)=sign(one,-xdot3)*ay3*ds14i
            zconst(it+ 5*ntets)=sign(one,-xdot3)*az3*ds14i
 
            xconst(it+ 6*ntets)=sign(one, xdot4)*ax4*ds23i
            yconst(it+ 6*ntets)=sign(one, xdot4)*ay4*ds23i
            zconst(it+ 6*ntets)=sign(one, xdot4)*az4*ds23i
            xconst(it+ 7*ntets)=sign(one,-xdot4)*ax4*ds23i
            yconst(it+ 7*ntets)=sign(one,-xdot4)*ay4*ds23i
            zconst(it+ 7*ntets)=sign(one,-xdot4)*az4*ds23i
 
            xconst(it+ 8*ntets)=sign(one, xdot5)*ax5*ds24i
            yconst(it+ 8*ntets)=sign(one, xdot5)*ay5*ds24i
            zconst(it+ 8*ntets)=sign(one, xdot5)*az5*ds24i
            xconst(it+ 9*ntets)=sign(one,-xdot5)*ax5*ds24i
            yconst(it+ 9*ntets)=sign(one,-xdot5)*ay5*ds24i
            zconst(it+ 9*ntets)=sign(one,-xdot5)*az5*ds24i
 
            xconst(it+10*ntets)=sign(one, xdot6)*ax6*ds34i
            yconst(it+10*ntets)=sign(one, xdot6)*ay6*ds34i
            zconst(it+10*ntets)=sign(one, xdot6)*az6*ds34i
            xconst(it+11*ntets)=sign(one,-xdot6)*ax6*ds34i
            yconst(it+11*ntets)=sign(one,-xdot6)*ay6*ds34i
            zconst(it+11*ntets)=sign(one,-xdot6)*az6*ds34i
C
       elseif(isign_meth .eq. 2)then
            sfac1 = 1.0d0
            sfac2 = 1.0d0
            xconst(it         )= sfac1*ax1*ds12i
            yconst(it         )= sfac1*ay1*ds12i
            zconst(it         )= sfac1*az1*ds12i
            xconst(it+   ntets)=-sfac2*ax1*ds12i
            yconst(it+   ntets)=-sfac2*ay1*ds12i
            zconst(it+   ntets)=-sfac2*az1*ds12i
 
            xconst(it+ 2*ntets)= sfac1*ax2*ds13i
            yconst(it+ 2*ntets)= sfac1*ay2*ds13i
            zconst(it+ 2*ntets)= sfac1*az2*ds13i
            xconst(it+ 3*ntets)=-sfac2*ax2*ds13i
            yconst(it+ 3*ntets)=-sfac2*ay2*ds13i
            zconst(it+ 3*ntets)=-sfac2*az2*ds13i
 
            xconst(it+ 4*ntets)= sfac1*ax3*ds14i
            yconst(it+ 4*ntets)= sfac1*ay3*ds14i
            zconst(it+ 4*ntets)= sfac1*az3*ds14i
            xconst(it+ 5*ntets)=-sfac2*ax3*ds14i
            yconst(it+ 5*ntets)=-sfac2*ay3*ds14i
            zconst(it+ 5*ntets)=-sfac2*az3*ds14i
 
            xconst(it+ 6*ntets)= sfac1*ax4*ds23i
            yconst(it+ 6*ntets)= sfac1*ay4*ds23i
            zconst(it+ 6*ntets)= sfac1*az4*ds23i
            xconst(it+ 7*ntets)=-sfac2*ax4*ds23i
            yconst(it+ 7*ntets)=-sfac2*ay4*ds23i
            zconst(it+ 7*ntets)=-sfac2*az4*ds23i
 
            xconst(it+ 8*ntets)= sfac1*ax5*ds24i
            yconst(it+ 8*ntets)= sfac1*ay5*ds24i
            zconst(it+ 8*ntets)= sfac1*az5*ds24i
            xconst(it+ 9*ntets)=-sfac2*ax5*ds24i
            yconst(it+ 9*ntets)=-sfac2*ay5*ds24i
            zconst(it+ 9*ntets)=-sfac2*az5*ds24i
 
            xconst(it+10*ntets)= sfac1*ax6*ds34i
            yconst(it+10*ntets)= sfac1*ay6*ds34i
            zconst(it+10*ntets)= sfac1*az6*ds34i
            xconst(it+11*ntets)=-sfac2*ax6*ds34i
            yconst(it+11*ntets)=-sfac2*ay6*ds34i
            zconst(it+11*ntets)=-sfac2*az6*ds34i
       elseif(isign_meth .eq. 3)then
            xconst(it         )=sign(1,i2-i1)*ax1*ds12i
            yconst(it         )=sign(1,i2-i1)*ay1*ds12i
            zconst(it         )=sign(1,i2-i1)*az1*ds12i
            xconst(it+   ntets)=-sign(1,i2-i1)*ax1*ds12i
            yconst(it+   ntets)=-sign(1,i2-i1)*ay1*ds12i
            zconst(it+   ntets)=-sign(1,i2-i1)*az1*ds12i
 
            xconst(it+ 2*ntets)=sign(1,i3-i1)*ax2*ds13i
            yconst(it+ 2*ntets)=sign(1,i3-i1)*ay2*ds13i
            zconst(it+ 2*ntets)=sign(1,i3-i1)*az2*ds13i
            xconst(it+ 3*ntets)=-sign(1,i3-i1)*ax2*ds13i
            yconst(it+ 3*ntets)=-sign(1,i3-i1)*ay2*ds13i
            zconst(it+ 3*ntets)=-sign(1,i3-i1)*az2*ds13i
 
            xconst(it+ 4*ntets)=sign(1,i4-i1)*ax3*ds14i
            yconst(it+ 4*ntets)=sign(1,i4-i1)*ay3*ds14i
            zconst(it+ 4*ntets)=sign(1,i4-i1)*az3*ds14i
            xconst(it+ 5*ntets)=-sign(1,i4-i1)*ax3*ds14i
            yconst(it+ 5*ntets)=-sign(1,i4-i1)*ay3*ds14i
            zconst(it+ 5*ntets)=-sign(1,i4-i1)*az3*ds14i
 
            xconst(it+ 6*ntets)=sign(1,i3-i2)*ax4*ds23i
            yconst(it+ 6*ntets)=sign(1,i3-i2)*ay4*ds23i
            zconst(it+ 6*ntets)=sign(1,i3-i2)*az4*ds23i
            xconst(it+ 7*ntets)=-sign(1,i3-i2)*ax4*ds23i
            yconst(it+ 7*ntets)=-sign(1,i3-i2)*ay4*ds23i
            zconst(it+ 7*ntets)=-sign(1,i3-i2)*az4*ds23i
 
            xconst(it+ 8*ntets)=sign(1,i4-i2)*ax5*ds24i
            yconst(it+ 8*ntets)=sign(1,i4-i2)*ay5*ds24i
            zconst(it+ 8*ntets)=sign(1,i4-i2)*az5*ds24i
            xconst(it+ 9*ntets)=-sign(1,i4-i2)*ax5*ds24i
            yconst(it+ 9*ntets)=-sign(1,i4-i2)*ay5*ds24i
            zconst(it+ 9*ntets)=-sign(1,i4-i2)*az5*ds24i
 
            xconst(it+10*ntets)=sign(1,i4-i3)*ax6*ds34i
            yconst(it+10*ntets)=sign(1,i4-i3)*ay6*ds34i
            zconst(it+10*ntets)=sign(1,i4-i3)*az6*ds34i
            xconst(it+11*ntets)=-sign(1,i4-i3)*ax6*ds34i
            yconst(it+11*ntets)=-sign(1,i4-i3)*ay6*ds34i
            zconst(it+11*ntets)=-sign(1,i4-i3)*az6*ds34i
      endif
C
         if(idebug.ge.2) then
            write(6,*)'Tet number ', it, '  node number ',i1,i2,i3,i4
            do iedge = 0,11
            write(6,740)'edge area ',
     1                  iedge, lconn(1,iedge+1), lconn(2,iedge+1),
     1                  itet(lconn(1,iedge+1),it),
     1                  itet(lconn(2,iedge+1),it),
     1                     xconst(it+ iedge*ntets),
     2                     yconst(it+ iedge*ntets),
     3                     zconst(it+ iedge*ntets)
  740        format(a10,5i5,2x,3e14.5)
             enddo
        write(6,741)'xdot1 ',xdot11,xdot21,xdot31,xdot41,xdot51,xdot61
        write(6,741)'xdot2 ',xdot12,xdot22,xdot32,xdot42,xdot52,xdot62
        write(6,741)'xdot12',xdot1,xdot2,xdot3,xdot4,xdot5,xdot6
  741        format(a6,6e13.3)
         endif
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C  This is the end of the loop over all the tets
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      enddo
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      do i=1,npoints
         xconst(i+12*ntets)=0.0
         yconst(i+12*ntets)=0.0
         zconst(i+12*ntets)=0.0
      enddo
C
C   Reorder the const values into the sendnn arrays
C
      length=n12
 
      if (idebug.gt.1) then
      write(logmess,'(a,i14)')
     *   "Matbld3d_stor: mmgetblk xsendnn: ",length
      call writloga('default',0,logmess,0,ierrw)
      endif
 
      call mmgetblk("xsendnn",isubname,ipxsendnn,length,2,icscode)
      if (icscode .ne. 0) then
        write(logmess,'(a,i5)')
     *   "Matbld3d_stor: mmgetblk failed: xsendnn ",icscode
        call writloga('default',0,logmess,0,ierrw)
        istatus = icscode
        goto 9999
      endif

      do i=1,n12
         xsendnn(i) = xconst(isort(i))
      enddo

      if (idebug.gt.1) then
      write(logmess,'(a,i14)')
     *   "Matbld3d_stor: mmrelblk  xconst: ",length
      call writloga('default',0,logmess,0,ierrw)
      endif
 
      call mmrelblk("xconst",isubname,ipxconst,icscode)
      if (icscode .ne. 0) then
        write(logmess,'(a,i5)')
     *   "Matbld3d_stor: mmrelblk failed: xconst ",icscode
        call writloga('default',0,logmess,0,ierrw)
        istatus = icscode
      endif

      if (idebug.gt.1) then 
      write(logmess,'(a,i14)')
     *   "Matbld3d_stor: mmgetblk ysendnn: ",length
      call writloga('default',0,logmess,0,ierrw)
      endif
 
      call mmgetblk("ysendnn",isubname,ipysendnn,length,2,icscode)
      if (icscode .ne. 0) then
        write(logmess,'(a,i5)')
     *   "Matbld3d_stor: mmgetblk failed: ysendnn ",icscode
        call writloga('default',0,logmess,0,ierrw)
        istatus = icscode
        goto 9999
      endif

      do i=1,n12
         ysendnn(i) = yconst(isort(i))
      enddo

      if (idebug.gt.1) then
      write(logmess,'(a,i14)')
     *   "Matbld3d_stor: mmrelblk  yconst: ",length
      call writloga('default',0,logmess,0,ierrw)
      endif
 
      call mmrelblk("yconst",isubname,ipyconst,icscode)
      if (icscode .ne. 0) then
        write(logmess,'(a,i5)')
     *   "Matbld3d_stor: mmrelblk failed: yconst ",icscode
        call writloga('default',0,logmess,0,ierrw)
        istatus = icscode
      endif

      if (idebug.gt.1) then 
      write(logmess,'(a,i14)')
     *   "Matbld3d_stor: mmgetblk zsendnn: ",length
      call writloga('default',0,logmess,0,ierrw)
      endif
 
      call mmgetblk("zsendnn",isubname,ipzsendnn,length,2,icscode)
      if (icscode .ne. 0) then
        write(logmess,'(a,i5)')
     *   "Matbld3d_stor: mmgetblk failed: zsendnn ",icscode
        call writloga('default',0,logmess,0,ierrw)
        istatus = icscode
        goto 9999
      endif

      do i=1,n12
         zsendnn(i) = zconst(isort(i))
      enddo

      if (idebug.gt.1) then
      write(logmess,'(a,i14)')
     *   "Matbld3d_stor: mmrelblk  zconst: ",length
      call writloga('default',0,logmess,0,ierrw)
      endif
 
      call mmrelblk("zconst",isubname,ipzconst,icscode)
      if (icscode .ne. 0) then
        write(logmess,'(a,i5)')
     *   "Matbld3d_stor: mmrelblk failed: zconst ",icscode
        call writloga('default',0,logmess,0,ierrw)
        istatus = icscode
      endif


      if (idebug.gt.1) then
      write(logmess,'(a,i14)')
     *   "Matbld3d_stor: mmrelblk   isort: ",length
      call writloga('default',0,logmess,0,ierrw)
      endif
 
      call mmrelblk("isort",isubname,ipisort,icscode)
      if (icscode .ne. 0) then
        write(logmess,'(a,i5)')
     *   "Matbld3d_stor: mmrelblk failed: isort ",icscode
        call writloga('default',0,logmess,0,ierrw)
        istatus = icscode
      endif

C
      if(idebug .gt. 0)call mmverify( )
C
C    Above is the last time xconst, yconst, zconst, isort are used.
C
      do i=1,ncoefs
          amat(i)=0.0
         xamat(i)=0.0
         yamat(i)=0.0
         zamat(i)=0.0
      enddo
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   Assemble the 3*12 edge coefficients for each tet
C   into a single array with has the coefficients accumulated
C   by edge.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
       do i = 1, n12
          if(isendnn(i).gt.0) then
            xamat(isendnn(i)) = xamat(isendnn(i))+xsendnn(i)
            yamat(isendnn(i)) = yamat(isendnn(i))+ysendnn(i)
            zamat(isendnn(i)) = zamat(isendnn(i))+zsendnn(i)
          endif
       enddo
c      call xsumsp2r(ncoefs,n12,xamat,isendnn,xsendnn)
c      call xsumsp2r(ncoefs,n12,yamat,isendnn,ysendnn)
c      call xsumsp2r(ncoefs,n12,zamat,isendnn,zsendnn)
C
C    This is the last time isendnn is used.
C

      if (idebug.gt.1) then
      write(logmess,'(a,i14)')
     *   "Matbld3d_stor: mmrelblk isendnn: ",length
      call writloga('default',0,logmess,0,ierrw)
      endif
 
      call mmrelblk("isendnn",isubname,ipisendnn,icscode)
      if (icscode .ne. 0) then
        write(logmess,'(a,i5)')
     *   "Matbld3d_stor: mmrelblk failed: isendnn ",icscode
        call writloga('default',0,logmess,0,ierrw)
        istatus = icscode
      endif


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   Find the magnitude of the Aij coefficient for each edge.
C   Note: this quantity is always positive since it is the
C         sum of squares. The vector coefficients are checked
C         later for negative values.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      do i=1,ncoefs
         amat(i)=sqrt(xamat(i)**2+yamat(i)**2+zamat(i)**2)
      enddo
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   Compile some statistics on the coefficients.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      amatmin=1.0d+30
      amatmax=-amatmin
      do i=1,ncoefs
         amatmin=min(amatmin,amat(i))
         amatmax=max(amatmax,amat(i))
      enddo
      volmin=1.0e+30
      volmax=-volmin
      do i=1,npoints
         volmin=min(volmin,volic(i))
         volmax=max(volmax,volic(i))
      enddo
      amatmin=1.0e+30
      amatmax=-amatmin
      do i=1,ncoefs
         amatmin=min(amatmin,amat(i))
         amatmax=max(amatmax,amat(i))
      enddo
      num_conn_max = irowoff(1)
      do i=2, npoints
         num_conn_max = max(num_conn_max,irowoff(i)-irowoff(i-1))
      enddo
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   Output some statistics on the coefficients.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      ave_con_node = float(ncoefs)/float(npoints)
 
      write(logmess,'(a,i8,a,i10)')
     *   "Matbld3d_stor: npoints = ",npoints,
     *   "  n connections = ",ncoefs
      call writloga('default',0,logmess,0,ierrw)
 
      write(logmess,'(a,f15.7)')
     *"Matbld3d_stor: Ave. num connections (ncoefs/npoints) = ",
     * ave_con_node
      call writloga('default',0,logmess,0,ierrw)
 
      write(logmess,'(a,i10)')
     *"Matbld3d_stor: Maximum num. connections to a node = ",
     * num_conn_max
      call writloga('default',0,logmess,0,ierrw)
 
      write(logmess,'(a,1pe15.7)')
     *   "Matbld3d_stor: Volume min = ",volmin
      call writloga('default',0,logmess,0,ierrw)
 
      write(logmess,'(a,1pe15.7)')
     *   "Matbld3d_stor: Volume max = ",volmax
      call writloga('default',0,logmess,0,ierrw)
 
      if(num_area_coef .gt. 0)then
      write(logmess,'(a,1pe15.7)')
     *   "Matbld3d_stor: abs(Aij/xij) min = ",amatmin
      call writloga('default',0,logmess,0,ierrw)
 
      write(logmess,'(a,1pe15.7)')
     *   "Matbld3d_stor: abs(Aij/xij) max = ",amatmax
      call writloga('default',0,logmess,0,ierrw)
      elseif(num_area_coef .lt. 0)then
      write(logmess,'(a,1pe15.7)')
     *   "Matbld3d_stor: abs(Aij) min = ",amatmin
      call writloga('default',0,logmess,0,ierrw)
 
      write(logmess,'(a,1pe15.7)')
     *   "Matbld3d_stor: abs(Aij) max = ",amatmax
      call writloga('default',0,logmess,0,ierrw)
      endif
      write(logmess,'(a,3(1pe15.7))')
     *   "Matbld3d_stor: Total Volume: ",voltot/6.0d+00,
     *                    voltot_vor1/6.0d+00,
     *                    voltot_vor2/6.0d+00
      call writloga('default',0,logmess,0,ierrw)
 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   Assemble and output volume, pointer and area coefficients
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
 
 
      if(ijob .eq. 1)then
      ifilename=ifile(1:icharlnf(ifile)) // '.stor'
 
c     get next available unit number
c     use nextlun(), as nextlun1() can return a negative integer
 
      iunit = nextlun()
      if (iunit.lt.0) then
        call x3d_error("Matbld3d_stor ",'hassign bad file unit')
      endif
 
      if (io_type.eq.1) io_type=3
      if(io_type.eq.3)then
         open(unit=iunit, file = ifilename, form = 'unformatted')
      elseif(io_type .eq. 2)then
         open(unit=iunit, file = ifilename, form = 'formatted')
      endif
 
c
c set the imatptrs now because it could be used by the compression routine.
c
      neq=npoints
      neqp1=neq+1
      ncont=neqp1+ncoefs
 
      length=ncont

      write(logmess,'(a,i14)')
     *   "Matbld3d_stor: mmgetblk   itemp: ",length
      call writloga('default',0,logmess,0,ierrw)
 
      call mmgetblk('itemp',isubname,ipitemp,length,2,icscode)
      if (icscode .ne. 0) then
        write(logmess,'(a,i5)')
     *   "Matbld3d_stor: mmgetblk failed:  itemp ",icscode
        call writloga('default',0,logmess,0,ierrw)
        istatus = icscode
        goto 9999
      endif

      write(logmess,'(a,i14)')
     *   "Matbld3d_stor:mmgetblk imatptrs: ",length
      call writloga('default',0,logmess,0,ierrw)
 
      call mmgetblk('imatptrs',isubname,ipimatptrs,length,2,icscode)
      if (icscode .ne. 0) then
        write(logmess,'(a,i5)')
     *   "Matbld3d_stor: mmgetblk failed: imatptrs ",icscode
        call writloga('default',0,logmess,0,ierrw)
        istatus = icscode
        goto 9999
      endif

 
      do i=1,ncoefs
         imatptrs(i)=i
      enddo
 
c     perform compression
c
c The .stor file format allows us to compress redundant matrix entries (a.k.a
c values) with integer pointers.  That is, instead of saving a redundant
c matrix entry many times, we store the value of the entry once, and point
c to that value using the array imatptrs.  (See "The .stor file format" by
c M. Murphy at the top of this file for a hopefully complete list of the
c features and idiosyncrasies.)  Currently, the integer pointers to the
c double precision matrix entries, populated in imatptrs above,
c allow redundant entries.  The following compression routine, if called,
c will identify them.  If it is called, then num_written_coefs, the number of
c unique matrix entries, can differ from ncoefs, the number of matrix entries.
c Also, the arrays pointed to by cxamat, cyamat, czamat, and camat
c will be allocated and populated.  As these arrays are allocated by the
c compressValues routine, they need to be deallocated by calling
c freecompressedvaluesarrays() after they are no longer needed.
c
 
 
 
      num_written_coefs = ncoefs
      if(ifcompress.eq.1) then
c         compress_epsilon (Compression epsilon) corresponds roughly to the
c         number of significant digits you wish to maintain.
c         Right now, it is hard coded to eight significant
c         digits.
 
         compress_epsilon = 1.0e-08
 
 
         call compressmatrixvalues(ncoefs,neq,num_area_coef, ipimatptrs,
     x        ipxamat, ipyamat, ipzamat, ipamat,
     x        ipcxamat,ipcyamat,ipczamat,ipcamat,
     x        num_written_coefs, compress_epsilon)
 
 
      endif
 
c
C      Header is written to the file.
c      assumed header 1st line is 72 characters long:
 
C
C     Get a time stamp for the file header (second line).
C
      string = fdate()
 
c
c formatted ascii
c fehmstor ascir8i4 LaGriT Sparse Matrix, Voronoi Coefficients
c 123456789012345678901234567890123456789012345678901234567890123456789012
c fehmstor ieeer8i4 LaGriT Sparse Matrix, Voronoi Coefficients
c unformatted

c 123456789012345678901234567890123456789012345678901234567890123456789012
c       06/03 09:57:46 20093-D 3-D Linear Diffusion Model (Matbld3d_stor)
c      possible compression types are none, coefs, graph, all
c      this version matbld3d only does none or coefs 
c       matbld3d_stor with no compression
c       06/03 09:57:46 20093-D 3-D Linear Diffusion Model (matbld3d_nstor)
c       matbld3d_stor with area coefficient compression
c       06/03 09:57:46 20093-D 3-D Linear Diffusion Model (matbld3d_cstor)
 
c
c      The possible values for characters 14-17 are:
c        r4i4  r4i8  r8i4  r8i8
C
C     BINARY header
c
      if(io_type .eq. 3)then
         write(title_string,'(a)')
     1'fehmstor ieeer8i4 LaGriT Sparse Matrix Voronoi Coefficients'
         write(iunit)title_string

         if (ifcompress .ne. 0) then
         write(title_string,*)
     1   string,' 3-D Linear Diffusion Model (matbld3d_cstor)'
         else
         write(title_string,*)
     1   string,' 3-D Linear Diffusion Model (matbld3d_nstor)'
         endif
         write(iunit)title_string
 
C     ASCII header
      elseif(io_type .eq. 2)then

         if (ifcompress .ne. 0) then
         write(iunit,'(a)')
     1'fehmstor ascir8i4 LaGriT Sparse Matrix Voronoi Coefficients'
      write(iunit,*)string,'3-D Linear Diffusion Model (matbld3d_cstor)'
         else
         write(iunit,'(a)')
     1'fehmstor ascir8i4 LaGriT Sparse Matrix Voronoi Coefficients'
      write(iunit,*)string,'3-D Linear Diffusion Model (matbld3d_nstor)'
         endif

      endif
 
C
C   Set stuff for the third line of output.
C
 
      if (ifcompress.eq.1) then
         iwtotl=num_written_coefs
      else
         iwtotl=ncoefs
      endif
C
      neq=npoints
      neqp1=neq+1
      ncont=neqp1+ncoefs
 
c
c output the third line of output.
c
 
      if(io_type .eq. 3)then
         write(iunit)      iwtotl,neq,ncont, num_area_coef, num_conn_max
      elseif(io_type .eq. 2)then
         write(iunit,9010) iwtotl,neq,ncont, num_area_coef, num_conn_max
      endif
C
      if(io_type .eq. 3)then
         write(iunit)      (volic(i),i=1,neq)
      elseif(io_type .eq. 2)then
         write(iunit,9000) (volic(i),i=1,neq)
 9000    format(5(1pe20.12))
      endif
C
      do i=1,neq
         itemp(i)=irowoff(i)+neqp1
      enddo
      itemp(neqp1)=irowoff(neq)+irowcnt(neq)+neqp1
      do i=1,ncoefs
         itemp(neqp1+i)=icolmat(i)
      enddo
C
      if(io_type .eq. 3)then
         write(iunit)      (itemp(i),i=1,ncont)
      elseif(io_type .eq. 2)then
         write(iunit,9010) (itemp(i),i=1,ncont)
 9010    format(5i10)
      endif
c
c     set the pointers to that extra stuff George wants.
c
      do i=1,neqp1
         itemp(i)=0
      enddo
 
      if(io_type .eq. 3)then
         write(iunit)      (imatptrs(i),i=1,ncoefs),(itemp(i),i=1,neqp1)
         write(iunit)      (irowdag(i)+neqp1,i=1,neq)
      elseif(io_type .eq. 2)then
         write(iunit,9010) (imatptrs(i),i=1,ncoefs),(itemp(i),i=1,neqp1)
         write(iunit,9010) (irowdag(i)+neqp1,i=1,neq)
      endif
 
      if(ifcompress.eq.1) then
c        output the compressed arrays.
         if((abs(num_area_coef) .eq. 3) .or.
     1        (abs(num_area_coef) .eq. 4))then
            if(io_type .eq. 3)then
               write(iunit)      (-cxamat(i),i=1,num_written_coefs)
               write(iunit)      (-cyamat(i),i=1,num_written_coefs)
               write(iunit)      (-czamat(i),i=1,num_written_coefs)
            elseif(io_type .eq. 2)then
               write(iunit,9000) (-cxamat(i),i=1,num_written_coefs)
               write(iunit,9000) (-cyamat(i),i=1,num_written_coefs)
               write(iunit,9000) (-czamat(i),i=1,num_written_coefs)
            endif
         endif
 
         if((abs(num_area_coef) .eq. 1) .or.
     1        (abs(num_area_coef) .eq. 4))then
 
            if(io_type .eq. 3)then
               write(iunit)      (-camat(i),i=1,num_written_coefs)
            elseif(io_type .eq. 2)then
               write(iunit,9000) (-camat(i),i=1,num_written_coefs)
            endif
         endif
         call freecompressedvaluesarrays()
 
      else
c        no compression
         if((abs(num_area_coef) .eq. 3) .or.
     1        (abs(num_area_coef) .eq. 4))then
            if(io_type .eq. 3)then
               write(iunit)      (-xamat(i),i=1,num_written_coefs)
               write(iunit)      (-yamat(i),i=1,num_written_coefs)
               write(iunit)      (-zamat(i),i=1,num_written_coefs)
            elseif(io_type .eq. 2)then
               write(iunit,9000) (-xamat(i),i=1,num_written_coefs)
               write(iunit,9000) (-yamat(i),i=1,num_written_coefs)
               write(iunit,9000) (-zamat(i),i=1,num_written_coefs)
            endif
         endif
         if((abs(num_area_coef) .eq. 1) .or.
     1        (abs(num_area_coef) .eq. 4))then
            if(io_type .eq. 3)then
               write(iunit)      (-amat(i),i=1,num_written_coefs)
            elseif(io_type .eq. 2)then
               write(iunit,9000) (-amat(i),i=1,num_written_coefs)
            endif
         endif
      endif
 
 
      close(iunit)
      endif
 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     Check the sign of all the Aij coupling coefficients.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
 
      call mmfindbk(icr_ccoef,cmo,ipccoef,ilenout,ics)
      if(ics.ne.0) then
         dotask_command = 'cmo/addatt/' //
     >                     cmo(1:icharlnf(cmo)) //
     >                     '/' //
     >     icr_ccoef(1:icharlnf(icr_ccoef)) //
     >        '/VDOUBLE/scalar/nnodes/linear/permanent/afgx/0.0/' //
     >        ' ; finish '
         call dotaskx3d(dotask_command,ierror)
         if (ierror.ne.0)
     >      call x3d_error(isubname,icr_ccoef(1:icharlnf(icr_ccoef)))
         call mmfindbk(icr_ccoef,cmo,ipccoef,ilenout,ics)
 
      else
c        CWG
c        The newlen call seems odd but I'll leave it in and add a warning
c
c        if (ilenout.lt.npoints) call cmo_newlen(cmo,ics)
        if (ilenout.lt.npoints) then
           write(logmess,'(a,2i10)')
     *         "Matbld3d_stor: Odd condition (ilenout.lt.npoints)",
     *          ilenout, npoints
           call writloga('default',0,logmess,0,ics)
           call cmo_newlen(cmo,ierror)
        endif
      endif
 2    iatt_type=2
 
      do i = 1,npoints
        ccoef(i) = 0.0
      enddo
 
      do i=1,neq
         itemp(i)=irowoff(i)+neqp1
      enddo
      itemp(neqp1)=irowoff(neq)+irowcnt(neq)+neqp1
      do i=1,ncoefs
         itemp(neqp1+i)=icolmat(i)
      enddo
C
      area_scale = amatmax
      icount_neg = 0
      icount = 0
      amatmin=1.0d+30
      amatmax=-amatmin
      do i = 1, neq
         numj = itemp(i+1)-itemp(i)
         do jnum = 1, numj
            icount = icount + 1
            j = itemp(itemp(i)+jnum)
            amatsave = amat(icount)
            amat(icount) = 0.0
c
c   Only look at the cases where i .ne. j
c
         if(i .ne. j)then
            xij = xic(j) - xic(i)
            yij = yic(j) - yic(i)
            zij = zic(j) - zic(i)
            xijmag = sqrt(xij**2 + yij**2 + zij**2)
            amag   = sqrt(xamat(icount)**2 +
     1                    yamat(icount)**2 +
     2                    zamat(icount)**2)
            xij_dot_aij =
     1         xij*xamat(icount)+
     2         yij*yamat(icount)+
     3         zij*zamat(icount)
            amat(icount) = xij_dot_aij/xijmag
            ccoef(i) = min(amat(icount),ccoef(i))
            ccoef(j) = min(amat(icount),ccoef(j))
            amatmin=min(amatmin,amat(icount))
            amatmax=max(amatmax,amat(icount))
 
          if(amat(icount) .lt. 0.0)then
          if(idebug.ne.0) then
             write(6,766)icount,i,j,xij_dot_aij/xijmag,xij_dot_aij,
     *                  amatsave
          endif
  766     format(3i6,4e14.6)
  767     format(a12,3e14.6)
          endif
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     Store the location and value of negative coefs.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
            if(amat(icount) .lt. 0.0)then
               icount_neg = icount_neg + 1
               iamat(icount_neg) = i
               zamat(icount_neg) = j
               xamat(icount_neg) = amat(icount)
               yamat(icount_neg) = icount_neg
                amat(icount_neg) = icount
            endif
          endif
         enddo
      enddo
 
      if(icount_neg.gt.0) then
c
c     Allocate memory to store value and index of negative edge coefs.
c
      call mmgetblk
     1    ("coef_neg",isubname,ipcoef_neg,icount_neg,2,icscode)
      if (icscode .ne. 0) then
        write(logmess,'(a,i5)')
     *   "Matbld3d_stor: mmgetblk failed: coef_neg ",icscode
        call writloga('default',0,logmess,0,ierrw)
        istatus = icscode
        goto 9999
      endif

      call mmgetblk
     1    ("index_neg",isubname,ipindex_neg,2*icount_neg,1,icscode)
      if (icscode .ne. 0) then
        write(logmess,'(a,i5)')
     *   "Matbld3d_stor: mmgetblk failed: index_neg ",icscode
        call writloga('default',0,logmess,0,ierrw)
        istatus = icscode
        goto 9999
      endif
 
      write(logmess,'(a,1pe15.7)')
     *   "Matbld3d_stor:     Aij/xij min = ",amatmin
      call writloga('default',0,logmess,0,ierrw)
 
      write(logmess,'(a,1pe15.7)')
     *   "Matbld3d_stor:     Aij/xij max = ",amatmax
      call writloga('default',0,logmess,0,ierrw)
 
c I uncommented this section and dump never finished.
c under opt version - tam 5/99
c       call ssort(xamat,yamat,icount_neg,2)
c         jcount=0
c         do i=1,icount_neg
c            j=iamat(int(yamat(i)))
c            if(-amat(j).lt.-1.0e-06*abs(amatmax)) then
c               jcount=jcount+1
c            endif
c         enddo
c         if(jcount.gt.0) then
c            ierr1=1
c         else
c            ierr1=0
c         endif
 
        write(logmess,'(a,i9,a,1pe15.7,a,1pe15.7)')
     *    "Matbld3d_stor: total neg",icount_neg,
     *    " mincoef= ",amatmin," maxcoef= ",amatmax
        call writloga('default',0,logmess,0,ierrw)
 
        write(logmess,'(a)')
     *   'NegC: sort              edge            vor_coeff
     *      row_node     col_node'
 
        call writloga('default',0,logmess,0,ierrw)
C
c since ssort does not currently work under opt version, and we want
c avoid printing all neg coeffs, only print those that are valid neg coef
c only write neg coeffs that are smaller than -espsilon
c amatmax is the largest coeff = area_scale
         jcount = 0
         if(idebug.eq.0) then
C            do i=1,min(icount_neg,30)
             do i = 1, icount_neg
               j=iamat(int(yamat(i)))
               ii = iamat(i)
               jj = nint(zamat(i))
               kk = nint(yamat(i))
               ll = nint( amat(i))
               value = xamat(i)
 
             if (value .lt. -1.0e-08*abs(area_scale)) then
               write(logmess,'(a,4i10,1pe15.7,2i10)')
     *            "NegC:",ii,jj,kk,ll,value,
     *               irowmat(j),icolmat(j)
               call writloga('default',0,logmess,0,ierrw)
               jcount = jcount + 1
             endif
            enddo
            write(logmess,'(a,i10)') "Total suspect coeffs: ",jcount
            call writloga('default',1,logmess,1,ierrw)
 
 
         else
            do i=1,icount_neg
               j=iamat(int(yamat(i)))
               ii = iamat(i)
               jj = nint(zamat(i))
               kk = nint(yamat(i))
               ll = nint( amat(i))
               value = xamat(i)
               coef_neg(i) = value
               index_neg(2*i-1) = ii
               index_neg(2*i)   = jj
               if(value .lt. -abs(area_scale)*scale_factor)then
                   write(logmess,'(a,4i10,1pe15.7,2i10)')
     *                "NegC:",ii,jj,kk,ll,value,
     *                   irowmat(j),icolmat(j)
                   call writloga('default',0,logmess,0,ierrw)
               endif
            enddo
         endif
      else
 
         ierr1=0
         write(logmess,'(a,i10)')
     *   "Matbld3d_stor: **** Zero Negative Coefficients *****"
         call writloga('default',0,logmess,0,ierrw)
      endif
c
      if(if_vor_io .ne. 0)then
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
c
c       output 24 voronoi tets
c
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      ifilename=ifile(1:icharlnf(ifile)) // 'vor_tet.inp'
      iunit1=-1
      call hassign(iunit1,ifilename,ierror)
      if (iunit1.lt.0 .or. ierror.lt.0) then
       call x3d_error("Matbld3d_stor",'1 hassign bad file unit')
      endif

      ifilename=ifile(1:icharlnf(ifile)) // 'vor_tri.inp'
      iunit2=-1
      call hassign(iunit2,ifilename,ierror)
      if (iunit2.lt.0 .or. ierror.lt.0) then
       call x3d_error("Matbld3d_stor",'2 hassign bad file unit')
      endif

      if(if_vor_io .eq. 2)then
        ifilename=ifile(1:icharlnf(ifile)) // 'vor_minmax.inp'
        iunit3=-1
        call hassign(iunit3,ifilename,ierror)
        if (iunit3.lt.0 .or. ierror.lt.0) then
       call x3d_error("Matbld3d_stor",'3 hassign bad file unit')
        endif

        write(iunit3,326)
  326   format('       node   ',
     1         'x_vor_min           x_vor_max           ',
     2         'y_vor_min           y_vor_max           ',
     3         'z_vor_min           z_vor_max')
        do i = 1, npoints
        write(iunit3,327)i, 
     1                   vor_xyz_min_max(1,1,i),
     2                   vor_xyz_min_max(1,2,i),
     3                   vor_xyz_min_max(2,1,i),
     4                   vor_xyz_min_max(2,2,i),
     5                   vor_xyz_min_max(3,1,i),
     6                   vor_xyz_min_max(3,2,i)
        enddo
  327  format(i9,6e20.12)
       close(iunit3)
      endif
        write(iunit1,*)15*ntets, 24*ntets, 4, 0, 0
        write(iunit2,*)15*ntets, 24*ntets, 4, 0, 0
        do it = 1,ntets
        do i = 1,15
           write(iunit1,337)i+15*(it-1),vor_pts(1,i,it),
     1                    vor_pts(2,i,it),
     2                    vor_pts(3,i,it)
  337      format(i10.10,1x,3(1pe20.12))
        enddo
        enddo
        do it = 1,ntets
        do i = 1,24
           write(iunit1,*) i, i, '  tet ',
     1       itet_vor(2,i)+15*(it-1),
     2       itet_vor(1,i)+15*(it-1),
     3       itet_vor(3,i)+15*(it-1),
     3       itet_vor(4,i)+15*(it-1)
        enddo
        enddo
        write(iunit1,*)4,1,1,1,1
        write(iunit1,338)'xcord,'
        write(iunit1,338)'ycord,'
        write(iunit1,338)'zcord,'
        write(iunit1,338)'index,'
  338   format(a6)
        do it = 1,ntets
        do i = 1,15
           write(iunit1,337)i+15*(it-1),vor_pts(1,i,it),
     1                                  vor_pts(2,i,it),
     2                                  vor_pts(3,i,it),
     3                                  float(i)
        enddo
        enddo
 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
c
c       output 24 voronoi triangle faces
c
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        do it = 1,ntets
        do i = 1,15
           write(iunit2,337)i+15*(it-1),vor_pts(1,i,it),
     1                    vor_pts(2,i,it),
     2                    vor_pts(3,i,it)
        enddo
        enddo
        do it = 1,ntets
        do i = 1,24
c           write(iunit2,*) i+15*(it-1),
c     1                     isign_face(i+15*(it-1)), '  tri ',
           write(iunit2,*) i+15*(it-1), it, '  tri ',
     1       itet_vor(1,i)+15*(it-1),
     2       itet_vor(2,i)+15*(it-1),
     3       itet_vor(3,i)+15*(it-1)
        enddo
        enddo
        write(iunit2,*)4,1,1,1,1
        write(iunit2,338)'xcord,'
        write(iunit2,338)'ycord,'
        write(iunit2,338)'zcord,'
        write(iunit2,338)'index,'
        do it = 1,ntets
        do i = 1,15
           write(iunit2,337)i+15*(it-1),vor_pts(1,i,it),
     1                                  vor_pts(2,i,it),
     2                                  vor_pts(3,i,it),
     3                                  float(i)
        enddo
        enddo
 
 
        close(iunit1)
        close(iunit2)
      endif
 
      goto 9999

 9999 continue

C convert options to readable strings and report
C
C io_type = 2 Output ascii coefficient (stor) file.
C         = 1 = 3 Output unformatted coefficient (stor) file.
C
C num_area_coef = 1 Output single component scalar area/distance coefficients.
C         = 3 Output x,y,z  vector area/distance coefficients.
C         = 4 Output scalar and vector area/distance coefficients.
C         =-1 Output single component scalar area coefficients.
C         =-3 Output x,y,z  vector area coefficients.
C         =-4 Output scalar and vector area coefficients.
c
c  ifcompress   = 0 If no compression of the .stor file is desired. (none = nstor)
C               = 1 If coef compression is desired. (coefs = _cstor)
C     (note anothermatbld3d_stor writes _astor and _gstor (all and graph))

      io_string = 'not set                        '
      coef_string = 'not set                        '
      comp_string = 'not set                        '

      if (io_type.eq.1) io_string='unformatted'
      if (io_type.eq.2) io_string='ascii'
      if (io_type.eq.3) io_string='unformatted'
      if (io_type.eq.5) io_string='attribute'
      if (num_area_coef.eq.1) coef_string = 'scalar area/distance'
      if (num_area_coef.eq.3) coef_string = 'vector area/distance'
      if (num_area_coef.eq.4)
     *    coef_string = 'scalar and vector area/distance'
      if (num_area_coef.eq. -1) coef_string = 'scalar area'
      if (num_area_coef.eq. -3) coef_string = 'vector area'
      if (num_area_coef.eq. -4) coef_string = 'scalar and vector area'
      if (ifcompress.eq. 1) comp_string='for coefficient values'
      if (ifcompress.eq. 0) comp_string='for none'

C Report any errors or success
C todo - figure out and add ijob option to report 
C
      if (istatus .ne. 0) then
        call mmprint()
        write(logmess,'(a,i5)')
     *  "Matbld3d_stor: ERROR could not finish: ",istatus
        call writloga('default',1,logmess,0,ierrw)

        write(logmess,'(a,a32)')
     *  "Attempted sparse matrix with ",
     *  comp_string
        call writloga('default',0,logmess,0,ierrw)
        write(logmess,'(a,a32)')
     *  "with coefficients written as ",
     *  coef_string
        call writloga('default',0,logmess,0,ierrw)

        if (ifcompress.eq.0) then
        write(logmess,'(a)')
     *  "  *** _nstor SPARSE COEFFICIENT MATRIX ERROR ***"
        call writloga('default',1,logmess,0,ierrw)
        else
        write(logmess,'(a)')
     *  "  *** _cstor SPARSE COEFFICIENT MATRIX ERROR ***"
        call writloga('default',1,logmess,0,ierrw)
        endif
        write(logmess,'(a)')
     *  "*** INCOMPLETE or NO STOR FILE WRITTEN!! ***"
        call writloga('default',0,logmess,1,ierrw)

       else


        write(logmess,'(a,a32)')
     *  "Compression used for ",
     *  comp_string
        call writloga('default',0,logmess,0,ierrw)

        write(logmess,'(a,a32)')
     *  "The area coefficient values were written as ",
     *  coef_string
        call writloga('default',0,logmess,0,ierrw)

        if (io_type .eq. 5) then
        write(logmess,'(a,a,a)')
     *  cmo(1:icharlnf(cmo)),
     *  " attribute with voronoi volumes created with name ",
     *  ifile(1:icharlnf(ifile))
        call writloga('default',0,logmess,0,ierrw)
        else
        write(logmess,'(a,a,a)')
     *  io_string(1:icharlnf(io_string)),
     *  " STOR file written with name ",
     *  ifilename(1:icharlnf(ifilename))
        call writloga('default',0,logmess,0,ierrw)
        endif

        if (ifcompress .eq. 0) then
        write(logmess,'(a)')
     *  "*** SPARSE COEFFICIENT MATRIX _nstor SUCCESSFUL ***"
        call writloga('default',1,logmess,1,ierrw)
        else

        write(logmess,'(a)')
     *  "*** SPARSE COEFFICIENT MATRIX _cstor SUCCESSFUL ***"
        call writloga('default',1,logmess,1,ierrw)

c       warning re old code no longer supported
c       compress option of this code may produce errors if called twice
        write(logmess,'(a)')
     *  "          ** MATBLD3D WARNING: ** "
        call writloga('default',1,logmess,0,ierrw)
        write(logmess,'(a)')
     *  "  Repeated calls with compressed coefs (_cstor) " 
        call writloga('default',0,logmess,0,ierrw)
        write(logmess,'(a)')
     *  "  may lead to errors in subsequent stor files."
        call writloga('default',0,logmess,1,ierrw)

        endif


      endif
C     end report


      if(ijob .eq. 1) then
        call mmrelprt(isubname,icscode)
      else
        write(logmess,'(a,a)')
     *  "MATBLD3D WARNING: Partition memory not released: ",isubname
        call writloga('default',1,logmess,1,ierrw)
      endif
c   

      return
      end
 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine vor_pts_minmax(vor_pts,i1,i2,i3,i4, xyz_min_max, nnode)
      real*8 vor_pts(3,15,1)
      real*8 xyz_min_max(3,2,nnode)
      integer nnode
      
      integer i, j, i1, i2, i3, i4
      
      do j = 1, 3
C
C     Test all points associated with node i1
C      
      i = i1
      xyz_min_max(j,1,i) = min(xyz_min_max(j,1,i),vor_pts(j,1,1))
      xyz_min_max(j,2,i) = max(xyz_min_max(j,2,i),vor_pts(j,1,1))

      xyz_min_max(j,1,i) = min(xyz_min_max(j,1,i),vor_pts(j,5,1))
      xyz_min_max(j,2,i) = max(xyz_min_max(j,2,i),vor_pts(j,5,1))
      
      xyz_min_max(j,1,i) = min(xyz_min_max(j,1,i),vor_pts(j,6,1))
      xyz_min_max(j,2,i) = max(xyz_min_max(j,2,i),vor_pts(j,6,1))

      xyz_min_max(j,1,i) = min(xyz_min_max(j,1,i),vor_pts(j,7,1))
      xyz_min_max(j,2,i) = max(xyz_min_max(j,2,i),vor_pts(j,7,1))

      xyz_min_max(j,1,i) = min(xyz_min_max(j,1,i),vor_pts(j,11,1))
      xyz_min_max(j,2,i) = max(xyz_min_max(j,2,i),vor_pts(j,11,1))

      xyz_min_max(j,1,i) = min(xyz_min_max(j,1,i),vor_pts(j,15,1))
      xyz_min_max(j,2,i) = max(xyz_min_max(j,2,i),vor_pts(j,15,1))
      
C
C     Test all points associated with node i2
C      
      i = i2
      xyz_min_max(j,1,i) = min(xyz_min_max(j,1,i),vor_pts(j,2,1))
      xyz_min_max(j,2,i) = max(xyz_min_max(j,2,i),vor_pts(j,2,1))

      xyz_min_max(j,1,i) = min(xyz_min_max(j,1,i),vor_pts(j,5,1))
      xyz_min_max(j,2,i) = max(xyz_min_max(j,2,i),vor_pts(j,5,1))
      
      xyz_min_max(j,1,i) = min(xyz_min_max(j,1,i),vor_pts(j,8,1))
      xyz_min_max(j,2,i) = max(xyz_min_max(j,2,i),vor_pts(j,8,1))

      xyz_min_max(j,1,i) = min(xyz_min_max(j,1,i),vor_pts(j,9,1))
      xyz_min_max(j,2,i) = max(xyz_min_max(j,2,i),vor_pts(j,9,1))

      xyz_min_max(j,1,i) = min(xyz_min_max(j,1,i),vor_pts(j,12,1))
      xyz_min_max(j,2,i) = max(xyz_min_max(j,2,i),vor_pts(j,12,1))

      xyz_min_max(j,1,i) = min(xyz_min_max(j,1,i),vor_pts(j,15,1))
      xyz_min_max(j,2,i) = max(xyz_min_max(j,2,i),vor_pts(j,15,1))
      
C
C     Test all points associated with node i3
C      
      i = i3
      xyz_min_max(j,1,i) = min(xyz_min_max(j,1,i),vor_pts(j,3,1))
      xyz_min_max(j,2,i) = max(xyz_min_max(j,2,i),vor_pts(j,3,1))

      xyz_min_max(j,1,i) = min(xyz_min_max(j,1,i),vor_pts(j,6,1))
      xyz_min_max(j,2,i) = max(xyz_min_max(j,2,i),vor_pts(j,6,1))
      
      xyz_min_max(j,1,i) = min(xyz_min_max(j,1,i),vor_pts(j,8,1))
      xyz_min_max(j,2,i) = max(xyz_min_max(j,2,i),vor_pts(j,8,1))

      xyz_min_max(j,1,i) = min(xyz_min_max(j,1,i),vor_pts(j,10,1))
      xyz_min_max(j,2,i) = max(xyz_min_max(j,2,i),vor_pts(j,10,1))

      xyz_min_max(j,1,i) = min(xyz_min_max(j,1,i),vor_pts(j,13,1))
      xyz_min_max(j,2,i) = max(xyz_min_max(j,2,i),vor_pts(j,13,1))

      xyz_min_max(j,1,i) = min(xyz_min_max(j,1,i),vor_pts(j,15,1))
      xyz_min_max(j,2,i) = max(xyz_min_max(j,2,i),vor_pts(j,15,1))
      
C
C     Test all points associated with node i4
C      
      i = i4
      xyz_min_max(j,1,i) = min(xyz_min_max(j,1,i),vor_pts(j,4,1))
      xyz_min_max(j,2,i) = max(xyz_min_max(j,2,i),vor_pts(j,4,1))

      xyz_min_max(j,1,i) = min(xyz_min_max(j,1,i),vor_pts(j,7,1))
      xyz_min_max(j,2,i) = max(xyz_min_max(j,2,i),vor_pts(j,7,1))
      
      xyz_min_max(j,1,i) = min(xyz_min_max(j,1,i),vor_pts(j,9,1))
      xyz_min_max(j,2,i) = max(xyz_min_max(j,2,i),vor_pts(j,9,1))

      xyz_min_max(j,1,i) = min(xyz_min_max(j,1,i),vor_pts(j,10,1))
      xyz_min_max(j,2,i) = max(xyz_min_max(j,2,i),vor_pts(j,10,1))

      xyz_min_max(j,1,i) = min(xyz_min_max(j,1,i),vor_pts(j,14,1))
      xyz_min_max(j,2,i) = max(xyz_min_max(j,2,i),vor_pts(j,14,1))

      xyz_min_max(j,1,i) = min(xyz_min_max(j,1,i),vor_pts(j,15,1))
      xyz_min_max(j,2,i) = max(xyz_min_max(j,2,i),vor_pts(j,15,1))
      
      enddo
      
      return
      end
      
      
