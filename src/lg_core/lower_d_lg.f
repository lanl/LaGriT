Cdk,lower_d_lg
C
c23456789012345678901234567890123456789012345678901234567890123456789012
C #####################################################################
C
C   PURPOSE -
C
C      This suite of subroutines handles the "lower_d" commands which
C      deal with creating and using the lower_d structures within the
C      mesh object.
C
C   LOWER D DATA STRUCTURES -
C
C      For each dimension from the dimension of the input "top" mesh (d0)
C      down to points, the data strucutures which parallel the basic lagrit data
C      structures of the "bare" mesh are created.
C      If ndimensions_topo=3, then
C      the 2D surfaces separating regions are called the "d1" structures,
C      the 1D lines separating surfaces are called the "d2" structures, and
C      the 0D points where lines meet are called the "d3" structures.
C      For ndimensions_topo=2, then
C      the 1D lines separating regions are called the "d1" structures,
C      the 0D points where lines meet are called the "d2" structures.
C      For ndimensions_topo=1, then
C      the 0D points where regions meet are called the "d1" structures.
C      The new attributes added live in the same mesh object as the parent mesh,
C      although they can be extracted using the "lower_d/extract" command.
C
C      new attributes in the top dimension "d0":
C         d0 nnodes attributes:
C            d0_node_topo - 0 = in the interior of a region of the mesh
C                           1 = an interior point of the surfaces/
C                               a point at which regions meet
C                           2 = an interior point of the edges/
C                               a point at which surfaces intersect
C                           3 = a point at which edges intersect
C
C      new attributes in the 1-lower dimension "d1" (if not points):
C         d1 scalar attributes:
C            d1_nnodes    - the number of nodes (after filtering) in this topology class.
C            d1_nelements - the number of elements in this topology class.
C            d1_nef_cmo   - the number of faces per element in this topology class.
C            d1_nee_cmo   - the number of edges per element in this topology class.
C            d1_nen_cmo   - the number of nodes per element in this topology class.
C            d1_jtet_cycle_max - the longest jtet cycle in this topology class.
C         d1 nnodes attributes: none
C         d1 nelements attributes:
C            d1_itettyp - lagrit element type
C            d1_itetclr - packed color (see clor table note below)
C            d1_itetoff - offset for the d1_itet
C            d1_itet    - list of nodes for each d1 element
C            d1_jtetoff - offset for the d1_jtet
C            d1_jtet    - face neighbor table for each d1 element
C            d1_elm_d0  - packed d0 element and face from which this d1 element came.
C
C      new attributes in the 2-lower dimension "d2" (if not points and d1 not points):
C         d2 scalar attributes:
C            d2_nnodes   - the number of nodes (after filtering) in this topology class.
C            d2_nelements - the number of elements in this topology class.
C            d2_nef_cmo   - the number of faces per element in this topology class.
C            d2_nee_cmo   - the number of edges per element in this topology class.
C            d2_nen_cmo   - the number of nodes per element in this topology class.
C            d2_jtet_cycle_max - the longest jtet cycle in this topology class.
C         d2 nnodes attributes: none
C         d2 nelements attributes:
C            d2_itettyp - lagrit element type
C            d2_itetclr - packed color (see clor table note below)
C            d2_itetoff - offset for the d2_itet
C            d2_itet    - list of nodes for each d2 element
C            d2_jtetoff - offset for the d2_jtet
C            d2_jtet    - face neighbor table for each d2 element
C            d2_elm_d1  - packed d1 element and face from which this d2 element came.
C               ( create_d2_elm_d0_lower_d_lg will create the packed d0 element and edge
C                 from which this d2 element came if desired )
C
C      new attributes for the lower d which is points
C          (if ndimensions_topo=3, the "d3" structure;
C                               2      "d2"
C                               1      "d1"
C           "dp" is used here to represent it, which has to be replaced
C           with the appropriate "d#" for the current ndimensions_topo)
C         dp scalar attributes:
C            dp_nnodes   - the number of nodes (after filtering) in this topology class.
C         dp nnodes attributes: none
C         dp nelements attributes: none
C
C      other new attributes:
C         flags:
C            lower_d_flag = 0 - no lower d structures
C                         = 1 - lower d structures are valid
C                         = 2 - lower d structures need updating
C            interior_icr_flag = 0 - all constrained surface types can exist
C                              = 1 - no virtual constrained surfaces exist
C                              = 2 - no intrcons constrained surfaces exist
C                              = 3 - no virtual or intrcons constrained surfaces exist
C                              = 4 - no reflect constrained surfaces exist
C                              = 5 - no reflect or virtual constrained surfaces exist
C                              = 6 - no reflect or intrcons constrained surfaces exist
C                              = 7 - no constrained surfaces exist
C                  note: interior_icr_flag must be created by the user
C                        and is only used by the subroutines sizes_lower_d_lg
C                        and itetclr_lower_d_lg (passed from create_lower_d_lg).
C                        It is assumed 0 if not defined. Hence if it is defined
C                        after the lower_d structures are created, the color table
C                        and/or itetclr values may not be correct.
C         color table attributes:
C            itetclr is a packed representation of the ipt1,icr1,imt1's
C            for the given lower d element, and d0_clrtab is used to unpack it.
C               d0_nclrs   - the number of colors
C               d0_clrlen  - the length of the d0_clrtab storage
C               d0_clroff  - offset to the entry in the color table
C               d0_clrtab  - the color table
C            for iclr=d1_itetclr(iel) [ or d2_itetclr(iel) ]
C               d0_clrtab(d0_clroff(iclr)+1) = the itp1 value
C                    associated with lower d elements of this iclr
C               d0_clrtab(d0_clroff(iclr)+2) = the icr1 value
C                    associated with lower d elements of this color
C               d0_clrtab(d0_clroff(iclr)+3) = the number of imt1
C                    associated with lower d elements of this iclr
C               d0_clrtab(d0_clroff(iclr)+3) = the number of imt1
C                    associated with lower d elements of this iclr
C            No entries in the color table are generated for the
C            top dimension or the point dimension, and the d1_itetclr
C            entries start with d1_itetclr=1, which does not have
C            the same meaning as itetclr=1 for d0 elements:
C            a d1_itetclr value corresponding
C                d0_clrtab(d0_clroff(iclr)+1)=0
C                d0_clrtab(d0_clroff(iclr)+2)=0
C                d0_clrtab(d0_clroff(iclr)+3)=0
C                d0_clrtab(d0_clroff(iclr)+4)=imt1
C            would be need to be added if the d0 element colors were
C            to be included in the color table. Since there are
C            no elements associated with the point mesh, I decided
C            not to pack their colors (essentially ipt1,icr1,imt1(isn1)
C            on that point's parent node).
C
C   COMMAND FORMAT -
C     (* marks defaults; cmo0 is the current "bare" or "top" mesh object name)
C
C      lower_d/*create[/cmo0|*-def-][/recreate|refilter|*new]
C            creates lower d structures in cmo0
C            (to modify how selected, use filter after create; see filter)
C            "recreate" indicates that exisiting color table if any should be used
C            rather than being re-created
C
C      lower_d/release[/cmo0|*-def-]
C            releases the lower d structures in cmo0
C
C      lower_d/extract[/cmo0|*-def-][/cmo1|*-none-,cmo2|*-none-,cmo3|*-none-]
C                 /[*itetclr|no_color|recolor]
C            if -none- for given dimension (default), lower d cmo not created
c               otherwise overwrites cmo1-3 if already exist
C            should add option to create lower d structures in cmo0 if not
C               already extant: for now, just return error
C            cmo1,cmo2,cmo3 are the names of the mesh objects to extract
C              the corresponding d1,d2,d3 structures to.
C            Unlike other extracts, node attibutes are copied to the extracted mesh.
C
C       lower_d/print[/cmo0|*-def-][/*clrtab]
C            print the color table
C
C       lower_d/filter[/cmo0|*-def-]/[icr|itp|imt|clr]/#[/and|*or|new]
C            increment the filter to select (set itetclr>0)
C            lower d elements with the specified icr or itp or imt or clr #
C       lower_d/filter[/cmo0|*-def-]/[no_icr|no_itp|no_imt|no_clr]/#[/and|*or|new]
C            increment the filter to unselect (set itetclr<0)
C            lower d elements with the specified icr or itp or imt or clr #
C       lower_d/filter[/cmo0|*-def-]/[ext|int|vrt|real][/-not_used-/and|*or|new]
C            increment the filter to select (set itetclr>0)
C            lower d elements with the specified itp type class
C       lower_d/filter[/cmo0|*-def-]/[no_ext|no_int|no_vrt|no_real][/-not_used-/and|*or|new]
C            increment the filter to unselect (set itetclr<0)
C            lower d elements with the specified itp type class
C       lower_d/filter[/cmo0|*-def-]/reset
C            reset to no filter
C       lower_d/filter[/cmo0|*-def-]/refilter[/#]
C            re-filter the lower_d elements using the stored filter commands
C            for this mesh object, if # is specified, the stored filters
C            will be (permanently) truncated to the last # filters
C
C   NON-COMMAND SUBROUTINES -
C     These may be of use when writing code which uses the lower d structures.
C     Hopefully the intent is obvious from the name -
C     see the notes above the subroutine for a detailed explanation.
C
C       reset_mbndry_lower_d_lg
C       create_d2_elm_d0_lower_d_lg
C       create_d0d1_node_lower_d_lg
C       create_d0_elm_d1_lower_d_lg
C       create_d0_elm_d2_lower_d_lg
C       order_surface_lower_d_lg
C
C   USAGE NOTES / CAVEATS -
C
C   WARNING: "alpha" version: Once the way this suite is used becomes established,
C   some data storage/usage conventions may need to be changed....
C
C    - The initial assumption is that having the lower d structures parallel
C      the top-dimension structures is the best way to handle it.
C      The main changes are that jtet is a loop similar to isn1,
C      and the itetclr refers to an entry in d0_clrtab similar to icr1.
C      Currently jtet uses the same mbndry convention and mbndry as the
C      top dimension, except that if the jtet loop is longer than 3 it is
C      treated as an interface even if all 3 surfaces have the same itetclr.
C      Eventually I will recode so that it usees the jtet <>0 convention rather
C      than the <> mbndry convention to signal interfaces.
C      Because jtet is now a loop, only subroutines which have been modified
C      to accept jtet loops (and eventually jtet<0) will work.
C      See the coding below for examples using the jtet loop, or, eg, compare
C      Ver 1.16 and Ver 1.17 of tettestd.f for an example of the conversion
C      from no jtet loops allowed to jtet loops permitted.
C
C    - currently, the itetclr <-> imt1 translation table is not ordered.
C      for "d1" structures, it may be desireable to order it so that
C      one distinguishes between the imt1 above vs below (if 2d, or on the
C      left vs right if 1d)
C      Also, while it is attempted to order the surfaces consistently, there
C      is no guarantee. The subroutine order_surface_lower_d_lg is provided
C      which returns an integer flag "order(1:d#_nelements)" (where #=1 or 2)
C      the sign of which tells if an elements needs to be flipped to have
C      the surface order consistently. Possibly the flip should just be done
C      after creating the data structure, but I got tired of coding ...
C
C    - It is assumed no tet can touch itself with 2 different faces.
C      This means that I can just test if I am back in the original element
C      to see if I am finished looping around the jtet chain of a given face,
C      rather than having to test that I am back in the same element at the
C      same face.
C
C    - For coding convenience, I may have assumed in some subroutines
C      that if the topological dimension is three, then the jtet loops
C      are at most 2 long (ie: for 3D elements I assumes at most one
C      other element touches a given face, which rules out the
C      mesh coming from the surfaces of a 4-D object)
C
C    - While the user can create attributes which, eg, have length
C      d1_nelements or d1_nnodes, they will probably not be correctly
C      maintained if any command which changes the numbering or topology
C      is executed  (eg, massage, rmpoint compress, etc).
C      Only fields which live on nodes should be considered robust:
C      if used only for the lower d structures their values on the
C      interior nodes will simply be ignored.
C      The lower d data structures are "quasi-maintained" in the sense
C      that I have attempted to maintain the mesh attribute lower_d_flag
C      which signals that the data strucutres need to be refreshed.
C      (lower_d_flag=2 means the lower d data structures need updating,
C      lower_d_flag=1 means with any luck the lower d data structures are OK,
C      and lower_d_flag=0 or not existing means the data structures are not
C      desired; currently only geniee and rmpoint reset lower_d_flag=1 to
C      lower_d_flag=2, but hopefully this gets almost all topology-changing
C      commands....)
C      The sequence of filters should be remembered when the lower d
C      structures are refreshed, although this has not been extensively tested.
C
C    - When filtering, the lower d elements are not removed from
C      the data structure: the itetclr is simply set to negative.
C      Thus, eg, when writing a smoothing routine, one could still
C      use the full hierarchy but only restrict damage on the positve
C      itetclr elements.
C      Once the way this suite is used becomes established,
C      this convention may need to be changed.
C
C    - Rather than keeping separate node lists for each topological
C      hierarchy, the lower d elements point to the parent nodes
C      of the top dimension's mesh. The field d0_node_topo tells
C      the lowest relative dimension the node belongs to, and can
C      thus be used to create psets of nodes in the dimension of
C      interest.
C
C    - see also the notes above each subroutine
C
C   CHANGE HISTORY -
C
C  $Log: lower_d_lg.f,v $
C  Revision 2.00  2007/11/05 19:46:00  spchu
C  Import to CVS
C
CPVCS    
CPVCS       Rev 1.21   28 Jul 2001 14:13:04   jtg
CPVCS    fixed test for increasing d0_clrlen_att
CPVCS    
CPVCS       Rev 1.20   18 Sep 2000 15:13:56   dcg
CPVCS    initialize correctly
CPVCS
CPVCS       Rev 1.19   18 Sep 2000 12:47:26   dcg
CPVCS    initialize d0_clrlen_att and d0_nclrs_att
CPVCS
CPVCS       Rev 1.18   17 Aug 2000 17:31:00   jtg
CPVCS    ibtype added to discriminate intrcons from reflect surfaces;
CPVCS    also interior_icr_flag for controlling this by hand, or when
CPVCS    ibtype doesn't exist. (for ibtype, see surface routines)
CPVCS
CPVCS       Rev 1.17   27 Jul 2000 12:07:36   dcg
CPVCS    fix another place where ioff was not defined
CPVCS
CPVCS       Rev 1.16   27 Jul 2000 08:28:20   dcg
CPVCS    define ioff before using it
CPVCS    fix dimension on tmsgout
CPVCS
CPVCS       Rev 1.15   03 May 2000 01:25:56   jtg
CPVCS    vor2d set to "no" in d1 mesh,
CPVCS    and io flag for d0_node_topo set to "agx" instead of "x"
CPVCS
CPVCS       Rev 1.14   02 May 2000 12:14:08   jtg
CPVCS    added child points to extracted lower_d meshes
CPVCS
CPVCS       Rev 1.13   24 Apr 2000 12:19:08   jtg
CPVCS    mbndry values set using same conventions as in set_mbndry
CPVCS    (multiple of 1000000 .ge. 16000000, 1000000*int(1.2e-6*nelements*nef))
CPVCS    and when creating lower_d cmos mbndry set after calling cmo_newlen
CPVCS    so that mbndry consistent in the lower d meshes.
CPVCS
CPVCS       Rev 1.12   24 Apr 2000 11:46:56   jtg
CPVCS
CPVCS       Rev 1.11   17 Feb 2000 20:41:30   jtg
CPVCS    toned down verbosity
CPVCS
CPVCS       Rev 1.10   08 Feb 2000 16:37:10   jtg
CPVCS    if icontab exists but the constraint surf corresponding to the
CPVCS    surfaces in a lower_d element does not have a corresponding icr1 value,
CPVCS    print warning and use "minimum constraint" (rather than incrementing
CPVCS    nconbnd,icontab as is perhaps more correct)
CPVCS
CPVCS       Rev 1.9   27 Jan 2000 19:51:44   jtg
CPVCS    fixed error setting itet when extracting a d2 cmo with filters in effect.
CPVCS
CPVCS       Rev 1.8   27 Jan 2000 16:34:22   jtg
CPVCS    Log line was incorrect. Since the last log, this is jtet-loop, jtet_cycle_max
CPVCS    and mbndry=0 safe, and uses mbndry=0 when creating the lower d but
CPVCS    resetsit to the "input convention" at the end. Also it calls the cmo_delatt_all_lg
CPVCS    to delete the scalar attributes and all attributes that depend on them.
CPVCS    And the "lower_d/print" command has been added.
CPVCS
CPVCS       Rev 1.2   Mon Jan 03 14:40:20 2000   nnc
CPVCS    Fixed minor errors reported by the Dec compiler: out-of-order statements,
CPVCS    null character strings, multiply declared variables.
CPVCS
CPVCS       Rev 1.1   Tue Nov 30 19:31:44 1999   jtg
CPVCS    changed io field of attributes created when extracting from 'x' to 'agx'
CPVCS
CPVCS       Rev 1.0   Tue Nov 30 16:48:36 1999   jtg
CPVCS    Initial revision.
C
C #####################################################################
c-----------------------------------------------------------------------
C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|
c-----------------------------------------------------------------------
C #####################################################################
C   control_lower_d_lg
C
C   PURPOSE -
C
C      control_lower_d_lg is the command interface for the commands which
C      deal with creating and using the lower_d structures within the
C      mesh object.
C
C   INPUT ARGUMENTS -
C
C        imsgin() - Integer array of command input tokens
C        xmsgin() - Real array of command input tokens
C        cmsgin() - Character array of command input tokens
C        msgtyp() - Integer array of command input token types
C        nwds     - Number of command input tokens
C
C   OUTPUT ARGUMENTS -
C
C        ierror - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C   USAGE NOTES / CAVEATS / CHANGES -
C
C #####################################################################
 
      subroutine control_lower_d_lg(imsgin,xmsgin,cmsgin,msgtyp,nwds
     &                             ,ierror)
 
c ........................................................................
 
      implicit none
 
C arguments
      integer nwds,ierror
      character*(*) cmsgin(nwds)
      integer imsgin(nwds),msgtyp(nwds)
      real*8 xmsgin(nwds)
 
C variables
 
      integer lenaction,lcmo0,lcmo1,lcmo2,lcmo3
     &       ,ierr,local_debug,nwds_skip,iwd,n_extract
     &       ,lower_d_flag,len,ityp,ivalue,new_storage
     &       ,d0_nfilters,rankfilter,lact2,d0_nclrs
 
      pointer (ip_d0_clrtab,d0_clrtab),(ip_d0_clroff,d0_clroff)
      integer d0_clrtab(*),d0_clroff(*)
 
      integer icharlnf

      character*32 cmo0,cmo1,cmo2,cmo3,action,cmo_save,action2
      character*132 cbuf
 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C BEGIN begin
C
      call cmo_get_name(cmo_save,ierr)
      local_debug=0
 
      ierror=0
 
c  modify if command root changed ..............
 
      nwds_skip=1
      iwd=nwds_skip
 
c  find action ................
 
      iwd=iwd+1
      if (nwds.lt.iwd.or.msgtyp(iwd).ne.3) then
         action='create'
      else
         lenaction=icharlnf(cmsgin(iwd))
         action=cmsgin(iwd)(1:lenaction)
      endif
      lenaction=icharlnf(action)
 
c  find cmo0  - the name of the "parent" cmo ................
 
      iwd=iwd+1
      if (nwds.lt.iwd.or.msgtyp(iwd).ne.3) then
         cmo0='-def-'
      else
         lcmo0=icharlnf(cmsgin(iwd))
         cmo0=cmsgin(iwd)(1:lcmo0)
      endif
      lcmo0=icharlnf(cmo0)
      if (cmo0(1:lcmo0).eq.'-def-'.or.cmo0(1:lcmo0).eq.'-cmo-') then
         call cmo_get_name(cmo0,ierror)
         if (ierror.ne.0) goto 9999
         lcmo0=icharlnf(cmo0)
      endif
      call cmo_exist(cmo0,ierror)
      if (ierror.ne.0) goto 9999
 
      call cmo_get_info('lower_d_flag',cmo0(1:lcmo0)
     &                     ,lower_d_flag,len,ityp,ierr)
      if (ierror.ne.0) lower_d_flag=0
c .....................................................
 
      if (action(1:lenaction).eq.'create') then
 
c -----------------------------------------------------
c lower_d/*create[/cmo0|*-def-][/recreate|refilter|*new]
c (* marks default)
c creates lower d structures in cmo0
c (to modify how selected, use filter after create; see filter)
c "recreate" indicates that exisiting color table if any should be used
c rather than being re-created
c -----------------------------------------------------
 
         new_storage=1
         iwd=iwd+1
         if (nwds.ge.iwd.and.msgtyp(iwd).ne.3.and.
     &             (lower_d_flag.eq.2.or.lower_d_flag.eq.1)) then
            lenaction=icharlnf(cmsgin(iwd))
            action=cmsgin(iwd)(1:lenaction)
            if (action(1:lenaction).eq.'recreate') then
               ! new_storage=2 indicates don't destroy color table
               ! but do destroy filter table
               new_storage=2
            elseif (action(1:lenaction).eq.'refilter') then
               ! new_storage=0 indicates don't destroy color table
               ! and re-use filter table
               new_storage=0
            endif
         endif
 
         call create_lower_d_lg(cmo0(1:lcmo0),new_storage,ierror)
         if (ierror.ne.0) goto 9999
 
c .....................................................
 
      elseif (action(1:lenaction).eq.'print') then
 
c -----------------------------------------------------
c lower_d/print[/cmo0|*-def-][/*clrtab][filename|*-def-]
c (* marks default)
c + print the color table
c + "filename" option not yet implemented
c -----------------------------------------------------
 
         call cmo_get_info('d0_nclrs',cmo0,d0_nclrs,len,ityp,ierr)
         if (ierr.ne.0.or.d0_nclrs.le.0) d0_nclrs=0
         call cmo_get_info('d0_clrtab',cmo0,ip_d0_clrtab,len,ityp,ierr)
         call cmo_get_info('d0_clroff',cmo0,ip_d0_clroff,len,ityp,ierr)
         call print_clrtab_lower_d_lg(d0_nclrs,d0_clroff,d0_clrtab)
 
c .....................................................
 
      elseif (action(1:lenaction).eq.'release') then
 
c -----------------------------------------------------
c lower_d/release[/cmo0|*-def-]
c (* marks default)
c + releases lower d structure in cmo0
c -----------------------------------------------------
 
         call release_lower_d_lg(cmo0(1:lcmo0),ierror)
 
c .....................................................
 
      elseif (action(1:lenaction).eq.'extract') then
 
c -----------------------------------------------------
c lower_d/extract[/cmo0|*-def-][/cmo1|*-none-,cmo2|*-none-,cmo3|*-none-]
c                 /[*itetclr|no_color|recolor]
c (* marks default; recolor recommended)
c + if -none- for given dimension (default), lower d cmo not created
c   otherwise overwrites cmo1-3 if already exist
c + should add option to create lower d strucutres in cmo0 if not already extant: for now, just return error
c -----------------------------------------------------
 
         iwd=iwd+1
         if (nwds.lt.iwd.or.msgtyp(iwd).ne.3) then
            cmo1='-none-'
         else
            lcmo1=icharlnf(cmsgin(iwd))
            cmo1=cmsgin(iwd)(1:lcmo1)
            if (lcmo1.lt.1.or.cmo1(1:1).eq.'-') cmo1='-none-'
         endif
         lcmo1=icharlnf(cmo1)
         if ( cmo1(1:lcmo1).eq.cmo0(1:lcmo0) ) then
            cmo1='-none-'
            lcmo1=6
            cbuf='ERROR IN lower_d/extract: not extracting d1'
            call writloga('default',0,cbuf,0,ierr)
         endif
 
         iwd=iwd+1
         if (nwds.lt.iwd.or.msgtyp(iwd).ne.3) then
            cmo2='-none-'
         else
            lcmo2=icharlnf(cmsgin(iwd))
            cmo2=cmsgin(iwd)(1:lcmo2)
            if (lcmo2.lt.1.or.cmo2(1:1).eq.'-') cmo2='-none-'
         endif
         lcmo2=icharlnf(cmo2)
         if (     cmo2(1:lcmo2).eq.cmo0(1:lcmo0)
     &       .or. cmo2(1:lcmo2).eq.cmo1(1:lcmo1) ) then
            cmo2='-none-'
            lcmo2=6
            cbuf='ERROR IN lower_d/extract: not extracting d2'
            call writloga('default',0,cbuf,0,ierr)
         endif
 
         iwd=iwd+1
         if (nwds.lt.iwd.or.msgtyp(iwd).ne.3) then
            cmo3='-none-'
         else
            lcmo3=icharlnf(cmsgin(iwd))
            cmo3=cmsgin(iwd)(1:lcmo3)
            if (lcmo3.lt.1.or.cmo3(1:1).eq.'-') cmo3='-none-'
         endif
         lcmo3=icharlnf(cmo3)
         if (     cmo3(1:lcmo3).eq.cmo0(1:lcmo0)
     &       .or. cmo3(1:lcmo3).eq.cmo1(1:lcmo1)
     &       .or. cmo3(1:lcmo3).eq.cmo2(1:lcmo2) ) then
            cmo3='-none-'
            lcmo3=6
            cbuf='ERROR IN lower_d/extract: not extracting d3'
            call writloga('default',0,cbuf,0,ierr)
         endif
 
         n_extract=0
         if (cmo1(1:1).ne.'-') n_extract=n_extract+1
         if (cmo2(1:1).ne.'-') n_extract=n_extract+1
         if (cmo3(1:1).ne.'-') n_extract=n_extract+1
         if (n_extract.eq.0) goto 9999
 
         iwd=iwd+1
         if (nwds.lt.iwd.or.msgtyp(iwd).ne.3) then
            action='itetclr'
         else
            lenaction=icharlnf(cmsgin(iwd))
            action=cmsgin(iwd)(1:lenaction)
            if (action(1:lenaction).ne.'no_color' .and.
     &           action(1:lenaction).ne.'recolor') action='itetclr'
         endif
 
         if (lower_d_flag.ne.1) then
            new_storage=0
            if (lower_d_flag.ne.2.and.lower_d_flag.ne.1) new_storage=1
            call create_lower_d_lg(cmo0(1:lcmo0),new_storage,ierror)
            if (ierror.ne.0) goto 9999
         endif
 
         call extract_lower_d_lg(cmo0(1:lcmo0),cmo1(1:lcmo1)
     &                          ,cmo2(1:lcmo2),cmo3(1:lcmo3)
     &                          ,action,ierror)
         if (ierror.ne.0) goto 9999
 
         if (lower_d_flag.eq.0) then
            call release_lower_d_lg(cmo0(1:lcmo0),ierror)
            !NO - KEEP! call cmo_release(cmo1(1:lcmo1),ierror)
            !NO - KEEP! call cmo_release(cmo2(1:lcmo2),ierror)
            !NO - KEEP! call cmo_release(cmo3(1:lcmo3),ierror)
         endif
 
c .....................................................
 
      elseif (action(1:lenaction).eq.'filter'
     &              .or.action(1:lenaction).eq.'refilter'
     &              .or.action(1:lenaction).eq.'recreate') then
 
c -----------------------------------------------------
 
c lower_d/filter[/cmo0|*-def-]/[icr|itp|imt|clr]/#[/and|*or|new]
c lower_d/filter[/cmo0|*-def-]/[no_icr|no_itp|no_imt|no_clr]/#[/and|*or|new]
c lower_d/filter[/cmo0|*-def-]/[ext|int|vrt|real][/-not_used-/and|*or|new]
c lower_d/filter[/cmo0|*-def-]/[no_ext|no_int|no_vrt|no_real][/-not_used-/and|*or|new]
c lower_d/filter[/cmo0|*-def-]/reset
c lower_d/filter[/cmo0|*-def-]/refilter[/#]
c lower_d/filter[/cmo0|*-def-]/recreate
 
C$$ vs (old idea):
C$$   [/*all|interior|exterior|(imt #)][/*color_icr|nocolor_icr]
C$$ + if color_icr, then the "color" of the interface
C$$      is a unique number for each constraint + material colors combination
C$$      itetclr{surface} = serially packed {icr,imt 1-n}
C$$      (and constraint interfaces within a material DO appear in lower d structures)
C$$   otherwise the interface color depends only on the material colors encountered
C$$      itetclr{surface} = serially packed {imt 1-n}
C$$      (and constraint interfaces within a material do NOT appear in lower d structures)
c -----------------------------------------------------
 
 
         iwd=iwd+1
         if (action(1:lenaction).eq.'refilter'
     &              .or.action(1:lenaction).eq.'recreate') then
            continue
         elseif (nwds.ge.iwd.and.msgtyp(iwd).eq.3) then
            lenaction=icharlnf(cmsgin(iwd))
            if (lenaction.gt.0) then
               action=cmsgin(iwd)(1:lenaction)
            else
               action='reset'
               lenaction=5
            endif
         else
            action='reset'
            lenaction=5
         endif
 
         iwd=iwd+1
         if (nwds.ge.iwd.and.msgtyp(iwd).eq.2) then
            ivalue=xmsgin(iwd)
         elseif (nwds.ge.iwd.and.msgtyp(iwd).eq.1) then
            ivalue=xmsgin(iwd)
         else
            ivalue=0
         endif
 
         iwd=iwd+1
         if (nwds.ge.iwd.and.msgtyp(iwd).eq.3) then
            lact2=icharlnf(cmsgin(iwd))
            if (lact2.gt.0) then
               action2=cmsgin(iwd)(1:lact2)
            else
               action2='or'
               lact2=2
            endif
         else
            action2='or'
            lact2=2
         endif
 
         if (action(1:lenaction).eq.'refilter'.and.ivalue.gt.0) then
            call cmo_get_info('d0_nfilters',cmo0(1:lcmo0)
     &                     ,d0_nfilters,len,ityp,ierr)
            if (ierr.ne.0.and.d0_nfilters.gt.0) then
               ! coding caution:
               ! rankfilter must be consistent with rankfilter in filter_lower_d_lg
               rankfilter=3
               d0_nfilters=d0_nfilters-rankfilter*ivalue
               if (d0_nfilters.lt.0) d0_nfilters=0
               len=1
               ityp=1
               call cmo_set_info('d0_nfilters',cmo0(1:lcmo0)
     &                     ,d0_nfilters,len,ityp,ierr)
            endif
         endif
 
         if (lower_d_flag.ne.1
     &            .or.action(1:lenaction).eq.'refilter'
     &              .or.action(1:lenaction).eq.'recreate') then
            ! new_storage: 0=reuse color and filter tables
            !              1=reuse neither
            !              2=reuse color table only
            new_storage=0
            if (action(1:lenaction).eq.'recreate') new_storage=2
            if (lower_d_flag.ne.1.and.lower_d_flag.ne.2) new_storage=1
            call create_lower_d_lg(cmo0(1:lcmo0),new_storage,ierror)
            if (ierror.ne.0) goto 9999
         endif
         if (action(1:lenaction).ne.'refilter'
     &              .or.action(1:lenaction).eq.'recreate') then
            ! filtering done in create call for refilter
            call filter_lower_d_lg(cmo0,action,ivalue,action2,ierror)
            if (ierror.ne.0) goto 9999
         endif
 
c .....................................................
 
      else
 
c -----------------------------------------------------
c what other commands would one want?
c
c lower_d/dump/....
c   not coded: use, eg to dump just the 1-dimension lower mesh
c   use the command sequence:
c      lower_d/create/cmo0/(options)
c      lower_d/extract/cmo0/cmo1
c      dump/gmv/gmv.cmo1/cmo1
c      cmo release cmo1
c      lower_d/release/cmo0
c   to code, translate above sequence into single command,
c      including creates and releases as desired....
c
c -----------------------------------------------------
 
         goto 9999
 
c .....................................................
      endif
 
 
c........ (sucessful return) ..................
1000  ierror=0
      if (local_debug.gt.0) then
         write(*,*) 'finished control_lower_d_lg succesfully'
         call mmverify()
      endif
      len=icharlnf(cmo_save)
      cbuf='cmo select '//cmo_save(1:len)//'; finish'
      call cmo_get_name(cmo_save,ierr)
      return
 
c........ (failure return) ..................
9999  if (ierror.eq.0) ierror=1
      if (local_debug.gt.0) then
         write(*,*) 'finished control_lower_d_lg unsuccesfully'
         call mmverify()
      endif
      cbuf='ERROR IN ROUTINE control_lower_d_lg: ABORTING'
      call writloga('default',0,cbuf,0,ierr)
      cbuf='cmo select '//cmo_save(1:len)//'; finish'
      call cmo_get_name(cmo_save,ierr)
      return
c .....................................................
      end
 
c-----------------------------------------------------------------------
C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|
c-----------------------------------------------------------------------
C #####################################################################
C   create_lower_d_lg
C
C   PURPOSE -
C
C      create the lower d structures witin the mesh object cmo
C
C   INPUT ARGUMENTS -
C
C        cmo          - name of mesh object
C        new_storage  - flag to indicate whether storage exists
C                          0=reuse color and filter tables
C                          1=reuse neither
C                          2=reuse color table only
C
C   OUTPUT ARGUMENTS -
C
C        ierror - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C   USAGE NOTES / CAVEATS / CHANGES -
C
C #####################################################################
 
        subroutine create_lower_d_lg(cmo,new_storage,ierror)
 
c ........................................................................
 
        implicit none
 
        include 'local_element.h'
        include 'chydro.h'
 
        character*(*) cmo
        integer ierror,new_storage
 
        integer  mbndry,d0_nelements,nnodes,nconbnd,mbndry_save
        integer  d0_topo,d0_nee_cmo,d0_nef_cmo,d0_nen_cmo,d0_geom
     &          ,d0_nclrs,jtet_reduce_nnd
     &          ,d0_jtet_cycle_max,interior_icr_flag
 
        pointer  (ip_jtet,jtet),(ip_jtetoff,jtetoff)
     &          ,(ip_itet,itet),(ip_itetoff,itetoff)
     &          ,(ip_itettyp,itettyp),(ip_itetclr,itetclr)
     &          ,(ip_itp1,itp1),(ip_icr1,icr1)
     &          ,(ip_isn1,isn1),(ip_iparent,iparent)
     &          ,(ip_icontab,icontab)
     &          ,(ip_d0_node_topo,d0_node_topo)
     &          ,(ip_d0_clrtab,d0_clrtab),(ip_d0_clroff,d0_clroff)
        integer jtet(*),jtetoff(*)
     &          ,itet(*),itetoff(*)
     &          ,itettyp(*),itetclr(*)
     &          ,itp1(*),icr1(*)
     &          ,isn1(*),iparent(*)
     &          ,icontab(50,*)
     &          ,d0_node_topo(*)
     &          ,d0_clrtab(*),d0_clroff(*)
 
        character*32 geom_name
        pointer (ip_ibtype,ibtype)
        character*32 ibtype(*)
 
        integer  d1_nnodes,d1_nelements
     &          ,d1_nef_cmo,d1_nee_cmo,d1_nen_cmo
     &          ,d1_jtet_cycle_max
 
        pointer  (ip_d1_itet,d1_itet),(ip_d1_itetoff,d1_itetoff)
     &          ,(ip_d1_jtet,d1_jtet),(ip_d1_jtetoff,d1_jtetoff)
     &          ,(ip_d1_itettyp,d1_itettyp),(ip_d1_itetclr,d1_itetclr)
     &          ,(ip_d1_elm_d0,d1_elm_d0)
        integer  d1_itet(*),d1_itetoff(*)
     &          ,d1_jtet(*),d1_jtetoff(*)
     &          ,d1_itettyp(*),d1_itetclr(*)
     &          ,d1_elm_d0(*)
 
        integer  d2_nnodes,d2_nelements
     &          ,d2_nef_cmo,d2_nee_cmo,d2_nen_cmo
     &          ,d2_jtet_cycle_max
 
        pointer  (ip_d2_itet,d2_itet),(ip_d2_itetoff,d2_itetoff)
     &          ,(ip_d2_jtet,d2_jtet),(ip_d2_jtetoff,d2_jtetoff)
     &          ,(ip_d2_itettyp,d2_itettyp),(ip_d2_itetclr,d2_itetclr)
     &          ,(ip_d2_elm_d1,d2_elm_d1)
        integer  d2_itet(*),d2_itetoff(*)
     &          ,d2_jtet(*),d2_jtetoff(*)
     &          ,d2_itettyp(*),d2_itetclr(*)
     &          ,d2_elm_d1(*)

        pointer (ipd2_elm_tmp, d2_elm_tmp)
        integer d2_elm_tmp(*)
 
        integer  d3_nnodes,d3_nelements
     &          ,d3_nef_cmo,d3_nee_cmo,d3_nen_cmo
 
        pointer  (ip_dn_elm,dn_elm)
        integer  dn_elm(*)
 
        integer j,len,ityp,ierr,ioff
     &          ,iel
     &          ,d_topo,local_debug
        real*8 xxx
 
 
        character*132 cbuf
        character*32 isubname
 
        integer icharlnf,lcmo
        external icharlnf
 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        local_debug=0
 
        lcmo=icharlnf(cmo)
        call cmo_exist(cmo(1:lcmo),ierr)
        if (ierr.ne.0) goto 9998
 
        isubname='tmp_lower_d_lg'
 
        if (local_debug.ne.0) then
           write(*,*) 'reached create_lower_d_lg'
           write(cbuf,*)'cmo status; finish'
           call dotask(cbuf,ierr)
        endif
 
c........ (create new attributes) ..................
 
        if (local_debug.gt.0) then
           write(*,*) 'starting storage_lower_d_lg'
           call mmverify()
        endif
        call storage_lower_d_lg(cmo,new_storage,ierror)
        if (ierror.ne.0) goto 9998
        if (local_debug.gt.0) then
           write(*,*) 'finished storage_lower_d_lg'
           call mmverify()
        endif
 
c........ (reset mbndry to mbndry=0 convention) ..................
 
        call cmo_get_info('mbndry',cmo,mbndry,len,ityp,ierr)
        if (ierr.ne.0) mbndry=0
        mbndry_save=mbndry
 
        ! reset mbndry to use mbndry=0 convention
        if (mbndry_save.ne.0) then
           mbndry=0
           call reset_mbndry_lower_d_lg(cmo,mbndry,ierr)
           if (ierr.ne.0) goto 9999
        endif
 
c........ (get d0 info) ..................
 
        ! if jtet_reduce_nnd exists (created by user), then
        ! pass to geniee, otherwise just pass zero
        ! (faces of different nnd cannot touch)
        call cmo_get_info('jtet_reduce_nnd',cmo,jtet_reduce_nnd
     &                    ,len,ityp,ierr)
        if (ierr.ne.0) jtet_reduce_nnd=0
        call cmo_get_info('jtet_cycle_max',cmo,d0_jtet_cycle_max
     &                    ,len,ityp,ierr)
        if (ierr.ne.0) d0_jtet_cycle_max=2
 
        call cmo_get_info('nnodes',cmo,nnodes,len,ityp,ierr)
        if (ierr.ne.0.or.nnodes.lt.1) goto 9999
        call cmo_get_info('nelements',cmo,d0_nelements,len,ityp,ierr)
        if (ierr.ne.0.or.d0_nelements.lt.1) goto 9999
        call cmo_get_info('ndimensions_topo',cmo,d0_topo
     &                    ,len,ityp,ierr)
        if (ierr.ne.0) goto 9999
 
c no lower dimensional structures if d0_topo=0
        if (d0_topo.lt.1) goto 500
 
        call cmo_get_info('faces_per_element',cmo,d0_nef_cmo
     &                    ,len,ityp,ierr)
        if (ierr.ne.0.or.
     &    (d0_nelements.gt.0.and.d0_nef_cmo.lt.d0_topo+1)) goto 9999
        call cmo_get_info('edges_per_element',cmo,d0_nee_cmo
     &                    ,len,ityp,ierr)
        if (ierr.ne.0.or.d0_nee_cmo.lt.0) goto 9999
        call cmo_get_info('nodes_per_element',cmo,d0_nen_cmo
     &                    ,len,ityp,ierr)
        if (ierr.ne.0.or.
     &       (d0_nelements.gt.0.and.d0_nen_cmo.lt.d0_topo+1)) goto 9999
        call cmo_get_info('ndimensions_geom',cmo,d0_geom
     &                    ,len,ityp,ierr)
        if (ierr.ne.0.or.d0_geom.lt.1.or.d0_geom.gt.3) goto 9999
 
        call cmo_get_info('jtet',cmo,ip_jtet,len,ityp,ierr)
        call cmo_get_info('jtetoff',cmo,ip_jtetoff,len,ityp,ierr)
        call cmo_get_info('itet',cmo,ip_itet,len,ityp,ierr)
        call cmo_get_info('itetoff',cmo,ip_itetoff,len,ityp,ierr)
        call cmo_get_info('itetclr',cmo,ip_itetclr,len,ityp,ierr)
        call cmo_get_info('itettyp',cmo,ip_itettyp,len,ityp,ierr)
        call cmo_get_info('isn1',cmo,ip_isn1,len,ityp,ierr)
        call cmo_get_info('itp1',cmo,ip_itp1,len,ityp,ierr)
        call cmo_get_info('icr1',cmo,ip_icr1,len,ityp,ierr)
        call cmo_get_info('nconbnd',cmo,nconbnd,len,ityp,ierr)
        if (ierr.ne.0.or.nconbnd.le.0) then
           nconbnd=0
        else
           call cmo_get_info('icontab',cmo,ip_icontab,len,ityp,ierr)
           if (ierr.ne.0) nconbnd=0
        endif
 
        call mmggetbk('iparent',isubname,ip_iparent,nnodes,1,ierr)
        call unpackpc(nnodes,itp1,isn1,iparent)
 
        call cmo_get_info('d0_node_topo',cmo,ip_d0_node_topo
     &                       ,len,ityp,ierr)
        if (d0_topo.gt.1) call mmggetbk('dn_elm',isubname
     &                     ,ip_dn_elm,d0_nelements*d0_nef_cmo,1,ierr)
 
        ! no need to check mbndry: using mbndry=0 convention
 
c  lower_d_flag ...............
c  lower_d_flag=0: no lower d; =1: has lower d; =2: lower d wanted but not set
 
        call cmo_set_info('lower_d_flag',cmo,2,len,ityp,ierr)
        if (ierr.ne.0) goto 9999
 
c interior_icr_flag .........
        ! check for global flag to ignore possibly spurious
        ! interior interface or virtual constraints
        call cmo_get_info('interior_icr_flag',cmo,interior_icr_flag
     &       ,len,ityp,ierr)
        if (ierr.ne.0.or.interior_icr_flag.gt.7
     &               .or.interior_icr_flag.lt.0) interior_icr_flag=0
 
        ! get info for checking surfaces defns as virtual,intrcons,etc
        call cmo_get_attinfo('geom_name',cmo,iel,xxx,geom_name,
     &                        j,len,ityp,ierr)
        if (ierr.ne.0) geom_name='-defaultgeom-'
        call mmfindbk('ibtype',geom_name,ip_ibtype,len,ierr)
        if (ierr.ne.0) geom_name='-none-'
 
c =======================================================================
 
c........ (find d1 lengths) ..................
 
c d1_nnodes,d1_nelements
c d1_nef_cmo,d1_nee_cmo,d1_nen_cmo
c to find d1_nnodes, use d0_node_topo
c also find dn_elm if d0_topo>1
 
        d_topo=d0_topo
        if (local_debug.gt.0) then
           write(*,*) 'starting d1 sizes_lower_d_lg'
           call mmverify()
        endif
        call sizes_lower_d_lg(d0_topo,d_topo
     &          ,nnodes,d0_nelements,d0_nef_cmo,mbndry,nconbnd
     &          ,d0_jtet_cycle_max,interior_icr_flag,ibtype,geom_name
     &          ,itettyp,itetclr,itet,itetoff,jtet,jtetoff
     &          ,itp1,icr1,icontab,isn1,iparent
     &          ,d1_nnodes,d1_nelements
     &          ,d1_nef_cmo,d1_nee_cmo,d1_nen_cmo
     &          ,d0_node_topo,dn_elm
     &          ,ierror)
        if (ierror.ne.0) goto 9999
        if (local_debug.gt.0) then
           write(*,*) 'finished d1 sizes_lower_d_lg'
     &          ,d1_nnodes,d1_nelements
     &          ,d1_nef_cmo,d1_nee_cmo,d1_nen_cmo
           call mmverify()
        endif
        ! write(*,*)'d1_nnodes,d1_nelements',d1_nnodes,d1_nelements
 
c........ (assign d1 storage) ..................
c Note: aborted above for d0_topo<1 ...
c for d0_topo=1 => set d1_nelements=0 as not using elements for dim=0
 
        if (d0_topo.eq.1) then
           d1_nelements=0
           d1_nef_cmo=0
           d1_nee_cmo=0
           d1_nen_cmo=0
        endif
 
        len=1
        ityp=1
        call cmo_set_info('d1_nnodes',cmo,d1_nnodes,len,ityp,ierr)
 
        !! nothing more to do if no elements ......
        if (d1_nelements.eq.0.or.d0_topo.le.1) goto 500
 
        call cmo_set_info('d1_nelements',cmo,d1_nelements,len,ityp,ierr)
        call cmo_set_info('d1_nef_cmo',cmo,d1_nef_cmo,len,ityp,ierr)
        call cmo_set_info('d1_nee_cmo',cmo,d1_nee_cmo,len,ityp,ierr)
        call cmo_set_info('d1_nen_cmo',cmo,d1_nen_cmo,len,ityp,ierr)
 
        call cmo_newlen(cmo,ierr) ! OK re mbndry as using mbndry=0
 
        call cmo_get_info('d1_itet',cmo,ip_d1_itet,len,ityp,ierr)
        call cmo_get_info('d1_itetoff',cmo,ip_d1_itetoff,len,ityp,ierr)
        call cmo_get_info('d1_jtet',cmo,ip_d1_jtet,len,ityp,ierr)
        call cmo_get_info('d1_jtetoff',cmo,ip_d1_jtetoff,len,ityp,ierr)
        call cmo_get_info('d1_itettyp',cmo,ip_d1_itettyp,len,ityp,ierr)
        call cmo_get_info('d1_itetclr',cmo,ip_d1_itetclr,len,ityp,ierr)
        call cmo_get_info('d1_elm_d0',cmo,ip_d1_elm_d0,len,ityp,ierr)
 
        ! re-get re newlen ...
        call cmo_get_info('jtet',cmo,ip_jtet,len,ityp,ierr)
        call cmo_get_info('jtetoff',cmo,ip_jtetoff,len,ityp,ierr)
        call cmo_get_info('itet',cmo,ip_itet,len,ityp,ierr)
        call cmo_get_info('itetoff',cmo,ip_itetoff,len,ityp,ierr)
        call cmo_get_info('itetclr',cmo,ip_itetclr,len,ityp,ierr)
        call cmo_get_info('itettyp',cmo,ip_itettyp,len,ityp,ierr)
        call cmo_get_info('isn1',cmo,ip_isn1,len,ityp,ierr)
        call cmo_get_info('itp1',cmo,ip_itp1,len,ityp,ierr)
        call cmo_get_info('icr1',cmo,ip_icr1,len,ityp,ierr)
        if (nconbnd.gt.0)
     &      call cmo_get_info('icontab',cmo,ip_icontab,len,ityp,ierr)
        call mmfindbk('iparent',isubname,ip_iparent,len,ierr)
        call cmo_get_info('d0_node_topo',cmo,ip_d0_node_topo
     &                    ,len,ityp,ierr)
        if (d0_topo.gt.1) call mmfindbk('dn_elm',isubname
     &                     ,ip_dn_elm,len,ierr)
        if (geom_name(1:6).ne.'-none-')
     &      call mmfindbk('ibtype',geom_name,ip_ibtype,len,ierr)
 
        if (local_debug.ne.0) then
           !write(cbuf,*)'cmo status; finish'
           !call dotask(cbuf,ierr)
        endif
 
c........ (assign d1 itet info) ..................
c assign d1_elm_d0,itettyp,d1_itet,d1_itetoff,d1_jtetoff
 
        if (local_debug.gt.0) then
           write(*,*) 'starting d1 itet_lower_d_lg'
           call mmverify()
        endif
        d_topo=d0_topo
        call itet_lower_d_lg(d0_nelements,d0_nef_cmo
     &      ,iparent,itettyp,itetoff,itet,dn_elm,jtetoff
     &      ,d1_nelements,d1_nef_cmo
     &      ,d1_elm_d0,d1_itettyp,d1_itetoff,d1_itet,d1_jtetoff
     &      ,ierror)
        if (ierror.ne.0) goto 9999
 
        if (local_debug.gt.0) then
           write(*,*) 'finished d1 itet_lower_d_lg'
           call mmverify()
        endif
 
c........ (find d1 itetclr) ..................
c increment color table as necessary...
 
        d_topo=1
        call itetclr_lower_d_lg(cmo
     &          ,d_topo,nconbnd,mbndry
     &          ,d0_jtet_cycle_max,jtet_reduce_nnd
     &          ,interior_icr_flag,ibtype,geom_name
     &          ,d0_nelements,d0_nef_cmo
     &          ,icontab,itp1,icr1,isn1,iparent
     &          ,itetclr,jtetoff,jtet
     &          ,d1_nelements,d1_nef_cmo
     &          ,d1_itettyp,d1_itetclr,d1_itetoff,d1_itet,d1_elm_d0
     &          ,ierror)
        if (ierror.ne.0) goto 9999
        ! need to re-get pointers ...
 
        if (local_debug.gt.0) then
           write(*,*) 'finished d1 itetclr_lower_d_lg'
           call mmverify()
        endif
 
        ! do I need to re-get info re itetclr_lower_d_lg
        ! increasing clrtab block?? Hopefully not ...
 
c........ (find jtet from itet) ..................
c note: aborted above for d0_topo<2 => jtet exists
 
        if (local_debug.gt.0) then
           write(*,*) 'starting d1 sub_geniee_cmo_lg'
           call mmverify()
        endif
        call sub_geniee_cmo_lg(
     &         jtet_reduce_nnd,d1_nelements,d1_nef_cmo,mbndry
     &        ,ip_d1_itetclr,ip_d1_itettyp,ip_d1_itetoff
     &        ,ip_d1_jtetoff,ip_d1_itet
     &        ,ip_iparent,ip_d1_jtet,d1_jtet_cycle_max,ierror)
        if (ierror.ne.0) goto 9999
        if (local_debug.gt.0) then
           write(*,*) 'finished d1 sub_geniee_cmo_lg'
           call mmverify()
        endif
        ! set d1_jtet_cycle_max
        call cmo_set_info('d1_jtet_cycle_max',cmo,d1_jtet_cycle_max
     &                    ,len,ityp,ierr)
 
c ...... (see if need to order surfaces) ....
        call mmggetbk('dn_elm',isubname,ip_dn_elm,d1_nelements,1,ierr)
        call order_surface_lower_d_lg(
     &           d1_nelements,d1_nef_cmo,mbndry,iparent
     &          ,d1_itettyp,d1_itetoff,d1_jtetoff,d1_itet,d1_jtet
     &          ,dn_elm,ierror)
        do iel=1,d1_nelements
           ityp=d1_itettyp(iel)
           if (dn_elm(iel).lt.0 .and. (ityp.eq.ifelmtri
     &               .or.ityp.eq.ifelmqud.or.ityp.eq.ifelmlin)
     &        ) goto 440
        enddo
        goto 442
440     continue
        if (local_debug.gt.0) then
          write(*,*) 'd1 surfaces required re-ordering'
        endif
        ! this should not occur except for virtual surfaces
        ! and parents being a network
        ! for now, do lazy way: re-order itet and call jtet.
        ! should fix parent as no longer ordered same as parent...
        ! for now, ignore.
        do iel=1,d1_nelements
           if (dn_elm(iel).lt.0) then
              ityp=d1_itettyp(iel)
              ioff=d1_itetoff(iel)
              if (ityp.ne.ifelmtri.and.ityp.ne.ifelmqud
     &               .and.ityp.ne.ifelmlin ) goto 441
              if (ityp.eq.ifelmlin) then
                 j=d1_itet(ioff+1)
                 d1_itet(ioff+1)=d1_itet(ioff+2)
                 d1_itet(ioff+2)=j
              elseif (ityp.eq.ifelmtri) then
                 j=d1_itet(ioff+1)
                 d1_itet(ioff+1)=d1_itet(ioff+3)
                 d1_itet(ioff+3)=j
              else ! if (ityp.eq.ifelmqud) then
                 j=d1_itet(ioff+1)
                 d1_itet(ioff+1)=d1_itet(ioff+4)
                 d1_itet(ioff+4)=j
                 j=d1_itet(ioff+2)
                 d1_itet(ioff+2)=d1_itet(ioff+3)
                 d1_itet(ioff+3)=j
              endif
           endif
441        continue
        enddo
        call sub_geniee_cmo_lg(
     &         jtet_reduce_nnd,d1_nelements,d1_nef_cmo,mbndry
     &        ,ip_d1_itetclr,ip_d1_itettyp,ip_d1_itetoff
     &        ,ip_d1_jtetoff,ip_d1_itet
     &        ,ip_iparent,ip_d1_jtet,d1_jtet_cycle_max,ierror)
        if (ierror.ne.0) goto 9999
        ! set d1_jtet_cycle_max
        call cmo_set_info('d1_jtet_cycle_max',cmo,d1_jtet_cycle_max
     &                    ,len,ityp,ierr)
442     continue
 
 
c =======================================================================
c........ (start d2 structures) ..................
 
c........ (find d2 lengths) ..................
 
c d2_nnodes,d2_nelements
c d2_nef_cmo,d2_nee_cmo,d2_nen_cmo
c to find d2_nnodes
c also find dn_elm if d0_topo>2
 
        d_topo=d0_topo-1
        if (local_debug.gt.0) then
           write(*,*) 'starting d2 sizes_lower_d_lg'
           call mmverify()
        endif
        if (d_topo.gt.1) call mmggetbk('dn_elm',isubname
     &          ,ip_dn_elm,d1_nelements*d1_nef_cmo,1,ierr)
        call sizes_lower_d_lg(d0_topo,d_topo
     &          ,nnodes,d1_nelements,d1_nef_cmo,mbndry,nconbnd
     &          ,d1_jtet_cycle_max,interior_icr_flag,ibtype,geom_name
     &          ,d1_itettyp,d1_itetclr,d1_itet,d1_itetoff
     &          ,d1_jtet,d1_jtetoff
     &          ,itp1,icr1,icontab,isn1,iparent
     &          ,d2_nnodes,d2_nelements
     &          ,d2_nef_cmo,d2_nee_cmo,d2_nen_cmo
     &          ,d0_node_topo,dn_elm
     &          ,ierror)
        if (ierror.ne.0) goto 9999
        if (local_debug.gt.0) then
           write(*,*) 'finished d2 sizes_lower_d_lg'
     &          ,d2_nnodes,d2_nelements
     &          ,d2_nef_cmo,d2_nee_cmo,d2_nen_cmo
           call mmverify()
        endif
        ! write(*,*)'d2_nnodes,d2_nelements',d2_nnodes,d2_nelements
 
c Note: aborted above for d0_topo<2 ...
c for d0_topo=2 => set d2_nelements=0 as not using elements for dim=0
        if (d0_topo.eq.2) then
           d2_nelements=0
           d2_nef_cmo=0
           d2_nee_cmo=0
           d2_nen_cmo=0
        endif
 
c........ (assign d2 storage, d1 to d2 storage) ..................
 
        len=1
        ityp=1
        call cmo_set_info('d2_nnodes',cmo,d2_nnodes,len,ityp,ierr)
 
        !! nothing more to do if no elements ....
        if (d2_nelements.eq.0.or.d0_topo.le.2) goto 500
 
        call cmo_set_info('d2_nelements',cmo,d2_nelements,len,ityp,ierr)
        call cmo_set_info('d2_nef_cmo',cmo,d2_nef_cmo,len,ityp,ierr)
        call cmo_set_info('d2_nee_cmo',cmo,d2_nee_cmo,len,ityp,ierr)
        call cmo_set_info('d2_nen_cmo',cmo,d2_nen_cmo,len,ityp,ierr)
 
        call cmo_newlen(cmo,ierr) ! OK re mbndry as using mbndry=0
 
        call cmo_get_info('d2_itet',cmo,ip_d2_itet,len,ityp,ierr)
        call cmo_get_info('d2_itetoff',cmo,ip_d2_itetoff,len,ityp,ierr)
        call cmo_get_info('d2_jtet',cmo,ip_d2_jtet,len,ityp,ierr)
        call cmo_get_info('d2_jtetoff',cmo,ip_d2_jtetoff,len,ityp,ierr)
        call cmo_get_info('d2_itettyp',cmo,ip_d2_itettyp,len,ityp,ierr)
        call cmo_get_info('d2_itetclr',cmo,ip_d2_itetclr,len,ityp,ierr)
        call cmo_get_info('d2_elm_d1',cmo,ip_d2_elm_d1,len,ityp,ierr)

C       this is not expected to be used, but pass into routines 
C       with correct length in case it is
        call cmo_get_info('d2_elm_tmp',cmo,ipd2_elm_tmp,len,ityp,ierr)
 
        ! re-get re newlen ...
        call cmo_get_info('d1_jtet',cmo,ip_d1_jtet,len,ityp,ierr)
        call cmo_get_info('d1_jtetoff',cmo,ip_d1_jtetoff,len,ityp,ierr)
        call cmo_get_info('d1_itet',cmo,ip_d1_itet,len,ityp,ierr)
        call cmo_get_info('d1_itetoff',cmo,ip_d1_itetoff,len,ityp,ierr)
        call cmo_get_info('d1_itetclr',cmo,ip_d1_itetclr,len,ityp,ierr)
        call cmo_get_info('d1_itettyp',cmo,ip_d1_itettyp,len,ityp,ierr)
        if (geom_name(1:6).ne.'-none-')
     &      call mmfindbk('ibtype',geom_name,ip_ibtype,len,ierr)
        if (nconbnd.gt.0)
     &      call cmo_get_info('icontab',cmo,ip_icontab,len,ityp,ierr)
        call mmfindbk('iparent',isubname,ip_iparent,len,ierr)
        call cmo_get_info('d0_node_topo',cmo,ip_d0_node_topo
     &                    ,len,ityp,ierr)
        if (d0_topo.gt.2) call mmfindbk('dn_elm',isubname
     &                     ,ip_dn_elm,len,ierr)
 
        if (local_debug.ne.0) then
           !write(cbuf,*)'cmo status; finish'
           !call dotask(cbuf,ierr)
        endif
 
c........ (find d2 itet info) ..................
c d2_elm_d1
c and find d2_itet,d2_itetoff,d2_jtetoff,d2_itettyp
 
        if (local_debug.gt.0) then
           write(*,*) 'starting d2 itet_lower_d_lg'
           call mmverify()
        endif
 
        d_topo=d0_topo-1
        call itet_lower_d_lg(d1_nelements,d1_nef_cmo
     &      ,iparent,d1_itettyp,d1_itetoff,d1_itet,dn_elm,d1_jtetoff
     &      ,d2_nelements,d2_nef_cmo
     &      ,d2_elm_d1,d2_itettyp,d2_itetoff,d2_itet,d2_jtetoff
     &      ,ierror)
        if (ierror.ne.0) goto 9999
 
        if (local_debug.gt.0) then
           write(*,*) 'finished d2 itet_lower_d_lg'
           call mmverify()
        endif
 
c........ (find d2_itetclr) ..................
c increment color table as necessary...
 
        d_topo=2
        call itetclr_lower_d_lg(cmo
     &          ,d_topo,nconbnd,mbndry
     &          ,d1_jtet_cycle_max,jtet_reduce_nnd
     &          ,interior_icr_flag,ibtype,geom_name
     &          ,d1_nelements,d1_nef_cmo
     &          ,icontab,itp1,icr1,isn1,iparent
     &          ,d1_itetclr,d1_jtetoff,d1_jtet
     &          ,d2_nelements,d2_nef_cmo
     &          ,d2_itettyp,d2_itetclr,d2_itetoff,d2_itet,d2_elm_d1
     &          ,ierror)
        if (ierror.ne.0) goto 9999
 
        if (local_debug.gt.0) then
           write(*,*) 'finished d2 itetclr_lower_d_lg'
           call mmverify()
        endif
 
        ! do I need to re-get info re itetclr_lower_d_lg
        ! increasing clrtab block?? Hopefully not ...
 
c........ (now find jtet from itet) ..................
c note: aborted above for d0_topo<3 => jtet exists
 
        if (local_debug.gt.0) then
           write(*,*) 'starting d2 sub_geniee_cmo_lg'
           call mmverify()
        endif
 
        call sub_geniee_cmo_lg(
     &         jtet_reduce_nnd,d2_nelements,d2_nef_cmo,mbndry
     &        ,ip_d2_itetclr,ip_d2_itettyp
     &        ,ip_d2_itetoff,ip_d2_jtetoff,ip_d2_itet
     &        ,ip_iparent,ip_d2_jtet,d2_jtet_cycle_max,ierror)
        if (ierror.ne.0) goto 9999
        ! set d2_jtet_cycle_max
        call cmo_set_info('d2_jtet_cycle_max',cmo,d2_jtet_cycle_max
     &                    ,len,ityp,ierr)
 
        if (local_debug.gt.0) then
           write(*,*) 'finished d2 sub_geniee_cmo_lg'
           call mmverify()
        endif
 
c ...... (see if need to order surfaces) ....
        call mmggetbk('dn_elm',isubname,ip_dn_elm,d2_nelements,1,ierr)
        call order_surface_lower_d_lg(
     &           d2_nelements,d2_nef_cmo,mbndry,iparent
     &          ,d2_itettyp,d2_itetoff,d2_jtetoff,d2_itet,d2_jtet
     &          ,dn_elm,ierror)
        do iel=1,d1_nelements
           ityp=d2_itettyp(iel)
           if (dn_elm(iel).lt.0 .and. (ityp.eq.ifelmtri
     &               .or.ityp.eq.ifelmqud.or.ityp.eq.ifelmlin)
     &        ) goto 450
        enddo
        goto 452
450     continue
        if (local_debug.gt.0) then
          write(*,*) 'd2 surfaces required re-ordering'
        endif
        ! this should not occur except for virtual surfaces
        ! and parents being a network
        ! for now, do lazy way: re-order itet and call jtet.
        ! should fix parent as no longer ordered same as parent...
        ! for now, ignore.
        do iel=1,d2_nelements
           if (dn_elm(iel).lt.0) then
              ityp=d2_itettyp(iel)
              ioff=d2_itetoff(iel)
              if (ityp.ne.ifelmtri.and.ityp.ne.ifelmqud
     &               .and.ityp.ne.ifelmlin ) goto 451
              if (ityp.eq.ifelmlin) then
                 j=d2_itet(ioff+1)
                 d2_itet(ioff+1)=d2_itet(ioff+2)
                 d2_itet(ioff+2)=j
              elseif (ityp.eq.ifelmtri) then
                 j=d2_itet(ioff+1)
                 d2_itet(ioff+1)=d2_itet(ioff+3)
                 d2_itet(ioff+3)=j
              else ! if (ityp.eq.ifelmqud) then
                 j=d2_itet(ioff+1)
                 d2_itet(ioff+1)=d1_itet(ioff+4)
                 d2_itet(ioff+4)=j
                 j=d2_itet(ioff+2)
                 d2_itet(ioff+2)=d2_itet(ioff+3)
                 d2_itet(ioff+3)=j
              endif
           endif
451        continue
        enddo
        call sub_geniee_cmo_lg(
     &         jtet_reduce_nnd,d2_nelements,d2_nef_cmo,mbndry
     &        ,ip_d2_itetclr,ip_d2_itettyp,ip_d2_itetoff
     &        ,ip_d2_jtetoff,ip_d2_itet
     &        ,ip_iparent,ip_d2_jtet,d2_jtet_cycle_max,ierror)
        if (ierror.ne.0) goto 9999
        ! set d2_jtet_cycle_max
        call cmo_set_info('d2_jtet_cycle_max',cmo,d2_jtet_cycle_max
     &                    ,len,ityp,ierr)
452     continue
 
c =======================================================================
 
c........ (start d3 structures) ..................
 
c........ (find d3 lengths) ..................
 
c d3_nnodes,d3_nelements
c d3_nef_cmo,d3_nee_cmo,d3_nen_cmo
 
C TAM - can not pass j scalar just because d2_elm_d3 is not used
C       need to pass appropriate non scalar
C          ,d0_node_topo,j changed to 0_node_topo,d2_elm_tmp

        j=0  ! d2_elm_d3 -> not used as d_topo=1
        d_topo=d0_topo-2
 
c for d0_topo>3, not enough storage was created,
c but press on just creating node translation
        if (d_topo.gt.1) d_topo=1 ! goto 9999
 
        if (local_debug.gt.0) then
           write(*,*) 'starting d3 sizes_lower_d_lg'
           call mmverify()
        endif
        call sizes_lower_d_lg(d0_topo,d_topo
     &          ,nnodes,d2_nelements,d2_nef_cmo,mbndry,nconbnd
     &          ,d2_jtet_cycle_max,interior_icr_flag,ibtype,geom_name
     &          ,d2_itettyp,d2_itetclr,d2_itet,d2_itetoff
     &          ,d2_jtet,d2_jtetoff
     &          ,itp1,icr1,icontab,isn1,iparent
     &          ,d3_nnodes,d3_nelements
     &          ,d3_nef_cmo,d3_nee_cmo,d3_nen_cmo
     &          ,d0_node_topo,d2_elm_tmp
     &          ,ierror)
        if (ierror.ne.0) goto 9999
        if (local_debug.gt.0) then
           write(*,*) 'finished d3 sizes_lower_d_lg'
     &          ,d3_nnodes,d3_nelements
     &          ,d3_nef_cmo,d3_nee_cmo,d3_nen_cmo
           call mmverify()
        endif
 
c Note: aborted above for d0_topo<3, and just doing node transl if d0_topo>3
c => set d3_nelements=0 as not using elements for dim=0
        d3_nelements=0
        d3_nef_cmo=0
        d3_nee_cmo=0
        d3_nen_cmo=0
 
c........ (assign d3 storage) ..................
c note there are no elements -> no element storage
 
        ! .............
 
        len=1
        ityp=1
        call cmo_set_info('d3_nnodes',cmo,d3_nnodes,len,ityp,ierr)
 
        !call cmo_set_info('d3_nelements',cmo,d3_nelements,len,ityp,ierr)
        !call cmo_set_info('d3_nef_cmo',cmo,d3_nef_cmo,len,ityp,ierr)
        !call cmo_set_info('d3_nee_cmo',cmo,d3_nee_cmo,len,ityp,ierr)
        !call cmo_set_info('d3_nen_cmo',cmo,d3_nen_cmo,len,ityp,ierr)
 
        ! not needed as nothing depends on d3_nnodes
        ! call cmo_newlen(cmo,ierr) ! OK re mbndry as using mbndry=0
 
 
c =======================================================================
 
c........ (sucessful return) ..................
500     ierror=0
 
        if (new_storage.eq.0) then
           call filter_lower_d_lg(cmo,'refilter',ityp,'-not_used-',ierr)
           if (ierr.ne.0) goto 9999
        endif
 
        ! do this re "dangerous" lack of it in itetclr_lower_d_lg
        call cmo_newlen(cmo,ierr) ! OK re mbndry as using mbndry=0
 
        ! set lower_d_flag ...............
        call cmo_set_info('lower_d_flag',cmo,1,len,ityp,ierr)
 
        call mmrelprt(isubname,ierr)
 
        if (local_debug.gt.0) then
           write(*,*) 'finished create_lower_d_lg succesfully'
           call mmverify()
           write(cbuf,*)'cmo status; finish'
           call dotask(cbuf,ierr)
           call cmo_get_info('d0_nclrs',cmo,d0_nclrs,len,ityp,ierr)
           if (ierr.ne.0.or.d0_nclrs.le.0) goto 9999
           call cmo_get_info('d0_clrtab',cmo,ip_d0_clrtab,len,ityp,ierr)
           if (ierr.ne.0) goto 9999
           call cmo_get_info('d0_clroff',cmo,ip_d0_clroff,len,ityp,ierr)
           if (ierr.ne.0) goto 9999
           write(*,*) 'd0_nclrs=',d0_nclrs
           call print_clrtab_lower_d_lg(d0_nclrs,d0_clroff,d0_clrtab)
        endif
 
        ! reset mbndry to use input mbndry convention
        ! make even multiple of 1000000 using same rule as set_mbndry
        ! so not increased by that routine
        if (mbndry_save.ne.0) then
           mbndry=mbndry_save
           if (mbndry.le.16000000) mbndry=16000000
           j=1000000*int(1.2e-6*float(d0_nelements*d0_nef_cmo)+0.5)
           if (mbndry.le.j) mbndry=j
           j=1000000*int(1.2e-6*float(d1_nelements*d1_nef_cmo)+0.5)
           if (mbndry.le.j) mbndry=j
           j=1000000*int(1.2e-6*float(d2_nelements*d2_nef_cmo)+0.5)
           if (mbndry.le.j) mbndry=j
           call reset_mbndry_lower_d_lg(cmo,mbndry,ierr)
           if (ierr.ne.0) goto 9999
        endif
 
        return
 
c........ (failure return) ..................
9999    continue
        ! reset mbndry to use input mbndry convention
        if (mbndry_save.ne.0) then
           mbndry=mbndry_save
           if (mbndry.le.d0_nelements*d0_nef_cmo)
     &           mbndry=d0_nelements*d0_nef_cmo+10000
           if (mbndry.le.d1_nelements*d1_nef_cmo)
     &           mbndry=d1_nelements*d1_nef_cmo+10000
           if (mbndry.le.d2_nelements*d2_nef_cmo)
     &           mbndry=d2_nelements*d2_nef_cmo+10000
           call reset_mbndry_lower_d_lg(cmo,mbndry,ierr)
           if (ierr.ne.0) goto 9999
        endif
 
9998    ierror=1
        call mmrelprt(isubname,ierr)
        if (local_debug.gt.0) then
           write(*,*) 'finished create_lower_d_lg unsuccesfully'
           call mmverify()
           stop
        endif
        cbuf='ERROR IN ROUTINE create_lower_d_lg: ABORTING'
        call writloga('default',0,cbuf,0,ierr)
        return
c .....................................................
        end
 
c-----------------------------------------------------------------------
C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|
c-----------------------------------------------------------------------
C #####################################################################
C   extract_lower_d_lg
C
C   PURPOSE -
C
C      extract low d structures and place in the desired mesh object
C
C   INPUT ARGUMENTS -
C
C        cmo0  - name of the mesh to extract from
C        cmo1  - name of mesh to extract d1 structure to (if -none-, not extracted)
C        cmo2  - name of mesh to extract d2 structure to (if -none-, not extracted)
C        cmo3  - name of mesh to extract d3 structure to (if -none-, not extracted)
C        action - flag to recolor extracted mesh:
C             = "recolor", use gcolor routines in neighbor_recolor_lg.f
C             = "no_color", make single material mesh, saving itetclr in d0_clr_up
C
C   OUTPUT ARGUMENTS -
C
C        ierror - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C   USAGE NOTES / CAVEATS / CHANGES -
C
C #####################################################################
 
        subroutine extract_lower_d_lg(cmo0,cmo1,cmo2,cmo3,action,ierror)
 
c ........................................................................
 
        implicit none
 
        include 'local_element.h'
        include 'chydro.h'
 
        character*(*) cmo0,cmo1,cmo2,cmo3,action
        integer ierror
 
        integer  mbndry,nelements,nnodes
        integer  d0_topo,d0_nee_cmo,d0_nef_cmo,d0_nen_cmo,d0_geom
     &          ,d0_jtet_cycle_max,jtet_reduce_nnd
 
        pointer  (ip_jtet,jtet),(ip_jtetoff,jtetoff)
     &          ,(ip_itet,itet),(ip_itetoff,itetoff)
     &          ,(ip_itettyp,itettyp),(ip_itetclr,itetclr)
     &          ,(ip_itp1,itp1),(ip_imt1,imt1),(ip_icr1,icr1)
     &          ,(ip_isn1,isn1),(ip_iparent,iparent)
     &          ,(ip_d0_node_topo,d0_node_topo)
     &          ,(ip_d0_node_up,d0_node_up),(ip_d0_elm_up,d0_elm_up)
     &          ,(ip_d0_clr_up,d0_clr_up)
        integer jtet(*),jtetoff(*)
     &          ,itet(*),itetoff(*)
     &          ,itettyp(*),itetclr(*)
     &          ,itp1(*),imt1(*),icr1(*)
     &          ,isn1(*),iparent(*)
     &          ,d0_node_topo(*)
     &          ,d0_node_up(*),d0_elm_up(*),d0_clr_up(*)
 
        integer  d1_nnodes,d1_nelements
     &          ,d1_nef_cmo,d1_nee_cmo,d1_nen_cmo
     &          ,d1_jtet_cycle_max
 
        pointer  (ip_d1_itet,d1_itet),(ip_d1_itetoff,d1_itetoff)
     &          ,(ip_d1_jtet,d1_jtet),(ip_d1_jtetoff,d1_jtetoff)
     &          ,(ip_d1_itettyp,d1_itettyp),(ip_d1_itetclr,d1_itetclr)
     &          ,(ip_d1_elm_d0,d1_elm_d0)
        integer  d1_itet(*),d1_itetoff(*)
     &          ,d1_jtet(*),d1_jtetoff(*)
     &          ,d1_itettyp(*),d1_itetclr(*)
     &          ,d1_elm_d0(*)
 
        integer  d2_nnodes,d2_nelements
     &          ,d2_nef_cmo,d2_nee_cmo,d2_nen_cmo
     &          ,d2_jtet_cycle_max
 
        pointer  (ip_d2_itet,d2_itet),(ip_d2_itetoff,d2_itetoff)
     &          ,(ip_d2_jtet,d2_jtet),(ip_d2_jtetoff,d2_jtetoff)
     &          ,(ip_d2_itettyp,d2_itettyp),(ip_d2_itetclr,d2_itetclr)
     &          ,(ip_d2_elm_d1,d2_elm_d1)
        integer  d2_itet(*),d2_itetoff(*)
     &          ,d2_jtet(*),d2_jtetoff(*)
     &          ,d2_itettyp(*),d2_itetclr(*)
     &          ,d2_elm_d1(*)
 
        integer  d3_nnodes,d3_nelements
     &          ,d3_nef_cmo,d3_nee_cmo,d3_nen_cmo
 
        pointer  (ip_xic,xic),(ip_yic,yic),(ip_zic,zic)
        real*8   xic(*),yic(*),zic(*)
        pointer  (ip_xic2,xic2),(ip_yic2,yic2),(ip_zic2,zic2)
        real*8   xic2(*),yic2(*),zic2(*)
        pointer  (ip_icr2,icr2),(ip_node_dn,node_dn)
        integer  icr2(*),node_dn(*)
 
 
        integer i,j,len,ityp,ierr,ind,ioff,joff
     &          ,iel,jel,iface,jface,jt,iclr,lact
     &          ,i1,local_debug,kel,kface
     &          ,nclrs,d0_nclrs
 
        character*132 cbuf
        character*32 isubname
        character*40 cmo_save
 
        integer icharlnf,lcmo1,lcmo2,lcmo3,lcmo0
        external icharlnf
 
        character*32 cmsgout(12)
        real*8 xmsgout(12)
        integer imsgout(12),nwdsout,tmsgout(12)
 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        local_debug=0
        isubname='tmp_lower_d_lg'
        lact=icharlnf(action)
        call cmo_get_name(cmo_save,ierr)
 
        if (local_debug.ne.0) then
           write(*,*) 'reached extract_lower_d_lg'
           write(cbuf,*)'cmo status; finish'
           call dotask(cbuf,ierr)
           call mmverify()
        endif
 
        nwdsout=12
        do i=1,12
           xmsgout(i)=0.d0
           imsgout(i)=0
           tmsgout(i)=3
           cmsgout(i)=' '
        enddo
        tmsgout(11)=1
        cmsgout(1)='cmo'
        cmsgout(2)='addatt'
        cmsgout(6)='scalar'
        cmsgout(9)='temporary'
        cmsgout(10)='agx'
        cmsgout(11)=' '
        tmsgout(12)=1
        imsgout(12)=0
 
c........ (check if cmo0 and lower d cmo names legal
c          and set lcmo1-lcmo3 as flag to extract) ..................
 
        lcmo0=icharlnf(cmo0)
        if (lcmo0.lt.1) goto 9999
        call cmo_exist(cmo0,ierr)
        if (ierr.ne.0) goto 9999
 
        if (local_debug.ne.0) then
           call mmverify()
        endif
 
        call cmo_get_info('ndimensions_topo',cmo0,d0_topo,len,ityp,ierr)
        if (ierr.ne.0.or.d0_topo.lt.0) goto 9999
        if (d0_topo.eq.0) goto 4000
        call cmo_get_info('ndimensions_geom',cmo0,d0_geom,len,ityp,ierr)
        if (ierr.ne.0.or.d0_geom.lt.1.or.d0_geom.gt.3) goto 9999
        call cmo_get_info('nnodes',cmo0,nnodes,len,ityp,ierr)
        if (ierr.ne.0.or.nnodes.lt.1) goto 9999
        call cmo_get_info('nelements',cmo0,nelements,len,ityp,ierr)
        if (ierr.ne.0.or.nelements.lt.1) nelements=0
        call cmo_get_info('faces_per_element',cmo0,d0_nef_cmo
     &                    ,len,ityp,ierr)
        if (ierr.ne.0.or.d0_nef_cmo.lt.d0_topo+1) nelements=0
        call cmo_get_info('edges_per_element',cmo0,d0_nee_cmo
     &                    ,len,ityp,ierr)
        if (ierr.ne.0.or.d0_nee_cmo.lt.0) nelements=0
        call cmo_get_info('nodes_per_element',cmo0,d0_nen_cmo
     &                    ,len,ityp,ierr)
        if (ierr.ne.0.or.d0_nen_cmo.lt.d0_topo+1) nelements=0
        call cmo_get_info('mbndry',cmo0,mbndry,len,ityp,ierr)
        if (ierr.ne.0) mbndry=0
        if (mbndry.le.d0_nef_cmo*nelements
     &                    .and.mbndry.ne.0) goto 9999
        call cmo_get_info('d0_nclrs',cmo0,d0_nclrs,len,ityp,ierr)
        if (ierr.ne.0.or.d0_nclrs.lt.0) d0_nclrs=0
        call cmo_get_info('jtet_reduce_nnd',cmo0,jtet_reduce_nnd
     &                    ,len,ityp,ierr)
        if (ierr.ne.0) jtet_reduce_nnd=0
        call cmo_get_info('jtet_cycle_max',cmo0,d0_jtet_cycle_max
     &                    ,len,ityp,ierr)
        if (ierr.ne.0.or.d0_jtet_cycle_max.lt.2) d0_jtet_cycle_max=2
 
        ! get lower d cmo names
        lcmo1=icharlnf(cmo1)
        if (lcmo1.lt.1 .or. cmo1(1:1).eq.'-' .or. d0_topo.lt.1 ) lcmo1=0
        if (lcmo1.gt.0 .and. cmo0(1:lcmo0).eq.cmo1(1:lcmo1) )  lcmo1=-1
        lcmo2=icharlnf(cmo2)
        if (lcmo2.lt.1 .or. cmo2(1:1).eq.'-' .or. d0_topo.lt.2 ) lcmo2=0
        if (lcmo2.gt.0
     &       .and. (     cmo0(1:lcmo0).eq.cmo2(1:lcmo2)
     &              .or. cmo1(1:lcmo1).eq.cmo2(1:lcmo2) )  ) lcmo2=-1
        lcmo3=icharlnf(cmo3)
        if (lcmo3.lt.1 .or. cmo3(1:1).eq.'-' .or. d0_topo.lt.3 ) lcmo3=0
        if (lcmo2.gt.0
     &       .and. (     cmo0(1:lcmo0).eq.cmo3(1:lcmo3)
     &              .or. cmo1(1:lcmo1).eq.cmo3(1:lcmo3)
     &              .or. cmo2(1:lcmo2).eq.cmo3(1:lcmo3) )  ) lcmo3=-1
 
c........ (get sizes for lower d) ..................
 
        ! get lower d nnode info
        if (lcmo1.gt.0) then
           call cmo_get_info('d1_nnodes',cmo0,d1_nnodes,len,ityp,ierr)
           if (ierr.ne.0.or.d1_nnodes.lt.1) lcmo1=-2
        endif
        if (lcmo2.gt.0) then
           call cmo_get_info('d2_nnodes',cmo0,d2_nnodes,len,ityp,ierr)
           if (ierr.ne.0.or.d2_nnodes.lt.1) lcmo2=-2
        endif
        if (lcmo3.gt.0) then
           call cmo_get_info('d3_nnodes',cmo0,d3_nnodes,len,ityp,ierr)
           if (ierr.ne.0.or.d3_nnodes.lt.1) lcmo3=-2
        endif
 
        if (lcmo1.gt.0) then
           call cmo_get_info('d1_nelements',cmo0,d1_nelements
     &                       ,len,ityp,ierr)
           if (ierr.ne.0) d1_nelements=0
           call cmo_get_info('d1_nef_cmo',cmo0,d1_nef_cmo,len,ityp,ierr)
           if (ierr.ne.0.or.d1_nef_cmo.lt.d0_topo) d1_nelements=0
           call cmo_get_info('d1_nee_cmo',cmo0,d1_nee_cmo,len,ityp,ierr)
           if (ierr.ne.0.or.d1_nee_cmo.lt.0) d1_nelements=0
           call cmo_get_info('d1_nen_cmo',cmo0,d1_nen_cmo,len,ityp,ierr)
           if (ierr.ne.0.or.d1_nen_cmo.lt.d0_topo) d1_nelements=0
           call cmo_get_info('d1_jtet_cycle_max',cmo0,d1_jtet_cycle_max
     &                    ,len,ityp,ierr)
           if (ierr.ne.0.or.d1_jtet_cycle_max.lt.2) d1_jtet_cycle_max=2
           if (d1_nelements.le.0.or.d0_topo.eq.1.or.nelements.eq.0) then
              d1_nelements=0
              d1_nef_cmo=0
              d1_nee_cmo=0
              d1_nen_cmo=0
           endif
        endif
        if (lcmo2.gt.0) then
           call cmo_get_info('d2_nelements',cmo0,d2_nelements
     &                       ,len,ityp,ierr)
           if (ierr.ne.0) d2_nelements=0
           call cmo_get_info('d2_nef_cmo',cmo0,d2_nef_cmo,len,ityp,ierr)
           if (ierr.ne.0.or.d2_nef_cmo.lt.d0_topo-1) d2_nelements=0
           call cmo_get_info('d2_nee_cmo',cmo0,d2_nee_cmo,len,ityp,ierr)
           if (ierr.ne.0.or.d2_nee_cmo.lt.0) d2_nelements=0
           call cmo_get_info('d2_nen_cmo',cmo0,d2_nen_cmo,len,ityp,ierr)
           if (ierr.ne.0.or.d2_nen_cmo.lt.d0_topo-1) d2_nelements=0
           call cmo_get_info('d2_jtet_cycle_max',cmo0,d2_jtet_cycle_max
     &                    ,len,ityp,ierr)
           if (ierr.ne.0.or.d2_jtet_cycle_max.lt.2) d2_jtet_cycle_max=2
           if (d2_nelements.le.0.or.d0_topo.eq.2
     &           .or.nelements.eq.0.or.d1_nelements.eq.0) then
              d2_nelements=0
              d2_nef_cmo=0
              d2_nee_cmo=0
              d2_nen_cmo=0
           endif
        endif
        if (lcmo3.gt.0) then
           d3_nelements=0
           d3_nef_cmo=0
           d3_nee_cmo=0
           d3_nen_cmo=0
        endif
 
        if (local_debug.ne.0) then
           call mmverify()
        endif
 
c........ (check that any valid nodes) ..................
 
        ! this should be equivalent to nnodes test above!!
        if (local_debug.gt.0) then
 
           call cmo_get_info('itp1',cmo0,ip_itp1,len,ityp,ierr)
           if (ierr.ne.0) goto 9999
           call cmo_get_info('isn1',cmo0,ip_isn1,len,ityp,ierr)
           if (ierr.ne.0) goto 9999
 
           call mmggetbk('iparent',isubname,ip_iparent,nnodes,1,ierr)
           if (ierr.ne.0) goto 9999
           call unpackpc(nnodes,itp1,isn1,iparent)
 
           call cmo_get_info('d0_node_topo',cmo0,ip_d0_node_topo
     &                       ,len,ityp,ierr)
           if (ierr.ne.0) goto 9999
 
           ! check that any valid nodes
           if (lcmo1.gt.0) then
              do i=1,nnodes
                 if (abs(d0_node_topo(i)).ge.1
     &                   .and.i.eq.iparent(i)) goto 501
              enddo
              stop ! lcmo1=-lcmo1
           endif
501        if (lcmo2.gt.0) then
              do i=1,nnodes
                 if (abs(d0_node_topo(i)).ge.1
     &                   .and.i.eq.iparent(i)) goto 502
              enddo
              cbuf=' lower_d/extract not creating cmo2= '//cmo2(1:lcmo2)
              call writloga('default',0,cbuf,0,ierr)
              stop ! lcmo2=-lcmo2
           endif
502        if (lcmo3.gt.0) then
              do i=1,nnodes
                 if (abs(d0_node_topo(i)).ge.1
     &                   .and.i.eq.iparent(i)) goto 503
              enddo
              cbuf=' lower_d/extract not creating cmo3= '//cmo3(1:lcmo3)
              call writloga('default',0,cbuf,0,ierr)
              stop ! lcmo3=-lcmo3
           endif
503        continue
 
        endif
 
c........ (create lower d cmos) ..................
 
        if (lcmo1.gt.0) then
C$         cbuf='cmo/release/'//cmo1(1:lcmo1)
C$   &         //'; cmo/create /'//cmo1(1:lcmo1)//'; finish'
C$         call dotask(cbuf,ierr)
           call cmo_exist(cmo1,ierr)
           if (ierr.eq.0) call cmo_release(cmo1,ierr)
           call cmo_create(cmo1,ierr)
           len=1
           ityp=1
           call cmo_set_info('ndimensions_topo',cmo1,d0_topo-1
     &                    ,len,ityp,ierr)
           if (ierr.ne.0) lcmo1=-3
           call cmo_set_info('ndimensions_geom',cmo1,d0_geom
     &                    ,len,ityp,ierr)
           if (ierr.ne.0) lcmo1=-3
           call cmo_set_info('nnodes',cmo1,d1_nnodes,len,ityp,ierr)
           if (ierr.ne.0) lcmo1=-3
           call cmo_set_info('nelements',cmo1,d1_nelements
     &                     ,len,ityp,ierr)
           if (ierr.ne.0) lcmo1=-3
           call cmo_set_info('faces_per_element',cmo1,d1_nef_cmo
     &                     ,len,ityp,ierr)
           if (ierr.ne.0) lcmo1=-3
           call cmo_set_info('edges_per_element',cmo1,d1_nee_cmo
     &                     ,len,ityp,ierr)
           if (ierr.ne.0) lcmo1=-3
           call cmo_set_info('nodes_per_element',cmo1,d1_nen_cmo
     &                     ,len,ityp,ierr)
           if (ierr.ne.0) lcmo1=-3
           call cmo_newlen(cmo1,ierr)
           if (ierr.ne.0) lcmo1=-3
           ! set mbndry to same as higher-d mesh
           call cmo_set_info('mbndry',cmo1,mbndry,len,ityp,ierr)
           if (ierr.ne.0.and.mbndry.ne.0) lcmo1=-3
           if (lcmo1.eq.-3) then
C$            cbuf='cmo/release/'//cmo1(1:lcmo1)//'; finish'
C$            call dotask(cbuf,ierr)
              call cmo_release(cmo1,ierr)
           else
C$            cbuf=cmo/addatt/'//cmo1(1:lcmo1)//'/d0_node_up'
C$   &             //'/VINT/scalar/nnodes/user/temporary/agx/0/; finish'
C$            call dotask(cbuf,ierr)
              cmsgout(3)=cmo1
              cmsgout(4)='d0_node_up'
              cmsgout(5)='VINT'
              cmsgout(7)='nnodes'
              cmsgout(8)='user'
              call cmo_addatt(imsgout,xmsgout,cmsgout,tmsgout
     &                           ,nwdsout,ierr)
              if (d1_nelements.gt.0.and.jtet_reduce_nnd.eq.1) then
C$               cbuf='cmo/addatt/'//cmo1(1:lcmo1)//'/jtet_cycle_max'
C$   &              //'/INT/scalar/scalar//temporary/x/2/; finish'
C$               call dotask(cbuf,ierr)
                 cmsgout(4)='jtet_cycle_max'
                 cmsgout(5)='INT'
                 cmsgout(7)='scalar'
                 cmsgout(8)='constant'
                 imsgout(11)=2
                 call cmo_addatt(imsgout,xmsgout,cmsgout,tmsgout
     &                           ,nwdsout,ierr)
                 imsgout(11)=0
                 len=1
                 ityp=1
                 call cmo_set_info('jtet_cycle_max',cmo1(1:lcmo1)
     &                              ,d1_jtet_cycle_max,len,ityp,ierr)
              endif
              if (d1_nelements.gt.0.and.d1_jtet_cycle_max.gt.2) then
C$               cbuf='cmo/addatt/'//cmo1(1:lcmo1)//'/jtet_cycle_max'
C$   &              //'/INT/scalar/scalar//temporary/x/2/; finish'
C$               call dotask(cbuf,ierr)
                 cmsgout(4)='jtet_cycle_max'
                 cmsgout(5)='INT'
                 cmsgout(7)='scalar'
                 cmsgout(8)='constant'
                 imsgout(11)=2
                 call cmo_addatt(imsgout,xmsgout,cmsgout,tmsgout
     &                           ,nwdsout,ierr)
                 imsgout(11)=0
                 len=1
                 ityp=1
                 call cmo_set_info('jtet_cycle_max',cmo1(1:lcmo1)
     &                              ,d1_jtet_cycle_max,len,ityp,ierr)
              endif
              if (d1_nelements.gt.0) then
C$               cbuf='cmo/addatt/'//cmo1(1:lcmo1)//'/d0_elm_up'
C$   &              //'/VINT/scalar/nelements//temporary/agx/0/; finish'
C$               call dotask(cbuf,ierr)
                 cmsgout(4)='d0_elm_up'
                 cmsgout(5)='VINT'
                 cmsgout(7)='nelements'
                 cmsgout(8)='user'
                 call cmo_addatt(imsgout,xmsgout,cmsgout,tmsgout
     &                           ,nwdsout,ierr)
                 if (action(1:lact).eq.'recolor'
     &               .or.action(1:lact).eq.'no_color') then
C$                  cbuf='cmo/addatt/'//cmo1(1:lcmo1)//'/d0_clr_up'
C$   &              //'/VINT/scalar/nelements//temporary/agx/0/; finish'
C$                  call dotask(cbuf,ierr)
                    cmsgout(4)='d0_clr_up'
                    call cmo_addatt(imsgout,xmsgout,cmsgout,tmsgout
     &                           ,nwdsout,ierr)
                 endif
              endif
           endif
        endif
        if (lcmo1.lt.0) then
           cbuf=' lower_d/extract not creating cmo1= '
     &                 //cmo1(1:icharlnf(cmo1))
           call writloga('default',0,cbuf,0,ierr)
        endif
 
        if (lcmo2.gt.0) then
C$         cbuf='cmo/release/'//cmo2(1:lcmo2)
C$   &         //'; cmo/create /'//cmo2(1:lcmo2)//'; finish'
C$         call dotask(cbuf,ierr)
           call cmo_exist(cmo2,ierr)
           if (ierr.eq.0) call cmo_release(cmo2,ierr)
           call cmo_create(cmo2,ierr)
           len=1
           ityp=1
           call cmo_set_info('ndimensions_topo',cmo2,d0_topo-2
     &                    ,len,ityp,ierr)
           if (ierr.ne.0) lcmo2=-3
           call cmo_set_info('ndimensions_geom',cmo2,d0_geom
     &                    ,len,ityp,ierr)
           if (ierr.ne.0) lcmo2=-3
           call cmo_set_info('nnodes',cmo2,d2_nnodes,len,ityp,ierr)
           if (ierr.ne.0) lcmo2=-3
           call cmo_set_info('nelements',cmo2,d2_nelements
     &                     ,len,ityp,ierr)
           if (ierr.ne.0) lcmo2=-3
           call cmo_set_info('faces_per_element',cmo2,d2_nef_cmo
     &                     ,len,ityp,ierr)
           if (ierr.ne.0) lcmo2=-3
           call cmo_set_info('edges_per_element',cmo2,d2_nee_cmo
     &                     ,len,ityp,ierr)
           if (ierr.ne.0) lcmo2=-3
           call cmo_set_info('nodes_per_element',cmo2,d2_nen_cmo
     &                     ,len,ityp,ierr)
           if (ierr.ne.0) lcmo2=-3
           i=0
           call cmo_newlen(cmo2,ierr)
           if (ierr.ne.0) lcmo2=-3
           ! set mbndry to same as higher-d mesh
           call cmo_set_info('mbndry',cmo2,mbndry,len,ityp,ierr)
           if (ierr.ne.0.and.mbndry.ne.0) lcmo2=-3
           if (lcmo2.eq.-3) then
C$            cbuf='cmo/release/'//cmo2(1:lcmo2)//'; finish'
C$            call dotask(cbuf,ierr)
              call cmo_release(cmo2,ierr)
           else
C$            cbuf=cmo/addatt/'//cmo2(1:lcmo2)//'/d0_node_up'
C$   &             //'/VINT/scalar/nnodes/user/temporary/agx/0/; finish'
C$            call dotask(cbuf,ierr)
              cmsgout(3)=cmo2(1:lcmo2)
              cmsgout(4)='d0_node_up'
              cmsgout(5)='VINT'
              cmsgout(7)='nnodes'
              cmsgout(8)='user'
              call cmo_addatt(imsgout,xmsgout,cmsgout,tmsgout
     &                           ,nwdsout,ierr)
              if (d2_nelements.gt.0.and.d2_jtet_cycle_max.gt.2) then
C$               cbuf='cmo/addatt/'//cmo2(1:lcmo2)//'/jtet_cycle_max'
C$   &             //'/INT/scalar/scalar//temporary/x/2/; finish'
C$               call dotask(cbuf,ierr)
                 cmsgout(4)='jtet_cycle_max'
                 cmsgout(5)='INT'
                 cmsgout(7)='scalar'
                 cmsgout(8)='constant'
                 imsgout(11)=2
                 call cmo_addatt(imsgout,xmsgout,cmsgout,tmsgout
     &                           ,nwdsout,ierr)
                 imsgout(11)=0
                 len=1
                 ityp=1
                 call cmo_set_info('jtet_cycle_max',cmo2(1:lcmo2)
     &                             ,d2_jtet_cycle_max,len,ityp,ierr)
              endif
              if (d2_nelements.gt.0) then
C$               cbuf='cmo/addatt/'//cmo2(1:lcmo2)//'/d0_elm_up'
C$   &             //'/VINT/scalar/nelements//temporary/agx/0/; finish'
C$               call dotask(cbuf,ierr)
                 cmsgout(4)='d0_elm_up'
                 cmsgout(5)='VINT'
                 cmsgout(7)='nelements'
                 cmsgout(8)='user'
                 call cmo_addatt(imsgout,xmsgout,cmsgout,tmsgout
     &                           ,nwdsout,ierr)
                 if (action(1:lact).eq.'recolor'
     &               .or.action(1:lact).eq.'no_color') then
C$                  cbuf='cmo/addatt/'//cmo2(1:lcmo2)//'/d0_clr_up'
C$   &                //'/VINT/scalar/nelements//temporary/agx/0/; finish'
C$                  call dotask(cbuf,ierr)
                    cmsgout(4)='d0_clr_up'
                    call cmo_addatt(imsgout,xmsgout,cmsgout,tmsgout
     &                           ,nwdsout,ierr)
                 endif
              endif
           endif
        endif
        if (lcmo2.lt.0) then
           cbuf=' lower_d/extract not creating cmo2= '
     &                 //cmo2(1:icharlnf(cmo2))
           call writloga('default',0,cbuf,0,ierr)
        endif
 
        if (lcmo3.gt.0) then
C$         cbuf='cmo/release/'//cmo3(1:lcmo3)
C$   &         //'; cmo/create /'//cmo3(1:lcmo3)//'; finish'
C$         call dotask(cbuf,ierr)
           call cmo_exist(cmo3,ierr)
           if (ierr.eq.0) call cmo_release(cmo3,ierr)
           call cmo_create(cmo3,ierr)
           len=1
           ityp=1
           call cmo_set_info('ndimensions_topo',cmo3,d0_topo-3
     &                    ,len,ityp,ierr)
           if (ierr.ne.0) lcmo3=-3
           call cmo_set_info('ndimensions_geom',cmo3,d0_geom
     &                    ,len,ityp,ierr)
           if (ierr.ne.0) lcmo3=-3
           call cmo_set_info('nnodes',cmo3,d3_nnodes,len,ityp,ierr)
           if (ierr.ne.0) lcmo3=-3
           call cmo_set_info('nelements',cmo3,d3_nelements
     &                     ,len,ityp,ierr)
           if (ierr.ne.0) lcmo3=-3
           call cmo_set_info('faces_per_element',cmo3,d3_nef_cmo
     &                     ,len,ityp,ierr)
           if (ierr.ne.0) lcmo3=-3
           call cmo_set_info('edges_per_element',cmo3,d3_nee_cmo
     &                     ,len,ityp,ierr)
           if (ierr.ne.0) lcmo3=-3
           call cmo_set_info('nodes_per_element',cmo3,d3_nen_cmo
     &                     ,len,ityp,ierr)
           if (ierr.ne.0) lcmo3=-3
           call cmo_newlen(cmo3,ierr)
           if (ierr.ne.0) lcmo3=-3
           ! set mbndry to same as higher-d mesh
           call cmo_set_info('mbndry',cmo3,mbndry,len,ityp,ierr)
           if (ierr.ne.0.and.mbndry.ne.0) lcmo3=-3
           if (lcmo3.eq.-3) then
C$            cbuf='cmo/release/'//cmo3(1:lcmo3)//'; finish'
C$            call dotask(cbuf,ierr)
              call cmo_release(cmo3,ierr)
           endif
        endif
        if (lcmo3.lt.0) then
           cbuf=' lower_d/extract not creating cmo3= '
     &                 //cmo3(1:icharlnf(cmo3))
           call writloga('default',0,cbuf,0,ierr)
        else
C$         cbuf=cmo/addatt/'//cmo3(1:lcmo3)//'/d0_node_up'
C$   &             //'/VINT/scalar/nnodes/user/temporary/agx/0/; finish'
C$         call dotask(cbuf,ierr)
           cmsgout(3)=cmo3(1:lcmo3)
           cmsgout(4)='d0_node_up'
           cmsgout(5)='VINT'
           cmsgout(7)='nnodes'
           cmsgout(8)='user'
           call cmo_addatt(imsgout,xmsgout,cmsgout,tmsgout,nwdsout,ierr)
        endif
 
        if (local_debug.ne.0) then
           call mmverify()
        endif
 
        ! make sure that at least one mesh to extract
        if (lcmo1.le.0 .and. lcmo2.le.0 .and. lcmo3.le.0) goto 4000
 
c........ (get d0 info) ..................
 
        call cmo_get_info('itp1',cmo0,ip_itp1,len,ityp,ierr)
        if (ierr.ne.0) goto 9999
        call cmo_get_info('isn1',cmo0,ip_isn1,len,ityp,ierr)
        if (ierr.ne.0) goto 9999
        call cmo_get_info('icr1',cmo0,ip_icr1,len,ityp,ierr)
        if (ierr.ne.0) goto 9999
        call cmo_get_info('xic',cmo0,ip_xic,len,ityp,ierr)
        if (ierr.ne.0) goto 9999
        call cmo_get_info('yic',cmo0,ip_yic,len,ityp,ierr)
        if (ierr.ne.0.and.d0_geom.ge.2) goto 9999
        call cmo_get_info('zic',cmo0,ip_zic,len,ityp,ierr)
        if (ierr.ne.0.and.d0_geom.ge.3) goto 9999
 
        call mmggetbk('iparent',isubname,ip_iparent,nnodes,1,ierr)
        if (ierr.ne.0) goto 9999
        call mmggetbk('node_dn',isubname,ip_node_dn,nnodes,1,ierr)
        if (ierr.ne.0) goto 9999
        call unpackpc(nnodes,itp1,isn1,iparent)
 
        call cmo_get_info('d0_node_topo',cmo0,ip_d0_node_topo
     &                       ,len,ityp,ierr)
        if (ierr.ne.0) goto 9999
 
c =======================================================================
 
c........ (extract d1 into cmo1) ..................
 
1000    if (lcmo1.le.0) goto 2000
 
        if (local_debug.ne.0) then
           call mmverify()
        endif
 
        call cmo_get_info('d0_node_up',cmo1,ip_d0_node_up,len,ityp,ierr)
        if (ierr.ne.0) goto 9999
        call cmo_get_info('isn1',cmo1,ip_isn1,len,ityp,ierr)
        if (ierr.ne.0) goto 9999
        call cmo_get_info('imt1',cmo1,ip_imt1,len,ityp,ierr)
        if (ierr.ne.0) goto 9999
        call cmo_get_info('itp1',cmo1,ip_itp1,len,ityp,ierr)
        if (ierr.ne.0) goto 9999
 
        call cmo_get_info('icr1',cmo1,ip_icr2,len,ityp,ierr)
        if (ierr.ne.0) goto 9999
        call cmo_get_info('xic',cmo1,ip_xic2,len,ityp,ierr)
        if (ierr.ne.0) goto 9999
        call cmo_get_info('yic',cmo1,ip_yic2,len,ityp,ierr)
        if (ierr.ne.0.and.d0_geom.ge.2) goto 9999
        call cmo_get_info('zic',cmo1,ip_zic2,len,ityp,ierr)
        if (ierr.ne.0.and.d0_geom.ge.3) goto 9999
 
        j=0
        do i=1,nnodes
           if (d0_node_topo(i).ne.0.and.i.eq.iparent(i)) then
              j=j+1
              d0_node_up(j)=i
              node_dn(i)=j
           else
              node_dn(i)=0
           endif
        enddo
        if (j.ne.d1_nnodes) then
           goto 9999
        endif
 
        do i=1,d1_nnodes
           j=d0_node_up(i)
           icr2(i)=icr1(j)
           imt1(i)=0
           isn1(i)=0
           itp1(i)=0
           xic2(i)=xic(j)
           if (d0_geom.gt.1) yic2(i)=yic(j)
           if (d0_geom.gt.2) zic2(i)=zic(j)
        enddo
 
        ! add scalar and node attributes to cmo1, plus the constraint table
        call copyatt_mpary_lg(cmo0,cmo1,'NNODES'
     &                           ,d0_node_up,d1_nnodes,ierr)
        if (ierr.ne.0) goto 9999
        ! but don't print vor and med to gmv (only affects cmo1)
        if (lcmo1.gt.0) then
           cbuf='cmo/setatt/'//cmo1(1:lcmo1)//'/vor2d////no; finish'
           call dotask(cbuf,ierr)
        endif
 
        if (d1_nelements.eq.0.or.d0_topo.le.1) then
           ! mark unselcted points for deletion
           do i=1,d1_nnodes
              j=d0_node_up(i)
              if (d0_node_topo(j).lt.0) itp1(i)=21
           enddo
           goto 2000
        endif
 
        call cmo_get_info('d1_itet',cmo0,ip_d1_itet,len,ityp,ierr)
        if (ierr.ne.0) goto 9999
        call cmo_get_info('d1_itetoff',cmo0,ip_d1_itetoff,len,ityp,ierr)
        if (ierr.ne.0) goto 9999
        call cmo_get_info('d1_jtet',cmo0,ip_d1_jtet,len,ityp,ierr)
        if (ierr.ne.0) goto 9999
        call cmo_get_info('d1_jtetoff',cmo0,ip_d1_jtetoff,len,ityp,ierr)
        if (ierr.ne.0) goto 9999
        call cmo_get_info('d1_itettyp',cmo0,ip_d1_itettyp,len,ityp,ierr)
        if (ierr.ne.0) goto 9999
        call cmo_get_info('d1_itetclr',cmo0,ip_d1_itetclr,len,ityp,ierr)
        if (ierr.ne.0) goto 9999
        call cmo_get_info('d1_elm_d0',cmo0,ip_d1_elm_d0,len,ityp,ierr)
        if (ierr.ne.0) goto 9999
 
        call cmo_get_info('itet',cmo1,ip_itet,len,ityp,ierr)
        if (ierr.ne.0) goto 9999
        call cmo_get_info('itetoff',cmo1,ip_itetoff,len,ityp,ierr)
        if (ierr.ne.0) goto 9999
        call cmo_get_info('jtet',cmo1,ip_jtet,len,ityp,ierr)
        if (ierr.ne.0) goto 9999
        call cmo_get_info('jtetoff',cmo1,ip_jtetoff,len,ityp,ierr)
        if (ierr.ne.0) goto 9999
        call cmo_get_info('itettyp',cmo1,ip_itettyp,len,ityp,ierr)
        if (ierr.ne.0) goto 9999
        call cmo_get_info('itetclr',cmo1,ip_itetclr,len,ityp,ierr)
        if (ierr.ne.0) goto 9999
        call cmo_get_info('d0_elm_up',cmo1,ip_d0_elm_up,len,ityp,ierr)
        if (ierr.ne.0) goto 9999
        if (action(1:lact).eq.'recolor'
     &        .or.action(1:lact).eq.'no_color') then
           call cmo_get_info('d0_clr_up',cmo1,ip_d0_clr_up
     &                       ,len,ityp,ierr)
           if (ierr.ne.0) goto 9999
        endif
 
        ! for now: don't attempt free vs reflective vs virtual distinction ...
        ! note: positions already set above,
        !       defaults also set (but need pointers re even lower d)
 
        nclrs=0
        do iel=1,d1_nelements
           d0_elm_up(iel)=d1_elm_d0(iel)
           ityp=d1_itettyp(iel)
           itettyp(iel)=ityp
           ioff=d1_itetoff(iel)
           itetoff(iel)=ioff
           joff=d1_jtetoff(iel)
           jtetoff(iel)=joff
           iclr=d1_itetclr(iel)
           if (abs(iclr).gt.nclrs) nclrs=abs(iclr)
           itetclr(iel)=abs(iclr)
           if (action(1:lact).eq.'recolor'
     &           .or.action(1:lact).eq.'no_color') then
              d0_clr_up(iel)=abs(iclr)
              if (action(1:lact).eq.'no_color') itetclr(iel)=1
           endif
           if (iclr.lt.0) then
              do ind=1,nelmnen(ityp)
                 i=node_dn(d1_itet(ioff+ind))
                 if (i.eq.0) i=1
                 itet(ioff+ind)=i
              enddo
              do iface=1,nelmnef(ityp)
                 jtet(joff+iface)=d1_jtet(joff+iface)
              enddo
              ! mark element for deletion as clr<0
              itet(ioff+1)=-itet(ioff+1)
           else
              do ind=1,nelmnen(ityp)
                 ! d1_itet refers only to parent nodes so OK
                 i=d1_itet(ioff+ind)
                 i=node_dn(i)
                 if (i.eq.0) stop 'err'
                 itet(ioff+ind)=i
                 ! flag imt for interface faces
                 if (imt1(i).eq.0) then
                    imt1(i)=iclr
                 elseif (imt1(i).gt.0) then
                    imt1(i)=-imt1(i)
                 endif
              enddo
              do iface=1,nelmnef(ityp)
                 jt=d1_jtet(joff+iface)
                 jtet(joff+iface)=jt
                 ! flag imt for external boundary faces
                 if ((jt.eq.mbndry.and.mbndry.gt.0)
     &                    .or.(jt.eq.0.and.mbndry.eq.0)) then
                    do ind=1,ielmface0(iface,ityp)
                       i1=ielmface1(ind,iface,ityp)
                       i=itet(ioff+i1)
                       itp1(i)=10
                    enddo
                 endif
              enddo
           endif
        enddo
        nclrs=max(nclrs,d0_nclrs)
        do i=1,d1_nnodes
           if (imt1(i).eq.0) then
              ! mark elementless points for deletion
              itp1(i)=21
           elseif (imt1(i).lt.0) then
              imt1(i)=nclrs+1
              if (itp1(i).eq.10) then
                 itp1(i)=12
              else
                 itp1(i)=2
              endif
           endif
        enddo
        if (action(1:lact).eq.'recolor') then
           cbuf='colormap/isn/lower_d_map_lg/recolor/'//cmo1(1:lcmo1)
     &            //'; colormap/isn/lower_d_map_lg/release/; finish'
           call dotask(cbuf,ierr)
        elseif (action(1:lact).eq.'no_color') then
           do iel=1,d1_nelements
              ityp=d1_itettyp(iel)
              ioff=d1_jtetoff(iel)
              do iface=1,nelmnef(ityp)
                 jt=d1_jtet(ioff+iface)
                 jtet(ioff+iface)=jt
                 if ((jt.gt.mbndry.and.mbndry.gt.0)
     &                 .or.(jt.lt.0.and.mbndry.eq.0)) then
                    jt=abs(jt)-mbndry
                    jel=1+(jt-1)/d1_nef_cmo
                    jface=jt-(jt-1)*d1_nef_cmo
                    joff=jtetoff(jel)
                    jt=abs(jtet(joff+jface))-mbndry
                    kel=1+(jt-1)/d1_nef_cmo
                    kface=jt-(kel-1)*d1_nef_cmo
                    if (kel.eq.iel.and.kface.eq.iface) then
                       ! jtet_reduce_nnd: should check nnd,dtopo
                       jtet(ioff+iface)=abs(jtet(ioff+iface))-mbndry
                       jtet(joff+jface)=abs(jtet(joff+jface))-mbndry
                    endif
                 endif
              enddo
           enddo
        endif
 
        if (local_debug.ne.0) then
           call mmverify()
        endif
 
c =======================================================================
 
c........ (extract d2 into cmo2) ..................
 
2000    if (lcmo2.le.0) goto 3000
 
        if (local_debug.ne.0) then
           call mmverify()
        endif
 
        call cmo_get_info('d0_node_up',cmo2,ip_d0_node_up,len,ityp,ierr)
        if (ierr.ne.0) goto 9999
        call cmo_get_info('isn1',cmo2,ip_isn1,len,ityp,ierr)
        if (ierr.ne.0) goto 9999
        call cmo_get_info('imt1',cmo2,ip_imt1,len,ityp,ierr)
        if (ierr.ne.0) goto 9999
        call cmo_get_info('itp1',cmo2,ip_itp1,len,ityp,ierr)
        if (ierr.ne.0) goto 9999
 
        call cmo_get_info('icr1',cmo2,ip_icr2,len,ityp,ierr)
        if (ierr.ne.0) goto 9999
        call cmo_get_info('xic',cmo2,ip_xic2,len,ityp,ierr)
        if (ierr.ne.0) goto 9999
        call cmo_get_info('yic',cmo2,ip_yic2,len,ityp,ierr)
        if (ierr.ne.0.and.d0_geom.ge.2) goto 9999
        call cmo_get_info('zic',cmo2,ip_zic2,len,ityp,ierr)
        if (ierr.ne.0.and.d0_geom.ge.3) goto 9999
 
        j=0
        do i=1,nnodes
           if (abs(d0_node_topo(i)).gt.1.and.i.eq.iparent(i)) then
              j=j+1
              d0_node_up(j)=i
              node_dn(i)=j
           else
              node_dn(i)=0
           endif
        enddo
        if (j.ne.d2_nnodes) goto 9999
 
        do i=1,d2_nnodes
           j=d0_node_up(i)
           icr2(i)=icr1(j)
           imt1(i)=0
           isn1(i)=0
           itp1(i)=0
           xic2(i)=xic(j)
           if (d0_geom.gt.1) yic2(i)=yic(j)
           if (d0_geom.gt.2) zic2(i)=zic(j)
        enddo
 
        ! add node attributes to cmo2
        call copyatt_mpary_lg(cmo0,cmo2,'NNODES'
     &                           ,d0_node_up,d2_nnodes,ierr)
        if (ierr.ne.0) goto 9999
 
        if (d2_nelements.eq.0.or.d0_topo.le.2) then
           ! mark unselected points for deletion
           do i=1,d2_nnodes
              j=d0_node_up(i)
              if (d0_node_topo(j).lt.0) itp1(i)=21
           enddo
           goto 3000
        endif
 
        call cmo_get_info('d2_itet',cmo0,ip_d2_itet,len,ityp,ierr)
        if (ierr.ne.0) goto 9999
        call cmo_get_info('d2_itetoff',cmo0,ip_d2_itetoff,len,ityp,ierr)
        if (ierr.ne.0) goto 9999
        call cmo_get_info('d2_jtet',cmo0,ip_d2_jtet,len,ityp,ierr)
        if (ierr.ne.0) goto 9999
        call cmo_get_info('d2_jtetoff',cmo0,ip_d2_jtetoff,len,ityp,ierr)
        if (ierr.ne.0) goto 9999
        call cmo_get_info('d2_itettyp',cmo0,ip_d2_itettyp,len,ityp,ierr)
        if (ierr.ne.0) goto 9999
        call cmo_get_info('d2_itetclr',cmo0,ip_d2_itetclr,len,ityp,ierr)
        if (ierr.ne.0) goto 9999
 
        call cmo_get_info('itet',cmo2,ip_itet,len,ityp,ierr)
        if (ierr.ne.0) goto 9999
        call cmo_get_info('itetoff',cmo2,ip_itetoff,len,ityp,ierr)
        if (ierr.ne.0) goto 9999
        call cmo_get_info('jtet',cmo2,ip_jtet,len,ityp,ierr)
        if (ierr.ne.0) goto 9999
        call cmo_get_info('jtetoff',cmo2,ip_jtetoff,len,ityp,ierr)
        if (ierr.ne.0) goto 9999
        call cmo_get_info('itetclr',cmo2,ip_itetclr,len,ityp,ierr)
        if (ierr.ne.0) goto 9999
        if (action(1:lact).eq.'recolor'
     &        .or.action(1:lact).eq.'no_color') then
           call cmo_get_info('d0_clr_up',cmo2,ip_d0_clr_up
     &                       ,len,ityp,ierr)
           if (ierr.ne.0) goto 9999
        endif
 
        ! d0_elm_up has to be done slightly differently than d1 case
        ! as I think using d2_elm_d0 here is more logical than d2_elm_d1
        call cmo_get_info('itettyp',cmo0,ip_itettyp,len,ityp,ierr)
        if (ierr.ne.0) goto 9999
        call cmo_get_info('d1_elm_d0',cmo0,ip_d1_elm_d0,len,ityp,ierr)
        if (ierr.ne.0) goto 9999
        call cmo_get_info('d1_nef_cmo',cmo0,d1_nef_cmo,len,ityp,ierr)
        if (ierr.ne.0.or.d1_nef_cmo.lt.d0_topo) d1_nelements=0
        call cmo_get_info('d2_elm_d1',cmo0,ip_d2_elm_d1,len,ityp,ierr)
        if (ierr.ne.0) goto 9999
        call cmo_get_info('d0_elm_up',cmo2,ip_d0_elm_up,len,ityp,ierr)
        if (ierr.ne.0) goto 9999
        call create_d2_elm_d0_lower_d_lg(d2_nelements,d1_nef_cmo
     &         ,d0_nef_cmo,d0_nee_cmo,d2_elm_d1,d1_elm_d0,itettyp
     &         ,d0_elm_up,ierr)
        if (ierr.ne.0) goto 9999
 
        ! back to itettyp being for cmo2
        call cmo_get_info('itettyp',cmo2,ip_itettyp,len,ityp,ierr)
        if (ierr.ne.0) goto 9999
 
        ! for now: don't attempt free vs reflective vs virtual distinction ...
        ! note: positions already set above,
        !       defaults also set (but need pointers re even lower d)
 
        nclrs=0
        do iel=1,d2_nelements
           ityp=d2_itettyp(iel)
           itettyp(iel)=ityp
           ioff=d2_itetoff(iel)
           itetoff(iel)=ioff
           joff=d2_jtetoff(iel)
           jtetoff(iel)=joff
           iclr=d2_itetclr(iel)
           if (abs(iclr).gt.nclrs) nclrs=abs(iclr)
           itetclr(iel)=abs(iclr)
           if (action(1:lact).eq.'recolor'
     &           .or.action(1:lact).eq.'no_color') then
              d0_clr_up(iel)=abs(iclr)
              if (action(1:lact).eq.'no_color') itetclr(iel)=1
           endif
           if (iclr.lt.0) then
              do ind=1,nelmnen(ityp)
                 i=node_dn(d2_itet(ioff+ind))
                 if (i.eq.0) i=1
                 itet(ioff+ind)=i
              enddo
              do iface=1,nelmnef(ityp)
                 jtet(joff+iface)=d2_jtet(joff+iface)
              enddo
              ! mark element for deletion as clr<0
              itet(ioff+1)=-itet(ioff+1)
           else
              do ind=1,nelmnen(ityp)
                 ! d2_itet refers only to parent nodes so OK
                 i=d2_itet(ioff+ind)
                 i=node_dn(i)
                 if (i.eq.0) stop 'err'
                 itet(ioff+ind)=i
                 if (imt1(i).eq.0) then
                    imt1(i)=iclr
                 elseif (imt1(i).gt.0) then
                    imt1(i)=-imt1(i)
                 endif
              enddo
              do iface=1,nelmnef(ityp)
                 jt=d2_jtet(joff+iface)
                 jtet(joff+iface)=jt
                 if ((jt.eq.mbndry.and.mbndry.gt.0)
     &                 .or.(jt.eq.0.and.mbndry.eq.0)) then
                    do ind=1,ielmface0(iface,ityp)
                       i1=ielmface1(ind,iface,ityp)
                       i=itet(ioff+i1)
                       itp1(i)=10
                    enddo
                 endif
              enddo
           endif
        enddo
        nclrs=max(nclrs,d0_nclrs)
        do i=1,d2_nnodes
           if (imt1(i).eq.0) then
              ! mark elementless points for deletion
              itp1(i)=21
           elseif (imt1(i).lt.0) then
              imt1(i)=nclrs+1
              if (itp1(i).eq.10) then
                 itp1(i)=12
              else
                 itp1(i)=2
              endif
           endif
        enddo
        if (action(1:lact).eq.'recolor') then
           cbuf='colormap/isn/lower_d_map_lg/recolor/'//cmo2(1:lcmo2)
     &            //'; colormap/isn/lower_d_map_lg/release/; finish'
           call dotask(cbuf,ierr)
        elseif (action(1:lact).eq.'no_color') then
           do iel=1,d2_nelements
              ityp=d2_itettyp(iel)
              ioff=d2_jtetoff(iel)
              do iface=1,nelmnef(ityp)
                 jt=d2_jtet(ioff+iface)
                 jtet(ioff+iface)=jt
                 if ((jt.gt.mbndry.and.mbndry.gt.0)
     &                .or.(jt.lt.0.and.mbndry.eq.0)) then
                    jt=abs(jt)-mbndry
                    jel=1+(jt-1)/d2_nef_cmo
                    jface=jt-(jt-1)*d2_nef_cmo
                    joff=jtetoff(jel)
                    jt=abs(jtet(joff+jface))-mbndry
                    kel=1+(jt-1)/d2_nef_cmo
                    kface=jt-(kel-1)*d2_nef_cmo
                    if (kel.eq.iel.and.kface.eq.iface) then
                       ! jtet_reduce_nnd: should check nnd,topo
                       jtet(ioff+iface)=abs(jtet(ioff+iface))-mbndry
                       jtet(joff+jface)=abs(jtet(joff+jface))-mbndry
                    endif
                 endif
              enddo
           enddo
        endif
 
        if (local_debug.ne.0) then
           call mmverify()
        endif
 
c =======================================================================
 
c........ (extract d3 into cmo3) ..................
 
3000    if (lcmo3.le.0) goto 4000
 
        if (local_debug.ne.0) then
           call mmverify()
        endif
 
        call cmo_get_info('d0_node_up',cmo3,ip_d0_node_up,len,ityp,ierr)
        if (ierr.ne.0) goto 9999
        call cmo_get_info('isn1',cmo3,ip_isn1,len,ityp,ierr)
        if (ierr.ne.0) goto 9999
        call cmo_get_info('imt1',cmo3,ip_imt1,len,ityp,ierr)
        if (ierr.ne.0) goto 9999
        call cmo_get_info('itp1',cmo3,ip_itp1,len,ityp,ierr)
        if (ierr.ne.0) goto 9999
 
        call cmo_get_info('icr1',cmo3,ip_icr2,len,ityp,ierr)
        if (ierr.ne.0) goto 9999
        call cmo_get_info('xic',cmo3,ip_xic2,len,ityp,ierr)
        if (ierr.ne.0) goto 9999
        call cmo_get_info('yic',cmo3,ip_yic2,len,ityp,ierr)
        if (ierr.ne.0.and.d0_geom.ge.2) goto 9999
        call cmo_get_info('zic',cmo3,ip_zic2,len,ityp,ierr)
        if (ierr.ne.0.and.d0_geom.ge.3) goto 9999
 
        j=0
        do i=1,nnodes
           if (abs(d0_node_topo(i)).gt.2.and.i.eq.iparent(i)) then
              j=j+1
              d0_node_up(j)=i
              node_dn(i)=j
           else
              node_dn(i)=0
           endif
        enddo
        if (j.ne.d3_nnodes) goto 9999
 
        do i=1,d3_nnodes
           j=d0_node_up(i)
           icr2(i)=icr1(j)
           imt1(i)=d0_nclrs+1
           isn1(i)=0
           if (d0_node_topo(j).lt.0) then
              ! mark unselected points for deletion
             itp1(i)=21
           else
             itp1(i)=0
           endif
           xic2(i)=xic(j)
           if (d0_geom.gt.1) yic2(i)=yic(j)
           if (d0_geom.gt.2) zic2(i)=zic(j)
        enddo
 
        ! add node attributes to cmo3
        call copyatt_mpary_lg(cmo0,cmo3,'NNODES'
     &                           ,d0_node_up,d3_nnodes,ierr)
        if (ierr.ne.0) goto 9999
 
        if (local_debug.ne.0) then
           call mmverify()
        endif
 
c =======================================================================
 
c........ (sucessful return) ..................
4000    ierror=0
        call mmrelprt(isubname,ierr)
        if (lcmo1.gt.0) then
            cbuf='cmo/select/'//cmo1(1:lcmo1)//'; rmpoint element'
     &           //'; rmpoint compress; settets color_points; finish'
            call dotask(cbuf,ierr)
        endif
        if (lcmo2.gt.0) then
            cbuf='cmo/select/'//cmo2(1:lcmo2)//'; rmpoint element'
     &           //'; rmpoint compress; settets color_points; finish'
            call dotask(cbuf,ierr)
        endif
        if (lcmo3.gt.0) then
            cbuf='cmo/select/'//cmo3(1:lcmo3)
     &           //'; rmpoint compress; finish'
            call dotask(cbuf,ierr)
        endif
        cbuf='cmo/select/'//cmo_save(1:icharlnf(cmo_save))//'; finish'
        call dotask(cbuf,ierr)
        if (local_debug.gt.0) then
           write(*,*) 'finished extract_lower_d_lg succesfully'
           call mmverify()
           write(cbuf,*)'cmo status; finish'
           call dotask(cbuf,ierr)
        endif
        return
 
c........ (failure return) ..................
9999    ierror=1
        call mmrelprt(isubname,ierr)
        if (local_debug.gt.0) then
           write(*,*) 'finished extract_lower_d_lg unsuccesfully'
           call mmverify()
           stop
        endif
        cbuf='ERROR IN ROUTINE extract_lower_d_lg: ABORTING'
        call writloga('default',0,cbuf,0,ierr)
        cbuf='cmo/select/'//cmo_save(1:icharlnf(cmo_save))//'; finish'
        call dotask(cbuf,ierr)
        return
c .....................................................
        end
 
c-----------------------------------------------------------------------
C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|
c-----------------------------------------------------------------------
C #####################################################################
C   filter_lower_d_lg
C
C   PURPOSE -
C
C      filter the lower d elements by setting d1_itetclr/d2_itetclr/d3_itetclr
C      to negative if they are not to be considered, or to positive if they are,
C      and update d0_node_topo, d1_nnodes, d2_nnodes, d3_nnodes, to reflect this.
C
C   INPUT ARGUMENTS -
C
C        cmo_in        - the cmo to filter
C        input_action1 - the field to filter
C        input_value   - the value to filter on
C        input_action2 - the and/or flag to use with the filter
C
C   OUTPUT ARGUMENTS -
C
C        ierror - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C   USAGE NOTES / CAVEATS / CHANGES -
C
C    it is probably not advisable to call this routine outside this
C    lower_d command suite.
C
C less clear notes to myself for code development .......................
 
c lower_d/filter[/cmo0|*-def-]/[icr|itp|imt|clr]/#
c lower_d/filter[/cmo0|*-def-]/[no_icr|no_itp|no_imt|no_clr]/#
c lower_d/filter[/cmo0|*-def-]/[ext|int|vrt|real|no_int|no_vrt|no_real]
c    set lower_d element itetclr to negative if don't exist with this criterion,
c lower_d/filter[/cmo0|*-def-]/reset
c    reset lower_d element itetclr to all positive
 
c also reset d0_node_topo to reflect new topology
c note jtet is NOT reset -> filter on positive itetclr (and d0_node_topo) for usage
c Q re d0_node_topo set to <0 vs d0_node_topo from new topology:
c    for now, reset...
c  for eg smooth, compare d0_node_topo with effective one set from elements...
 
c this is used, eg, by smooth to distinguish surfaces with no damage
c and d0_node_topo rather than jtet is used to distinguish topology class of node
 
c except for reset, multiple calls to this routine treated as "or"
c (current itetclr left alone if negative,
c   positive reset to negative if fail criterion)
c
c not the icr,itp,clr filters are not smart enough to
c keep lower d children of kept higher d
c -> the "no_" versions are safer...
 
c note re "or" default - add coding later to have "and|or|new"
c appended as modifier to action -> "or" and "new" trivial,
c for "and" would have to save old itetclr, redo as here,
c "and" the two itetclr signs, then reset d0_node_topo...
c right now, input_action2 is not used, but eventually will be the "and/or"...
 
c is this "jtet_reduce_nnd" safe?
 
c test that set d0_nnodes right for mixed-d case, also new endpts with filter
 
C #####################################################################
 
        subroutine filter_lower_d_lg(cmo_in,input_action1
     &                     ,input_value,input_action2,ierror)
 
c ........................................................................
 
 
        implicit none
        include 'chydro.h'
        include 'local_element.h'

C arguments
        character*(*) cmo_in,input_action1,input_action2
        integer input_value,ierror,ivalue

C variables 
 
        pointer (ip_itetclr,itetclr),(ip_clrtab,clrtab)
     &         ,(ip_clroff,clroff),(ip_d0_node_topo,d0_node_topo)
     &         ,(ip_itetoff,itetoff),(ip_itettyp,itettyp)
     &         ,(ip_itet,itet),(ip_jtet,jtet),(ip_jtetoff,jtetoff)
     &         ,(ip_iparent,iparent),(ip_itp1,itp1),(ip_isn1,isn1)
     &         ,(ip_imt1,imt1),(ip_icr1,icr1),(ip_filters,filters)
        integer itetclr(*),clrtab(*),clroff(*),d0_node_topo(*)
     &         ,itetoff(*),itettyp(*),iparent(*),itp1(*),isn1(*)
     &         ,itet(*),jtet(*),jtetoff(*),imt1(*),icr1(*),filters(*)
 
        integer laction,local_debug,len,ityp,ierr,nelts,iel
     &         ,mbndry,d_topo,iclr,d0_nnodes,i,j,ind,ioff,d0_nclrs
     &         ,iface,d1_nelements,d2_nelements,d0_nelements
     &         ,nef_cmo,jel,jt,jclr,pttyp,linact,iaction,d0_nfilters
     &         ,d1_nnodes,d2_nnodes,d3_nnodes,last_action
     &         ,d1_nelts,d2_nelts,lact2
 

        integer icharlnf 

C       coding caution: rankfilter in filter_lower_d_lg
C       must be consistent with rankfilter in control_lower_d_lg
        integer nactions,rankfilter
        parameter(nactions=17,rankfilter=3)
        character*32 transl_action(nactions)
 
        character*32 isubname,action,action2,cmo

c -----------------------------------------------------
C BEGIN begin
C
        local_debug=0
        isubname='filter_lower_d_lg'
 
        if (local_debug.gt.0) then
           write(*,*) 'filter action 1 :',input_action1
           write(*,*) 'filter action 2 :',input_action2
           write(*,*) 'filter value: ',input_value
        endif
 
c ...   !! set up the translate table
        transl_action(1)='reset'
        transl_action(2)='ext'
        transl_action(3)='int'
        transl_action(4)='vrt'
        transl_action(5)='real'
        transl_action(6)='no_ext'
        transl_action(7)='no_int'
        transl_action(8)='no_vrt'
        transl_action(9)='no_real'
        transl_action(10)='imt'
        transl_action(11)='icr'
        transl_action(12)='itp'
        transl_action(13)='clr'
        transl_action(14)='no_imt'
        transl_action(15)='no_icr'
        transl_action(16)='no_itp'
        transl_action(17)='no_clr'
 
c ...   !! get the cmo
        cmo=cmo_in
        if (icharlnf(cmo).lt.1.or.cmo(1:1).eq.'-') then
           call cmo_get_name(cmo,ierr)
        endif
 
c ...   !! make action icr1 equiv to icr, etc
        laction=icharlnf(input_action1)
        if (laction.le.1.or.laction.gt.32) then
          action='reset'
          laction=5
        else
          if (input_action1(laction:laction).eq."1") laction=laction-1
          action=input_action1(1:laction)
        endif
        lact2=icharlnf(input_action2)
        if (lact2.le.0.or.lact2.gt.32) then
           action2='or'
           lact2=2
        else
           action2=input_action2(1:lact2)
        endif
 
        ivalue=input_value
 
c ...   !! save the current filter into the filters table
        !! (unless resetting or refiltering)
 
        linact=laction
 
        call cmo_get_info('d0_nfilters',cmo,d0_nfilters
     &                    ,len,ityp,ierr)
        if (ierr.ne.0) goto 9999
        if (local_debug.gt.0) then
           write(*,*) 'filter d0_nfilters before ',d0_nfilters
        endif
        if (action(1:laction).eq.'refilter') then
           if (d0_nfilters.ge.rankfilter) then
              call cmo_get_info('d0_filters',cmo,ip_filters
     &                    ,len,ityp,ierr)
              if (ierr.ne.0) goto 9999
              last_action=d0_nfilters/rankfilter
           endif
        else
           last_action=1
           len=1
           ityp=1
           if (action(1:laction).eq.'reset'
     &          .or.action2(1:lact2).eq.'new') then
              d0_nfilters=0
              call cmo_set_info('d0_nfilters',cmo,d0_nfilters
     &                          ,len,ityp,ierr)
           else
              d0_nfilters=d0_nfilters+rankfilter
              call cmo_set_info('d0_nfilters',cmo,d0_nfilters
     &                          ,len,ityp,ierr)
              if (ierr.ne.0) goto 9999
              call mmnewlen('d0_filters',cmo,ip_filters
     &                                    ,d0_nfilters,ierr)
              if (ierr.ne.0) goto 9999
              call cmo_get_info('d0_filters',cmo,ip_filters
     &                           ,len,ityp,ierr)
              if (ierr.ne.0) goto 9999
              filters(d0_nfilters-rankfilter+2)=ivalue
              filters(d0_nfilters-rankfilter+1)=1
              if (action2(1:lact2).eq.'new') then
                 filters(d0_nfilters-rankfilter+3)=2  ! should never happen ...
              elseif (action2(1:lact2).eq.'and') then
                 filters(d0_nfilters-rankfilter+3)=1
              else ! if (action2(1:lact2).eq.'or') then
                 filters(d0_nfilters-rankfilter+3)=0
              endif
              do i=2,nactions
                 len=icharlnf(transl_action(i))
                 if (action(1:laction).eq.transl_action(i)(1:len)) then
                    filters(d0_nfilters-rankfilter+1)=i
                    goto 222
                 endif
              enddo
222           continue
           endif
        endif
        if (local_debug.gt.0) then
           write(*,*) 'filter d0_nfilters after ',d0_nfilters
        endif
 
c ...   !! get the node info .................
 
        call cmo_get_info('d0_nclrs',cmo,d0_nclrs,len,ityp,ierr)
        if (ierr.ne.0.or.d0_nclrs.le.0) goto 9999
        call cmo_get_info('d0_clrtab',cmo,ip_clrtab,len,ityp,ierr)
        if (ierr.ne.0) goto 9999
        call cmo_get_info('d0_clroff',cmo,ip_clroff,len,ityp,ierr)
        if (ierr.ne.0) goto 9999
 
        if (local_debug.gt.0) then
           write(*,*) 'd0_nclrs=',d0_nclrs
           call print_clrtab_lower_d_lg(d0_nclrs,clroff,clrtab)
        endif
 
        call cmo_get_info('mbndry',cmo,mbndry,len,ityp,ierr)
        if (ierr.ne.0) mbndry=0
 
        call cmo_get_info('nnodes',cmo,d0_nnodes,len,ityp,ierr)
        if (ierr.ne.0.or.d0_nnodes.le.0) goto 9999
        call cmo_get_info('nelements',cmo,d0_nelements,len,ityp,ierr)
        if (ierr.ne.0.or.d0_nelements.le.0) goto 9999
        call cmo_get_info('d0_node_topo',cmo,ip_d0_node_topo
     &                    ,len,ityp,ierr)
        if (ierr.ne.0) goto 9999
        call cmo_get_info('itp1',cmo,ip_itp1,len,ityp,ierr)
        if (ierr.ne.0) goto 9999
        call cmo_get_info('isn1',cmo,ip_isn1,len,ityp,ierr)
        if (ierr.ne.0) goto 9999
        call cmo_get_info('imt1',cmo,ip_imt1,len,ityp,ierr)
        if (ierr.ne.0) goto 9999
        call cmo_get_info('icr1',cmo,ip_icr1,len,ityp,ierr)
        if (ierr.ne.0) goto 9999
 
        call mmggetbk('iparent',isubname,ip_iparent,d0_nnodes,1,ierr)
        call unpackpc(d0_nnodes,itp1,isn1,iparent)
 
        if (local_debug.gt.0) then
           write(*,*) 'd0_node_topo before'
           write(*,*) (d0_node_topo(i),i=1,d0_nnodes)
        endif
 
c ===================================================
        do iaction=1,last_action
 
        if (input_action1(1:linact).eq.'refilter') then
            j=(iaction-1)*rankfilter
            action=transl_action(filters(j+1))
            ivalue=filters(j+2)
            laction=icharlnf(action)
            j=filters(j+3)
            if (j.eq.2) then
               action2='new'  ! should never happen ...
            elseif (j.eq.1) then
               action2='and'
            else ! if (j.eq.0) then
               action2='or'
            endif
            lact2=icharlnf(action2)
        endif
 
        do i=1,d0_nnodes
           d0_node_topo(i)=0   ! -abs(d0_node_topo(i))
        enddo
 
c -----------------------------------------------------
        do d_topo=1,2
 
c ...      !! get the current d element info
 
           if (d_topo.eq.1) then
              call cmo_get_info('d1_nelements',cmo,nelts,len,ityp,ierr)
              if (ierr.ne.0.or.nelts.le.0) then
                 d1_nelements=0
                 d2_nelements=0
                 d1_nelts=d1_nelements
                 d2_nelts=d2_nelements
                 nelts=d0_nelements
                 goto 8000
              else
                 d1_nelements=nelts
                 d1_nelts=d1_nelements
              endif
              call cmo_get_info('d1_itetclr',cmo,ip_itetclr
     &                          ,len,ityp,ierr)
              if (ierr.ne.0) goto 9999
              call cmo_get_info('d1_itetoff',cmo,ip_itetoff
     &                          ,len,ityp,ierr)
              if (ierr.ne.0) goto 9999
              call cmo_get_info('d1_jtetoff',cmo,ip_jtetoff
     &                          ,len,ityp,ierr)
              if (ierr.ne.0) goto 9999
              call cmo_get_info('d1_itettyp',cmo,ip_itettyp
     &                          ,len,ityp,ierr)
              if (ierr.ne.0) goto 9999
              call cmo_get_info('d1_itet',cmo,ip_itet,len,ityp,ierr)
              if (ierr.ne.0) goto 9999
              call cmo_get_info('d1_jtet',cmo,ip_jtet,len,ityp,ierr)
              if (ierr.ne.0) goto 9999
              call cmo_get_info('d1_nef_cmo',cmo,nef_cmo,len,ityp,ierr)
              if (ierr.ne.0) goto 9999
           else ! if (d_topo.eq.2) then
              call cmo_get_info('d2_nelements',cmo,nelts,len,ityp,ierr)
              if (ierr.ne.0.or.nelts.le.0) then
                 d2_nelements=0
                 d2_nelts=d2_nelements
                 nelts=d1_nelements
                 goto 8000
              else
                 d2_nelements=nelts
                 d2_nelts=d2_nelements
              endif
              call cmo_get_info('d2_itetclr',cmo,ip_itetclr
     &                          ,len,ityp,ierr)
              if (ierr.ne.0) goto 9999
              call cmo_get_info('d2_itetoff',cmo,ip_itetoff
     &                          ,len,ityp,ierr)
              if (ierr.ne.0) goto 9999
              call cmo_get_info('d2_jtetoff',cmo,ip_jtetoff
     &                          ,len,ityp,ierr)
              if (ierr.ne.0) goto 9999
              call cmo_get_info('d2_itettyp',cmo,ip_itettyp
     &                          ,len,ityp,ierr)
              if (ierr.ne.0) goto 9999
              call cmo_get_info('d2_itet',cmo,ip_itet,len,ityp,ierr)
              if (ierr.ne.0) goto 9999
              call cmo_get_info('d2_jtet',cmo,ip_jtet,len,ityp,ierr)
              if (ierr.ne.0) goto 9999
              call cmo_get_info('d2_nef_cmo',cmo,nef_cmo,len,ityp,ierr)
              if (ierr.ne.0) goto 9999
           endif
 
           if (local_debug.gt.0) then
              write(*,*) 'itetclr before for d_topo = ',d_topo
              write(*,*) (itetclr(iel),iel=1,nelts)
           endif
 
           if (action2(1:lact2).eq.'new') then
              do iel=1,nelts
                 itetclr(iel)=abs(itetclr(iel))
              enddo
           endif
 
c ...      !! only d2 elements with valid d1 parents are allowed
           if (d_topo.eq.2) then
              do iel=1,nelts
                 iclr=itetclr(iel)
                 if (iclr.gt.0) then
                    ityp=itettyp(iel)
                    ioff=itetoff(iel)
                    do ind=1,nelmnen(ityp)
                       i=iparent(itet(ioff+ind))
                       if (d0_node_topo(i).ne.d_topo-1) then
                          itetclr(iel)=-iclr
                          goto 300
                       endif
                    enddo
300                 continue
                 endif
              enddo
           endif
 
c ...      !! perform the desired action ............
           !! -- modify itetclr and d0_node_topo
 
c       .........................................
           if (action(1:laction).eq.'no_ext'
     &              .or.action(1:laction).eq.'int') then
 
              do iel=1,nelts
                 iclr=itetclr(iel)
                 if (iclr.gt.0.and.iclr.le.d0_nclrs) then
                    ityp=clrtab(clroff(iclr)+1)
                    if (ityp.ne.ifitpint.and.ityp.ne.ifitpini
     &                    .and.ityp.ne.ifitpvrt.and.ityp.ne.ifitpvin
     &                    ) itetclr(iel)=-iclr
                 endif
              enddo
 
c       .........................................
           elseif (action(1:laction).eq.'ext'
     &                 .or.action(1:laction).eq.'no_int') then
 
              do iel=1,nelts
                 iclr=itetclr(iel)
                 if (iclr.gt.0) then
                    if (iclr.le.d0_nclrs) ityp=clrtab(clroff(iclr)+1)
                    if (iclr.gt.d0_nclrs
     &                    .or.ityp.eq.ifitpint.or.ityp.eq.ifitpini
     &                    .or.ityp.eq.ifitpvrt.or.ityp.eq.ifitpvin
     &                    ) itetclr(iel)=-iclr
                 endif
              enddo
 
c       .........................................
           elseif (action(1:laction).eq.'vrt'
     &              .or.action(1:laction).eq.'no_real') then
 
              do iel=1,nelts
                 iclr=itetclr(iel)
                 if (iclr.gt.0.and.iclr.le.d0_nclrs) then
                    ityp=clrtab(clroff(iclr)+1)
                    if (ityp.ne.ifitpvrt.and.ityp.ne.ifitpvrb
     &                    .and.ityp.ne.ifitpvin.and.ityp.ne.ifitpvfb
     &                    .and.ityp.ne.ifitpvrf.and.ityp.ne.ifitpvif
     &                    .and.ityp.ne.ifitpvir.and.ityp.ne.ifitpalb
     &                    ) itetclr(iel)=-iclr
                 endif
              enddo
 
c       .........................................
           elseif (action(1:laction).eq.'no_vrt'
     &              .or.action(1:laction).eq.'real') then
 
              do iel=1,nelts
                 iclr=itetclr(iel)
                 if (iclr.gt.0) then
                    ityp=clrtab(clroff(iclr)+1)
                    if (iclr.le.d0_nclrs) ityp=clrtab(clroff(iclr)+1)
                    if (iclr.gt.d0_nclrs
     &                    .or.ityp.eq.ifitpvin.or.ityp.eq.ifitpvfb
     &                    .or.ityp.eq.ifitpvrf.or.ityp.eq.ifitpvif
     &                    .or.ityp.eq.ifitpvir.or.ityp.eq.ifitpalb
     &                    .or.ityp.eq.ifitpvrb
     &                    ) itetclr(iel)=-iclr
                 endif
              enddo
 
c       .........................................
           elseif (action(1:laction).eq.'clr'
     &              .or.action(1:laction).eq.'no_clr') then
 
              ! this removes bndry of kept lower_d ->
              ! "use caution" (or fix to know about this...)
              do iel=1,nelts
                 iclr=itetclr(iel)
                 if (iclr.ge.0) then
                    if (iclr.ne.ivalue
     &                    .and.action(1:laction).eq.'clr') then
                       itetclr(iel)=-iclr
                    elseif (iclr.eq.ivalue
     &                    .and.action(1:laction).eq.'no_clr') then
                       itetclr(iel)=-iclr
                    endif
                 endif
              enddo
 
c       .........................................
           elseif (action(1:laction).eq.'icr'
     &              .or.action(1:laction).eq.'no_icr'
     &              .or.action(1:laction).eq.'itp'
     &              .or.action(1:laction).eq.'no_itp') then
 
              if (action(1:laction).eq.'icr'.or.
     &             action(1:laction).eq.'no_icr') then
                 ioff=2
              else
                 ioff=1
              endif
              do iel=1,nelts
                 iclr=itetclr(iel)
                 if (iclr.gt.d0_nclrs
     &                   .and.action(1:3).ne.'no_') then
                    itetclr(iel)=-iclr
                 elseif (iclr.gt.0.and.iclr.le.d0_nclrs) then
                    ityp=clrtab(clroff(iclr)+ioff)
                    if (ityp.eq.ivalue
     &                    .and.action(1:3).eq.'no_') then
                       itetclr(iel)=-iclr
                    elseif (ityp.ne.ivalue
     &                    .and.action(1:3).ne.'no_') then
                       itetclr(iel)=-iclr
                    endif
                 endif
              enddo
 
c       .........................................
           elseif (action(1:laction).eq.'imt'
     &              .or.action(1:laction).eq.'no_imt') then
 
              do iel=1,nelts
                 iclr=itetclr(iel)
                 if (iclr.gt.d0_nclrs
     &                   .and.action(1:3).ne.'no_') then
                    itetclr(iel)=-iclr
                 elseif (iclr.gt.0.and.iclr.le.d0_nclrs) then
                    do i=1,clrtab(clroff(iclr)+3)
                       if (clrtab(clroff(iclr)+3+i).eq.ivalue) then
                          ityp=1
                          goto 600
                       endif
                    enddo
                    ityp=0
600                 if (ityp.eq.1.and.action(1:3).eq.'no_') then
                          itetclr(iel)=-iclr
                    elseif (ityp.eq.0.and.action(1:3).ne.'no_') then
                          itetclr(iel)=-iclr
                    endif
                 endif
              enddo
 
c       .........................................
           else ! if (action(1:laction).eq.'reset') then
 
              ! any other action interpreted as reset
 
              if (local_debug.gt.0
     &             .and.action(1:laction).ne.'reset') then
                write(*,*) 'add filter lower_d '//action(1:laction)
                write(*,*) 'using reset as default'
              endif
 
              do iel=1,nelts
                 itetclr(iel)=abs(itetclr(iel))
              enddo
 
              if (action(1:laction).eq.'reset') then
                 len=1
                 ityp=1
                 d0_nfilters=0
                 call cmo_set_info('d0_nfilters',cmo,d0_nfilters
     &                          ,len,ityp,ierr)
              elseif (d_topo.eq.1
     &              .and.input_action1(1:linact).ne.'refilter') then
                 call writloga('default',0,
     &            'caution: no such filter so reset',0,ierr)
                 call writloga('default',0,
     &      '- use lower_d/filter/-cmo-/refilter/1 to restore previous'
     &            ,0,ierr)
              endif
 
c       .........................................
           endif
 
           if (local_debug.gt.0) then
              write(*,*) 'itetclr after for d_topo = ',d_topo
              write(*,*) (itetclr(iel),iel=1,nelts)
           endif
 
c ...      !! fix d0_node_topo for this d_topo .........
 
           len=0
           do iel=1,nelts
              iclr=itetclr(iel)
              if (iclr.ge.0) then  ! vs gt.0 -> but =0 should never happen?
                 len=len+1
                 ityp=itettyp(iel)
                 ioff=itetoff(iel)
                 do ind=1,nelmnen(ityp)
                    i=iparent(itet(ioff+ind))
                    d0_node_topo(i)=d_topo
                 enddo
              endif
           enddo
           if (d_topo.eq.1) then
              d1_nelts=len
           else
              d2_nelts=len
           endif
 
           if (local_debug.gt.0) then
              write(*,*) 'd0_node_topo after for d_topo = ',d_topo
              write(*,*) (d0_node_topo(i),i=1,d0_nnodes)
           endif
 
c -----------------------------------------------------
        enddo
 
8000    continue
 
c ...   !! get the element info corresponding to 1-up from the the points
        if (d1_nelements.eq.0) then
           d_topo=1
           call cmo_get_info('itetclr',cmo,ip_itetclr,len,ityp,ierr)
           if (ierr.ne.0) goto 9999
           call cmo_get_info('itetoff',cmo,ip_itetoff,len,ityp,ierr)
           if (ierr.ne.0) goto 9999
           call cmo_get_info('jtetoff',cmo,ip_jtetoff,len,ityp,ierr)
           if (ierr.ne.0) goto 9999
           call cmo_get_info('itettyp',cmo,ip_itettyp,len,ityp,ierr)
           if (ierr.ne.0) goto 9999
           call cmo_get_info('itet',cmo,ip_itet,len,ityp,ierr)
           if (ierr.ne.0) goto 9999
           call cmo_get_info('jtet',cmo,ip_jtet,len,ityp,ierr)
           if (ierr.ne.0) goto 9999
           call cmo_get_info('faces_per_element',cmo,nef_cmo
     &                       ,len,ityp,ierr)
           if (ierr.ne.0) goto 9999
        elseif (d2_nelements.eq.0) then
           d_topo=2
        else
           d_topo=3
        endif
 
c ...   !! fix for d0_node_topo for points ...............
 
        do iel=1,nelts
          iclr=itetclr(iel)
          ityp=itettyp(iel)
          if (iclr.ge.0) then
            ioff=jtetoff(iel)
            do iface=1,nelmnef(ityp)
              jt=jtet(ioff+iface)
              if ((jt.lt.mbndry.and.mbndry.gt.0)
     &              .or.(jt.gt.0.and.mbndry.eq.0)) then
                 jel=1+(jt-1)/nef_cmo
                 jclr=itetclr(jel)
              endif
              if ((jt.ge.mbndry.and.mbndry.gt.0)
     &              .or.(jt.le.0.and.mbndry.eq.0)
     &             .or.jclr.lt.0) then
                ! should really only get  here if ityp=ifelmlin ...
                ! WHY??
                do ind=1,ielmface0(iface,ityp)
                   i=ielmface1(ind,iface,ityp)
                   i=iparent(itet(ioff+i))
                   pttyp=isn1(i)
                   if (pttyp.ne.0) then
                      pttyp=itp1(pttyp)
                   else
                      pttyp=itp1(i)
                   endif
                   if (d0_node_topo(i).ne.d_topo-1) goto 700
                   if (action(1:laction).eq.'no_icr') then
                      if (icr1(i).eq.ivalue) goto 700
                   elseif (action(1:laction).eq.'icr') then
                      if (icr1(i).ne.ivalue) goto 700
                   elseif (action(1:laction).eq.'no_itp') then
                      if (pttyp.eq.ivalue) goto 700
                   elseif (action(1:laction).eq.'itp') then
                      if (pttyp.ne.ivalue) goto 700
                   elseif (action(1:laction).eq.'no_clr') then
                      if (ivalue.gt.d0_nclrs) goto 700
                   elseif (action(1:laction).eq.'clr') then
                      if (ivalue.le.d0_nclrs) goto 700
                   elseif (action(1:laction).eq.'no_imt'
     &                    .or.action(1:laction).eq.'imt') then
                      pttyp=0
                      j=isn1(i)
                      if (j.eq.0) then
                         if (imt1(i).eq.ivalue) pttyp=1
                      else
                         do while (j.ne.i)
                            if (imt1(i).eq.ivalue) then
                               pttyp=1
                               goto 650
                            endif
                            j=isn1(j)
                         enddo
                      endif
650                   if (action(1:laction).eq.'no_imt'
     &                                    .and.pttyp.eq.1) goto 700
                      if (action(1:laction).eq.'imt'
     &                                    .and.pttyp.eq.0) goto 700
                   elseif (action(1:laction).eq.'no_vrt'
     &                    .or.action(1:laction).eq.'real') then
                      if (pttyp.eq.ifitpvrt.or.pttyp.eq.ifitpvrb
     &                    .or.pttyp.eq.ifitpvin.or.pttyp.eq.ifitpvfb
     &                    .or.pttyp.eq.ifitpvrf.or.pttyp.eq.ifitpvif
     &                    .or.pttyp.eq.ifitpvir.or.pttyp.eq.ifitpalb
     &                    ) goto 700
                   elseif (action(1:laction).eq.'no_real'
     &                    .or.action(1:laction).eq.'vrt') then
                      if (pttyp.ne.ifitpvrt.and.pttyp.ne.ifitpvrb
     &                    .and.pttyp.ne.ifitpvin.and.pttyp.ne.ifitpvfb
     &                    .and.pttyp.ne.ifitpvrf.and.pttyp.ne.ifitpvif
     &                    .and.pttyp.ne.ifitpvir.and.pttyp.ne.ifitpalb
     &                    ) goto 700
                   elseif (action(1:laction).eq.'no_int'
     &                    .or.action(1:laction).eq.'ext') then
                      if (pttyp.eq.ifitpint.or.pttyp.eq.ifitpini
     &                   .or.pttyp.eq.ifitpvrt.or.pttyp.eq.ifitpvin
     &                   ) goto 700
                   elseif (action(1:laction).eq.'no_ext'
     &                    .or.action(1:laction).eq.'int') then
                      if (pttyp.ne.ifitpint.and.pttyp.ne.ifitpini
     &                   .and.pttyp.ne.ifitpvrt.and.pttyp.ne.ifitpvin
     &                   ) goto 700
                   endif
                   d0_node_topo(i)=d_topo
700                continue
                enddo
              endif
            enddo
          endif
        enddo
 
        if (local_debug.gt.0) then
           write(*,*) 'd0_node_topo after for d_topo = ',d_topo
           write(*,*) (d0_node_topo(i),i=1,d0_nnodes)
        endif
 
        d1_nnodes=0
        d2_nnodes=0
        d3_nnodes=0
        do i=1,d0_nnodes
           ityp=d0_node_topo(i)
           if (ityp.gt.0.and.i.eq.iparent(i)) then
             if (ityp.ge.1) d1_nnodes=d1_nnodes+1
             if (ityp.ge.2) d2_nnodes=d2_nnodes+1
             if (ityp.ge.3) d3_nnodes=d3_nnodes+1
             j=isn1(i)
             do while (j.ne.i.and.j.ne.0)
                d0_node_topo(j)=-d0_node_topo(i)
                j=isn1(j)
             enddo
           endif
        enddo
 
        if (local_debug.gt.0) then
           write(*,*) 'd0_node_topo after isn'
           write(*,*) (d0_node_topo(i),i=1,d0_nnodes)
           write(*,*) 'nnodes:',d0_nnodes,d1_nnodes,d2_nnodes,d3_nnodes
        endif
 
        call cmo_get_info('d1_nnodes',cmo,i,len,ityp,ierr)
        if (ierr.eq.0) then
           ityp=1
           len=1
           call cmo_set_info('d1_nnodes',cmo,d1_nnodes,len,ityp,ierr)
        endif
 
        call cmo_get_info('d2_nnodes',cmo,i,len,ityp,ierr)
        if (ierr.eq.0) then
           ityp=1
           len=1
           call cmo_set_info('d2_nnodes',cmo,d2_nnodes,len,ityp,ierr)
        endif
 
        call cmo_get_info('d3_nnodes',cmo,i,len,ityp,ierr)
        if (ierr.eq.0) then
           ityp=1
           len=1
           call cmo_set_info('d3_nnodes',cmo,d3_nnodes,len,ityp,ierr)
        endif
 
c ===================================================
        enddo ! last_action
 
        write(*,*)
     &    'total d0_nelements,d1_nelements,d2_nelements='
     &             ,d0_nelements,d1_nelements,d2_nelements
        write(*,*) ' valid d0_nnodes,d1_nnodes,d2_nnodes,d3_nnodes='
     &             ,d0_nnodes,d1_nnodes,d2_nnodes,d3_nnodes
        write(*,*)
     &    'valid d1_nelements,d2_nelements=',d1_nelts,d2_nelts
 
c -----------------------------------------------------
9000    continue
        ierror=0
        return
 
c -----------------------------------------------------
9999    continue
        ierror=1
        if (local_debug.gt.0) stop
        call writloga('default',0,'ERROR in filter_lower_d_lg',0,ierr)
        return
        end
 
c-----------------------------------------------------------------------
C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|
c-----------------------------------------------------------------------
C #####################################################################
C   itet_lower_d_lg
C
C   PURPOSE -
C
C      Set itettyp,itetoff,jtetoff,itet for the lower d structures.
C
C   INPUT ARGUMENTS -
C
C        nelements,d0_nef_cmo
C        iparent,itettyp,itetoff,itet,d0_elm_d1,jtetoff
C        d1_nelements,d1_nef_cmo
C
C   OUTPUT ARGUMENTS -
C
C        d1_elm_d0,d1_itettyp,d1_itetoff,d1_itet,d1_jtetoff
C        ierror - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C   USAGE NOTES / CAVEATS / CHANGES -
C
C    it is probably not advisable to call this routine outside this
C    lower_d command suite.
C
C #####################################################################
 
        subroutine itet_lower_d_lg(
     &       nelements,d0_nef_cmo
     &      ,iparent,itettyp,itetoff,itet,d0_elm_d1,jtetoff
     &      ,d1_nelements,d1_nef_cmo
     &      ,d1_elm_d0,d1_itettyp,d1_itetoff,d1_itet,d1_jtetoff
     &      ,ierror)
 
c ........................................................................
 
        implicit none
        include 'local_element.h'
 
        integer  nelements,d0_nef_cmo,ierror
 
        integer  iparent(*),itettyp(*),itetoff(*),itet(*)
     &          ,d0_elm_d1(*),jtetoff(*)
 
        integer  d1_nelements,d1_nef_cmo
 
        integer  d1_elm_d0(*),d1_itettyp(*)
     &          ,d1_itetoff(*),d1_itet(*),d1_jtetoff(*)
 
        integer iel,jel,iface,jface,ioff,joff,ityp,jtyp
     &         ,ioffsum,joffsum,jt,ind,nnd,i,ierr
 
        character*132 cbuf
 
        integer local_debug
 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        local_debug=0
 
c........ (check if anything to do) ..................
 
        if (d1_nelements.eq.0) goto 1000
 
c........ (assign d1 element translation info) ..................
c d0_elm_d1 is input -> set d1_elm_d0
 
        do iel=1,nelements
           ioff=jtetoff(iel)
           ityp=itettyp(iel)
           do iface=1,nelmnef(ityp)
              ioff=ioff+1
              jel=d0_elm_d1(ioff)
              if (jel.gt.0.and.jel.le.d1_nelements) then
                 d1_elm_d0(jel)=(iel-1)*d0_nef_cmo+iface
              elseif (jel.lt.-d1_nelements.or.jel.gt.d1_nelements) then
                 goto 9999
              endif
           enddo
        enddo
 
        ! check
        do iel=1,d1_nelements
           jt=d1_elm_d0(iel)
           if (jt.gt.nelements*d0_nef_cmo.or.jt.lt.1) goto 9999
           jel=1+(jt-1)/d0_nef_cmo
           jface=jt-(jel-1)*d0_nef_cmo
           if (d0_elm_d1(jtetoff(jel)+jface).ne.iel) goto 9999
        enddo
 
c........ (assign d1 element info) ..................
c d1_itettyp, d1_itetoff, d1_jtetoff, d1_itet
 
        ioffsum=0
        joffsum=0
        do iel=1,d1_nelements
           jt=d1_elm_d0(iel)
           jel=1+(jt-1)/d0_nef_cmo
           jface=jt-(jel-1)*d0_nef_cmo
           joff=itetoff(jel)
           jtyp=itettyp(jel)
           ityp=ielmface3(jface,jtyp)
           nnd=nelmnen(ityp)
           !! set d1_itettyp, d1_itetoff, d1_jtetoff ......
           d1_itettyp(iel)=ityp
           d1_itetoff(iel)=ioffsum
           d1_jtetoff(iel)=joffsum
           !! set d1_itet ......
           do ind=1,nnd
              i=iparent(itet(joff+ielmface1(ind,jface,jtyp)))
              d1_itet(ioffsum+ind)=i
           enddo
           ioffsum=ioffsum+nnd
           joffsum=joffsum+nelmnef(ityp)
        enddo
 
c =======================================================================
 
c........ (sucessful return) ..................
1000    ierror=0
        !(release in lower_d_control)! call mmrelprt(isubname,ierr)
        if (local_debug.gt.0) call mmverify()
        return
 
c........ (failure return) ..................
9999    ierror=1
        !(release in lower_d_control)! call mmrelprt(isubname,ierr)
        if (local_debug.gt.0) call mmverify()
        cbuf='ERROR IN ROUTINE itet_lower_d_lg: ABORTING'
        call writloga('default',0,cbuf,0,ierr)
        if (local_debug.gt.0) stop
        return
c .....................................................
        end
 
c-----------------------------------------------------------------------
C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|
c-----------------------------------------------------------------------
C #####################################################################
C   itetclr_lower_d_lg
C
C   PURPOSE -
C
C      set the itetclr for the lower d elements,
C      incrementing the color table as necessary.
C
C   INPUT ARGUMENTS -
C
C        cmo,d_topo,nconbnd,mbndry
C        d0_nelements,d0_nef_cmo
C        icontab,itp1,icr1,isn1,iparent
C        d0_itetclr,d0_jtetoff,d0_jtet
C        d1_nelements,d1_nef_cmo
C        d1_itettyp,d1_itetoff,d1_itet,d1_elm_d0
C
C   OUTPUT ARGUMENTS -
C
C        d1_itetclr
C        ierror - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C   USAGE NOTES / CAVEATS / CHANGES -
C
C    it is probably not advisable to call this routine outside this
C    lower_d command suite.
C
C    Note: if an icr1 value is not in the icontab table,
C    then treated as icr1="minimum constraint".
C
C    see notes for create_lower_d_lg for interior_icr_flag
C
 
C less clear notes to myself for code development .......................
 
c set itetclr to packed itp,icr,{imt}
c d_topo is RELATIVE dimension, not current
 
! pass input to avoid all those "gets"
! pass output?? or just let user get?
!     &         ,d0_nclrs,ip_d0_clroff,ip_d0_clrtab
 
c dang: jtet_reduce_nnd:
c ought to pack dimensionality into the itetclr....
c and should the "diff # nnd" connections get a "vrt"??
 
c if icontab exists and find lower_d icr1 with no corresponding entry,
c should increment icontab,ncontab instead of using mincon...
 
C #####################################################################
 
        subroutine itetclr_lower_d_lg(cmo
     &          ,d_topo,nconbnd,mbndry
     &          ,jtet_cycle_max,jtet_reduce_nnd
     &          ,interior_icr_flag,ibtype,geom_name
     &          ,d0_nelements,d0_nef_cmo
     &          ,icontab,itp1,icr1,isn1,iparent ! iparent only needed for debug...
     &          ,d0_itetclr,d0_jtetoff,d0_jtet
     &          ,d1_nelements,d1_nef_cmo
     &          ,d1_itettyp,d1_itetclr,d1_itetoff,d1_itet,d1_elm_d0
     &          ,ierror)
 
c ........................................................................
 
        implicit none
 
        include 'local_element.h'
        include 'chydro.h'
 
        character*(*) cmo
 
        integer  d_topo,nconbnd,mbndry,ierror
     &           ,jtet_cycle_max,jtet_reduce_nnd
     &           ,interior_icr_flag
 
        character*(*) ibtype(*),geom_name ! need re testing if virtual constraint
 
        integer  icontab(50,*),itp1(*),icr1(*),isn1(*),iparent(*)
 
        integer  d0_nelements,d0_nef_cmo
 
        integer  d0_jtet(*),d0_jtetoff(*)
     &          ,d0_itetclr(*)
 
        integer  d1_nelements,d1_nef_cmo
 
        integer  d1_itet(*),d1_itetoff(*)
     &          ,d1_elm_d0(*),d1_itettyp(*),d1_itetclr(*)
 
        integer  d0_nclrs,d0_clrlen
        pointer  (ip_srfs,srfs),(ip_d0_clrtab,d0_clrtab)
     &          ,(ip_d0_clroff,d0_clroff)
        integer  srfs(*),d0_clrtab(*),d0_clroff(*)
 
        integer iel,jel,kel,jface,kface,ioff,joff
     &          ,ityp,jtyp,ktyp,jt,kt,ieltyp,ielcon
     &          ,i,j,len,ierr,ind,iclr,nsrfs,mxsrfs
     &          ,nnd,minicr,maxicr,ncon,mincon,icon,jclr,kclr
     &          ,k,d0_nclrs_att,d0_clrlen_att,icycle
     &          ,loc_icr(maxnee1),loc_itp(maxnee1),isrf
     &          ,loc_par(maxnee1)
 
        character*132 cbuf
        character*32 isubname2
 
        integer local_debug,missing_icr1
 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
 
        isubname2='tmp2_lower_d_lg'
        local_debug=0
        missing_icr1=0
      d0_nclrs_att=0
      d0_clrlen_att=0
 
        if (jtet_reduce_nnd.eq.1) then
          cbuf='WARNING: itetclr_lower_d_lg not jtet_reduce_nnd=1 safe'
          call writloga('default',0,cbuf,0,ierr)
        endif
 
c........ (get info, initialize table if d_topo=1) ..................
 
        ! check if anything to do
        if (d0_nelements.le.0.or.d1_nelements.le.0
     &        .or.d_topo.lt.1.or.d_topo.gt.2) goto 9999
 
        ! get color table info
        call cmo_get_info('d0_nclrs',cmo,d0_nclrs,len,ityp,ierr)
        if (ierr.ne.0) goto 9999
        call cmo_get_info('d0_clrlen',cmo,d0_clrlen,len,ityp,ierr)
        if (ierr.ne.0) goto 9999
        if (d_topo.eq.1) then
           ! don't set to zero in case want to save over re-creates.
           ! should be OK as set to zero when created...
           if (d0_nclrs.lt.0) goto 9999  ! d0_nclrs=0
           if (d0_clrlen.lt.0) goto 9999 ! d0_clrlen=0
        else
           if (d0_nclrs.lt.0) goto 9999
           if (d0_clrlen.lt.0) goto 9999
        endif
 
        ! increase clr lengths by default amount (will be corrected later)
        nsrfs=min(100,d1_nelements)
        call cmo_get_info('d0_clrtab',cmo,ip_d0_clrtab,d0_clrlen_att,
     *                ityp,ierr)
        if (ierr.ne.0) goto 9999
        call cmo_get_info('d0_clroff',cmo,ip_d0_clroff,d0_nclrs_att,
     *                ityp,ierr)
        if (ierr.ne.0) goto 9999
        d0_nclrs_att=d0_nclrs_att+nsrfs
        d0_clrlen_att=d0_clrlen_att+50*nsrfs
        call mmnewlen('d0_clroff',cmo,ip_d0_clroff,d0_nclrs_att,ierr)
        if (ierr.ne.0) goto 9999
        call mmnewlen('d0_clrtab',cmo,ip_d0_clrtab,d0_clrlen_att,ierr)
        if (ierr.ne.0) goto 9999
 
c........ (assign d1 element info) ..................
c d1_itetclr
 
        ityp=1
        mxsrfs=500
        call mmggetbk('srfs',isubname2,ip_srfs,mxsrfs,ityp,ierr)
 
        do iel=1,d1_nelements
 
           d1_itetclr(iel)=0
 
           ityp=d1_itettyp(iel)
           nnd=nelmnen(ityp)
           ioff=d1_itetoff(iel)
           do ind=1,nnd
              i=d1_itet(ioff+ind)
              j=itp1(i)
              ! fix
              if (isn1(i).ne.0.and.j.eq.ifitpcup) i=isn1(i)
              loc_icr(ind)=icr1(i)
              loc_itp(ind)=itp1(i)
              loc_par(ind)=iparent(i)
           enddo
           if (local_debug.gt.9) then
              write(*,*) 'iel',iel,'--------------'
              write(*,*) 'icr',(loc_icr(ind),ind=1,nnd)
              write(*,*) 'itp',(loc_itp(ind),ind=1,nnd)
              write(*,*) 'par',(loc_par(ind),ind=1,nnd)
           endif
 
           jt=d1_elm_d0(iel)
           jel=1+(jt-1)/d0_nef_cmo
           jface=jt-(jel-1)*d0_nef_cmo
           joff=d0_jtetoff(jel)
           jt=d0_jtet(joff+jface)
           jclr=d0_itetclr(jel)
           if (jclr.gt.0.and.jclr.le.d0_nclrs) then
              jtyp=d0_clrtab(d0_clroff(jclr)+1)
           else
              jtyp=0
           endif
 
           !! find ieltyp......
           ieltyp=0
           if (d_topo.eq.1) then
              ! only possible types are ini,rfl,fre,vrt
              if ((jt.gt.mbndry.and.mbndry.gt.0)
     &            .or.(jt.lt.0.and.mbndry.eq.0)) then
                 ! must be ini
                 ieltyp=ifitpini
              elseif ((jt.lt.mbndry.and.mbndry.gt.0)
     &            .or.(jt.gt.0.and.mbndry.eq.0)) then
                 ! must be vrt
                 ieltyp=ifitpvrt
              else
                 ! must be fre or rfl
                 ieltyp=ifitprfl
                 do ind=1,nnd
                    i=loc_itp(ind)
                    if ( i.eq.ifitpfre.or.i.eq.ifitpvfb
     &                     .or.i.eq.ifitpifb.or.i.eq.ifitpvif
     &                 ) ieltyp=ifitpfre
                 enddo
              endif
           else ! if (d_topo.eq.2) then
              if ((jt.lt.mbndry.and.mbndry.gt.0)
     &            .or.(jt.gt.0.and.mbndry.eq.0)) then
                 ! WHY WAS THIS ".le.mbndry" in old version??
                 ! must be vrt giving fake bndry
                 ieltyp=ifitpvrt
              else
                 ! above types ini,rfl,fre,vrt
                 !  plus vin,vrb,vfb,vrf,vir,vif,alb
                 !      ,rfb,irb,ifb,irf now possible
                 ! cycle around parent edge rather than setting from nodes
                 ! since node info may be misleading re "interior" edges
                 do i=1,4
                    srfs(i)=0
                 enddo
                 if (jtyp.eq.ifitpfre) then
                    srfs(1)=1
                 elseif (jtyp.eq.ifitprfl) then
                    srfs(2)=1
                 elseif (jtyp.eq.ifitpvrt) then
                    srfs(3)=1
                 elseif (jtyp.eq.ifitpini) then
                    srfs(4)=1
                 endif
                 kt=abs(jt)-mbndry
                 kel=1+(kt-1)/d0_nef_cmo
                 kface=kt-(kel-1)*d0_nef_cmo
                 icycle=0
                 do while ((kel.ne.jel.or.kface.ne.jface)
     &                 .and.kt.gt.0
     &                 .and.icycle.lt.jtet_cycle_max)
                    icycle=icycle+1
                    kclr=d0_itetclr(kel)
                    if (kclr.le.d0_nclrs.and.kclr.gt.0) then
                       ktyp=d0_clrtab(d0_clroff(kclr)+1)
                       if (ktyp.eq.ifitpfre) then
                          srfs(1)=1
                       elseif (ktyp.eq.ifitprfl) then
                          srfs(2)=1
                       elseif (ktyp.eq.ifitpvrt) then
                          srfs(3)=1
                       elseif (ktyp.eq.ifitpini) then
                          srfs(4)=1
                       endif
                    endif
                    kt=d0_jtet(d0_jtetoff(kel)+kface)
                    kt=abs(kt)-mbndry
                    kel=1+(kt-1)/d0_nef_cmo
                    kface=kt-(kel-1)*d0_nef_cmo
                 enddo
                 if (kt.eq.0) srfs(3)=0
                 len=srfs(1)+srfs(2)+srfs(3)+srfs(4)
                 if (len.eq.4) then
                    ieltyp=ifitpalb
                 elseif (len.eq.0) then
                    ieltyp=0
                 elseif (len.eq.3.and.srfs(1).eq.0) then
                    ieltyp=ifitpvir
                 elseif (len.eq.3.and.srfs(2).eq.0) then
                    ieltyp=ifitpvif
                 elseif (len.eq.3.and.srfs(3).eq.0) then
                    ieltyp=ifitpirf
                 elseif (len.eq.3.and.srfs(4).eq.0) then
                    ieltyp=ifitpvrf
                 elseif (len.eq.1.and.srfs(1).eq.1) then
                    ieltyp=ifitpfre
                 elseif (len.eq.1.and.srfs(2).eq.1) then
                    ieltyp=ifitprfl
                 elseif (len.eq.1.and.srfs(3).eq.1) then
                    ieltyp=ifitpvrt
                 elseif (len.eq.1.and.srfs(4).eq.1) then
                    ieltyp=ifitpini
                 elseif (srfs(1).eq.1.and.srfs(2).eq.1) then
                    ieltyp=ifitprfb
                 elseif (srfs(1).eq.1.and.srfs(3).eq.1) then
                    ieltyp=ifitpvfb
                 elseif (srfs(1).eq.1.and.srfs(4).eq.1) then
                    ieltyp=ifitpifb
                 elseif (srfs(2).eq.1.and.srfs(3).eq.1) then
                    ieltyp=ifitpvrb
                 elseif (srfs(2).eq.1.and.srfs(4).eq.1) then
                    ieltyp=ifitpirb
                 elseif (srfs(3).eq.1.and.srfs(4).eq.1) then
                    ieltyp=ifitpvin
                 endif
              endif
           endif
 
           !! find constraint ......
 
c first "correct" virtual ieltyp's if interior_icr_flag value prohibits virtual...
           if (interior_icr_flag.eq.1.or.interior_icr_flag.eq.3
     &     .or.interior_icr_flag.eq.5.or.interior_icr_flag.eq.7) then
           endif
 
c first "correct" ieltyp and/or loc_icr/ielcon based on interior_icr_flag value
C     = 0 - all constrained surface types can exist
C     = 1 - no virtual constrained surfaces exist
C     = 2 - no intrcons constrained surfaces exist
C     = 3 - no virtual or intrcons constrained surfaces exist
C     = 4 - no reflect constrained surfaces exist
C     = 5 - no reflect or virtual constrained surfaces exist
C     = 6 - no reflect or intrcons constrained surfaces exist
C     = 7 - no constrained surfaces exist
 
           if (interior_icr_flag.ne.0) then
              if (   interior_icr_flag.eq.1.or.interior_icr_flag.eq.3
     &           .or.interior_icr_flag.eq.5.or.interior_icr_flag.eq.7
     &                ) then
                 ! "correct" virtual ieltyp's as no virtual allowed
                 if (ieltyp.eq.ifitpvin) then
                    ieltyp=ifitpini
                 elseif (ieltyp.eq.ifitpvrb) then
                    ieltyp=ifitprfl
                 elseif (ieltyp.eq.ifitpvfb) then
                    ieltyp=ifitpfre
                    do ind=1,nnd
                       loc_icr(ind)=0
                    enddo
                 elseif (ieltyp.eq.ifitpvrf) then
                    ieltyp=ifitprfb
                 elseif (ieltyp.eq.ifitpvir) then
                    ieltyp=ifitpirb
                 elseif (ieltyp.eq.ifitpvif) then
                    ieltyp=ifitpifb
                 elseif (ieltyp.eq.ifitpalb) then
                    ieltyp=ifitpirf
                 elseif (ieltyp.eq.ifitpvrt) then
                    do ind=1,nnd
                       loc_icr(ind)=0
                    enddo
                    if (local_debug.ne.0) stop 'err'
                    goto 9999
                 endif
              endif
              if (interior_icr_flag.eq.7) then
                 ! no constrained interfaces exist as far as lower_d is concerned
                 do ind=1,nnd
                    loc_icr(ind)=0
                 enddo
              elseif (interior_icr_flag.eq.2
     &                  .or.interior_icr_flag.eq.3) then
                 ! intrcons surfaces are not allowed
                 if (ieltyp.eq.ifitpini.or.ieltyp.eq.ifitpifb) then
                    do ind=1,nnd
                       loc_icr(ind)=0
                    enddo
                 endif
              elseif (interior_icr_flag.eq.4
     &                  .or.interior_icr_flag.eq.5) then
                 ! reflect surfaces are not allowed
                 if (ieltyp.eq.ifitprfl.or.ieltyp.eq.ifitprfb) then
                    do ind=1,nnd
                       loc_icr(ind)=0
                    enddo
                 endif
              elseif (interior_icr_flag.eq.6) then
                 ! intrcons surfaces are not allowed
                 ! and reflect surfaces are not allowed
                 if (ieltyp.eq.ifitpini.or.ieltyp.eq.ifitpifb
     &           .or.ieltyp.eq.ifitprfl.or.ieltyp.eq.ifitprfb
     &           .or.ieltyp.eq.ifitpirb.or.ieltyp.eq.ifitpirf) then
                    do ind=1,nnd
                       loc_icr(ind)=0
                    enddo
                 endif
              endif
           endif
 
c now continue using the "interior_icr_flag corrected" loc_icr and ieltyp
 
           if (nnd.gt.0) then
              minicr=loc_icr(1)
           else
              minicr=0
           endif
           maxicr=minicr
           ncon=-50
           do ind=1,nnd
              if (loc_icr(ind).lt.minicr) minicr=loc_icr(ind)
              if (loc_icr(ind).gt.maxicr) maxicr=loc_icr(ind)
              if (nconbnd.gt.0.and.loc_icr(ind).gt.0) then
                 if (icontab(1,loc_icr(ind)).lt.ncon
     &                        .or.ncon.lt.0) then
                    mincon=loc_icr(ind)
                    ncon=icontab(1,mincon)
                    if (ncon.le.0) minicr=0
                 endif
              endif
           enddo
 
           ! define surface constraint
           ! use smallest as best guess when icontab does not exist
           if (ieltyp.eq.ifitpfre) then
              ! free surfaces can't be constrained
              ielcon=0
           elseif (minicr.eq.0.or.nconbnd.eq.0.or.ncon.le.0
     &                .or.(minicr.eq.maxicr.and.geom_name(1:6)
     &                     .eq.'-none-'.and.interior_icr_flag.eq.0)
     &                ) then
              ielcon=minicr
           else
              ! find set of constraints common to all nodes
              ! and use this for surface
              ! (must be more than 1 ind or would not be here)
              nsrfs=0
              do icon=1,ncon
                 isrf=icontab(2+icon,mincon)
                 if (geom_name(1:6).ne.'-none-') then
                    ! ------------------
                    ! test that constraint agrees with ieltyp
                    ! ------------------
                    if (ibtype(isrf).eq.'virtual'
     &                 .and.ieltyp.ne.ifitpvrt.and.ieltyp.ne.ifitpvin
     &                 .and.ieltyp.ne.ifitpvrb.and.ieltyp.ne.ifitpvfb
     &                 .and.ieltyp.ne.ifitpvrf.and.ieltyp.ne.ifitpvir
     &                 .and.ieltyp.ne.ifitpvif.and.ieltyp.ne.ifitpalb
     &                     ) then
                       goto 111
                    elseif (ibtype(isrf).eq.'intrcons'
     &                 .and.ieltyp.ne.ifitpini.and.ieltyp.ne.ifitpvin
     &                 .and.ieltyp.ne.ifitpvir.and.ieltyp.ne.ifitpvif
     &                 .and.ieltyp.ne.ifitpalb.and.ieltyp.ne.ifitpirb
     &                 .and.ieltyp.ne.ifitpifb.and.ieltyp.ne.ifitpirf
     &                     ) then
                       goto 111
                    elseif (ibtype(isrf).eq.'reflect'
     &                 .and.ieltyp.ne.ifitprfl.and.ieltyp.ne.ifitpvrb
     &                 .and.ieltyp.ne.ifitpvrf.and.ieltyp.ne.ifitpvir
     &                 .and.ieltyp.ne.ifitpalb.and.ieltyp.ne.ifitprfb
     &                 .and.ieltyp.ne.ifitpirb.and.ieltyp.ne.ifitpirf
     &                     ) then
                       goto 111
                    elseif (ibtype(isrf).eq.'intrface'
     7                      .or.ibtype(isrf).eq.'free') then
                       if (local_debug.gt.0)
     &                      stop 'error: no icr for intrface or free?'
                       goto 9999
                    endif
                    ! ------------------
                 endif
                 do ind=1,nnd
                    if (loc_icr(ind).eq.mincon) goto 110
                    do i=1,icontab(1,loc_icr(ind))
                       if (icontab(2+i,loc_icr(ind)).eq.isrf)
     &                              goto 110
                    enddo
                    goto 111
110                 continue
                 enddo
                 nsrfs=nsrfs+1
                 srfs(nsrfs)=isrf
111              continue
              enddo
              if (nsrfs.eq.0) then
                 ielcon=0
              elseif (nsrfs.eq.ncon) then
                 ielcon=mincon
              else
                 ! find corresp constraint
                 ielcon=0
                 do icon=1,nconbnd
                    if (icontab(1,icon).eq.nsrfs) then
                       do i=1,nsrfs
                          isrf=srfs(i)
                          do j=1,icontab(1,icon)
                             if (icontab(2+j,icon).eq.isrf) goto 112
                          enddo
                          goto 113
112                       continue
                       enddo
                       ielcon=icon
                       goto 114
                    endif
113                 continue
                 enddo
                 ! if get here, icr1 entry not in table
                 ! report error vs update nconbbnd,icontab...
                 ! for now: use mincon even though incorrect ...
                 ielcon=mincon
                 missing_icr1=missing_icr1+1
                 if (local_debug.gt.0) then
                    write(*,*)
     &               'warning: these surfs have no corresp icr1'
                 endif
114              continue
              endif
           endif
 
           !! have ieltyp and ielcon, now find imt's
           if (d_topo.eq.1) then
              srfs(1)=d0_itetclr(jel)
              nsrfs=1
              kt=abs(jt)
              if (kt.ge.mbndry.and.mbndry.gt.0) kt=kt-mbndry
              kel=1+(kt-1)/d0_nef_cmo
              kface=kt-(kel-1)*d0_nef_cmo
              icycle=0
              do while ((kel.ne.jel.or.kface.ne.jface)
     &                 .and.kt.gt.0
     &                 .and.icycle.lt.jtet_cycle_max)
                 icycle=icycle+1
                 iclr=d0_itetclr(kel)
                 do j=1,nsrfs
                    if (srfs(j).eq.iclr) goto 443
                 enddo
                 if (nsrfs.eq.mxsrfs) then
                    call mmincblk('srfs',isubname2,ip_srfs,100,ierr)
                    mxsrfs=mxsrfs+100
                 endif
                 nsrfs=nsrfs+1
                 srfs(nsrfs)=iclr
443              continue
                 kt=abs(d0_jtet(d0_jtetoff(kel)+kface))
                 if (kt.ge.mbndry.and.mbndry.gt.0) kt=kt-mbndry
                 kel=1+(kt-1)/d0_nef_cmo
                 kface=kt-(kel-1)*d0_nef_cmo
              enddo
           else
              iclr=d0_itetclr(jel)
              nsrfs=0
              if (iclr.gt.0.and.iclr.lt.d0_nclrs) then
                 ioff=d0_clroff(iclr)
                 nsrfs=d0_clrtab(ioff+3)
                 do i=1,nsrfs
                    srfs(i)=d0_clrtab(ioff+3+i)
                 enddo
              endif
              kt=abs(jt)
              if (kt.ge.mbndry.and.mbndry.gt.0) kt=kt-mbndry
              kel=1+(kt-1)/d0_nef_cmo
              kface=kt-(kel-1)*d0_nef_cmo
              icycle=0
              do while ((kel.ne.jel.or.kface.ne.jface)
     &                 .and.kt.gt.0
     &                 .and.icycle.lt.jtet_cycle_max)
                 icycle=icycle+1
                 iclr=d0_itetclr(kel)
                 if (iclr.gt.0.and.iclr.lt.d0_nclrs) then
                    ioff=d0_clroff(iclr)
                    nnd=d0_clrtab(ioff+3)
                    do i=1,nnd
                       iclr=d0_clrtab(ioff+3+i)
                       do j=1,nsrfs
                          if (srfs(j).eq.iclr) goto 444
                       enddo
                       if (nsrfs.eq.mxsrfs) then
                          call mmincblk('srfs',isubname2
     &                                   ,ip_srfs,100,ierr)
                          mxsrfs=mxsrfs+100
                       endif
                       nsrfs=nsrfs+1
                       srfs(nsrfs)=iclr
444                    continue
                    enddo
                 endif
                 kt=abs(d0_jtet(d0_jtetoff(kel)+kface))
                 if (kt.ge.mbndry.and.mbndry.gt.0) kt=kt-mbndry
                 kel=1+(kt-1)/d0_nef_cmo
                 kface=kt-(kel-1)*d0_nef_cmo
              enddo
           endif
           ! order srfs
445        i=0
           do j=2,nsrfs
              if (srfs(j).lt.srfs(j-1)) then
                 k=srfs(j)
                 srfs(j)=srfs(j-1)
                 srfs(j-1)=k
                 i=1
              endif
           enddo
           if (i.ne.0) goto 445
 
           !! set d1_itetclr
           !! see if previously existing color
           do i=1,d0_nclrs
              ioff=d0_clroff(i)
              ityp=d0_clrtab(ioff+1)
              icon=d0_clrtab(ioff+2)
              nnd=d0_clrtab(ioff+3)
              if (ityp.eq.ieltyp.and.icon.eq.ielcon
     &           .and.nnd.eq.nsrfs) then
                 do j=1,nsrfs
                     if (srfs(j).ne.d0_clrtab(ioff+3+j)) goto 446
                 enddo
                 d1_itetclr(iel)=i
                 goto 447
              endif
446           continue
           enddo
           !! if not, increment table.....
           if (d0_nclrs.ge.d0_nclrs_att) then
              d0_nclrs_att=d0_nclrs+100
              call mmnewlen('d0_clroff',cmo,ip_d0_clroff
     &                      ,d0_nclrs_att,ierr)
              if (ierr.ne.0) goto 9999
           endif
           if (d0_clrlen+3+nsrfs.gt.d0_clrlen_att) then
              d0_clrlen_att=d0_clrlen+5000
              call mmnewlen('d0_clrtab',cmo,ip_d0_clrtab
     &                      ,d0_clrlen_att,ierr)
              if (ierr.ne.0) goto 9999
           endif
           d0_nclrs=d0_nclrs+1
           d1_itetclr(iel)=d0_nclrs
           d0_clroff(d0_nclrs)=d0_clrlen
           d0_clrtab(d0_clrlen+1)=ieltyp
           d0_clrtab(d0_clrlen+2)=ielcon
           d0_clrtab(d0_clrlen+3)=nsrfs
           do i=1,nsrfs
              d0_clrtab(d0_clrlen+3+i)=srfs(i)
           enddo
           d0_clrlen=d0_clrlen+3+nsrfs
 
 447       continue
 
           if (local_debug.gt.9) then
              write(*,'(a,3i7)') 'jel,jface,jclr',jel,jface,jclr
              kt=abs(jt)
              if (kt.ge.mbndry.and.mbndry.gt.0) kt=kt-mbndry
              kel=1+(kt-1)/d0_nef_cmo
              kface=kt-(kel-1)*d0_nef_cmo
              icycle=0
              do while ((kel.ne.jel.or.kface.ne.jface)
     &                 .and.kt.gt.0
     &                 .and.icycle.lt.jtet_cycle_max)
                 icycle=icycle+1
                 kclr=d0_itetclr(kel)
                 write(*,'(a,3i7)') 'kel,kface,kclr',kel,kface,kclr
                 kt=abs(d0_jtet(d0_jtetoff(kel)+kface))
                 if (kt.ge.mbndry.and.mbndry.gt.0) kt=kt-mbndry
                 kel=1+(kt-1)/d0_nef_cmo
                 kface=kt-(kel-1)*d0_nef_cmo
              enddo
              call print_clrtab_lower_d_lg(d0_nclrs,d0_clroff,d0_clrtab)
              write(*,*) 'itetclr chosen is ',d1_itetclr(iel)
           endif
 
 
        enddo
 
        if (local_debug.gt.0) then
           call print_clrtab_lower_d_lg(d0_nclrs,d0_clroff,d0_clrtab)
           j=min(d1_nelements,100)
           if (local_debug.gt.9) j=d1_nelements
           write(*,*) (d1_itetclr(i),i=1,j)
        endif
 
        ! reset d0_nclrs to current length
        len=1
        ityp=1
        call cmo_set_info('d0_nclrs',cmo,d0_nclrs,len,ityp,ierr)
        if (ierr.ne.0) goto 9999
        call cmo_set_info('d0_clrlen',cmo,d0_clrlen,len,ityp,ierr)
        if (ierr.ne.0) goto 9999
        call mmnewlen('d0_clroff',cmo,ip_d0_clroff,d0_nclrs,ierr)
        if (ierr.ne.0) goto 9999
        call mmnewlen('d0_clrtab',cmo,ip_d0_clrtab,d0_clrlen,ierr)
        if (ierr.ne.0) goto 9999
 
        ! do this way to avoid messing up other pointers.
        ! actually, nothing should be changed but these 2 since recently got pointers.
        ! Re-get on return anyway to be safe ??
        ! call cmo_newlen(cmo,ierr)
        ! for now, "live dangerously"...
 
c .....................................................
        ! write(*,*)'d1_nnodes,d1_nelements',d1_nnodes,d1_nelements
 
c =======================================================================
 
c........ (sucessful return) ..................
1000    ierror=0
 
        if (missing_icr1.gt.0) then
           write(cbuf,*) 'lower_d warning: ',missing_icr1
     &            ,d_topo,'-elements with icr1 missing from icontab'
           call writloga('default',0,cbuf,0,ierr)
        endif
        if (local_debug.gt.0) then
           call print_clrtab_lower_d_lg(d0_nclrs,d0_clroff,d0_clrtab)
        endif
        if (local_debug.gt.1) stop
 
        !(release in lower_d_control)! call mmrelprt(isubname2,ierr)
        if (local_debug.gt.0) call mmverify()
        return
 
c........ (failure return) ..................
9999    ierror=1
        !(release in lower_d_control)! call mmrelprt(isubname2,ierr)
        if (local_debug.gt.0) call mmverify()
        cbuf='ERROR IN ROUTINE itetclr_lower_d_lg: ABORTING'
        call writloga('default',0,cbuf,0,ierr)
        if (local_debug.gt.0) stop
        return
c .....................................................
        end
 
c-----------------------------------------------------------------------
C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|
c-----------------------------------------------------------------------
C #####################################################################
C   release_lower_d_lg
C
C   PURPOSE -
C
C      release storage associated with lower d data structures
C      in the specified mesh, and reset lower_d_flag to indicate no
C      lower d structures desired.
C
C   INPUT ARGUMENTS -
C
C        cmo - the mesh object to release the lower d structures in
C
C   OUTPUT ARGUMENTS -
C
C        ierror - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C   USAGE NOTES / CAVEATS / CHANGES -
C
C #####################################################################
 
        subroutine release_lower_d_lg(cmo,ierror)
 
c ........................................................................
 
        implicit none
 
        character*(*) cmo
        integer ierror
 
        integer local_debug,lower_d_flag,len
     &         ,lcmo,i,ityp,ierr,d0_topo
 
        character*200 cbuf
 
        character*32 cmsgout(4)
        real*8 xmsgout(4)
        integer imsgout(4),nwdsout,tmsgout(4)
 
        integer icharlnf
        external icharlnf
 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
 
        local_debug=0
        if (local_debug.gt.0) call mmverify()
 
        lcmo=icharlnf(cmo)
        call cmo_exist(cmo(1:lcmo),ierr)
        if (ierr.ne.0) goto 9999
        call cmo_get_info('ndimensions_topo',cmo,d0_topo
     &           ,len,ityp,ierr)
        if (ierr.eq.0) d0_topo=3
 
        nwdsout=4
        do i=1,4
           xmsgout(i)=0.d0
           imsgout(i)=0
           tmsgout(i)=3
           cmsgout(i)=' '
        enddo
        cmsgout(1)='cmo'
        cmsgout(2)='DELATT'
        cmsgout(3)=cmo
        if (cmo(1:1).eq.'-') then
           call cmo_get_name(cmsgout(3),ierr)
        endif
 
c .................................................................
 
c  lower_d_flag ...............
c       ! just set back to flag to indicate no lower d structures
        call cmo_get_info('lower_d_flag',cmo,lower_d_flag
     &           ,len,ityp,ierr)
        if (ierr.eq.0) then
           len=1
           ityp=1
           lower_d_flag=0
           call cmo_set_info('lower_d_flag',cmo,lower_d_flag
     &           ,len,ityp,ierr)
        endif
 
c .................................................................
c  delete d0 vector attribute d0_node_topo
 
        cmsgout(4)='d0_node_topo'
        call cmo_delatt_all_lg(imsgout,xmsgout,cmsgout,tmsgout
     &                        ,nwdsout,ierr)
 
c .................................................................
c delete scalar attributes and all attributes with them as length or rank
c   d0_nclrs,d0_clrlen,d0_nfilters
c   d1_nnodes,d1_nelements,d1_nen_cmo,d1_nef_cmo,d1_nee_cmo,d1_jtet_cycle_max
c   d2_nnodes,d2_nelements,d2_nen_cmo,d2_nef_cmo,d2_nee_cmo,d2_jtet_cycle_max
c   d3_nnodes,d3_nelements,d3_nen_cmo,d3_nef_cmo,d3_nee_cmo
 
        cmsgout(4)='d1_nnodes'
        call cmo_delatt_all_lg(imsgout,xmsgout,cmsgout,tmsgout
     &                         ,nwdsout,ierr)
 
        if (d0_topo.le.1) goto 100
        cmsgout(4)='d1_nelements'
        call cmo_delatt_all_lg(imsgout,xmsgout,cmsgout,tmsgout
     &                         ,nwdsout,ierr)
        cmsgout(4)='d1_nen_cmo'
        call cmo_delatt_all_lg(imsgout,xmsgout,cmsgout,tmsgout
     &                         ,nwdsout,ierr)
        cmsgout(4)='d1_nef_cmo'
        call cmo_delatt_all_lg(imsgout,xmsgout,cmsgout,tmsgout
     &                         ,nwdsout,ierr)
        cmsgout(4)='d1_nee_cmo'
        call cmo_delatt_all_lg(imsgout,xmsgout,cmsgout,tmsgout
     &                         ,nwdsout,ierr)
        cmsgout(4)='d1_jtet_cycle_max'
        call cmo_delatt_all_lg(imsgout,xmsgout,cmsgout,tmsgout
     &                         ,nwdsout,ierr)
 
        cmsgout(4)='d2_nnodes'
        call cmo_delatt_all_lg(imsgout,xmsgout,cmsgout,tmsgout
     &                         ,nwdsout,ierr)
 
        if (d0_topo.le.2) goto 100
        cmsgout(4)='d2_nelements'
        call cmo_delatt_all_lg(imsgout,xmsgout,cmsgout,tmsgout
     &                         ,nwdsout,ierr)
        cmsgout(4)='d2_nen_cmo'
        call cmo_delatt_all_lg(imsgout,xmsgout,cmsgout,tmsgout
     &                         ,nwdsout,ierr)
        cmsgout(4)='d2_nef_cmo'
        call cmo_delatt_all_lg(imsgout,xmsgout,cmsgout,tmsgout
     &                         ,nwdsout,ierr)
        cmsgout(4)='d2_nee_cmo'
        call cmo_delatt_all_lg(imsgout,xmsgout,cmsgout,tmsgout
     &                         ,nwdsout,ierr)
        cmsgout(4)='d2_jtet_cycle_max'
        call cmo_delatt_all_lg(imsgout,xmsgout,cmsgout,tmsgout
     &                         ,nwdsout,ierr)
 
        cmsgout(4)='d3_nnodes'
        call cmo_delatt_all_lg(imsgout,xmsgout,cmsgout,tmsgout
     &                         ,nwdsout,ierr)
 
C$      cmsgout(4)='d3_nelements'
C$      call cmo_delatt_all_lg(imsgout,xmsgout,cmsgout,tmsgout
C$   &                         ,nwdsout,ierr)
C$      cmsgout(4)='d3_nen_cmo'
C$      call cmo_delatt_all_lg(imsgout,xmsgout,cmsgout,tmsgout
C$   &                         ,nwdsout,ierr)
C$      cmsgout(4)='d3_nef_cmo'
C$      call cmo_delatt_all_lg(imsgout,xmsgout,cmsgout,tmsgout
C$   &                         ,nwdsout,ierr)
C$      cmsgout(4)='d3_nee_cmo'
C$      call cmo_delatt_all_lg(imsgout,xmsgout,cmsgout,tmsgout
C$   &                         ,nwdsout,ierr)
 
100     cmsgout(4)='d0_nclrs'
        call cmo_delatt_all_lg(imsgout,xmsgout,cmsgout,tmsgout
     &                         ,nwdsout,ierr)
        cmsgout(4)='d0_clrlen'
        call cmo_delatt_all_lg(imsgout,xmsgout,cmsgout,tmsgout
     &                         ,nwdsout,ierr)
        cmsgout(4)='d0_nfilters'
        call cmo_delatt_all_lg(imsgout,xmsgout,cmsgout,tmsgout
     &                         ,nwdsout,ierr)
 
c .................................................................
 
c........ (successful return) ..................
 
1000    ierror=0
        if (local_debug.gt.0) call mmverify()
        return
 
c........ (failure return) ..................
9999    ierror=1
        if (local_debug.gt.0) call mmverify()
        cbuf=' LOWER D ERROR'
        call writloga('default',0,cbuf,0,ierr)
        return
c .....................................................
        end
 
c-----------------------------------------------------------------------
C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|
c-----------------------------------------------------------------------
C #####################################################################
C   sizes_lower_d_lg
C
C   PURPOSE -
C
C      figure out how much storage we need to reserve
C      for the lower d strucutures.
C
C   INPUT ARGUMENTS -
C
C        max_topo,d0_topo
C        nnodes,nelements,d0_nef_cmo,mbndry,nconbnd
C        itettyp,itetclr,itet,itetoff,jtet,jtetoff
C        itp1,icr1,icontab,isn1,iparent
C
C   OUTPUT ARGUMENTS -
C
C        d1_nnodes,d1_nelements
C        d1_nef_cmo,d1_nee_cmo,d1_nen_cmo
C        d0_node_topo,d0_elm_d1
C        ierror - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C   USAGE NOTES / CAVEATS / CHANGES -
C
C    it is probably not advisable to call this routine outside this
C    lower_d command suite.
C
C    see notes for create_lower_d_lg for interior_icr_flag
C
 
C less clear notes to myself for code development .......................
 
c if d0_topo=1, don't create element array
c d0 refers to input mesh, d1 refers to 1 dimension lower (output) mesh
 
C #####################################################################
 
        subroutine sizes_lower_d_lg(max_topo,d0_topo
     &          ,nnodes,nelements,d0_nef_cmo,mbndry,nconbnd
     &          ,jtet_cycle_max,interior_icr_flag,ibtype,geom_name
     &          ,itettyp,itetclr,itet,itetoff,jtet,jtetoff
     &          ,itp1,icr1,icontab,isn1,iparent
     &          ,d1_nnodes,d1_nelements
     &          ,d1_nef_cmo,d1_nee_cmo,d1_nen_cmo
     &          ,d0_node_topo,d0_elm_d1
     &          ,ierror)
c ........................................................................
 
        implicit none
 
        include 'local_element.h'
        include 'chydro.h'
 
        ! passed variables
 
        integer max_topo       ! topological dimension of the highest mesh
     &         ,d0_topo        ! topological dimension of the input
     &         ,nnodes         ! standard lagrit scalars for d0 mesh
     &         ,nelements
     &         ,d0_nef_cmo     !   (=faces_per_element)
     &         ,mbndry
     &         ,nconbnd
     &         ,jtet_cycle_max     ! longest jtet cycle length
     &         ,interior_icr_flag  ! flag as to whether constraints exist in the interior
 
        character*(*) ibtype(*),geom_name ! need re testing if virtual constraint
 
        integer itettyp(*)     ! standard lagrit arrays for d0 mesh
     &         ,itetclr(*)
     &         ,itet(*)
     &         ,itetoff(*)
     &         ,jtet(*)
     &         ,jtetoff(*)
     &         ,itp1(*)
     &         ,icr1(*)
     &         ,icontab(50,*)
     &         ,isn1(*)
     &         ,iparent(*)
 
        integer  d1_nnodes     ! lagrit scalars for lower d output
     &          ,d1_nelements
     &          ,d1_nef_cmo    !    (=faces per element)
     &          ,d1_nee_cmo    !    (=edges per element)
     &          ,d1_nen_cmo    !    (=nodes per element)
 
        integer  d0_node_topo(*) ! max lower d in which node participates
     &          ,d0_elm_d1(*)  ! element translation from input to lower d
 
        integer ierror         ! error flag: 0 on return if successful
 
        ! local variables
 
        integer i,j,ityp,jtyp,ind,nnd,nef,ioff,joff,iuse
     &          ,iel,jel,kel,iface,jface,kface,it,jt
     &          ,local_debug,ierr,iclr,jclr,kclr,d1_topo
     &          ,d1_nef_min,d1_nee_min,d1_nen_min
     &          ,isurf,n_surf,icon,ncon,mincon
     &          ,loc_icr(maxnee1),icycle,examine
 
        character*132 cbuf
 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
 
        local_debug=0
 
        if (local_debug.gt.0) then
           write(*,*) 'in sizes_lower_d_lg'
           call mmverify()
           write(*,*) 'fix for mixed-d meshes: no ifelmpnts...'
        endif
 
c........ (find one dimension lower lengths) ..................
c and create translation from current topological dimension
c to one-dimension lower, ie, find:
c      d1_nnodes    : nnodes
c      d1_nelements : nelements
c      d1_nef_cmo   : faces_per_element
c      d1_nee_cmo   : edges_per_element
c      d1_nen_cmo   : nodes_per_element
c      d0_node_topo   : max lower d in which node participates
c                       will have values from previous calls if max_topo.ne.d0_topo
c      d0_elm_d1    : element of current d to element of lower d translation -
c
c on output:
c d0_node_topo >0 for the parent nodes which have lower-d "child" nodes
c            <0 for the d0 children of d0 parent nodes with d1 "children"
c            =0 for all other nodes
c d0_elm_d1 >0 for the "parent" faces of lower-d elements
c              (the "parent" faces are in the element with the lowest itetclr)
c           <0 for other faces which touch lower-d elements
c           =0 for all other faces
c
c if d0_topo=1, then don't create element arrays
c if d0_topo=0, then don't do anything
c
c Note:
c The parent choice is meant to give consistently oriented surfaces.
c However, note for virtual surfaces (or any other case
c where itetclr is not a unique direction indicator),
c the surfaces will not be consistently oriented
c simply from the parent choice, and should be re-oriented
c once jtet on the lower d surface is known
c
c As may be coming from lower-d, jtet is a linked list
c pointing to all element faces of the d0 mesh which have
c the given face in common.
c It is assumed that only a single face of a given element
c occurs in any jtet loop (no 'folded' elements), and that
c each face occurs in at most one jtet loop.
c Note also that the standard jtet also has this definition,
c except that the maximim loop length is two.
c This is similar to the standard isn1 for parent/child nodes,
c except there is no "parent": the smallest element number
c could be used as a marker if a marker was desired.
c..........................
 
        d1_nef_cmo=0
        d1_nen_cmo=0
        d1_nee_cmo=0
        d1_nef_min=maxnef
        d1_nen_min=maxnen
        d1_nee_min=maxnee2
        d1_nnodes=0
        d1_nelements=0
        d1_nnodes=0
 
        ! if d0_topo=max_topo, zero d0_node_topo to indicate node belongs
        ! to default highest dimension class (eg, interior nodes of original mesh)
        if (d0_topo.eq.max_topo) then
           do i=1,nnodes
              d0_node_topo(i)=0
           enddo
        endif
 
        ! return if nothing to do
        if (d0_topo.lt.1.or.nnodes.lt.1.or.nelements.lt.1) goto 1000
 
        if (local_debug.gt.0) then
           write(*,*) 'at test 1 in sizes_lower_d_lg'
           call mmverify()
        endif
 
        ! set "examine" greater than largest possible d1_nelements,d1_nnodes
        examine=abs(nelements*d0_nef_cmo)+10
        if (examine.lt.nnodes+10) examine=nnodes+10
 
        ! first set d0_elm_d1=examine to mark for examination
        if (d0_topo.gt.1) then
           do iel=1,nelements
              ioff=jtetoff(iel)
              do iface=1,nelmnef(itettyp(iel))
                 d0_elm_d1(ioff+iface)=examine
              enddo
           enddo
        endif
 
        if (local_debug.gt.0) then
           write(*,*) 'at test 2 in sizes_lower_d_lg'
           call mmverify()
        endif
 
        ! now mark interface nodes and faces.
 
        ! mark interface nodes only for case d0_topo=1.
        if (d0_topo.eq.1) then
           do iel=1,nelements
              ioff=itetoff(iel)
              joff=jtetoff(iel)
              ityp=itettyp(iel)
              iclr=itetclr(iel)
              nef=nelmnef(ityp)
              !! mark all nodes on interface faces
              do iface=1,nef
                 it=jtet(joff+iface)
                 if ((it.le.0.and.mbndry.eq.0)
     &                 .or.(it.ge.mbndry.and.mbndry.gt.0)) then
                    jtyp=ielmface3(iface,ityp)
                    nnd=nelmnen(jtyp)
                    do ind=1,nnd
                       j=ielmface1(ind,iface,ityp)
                       j=iparent(itet(ioff+j))
                       d0_node_topo(j)=examine
                    enddo
                 endif
              enddo
              !! mark all virtual nodes if d0_topo=max_topo
              !! (note vrt are own parent)
              if (d0_topo.eq.max_topo) then
                 do ind=1,nelmnen(ityp)
                    j=itet(ioff+ind)
                    if (itp1(j).eq.ifitpvrt) d0_node_topo(j)=examine
                 enddo
              endif
           enddo
           goto 600
        endif
 
        ! mark interface nodes and faces for case d0_topo>1.
 
        do iel=1,nelements
           ioff=jtetoff(iel)
           ityp=itettyp(iel)
           iclr=itetclr(iel)
           nef=nelmnef(ityp)
           do iface=1,nef
              !! skip if already examined
              if (d0_elm_d1(ioff+iface).ne.examine) goto 500
              !! check if real or virtual interface face
              it=jtet(ioff+iface)
              iuse=0
              if ((it.ge.mbndry.and.mbndry.gt.0)
     &             .or.(it.le.0.and.mbndry.eq.0)) then
                 !! exterior or interface face -> use
                 iuse=1
                 it=abs(it)-mbndry
              elseif (d0_topo.eq.max_topo) then
                 !! for top dimension, check if all nodes virtual
                 !! and part of same (virtual) constraint surface. Use only if so.
                 jel=1+(it-1)/d0_nef_cmo
                 jface=it-(jel-1)*d0_nef_cmo
                 ! only 2 elements border as otherwise jtet>mbndry
                 if ( 1+(jtet(jtetoff(jel)+jface)-1)/d0_nef_cmo
     &               .ne. iel) goto 9999
                 ! check if all virtual pts
                 joff=itetoff(iel)
                 nnd=nelmnen(ielmface3(iface,ityp))
                 iuse=0
                 do ind=1,nnd
                    j=ielmface1(ind,iface,ityp)
                    j=itet(joff+j) ! NOT parent as need itp
                    if (itp1(j).eq.ifitpcup) j=isn1(j)
                    i=itp1(j)
                    loc_icr(ind)=icr1(j)
                    if (loc_icr(ind).ne.0 .and.
     &                    (i.eq.ifitpvrt.or.i.eq.ifitpalb
     &                 .or.i.eq.ifitpvin.or.i.eq.ifitpvif
     &                 .or.i.eq.ifitpvrb.or.i.eq.ifitpvfb
     &                 .or.i.eq.ifitpvrf.or.i.eq.ifitpvir)) iuse=iuse+1
                 enddo
                 if (iuse.ne.nnd
     &                   .or.interior_icr_flag.eq.1
     &                   .or.interior_icr_flag.eq.3
     &                   .or.interior_icr_flag.eq.5
     &                   .or.interior_icr_flag.eq.7) then
                    iuse=0
                 elseif (nconbnd.gt.0.and.nnd.gt.1) then
                    ! find if share non-zero constraint
                    ! (must be more than 1 ind or would not be here)
                    mincon=loc_icr(1)
                    ncon=icontab(1,mincon)
                    do ind=2,nnd
                       icon=icontab(1,loc_icr(ind))
                       if (icon.lt.ncon) then
                          mincon=loc_icr(ind)
                          ncon=icon
                       endif
                    enddo
                    n_surf=0
                    do icon=1,ncon
                       isurf=icontab(2+icon,mincon)
                       ! test isurf is vrt
                       if (geom_name.ne.'-none-') then
                          if (ibtype(isurf)(1:7).ne.'virtual') goto 711
                       endif
                       do ind=1,nnd
                          if (loc_icr(ind).eq.mincon) goto 710
                          do i=1,icontab(1,loc_icr(ind))
                             if (icontab(2+i,loc_icr(ind)).eq.isurf)
     &                                       goto 710
                          enddo
                          goto 711
710                       continue
                       enddo
                       n_surf=n_surf+1
711                    continue
                    enddo
                    if (n_surf.eq.0) then
                       iuse=0
                    else
                       iuse=1
                    endif
                 else
                    ! assume lowest icr1 is constraint of surface
                    iuse=1
                 endif
              else
                 !! interior face in lower d -> don't use
                 iuse=0
                 jel=1+(it-1)/d0_nef_cmo
                 jface=it-(jel-1)*d0_nef_cmo
              endif
              if (iuse.eq.0) then
                 !! set d0_elm_d1 to indicate no d1 element for these faces
                 ! must be case it<mbndry above
                 ! -> exactly 1 opposite face and jel,jface calc'd above
                 d0_elm_d1(ioff+iface)=0
                 d0_elm_d1(jtetoff(jel)+jface)=0
              else
                 !! set d0_elm_d1 to indicate these faces are a d1 element
                 !! and mark d0_node_topo
                 ! first find "parent" face (smallest element # in lowest color)
                 ! this will help to "order" the surface consistently
                 ! (will need to re-check once jtet known if really want ordered)
                 kel=iel
                 kface=iface
                 kclr=iclr
                 jt=it
                 jel=1+(it-1)/d0_nef_cmo
                 jface=jt-(jel-1)*d0_nef_cmo
                 icycle=0
                 do while (    jt.gt.0
     &                   .and. (jel.ne.iel.or.jface.ne.iface)
     &                   .and. icycle.le.jtet_cycle_max      )
                    icycle=icycle+1
                    jclr=itetclr(jel)
                    if (kclr.gt.jclr .or.
     &                      (kclr.eq.jclr.and.jel.lt.kel) ) then
                       kclr=jclr
                       kel=jel
                       kface=jface
                    endif
                    joff=jtetoff(jel)
                    jt=abs(jtet(joff+jface))
                    if (jt.ge.mbndry) jt=jt-mbndry
                    jel=1+(jt-1)/d0_nef_cmo
                    jface=jt-(jel-1)*d0_nef_cmo
                 enddo
                 d1_nelements=d1_nelements+1
                 d0_elm_d1(ioff+iface)=d1_nelements
                 jtyp=ielmface3(iface,ityp)
                 if (d1_nef_cmo.lt.nelmnef(jtyp))
     &               d1_nef_cmo = nelmnef(jtyp)
                 if (d1_nee_cmo.lt.nelmnee(jtyp))
     &               d1_nee_cmo = nelmnee(jtyp)
                 if (d1_nen_cmo.lt.nelmnen(jtyp))
     &               d1_nen_cmo = nelmnen(jtyp)
                 if (d1_nef_min.gt.nelmnef(jtyp))
     &               d1_nef_min = nelmnef(jtyp)
                 if (d1_nee_min.gt.nelmnee(jtyp))
     &               d1_nee_min = nelmnee(jtyp)
                 if (d1_nen_min.gt.nelmnen(jtyp))
     &               d1_nen_min = nelmnen(jtyp)
                 jt=jtetoff(kel)+kface
                 d0_elm_d1(jt)=d1_nelements
                 jt=abs(jtet(jt))
                 if (jt.ge.mbndry) jt=jt-mbndry
                 jel=1+(jt-1)/d0_nef_cmo
                 jface=jt-(jel-1)*d0_nef_cmo
                 icycle=0
                 do while (     jt.gt.0
     &                    .and. (jel.ne.kel .or. jface.ne.kface)
     &                    .and. icycle.le.jtet_cycle_max        )
                    icycle=icycle+1
                    jclr=itetclr(jel)
                    joff=jtetoff(jel)+jface
                    d0_elm_d1(joff)=-d1_nelements
                    jt=abs(jtet(joff))
                    if (jt.ge.mbndry) jt=jt-mbndry
                    jel=1+(jt-1)/d0_nef_cmo
                    jface=jt-(jel-1)*d0_nef_cmo
                 enddo
                 jtyp=ielmface3(iface,ityp)
                 nnd=nelmnen(jtyp)
                 do ind=1,nnd
                    j=ielmface1(ind,iface,ityp)
                    j=iparent(itet(itetoff(iel)+j))
                    d0_node_topo(j)=examine
                 enddo
                 ! ioffsum=ioffsum+nnd
                 ! joffsum=joffsum+nelmnef(jtyp)
              endif
500           continue
           enddo
        enddo
 
600     if (local_debug.gt.0) then
           write(*,*) 'at test 3 in sizes_lower_d_lg'
           call mmverify()
        endif
 
        d1_topo=max_topo-d0_topo+1
        do i=1,nnodes
           if (d0_node_topo(i).eq.examine) then
              d1_nnodes=d1_nnodes+1
              d0_node_topo(i)=d1_topo ! d1_nnodes
              j=isn1(i)
              do while (j.ne.i.and.j.ne.0)
                 d0_node_topo(j)=-d1_topo ! -d1_nnodes
                 j=isn1(j)
              enddo
           endif
        enddo
 
        if  (d1_nef_min.ne.d1_nef_cmo.or.d1_nee_min.ne.d1_nee_cmo
     &            .or.d1_nen_min.ne.d1_nen_cmo) then
           d1_nef_cmo=nelmnef(ifelmhyb)
           d1_nee_cmo=nelmnee(ifelmhyb)
           d1_nen_cmo=nelmnen(ifelmhyb)
        endif
 
c........ (successful return) ..................
 
1000    ierror=0
        if (local_debug.gt.0) then
           write(*,*) 'returning from sizes_lower_d_lg succesfully'
           call mmverify()
        endif
        return
 
c........ (failure return) ..................
9999    ierror=1
        if (local_debug.gt.0) then
           write(*,*) 'returning from sizes_lower_d_lg unsuccesfully'
           call mmverify()
           stop
        endif
        cbuf='ERROR IN ROUTINE lower_d_sizes: ABORTING'
        call writloga('default',0,cbuf,0,ierr)
        return
c .....................................................
        end
 
c-----------------------------------------------------------------------
C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|
c-----------------------------------------------------------------------
C #####################################################################
C   storage_lower_d_lg
C
C   PURPOSE -
C
C       create the needed lower d attribute storage.
C
C   INPUT ARGUMENTS -
C
C        cmo         - the mesh object of interest
C        new_storage - flag to delete old before reserving new (or not)
C
C   OUTPUT ARGUMENTS -
C
C        ierror - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C   USAGE NOTES / CAVEATS / CHANGES -
C
C    it is probably not advisable to call this routine outside this
C    lower_d command suite.
C
C    set default d0,d1,d2,d3 nelements,nnodes,nef_cmo,nen_cmo,nee_cmo to 0
C    and lower_d_flag to 1
C
C    it is assumed that is the attribute exists, the length,type,etc are correct
C    -- could add test and fix as needed ...
C
C less clear notes to myself for code development .......................
 
c !NO! release any pre-existing blocks/attributes/meshes as needed
 
C #####################################################################
 
        subroutine storage_lower_d_lg(cmo,new_storage,ierror)
c ........................................................................
 
        implicit none
 
        include 'local_element.h'
 
        character*(*) cmo
        integer ierror,new_storage
 
        integer  d0_topo
 
        integer  local_debug,lower_d_flag,len,ityp,ierr,ip,lcmo
 
        character*132 cbuf
        character*32 cmsgout(11)
        real*8 xmsgout(11)
        integer imsgout(11),nwdsout,tmsgout(11)
 
        integer icharlnf
        external icharlnf
 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
 
        local_debug=0
        if (local_debug.gt.0) then
           write(*,*) 'fix interp...'
           call mmverify()
        endif
 
        nwdsout=11
        do ip=1,11
           xmsgout(ip)=0.d0
           imsgout(ip)=0
           tmsgout(ip)=3
           cmsgout(ip)=' '
        enddo
        tmsgout(11)=1
        cmsgout(1)='cmo'
        cmsgout(2)='addatt'
        cmsgout(3)=cmo
        if (cmo(1:1).eq.'-') then
           call cmo_get_name(cmsgout(3),ierr)
        endif
        cmsgout(9)='temporary'
        cmsgout(10)='x'
        cmsgout(11)=' '
 
c........ (make sure cmo exists) ..................
 
        lcmo=icharlnf(cmo)
        call cmo_exist(cmo(1:lcmo),ierr)
        if (ierr.ne.0) goto 9999
 
c........ (release old storage if any) ..................
 
        ! no: presume OK ...., hence should add "cmo_get_info" checks
        ! call release_lower_d_lg(cmo(1:lcmo),ierr)
        ! if (ierr.ne.0) goto 9999
 
c........ (get d0 info: abort if not legal) ..................
c abort only on d0_topo and not sizes as might want to call this
c before nnodes,nelements have a value since it is only allocation...
 
        call cmo_get_info('ndimensions_topo',cmo,d0_topo
     &                    ,len,ityp,ierr)
        if (ierr.ne.0) goto 9999
        if (d0_topo.lt.1) goto 1000
        call cmo_get_info('nnodes',cmo,ip,len,ityp,ierr)
        if (ierr.ne.0) goto 9999
        call cmo_get_info('nelements',cmo,ip,len,ityp,ierr)
        if (ierr.ne.0) goto 9999
 
        ! d0_topo>3: not enough structures built ..., for now, press on ...
        ! if (d0_topo.gt.3) goto 9999
 
c........ (assign defaults) ..................
 
c .................................................................
c *-*-*-*-*-* (create lower d attributes) *-*-*-*-*-*-*-*
 
c new attributes --  Q:
c - ioflag: x vs agx ???
c - persistence: temporary vs permanent ???
c - deleting if exists vs checking fields vs assuming OK??
c   for now: just delete so "clean"....
 
c ==== create d0 storage ====
 
        cmsgout(5)='INT'
        cmsgout(6)='scalar'
        cmsgout(7)='scalar'
        cmsgout(8)='constant'
 
c... flag d0 attributes
c  lower_d_flag ...............
        call cmo_get_info('lower_d_flag',cmo,lower_d_flag
     &                    ,len,ityp,ierr)
        if (ierr.ne.0) then
C$         write(cbuf,*)'cmo/addatt/'//cmo(1:lcmo)//'/lower_d_flag'
C$   &        //'/INT/scalar/scalar/constant/temporary/x/0/; finish'
C$         call dotask(cbuf,ierr)
           cmsgout(4)='lower_d_flag'
           call cmo_addatt(imsgout,xmsgout,cmsgout,tmsgout,nwdsout,ierr)
        endif
        len=1
        ityp=1
        lower_d_flag=2
        call cmo_set_info('lower_d_flag',cmo,lower_d_flag
     &                    ,len,ityp,ierr)
 
c... filter d0 attributes
 
c  d0_nfilters ...............
        call cmo_get_info('d0_nfilters',cmo,ip,len,ityp,ierr)
        if (ierr.ne.0) then
           cmsgout(4)='d0_nfilters'
           call cmo_addatt(imsgout,xmsgout,cmsgout,tmsgout,nwdsout,ierr)
        endif
        if (new_storage.ne.0) then
           len=1
           ityp=1
           ip=0
           call cmo_set_info('d0_nfilters',cmo,ip,len,ityp,ierr)
        endif
 
c  d0_filters ...............
        call cmo_get_info('d0_filters',cmo,ip,len,ityp,ierr)
        if (ierr.ne.0) then
           cmsgout(4)='d0_filters'
           cmsgout(5)='VINT'
           cmsgout(7)='d0_nfilters'
           cmsgout(8)='user'
           call cmo_addatt(imsgout,xmsgout,cmsgout,tmsgout,nwdsout,ierr)
        endif
 
c... color d0 attributes
        cmsgout(5)='INT'
        cmsgout(6)='scalar'
        cmsgout(7)='scalar'
        cmsgout(8)='constant'
c  d0_nclrs ...............
        call cmo_get_info('d0_nclrs',cmo,ip,len,ityp,ierr)
        if (ierr.ne.0) then
C$         write(cbuf,*)'cmo/addatt/'//cmo(1:lcmo)//'/d0_nclrs'
C$   &        //'/INT/scalar/scalar/constant/temporary/x/0/; finish'
C$         call dotask(cbuf,ierr)
           cmsgout(4)='d0_nclrs'
           call cmo_addatt(imsgout,xmsgout,cmsgout,tmsgout,nwdsout,ierr)
        endif
        if (new_storage.eq.1) then
           len=1
           ityp=1
           ip=0
           call cmo_set_info('d0_nclrs',cmo,ip,len,ityp,ierr)
        endif
c  d0_clrlen ...............
        call cmo_get_info('d0_clrlen',cmo,ip,len,ityp,ierr)
        if (ierr.ne.0) then
C$         write(cbuf,*)'cmo/addatt/'//cmo(1:lcmo)//'/d0_clrlen'
C$   &        //'/INT/scalar/scalar/constant/temporary/x/0/; finish'
C$         call dotask(cbuf,ierr)
           cmsgout(4)='d0_clrlen'
           call cmo_addatt(imsgout,xmsgout,cmsgout,tmsgout,nwdsout,ierr)
        endif
        if (new_storage.eq.1) then
           len=1
           ityp=1
           ip=0
           call cmo_set_info('d0_clrlen',cmo,ip,len,ityp,ierr)
        endif
 
        cmsgout(5)='VINT'
        cmsgout(8)='user'
c  d0_clroff ...............
        call cmo_get_info('d0_clroff',cmo,ip,len,ityp,ierr)
        if (ierr.ne.0) then
C$         write(cbuf,*)'cmo/addatt/'//cmo(1:lcmo)//'/d0_clroff'
C$   &        //'/VINT/scalar/d0_nclrs/user/temporary/x/0/; finish'
C$         call dotask(cbuf,ierr)
           cmsgout(4)='d0_clroff'
           cmsgout(7)='d0_nclrs'
           call cmo_addatt(imsgout,xmsgout,cmsgout,tmsgout,nwdsout,ierr)
        endif
c  d0_clrtab ...............
        call cmo_get_info('d0_clrtab',cmo,ip,len,ityp,ierr)
        if (ierr.ne.0) then
C$         write(cbuf,*)'cmo/addatt/'//cmo(1:lcmo)//'/d0_clrtab'
C$   &         //'/VINT/scalar/d0_clrlen/user/temporary/x/0/; finish'
C$         call dotask(cbuf,ierr)
           cmsgout(4)='d0_clrtab'
           cmsgout(7)='d0_clrlen'
           call cmo_addatt(imsgout,xmsgout,cmsgout,tmsgout,nwdsout,ierr)
        endif
 
c... node d0 attributes
c  d0_node_topo ...............
        call cmo_get_info('d0_node_topo',cmo,ip,len,ityp,ierr)
        if (ierr.ne.0) then
C$         write(cbuf,*)'cmo/addatt/'//cmo(1:lcmo)//'/d0_node_topo'
C$   &         //'/VINT/scalar/nnodes/max/temporary/x/0/; finish'
C$         call dotask(cbuf,ierr)
           cmsgout(4)='d0_node_topo'
           cmsgout(7)='nnodes'
           cmsgout(8)='max'
           cmsgout(10)='agx'
           call cmo_addatt(imsgout,xmsgout,cmsgout,tmsgout,nwdsout,ierr)
           cmsgout(10)='x'
        endif
 
c... element d0 attributes
c don't create as default....
 
c ==== create d1 storage ====
 
        cmsgout(5)='INT'
        cmsgout(6)='scalar'
        cmsgout(7)='scalar'
        cmsgout(8)='constant'
 
c... size d1 attributes
c  d1_nnodes ...............
        call cmo_get_info('d1_nnodes',cmo,ip,len,ityp,ierr)
        if (ierr.ne.0) then
C$         write(cbuf,*)'cmo/addatt/'//cmo(1:lcmo)//'/d1_nnodes'
C$   &        //'/INT/scalar/scalar/constant/temporary/x/0; finish'
C$         call dotask(cbuf,ierr)
           cmsgout(4)='d1_nnodes'
           call cmo_addatt(imsgout,xmsgout,cmsgout,tmsgout,nwdsout,ierr)
        endif
 
c... check if need d1 element info
        if (d0_topo.eq.1) goto 1000
 
c  d1_nelements ...............
        call cmo_get_info('d1_nelements',cmo,ip,len,ityp,ierr)
        if (ierr.ne.0) then
C$         write(cbuf,*)'cmo/addatt/'//cmo(1:lcmo)//'/d1_nelements'
C$   &        //'/INT/scalar/scalar/constant/temporary/x/0; finish'
C$         call dotask(cbuf,ierr)
           cmsgout(4)='d1_nelements'
           call cmo_addatt(imsgout,xmsgout,cmsgout,tmsgout,nwdsout,ierr)
        endif
c  d1_nef_cmo ...............
        call cmo_get_info('d1_nef_cmo',cmo,ip,len,ityp,ierr)
        if (ierr.ne.0) then
C$         write(cbuf,*)'cmo/addatt/'//cmo(1:lcmo)//'/d1_nef_cmo'
C$   &        //'/INT/scalar/scalar/constant/temporary/x/0; finish'
C$         call dotask(cbuf,ierr)
           cmsgout(4)='d1_nef_cmo'
           call cmo_addatt(imsgout,xmsgout,cmsgout,tmsgout,nwdsout,ierr)
        endif
c  d1_nee_cmo ...............
        call cmo_get_info('d1_nee_cmo',cmo,ip,len,ityp,ierr)
        if (ierr.ne.0) then
C$         write(cbuf,*)'cmo/addatt/'//cmo(1:lcmo)//'/d1_nee_cmo'
C$   &        //'/INT/scalar/scalar/constant/temporary/x/0; finish'
C$         call dotask(cbuf,ierr)
           cmsgout(4)='d1_nee_cmo'
           call cmo_addatt(imsgout,xmsgout,cmsgout,tmsgout,nwdsout,ierr)
        endif
c  d1_nen_cmo ...............
        call cmo_get_info('d1_nen_cmo',cmo,ip,len,ityp,ierr)
        if (ierr.ne.0) then
C$         write(cbuf,*)'cmo/addatt/'//cmo(1:lcmo)//'/d1_nen_cmo'
C$   &        //'/INT/scalar/scalar/constant/temporary/x/0; finish'
C$         call dotask(cbuf,ierr)
           cmsgout(4)='d1_nen_cmo'
           call cmo_addatt(imsgout,xmsgout,cmsgout,tmsgout,nwdsout,ierr)
        endif
c  d1_jtet_cycle_max...............
        call cmo_get_info('d1_jtet_cycle_max',cmo,ip,len,ityp,ierr)
        if (ierr.ne.0) then
C$         write(cbuf,*)'cmo/addatt/'//cmo(1:lcmo)//'/d1_jtet_cycle_max'
C$   &        //'/INT/scalar/scalar/constant/temporary/x/0; finish'
C$         call dotask(cbuf,ierr)
           cmsgout(4)='d1_jtet_cycle_max'
           imsgout(11)=2
           call cmo_addatt(imsgout,xmsgout,cmsgout,tmsgout,nwdsout,ierr)
           imsgout(11)=0
        endif
 
c... element d1 attributes
 
        cmsgout(5)='VINT'
        cmsgout(6)='scalar'
        cmsgout(7)='d1_nelements'
        cmsgout(8)='user'
 
c  d1_itettyp ...............
        call cmo_get_info('d1_itettyp',cmo,ip,len,ityp,ierr)
        if (ierr.ne.0) then
C$         write(cbuf,*)'cmo/addatt/'//cmo(1:lcmo)//'/d1_itettyp'
C$   &       //'/VINT/scalar/d1_nelements/user/temporary/x/0/; finish'
C$         call dotask(cbuf,ierr)
           cmsgout(4)='d1_itettyp'
           call cmo_addatt(imsgout,xmsgout,cmsgout,tmsgout,nwdsout,ierr)
        endif
c  d1_itetclr ...............
        call cmo_get_info('d1_itetclr',cmo,ip,len,ityp,ierr)
        if (ierr.ne.0) then
C$         write(cbuf,*)'cmo/addatt/'//cmo(1:lcmo)//'/d1_itetclr'
C$   &       //'/VINT/scalar/d1_nelements/user/temporary/x/0/; finish'
C$         call dotask(cbuf,ierr)
           cmsgout(4)='d1_itetclr'
           call cmo_addatt(imsgout,xmsgout,cmsgout,tmsgout,nwdsout,ierr)
        endif
c  d1_itetoff ...............
        call cmo_get_info('d1_itetoff',cmo,ip,len,ityp,ierr)
        if (ierr.ne.0) then
C$         write(cbuf,*)'cmo/addatt/'//cmo(1:lcmo)//'/d1_itetoff'
C$   &       //'/VINT/scalar/d1_nelements/user/temporary/x/0/; finish'
C$         call dotask(cbuf,ierr)
           cmsgout(4)='d1_itetoff'
           call cmo_addatt(imsgout,xmsgout,cmsgout,tmsgout,nwdsout,ierr)
        endif
c  d1_jtetoff ...............
        call cmo_get_info('d1_jtetoff',cmo,ip,len,ityp,ierr)
        if (ierr.ne.0) then
C$         write(cbuf,*)'cmo/addatt/'//cmo(1:lcmo)//'/d1_jtetoff'
C$   &       //'/VINT/scalar/d1_nelements/user/temporary/x/0/; finish'
C$         call dotask(cbuf,ierr)
           cmsgout(4)='d1_jtetoff'
           call cmo_addatt(imsgout,xmsgout,cmsgout,tmsgout,nwdsout,ierr)
        endif
 
c  d1_elm_d0 ...............
        call cmo_get_info('d1_elm_d0',cmo,ip,len,ityp,ierr)
        if (ierr.ne.0) then
C$         write(cbuf,*)'cmo/addatt/'//cmo(1:lcmo)//'/d1_elm_d0'
C$   &        //'/VINT/scalar/d1_nelements/user/temporary/x/0/; finish'
C$         call dotask(cbuf,ierr)
           cmsgout(4)='d1_elm_d0'
           call cmo_addatt(imsgout,xmsgout,cmsgout,tmsgout,nwdsout,ierr)
        endif
 
c  d1_itet ...............
        call cmo_get_info('d1_itet',cmo,ip,len,ityp,ierr)
        if (ierr.ne.0) then
C$         write(cbuf,*)'cmo/addatt/'//cmo(1:lcmo)//'/d1_itet'
C$   &       //'/VINT/d1_nen_cmo/d1_nelements'
C$   &             //'/user/temporary/x/0/; finish'
C$         call dotask(cbuf,ierr)
           cmsgout(4)='d1_itet'
           cmsgout(6)='d1_nen_cmo'
           call cmo_addatt(imsgout,xmsgout,cmsgout,tmsgout,nwdsout,ierr)
        endif
c  d1_jtet ...............
        call cmo_get_info('d1_jtet',cmo,ip,len,ityp,ierr)
        if (ierr.ne.0) then
C$         write(cbuf,*)'cmo/addatt/'//cmo(1:lcmo)//'/d1_jtet'
C$   &       //'/VINT/d1_nef_cmo/d1_nelements'
C$   &          //'/user/temporary/x/0/; finish'
C$         call dotask(cbuf,ierr)
           cmsgout(4)='d1_jtet'
           cmsgout(6)='d1_nef_cmo'
           call cmo_addatt(imsgout,xmsgout,cmsgout,tmsgout,nwdsout,ierr)
        endif
 
c don't create as default:
c... node d1->d0 attributes
c... d0->d1 attributes
 
c ==== create d2 storage ====
 
c... check if need d2 info
        if (d0_topo.lt.2) goto 1000
 
        cmsgout(5)='INT'
        cmsgout(6)='scalar'
        cmsgout(7)='scalar'
        cmsgout(8)='constant'
 
c... size d2 attributes
c  d2_nnodes ...............
        call cmo_get_info('d2_nnodes',cmo,ip,len,ityp,ierr)
        if (ierr.ne.0) then
C$         write(cbuf,*)'cmo/addatt/'//cmo(1:lcmo)//'/d2_nnodes'
C$   &        //'/INT/scalar/scalar/constant/temporary/x/0; finish'
C$         call dotask(cbuf,ierr)
           cmsgout(4)='d2_nnodes'
           call cmo_addatt(imsgout,xmsgout,cmsgout,tmsgout,nwdsout,ierr)
        endif
 
c... check if need element info
        if (d0_topo.eq.2) goto 1000
 
c  d2_nelements ...............
        call cmo_get_info('d2_nelements',cmo,ip,len,ityp,ierr)
        if (ierr.ne.0) then
C$         write(cbuf,*)'cmo/addatt/'//cmo(1:lcmo)//'/d2_nelements'
C$   &        //'/INT/scalar/scalar/constant/temporary/x/0; finish'
C$         call dotask(cbuf,ierr)
           cmsgout(4)='d2_nelements'
           call cmo_addatt(imsgout,xmsgout,cmsgout,tmsgout,nwdsout,ierr)
        endif
c  d2_nef_cmo ...............
        call cmo_get_info('d2_nef_cmo',cmo,ip,len,ityp,ierr)
        if (ierr.ne.0) then
C$         write(cbuf,*)'cmo/addatt/'//cmo(1:lcmo)//'/d2_nef_cmo'
C$   &        //'/INT/scalar/scalar/constant/temporary/x/0; finish'
C$         call dotask(cbuf,ierr)
           cmsgout(4)='d2_nef_cmo'
           call cmo_addatt(imsgout,xmsgout,cmsgout,tmsgout,nwdsout,ierr)
        endif
c  d2_nee_cmo ...............
        call cmo_get_info('d2_nee_cmo',cmo,ip,len,ityp,ierr)
        if (ierr.ne.0) then
C$         write(cbuf,*)'cmo/addatt/'//cmo(1:lcmo)//'/d2_nee_cmo'
C$   &        //'/INT/scalar/scalar/constant/temporary/x/0; finish'
C$         call dotask(cbuf,ierr)
           cmsgout(4)='d2_nee_cmo'
           call cmo_addatt(imsgout,xmsgout,cmsgout,tmsgout,nwdsout,ierr)
        endif
c  d2_nen_cmo ...............
        call cmo_get_info('d2_nen_cmo',cmo,ip,len,ityp,ierr)
        if (ierr.ne.0) then
C$         write(cbuf,*)'cmo/addatt/'//cmo(1:lcmo)//'/d2_nen_cmo'
C$   &        //'/INT/scalar/scalar/constant/temporary/x/0; finish'
C$         call dotask(cbuf,ierr)
           cmsgout(4)='d2_nen_cmo'
           call cmo_addatt(imsgout,xmsgout,cmsgout,tmsgout,nwdsout,ierr)
        endif
c  d2_jtet_cycle_max...............
        call cmo_get_info('d2_jtet_cycle_max',cmo,ip,len,ityp,ierr)
        if (ierr.ne.0) then
C$         write(cbuf,*)'cmo/addatt/'//cmo(1:lcmo)//'/d2_jtet_cycle_max'
C$   &        //'/INT/scalar/scalar/constant/temporary/x/0; finish'
C$         call dotask(cbuf,ierr)
           cmsgout(4)='d2_jtet_cycle_max'
           imsgout(11)=2
           call cmo_addatt(imsgout,xmsgout,cmsgout,tmsgout,nwdsout,ierr)
           imsgout(11)=0
        endif
 
c... element d2 attributes
 
        cmsgout(5)='VINT'
        cmsgout(6)='scalar'
        cmsgout(7)='d2_nelements'
        cmsgout(8)='user'
 
c  d2_itettyp ...............
        call cmo_get_info('d2_itettyp',cmo,ip,len,ityp,ierr)
        if (ierr.ne.0) then
C$         write(cbuf,*)'cmo/addatt/'//cmo(1:lcmo)//'/d2_itettyp'
C$   &       //'/VINT/scalar/d2_nelements/user/temporary/x/0/; finish'
C$         call dotask(cbuf,ierr)
           cmsgout(4)='d2_itettyp'
           call cmo_addatt(imsgout,xmsgout,cmsgout,tmsgout,nwdsout,ierr)
        endif
c  d2_itetclr ...............
        call cmo_get_info('d2_itetclr',cmo,ip,len,ityp,ierr)
        if (ierr.ne.0) then
C$         write(cbuf,*)'cmo/addatt/'//cmo(1:lcmo)//'/d2_itetclr'
C$   &       //'/VINT/scalar/d2_nelements/user/temporary/x/0/; finish'
C$         call dotask(cbuf,ierr)
           cmsgout(4)='d2_itetclr'
           call cmo_addatt(imsgout,xmsgout,cmsgout,tmsgout,nwdsout,ierr)
        endif
c  d2_itetoff ...............
        call cmo_get_info('d2_itetoff',cmo,ip,len,ityp,ierr)
        if (ierr.ne.0) then
C$         write(cbuf,*)'cmo/addatt/'//cmo(1:lcmo)//'/d2_itetoff'
C$   &       //'/VINT/scalar/d2_nelements/user/temporary/x/0/; finish'
C$         call dotask(cbuf,ierr)
           cmsgout(4)='d2_itetoff'
           call cmo_addatt(imsgout,xmsgout,cmsgout,tmsgout,nwdsout,ierr)
        endif
c  d2_jtetoff ...............
        call cmo_get_info('d2_jtetoff',cmo,ip,len,ityp,ierr)
        if (ierr.ne.0) then
C$         write(cbuf,*)'cmo/addatt/'//cmo(1:lcmo)//'/d2_jtetoff'
C$   &      //'/VINT/scalar/d2_nelements/user/temporary/x/0/; finish'
C$         call dotask(cbuf,ierr)
           cmsgout(4)='d2_jtetoff'
           call cmo_addatt(imsgout,xmsgout,cmsgout,tmsgout,nwdsout,ierr)
        endif
 
c  d2_elm_d1 ...............
        call cmo_get_info('d2_elm_d1',cmo,ip,len,ityp,ierr)
        if (ierr.ne.0) then
C$         write(cbuf,*)'cmo/addatt/'//cmo(1:lcmo)//'/d2_elm_d1'
C$   &       //'/VINT/scalar/d2_nelements/user/temporary/x/0/; finish'
C$         call dotask(cbuf,ierr)
           cmsgout(4)='d2_elm_d1'
           call cmo_addatt(imsgout,xmsgout,cmsgout,tmsgout,nwdsout,ierr)
        endif
 
c  d2_itet ...............
        call cmo_get_info('d2_itet',cmo,ip,len,ityp,ierr)
        if (ierr.ne.0) then
C$         write(cbuf,*)'cmo/addatt/'//cmo(1:lcmo)//'/d2_itet'
C$   &       //'/VINT/d2_nen_cmo/d2_nelements'
C$   &             //'/user/temporary/x/0/; finish'
C$         call dotask(cbuf,ierr)
           cmsgout(4)='d2_itet'
           cmsgout(6)='d2_nen_cmo'
           call cmo_addatt(imsgout,xmsgout,cmsgout,tmsgout,nwdsout,ierr)
        endif
c  d2_jtet ...............
        call cmo_get_info('d2_jtet',cmo,ip,len,ityp,ierr)
        if (ierr.ne.0) then
C$         write(cbuf,*)'cmo/addatt/'//cmo(1:lcmo)//'/d2_jtet'
C$   &       //'/VINT/d2_nef_cmo/d2_nelements'
C$   &             //'/user/temporary/x/0/; finish'
C$         call dotask(cbuf,ierr)
           cmsgout(4)='d2_jtet'
           cmsgout(6)='d2_nef_cmo'
           call cmo_addatt(imsgout,xmsgout,cmsgout,tmsgout,nwdsout,ierr)
        endif
 
c don't create as default:
c... node d2->d0 attributes
c... d0->d2 attributes
c... d1<->d2 attributes
 
c ==== create d3 storage ====
 
        cmsgout(5)='INT'
        cmsgout(6)='scalar'
        cmsgout(7)='scalar'
        cmsgout(8)='constant'
 
c... node size d3 attributes
c  d3_nnodes ...............
        call cmo_get_info('d3_nnodes',cmo,ip,len,ityp,ierr)
        if (ierr.ne.0) then
C$         write(cbuf,*)'cmo/addatt/'//cmo(1:lcmo)//'/d3_nnodes'
C$   &        //'/INT/scalar/scalar/constant/temporary/x/0; finish'
C$         call dotask(cbuf,ierr)
           cmsgout(4)='d3_nnodes'
           call cmo_addatt(imsgout,xmsgout,cmsgout,tmsgout,nwdsout,ierr)
        endif
 
c no elements so don't create:
c... element size d3 attributes
c... element d3 attributes
c... element d3->d0 attributes
 
c... d0 attributes for d3 - don't create as default
c don't create as default:
c... node d3->d0 attributes
c... d0->d3 attributes
c... d2<->d3 attributes
c... d1<->d3 attributes
 
c........ (successful return) ..................
 
1000    ierror=0
        if (local_debug.gt.0) then
           cbuf='cmo status; finish'
           call dotask(cbuf,ierr)
           call mmverify()
        endif
        return
 
c........ (failure return) ..................
9999    ierror=1
        if (local_debug.gt.0) call mmverify()
        if (local_debug.gt.9) stop
        cbuf=' LOWER D ERROR'
        call writloga('default',0,cbuf,0,ierr)
        return
c .....................................................
        end
 
c-----------------------------------------------------------------------
C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|
c-----------------------------------------------------------------------
C #####################################################################
C   reset_mbndry_lower_d_lg
C
C   PURPOSE -
C
C      increment mbndry as needed so that it can be used for all
C      topological hierarchies within this mesh object
C
C   INPUT ARGUMENTS -
C
C        cmo  - mesh object of interest
C        mbndry_new - new value of mbndry
C
C   OUTPUT ARGUMENTS -
C
C        ierror - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C   USAGE NOTES / CAVEATS / CHANGES -
C
C    - should work even if lower_d_structures don't exist
C    - if prior mbndry storage does not exist, then it is assumed to be zero
C    - if prior mbndry storage does not exist and mbndry_new is not zero,
C      currently returns error rather than creating mbndry storage
C
C #####################################################################
 
        subroutine reset_mbndry_lower_d_lg(cmo,mbndry_new,ierror)
 
c ........................................................................
        implicit none
        include 'local_element.h'
        character*(*) cmo
        integer mbndry_old,mbndry_new,ierror
     &         ,sign_old,sign_new
 
        pointer (ip_jtetoff,jtetoff),(ip_jtet,jtet)
     &         ,(ip_itettyp,itettyp)
        integer jtetoff(*),jtet(*),itettyp(*)
        integer iel,iface,ityp,ioff,len,ierr
     &         ,d0_nelements,d0_nef_cmo
     &         ,d1_nelements,d1_nef_cmo
     &         ,d2_nelements,d2_nef_cmo
        character*132 cbuf
c -------------------------------------------------
 
        ! get current mbndry
        call cmo_get_info('mbndry',cmo,mbndry_old
     &                       ,len,ityp,ierr)
        if (ierr.ne.0) then
           mbndry_old=0
           if (mbndry_new.ne.0) then
              ! if cmo exists, then could create mbndry instead:
              ! for now, just report error
              goto 9999
           endif
        endif
        if (mbndry_old.eq.mbndry_new) goto 3000
 
        ! get size info
        call cmo_get_info('nelements',cmo,d0_nelements
     &                       ,len,ityp,ierr)
        if (ierr.ne.0) goto 9999
        if (d0_nelements.le.0) d0_nelements=0
        call cmo_get_info('faces_per_element',cmo
     &                    ,d0_nef_cmo,len,ityp,ierr)
        if (ierr.ne.0) goto 9999
        if (d0_nef_cmo.le.0) d0_nef_cmo=0
        call cmo_get_info('d1_nelements',cmo,d1_nelements
     &                       ,len,ityp,ierr)
        if (ierr.ne.0.or.d1_nelements.le.0) d1_nelements=0
        call cmo_get_info('d1_nef_cmo',cmo,d1_nef_cmo,len,ityp,ierr)
        if (ierr.ne.0.or.d1_nef_cmo.le.0) d1_nef_cmo=0
        call cmo_get_info('d2_nelements',cmo,d2_nelements
     &                       ,len,ityp,ierr)
        if (ierr.ne.0.or.d2_nelements.le.0) d2_nelements=0
        call cmo_get_info('d2_nef_cmo',cmo,d2_nef_cmo,len,ityp,ierr)
        if (ierr.ne.0.or.d2_nef_cmo.le.0) d2_nef_cmo=0
 
        ! check sizes are legal
        if (mbndry_old.ne.0) then
           if (d0_nelements*d0_nef_cmo.ge.mbndry_old) goto 9999
           if (d1_nelements*d1_nef_cmo.ge.mbndry_old) goto 9999
           if (d2_nelements*d2_nef_cmo.ge.mbndry_old) goto 9999
           sign_old=+1
        else
           sign_old=-1
        endif
        if (mbndry_new.ne.0) then
           if (d0_nelements*d0_nef_cmo.ge.mbndry_new) goto 9999
           if (d1_nelements*d1_nef_cmo.ge.mbndry_new) goto 9999
           if (d2_nelements*d2_nef_cmo.ge.mbndry_new) goto 9999
           sign_new=+1
        else
           sign_new=-1
        endif
 
        ! reset mbndry
        len=1
        ityp=1
        call cmo_set_info('mbndry',cmo,mbndry_new,len,ityp,ierr)
        if (ierr.ne.0.and.mbndry_new.ne.0) goto 9999
 
        if (d0_nelements.le.0 .or. d0_nef_cmo.le.0) goto 1000
 
        ! get info for d0
        call cmo_get_info('itettyp',cmo,ip_itettyp,len,ityp,ierr)
        if (ierr.ne.0) goto 9998
        call cmo_get_info('jtetoff',cmo,ip_jtetoff,len,ityp,ierr)
        if (ierr.ne.0) goto 9998
        call cmo_get_info('jtet',cmo,ip_jtet,len,ityp,ierr)
        if (ierr.ne.0) goto 9998
 
        ! reset jtet for d0
        do iel=1,d0_nelements
           ityp=itettyp(iel)
           ioff=jtetoff(iel)
           do iface=1,nelmnef(ityp)
              if (sign_old*jtet(ioff+iface).ge.mbndry_old)
     &            jtet(ioff+iface) = mbndry_new + sign_new
     &                * (sign_old*jtet(ioff+iface) - mbndry_old)
           enddo
        enddo
 
 
1000    if (d1_nelements.le.0 .or. d1_nef_cmo.le.0) goto 2000
 
        ! get info for d1
        call cmo_get_info('d1_itettyp',cmo,ip_itettyp,len,ityp,ierr)
        if (ierr.ne.0) goto 9998
        call cmo_get_info('d1_jtetoff',cmo,ip_jtetoff,len,ityp,ierr)
        if (ierr.ne.0) goto 9998
        call cmo_get_info('d1_jtet',cmo,ip_jtet,len,ityp,ierr)
        if (ierr.ne.0) goto 9998
 
        ! reset jtet for d1
        do iel=1,d1_nelements
           ityp=itettyp(iel)
           ioff=jtetoff(iel)
           do iface=1,nelmnef(ityp)
              if (sign_old*jtet(ioff+iface).ge.mbndry_old)
     &            jtet(ioff+iface) = mbndry_new + sign_new
     &                * (sign_old*jtet(ioff+iface) - mbndry_old)
           enddo
        enddo
 
2000    if (d2_nelements.le.0 .or. d2_nef_cmo.le.0) goto 3000
 
        ! get info for d2
        call cmo_get_info('d2_itettyp',cmo,ip_itettyp,len,ityp,ierr)
        if (ierr.ne.0) goto 9998
        call cmo_get_info('d2_jtetoff',cmo,ip_jtetoff,len,ityp,ierr)
        if (ierr.ne.0) goto 9998
        call cmo_get_info('d2_jtet',cmo,ip_jtet,len,ityp,ierr)
        if (ierr.ne.0) goto 9998
 
        ! reset jtet for d2
        do iel=1,d2_nelements
           ityp=itettyp(iel)
           ioff=jtetoff(iel)
           do iface=1,nelmnef(ityp)
              if (sign_old*jtet(ioff+iface).ge.mbndry_old)
     &            jtet(ioff+iface) = mbndry_new + sign_new
     &                * (sign_old*jtet(ioff+iface) - mbndry_old)
           enddo
        enddo
 
3000    continue
 
        ierror=0
        return
 
9998    ierror=2
        cbuf='WARNING: reset_mbndry_lower_d_lg - jtet may be incorrect'
        call writloga('default',0,cbuf,0,ierr)
        return
 
9999    ierror=1
        cbuf='ERROR: reset_mbndry_lower_d_lg - mbndry not reset'
        call writloga('default',0,cbuf,0,ierr)
        return
 
        end
 
C-----------------------------------------------------------------------
C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|
C-----------------------------------------------------------------------
C #####################################################################
C   print_clrtab_lower_d_lg
C
C   PURPOSE -
C
C      print the color table used to decode the lower d itetclrs
C      (currently only to screen)
C
C   INPUT ARGUMENTS -
C
C        d0_nclrs,d0_clroff,d0_clrtab
C
C   OUTPUT ARGUMENTS -
C
C        ierror - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C   USAGE NOTES / CAVEATS / CHANGES -
C
C    should modify so that can print to file if desired
C    (in which case, need "read_clrtab_lower_d_lg" also?)
C
C #####################################################################
 
        subroutine print_clrtab_lower_d_lg(d0_nclrs,d0_clroff,d0_clrtab)
 
c ........................................................................
        implicit none
        integer d0_nclrs,d0_clroff(*),d0_clrtab(*)
        integer iclr,ioff,itp,icr,nmat,imat,imat1,imat2,line,ierr
        character*132 cbuf
 
        line=8
        write(cbuf,*) 'lower d color table, # colors=',d0_nclrs
        call writloga('default',0,cbuf,0,ierr)
        if (d0_nclrs.gt.0) then
          write(cbuf,'(a)')'  iclr: itp, icr, (imt1(imat),imat=1,nmat)'
          call writloga('default',0,cbuf,0,ierr)
          do iclr=1,d0_nclrs
            ioff=d0_clroff(iclr)
            itp=d0_clrtab(ioff+1)
            icr=d0_clrtab(ioff+2)
            nmat=d0_clrtab(ioff+3)
            imat1=1
            imat2=imat1+line-2
            if (imat2.gt.nmat) imat2=nmat
            write(cbuf,'(i7,a,2i4,a,7i8)') iclr,':',itp,icr,';'
     &            ,(d0_clrtab(ioff+3+imat),imat=imat1,imat2)
            call writloga('default',0,cbuf,0,ierr)
10          if (imat2.lt.nmat) then
               imat1=imat2+1
               imat2=imat1+line-1
               if (imat2.gt.nmat) imat2=nmat
               write(cbuf,'(9x,8i8)') (d0_clrtab(ioff+3+imat)
     &                                     ,imat=imat1,imat2)
               call writloga('default',0,cbuf,0,ierr)
               goto 10
            endif
          enddo
        endif
 
        return
        end
C-----------------------------------------------------------------------
C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|
C-----------------------------------------------------------------------
C #####################################################################
C   create_d2_elm_d0_lower_d_lg
C
C   PURPOSE -
C
C      create the "2-up" d2 element to d0 element,edge translation
C
C   INPUT ARGUMENTS -
C
C        d2_nelements,d1_nef_cmo,d0_nef_cmo
C        d2_elm_d1,d1_elm_d0,d0_itettyp
C
C   OUTPUT ARGUMENTS -
C
C        d2_elm_d0
C        ierror - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C   USAGE NOTES / CAVEATS / CHANGES -
C
C    ignores negative d2_itetclr
C
C #####################################################################
 
        subroutine create_d2_elm_d0_lower_d_lg(
     &          d2_nelements,d1_nef_cmo,d0_nef_cmo,d0_nee_cmo
     &         ,d2_elm_d1,d1_elm_d0,d0_itettyp
     &         ,d2_elm_d0
     &         ,ierror)
c ........................................................................
        implicit none
        include 'local_element.h'
 
        integer d2_nelements,d1_nef_cmo,d0_nef_cmo,ierror
        integer d2_elm_d1(*),d1_elm_d0(*),d0_itettyp(*)
     &         ,d2_elm_d0(*)
 
        integer jel,iel,it,jface,iface,d0_nee_cmo
 
        ! element to edges translation
        do jel=1,d2_nelements
           it=d2_elm_d1(jel)
           iel=1+(it-1)/d1_nef_cmo
           jface=it-(iel-1)*d1_nef_cmo
           it=d1_elm_d0(iel)
           iel=1+(it-1)/d0_nef_cmo
           iface=it-(iel-1)*d0_nef_cmo
           d2_elm_d0(jel)=(iel-1)*d0_nee_cmo
     &          +ielmface2(jface,iface,d0_itettyp(iel))
        enddo
 
        ierror=0
        return
        end
 
C-----------------------------------------------------------------------
C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|
C-----------------------------------------------------------------------
C #####################################################################
C   create_d0d1_node_lower_d_lg
C
C   PURPOSE -
C
C      create the lower d to higher d node translation tables
C
C   INPUT ARGUMENTS -
C
C      standard d0 mesh info:
C          d0_nnodes,d1_nnodes,d0_node_topo,iparent,isn1
C      d_lower - the relative lower dimension for which
C                the translation is desired.
C
C   OUTPUT ARGUMENTS -
C
C        d0_node_d1,d1_node_d0 - the d0 node to lower d node translation tables.
C        ierror - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C   USAGE NOTES / CAVEATS / CHANGES -
C
C        For d0_node_d1, if it is zero, then there is no lower d child node,
C        if it is >0, then it is the value of the lower d child node
C        and if it is negative, then it's parent has the node as a lower d child.
C
C less clear notes to myself for code development .......................
 
c negative d0_node_topo: how to handle??
 
C #####################################################################
 
        subroutine create_d0d1_node_lower_d_lg(
     &          d0_nnodes,d1_nnodes,d_lower
     &         ,d0_node_topo,iparent,isn1
     &         ,d0_node_d1,d1_node_d0
     &         ,ierror)
c ........................................................................
        implicit none
        integer d0_nnodes,d1_nnodes,d_lower,ierror
        integer d0_node_topo(d0_nnodes)
     &         ,iparent(d0_nnodes),isn1(d0_nnodes)
     &         ,d0_node_d1(d0_nnodes),d1_node_d0(d1_nnodes)
 
        integer i,j,nnd
 
        do i=1,d1_nnodes
           d1_node_d0(i)=0
        enddo
 
        nnd=0
        do i=1,d0_nnodes
           if (i.eq.iparent(i)) then
 
              ! which way ....
              ! if (abs(d0_node_topo(i)).eq.d_lower) then
              if (d0_node_topo(i).eq.d_lower) then
 
                 nnd=nnd+1
                 d0_node_d1(i)=nnd
                 d1_node_d0(nnd)=i
 
                 ! cycle around isn chain
                 j=isn1(i)
                 do while (j.ne.0.and.j.ne.i)
                    d0_node_d1(j)=-nnd
                    j=isn1(j)
                 enddo
 
              else
 
                 d0_node_d1(i)=0
 
              endif
 
           endif
        enddo
 
        ierror=d1_nnodes-nnd
 
        return
        end
 
C-----------------------------------------------------------------------
C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|
C-----------------------------------------------------------------------
C #####################################################################
C   create_d0_elm_d1_lower_d_lg
C
C   PURPOSE -
C
C      create the "1-down" version of the element translation table.
C
C   INPUT ARGUMENTS -
C
C       standard mesh info for the d0 dimension:
C        d0_nelements,d0_nef_cmo,mbndry
C        d0_itettyp,d0_jtetoff,d0_jtet
C       standard mesh info for the d1 dimension:
C        d1_nelements,d1_elm_d0
C
C   OUTPUT ARGUMENTS -
C
C        d0_elm_d1 - the d0 to d1 translation
C                    for faces with no d1 child, d0_elm_d1=0
C                    for parent faces of d1 child iel's, d0_elm_d1
C                       points to the element number in the d1 structure
C                    for non-parent faces with d1 child iel, -d0_elm_d1
C                       points to the element number in the d1 structure
C
C        ierror - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C   USAGE NOTES / CAVEATS / CHANGES -
C
C        if the "d0" input refers to the actual d1 structure and
C        the "d1" inout refers to the actuak d2 structure, then
C        this code acn be used to create the d1_elm_d2 translation.
C
C less clear notes to myself for code development .......................
 
c negative d1_itetclr: how to handle??
 
C #####################################################################
 
        subroutine create_d0_elm_d1_lower_d_lg(
     &          d0_nelements,d1_nelements,d0_nef_cmo,mbndry
     &         ,d0_jtet_cycle_max
     &         ,d1_elm_d0,d0_itettyp,d0_jtetoff,d0_jtet
     &         ,d0_elm_d1
     &         ,ierror)
c ........................................................................
 
        implicit none
        include 'local_element.h'
 
        integer d0_nelements,d1_nelements,d0_nef_cmo,mbndry,ierror
     &         ,d0_jtet_cycle_max
        integer d0_elm_d1(*)
     &         ,d1_elm_d0(*),d0_itettyp(*),d0_jtetoff(*),d0_jtet(*)
 
        integer iel,jel,kel,iface,it,ityp,ioff,kface,icycle
 
        ! element to faces translation
 
        do iel=1,d0_nelements
           ityp=d0_itettyp(iel)
           ioff=d0_jtetoff(iel)
           do iface=1,nelmnef(ityp)
              d0_elm_d1(ioff+iface)=0
           enddo
        enddo
 
        do jel=1,d1_nelements
 
           ! mark parent face
           it=d1_elm_d0(jel)
           iel=1+(it-1)/d0_nef_cmo
           iface=it-(iel-1)*d0_nef_cmo
           ioff=d0_jtetoff(iel)
           d0_elm_d1(ioff+iface)=jel
 
           ! mark rest of jtet chain
           it=abs(d0_jtet(ioff+iface))
           if (mbndry.gt.0.and.it.ge.mbndry) it=it-mbndry
           kel=1+(it-1)/d0_nef_cmo
           iface=it-(kel-1)*d0_nef_cmo
           kface=iface
           icycle=0
           do while ((kel.ne.iel.or.kface.ne.iface) .and. it.gt.0
     &                .and. icycle.le.d0_jtet_cycle_max)
              icycle=icycle+1
              ioff=d0_jtetoff(kel)
              if (d0_elm_d1(ioff+iface).ne.0) goto 9999
              d0_elm_d1(ioff+iface)=-jel
              it=abs(d0_jtet(ioff+iface))
              if (mbndry.gt.0.and.it.ge.mbndry) then
                 it=it-mbndry
              endif
              kel=1+(it-1)/d0_nef_cmo
              iface=it-(kel-1)*d0_nef_cmo
           enddo
 
        enddo
 
        ierror=0
        return
 
9999    ierror=1
        return
        end
 
C-----------------------------------------------------------------------
C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|
C-----------------------------------------------------------------------
C #####################################################################
C   create_d0_elm_d2_lower_d_lg
C
C   PURPOSE -
C
C      create the 2-down version of the element translation table.
C
C   INPUT ARGUMENTS -
C
C       standard mesh info for the d0 dimension:
C          d0_nelements,d0_nee_cmo,d0_nef_cmo
C         ,d0_itettyp,d0_jtetoff,d0_itetoff,d0_jtet
C       standard mesh info for the d2 dimension:
C          d2_nelements
C       d2_elm_d0 - d2 to d0 translation
C
C   OUTPUT ARGUMENTS -
C
C        d0_elm_d2 - d0 to d2 translation
C                    for edges with no lower d child, d0_elm_d2=0
C                    for parent edge of lower d child iel, d0_elm_d2
C                       points to the element number in the d2 structure
C                    for non-parent edges with lower d child iel, -d0_elm_d2
C                       should points to the element number in the d2 structure
C                       but currently is is just 0
C
C        ierror - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C   USAGE NOTES / CAVEATS / CHANGES -
C
C        for now, only the parent edge is marked.
C        I need to add code to cycle around and add the translation
C        on the child nodes.
C
C less clear notes to myself for code development .......................
 
c negative d2_itetclr: how to handle??
c Note:
c   in 3D: iedgeoff=itetoff+jtetoff-2*(iel-1)
c          as in 3-d, nnodes+nface-2=nedges
c   in 2D: iedgeoff=itetoff=jtetoff
c   in 0D/1D: (no edges)
c   dang: but what about mixed-d meshes? -> just use neecmo?, or d0_edgeoff?
c   for now, just pack d0_elm_d2 using neecmo
 
c how about an edgetet,edgetetoff ??
c could create with current sub_geniee_cmo and using the effective itet
c of all the faces...
c better to do from known jtet re huge number of "elements"...
c would be convenient if needed, but more storage...
 
C #####################################################################
 
        subroutine create_d0_elm_d2_lower_d_lg(
     &          d0_nelements,d2_nelements,d0_nee_cmo,d0_nef_cmo
     &         ,d2_elm_d0,d0_itettyp,d0_jtetoff,d0_itetoff,d0_jtet
     &         ,d0_elm_d2
     &         ,ierror)
 
c ........................................................................
 
        implicit none
        include 'local_element.h'
 
        integer d0_nelements,d2_nelements
     &         ,d0_nee_cmo,d0_nef_cmo,ierror
        integer d0_elm_d2(*),d0_itetoff(*)
     &         ,d2_elm_d0(*),d0_itettyp(*),d0_jtetoff(*),d0_jtet(*)
 
        integer iel,jel,iedge,it,ityp,ioff
 
        ! element to edges translation
        ! only get here if d0=3d, d2=1d
        ! assume 3d parent is NOT network?
        ! or just create generalized "get elements around edge/node"...
 
        do iel=1,d0_nelements
           ityp=d0_itettyp(iel)
           !no! ioff=d0_jtetoff(iel)+d0_itetoff(iel)-2*(iel-1)
           ioff=(iel-1)*d0_nee_cmo
           do iedge=1,nelmnef(ityp)
              d0_elm_d2(ioff+iedge)=0
           enddo
        enddo
 
        do jel=1,d2_nelements
 
           ! mark parent edge
           it=d2_elm_d0(jel)
           iel=1+(it-1)/d0_nee_cmo
           iedge=it-(iel-1)*d0_nee_cmo
           ioff=d0_jtetoff(iel)
           d0_elm_d2(ioff+iedge)=jel
 
           ! mark rest of elements sharing this edge
           ! face with this edge -> ielmedge2
 
           ! iface=ielmedge2(1,iedge,iel)+d0_nef_cmo*(iel-1)
           ! (etc: copy from "get elements around...")
 
           ! heck: just don't mark them for now...
           ! and create this when I actually need it.
           ! stop 'not finished'
 
        enddo
 
        ierror=0
        return
 
9999    ierror=1
        return
        end
 
C-----------------------------------------------------------------------
C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|
C-----------------------------------------------------------------------
C #####################################################################
C   order_surface_lower_d_lg
C
C   PURPOSE -
C
C      find sign need to order elements within a given surface consistently
C
C   INPUT ARGUMENTS -
C
C      standard mesh info for the current topological class:
C        nelements,nef_cmo,mbndry
C        iparent,itettyp,itetoff,jtetoff,itet,jtet
C
C   OUTPUT ARGUMENTS -
C
C        order  - +1 if orientation of the given element is correct,
C                 -1 if it should be flipped in order for the
C                    surface to be ordered consistently
C        ierror - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C   USAGE NOTES / CAVEATS / CHANGES -
C
C less clear notes to myself for code development .......................
 
c oh dang: have to redo jtet as well as itet....
c which might affect other things don't know about here
c -> just return in "order" a +/-1 to indicate if flipped
c order=0 on return means that couldn't decide order
c    as not allowed element typ (tri,qud,lin)
c The lowest element number in a given piece of surface determines
c the "positive" direction.
 
c for a 2d mobius strip this will result in a "boundary"
c between the two equivalent orientations, but otherwise
c it will be ordered.
 
C #####################################################################
 
        subroutine order_surface_lower_d_lg(
     &              nelements,nef_cmo,mbndry
     &             ,iparent,itettyp,itetoff,jtetoff,itet,jtet
     &             ,order
     &             ,ierror)
 
c ........................................................................
        implicit none
        include 'local_element.h'
 
        integer nelements,nef_cmo,mbndry,ierror
        integer itettyp(*),itetoff(*),jtetoff(*),itet(*),jtet(*)
     &         ,iparent(*),order(*)
        character*32 isubname
 
        pointer (ip_isearch,isearch)
        integer isearch(*)
 
        integer jtyp,ktyp,iel,jel,kel,ioff,joff,jface,kface
     &         ,jt,ierr,nsearch,j,k
 
c -----------------------------------------------------
        isubname="order_surface_lower_d_lg"
 
        do iel=1,nelements
           order(iel)=0
        enddo
 
        call mmggetbk('isearch',isubname,ip_isearch,nelements,1,ierr)
        if (ierr.ne.0) goto 9999
 
        do iel=1,nelements
           jtyp=itettyp(iel)
           if (order(iel).ne.0 .or.
     &           (jtyp.ne.ifelmlin.and.jtyp.ne.ifelmtri
     &                          .and.jtyp.ne.ifelmqud  ) ) goto 1000
           nsearch=0
           order(iel)=1
           jel=iel
100        joff=jtetoff(jel)
           ioff=itetoff(jel)
           jtyp=itettyp(jel)
           ! order the neighbors, and add them to the search bin
           do jface=1,nelmnef(jtyp)
              jt=jtet(joff+jface)
              if ((mbndry.gt.0.and.jt.lt.mbndry)
     &                 .or.(mbndry.eq.0.and.jt.gt.0)) then
                 kel=1+(jt-1)/nef_cmo
                 kface=jt-(kel-1)*nef_cmo
                 ktyp=itettyp(kel)
                 if (order(kel).eq.0 .and. (jtyp.eq.ifelmlin
     &                  .or.jtyp.eq.ifelmtri.or.jtyp.eq.ifelmqud)) then
                    nsearch=nsearch+1
                    isearch(nsearch)=kel
                    if (kel.eq.ifelmlin.and.jel.eq.ifelmlin) then
                       j=ielmface1(1,jface,jtyp)
                       k=ielmface1(1,kface,ktyp)
                       if (j.eq.k) then
                          order(kel)=-order(jel)
                       else
                          order(kel)=order(jel)
                       endif
                    elseif (jel.eq.ifelmlin.or.kel.eq.ifelmlin) then
                       ! goto 9999 ! shouldn't have lines joining surfaces
                       ! press on .... remove kel from current search list
                       nsearch=nsearch-1
                    else
                       j=ielmface1(1,jface,jtyp)
                       j=iparent(itet(ioff+j))
                       k=ielmface1(1,kface,ktyp)
                       k=iparent(itet(itetoff(kel)+k))
                       if (j.eq.k) then
                          order(kel)=-order(jel)
                       else
                          order(kel)=order(jel)
                       endif
                    endif
                 endif
              endif
           enddo
           if (nsearch.gt.0) then
              jel=isearch(nsearch)
              nsearch=nsearch-1
              goto 100
           endif
1000       continue
        enddo
 
c -----------------------------------------------------
9000    call mmrelblk('isearch',isubname,ip_isearch,ierr)
        return
 
c -----------------------------------------------------
9999    call mmrelblk('isearch',isubname,ip_isearch,ierr)
        ierror=1
        write(*,*)
     &    'add coding to order surfaces consistently'
        return
        end
 
C-----------------------------------------------------------------------
C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|
C-----------------------------------------------------------------------
