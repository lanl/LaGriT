      subroutine msgtty(imsgout,msgtype,xmsgout,cmsgout,nwds,
     *   ierr2)
C
C #####################################################################
C
C     PURPOSE -
C
C        Process commands
C
C     INPUT ARGUMENTS -
C
C        input message - command string
C
C     OUTPUT ARGUMENTS -
C
C        ierror return - 0 for successful completion - -1 otherwise
C
C     CHANGE HISTORY -
C
C        $Log: msgtty.f,v $
C        Revision 2.00  2007/11/05 19:46:02  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.35   14 Jun 2007 14:31:08   tam
CPVCS    added new command crush_thin_tets
CPVCS    
CPVCS       Rev 1.34   05 Jun 2007 15:32:32   gable
CPVCS    Added call to COMPUTE module.
CPVCS    
CPVCS       Rev 1.33   05 Jul 2006 16:24:46   gable
CPVCS    Added user prompts if someone tries to use quit or end. The
CPVCS    prompt suggests they try the command finish. Also modified
CPVCS    help syntax to accept capital letters.
CPVCS    
CPVCS       Rev 1.32   09 Nov 2005 09:21:00   dcg
CPVCS    add commands related to face sets
CPVCS    
CPVCS       Rev 1.31   02 Aug 2005 08:47:38   gable
CPVCS    Add filter / element options.
CPVCS    
CPVCS       Rev 1.30   18 Jan 2005 08:16:58   gable
CPVCS    Added option to call calc_rdist module. The code was already
CPVCS    there, it was just not installed in msgtty.
CPVCS    
CPVCS       Rev 1.29   30 Mar 2004 14:22:30   gable
CPVCS    Add geniee option to check and flip normal vectors of tri/quad mesh.
CPVCS    
CPVCS       Rev 1.28   01 Nov 2002 13:06:04   gable
CPVCS    Added metis and create_graph commands.
CPVCS    
CPVCS       Rev 1.27   11 Apr 2002 15:33:24   gable
CPVCS    Added support of LOOP command.
CPVCS    
CPVCS       Rev 1.26   18 Mar 2002 09:58:12   dcg
CPVCS    add bleed_color command
CPVCS    
CPVCS       Rev 1.25   24 Dec 2001 14:35:54   kuprat
CPVCS    Added call to SETHESSIAN.
CPVCS    
CPVCS       Rev 1.24   20 Dec 2001 16:22:22   dcg
CPVCS    add mode command
CPVCS    
CPVCS       Rev 1.23   28 Nov 2001 18:21:24   tam
CPVCS    enable call to squeeze command for cropping 2d planes
CPVCS    
CPVCS       Rev 1.22   23 Oct 2001 16:33:08   tam
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.21   03 Aug 2001 09:12:02   tam
CPVCS    added calls to new stack routines
CPVCS    added warnings for unsupported old read_trilayers codes
CPVCS    enable hextotet to have a default value of -2
CPVCS    add parameter rmvolume for hextotet command
CPVCS    
CPVCS       Rev 1.20   15 May 2001 14:41:24   kuprat
CPVCS    We now route all recon commands through the recon subroutine.
CPVCS    
CPVCS       Rev 1.19   02 May 2001 10:43:38   dcg
CPVCS    change typo on reorder call
CPVCS
CPVCS       Rev 1.18   24 Apr 2001 17:02:22   tam
CPVCS    allow command interpolate to call intrp
CPVCS
CPVCS       Rev 1.17   06 Apr 2001 13:50:44   gable
CPVCS    Added reorder command. Modified sort command.
CPVCS
CPVCS       Rev 1.16   20 Mar 2001 12:42:12   tam
CPVCS    added command intrp for grid to grid interpolation
CPVCS
CPVCS       Rev 1.15   01 Mar 2001 17:27:24   dcg
CPVCS    fix typo in name of reverse_elements routine
CPVCS
CPVCS       Rev 1.14   01 Mar 2001 17:14:02   dcg
CPVCS    add reverse elements
CPVCS
CPVCS       Rev 1.13   19 Sep 2000 16:17:28   tam
CPVCS    removed old calls to cmo_get_name and cmo_set_name in stack routine
CPVCS
CPVCS       Rev 1.12   19 Sep 2000 16:01:34   dcg
CPVCS    add call to perturb
CPVCS
CPVCS       Rev 1.11   30 May 2000 11:12:38   dcg
CPVCS    check for consistency in ndimension_geom and ndimension_topo
CPVCS    before calling connect routines
CPVCS
CPVCS       Rev 1.10   24 May 2000 09:13:46   dcg
CPVCS    remove calls to set_epsilon in regnpts and setpts
CPVCS
CPVCS       Rev 1.9   05 May 2000 09:08:44   jtg
CPVCS    nwrite added as argument to elmtest/jtet/nwrite
CPVCS    command, the default being nwrite=-20 (set to -|nwrite|
CPVCS    so that info spit to screen), and elmtest equivalent
CPVCS    to elmtest/jtet/20 (=elmtest/jtet/-20)
CPVCS
CPVCS       Rev 1.8   04 May 2000 16:16:42   dcg
CPVCS    add verify_edges command
CPVCS
CPVCS       Rev 1.7   04 May 2000 12:30:16   dcg
CPVCS    do not reset ipointj after call to hextotet_hybrid
CPVCS
CPVCS       Rev 1.6   Fri Apr 07 10:18:52 2000   nnc
CPVCS    Fixed calls to cmo_get_info which were missing the "call"paren.
CPVCS
CPVCS       Rev 1.5   Thu Apr 06 09:25:04 2000   dcg
CPVCS    replace get_info_i and set_info_i calls
CPVCS
CPVCS       Rev 1.4   14 Mar 2000 14:57:02   dcg
CPVCS    add command geometry
CPVCS
CPVCS       Rev 1.3   07 Mar 2000 09:37:46   tam
CPVCS    added command quadlayertotet and stack/quadlayer
CPVCS    mread command is changed to stack
CPVCS
CPVCS       Rev 1.2   09 Feb 2000 09:52:30   gable
CPVCS    RVG Added option to extract surface mesh.
CPVCS
CPVCS       Rev 1.1   Tue Feb 08 13:09:20 2000   dcg
CPVCS
CPVCS       Rev 1.0   Tue Feb 08 12:53:22 2000   dcg
CPVCS    Initial revision.
CPVCS
CPVCS       Rev 1.211   18 Jan 2000 08:50:20   gable
CPVCS     Changed intersectelements command to intersect_elements.
CPVCS     For backwards compatibility left in intersectelements syntax.
CPVCS     Also changed subroutine call name to reflect change in name
CPVCS     of subroutine which drives the intersect routines.
CPVCS
CPVCS       Rev 1.210   Fri Dec 10 10:54:32 1999   dcg
CPVCS    remove old code - replace reference to X3D
CPVCS
CPVCS       Rev 1.209   08 Dec 1999 09:37:30   llt
CPVCS    testing pvcs
CPVCS
CPVCS       Rev 1.209   08 Dec 1999 09:36:12   llt
CPVCS    No change.
CPVCS
CPVCS       Rev 1.208   Wed Dec 08 03:50:16 1999   kuprat
CPVCS    We now support 'extract/skeleton'.
CPVCS
CPVCS       Rev 1.207   Wed Dec 01 09:12:34 1999   jtg
CPVCS    as geniee can now handle meshes with parent/child isn chains,
CPVCS    removed lines storing and resetting itet, as well as re-checking
CPVCS    that geniee_cmo got the itetclr interfaces correct, when idsb='geniee'.
CPVCS
CPVCS       Rev 1.206   Tue Nov 30 16:48:04 1999   jtg
CPVCS    lower d command interface added
CPVCS
CPVCS       Rev 1.205   Tue Nov 30 09:15:16 1999   tam
CPVCS    added command intersectelements
CPVCS
CPVCS       Rev 1.204   Wed Nov 10 09:24:50 1999   dcg
CPVCS    remove include cnames.h
CPVCS
CPVCS       Rev 1.203   Fri Nov 05 13:27:22 1999   dcg
CPVCS    remove dictionary dependencies
CPVCS
CPVCS       Rev 1.202   Mon Sep 20 14:57:02 1999   jtg
CPVCS    "gcolor" made equivalent to "colormap/isn"
CPVCS
CPVCS       Rev 1.201   Fri Sep 17 10:11:02 1999   jtg
CPVCS    gcolor command link to neighbor_recolor_lg added
CPVCS
CPVCS       Rev 1.200   Mon Aug 30 11:28:24 1999   dcg
CPVCS    get rid of call to mathex3d
CPVCS
CPVCS       Rev 1.199   Thu Aug 26 15:02:04 1999   dcg
CPVCS    really get rid of solve poisson
CPVCS
CPVCS       Rev 1.198   Thu Aug 26 14:54:40 1999   dcg
CPVCS    remove calls to solve_poisson, matbld2 and matbld3
CPVCS
CPVCS       Rev 1.197   Wed Aug 25 13:49:20 1999   kuprat
CPVCS    Put in call to POPCOMPONENTS_LG.
CPVCS
CPVCS       Rev 1.196   Tue Aug 10 10:36:20 1999   dcg
CPVCS    add call for rmpoint/sparse
CPVCS
CPVCS       Rev 1.195   Fri Jul 23 09:12:10 1999   dcg
CPVCS    replace rz commands with createpts
CPVCS
CPVCS       Rev 1.194   Thu Jul 15 09:44:22 1999   dcg
CPVCS    add rzamr commmand
CPVCS    add optional syntax for ranpts (rzran)
CPVCS
CPVCS       Rev 1.193   Sat Jul 10 21:51:52 1999   jtg
CPVCS    added ranpts to randomly distribute points in a geometry
CPVCS
CPVCS       Rev 1.192   Fri Jul 09 10:08:14 1999   dcg
CPVCS    add arguments to triangulate command
CPVCS
CPVCS       Rev 1.191   Fri Jun 25 09:43:02 1999   dcg
CPVCS    add call for rzv command
CPVCS
CPVCS       Rev 1.190   Tue Apr 13 10:49:56 1999   dcg
CPVCS    change scale to scale_lg to avoid name conflict
CPVCS
CPVCS       Rev 1.189   Sun Apr 11 23:51:40 1999   kuprat
CPVCS    Put in call to POPCONES_LG.
CPVCS
CPVCS       Rev 1.188   Fri Apr 02 09:51:22 1999   nnc
CPVCS    Added colormap command.
CPVCS
CPVCS       Rev 1.187   Fri Mar 26 13:36:34 1999   dcg
CPVCS    add 2d connect call
CPVCS
CPVCS       Rev 1.186   Mon Feb 22 16:15:32 1999   dcg
CPVCS    rewrite of command processing to allow for recursion
CPVCS
CPVCS       Rev 1.182   Wed Jan 06 15:09:16 1999   dcg
CPVCS    add triangulate command
CPVCS
CPVCS       Rev 1.181   Mon Dec 21 13:51:50 1998   kuprat
CPVCS    Put in call to RMTETLESS.
CPVCS
CPVCS       Rev 1.180   Wed Dec 02 09:41:36 1998   nnc
CPVCS    Added NETWORK option to EXTRACT.
CPVCS
CPVCS       Rev 1.179   Mon Oct 26 09:38:50 1998   dcg
CPVCS    remove decimate
CPVCS
CPVCS       Rev 1.178   Sun Oct 11 09:09:34 1998   gable
CPVCS     Added call to SORTBINS
CPVCS
CPVCS       Rev 1.177   Fri Sep 25 12:32:20 1998   kuprat
CPVCS    Put in call to RANKVOLUME.
CPVCS
CPVCS       Rev 1.176   Mon Sep 14 12:22:26 1998   tam
CPVCS    added settets/normal, dbl_to_sngl, trilayer/derive
CPVCS
CPVCS       Rev 1.175   Mon Aug 31 12:26:08 1998   dcg
CPVCS    remove unused and undocumented command calls
CPVCS
CPVCS       Rev 1.174   Tue Aug 11 16:36:34 1998   dcg
CPVCS    add extrude, extract_line, offsetpara, bubble
CPVCS
CPVCS       Rev 1.173   Thu Jun 25 12:59:20 1998   dcg
CPVCS    put in missing quotes
CPVCS
CPVCS       Rev 1.172   Wed Jun 24 09:37:58 1998   gable
CPVCS    Added option setpts/ no_interface
CPVCS    This allows one to set the imt values of nodes without
CPVCS    getting any nodes labeled imt=interface. This is useful
CPVCS    if you do not want settets to create parent child chains
CPVCS    at interface points.
CPVCS
CPVCS       Rev 1.171   Wed Jun 03 08:43:46 1998   dcg
CPVCS    new merge implementation
CPVCS
CPVCS       Rev 1.170   Fri May 29 15:34:12 1998   dcg
CPVCS    add KDTREE and SETPTS/CLOSED_SURFACES/FREE|REFLECT command
CPVCS
CPVCS       Rev 1.169   Mon Apr 06 16:03:46 1998   tam
CPVCS    added trilayer/flip command
CPVCS
CPVCS       Rev 1.168   Tue Mar 17 09:41:42 1998   dcg
CPVCS    remove call to matbld0tet
CPVCS
CPVCS       Rev 1.167   Tue Mar 03 10:19:44 1998   dcg
CPVCS    add solve_poisson command
CPVCS
CPVCS       Rev 1.166   Thu Feb 26 12:14:58 1998   dcg
CPVCS    call wrapper routine for negative_aij command
CPVCS
CPVCS       Rev 1.165   Wed Feb 11 09:46:56 1998   dcg
CPVCS    add rzs/diamond option
CPVCS
CPVCS       Rev 1.164   Tue Jan 27 13:44:48 1998   dcg
CPVCS    add negative_coupling command
CPVCS
CPVCS       Rev 1.163   Wed Jan 14 13:06:52 1998   mcgavran
CPVCS    Added BOUNDARY command
CPVCS
CPVCS       Rev 1.162   Wed Dec 10 12:29:26 1997   mcgavran
CPVCS    For MATH command, don't ser ierr2 = 0; preserve the returned
CPVCS    value from the subroutin
CPVCS
CPVCS       Rev 1.161   Mon Dec 08 16:27:58 1997   mcgavran
CPVCS    Added MATH command
CPVCS
CPVCS       Rev 1.160   Mon Nov 24 16:36:32 1997   dcg
CPVCS    use geom.h and calls to get_regions, get_mregions, get_surfaces
CPVCS    to access geometry data - start to isolate integer*8 dependencies
CPVCS
CPVCS       Rev 1.159   Fri Oct 10 12:25:56 1997   tam
CPVCS    No change.
CPVCS
CPVCS       Rev 1.158   Mon Oct 06 17:20:22 1997   tam
CPVCS    added commands developed for geologic applications
CPVCS
CPVCS       Rev 1.157   Fri Sep 26 14:30:58 1997   dcg
CPVCS    remove call to nn3dn_sa and clean up duplicates
CPVCS
CPVCS       Rev 1.156   Tue Sep 02 23:07:54 1997   kuprat
CPVCS    Added RADAPT command.
CPVCS
CPVCS       Rev 1.155   Thu Aug 28 16:40:18 1997   dcg
CPVCS    add call to setsize
CPVCS
CPVCS       Rev 1.154   Wed Aug 20 11:14:08 1997   dcg
CPVCS    set epsilonr to machine epsilon
CPVCS
CPVCS       Rev 1.153   Wed Aug 20 09:32:44 1997   dcg
CPVCS    Add setsize command
CPVCS
CPVCS       Rev 1.152   Tue Aug 19 12:58:58 1997   dcg
CPVCS    add connect command
CPVCS
CPVCS       Rev 1.151   Tue Jul 01 21:47:56 1997   kuprat
CPVCS    Bracketed call to REGNPTS by calls to CHGLOCL and CHGNORM
CPVCS    (coordinate change calls).
CPVCS
CPVCS       Rev 1.150   Mon Jun 16 10:38:16 1997   kmb
CPVCS    Put in call commands for upscale and derefine commands.
CPVCS
CPVCS       Rev 1.149   Thu Jun 05 15:32:54 1997   dcg
CPVCS    fix typo with comment on wrong line
CPVCS
CPVCS       Rev 1.148   Thu Jun 05 09:47:26 1997   dcg
CPVCS    add massage command
CPVCS
CPVCS       Rev 1.147   Mon May 05 17:12:38 1997   dcg
CPVCS    remove statement setting nnfreq for merging
CPVCS
CPVCS       Rev 1.146   Mon Apr 14 16:54:10 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.145   Tue Apr 08 12:57:12 1997   kmb
CPVCS    Added command for dopmat routine
CPVCS
CPVCS       Rev 1.144   Mon Apr 07 14:55:54 1997   het
CPVCS    Add the rtt dumpfile option
CPVCS
CPVCS       Rev 1.143   Thu Mar 06 21:50:52 1997   het
CPVCS    Add the average_coord and average_coord1 commands
CPVCS
CPVCS       Rev 1.142   Sun Feb 23 10:40:00 1997   het
CPVCS    Add the qlink read command.
CPVCS
CPVCS       Rev 1.141   Fri Jan 24 13:40:54 1997   het
CPVCS    Add the "voronoi_stor" command.
CPVCS
CPVCS       Rev 1.140   Mon Dec 09 09:04:02 1996   het
CPVCS    Save the ipointi and ipointj counters in the rzbrick3 call
CPVCS
CPVCS       Rev 1.139   Tue Dec 03 12:56:02 1996   het
CPVCS    Add a dummy call for testing some recon stuff.
CPVCS
CPVCS       Rev 1.138   Thu Nov 21 19:06:34 1996   het
CPVCS    modify the gtg command
CPVCS
CPVCS       Rev 1.137   Thu Oct 10 08:38:50 1996   het
CPVCS    Add the "dump/flag" command.
CPVCS
CPVCS       Rev 1.136   Tue Sep 03 11:11:14 1996   dcg
CPVCS    fix typo on call to  rotateln
CPVCS
CPVCS       Rev 1.135   Tue Sep 03 08:54:40 1996   dcg
CPVCS    add quality call
CPVCS
CPVCS       Rev 1.134   Wed Jul 24 17:30:36 1996   dcg
CPVCS    use mesh object 'nef' attribute to pack element and
CPVCS    face number into jtet array
CPVCS
CPVCS       Rev 1.133   Wed Jul 03 10:36:36 1996   het
CPVCS    Correct a problem with the cmo_tet selection in the hextotet command.
CPVCS
CPVCS       Rev 1.132   Thu Jun 27 15:25:16 1996   dcg
CPVCS    add constrainv command to create xcontab table as needed
CPVCS
CPVCS       Rev 1.131   Mon Jun 03 11:13:56 1996   het
CPVCS    Add the partition command and routine.
CPVCS
CPVCS       Rev 1.131   Mon Jun 03 09:24:16 1996   het
CPVCS    Add the partition command.
CPVCS
CPVCS       Rev 1.130   Fri May 24 15:35:52 1996   dcg
CPVCS    replace hextotet_att with hextotet_hybrid
CPVCS
CPVCS       Rev 1.129   Wed May 22 07:00:28 1996   het
CPVCS    Add the elmtest/jtet command.
CPVCS
CPVCS       Rev 1.128   Thu May 02 12:45:34 1996   dcg
CPVCS    remove cmo_release from before call to hextotet_hybrid
CPVCS
CPVCS       Rev 1.127   Tue Apr 02 02:22:54 1996   het
CPVCS    Delete some obsolete code in the rzbrick command
CPVCS
CPVCS       Rev 1.126   Tue Mar 05 12:39:26 1996   het
CPVCS    Add the search2d, perturb, and pyoungs calles.
CPVCS
CPVCS       Rev 1.125   Wed Feb 14 14:08:42 1996   ahmed
CPVCS    Add offset surface call
CPVCS
CPVCS       Rev 1.124   Wed Feb 14 09:05:14 1996   dcg
CPVCS    remove call to rzbrick2
CPVCS
CPVCS       Rev 1.123   Tue Jan 30 16:51:12 1996   dcg
CPVCS    enable hextotet_hybird and nn3dn_sa
CPVCS
CPVCS       Rev 1.122   Tue Jan 30 14:34:02 1996   dcg
CPVCS    add eltset command
CPVCS
CPVCS       Rev 1.121   Mon Jan 29 22:14:38 1996   het
CPVCS    Add the hextotet_hybrid routine
CPVCS
CPVCS       Rev 1.120   Wed Jan 03 16:41:08 1996   dcg
CPVCS    remove call to helpmg
CPVCS
CPVCS       Rev 1.119   12/05/95 08:20:06   het
CPVCS    Make changes for UNICOS
CPVCS
CPVCS       Rev 1.118   11/21/95 10:54:18   dcg
CPVCS    replace smooth3d with smooth and search2 with search
CPVCS
CPVCS       Rev 1.117   11/17/95 15:23:12   dcg
CPVCS    replace literal character strings in calls
CPVCS
CPVCS       Rev 1.116   11/16/95 17:06:32   het
CPVCS    Add the bingrid routine
CPVCS
CPVCS       Rev 1.115   11/14/95 17:24:42   dcg
CPVCS    add call to offset surfaces
CPVCS
CPVCS       Rev 1.114   11/14/95 17:06:16   het
CPVCS    Add the Voronoi_search and grid_to_grid mapping commands
CPVCS
CPVCS       Rev 1.113   11/09/95 08:28:32   dcg
CPVCS    add copyright include file
CPVCS
CPVCS       Rev 1.112   11/07/95 17:21:12   dcg
CPVCS    change flag to 2 in mmgetblk calls
CPVCS
CPVCS       Rev 1.111   10/22/95 13:17:10   het
CPVCS    Fix a memory management error in the merge command
CPVCS
CPVCS       Rev 1.110   10/18/95 11:42:48   het
CPVCS    Correct a problem merge
CPVCS
CPVCS       Rev 1.109   10/16/95 10:07:20   het
CPVCS    Correct merging error. Element types where not being set
CPVCS
CPVCS       Rev 1.108   10/05/95 15:46:24   het
CPVCS    Add the intrface refinement option
CPVCS
CPVCS       Rev 1.107   10/04/95 07:42:00   het
CPVCS    Correct the calling sequence arguments
CPVCS
CPVCS       Rev 1.106   09/29/95 09:13:10   het
CPVCS    Put in added attributes inheritance
CPVCS
CPVCS       Rev 1.105   09/14/95 02:07:04   het
CPVCS    Fix ipcmoprm errors
CPVCS
CPVCS       Rev 1.104   09/11/95 14:40:20   het
CPVCS    Change to the storage block based CMO stuff.
CPVCS
CPVCS       Rev 1.103   08/31/95 14:57:54   dcg
CPVCS    reset command
CPVCS
CPVCS       Rev 1.102   08/29/95 11:51:26   dcg
CPVCS    set length for names to 40 characters
CPVCS
CPVCS       Rev 1.101   08/24/95 15:21:22   dcg
CPVCS    add calls to field and smooth commands
CPVCS    remove call to smooth2d
CPVCS
CPVCS       Rev 1.100   08/23/95 12:56:54   dcg
CPVCS    move cmo calls to subroutine rz from msgtty
CPVCS
CPVCS       Rev 1.99   08/23/95 06:57:36   het
CPVCS    Remove the CMO prefix from SB-ids
CPVCS
CPVCS       Rev 1.98   08/15/95 18:20:04   het
CPVCS    Cleanup code and correct errors
CPVCS
CPVCS       Rev 1.97   08/10/95 16:18:02   dcg
CPVCS    pull off utility subroutines
CPVCS
CPVCS       Rev 1.96   07/17/95 16:01:10   dcg
CPVCS    activate coordsys changes for rm, rz commands
CPVCS
CPVCS       Rev 1.95   07/13/95 09:03:30   ejl
CPVCS    Cleaned up interface of rotatept, rotateln, copypts.
CPVCS
CPVCS       Rev 1.94   07/12/95 15:04:38   het
CPVCS    Fix the rotateln/rotatept commands
CPVCS
CPVCS       Rev 1.93   06/16/95 16:47:48   dcg
CPVCS    add calls to hsb2getx for real and integer data replacing call to hsb2geta
CPVCS
CPVCS       Rev 1.92   06/13/95 10:27:44   ejl
CPVCS    Fixed error
CPVCS
CPVCS       Rev 1.91   06/13/95 09:02:08   ejl
CPVCS    Cleaned up msgtty, calling arguments.
CPVCS
CPVCS       Rev 1.90   06/08/95 03:54:50   het
CPVCS    Add the call to the intersect_cmo command for jxf
CPVCS
CPVCS       Rev 1.89   06/07/95 16:58:08   het
CPVCS    Add the intersect command
CPVCS
CPVCS       Rev 1.88   06/07/95 15:30:22   het
CPVCS    Change character*32 idsb to character*132 idsb
CPVCS
CPVCS       Rev 1.87   06/05/95 10:37:08   het
CPVCS    Make changes for hybrid_grids
CPVCS
CPVCS       Rev 1.86   05/30/95 07:50:12   het
CPVCS    Replace mesh_object subroutine parameters by cmo-calls
CPVCS
CPVCS       Rev 1.85   05/26/95 13:12:56   het
CPVCS    Replace subroutine parameter list with subroutine calles.
CPVCS
CPVCS       Rev 1.84   05/24/95 16:20:38   het
CPVCS    Correct an error in the ipointi/ipointj regnpts
CPVCS
CPVCS       Rev 1.83   05/24/95 15:37:50   het
CPVCS    Add the ipointi, ipointj variables to the dictionary
CPVCS
CPVCS       Rev 1.82   05/23/95 09:35:42   het
CPVCS    Move external blockcom to initx3d from msgtty
CPVCS
CPVCS       Rev 1.81   05/23/95 06:48:24   het
CPVCS    Change dictionary so that they are CMO specific
CPVCS
CPVCS       Rev 1.80   05/17/95 14:52:38   dcg
CPVCS    change paramater list to do_extract - get npoints,ntets with cmo_get
CPVCS
CPVCS       Rev 1.79   05/15/95 13:35:58   het
CPVCS    Make changes to the regset and surfset routines
CPVCS
CPVCS       Rev 1.78   05/12/95 12:13:44   ejl
CPVCS    Fixed problem with epsilon in REGNPTS
CPVCS
CPVCS       Rev 1.77   05/12/95 11:27:08   ejl
CPVCS    Put error checking in SURFACE
CPVCS
CPVCS       Rev 1.76   05/11/95 14:08:24   ejl
CPVCS    Installed epslion routines
CPVCS
CPVCS       Rev 1.75   05/11/95 13:12:00   het
CPVCS    Add the coordsys command and change arguments to hextotet
CPVCS
CPVCS       Rev 1.74   05/04/95 22:45:18   het
CPVCS    Add the diffusion matrix routines
CPVCS
CPVCS       Rev 1.73   05/01/95 16:28:34   dcg
CPVCS    remove calls to initnbr
CPVCS
CPVCS       Rev 1.72   05/01/95 08:35:26   het
CPVCS    Modifiy all the storage block calles for long names
CPVCS
CPVCS       Rev 1.71   03/31/95 09:08:32   het
CPVCS    Add the buildid calles before all storage block calls
CPVCS
CPVCS       Rev 1.70   03/30/95 04:59:16   het
CPVCS    Change the storage block id packing and preidsb to buildid for long names
CPVCS
CPVCS       Rev 1.69   03/28/95 14:15:32   het
CPVCS    Add the dumpx3d_asci back again
CPVCS
CPVCS       Rev 1.68   03/28/95 12:34:18   het
CPVCS    Add the binary dumpx3d/readx3d commands and correct associated mm-errors.
CPVCS
CPVCS       Rev 1.67   03/23/95 22:59:04   het
CPVCS    Add the model routines and add the cmo name into the idsbs
CPVCS
CPVCS       Rev 1.66   03/17/95 21:08:32   het
CPVCS    Add the model and dictionary calles
CPVCS
CPVCS       Rev 1.65   03/16/95 11:01:16   dcg
CPVCS     add cmo calls to get ntets
CPVCS
CPVCS       Rev 1.64   03/15/95 13:48:52   dcg
CPVCS    increment temp memory for recon if needed
CPVCS
CPVCS       Rev 1.61   03/10/95 17:15:30   dcg
CPVCS     move get mesh object calls to subroutines
CPVCS
CPVCS       Rev 1.60   03/07/95 12:05:24   dcg
CPVCS     add external blockcom statement
CPVCS
CPVCS       Rev 1.59   03/03/95 12:08:14   dcg
CPVCS     Remove hard-wired 2dmesh mesh object in decimate, dump routines
CPVCS
CPVCS       Rev 1.58   02/23/95 23:16:42   het
CPVCS    Construct a default bounding box using the setpts command
CPVCS
CPVCS       Rev 1.57   02/23/95 20:15:14   het
CPVCS    Correct an error with iptable2 and mtable2
CPVCS
CPVCS       Rev 1.56   02/21/95 15:06:14   dcg
CPVCS    add access to user written command processors through user_sub
CPVCS
CPVCS       Rev 1.55   02/18/95 09:38:50   het
CPVCS    Add the nnblock routine and nnblock command
CPVCS
CPVCS       Rev 1.54   02/18/95 08:53:10   het
CPVCS    Add the edit routine and edit command
CPVCS
CPVCS       Rev 1.53   02/16/95 07:37:20   het
CPVCS    Corrected an error with calling nn3dn_bin
CPVCS
CPVCS       Rev 1.52   02/13/95 17:05:36   dcg
CPVCS    Remove obsolete call to decimate1
CPVCS
CPVCS       Rev 1.51   02/13/95 11:29:12   kuprat
CPVCS    Put in call to smooth3d
CPVCS
CPVCS       Rev 1.50   02/10/95 17:25:28   dcg
CPVCS
CPVCS       Rev 1.49   02/10/95 09:03:24   jxf
CPVCS    Changed parameter names to do_extract.  interfac is now intrface.
CPVCS
CPVCS       Rev 1.48   02/10/95 08:36:32   het
CPVCS    Delete the cmo_get_name stuff after the cmo_command call
CPVCS
CPVCS       Rev 1.47   02/08/95 18:10:58   het
CPVCS    Change the default on decimate to eliminate obtuse angles
CPVCS
CPVCS       Rev 1.46   02/06/95 08:29:12   het
CPVCS    Add the new search routines for nn3dn_bin
CPVCS
CPVCS       Rev 1.45   02/03/95 20:08:44   het
CPVCS    Add the search2 command for nn3dn2
CPVCS
CPVCS       Rev 1.44   01/31/95 18:45:48   het
CPVCS    Correct an error in the 2d-dumpavs routine
CPVCS
CPVCS       Rev 1.43   01/30/95 12:37:34   jxf
CPVCS    added do_extract
CPVCS
CPVCS       Rev 1.42   01/27/95 06:39:00   het
CPVCS    Add the geniee command for generating connectivity
CPVCS
CPVCS       Rev 1.41   01/26/95 11:43:20   het
CPVCS    Test of changes for command version of pvcs
CPVCS
CPVCS       Rev 1.40   01/26/95 11:42:06   het
CPVCS    No change.
CPVCS
CPVCS       Rev 1.39   01/26/95 10:39:32   het
CPVCS    Add ipointj parameter to the surface command.
CPVCS
CPVCS       Rev 1.38   01/23/95 23:17:04   kuprat
CPVCS    Small change to smooth2d call
CPVCS
CPVCS       Rev 1.37   01/23/95 12:43:50   het
CPVCS    Delete the cmo_increment command and calles.
CPVCS
CPVCS       Rev 1.36   01/19/95 09:47:24   het
CPVCS    Add the runtime parameters.
CPVCS
CPVCS       Rev 1.35   01/15/95 16:03:46   het
CPVCS    Add the refineint command
CPVCS
CPVCS       Rev 1.33   01/13/95 10:18:38   het
CPVCS    Remove the call for recon.
CPVCS
CPVCS       Rev 1.32   01/12/95 18:01:26   het
CPVCS    Add the dumpchad command.
CPVCS
CPVCS       Rev 1.31   01/11/95 22:22:22   het
CPVCS    Added the usersub1 and usersub2 commands.
CPVCS
CPVCS       Rev 1.30   01/11/95 21:46:36   het
CPVCS    Correct errors associated with the rzbrick and rzs commands.
CPVCS
CPVCS       Rev 1.29   01/10/95 17:13:02   het
CPVCS
CPVCS       Rev 1.28   01/09/95 09:31:32   het
CPVCS    Add ialias to the call to intradd.
CPVCS
CPVCS       Rev 1.27   01/06/95 15:02:48   dcg
CPVCS     fix bug where npoints appeared in place of nnodes as parameter to cmo rtns
CPVCS
CPVCS       Rev 1.26   01/04/95 22:03:18   llt
CPVCS    unicos changes (made by het)
CPVCS
CPVCS       Rev 1.25   01/03/95 16:46:12   het
CPVCS
CPVCS       Rev 1.24   01/02/95 12:54:04   het
CPVCS
CPVCS       Rev 1.23   12/27/94 23:06:16   het
CPVCS
CPVCS       Rev 1.22   12/26/94 11:41:12   het
CPVCS    Corrected an error when calling the stitch command.
CPVCS
CPVCS       Rev 1.21   12/26/94 10:35:30   het
CPVCS    Corrected errors with the header format.
CPVCS
CPVCS       Rev 1.20   12/24/94 12:54:00   het
CPVCS    Changed the memory command to handle existing cmo's correctly.
CPVCS
CPVCS       Rev 1.19   12/23/94 17:07:48   het
CPVCS    Fix the filter command.
CPVCS
CPVCS       Rev 1.18   12/21/94 18:52:20   het
CPVCS    Add the new hextotet command.
CPVCS
CPVCS       Rev 1.17   12/21/94 12:39:18   het
CPVCS    Add the recon3d call
CPVCS
CPVCS       Rev 1.16   12/21/94 12:29:54   het
CPVCS    Correct some cmo errors.
CPVCS
CPVCS       Rev 1.15   12/21/94 08:51:34   het
CPVCS    Add the hextotet command.
CPVCS
CPVCS       Rev 1.14   12/19/94 08:27:14   het
CPVCS    Add the "comdict.h" include file.
CPVCS
CPVCS       Rev 1.13   12/11/94 17:56:40   het
CPVCS    Added the addmesh and stitch commands.
CPVCS
CPVCS       Rev 1.12   12/09/94 22:37:06   het
CPVCS    Added the new cmo_ calles.
CPVCS
CPVCS       Rev 1.11   12/06/94 19:07:12   het
CPVCS    Add the "call cmo_get_name" to return the current mesh object name.
CPVCS
CPVCS       Rev 1.10   12/01/94 18:49:32   het
CPVCS    Removed the call to "set_tet_color" since this
CPVCS       function is now done by the mass command.
CPVCS
CPVCS       Rev 1.9   12/01/94 18:40:34   het
CPVCS    Change "cmo" calles to add data type
CPVCS    Alias the "decimate2d"  command to "decimate"
CPVCS    Alias the "settets" command to "mass"
CPVCS
CPVCS       Rev 1.8   11/23/94 11:57:28   dcg
CPVCS     extract code for refine function to separate subroutine refine
CPVCS
CPVCS       Rev 1.7   11/17/94 21:28:52   het
CPVCS    Added the "merge" command that will merge two points into one.
CPVCS
CPVCS       Rev 1.6   11/14/94 15:24:50   het
CPVCS
CPVCS       Rev 1.5   11/14/94 12:25:02   het
CPVCS
CPVCS       Rev 1.0   11/10/94 12:16:24   pvcs
CPVCS    Original version.
C
C ######################################################################
C
      implicit none
C
C ######################################################################
C
      include "machine.h"
      include "copyrite.h"
      include "chydro.h"
      include "neibor.h"
      include "consts.h"
      include "cmerge.h"
      include "local_element.h"
      include "copyrite.h"
      include 'commands_lg.h'
C
C ######################################################################
C
      integer imsgout(maxcmd_args),msgtype(maxcmd_args)
      real*8 xmsgout(maxcmd_args)
      character*(*) cmsgout(maxcmd_args)
      integer nwds,ierrw,lenidsb,icharlnf,len,ierr2,ierr1,ijob,
     *  ipointi,ipointj,icscode,npoints,npoints_save,itype,
     *  ntets,ioption,lencmo,itpcmo,numhex,nsdgeom,
     *  npoints2d,ntets2d,ipointi2d,ipointj2d,ilen,icount,
     *  nsdtopo,length,icmotype,ier,nwrite
      character*32 isubname,idsb,icommand,cmo,ifile
C
C     *****************************************************************
C
C
      integer ierror
C
      data npoints, ntets / 0, 0 /
      data npoints2d, ntets2d / 0, 0 /
C
      data ipointi, ipointj / 0, 0 /
      data ipointi2d, ipointj2d / 0, 0 /
 
      character*132 logmess
      character*32  cmo1, cmo2
      character*32 coption
C
C
      isubname='msgtty'
C
C        ***************************************************************
C        DO THE COMMAND.
C
         idsb=cmsgout(1)
         lenidsb=icharlnf(idsb)
C
         if(idsb(1:lenidsb) .eq. 'log') then
C
C           ************************************************************
C           log      : SPECIFY WHERE THE WRITTEN OUTPUT SHOULD GO.
C
C                   FORMAT : LOG/TTY/ON        (INTERACTIVE DEFAULT)
C                            LOG/BAT/ON        (ALWAYS DEFAULTED TO 'ON')
C                            LOG/BAT/FLUSH
C
            icommand=cmsgout(2)
            len=icharlnf(icommand)
            if(icommand(1:len).eq.'flush') then
               call writfls(icommand(1:len),ierr2)
            elseif(icommand(1:len).ne.'tty') then
               call writset('stat',icommand(1:len),cmsgout(3),
     *                      ierr2)
            endif
C

         elseif(idsb(1:lenidsb) .eq. 'memory') then
C           ************************************************************
C           MEMORY: user access to memory routines 
C
C             MEMORY: Adjust length of Mesh Object Arrays.
            if (nwds.lt.2) then 
              call memory(imsgout,xmsgout,cmsgout,msgtype,nwds,
     *                  ierr2)
C
            elseif (cmsgout(2)(1:5) .eq. 'print') then
              call mmprint()

            elseif (cmsgout(2)(1:6) .eq. 'verify') then
              call mmverify()

            elseif (cmsgout(2)(1:9) .eq. 'maxmalloc') then
              
c             use default isize and n_blk parameters 

              call max_mmgetblk(0,0,ierr2)

              write(logmess,'(a,i5)')'MEMORY/ maxmalloc: '//
     *       "maxmalloc returned error: ",ierr2

            else
              write(logmess,'(a,a)')'MEMORY: ' //
     *        'unrecognized command option: ',cmsgout(2) 

            endif

C
         elseif(idsb(1:4) .eq. 'dump') then
C
C           ************************************************************
C           DUMPoption: Writes all Dump Files.
C
            call writedump(imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
C
         elseif(idsb(1:4) .eq. 'read') then
C
C           ************************************************************
C           READoption: Reads all Dump Files.
C
            call readdump(imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
C
         elseif(idsb(1:lenidsb) .eq. 'lower_d') then
C
C           ************************************************************
C           create and manipulate lower d structures
C
            call control_lower_d_lg(imsgout,xmsgout,cmsgout,msgtype
     &                          ,nwds,ierr2)
C
         elseif ( idsb(1:lenidsb) .eq. 'colormap'
     &          .or. idsb(1:lenidsb) .eq. 'gcolor' ) then
C
C           ************************************************************
C           colormap: CREATE A MATERIAL COLORMAP
C               gcolor or colormap/isn: based on isn/point-wise connectivity
C               otherwise: based on face connectivity
C
            coption='add'
            if(nwds.ge.2) then
               if(msgtype(2).eq.3) coption=cmsgout(2)
            endif
 
            if (idsb(1:lenidsb) .eq. 'gcolor'
     &          .or. coption(1:icharlnf(coption)) .eq. 'gcolor'
     &          .or. coption(1:icharlnf(coption)) .eq. 'isn'
     &          .or. coption(1:icharlnf(coption)) .eq. 'isn1'
     &          .or. coption(1:icharlnf(coption)) .eq. 'pointwise'
     &          .or. coption(1:icharlnf(coption)) .eq. 'burn'
     &          .or. coption(1:icharlnf(coption)) .eq. 'unique'
     &          ) then
 
C use point-based connectivity algorithms
 
               call neighbor_recolor_lg(imsgout,xmsgout,cmsgout
     &                               ,msgtype,nwds,ierr2)
 
            else
 
C use face-based connectivity algorithm
 
               call cmo_get_name(cmo,ierror)
               if(nwds.ge.3) then
                  if(msgtype(3).eq.3) then
                     cmo=cmsgout(3)
                     if(cmo.eq.'-def-') call cmo_get_name(cmo,ierror)
                  endif
               endif
 
               call colormap_lg(cmo,coption,ierr2)
 
            endif
C
C
         elseif(idsb(1:lenidsb) .eq. 'coordsys') then
C
C           ************************************************************
C           coordsys: SET UP THE CURRENT COORDINATE SYSTEM.
C
            call coordsys(imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
C
         elseif(idsb(1:lenidsb) .eq. 'massage') then
C
C           ************************************************************
C           massage: AUTOMATIC REFINE/DE-REFINE.
C
            call massage(imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
C
         elseif(idsb(1:lenidsb) .eq. 'geometry') then
C
C           ************************************************************
C           geometry: create a geometry
C
            call geometry_create_lg(imsgout,xmsgout,cmsgout,msgtype,
     *           nwds,ierr2)
C
         elseif(idsb(1:lenidsb) .eq. 'popcomponents') then
C
C           ************************************************************
C           popcomponents: REMOVE SMALL TOPOLOGICAL COMPONENTS.
C
            call popcomponents_lg(imsgout,xmsgout,cmsgout,msgtype,nwds
     &         ,ierr2)
C
C
         elseif(idsb(1:lenidsb) .eq. 'popcones') then
C
C           ************************************************************
C           popcones: CORRECT TANGENT CONE CONNECTIVITY.
C
            call popcones_lg(imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
C
C
c        elseif(idsb(1:lenidsb).eq.'search2d') then
c           call search2d(imsgout,xmsgout,cmsgout,
c    *                    msgtype,nwds,ierr2)
C
C
         elseif(idsb(1:lenidsb).eq.'connect'.or.
     *     idsb(1:lenidsb).eq.'search' ) then
C
C            **************************************************************
C            connect: Use Watson point insertion to grid the mesh
C
           call cmo_get_name(cmo,ierror)
           call cmo_get_info('ndimensions_topo',cmo,
     *                           nsdtopo,length,icmotype,ierror)
           call cmo_get_info('ndimensions_geom',cmo,
     *                           nsdgeom,length,icmotype,ierror)
           if(nsdtopo.eq.2.and.nsdgeom.eq.2) then
             call connect2d_lg(imsgout,xmsgout,cmsgout,msgtype,nwds,
     *                              ierr2)
           elseif(nsdtopo.eq.3.and.nsdgeom.eq.3) then
             call connect(imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
           else
               write(logmess,'(a,i3,a,i3,a)')
     *       'ndimensions_topo=',nsdtopo,' ndimensions_geom=',
     *        nsdgeom,' not consistent - connect will not work'
               call writloga('default',0,logmess,0,ierrw)
           endif
C
C
         elseif(idsb(1:lenidsb).eq.'addmesh') then
C
C           ************************************************************
C           addmesh : ADDS TWO MESHES TOGETHER TO PRODUCE A THIRD MESH.
C
            call addmesh(imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
C
         elseif(idsb(1:lenidsb).eq.'extract'  .or.
     *          idsb(1:lenidsb).eq.'decimate' .or.
     *          idsb(1:lenidsb).eq.'decimate2d') then
            if(msgtype(2).eq.3) then
                  if(cmsgout(2)(1:5).eq.'plane' .or.
     *               cmsgout(2)(1:3).eq.'iso' .or.
     *               cmsgout(2)(1:5).eq.'table' .or.
     *               cmsgout(2)(1:3).eq.'int' .or.
     *               cmsgout(2)(1:7).eq.'network' .or.
     *               cmsgout(2)(1:8).eq.'skeleton' .or.
     *               cmsgout(2)(1:8).eq.'surfmesh' .or.
     *               cmsgout(2)(1:5).eq.'funct') then
                     call do_extract(imsgout(2),xmsgout(2),
     *                               cmsgout(2),msgtype(2),nwds-1,
     *                               ierr2)
                  endif
            else
C              ijob=imsgout(2)
C              iobtuse=1
C              if(nwds.ge.3) then
C                 if(msgtype(3).eq.1) then
C                    iobtuse =min(1,imsgout(3))
C                 else
C                    iobtuse =1
C                 endif
C              endif
C              call decimate(ijob,iobtuse)
               write(logmess,'(a)') 'Decimate no longer supported'
               call writloga('default',0,logmess,0,ierrw)
            endif
C
C
         elseif(idsb(1:lenidsb).eq.'radapt') then
C
C           ************************************************************
C           radapt  : Perform r-adaption on the mesh.
C
            call radapt(imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
C
         elseif(idsb(1:lenidsb).eq.'sethessian') then
C
C           ************************************************************
C           sethessian: Calculate Hessian matrix of field or function.
C
            call sethessian(imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
C
         elseif(idsb(1:lenidsb).eq.'rankvolume') then
C
C           ************************************************************
C           rank: rank the smallest volume elements in mesh.
C
            call rankvolume(imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
C
         elseif(idsb(1:lenidsb).eq.'smooth'.or.
     *          idsb(1:lenidsb).eq.'smooth3d') then
C
C           ************************************************************
C           smooth  : SMOOTH THE MESH.
C
            call smooth(imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
C
         elseif(idsb(1:lenidsb).eq.'field') then
C
C           ************************************************************
C           field  : PROCESS FIELD COMMANDS
C                   (COMPOSE, SCALE, VOLAVG, DUMPMFE)
C
            call field(imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
C
C
c         elseif(idsb(1:lenidsb).eq.'smoothint') then
C
C           ************************************************************
C
c            call smooth_interface(imsgout,xmsgout,cmsgout,msgtype,nwds,
c     *                            ierr2)
C
C
c         elseif(idsb(1:lenidsb).eq.'average_coord1') then
C
C           ************************************************************
C
c            call average_coord1()
C
c        elseif(idsb(1:lenidsb).eq.'average_coord') then
C
C           ************************************************************
C
c            call average_coord()
C
         elseif(idsb(1:lenidsb).eq.'quadxy') then
C
C           ************************************************************
C           quadxy   : SET UP A QUAD OF POINTS IN X-Y SPACE.
C
            ierr2=0
C
C           ---------------------------------------------------------------
C           TRANSFORM POINTS AND VELOCITIES TO LOCAL COORD. SYSTEM
C
C
            call cmo_get_name(cmo,ierror)
C
            call cmo_get_info('ipointi',cmo,
     *                      ipointi,lencmo,itpcmo,icscode)
            if (icscode .ne. 0) call x3d_error(isubname,'get_info_i')
            call cmo_get_info('ipointj',cmo,
     *                      ipointj,lencmo,itpcmo,icscode)
            if (icscode .ne. 0) call x3d_error(isubname,'get_info_i')
            if (normflgc .gt. 0) call chglocl(1,ipointj,1)
C
            call quadxy(imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
C
C           ---------------------------------------------------------------
C           TRANSFORM POINTS AND VELOCITIES TO NORMAL COORD. SYSTEM
C
            call cmo_get_name(cmo,ierror)
C
            call cmo_get_info('ipointi',cmo,
     *                      ipointi,lencmo,itpcmo,icscode)
            if (icscode .ne. 0) call x3d_error(isubname,'get_info_i')
            call cmo_get_info('ipointj',cmo,
     *                      ipointj,lencmo,itpcmo,icscode)
            if (icscode .ne. 0) call x3d_error(isubname,'get_info_i')
            if (normflgc .gt. 0) call chgnorm(1,ipointj,1)
C
C
         elseif(idsb(1:lenidsb).eq.'quadxyz') then
C
C           ************************************************************
C           quadxy   : SET UP A QUAD OF POINTS IN X-Y SPACE.
C
            ierr2=0
C
C           ---------------------------------------------------------------
C           TRANSFORM POINTS AND VELOCITIES TO LOCAL COORD. SYSTEM
C
            call cmo_get_name(cmo,ierror)
C
            call cmo_get_info('ipointi',cmo,
     *                      ipointi,lencmo,itpcmo,icscode)
            if (icscode .ne. 0) call x3d_error(isubname,'get_info_i')
            call cmo_get_info('ipointj',cmo,
     *                      ipointj,lencmo,itpcmo,icscode)
            if (icscode .ne. 0) call x3d_error(isubname,'get_info_i')
            if (normflgc .gt. 0) call chglocl(1,ipointj,1)
C
            call quadxyz(imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
C
C           ---------------------------------------------------------------
C           TRANSFORM POINTS AND VELOCITIES TO NORMAL COORD. SYSTEM
C
C
            call cmo_get_name(cmo,ierror)
C
            call cmo_get_info('ipointi',cmo,
     *                      ipointi,lencmo,itpcmo,icscode)
            if (icscode .ne. 0) call x3d_error(isubname,'get_info_i')
            call cmo_get_info('ipointj',cmo,
     *                      ipointj,lencmo,itpcmo,icscode)
            if (icscode .ne. 0) call x3d_error(isubname,'get_info_i')
            if (normflgc .gt. 0) call chgnorm(1,ipointj,1)
C
C
         elseif(idsb(1:lenidsb).eq.'refine') then
C
C           ************************************************************
C           refine  : REFINE THE MESH.
C
            call refine(imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
C
         elseif(idsb(1:lenidsb).eq.'refine_edge_3d') then
            call cmo_get_name(cmo,ierror)
            call refine_edge_3d(cmo,ierror)
C
         elseif(idsb(1:lenidsb).eq.'refine2d') then
            call cmo_get_name(cmo,ierror)
            call cmo_get_info('nnodes',cmo,
     *                        npoints,ilen,itype,ierror)
            npoints_save=npoints
C
            call cmo_get_info('ipointi',cmo,
     *                      ipointi,lencmo,itpcmo,icscode)
            if (icscode .ne. 0) call x3d_error(isubname,'get_info_i')
            call cmo_get_info('ipointj',cmo,
     *                      ipointj,lencmo,itpcmo,icscode)
            if (icscode .ne. 0) call x3d_error(isubname,'get_info_i')
C
            call refine2db()
            call cmo_get_name(cmo,ierror)
            call cmo_get_info('nnodes',cmo,
     *                        npoints,ilen,itype,ierror)
            ipointi=npoints_save+1
            ipointj=npoints
C
            call cmo_set_info('ipointi',cmo,
     *                      ipointi,1,1,icscode)
            if (icscode .ne. 0) call x3d_error(isubname,'get_info_i')
            call cmo_set_info('ipointj',cmo,
     *                      ipointj,1,1,icscode)
            if (icscode .ne. 0) call x3d_error(isubname,'get_info_i')
 
         elseif(idsb(1:lenidsb).eq.'derefine') then
C
C
C        ******************************************************************
C        derefine : CALL THE DEREFINE ROUTINE.
C
            call derefine(imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
 
C
         elseif(idsb(1:lenidsb).eq.'recon'    .or.
     *          idsb(1:lenidsb).eq.'recon2'   .or.
     *          idsb(1:lenidsb).eq.'recon2d'   .or.
     *          idsb(1:lenidsb).eq.'recon3d') then
C
C           ************************************************************
C           recon  : RECONNECT THE MESH.
C
            call recon(imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
C
c         elseif(idsb(1:lenidsb).eq.'reconadd') then
C
C           ************************************************************
C           recon  : RECONNECT THE MESH.
C
c            call reconadd()
C
         elseif(idsb(1:lenidsb).eq.'rmtetless') then
            call rmtetless(imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
c
         elseif(idsb(1:lenidsb).eq.'creatpts'.or.
     *          idsb(1:lenidsb).eq.'createpts' ) then
C        ******************************************************************
C        creatpts - controller for all the rz type commands
c
            call createpts_lg(imsgout,xmsgout,cmsgout,msgtype,nwds,
     *         ierr2)
c
         elseif(idsb(1:lenidsb).eq.'rzs') then
C        ******************************************************************
C        rzs      : call diamond or rzs routine to use an
c             an icosahedral node distribution on a sphere or
c             a diamond segment of the icosahedron
C
            call rzs_lg(imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
C
C
         elseif(idsb(1:lenidsb).eq.'rzbrick') then
C
C     ******************************************************************
C
C        rzbrick  : GENERATE A BRICK MESH AND CONNECTIVITY MATRIX.
c
            call rzbrick_lg(imsgout,xmsgout,cmsgout,msgtype,nwds,
     *                       ierr2)
C
         elseif(idsb(1:lenidsb).eq.'rz') then
C
C        ******************************************************************
C        rz       : CALL THE RATIO ZONING ROUTINE.
C
            call rz(imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
C
 
         elseif(idsb(1:lenidsb).eq.'rzv') then
C
C        ******************************************************************
C        rzv      : CALL THE node distribution but linear combination
C                         of basis vector set routine
C
            call rzv_lg(imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
C
 
         elseif(idsb(1:lenidsb).eq.'rzamr') then
C
C        ******************************************************************
C        rzamr      : CALL refine hex mesh node distribution by amr routine
C                         of basis vector set routine
C
            call rzamr_lg(imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
C
 
         elseif(idsb(1:lenidsb).eq.'ranpts'.or.
     *    idsb(1:lenidsb).eq.'rzran') then
C
C        ******************************************************************
C        ranpts   : CALL THE random node distribution routine
C
            call ranpts_lg(imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
C
 
         elseif(idsb(1:lenidsb).eq.'partition') then
C
C
C        ******************************************************************
C        partition:
C
            call partition(imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
C
 
         elseif(idsb(1:lenidsb).eq.'upscale') then
C
C
C        ******************************************************************
C        upscale  : CALL THE UPSCALING  ROUTINE.
C
            call upscale(imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
 
         elseif(idsb(1:lenidsb).eq.'zq') then
C
C           ************************************************************
C           zq : SET MESH QUANTITIES.
C
            call zq(imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
C
         elseif(idsb(1:lenidsb).eq.'quality') then
C
C           ************************************************************
C           quality : CALCULATE AND PRINT MESH QUALITY (ASPECT,VOL).
C
            call quality(imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
C
         elseif(idsb(1:lenidsb).eq.'rotateln') then
C
C           ************************************************************
C           rotateln : ROTATE PART OF THE MESH ABOUT A LINE.
C
            call rotateln(imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
C
         elseif(idsb(1:lenidsb).eq.'rmsphere') then
C
C           ************************************************************
C
C           rmsphere : REMOVE THE POINTS THAT LIE WITHIN A SPECIFIED
C                      SPHERE.
C
            if(nwds.ge.3) then
C
C              -----------------------------------------------------
C              TRANSFORM POINTS AND VELOCITIES TO LOCAL COORD. SYSTEM
C
C
               call cmo_get_name(cmo,ierror)
               call cmo_get_info('ipointi',cmo,
     *                         ipointi,lencmo,itpcmo,icscode)
               if (icscode .ne. 0) call x3d_error(isubname,'get_info_i')
               call cmo_get_info('ipointj',cmo,
     *                         ipointj,lencmo,itpcmo,icscode)
               if (icscode .ne. 0) call x3d_error(isubname,'get_info_i')
               if (normflgc .gt. 0) call chglocl(1,ipointj,1)
C
               call rmsphere(imsgout(2),xmsgout(2),cmsgout(2),
     *                       msgtype(2),nwds-1,ierr2)
C
C              ------------------------------------------------------------
C              TRANSFORM POINTS AND VELOCITIES TO NORMAL COORD. SYSTEM
C
               call cmo_get_name(cmo,ierror)
               call cmo_get_info('ipointi',cmo,
     *                         ipointi,lencmo,itpcmo,icscode)
               if (icscode .ne. 0) call x3d_error(isubname,'get_info_i')
               call cmo_get_info('ipointj',cmo,
     *                         ipointj,lencmo,itpcmo,icscode)
               if (icscode .ne. 0) call x3d_error(isubname,'get_info_i')
               if (normflgc .gt. 0) call chgnorm(1,ipointj,1)
C
            endif
C
C
         elseif(idsb(1:lenidsb).eq.'rmsurf') then
C
C           ************************************************************
C
C           rmsurf : REMOVE THE POINTS THAT LIE WITHIN A SPECIFIED
C                    SURFACE.
C
            ierr2=0
            call rmsurf(imsgout(2),xmsgout(2),cmsgout(2),msgtype(2),
     *                  nwds-1,ierr2)
C
         elseif(idsb(1:lenidsb).eq.'rmregion') then
C
C           ************************************************************
C
C           rmregion : REMOVE THE POINTS THAT LIE WITHIN A SPECIFIED
C                         REGION.
C
            ierr2=0
            call rmregion(imsgout(2),xmsgout(2),cmsgout(2),msgtype(2),
     *                    nwds-1,ierr2)
C
         elseif(idsb(1:lenidsb).eq.'rmmat') then
C
C           ************************************************************
C
C           rmmat    : REMOVE THE POINT OF A SPECIFIED MATERIAL NUMBER.
C
            if(nwds.ge.2) then
               call rmmat(imsgout(2),xmsgout(2),cmsgout(2),msgtype(2),
     &                    nwds-1,ierr2)
            endif
C
         elseif(idsb(1:lenidsb).eq.'rmpoint') then
C
C           ************************************************************
C
C           rmpoint  : REMOVE A SPECIFIED LIST OF POINTS AND ASSOCIATED
C                      TETRAHEDRA.
C
            if(nwds.ge.2.and.msgtype(2).eq.3.and.
     *           cmsgout(2)(1:icharlnf(cmsgout(2))).eq.'sparse') then
               call sparse_cmd_lg(imsgout,xmsgout,cmsgout,
     *               msgtype,nwds,ierr2)
            else
               call rmpoint(imsgout(2),xmsgout(2),cmsgout(2),msgtype(2),
     *                   nwds-1,ierr2)
            endif
C
         elseif(idsb(1:lenidsb).eq.'rm') then
C
C           ************************************************************
C           rm       : REMOVE A SPECIFIED LIST OF POINTS BASED ON POINT
C                      NUMBER AND GEOMETRY FACTORS.
C
            call rm(imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
C
         elseif(idsb(1:lenidsb).eq.'scale') then
C
C           ************************************************************
C           scale  :  SCALE THE COORDINATE DIRECTIONS.
C
            call scale_lg(imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
C
         elseif(idsb(1:lenidsb).eq.'rotatept') then
C
C           ************************************************************
C           rotatept : ROTATE PART OF THE MESH ABOUT A CENTER POINT.
C
            call rotatept(imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
C
         elseif(idsb(1:lenidsb).eq.'compute') then
C
C           ************************************************************
C           compute : COMPUTE A NEW MESH ATTRIBUTE.
C
            call compute(imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
C
         elseif(idsb(1:lenidsb).eq.'crush_thin_tets') then
C
C           ************************************************************
C           crush_thin_tets : MERGE THIN TETS BASED ON MIN TET ALTITUDE 
C
            call crush_thin_tets(imsgout,xmsgout,cmsgout,msgtype,
     *         nwds,ierr2)
C
         elseif(idsb(1:lenidsb).eq.'copypts') then

C
C           ************************************************************
C           copypts : COPY PART OF THE MESH.
C
            call copypts(imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
C
         elseif(idsb(1:lenidsb).eq.'constrainv') then
C
C           ************************************************************
C           constrainv : MAKE XCONTAB ATTRIBUTE FOR MESH OBJECT
C                           3X3 VELOCITY CONSTRAINT
C                           OPTIONS ARE CLEAR, DEFAULT, AREA_VECTOR AND
C                           AREA_NORMAL
C
            call constrainv(imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
C
         elseif(idsb(1:lenidsb).eq.'mass'.or.
     *          idsb(1:lenidsb).eq.'settets') then
C
 
           if(cmsgout(2)(1:lenidsb).eq.'normal') then
C           ***************************************************************
C           settets/normal  : ASSIGNS INTEGER ID TO ITETCLR
C                             BASED ON 26 POSSIBLE NORMAL DIRECTIONS
C                             OF A CUBE
c           this should be incorporated into the intradd routine
            call assign_color_normal(
     *           imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
 
           else
C           ***************************************************************
C           settets  : ADD THE COUPLED (CHILD) INTERFACE POINTS TO THE
C                      COUPLED CHAINS FOR EACH PARENT INTERFACE POINT.
C
            call intradd(imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
           endif
C
         elseif(idsb(1:lenidsb).eq.'intersect') then
C
C           ***************************************************************
C           intersect :
C
            call intersect_cmo(imsgout(2),xmsgout(2),cmsgout(2),
     *                      msgtype(2),nwds-1,ierr2)
C
         elseif((idsb(1:lenidsb).eq.'intersectelements') .or.
     *          (idsb(1:lenidsb).eq.'intersect_elements'))then
C
C           ************************************************************
C           intersect_elements : IDENTIFY ELEMENTS OF ONE GRID INTERSECTED
C                                BY ELEMENTS OF A SECOND GRID, CREATE ATTRIBUTE
C
            call intersect_elements(imsgout,xmsgout,cmsgout,
     *                       msgtype,nwds,ierr2)
C
         elseif(idsb(1:lenidsb).eq.'trans') then
C
C           ************************************************************
C           trans  : TRANSLATE PART OF THE MESH IN XYZ-SPACE.
C
            call translate(imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
C
         elseif(idsb(1:lenidsb).eq.'filter') then
C
C           ************************************************************
C           filter  : FILTER POINTS OR ELEMENTS
C
            if((nwds.eq.1).or.(cmsgout(2)(1:7) .ne. 'element')) then
               call filter
     1                (imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
            elseif(cmsgout(2)(1:7) .eq. 'element')then
               call filter_elem_graph
     1                (imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
            endif
C
         elseif(idsb(1:lenidsb).eq.'pset') then
C
C           ************************************************************
C           pset   : BUILD POINT SET.
C
            call pset(imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
C
         elseif(idsb(1:lenidsb).eq.'eltset') then
C
C           ************************************************************
C           eltset   : BUILD ELEMENT SET.
C
            call eset(imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
C
         elseif(idsb(1:lenidsb).eq.'fset') then
C
C           ************************************************************
C           eltset   : BUILD FACE SET.
C
            call fset(imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
C
c        elseif(idsb(1:lenidsb).eq.'pstatus') then
C
C           ************************************************************
C           pstatus  : CHECK/DEFINE A SET OF POINTS TO HAVE A NAME.
C
c           call pstatus(imsgout(2),xmsgout(2),cmsgout(2),msgtype(2),
c    *                   nwds-1,ierr2)
C
         elseif(idsb(1:lenidsb).eq.'kdtree') then
C
C           ************************************************************
C           kdtree   : BUILD KDTREE OF MESH OBJECT.
C
            call kdtree_cmo(imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
C
         elseif(idsb(1:lenidsb).eq.'doping') then
C
C           ************************************************************
C           doping  : CALCULATE DOPING PROFILES GRID TO GRID.
C
            call doping(imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
C
         elseif((idsb(1:lenidsb).eq.'intrp') .or.
     *           (idsb(1:lenidsb).eq.'interpolate') ) then
 
C           ************************************************************
C           intrp  : CALCULATE INTERPOLATION GRID TO GRID
C
             call intrp_gtg(imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
C
         elseif(idsb(1:lenidsb).eq.'dopmat') then
C
C           ************************************************************
C           dopmat  : CALCULATE DOPMAT PROFILES.
C
            call dopmat(imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
C
         elseif(idsb(1:lenidsb).eq.'surface') then
C
C           ************************************************************
C           surface  : DEFINE A BOUNDARY SURFACE OF THE SPECIFIED TYPE.
C
            call surface(imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
         elseif(idsb(1:lenidsb).eq.'offsetsurf') then
C
C           ************************************************************
C           offsetsurf  : DEFINE A BOUNDARY SURFACE OF THE SPECIFIED TYPE.
C
            call offsetsurf(imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
C
C
         elseif(idsb(1:lenidsb).eq.'mregion') then
C
C           ************************************************************
C           mregion  : STORE THE MATERIAL REGION DEFINITIONS INTO THE
C                      MISCELLANEOUS STORAGE BLOCK UNDER MREGION.
C
            call mregion(imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
C
         elseif(idsb(1:lenidsb).eq.'region') then
C
C           ************************************************************
C           region  : STORE THE PHYSICAL REGION DEFINITIONS INTO THE
C                     MISCELLANEWOUS STORAGE BLOCK.
C
            call region(imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
C
         elseif(idsb(1:lenidsb).eq.'edit') then
C
C           ************************************************************
C           edit   : GENERATES EDITS OF VARIOUS QUANTITIES ASSOCIATED
C                    WITH GROUPS OF POINTS OR PARTS.
C
            call edit(imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
C
         elseif(idsb(1:lenidsb).eq.'surfpts') then
            call cmo_get_name(cmo,ierror)
            call cmo_get_info('nnodes',cmo,
     *                        npoints,length,icmotype,ierror)
C
            call cmo_get_info('ipointi',cmo,
     *                      ipointi,lencmo,itpcmo,icscode)
            if (icscode .ne. 0) call x3d_error(isubname,'get_info_i')
            call cmo_get_info('ipointj',cmo,
     *                      ipointj,lencmo,itpcmo,icscode)
            if (icscode .ne. 0) call x3d_error(isubname,'get_info_i')
C
            icount=ipointj
            npoints_save=ipointj
            call set_epsilon()
            call surfpts(icount,imsgout(2),xmsgout(2),cmsgout(2),
     *                   msgtype(2),nwds-1,ierr1)
            ipointi=npoints_save+1
            ipointj=icount
            npoints=icount
            call cmo_get_name(cmo,ierror)
            call cmo_set_info('nnodes',cmo,npoints,1,1,ierror)
            call cmo_get_name(cmo,ierror)
C
            call cmo_set_info('ipointi',cmo,
     *                      ipointi,1,1,icscode)
            if (icscode .ne. 0) call x3d_error(isubname,'get_info_i')
            call cmo_set_info('ipointj',cmo,
     *                      ipointj,1,1,icscode)
            if (icscode .ne. 0) call x3d_error(isubname,'get_info_i')
C
         elseif(idsb(1:lenidsb).eq.'regnpts') then
            call cmo_get_name(cmo,ierror)
            call cmo_get_info('nnodes',cmo,
     *                        npoints,length,icmotype,ierror)
C
            call cmo_get_info('ipointi',cmo,
     *                      ipointi,lencmo,itpcmo,icscode)
            if (icscode .ne. 0) call x3d_error(isubname,'get_info_i')
            call cmo_get_info('ipointj',cmo,
     *                      ipointj,lencmo,itpcmo,icscode)
            if (icscode .ne. 0) call x3d_error(isubname,'get_info_i')
C
            icount=ipointj
            npoints_save=ipointj
C           ---------------------------------------------------------------
C           TRANSFORM POINTS AND VELOCITIES TO LOCAL COORD. SYSTEM
C
C
            if (normflgc .gt. 0) call chglocl(1,ipointj,1)
            call regnpts(icount,imsgout(2),xmsgout(2),cmsgout(2),
     *                   msgtype(2),nwds-1,ierr1)
            ipointi=npoints_save+1
            ipointj=icount
            npoints=icount
            call cmo_get_name(cmo,ierror)
            call cmo_set_info('nnodes',cmo,npoints,1,1,ierror)
            call cmo_get_name(cmo,ierror)
C
            call cmo_set_info('ipointi',cmo,
     *                      ipointi,1,1,icscode)
            if (icscode .ne. 0) call x3d_error(isubname,'get_info_i')
            call cmo_set_info('ipointj',cmo,
     *                      ipointj,1,1,icscode)
            if (icscode .ne. 0) call x3d_error(isubname,'get_info_i')
C
C           ---------------------------------------------------------------
C           TRANSFORM POINTS AND VELOCITIES TO NORMAL COORD. SYSTEM
C
            if (normflgc .gt. 0) call chgnorm(1,ipointj,1)
C
         elseif(idsb(1:lenidsb).eq.'setpts') then
C
C           ************************************************************
C           setpts  :
C
            if(nwds.eq.1) then
               call surfset()
               call regset()
            elseif(cmsgout(2)(1:12) .eq. 'no_interface')then
               call regset()
            else
               call closed_surfaces(cmsgout(3))
            endif
C
         elseif(idsb(1:lenidsb).eq.'resetpts') then
C
C          **********************************************************
 
C         resetpts:
C
            call resetpts(imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)

         elseif(idsb(1:lenidsb).eq.'grid2grid') then
C
C           ************************************************************
C           grid2grid: Convert a grid of one type to another.
C
            call grid2grid(imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
C
C
         elseif(idsb(1:lenidsb).eq.'hextotet' .or.
     *          idsb(1:lenidsb).eq.'hextotet_hybrid') then

            if (msgtype(2).eq.1) then
               ioption=imsgout(2)
               if(ioption.eq.0) ioption=24
            else
               ioption = -2
            endif

            cmo1=cmsgout(3)
            cmo2=cmsgout(4)
            call cmo_set_name(cmo2,ierror)
            call cmo_exist(cmo1,ierror)
            if(ierror.eq.0) then
               call cmo_release(cmo1,ierror)
            endif

c           set hextotet globals to remove zero volume elements
            if (nwds.gt.4 .and. msgtype(5).eq.3) then
              if (cmsgout(5)(1:5) .eq. 'rmvol') then
                call dotaskx3d('assign///hextotet_remove_volume/yes'
     *          //' ; finish',ierror)
                call dotaskx3d('assign///hextotet_remove_duplicates/yes'
     *          //' ; finish',ierror)
              endif
            endif
            call hextotet_hybrid(ioption,cmo1,cmo2,ierror)
C
C
C
c        elseif(idsb(1:lenidsb).eq.'matbld2d') then
C
C           ***************************************************************
C           matbld3d   : BUILD A 2D SPARSE MATRIX.
C
c           call matbld2d(imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
C
c        elseif(idsb(1:lenidsb).eq.'matbld3d') then
C
C           ***************************************************************
C           matbld3d   : BUILD A 3D SPARSE MATRIX.
C
c           call cmo_get_name(cmo,ierror)
c           call matbld3d(cmo,ierr2)
C
c        elseif(idsb(1:lenidsb).eq.'mathex3d') then
C
C           ***************************************************************
C           matbld3d   : BUILD A 3D HEX SPARSE MATRIX.
C
c           call mathex3d(imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
C
c        elseif(idsb(1:lenidsb).eq.'ether') then
C
C           ***************************************************************
C           ether   : MOVE THE ETHER POINTS.
C
c           call ether(imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
C
         elseif(idsb(1:lenidsb).eq.'merge') then
C
C           ***************************************************************
C           merge   : MERGE A LIST OF PAIRS OF POINTS
C                     FIRST OF PAIR IS SURVIVOR
c
            call mergelst (imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
c
         elseif(idsb(1:lenidsb).eq.'cmo') then
C
C           ***************************************************************
C           cmo   : DO ALL MESH OBJECT COMMANDS.
C
            call cmo_command(imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
C
         elseif(idsb(1:lenidsb).eq.'triangulate') then
C
C           ***************************************************************
C           triangulate a 2d mesh
C
            call triangulate_lg(imsgout,xmsgout,cmsgout,msgtype,nwds,
     *           ierr2)
C
         elseif(idsb(1:lenidsb).eq.'geniee') then
C
C           ***************************************************************
C           assign the element face-neighbor table jtet
C           Note: geniee can now handle meshes with children
C
            if (nwds.eq.1) then
               call cmo_get_name(cmo,ierror)
            else
               cmo=cmsgout(2)
            endif
            call cmo_exist(cmo,ierror)
            if(nwds .le. 2)then
               if(ierror.eq.0) call geniee_cmo(cmo)
            elseif(cmsgout(3) .eq. '2dnormal')then
C
C          Call module to check and perhaps modify the orientation
C          of tri, quad or tri/quad mesh
C
               call normal_check_flip
     *              (imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
            endif
C
c        elseif(idsb(1:lenidsb).eq.'solve_poisson') then
C
C           ************************************************************
C
C           solve_poisson  : Run the poisson finite volume based solver.
C
C           ------------------------------------------------------------
C
c           ierr2=0
c           call solve_poisson(imsgout,xmsgout,cmsgout,
c    *                       msgtype,nwds,ierr2)
C
c         elseif(idsb(1:lenidsb).eq.'sph') then
C
C           ************************************************************
C
C           sph  : Submit an SPH command.
C
C           ------------------------------------------------------------
C
c            ierr2=0
c            call sph_command(imsgout(2),xmsgout(2),cmsgout(2),
c     *                       msgtype(2),nwds-1,ierr2)
C
         elseif(idsb(1:lenidsb).eq.'elmtest') then
C
C           ************************************************************
C
            if(nwds.ge.2) then
               coption=cmsgout(2)
            else
               coption='all'
            endif
C
C           elmtest  : Tests various element quality measures related to
C                      data structures, geometry, etc.
C
            if(coption(1:icharlnf(coption)).eq.'jtet'
     &         .or.coption(1:icharlnf(coption)).eq.'all') then
               nwrite=-20
               if(nwds.ge.3) then
                  if (msgtype(3).eq.1) then
                    nwrite=-abs(imsgout(3))
                  elseif (msgtype(3).eq.2) then
                    nwrite=-abs(xmsgout(3))
                  endif
               endif
               call cmo_get_name(cmo,ierror)
               call elmtestd(cmo,nwrite,ierror)
            endif
C
         elseif((idsb(1:lenidsb).eq.'help').or.
     1          (idsb(1:lenidsb).eq.'HELP').or.
     2          (idsb(1:lenidsb).eq.'Help')) then
C
C           ************************************************************
C           help     : USE HELP PACKAGE.
C
            call helpdic(imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
C
c         elseif(idsb(1:lenidsb).eq.'voronoi') then
C
C           ************************************************************
C           voronoi :
C
c            call voronoi(imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
C
         elseif(idsb(1:lenidsb).eq.'voronoi_stor') then
C
C           ************************************************************
C           voronoi_stor :
C
            if (nwds.le.1) then
               call cmo_get_name(cmo,ierror)
            elseif (cmsgout(2)(1:icharlnf(cmsgout(2))).eq.'-def-') then
               call cmo_get_name(cmo,ierror)
            else
               cmo=cmsgout(2)
            endif
            if (nwds.le.3) then
               coption='all'
            else
               coption=cmsgout(3)
            endif
            if (nwds.le.4) then
               ifile='voronoi'
            else
               ifile=cmsgout(4)
            endif
            call cmo_exist(cmo,ierror)
            if(ierror.eq.0) then
               call voronoi_stor(cmo,'all',ifile)
            endif
C
C
         elseif(idsb(1:lenidsb).eq.'gtg') then
C
C           ************************************************************
C           grid_to_grid:
C
            call grid_to_grid(imsgout,xmsgout,cmsgout,msgtype,nwds,
     *                        ierr2)
C
         elseif(idsb(1:lenidsb).eq.'bingrid') then
C
C           ************************************************************
C           bingrid: Create a bin structure for a CMO
C
            call bingrid(imsgout,xmsgout,cmsgout,msgtype,nwds,
     *                   ierr2)
C
         elseif(idsb(1:lenidsb).eq.'assign') then
C
C           ************************************************************
C           assign :
C
            call dict_assign(imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
C
         elseif(idsb(1:lenidsb).eq.'setsize') then
C
C           ************************************************************
C           setsize :
C
            call setsize()
C
         elseif(idsb(1:lenidsb).eq.'mmprint') then
C
C           ************************************************************
C           mmprint : PRINT A MAP OF THE MEMORY MANAGER STORAGE.
C
            call mmprint()
            ierr2 = 0
C
         elseif(idsb(1:lenidsb).eq.'mmverify') then
C
C           ************************************************************
C           mmverify : VERIFY THE MEMORY MANAGER STORAGE.
C
            call mmverify()
            ierr2 = 0
          elseif(idsb(1:lenidsb).eq.'verify_edges') then
c
C           ************************************************************
c           verify_edges  -- loop thru all edges to see if valid
c
             call verify_edges()
             ierr2=0
C
C GEO COMMANDS
C
         elseif(idsb(1:lenidsb).eq.'geohelp') then
C           ************************************************************
C           geohelp : SHORT HELP MESSEGE ABOUT THE GEOPHYSICAL COMMANDS
 
            call geohelp(imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
            ierr2 = 0
 
         elseif(idsb(1:lenidsb).eq.'smooth_recon') then
C           ************************************************************
C           smooth_recon : CALL SMOOTH and RECON 0   N TIMES
C
            call smooth_recon(
     >      imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
            ierr2 = 0
 
         elseif(idsb(1:lenidsb) .eq. 'trilayer' .and.
     >          cmsgout(2)(1:4) .eq. 'flip' ) then
C           ************************************************************
C           trilayer/flip : flip normal on all triangles in surface
C
            if(nwds.gt.2 .and. msgtype(3).eq.3) then
               cmo=cmsgout(3)
            else
               call cmo_get_name(cmo,ierror)
            endif
 
            call cktrilayer_norm(cmo, idebug, 1,ierr2)
            ierr2 = 0
 
 
         elseif(idsb(1:lenidsb).eq.'dbl_to_sngl') then
C           ************************************************************
C           dbl_to_sngl : CONVERT DBL-DEFINED NODES TO SINGLE
C           This 'wrapper' calls dotask with
C                 resetpts/parents
C                 rmpoint/compress
c
C           resulting in a mesh with double-defined nodes at interfaces
C           having duplicate points removes so that the mesh now has
C           singly-defined nodes.
C
C     FORMAT:  dbl_to_sngl
C
 
            call dotask('resetpts/parents ; finish ',ierror)
            call dotask('rmpoint/compress ; finish ',ierror)
            ierr2 = 0
 
 
         elseif(idsb(1:lenidsb).eq.'assign_color_normal') then
C           ************************************************************
C           assign_color_normal : ASSIGN COLOR DEPENDING ON NORMAL DIR
c           replaced by settets/normal
 
            call assign_color_normal(
     *           imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
            ierr2 = 0
 
         elseif(idsb(1:lenidsb).eq.'filter2d') then
C           ************************************************************
C           filter2d :
 
            call filter2d(imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
            ierr2 = 0
 
         elseif(idsb(1:13)     .eq.'boundary_comp'   .or.
     *          idsb(1:lenidsb).eq.'checkitp' ) then
C           ************************************************************
C           boundary_components : REPORT BOUNDARY ID INFORMATION
 
            call boundary_components(imsgout,xmsgout,cmsgout,
     *                               msgtype,nwds,ierr2)
            ierr2 = 0
 
         elseif(idsb(1:lenidsb).eq.'pgg') then
C           ************************************************************
C           pgg : GENERATE PRISM GRID FROM TRILAYER CMO
            call pgg(imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
            write(logmess,'(a)') 'pgg no longer supported.'
            call writloga('default',0,logmess,0,ierrw)
            ierr2 = 0
 
         elseif(idsb(1:lenidsb).eq.'trilayertotet') then
C           ************************************************************
C           trilayertotet : CONVERT TRILAYER CMO TO TET CMO
            call trilayertotet(
     *           imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
            write(logmess,'(a)') 'trilayertotet no longer supported.'
            call writloga('default',0,logmess,0,ierrw)
            ierr2 = 0
 
         elseif(idsb(1:lenidsb).eq.'quadlayertotet') then
C           ************************************************************
C           trilayertotet : CONVERT QUADLAYER CMO TO TET CMO
            call quadlayertotet(
     *           imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
            write(logmess,'(a)') 'quadlayertotet no longer supported.'
            call writloga('default',0,logmess,0,ierrw)
            ierr2 = 0
 
         elseif(idsb(1:lenidsb).eq.'trilayer' .and.
     *      cmsgout(2)(1:6) .eq. 'derive' ) then
C           ************************************************************
C           trilayer/derive : CREATE TRI CMO BETWEEN 2 OTHERS
            call trilayer_derive(
     *           imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
            ierr2 = 0
 
 
         elseif(idsb(1:lenidsb).eq.'report') then
C           ************************************************************
C           report : non-evasive reports on current cmo
c           replaced by quality and printatt commands
 
            write(logmess,'(a)')
     *      'report command replaced by quality and printatt commands'
            call writloga('default',1,logmess,0,ierrw)
 
            ierr2 = 0
 
         elseif(idsb(1:lenidsb).eq.'rfile') then
C           ************************************************************
C           readfile : read a gmv or avs file, given name or rootname
 
            call readfile(imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
            ierr2 = 0
 
         elseif(idsb(1:lenidsb).eq.'prismtohex') then
C           ************************************************************
C           prismtohex : CONVERT PRISM CMO TO DEGENERATE HEX CMO
            call prismtohex(imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
            write(logmess,'(a)') 'prismtohex no longer supported.'
            call writloga('default',0,logmess,0,ierrw)
            ierr2 = 0

         elseif(idsb(1:lenidsb).eq.'squeeze') then
C           ************************************************************
C           squeeze edge nodes inwards, ok for 2D work, needs work
            call squeeze(imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
            ierr2 = 0
 
         elseif(idsb(1:lenidsb) .eq.'stack' ) then 
C           ************************************************************
C           stack layers : READ/MERGE LIST OF SURFACE FILES INTO CMO
 
            if (cmsgout(2)(1:6) .eq. 'trilay' .or.
     *          cmsgout(2)(1:7) .eq. 'quadlay' .or.
     *          cmsgout(2)(1:3) .eq. 'lay') then

              if (cmsgout(2)(1:3) .eq. 'lay') then
                call stack_layers(imsgout,xmsgout,cmsgout,msgtype,
     *                          nwds,ierr2)
              else 
                write(logmess,'(a,a)')
     *          'WARNING: unsupported code: ',
     *          'read_trilayers has been replaced with stack/layers'
                call writloga('default',0,logmess,0,ierrw)
                call read_trilayers(imsgout,xmsgout,cmsgout,msgtype,
     *                          nwds,ierr2)
              endif

            elseif (cmsgout(2)(1:4) .eq. 'fill') then

                call stack_fill(imsgout,xmsgout,cmsgout,msgtype,
     *                          nwds,ierr2)

            else
              write(logmess,'(a)')'STACK options are layer or fill'
              call writloga('default',1,logmess,0,ierrw)
            endif
            ierr2 = 0
 
         elseif(idsb(1:lenidsb).eq.'math') then
C
C           ************************************************************
C           math  : ARITHMETIC OPERATIONS AND MATHEMATICAL FUNCTIONS
C
            call math(imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
 
         elseif(idsb(1:lenidsb).eq.'boundary') then
C
C           ************************************************************
C           boundary  : SET ATTRIBUTES ON BOUNDARIES
C
            call boundary(imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
 
C
         elseif(idsb(1:lenidsb).eq.'bubble') then
C
C           ************************************************************
C           bubble  : EXTRUDE A POLYGON INTO A CLOSED SURFACE
C
            call bubble(imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
 
C
         elseif(idsb(1:lenidsb).eq.'extrude') then
C
C           ************************************************************
C           extrude  : EXTRUDE A POLYGON INTO A SHEET SURFACE
C
            call extrude(imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
 
C
         elseif(idsb(1:lenidsb).eq.'offsetpara') then
C
C           ************************************************************
C           offsetpara  : OFFSET A SURFACE OR A LINE BY A CONSTANT DIST.
C
            call offsetpara(imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
 
C
         elseif(idsb(1:lenidsb).eq.'extract_line') then
C
C           ************************************************************
C           extract_line  : EXTRACT A LINE GIVEN 2 DEFINING POINTS
C
            call extract_line(imsgout,xmsgout,cmsgout,msgtype,nwds,
     *          ierr2)
 
C
 
         elseif(idsb(1:lenidsb).eq.'reverse_elements') then
C
C           ************************************************************
C           reverse_elements  : flip connectivity of a set of elts.
C
            call reverse_elements_lg(imsgout,xmsgout,cmsgout,msgtype,
     *          nwds,ierr2)
 
C
 
         elseif(idsb(1:lenidsb).eq.'negative_coupling_coefficient'.or.
     *   idsb(1:lenidsb).eq.'negative_aij') then
C
C           ************************************************************
C           negative coupling coefficient: Check for negative cc
C
            call coupling_coef_wrapper(imsgout,xmsgout,cmsgout,msgtype,
     *             nwds,ierr2)
 
C
         elseif(idsb(1:lenidsb).eq.'sort') then
C
C           ************************************************************
C           Sort x,y,z coordinates and create integer pointer arrays
C
            if(cmsgout(2)(1:3).eq.'xyz') then
C
C             Support old syntax
C
              call sort_old(imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
            else
              call sortbins(imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
            endif
            ierr2 = 0
C
         elseif(idsb(1:lenidsb).eq.'reorder') then
C
C           ************************************************************
C           Reorder nodes of a MO using sort key provided.
C
            call reorder(imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
            ierr2 = 0
C
         elseif(idsb(1:lenidsb).eq.'perturb') then
C
C           ************************************************************
C           Perturb x,y,z coordinates
C
            call perturb_lg(imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
            ierr2 = 0
c
         elseif(idsb(1:lenidsb).eq.'bleed_color') then
C
C           ************************************************************
C           Perturb x,y,z coordinates
C
            call bleed_color_lg(imsgout,xmsgout,cmsgout,msgtype,nwds,
     *         ierr2)
            ierr2 = 0
C
         elseif(idsb(1:lenidsb).eq.'mode') then
C
C           ************************************************************
C           Process mode setting
C
            call mode_lg(imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
            ierr2 = 0
C
         elseif(idsb(1:lenidsb).eq.'loop') then
C
C           ************************************************************
C           Process loop command
C
            call loop_lg(imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
            ierr2 = 0
C
         elseif(idsb(1:lenidsb).eq.'ung2avs') then
C
C           ************************************************************
C
C           ung2avs : CONVERT UNG FILE TO AVS FORMAT
C
            call ung2avs(imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
C
         elseif(idsb(1:lenidsb).eq.'metis') then
C
C           ************************************************************
C
C           metis : Call METIS library functions. If METIS library
C                   is not installed then dummy functions will be called.
C
            call metis_interface
     1        (imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
C
         elseif(idsb(1:lenidsb).eq.'create_graph') then
C
C           ************************************************************
C
C           create_graph : Call METIS library functions to create
C                          adjacency graph. If METIS library
C                   is not installed then dummy functions will be called.
C
            call create_graph
     1        (imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
C
         elseif(idsb(1:lenidsb).eq.'calc_rdist') then
C
C           ************************************************************
C
C           calc_dist : Calculate radial distance function
C
            call calc_rdist
     1        (imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
C
         elseif(idsb(1:lenidsb).eq.'quit') then
C
C           ************************************************************
C
C           Give the user some help to 'finish'.
C
            write(logmess,'(a)') 'Quit LaGriT using the command: finish'
            call writloga('default',0,logmess,0,ierrw)
C
         elseif(idsb(1:lenidsb).eq.'end') then
C
C           ************************************************************
C
C           Give the user some help to 'finish'.
C
            write(logmess,'(a)') 'End LaGriT using the command: finish'
            call writloga('default',0,logmess,0,ierrw)
C
         elseif(idsb(1:lenidsb).eq.'usersub1') then
C
C           ************************************************************
C           usersub1 : EXECUTE USERSUB1.
C
            write(logmess,9010) 1,cmsgout(2)
            call writloga('default',1,logmess,0,ierrw)
 9010       format('Execute usersub: ',i10,a32)
            call usersub1()
            ierr2 = 0
C
         elseif(idsb(1:lenidsb).eq.'usersub2') then
C
C           ************************************************************
C           usersub2 : EXECUTE USERSUB2.
C
            write(logmess,9010) 2,cmsgout(2)
            call writloga('default',1,logmess,0,ierrw)
            call usersub2()
            ierr2 = 0
C
         else
C
C           ************************************************************
C           usersub : EXECUTE USERSUB.
C
            call user_sub(imsgout,xmsgout,cmsgout,msgtype,nwds,ierr2)
            if(ierr2.ne.0) then
               ierror=-1
               write(logmess,9000) idsb(1:lenidsb)
               call writloga('default',1,logmess,1,ierrw)
 9000          format("Invalid LaGriT generator command: ",a)
            endif
C
         endif
C
 9999 continue
C
      return
      end
      subroutine usersub1()
C
      implicit real*8 (a-h,o-z)
C
C
C
      include 'cmo.h'
      include 'chydro.h'
      include 'consts.h'
C
      call cmo_get_name(cmo,ierror)
C
C
c      do i=1,100
C
         call cmo_get_info('nnodes',cmo,npoints,npoints,icmotype,ierror)
         call cmo_get_info('nelements',cmo,ntets,ntets,icmotype,ierror)
         call cmo_get_info('xic',cmo,ipxic,lenxic,icmotype,ierror)
         call cmo_get_info('yic',cmo,ipyic,lenyic,icmotype,ierror)
         call cmo_get_info('zic',cmo,ipzic,lenzic,icmotype,ierror)
C
c        call dotask('recon2;finish',ierror_return)
C
c        if(mod(i,10).eq.0) then
c        endif
C
c      enddo
 
      goto 9999
 9999 continue
      return
      end
      subroutine usersub2()
      return
      end
