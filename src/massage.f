C #####################################################################
      subroutine massage(imsgin,xmsgin,cmsgin,msgtype,nwds,
     &   ierror)
C
C #####################################################################
C
C     PURPOSE -
C
C        MASSAGE performs 'Graph Massage' on 3D meshes.  That is,
C        we create and annihilate nodes in a coordinated way
C        to improve the mesh.
C
C     INPUT ARGUMENTS -
C
C         imsgin()  - Integer array of command input tokens
C         xmsgin()  - Real array of command input tokens
C         cmsgin()  - Character array of command input tokens
C         msgtype() - Integer array of command input token types
C         nwds      - Number of command input tokens
C
C     OUTPUT ARGUMENTS -
C
C         ierror - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C     CHANGE HISTORY -
C
C        $Log:   /pvcs.config/t3d/src/massage.f_a  $
CPVCS    
CPVCS       Rev 1.40   19 Jul 2005 07:17:40   gable
CPVCS    Added norecon option.
CPVCS    
CPVCS       Rev 1.39   22 Apr 2004 10:23:42   gable
CPVCS    Corrected syntax in semiexclusive option.
CPVCS    
CPVCS       Rev 1.38   21 Apr 2004 18:20:48   kuprat
CPVCS    Corrected EXCLUSIVE option.  New SEMIEXCLUSIVE option.
CPVCS    
CPVCS       Rev 1.37   26 Feb 2004 16:22:42   kuprat
CPVCS    We now accept EXCLUSIVE flag.
CPVCS    
CPVCS       Rev 1.36   01 Mar 2002 14:44:28   dcg
CPVCS    adaptive merging
CPVCS    
CPVCS       Rev 1.35   25 Feb 2002 16:00:22   dcg
CPVCS    pass adaption field name to cee_chain
CPVCS
CPVCS       Rev 1.34   25 Feb 2002 14:14:30   dcg
CPVCS    use massage.h include file
CPVCS
CPVCS       Rev 1.33   05 Feb 2002 14:59:10   dcg
CPVCS    check for number of nodes smoothed for termination
CPVCS
CPVCS       Rev 1.32   31 Jan 2002 13:12:56   dcg
CPVCS    add changes for adaption_field option
CPVCS
CPVCS       Rev 1.31   28 Jul 2001 17:54:18   kuprat
CPVCS    Updated documentation.  Deleted DELUXE option.
CPVCS
CPVCS       Rev 1.30   30 May 2001 13:49:04   kuprat
CPVCS    Make sure TOLROUGHNESS only checked in 2-D case.
CPVCS
CPVCS       Rev 1.29   29 May 2001 17:59:20   kuprat
CPVCS    We now take TOLROUGHNESS in the argument list.  This is
CPVCS    a format change, but old decks should still work.
CPVCS
CPVCS       Rev 1.28   15 May 2001 18:39:00   kuprat
CPVCS    We pass CHECKAXY option onto RECON.
CPVCS
CPVCS       Rev 1.27   14 May 2001 08:44:42   kuprat
CPVCS    Put in CHECKAXY option.
CPVCS
CPVCS       Rev 1.26   Thu Apr 06 09:10:16 2000   dcg
CPVCS    replace get_info_i and set_info_i calls
CPVCS
CPVCS       Rev 1.25   01 Mar 2000 15:02:10   kuprat
CPVCS    We now support new options STRICTMERGELENGTH, IGNOREMATS, LITE.
CPVCS
CPVCS       Rev 1.24   Mon Jul 12 13:58:00 1999   kuprat
CPVCS    Put rmpoint/compress within iteration loop.
CPVCS
CPVCS       Rev 1.23   Wed Feb 10 17:34:56 1999   kuprat
CPVCS    Added more documentation.
CPVCS
CPVCS       Rev 1.22   Wed Jan 20 14:36:12 1999   kuprat
CPVCS    Got rid of control C's that got in file.
CPVCS
CPVCS       Rev 1.21   Wed Jan 20 14:34:10 1999   kuprat
CPVCS    Corrected problem that occurred when PSET info omitted.
CPVCS
CPVCS       Rev 1.20   Tue Jan 19 15:11:50 1999   kuprat
CPVCS    We now use INCLUSIVE=1 and we pass PSETNAME to CEL_CHAIN.
CPVCS
CPVCS       Rev 1.19   Fri Jan 15 09:30:42 1999   dcg
CPVCS    reset ipointi and ipointj to the entire node range at end.
CPVCS
CPVCS       Rev 1.18   Mon Jan 11 16:43:28 1999   kuprat
CPVCS    We now just pass the PSET info portion of the command parameter
CPVCS    arrays to GETMPARY.
CPVCS
CPVCS       Rev 1.17   Mon Jan 11 11:00:40 1999   kuprat
CPVCS    Changed name of length tolerances to BISECTION_LENGTH and
CPVCS    MERGE_LENGTH.  MERGE_LENGTH has a slightly different meaning
CPVCS    than GUARD_LENGTH, the parameter it replaces.
CPVCS
CPVCS       Rev 1.16   Wed Dec 23 13:54:40 1998   jtg
CPVCS    fixed duplicate declaration of nnfreq and idebug
CPVCS
CPVCS       Rev 1.15   Mon Nov 16 10:42:38 1998   kuprat
CPVCS    We now only call 'cel_chain'.
CPVCS
CPVCS       Rev 1.14   Wed Nov 04 23:56:10 1998   kuprat
CPVCS    Put in 'nosmooth' option.
CPVCS
CPVCS       Rev 1.13   Wed Nov 04 11:14:34 1998   dcg
CPVCS    fix never used warnings
CPVCS
CPVCS       Rev 1.12   Wed Nov 04 02:38:52 1998   kuprat
CPVCS    We now call SGD (regardless if the last argument to massage
CPVCS    is 'deluxe').  Action with 'deluxe' is toned down to prevent
CPVCS    total destruction of the mesh!  MAXAGDITER is now reduced to 4.
CPVCS
CPVCS       Rev 1.11   Fri Oct 30 15:19:14 1998   kuprat
CPVCS    Added new CSL call in AGD3D/RECON loop.  Since this actually can
CPVCS    cause the grid to get WORSE in some cases, you can only access this
CPVCS    feature with last argument = 'deluxe'.
CPVCS
CPVCS       Rev 1.10   Fri Aug 21 16:46:44 1998   dcg
CPVCS    make changes to allow for 2d massage
CPVCS
CPVCS       Rev 1.9   Mon Jun 29 14:52:12 1998   dcg
CPVCS    add rivara chain type refinement
CPVCS    command is refine/rivara
CPVCS    refine/cel gives longest edge first refinement
CPVCS
CPVCS       Rev 1.8   Thu Jun 25 09:57:10 1998   dcg
CPVCS    add inclusive argument to cel call
CPVCS
CPVCS       Rev 1.7   Thu Jun 18 10:10:48 1998   dcg
CPVCS    skip call to recon with ivornoi = 1 when abs(ivoronoi) = 2
CPVCS
CPVCS       Rev 1.6   Sun May 24 00:08:50 1998   kuprat
CPVCS    Now MASSAGE calls RECON with TOLDAMAGE.
CPVCS
CPVCS       Rev 1.5   Thu Apr 30 15:15:30 1998   kuprat
CPVCS    Increased agd3d loop limit to 100 iterations.
CPVCS
CPVCS       Rev 1.4   Thu Apr 09 19:24:40 1998   kuprat
CPVCS    If ivoronoi= +2 or -2, we call recon first with ivoronoi=1,
CPVCS    and then with the +/-2 value.  That's because there is
CPVCS
CPVCS       Rev 1.3   Tue Oct 07 22:16:04 1997   kuprat
CPVCS    Changed parameters for command.  We now have two different
CPVCS    length tolerances---one for creation and one for annihilation.
CPVCS
CPVCS       Rev 1.2   Wed Jul 02 17:42:10 1997   kuprat
CPVCS    Added gmv, x3d dumps if IDEBUG >= 3.
CPVCS
CPVCS       Rev 1.1   Thu Jun 05 17:17:52 1997   dcg
CPVCS    add finish to dotaskx3d calls
CPVCS
CPVCS       Rev 1.0   Tue Jun 03 12:44:34 1997   kuprat
CPVCS    Initial revision.
C
C #####################################################################
c
c   FORMAT:
c
c   MASSAGE / BISECTION_LENGTH / MERGE_LENGTH / TOLDAMAGE / TOLROUGHNESS
c      ifirst,ilast,istride / [nosmooth,norecon,strictmergelength,ignoremats,
c      lite,checkaxy,semiexclusive,exclusive]
c
c   MASSAGE creates, annihilates, and moves nodes and swaps
c   connections in a 2D or 3D mesh in order to improve
c   element aspect ratios and establish user-desired edge
c   lengths.
c
c   Specifically, MASSAGE performs up to four iterations of
c   a loop which calls AGD3D (a routine for automated merging
c   of nodes), RECON (a routine for automated reconnection
c   of edges), and SGD (a routine for element aspect ratio
c   improvement using smoothing).  MASSAGE then calls
c   CEL_CHAIN which performs Rivara edge refinement and
c   then another call to RECON.  In the case of 2-D surface grids,
c   this is then followed by a call
c   to CER_CHAIN which is another edge refinement routine and
c   then a final call to RECON if necessary.
c
c   AGD3D will attempt to merge edges that are shorter than
c   MERGE_LENGTH.  CEL_CHAIN will attempt to bisect edges
c   that are longer than BISECTION_LENGTH.  For 2-D surface grids,
c   CER_CHAIN will attempt
c   to bisect edges that deviate from an averaged surface normal
c   ("have a roughness of") greater than TOLROUGHNESS.  RECON will attempt
c   to create 'nice' elements by using face swapping.  (The
c   value of the LaGriT IVORONOI parameter determines the
c   meaning of 'nice'.)  SGD will attempt to improve element
c   aspect ratios by moving nodes.
c
c   To ensure that the actions of AGD3D, RECON, and
c   SGD are controlled and harmonious, the parameter
c   TOLDAMAGE exists, and there are guidelines for choosing the
c   relative values of BISECTION_LENGTH, MERGE_LENGTH,
c   TOLDAMAGE, and TOLROUGHNESS.
c
c   TOLDAMAGE is a parameter which controls how much the grid
c   will be deformed by AGD3D, CEL_CHAIN, RECON, and SGD.
c   The 'damage' is a measure of how much interfaces and
c   external boundaries are deformed.  Roughly, it measures
c   the depths of 'dents' that are invariably introduced when
c   nodes are moved, annihilated, and faces are swapped.
c   We guarantee that the damage of any single node movement,
c   node annihilation, or face swap is bounded by TOLDAMAGE.
c   So if TOLDAMAGE is set to an extremely small number, one
c   can expect hardly any node movements, annihilations, or
c   face swaps will be allowed.  Conversely, if TOLDAMAGE is
c   set too large, physical interfaces may be significantly
c   deformed by the action of MASSAGE.  Experience has
c   shown that setting TOLDAMAGE equal to approximately
c   .01 times the diameter of the mesh frequently gives
c   acceptable results.
c
c   The guidelines for selecting BISECTION_LENGTH, MERGE_LENGTH,
c   TOLDAMAGE, and TOLROUGHNESS are as follows.  BISECTION_LENGTH should not be
c   smaller than MERGE_LENGTH, or the action of merging nodes
c   together will be largely pointless because the newer, longer
c   edges created by merging will simply be bisected again.
c   In fact, merging all edges of length > MERGE_LENGTH together
c   can easily create edges of length roughly 3*MERGE_LENGTH in the mesh.
c   Hence it is recommended that BISECTION_LENGTH be at least
c   three times as large as merge length.
c
c   Merges of edges of length <= MERGE_LENGTH are meant to
c   coarsen the mesh, but are not meant to deform surfaces
c   and material interfaces on this scale.  The amount of
c   material/surface deformation (TOLROUGHNESS) is meant to be considerably
c   less than MERGE_LENGTH.
c
c   On the other hand, the maximum roughness tolerated in the graph
c   (TOLROUGHNESS) should be considerably more than TOLDAMAGE, or
c   roughness refinement will be triggered by actions such as
c   flipping or merging.
c
c   Hence, our guidelines for selecting these three parameters
c   are
c
c   BISECTION_LENGTH >= 3*MERGE_LENGTH >> TOLDAMAGE.
c
c                          AND
c
c   TOLROUGHNESS >= 10*TOLDMAMAGE  (for 2-D surface grids).
c
c   For example, for a grid with diameter of order three,
c   we have used
c
c   BISECTION_LENGTH, MERGE_LENGTH, TOLDAMAGE, TOLROUGHNESS =.3, .1, .01, .1
c
c   If one of {BISECTION_LENGTH, MERGE_LENGTH} are omitted, the omitted
c   one will be set so that BISECTION_LENGTH=3*MERGE_LENGTH.  If they are
c   both omitted, they will both be taken to be infinity.
c   If TOLDAMAGE is not specified, no node annihilation will take place.
c   IF TOLROUGHNESS is not specified, no refinement on roughness will
c   occur and thus the format is compatible with old decks where
c   refinement on roughness did not occur.
c
c   The final, optional arguments can be one or more of 
c   SMOOTH (the default), NOSMOOTH,NORECON,
c   LITE, IGNOREMATS, STRICTMERGELENGTH, CHECKAXY, SEMIEXCLUSIVE, and EXCLUSIVE.
c   NOSMOOTH causes MASSAGE to deviate from the above
c   description, in that node smoothing by SGD is not performed.
c   Smoothing as described above is performed when SMOOTH is specified.
c   The optional argument STRICTMERGELENGTH forces strict interpretation
c   of MERGE_LENGTH so that there is no merging along the edges of flat
c   elements.  This is important if IGNOREMATS is specified to avoid losing the interfaces.
c   The optional argument IGNOREMATS causes massage to process the a
c   mulitmaterial mesh in a single material mode; it ignores the material interfaces.
c   If LITE is specified, only one iteration of the merging/reconnection
c   loop is executed, and a reconnection after edge refinement is
c   omitted.  This is suitable for applications, such as Gradient
c   Weighted Moving Finite Elements, where MASSAGE is called repeatedly.
c   If CHECKAXY is given, then we insure that for 2D meshes,
c   the output mesh will have positive xy-projected triangle areas,
c   provided that the input mesh had them in the first place.
c   If EXCLUSIVE is given, then edge refinement operations will only
c   be performed on edges whose endpoints are BOTH in the PSET that
c   MASSAGE is working on.  (As usual, new nodes created by refinement
c   are added to the PSET so that massage can refine edges recursively.)
c   The default behavior is 'inclusive', where only ONE edge endpoint
c   has to belong to the PSET for the edge to be eligible for refinement.
c   If SEMIEXLUSIVE is given, refinement will only be triggered by 
c   edges with both endpoints in the PSET, but some edges with less than
c   two endpoints in the PSET might be refined as part of a 
c   'Rivara chain' triggered by the refinement of an edge with both
c   endpoints in the PSET.  This represents an intermediate case between
c   INCLUSIVE and EXCLUSIVE.
c
c   Note:  Since CEL_CHAIN is called only once at the end of MASSAGE,
c   it may be necessary to call MASSAGE twice for optimal results.  This
c   is because annihilation of nodes is done with an intent to improve
c   element aspect ratios, but cannot be effective if there are too
c   few nodes initially.
c
c   Note: The user may wish to issue a "RMPOINT/COMPRESS" after MASSAGE
c   operations that merge a significant number of nodes.
c
c   EXAMPLES:
c
c   massage /0.3/0.1/0.01/
c      Mesh edges longer than 0.3 will be bisected; mesh edges shorter than 0.1
c      might be collapsed if that causes damage (normal surface motion) to material
c      interfaces or external boundaries less than 0.01 ; smoothing of nodes
c      causing damage less than 0.01 is allowed ; face swapping causing damage
c      less than 0.01 is allowed.
c
c   massage /0.3/0.1/0.01/0.1/
c      Same as above but for 2-D surface meshes, roughness greater than
c      0.1 will trigger refinement.
c
c   massage /0.3/0.1/0.01/pset,get,PSET1
c      Mesh edges (containing at least one endpoint in PSET1) longer than 0.3
c      will be bisected; mesh edges shorter than 0.1 might be collapsed if that
c      causes damage (normal surface motion) to material interfaces or external
c      boundaries less than 0.01 and if the annihilated node is in PSET1;
c      smoothing of nodes in PSET1 causing damage less than 0.01 is allowed;
c      face swapping causing damage less than 0.01 is allowed (unfortunately,
c      LaGriT at this time does not restrict swapping to PSET1).
c
c   massage /0.3/0.1/0.01/pset,get,PSET1/nosmooth
c      As above, but without smoothing.
c
c   massage /1.e+20/0.1/0.1/1,0,0/nosmooth
c      Because of the virtually infinite value of BISECTION_LENGTH, no edges
c      will be bisected.  Since MERGE_LENGTH=TOLDAMAGE=0.1, merging of edges
c      of length less than 0.1 will be considered, and will not be rejected
c      because of excessive damage.  Hence we expect that all edges of length
c      less than 0.1 will be merged away (except in those cases where merging
c      would invert tetrahedra or change material topology).   Because
c      NOSMOOTH is specified, no smoothing will take place.  Face swapping
c      causing damage less than TOLDAMAGE is allowed.
c
c   massage/1.e+20/1.e-9/1.e-9/1,0,0/nosmooth/strictmergelength/ignoremats
c      This set of arguments will remove degenerate elements from a mesh
c      by merging nodes that have the same coordinate values ( within 1.e-9).
c
C #####################################################################
 
      implicit none
C
      include 'chydro.h'
      include 'massage.h'
 
      integer lenptr
      parameter (lenptr=1000000)
 
      integer nwds, imsgin(nwds), msgtype(nwds)
      real*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
C
      integer imsginsave(3),msgtypesave(3)
      real*8 xmsginsave(3)
      character*32 cmsginsave(3)
 
      integer ierror,ierrw
 
      character*132 logmess
      pointer (ipitp1, itp1)
      integer itp1(lenptr)
C
      pointer (ipmpary,mpary),(ipout,out)
      integer mpary(lenptr),out(*)
      pointer (ipireal1,ireal1),(ipirealold,irealold)
      integer ireal1(lenptr),irealold(lenptr)
C
      character*32 cmo,isubname,cout
      integer length,icmotype,icscode,mpno,inclusive,msmoothed,
     &   nnodes,ierrdum,i,nnodesold,iout,
     &   ierr,j,nsd_topo,len_nnodes,inc
      logical ldiffer,lnosmooth,lstrictmergelength,
     &   llite,lignoremats,lcheckaxy,lcheckroughness,lexclusive,
     &   lsemiexclusive, lrecon
      real*8 bisection_length,merge_length,toldamage,tolar,epsilonl,
     &   tolroughness,range
      character*32 comend, psetname, cmode
      character*132 cbuf
 
      integer maxagditer
 
      integer locdebug
      parameter (locdebug=1)
 
      logical lcompresswherepossible
      parameter (lcompresswherepossible=.true.)
 
      comend=' ; finish'
C
      isubname = 'massage'
C
      ierror=0
      lnosmooth=.false.
      lstrictmergelength=.false.
      lignoremats=.false.
      llite=.false.
      lcheckaxy=.false.
      lexclusive=.false.
      lsemiexclusive=.false.
      lrecon=.true.
C
C  Check that user has specified a valid mesh object.
 
      call cmo_get_name(cmo,ierror)
      call get_epsilon('epsilonl',epsilonl)
      if(ierror.ne.0) then
         write(logmess,'(a)')
     *      'MASSAGE: ',cmo,' not a valid mesh object'
         call writloga('default',0,logmess,0,ierrw)
         goto 9999
      endif
 
      call cmo_get_info('nnodes',cmo,
     *   nnodes,length,icmotype,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('ndimensions_topo',cmo,
     *   nsd_topo,length,icmotype,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('itp1',cmo,
     *   ipitp1,length,icmotype,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
C
      call mmgetblk('mpary',isubname,ipmpary,nnodes,1,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'mmgetblk')
C
      if (msgtype(3).le.0.or.cmsgin(3)(1:5).eq.'-def-') then
         if (msgtype(2).le.0.or.cmsgin(2)(1:5).eq.'-def-') then
            bisection_length=1.d99
            merge_length=1.d99
         else
            call test_argument_type(1,2,2,imsgin,xmsgin,cmsgin,msgtype,
     *         nwds)
            bisection_length=xmsgin(2)
            merge_length=bisection_length/3.
         endif
      else
         if (msgtype(2).le.0.or.cmsgin(2)(1:5).eq.'-def-') then
            call test_argument_type(1,2,3,imsgin,xmsgin,cmsgin,msgtype,
     *         nwds)
            merge_length=xmsgin(3)
            bisection_length=merge_length*3.
         else
            call test_argument_type(1,2,2,imsgin,xmsgin,cmsgin,msgtype,
     *         nwds)
            call test_argument_type(1,2,3,imsgin,xmsgin,cmsgin,msgtype,
     *         nwds)
            bisection_length=xmsgin(2)
            merge_length=xmsgin(3)
         endif
      endif
 
      if (msgtype(4).le.0.or.cmsgin(4)(1:5).eq.'-def-') then
         toldamage=-1.
      else
         call test_argument_type(1,2,4,imsgin,xmsgin,cmsgin,msgtype,
     *      nwds)
         toldamage=xmsgin(4)
      endif
 
c....The fifth argument, if present and type real will be TOLROUGHNESS.
c....This causes all remaining arguments to be shifted by one.
 
c.... TOLROUGHNESS only checked in 2-D case
      if (nwds.ge.5.and.msgtype(5).eq.2) then
         if (nsd_topo.eq.2) then
            tolroughness=xmsgin(5)
            lcheckroughness=.true.
         else
            lcheckroughness=.false.
         endif
 
c.... Save PSET info, since DOTASKX3D destroys it.
 
         if (nwds.ge.8) then
            do i=1,3
               imsginsave(i)=imsgin(i+5)
               xmsginsave(i)=xmsgin(i+5)
               cmsginsave(i)=cmsgin(i+5)
               msgtypesave(i)=msgtype(i+5)
            enddo
         else
            msgtypesave(1)=1
            imsginsave(1)=1
            imsginsave(2)=0
            imsginsave(3)=0
         endif
 
         do j=9,nwds
            if (msgtype(j).le.0.or.cmsgin(j)(1:5).eq.'-def-') then
            else
               call test_argument_type(1,3,j,imsgin,xmsgin,cmsgin
     &            ,msgtype,nwds)
               if (cmsgin(j)(1:8).eq.'nosmooth') then
                  lnosmooth=.true.
               endif
               if (cmsgin(j)(1:17).eq.'strictmergelength') then
                  lstrictmergelength=.true.
               endif
               if (cmsgin(j)(1:10).eq.'ignoremats') then
                  lignoremats=.true.
               endif
               if (cmsgin(j)(1:4).eq.'lite') then
                  llite=.true.
               endif
               if (cmsgin(j)(1:8).eq.'checkaxy') then
                  lcheckaxy=.true.
               endif
               if (cmsgin(j)(1:9).eq.'exclusive') then
                  lexclusive=.true.
               endif
               if (cmsgin(j)(1:13).eq.'semiexclusive') then
                  lsemiexclusive=.true.
               endif
               if (cmsgin(j)(1:7).eq.'norecon') then
                  lrecon=.false.
               endif
            endif
         enddo
 
      else
 
         lcheckroughness=.false.
 
c.... Save PSET info, since DOTASKX3D destroys it.
 
         if (nwds.ge.7) then
            do i=1,3
               imsginsave(i)=imsgin(i+4)
               xmsginsave(i)=xmsgin(i+4)
               cmsginsave(i)=cmsgin(i+4)
               msgtypesave(i)=msgtype(i+4)
            enddo
         else
            msgtypesave(1)=1
            imsginsave(1)=1
            imsginsave(2)=0
            imsginsave(3)=0
         endif
 
         do j=8,nwds
            if (msgtype(j).le.0.or.cmsgin(j)(1:5).eq.'-def-') then
            else
               call test_argument_type(1,3,j,imsgin,xmsgin,cmsgin
     &            ,msgtype,nwds)
               if (cmsgin(j)(1:8).eq.'nosmooth') then
                  lnosmooth=.true.
               endif
               if (cmsgin(j)(1:17).eq.'strictmergelength') then
                  lstrictmergelength=.true.
               endif
               if (cmsgin(j)(1:10).eq.'ignoremats') then
                  lignoremats=.true.
               endif
               if (cmsgin(j)(1:4).eq.'lite') then
                  llite=.true.
               endif
               if (cmsgin(j)(1:8).eq.'checkaxy') then
                  lcheckaxy=.true.
               endif
               if (cmsgin(j)(1:9).eq.'exclusive') then
                  lexclusive=.true.
               endif
               if (cmsgin(j)(1:13).eq.'semiexclusive') then
                  lsemiexclusive=.true.
               endif
               if (cmsgin(j)(1:7).eq.'norecon') then
                  lrecon=.false.
               endif
            endif
         enddo
 
      endif
 
      if (llite) then
         maxagditer=1
      else
         maxagditer=4
      endif
 
      len_nnodes=nnodes+100
      call mmgetblk('ireal1',isubname,ipireal1,len_nnodes,1,icscode)
      call mmgetblk('irealold',isubname,ipirealold,len_nnodes,1,icscode)
 
c        1) do we have a real point?
c             ireal1() =  0 ==> not a real point.
c             ireal1() =  1 ==> a real point.
c
      call unpacktp("allreal","set",nnodes,ipitp1,ipireal1,ierrdum)
      if(ierrdum.ne.0) call x3d_error(isubname, 'unpacktp')
 
c.... Copy IREAL1 array for later comparisons to determine if
c.... mesh has changed.
 
      do i=1,nnodes
         irealold(i)=ireal1(i)
      enddo
      nnodesold=nnodes
 
      call setsize()
 
c.... AGD3D / RECON loop.  AGD3D annihilates nodes, and RECON
c.... reconnects to possibly allow AGD3D to annihilate some more.
c.... Break out of loop if IREAL1 is unchanged meaning that
c.... AGD3D does not want to annihilate nodes even after
c.... a reconnection.
 
      tolar=0.05
 
      do i=1,maxagditer
 
c.... AGD3D attempts to annihilate nodes whose annihilation would
c.... cause a 'damage' less than TOLDAMAGE and which are within
c.... MERGE_LENGTH of a suitable merge candidate.
 
         call getmpary(imsginsave,xmsginsave,cmsginsave,msgtypesave,
     &      ipmpary,mpno,psetname,ierror)
         call agd3d(cmo,toldamage,merge_length,mpary,mpno
     &      ,lstrictmergelength,lignoremats,lcheckaxy,lcheckroughness
     &      ,tolroughness,ierror)
 
         if (lrecon)then
         if (lcheckaxy) then
            write(cbuf,'(a,e16.8,a)')'recon/0/',toldamage
     &         ,'/checkaxy/ ; finish'
            call dotaskx3d(cbuf,ierr)
         else
            write(cbuf,'(a,e16.8,a)')'recon/0/',toldamage
     &         ,'/ ; finish'
            call dotaskx3d(cbuf,ierr)
         endif
         endif
 
c.... New aspect ratio sensitive smoothing!
 
         if (.not.lnosmooth) then
            call getmpary(imsginsave,xmsginsave,cmsginsave,msgtypesave,
     &         ipmpary,mpno,psetname,ierror)
            msmoothed=0
c.... Debug dumps.
 
            call cmo_get_info('idebug',cmo,
     *      idebug,length,icmotype,icscode)
 
            if (idebug.ge.3) then
 
               write(cbuf,'(a,i6.6,a)')'dump/gmv/gmvbeforesgd',i,'/'
               call dotaskx3d(cbuf // comend,ierr)
 
            endif
            call sgd(cmo,toldamage,msmoothed,mpary,mpno,ierror)
 
         endif
 
c.... See if anything's changed in this loop.  If not, break out.
 
         call cmo_get_info('nnodes',cmo,
     *      nnodes,length,icmotype,icscode)
         call cmo_get_info('itp1',cmo,
     *      ipitp1,length,icmotype,icscode)
         if (len_nnodes.lt.nnodes) then
            inc=nnodes-len_nnodes+100
            len_nnodes=len_nnodes+inc
            call mmincblk('ireal1',isubname,ipireal1,inc,icscode)
            call mmincblk('irealold',isubname,ipirealold,inc,icscode)
         endif
         if(msmoothed.gt.0) then
            ldiffer=.true.
         else
            call unpacktp("allreal","set",nnodes,ipitp1,ipireal1,
     *         ierrdum)
            if(ierrdum.ne.0) call x3d_error(isubname, 'unpacktp')
            if (nnodes.eq.nnodesold) then
               ldiffer=.false.
               do j=1,nnodes
                  if (ireal1(j).ne.irealold(j)) ldiffer=.true.
               enddo
               if (.not.ldiffer) goto 100
            endif
         endif
 
c.... Something changed.  Store IREAL1, NNODES for future comparison.
 
         do j=1,nnodes
            irealold(j)=ireal1(j)
         enddo
         nnodesold=nnodes
 
c.... Debug dumps.
 
         call cmo_get_info('idebug',cmo,
     *      idebug,length,icmotype,icscode)
 
         if (idebug.ge.3) then
 
            write(cbuf,'(a,i6.6,a)')'dump/gmv/gmvafteragd',i,'/'
            call dotaskx3d(cbuf // comend,ierr)
 
            write(cbuf,'(a,i6.6,a)')'dump/lagrit/lgdafteragd',i,'/'
            call dotaskx3d(cbuf // comend,ierr)
 
         endif
 
c.... Every 5 iterations, reduce our exit aspect ratio tolerance.
 
         if (mod(i,5).eq.0) tolar=tolar*0.25
 
c.... Large grids demand point compression WITHIN the iteration loop.
 
         if (lcompresswherepossible) then
            call dotaskx3d('rmpoint/compress; finish',ierror)
         endif
 
      enddo
 
      write(logmess,'(a)') 'MASSAGE:  hit max. iteration limit.'
      call writloga('default',0,logmess,0,ierrw)
 
 100  continue
 
 
c.... CEL creates nodes to reduce the maximum edge length to
c.... less than BISECTION_LENGTH.
C.... CEE bisects edges whose errors are greatest
 
      call getmpary(imsginsave,xmsginsave,cmsginsave,msgtypesave,
     &   ipmpary,mpno,psetname,ierror)
 
      nnodesold=nnodes

      if (lexclusive) then
         inclusive=0
         cmode='truncated'
      elseif (lsemiexclusive) then
         inclusive=0
         cmode='full'
      else
         inclusive=1
         cmode='full'
      endif

      if(isafield) then
c.... issue SETHESSIAN command to generate necessary 2nd derivatives.
 
         write(cbuf,'(3a)') 'sethessian/',adaption_field_name,
     *        '/ ; finish'
         call dotask(cbuf,ierror)
         if (ierror.ne.0) then
            write(logmess,'(a)')
     &      'MASSAGE: error computing Hessian'
            call writloga('default',0,logmess,0,ierrw)
            goto 9999
         endif
         call cmo_get_attinfo('frange',cmo,iout,range,cout,ipout,length
     &   ,icmotype,ierror)
         if(range.gt.epsilonl.and.ierror.eq.0) then
            call cee_chain(cmo,toldamage,mpary,
     *      mpno,inclusive,psetname,epsilonl,adaption_field_name,ierror)
         else
            go to 9999
         endif
      else
         call cel_chain(cmo,bisection_length,toldamage,mpary,mpno,
     &    inclusive,psetname,cmode,ierror)
      endif
      call cmo_get_info('nnodes',cmo,
     &   nnodes,length,icmotype,icscode)
      if (idebug.ge.3) then
 
         write(cbuf,'(a,i6.6,a)')'dump/gmv/gmvafterce',i,'/'
         call dotaskx3d(cbuf // comend,ierr)
 
      endif
      if (.not.llite) then
          if (nnodes.ne.nnodesold) then
          if (lrecon)then
             if (lcheckaxy) then
                write(cbuf,'(a,e16.8,a)')'recon/0/',toldamage
     &             ,'/checkaxy/ ; finish'
                call dotaskx3d(cbuf,ierr)
             else
                write(cbuf,'(a,e16.8,a)')'recon/0/',toldamage
     &             ,'/ ; finish'
                call dotaskx3d(cbuf,ierr)
             endif
          endif
          endif
      endif
 
c.... Refine on roughness if LCHECKROUGHNESS=.true.
 
      if (lcheckroughness) then
         call getmpary(imsginsave,xmsginsave,cmsginsave,msgtypesave,
     &      ipmpary,mpno,psetname,ierror)
         nnodesold=nnodes

         if (lexclusive) then
            inclusive=0
c            cmode='truncated'
c... We go with semiexclusive behavior here, because cer_chain has
c... not been fixed yet.
            cmode='full'
         elseif (lsemiexclusive) then
            inclusive=0
            cmode='full'
         else
            inclusive=1
            cmode='full'
         endif
         call cer_chain(cmo,tolroughness,toldamage,mpary,mpno,
     &      inclusive,psetname,cmode,ierror)
         call cmo_get_info('nnodes',cmo,
     &      nnodes,length,icmotype,icscode)
 
         if (nnodes.ne.nnodesold) then
         if (lrecon)then
            if (lcheckaxy) then
               write(cbuf,'(a,e16.8,a)')'recon/0/',toldamage
     &            ,'/checkaxy/ ; finish'
               call dotaskx3d(cbuf,ierr)
            else
               write(cbuf,'(a,e16.8,a)')'recon/0/',toldamage
     &            ,'/ ; finish'
               call dotaskx3d(cbuf,ierr)
            endif
         endif
         endif
      endif
 
 9999 continue
c
c   set ipointi, and ipointj to 'all nodes' after massage
c   the concept of 'last set of nodes' makes no sense
c
      call cmo_set_info('ipointi',cmo,1,1,1
     *       ,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
      call cmo_set_info('ipointj',cmo,nnodes,1,1
     *       ,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
      call mmrelprt(isubname,icscode)
 
      return
      end
 
