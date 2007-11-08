*DK extrude
      subroutine extrude(imsgin,xmsgin,cmsgin,msgtype,nwds,ierror)
C
C#######################################################################
C
C     PURPOSE -
C
C        This subroutine extrudes a pseudo-2D polyline (normals of the
C        curve pointing in more or less the same direction) into three
C        dimensions along either the normal to the curve (default) or a
C        user entered value.
C
C     NOTES -
C
C        Currently only for xyz coordinate system.
C        Syntax for this command:
C          extrude/sink_mesh_object/source_mesh_object/
C          const|min/
C          real|integer/
C          volume|bubble/ Note: Currently only supports volume
C          [norm|x1,y1,z1]
C
C        if argument 4 is const, argument 5 is considered to be a
C        constant offset from the surface; min, it is the minimum
C        distance from the surface to a reference plane. The extruding
C        vector is normal to the reference plane.
C
C        if argument 6 is volume, the extrusion will create volumes
C        from 2D shapes; if it is bubble, the extrusion will run
C        hextotet and extract on the resulting volume to create a shell
C        around the volume that was created. This argument is ignored
C        if the initial MO passed to extrude is made up of line
C        segments or points (i.e., if it is 1D topologically).
C
C        argument 7 states whether or not the extruding vector will be
C        the average normal to the surface, or a user specified vector.
C        This argument is optional, norm is the default. If the user
C        specifies a vector, the vector will be normalized (i.e., only
C        the directionality will be used)
C
C
C
C     INPUT ARGUMENTS -
C
C        xmsgin()  - REAL ARRAY OF COMMAND INPUT VALUES
C        cmsgin()  - CHARACTER ARRAY OF COMMAND INPUT VALUES
C        imsgin()  - INTEGER ARRAY OF COMMAND INPUT VALUES
C        msgtype() - INTEGER ARRAY OF COMMAND INPUT TYPE
C        nwds      - NO. OF WORDS OF COMMAND INPUT VALUES
C
C     CHANGE HISTORY -
C$Log:   /pvcs.config/t3d/src/extrude.f_a  $
CPVCS    
CPVCS       Rev 1.6   30 Sep 2004 11:15:10   dcg
CPVCS    make epsln double precision
CPVCS
CPVCS       Rev 1.5   22 Oct 2003 08:04:10   gable
CPVCS     ndimensions_geom of new MO was not being set correctly
CPVCS     which resulted in problems in other modules.
CPVCS
CPVCS       Rev 1.4   20 Jul 2000 14:02:44   bap
CPVCS    change call from interp to interp_lg
CPVCS
CPVCS       Rev 1.3   29 Jun 2000 10:54:48   bap
CPVCS    Incorporated bubble and interp into extrude.
CPVCS
CPVCS       Rev 1.2   07 Feb 2000 17:41:54   dcg
CPVCS    remove comdict.h
CPVCS
CPVCS       Rev 1.1   02 Feb 2000 07:27:50   gable
CPVCS    Added call to set_jtetoff to fill jtetoff array
CPVCS    just before geneii call.
CPVCS
CPVCS       Rev 1.0   Fri Aug 07 13:27:32 1998   dcg
CPVCS    Initial revision.
C
C
C#######################################################################
C
      implicit none
C
      include "local_element.h"
C
C     General global Parameters
      integer lenptr
      parameter (lenptr=1000000)
      real epsln
      parameter (epsln=1.0d-10)
C
C     Subroutine Input Parameters
 
C
C#######################################################################
C
C Variable Declarations
C
C#######################################################################
C
C     Subroutine Input Variables
C
      integer nwds
      character*(*) cmsgin(nwds)
      integer       imsgin(nwds), msgtype(nwds)
      real*8        xmsgin(nwds)
C
C
C     Bubble specific Variables
      logical isbubble
      integer httopt
 
C     Integer error variables
      integer ierror
C
C     Name Variables and Message Variables
C
      character*32  isubname, cmoin, cmoout, cmoout2, cmofinal
      character*132 logmess, cmdmess
C
C     Variables used to store temporary data for normal calculations
C
      real*8 xnorm_curr, ynorm_curr, znorm_curr
      real*8 xnorm_ref, ynorm_ref, znorm_ref
      real*8 xvect, yvect, zvect
      real*8 anorm, d
      real*8 refptx, refpty, refptz
      real*8 dotproduct
      integer id1, id2, id3
      integer pclose
C
C     Variables that do not serve any purpose but are required for
C     backward compatibility.
      integer iout, lout
C
C
C     Variables for Number of nodes, elements, nodes/element, etc.
C     (i.e., MO defining variables)
      integer nnodes, nelements, nsdtopo, nsdgeom, nen, nef
      integer neno
 
C     Counters
      integer i, itri, idx
C
C     Pointers used to store node info for various reasons
C
      pointer (ipnodeidx, nodeidx)
C
      integer   nodeidx(lenptr)
C
C     Pointers for incoming CMO
C     Node Based Attributes
      pointer (ipimt1, imt1)
      pointer (ipitp1, itp1)
      pointer (ipisn1, isn1)
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
C
C     Element Based Attributes
      pointer (ipitetclr, itetclr)
      pointer (ipitettyp, itettyp)
      pointer (ipitetoff, itetoff)
C
C     Array of No. of Elements*No. of Nodes per Element
      pointer (ipitet, itet)
C
 
      real*8    xic(lenptr), yic(lenptr), zic(lenptr)
      integer   imt1(lenptr), itp1(lenptr), isn1(lenptr)
      integer   itetclr(lenptr), itettyp(lenptr)
      integer   itet(4*lenptr), itetoff(lenptr)
C     The 4 is used to ensure that the pointer is large enough to handle
C     any surface.
C
C     Pointers for outgoing CMO
C
C     Node Based Attributes
      pointer (ipimt1o, imt1o)
      pointer (ipitp1o, itp1o)
      pointer (ipisn1o, isn1o)
      pointer (ipxico, xico)
      pointer (ipyico, yico)
      pointer (ipzico, zico)
C
C     Element Based Attributes
      pointer (ipitetclro, itetclro)
      pointer (ipitettypo, itettypo)
      pointer (ipitetoffo, itetoffo)
C
C     Array of No. of Elements*No. of Nodes per Element
      pointer (ipiteto, iteto)
C
      real*8 xico(lenptr), yico(lenptr), zico(lenptr)
      integer   imt1o(lenptr), itp1o(lenptr), isn1o(lenptr)
      integer   itetclro(lenptr), itettypo(lenptr)
      integer   iteto(8*lenptr), itetoffo(lenptr)
C     The 8 is used to ensure that the pointer is large enough to handle
C     any surface.
C
      real*8 dbarea
C
C#######################################################################
C
C     Initialize Error Flag and other assorted goodies
C
      ierror = 0
      cmoin = '-cmo-'
      cmoout = '-none-'
      cmoout2 = '-none-'
      cmofinal = '-none-'
      isubname = 'extrude'
      isbubble = .FALSE.
C
C#######################################################################
C
C     Check the gross syntax of the command entered
C
      if ((nwds.eq.11).AND.(cmsgin(4).eq.'interp')) then
         call interp_lg(imsgin,xmsgin,cmsgin,msgtype,nwds,ierror)
         return
      elseif(.NOT.(((nwds.eq.6).OR.(nwds.eq.7).OR.(nwds.eq.9)).AND.
     &     ((cmsgin(4).eq.'min').OR.(cmsgin(4).eq.'const')))) then
         write(logmess,'(a)')
     &        'Error in subroutine extrude: The proper Syntax is:'
         call writloga('default',0,logmess,0,ierror)
         write(logmess,'(a)')
     &       'extrude/cmoout/cmoin/min|const/offset/'
     &       // 'volume|bubble/[norm|x1,y1,z1]  OR'
         call writloga('default',0,logmess,0,ierror)
         write(logmess,'(a)')
     $        'extrude/cmoout/cmoin/interp/layers/range1/range2'
         call writloga('default',0,logmess,0,ierror)
         write(logmess,'(a)')
     $        'Where range1 and range2 are of the form: '
     $        // 'pset,get,<name> or ifirst,ilast,istride'
         call writloga('default',0,logmess,0,ierror)
         ierror = 1
         go to 9999
      endif
C
C
C#######################################################################
C
C     Check for old version of input stack
C
      if((cmsgin(6).eq.'p').OR.(cmsgin(6).eq.'s')) then
         write(logmess,'(a)')
     &        'Warning: This syntax is obsolete. The new syntax is:'
         call writloga('default',0,logmess,0,ierror)
         write(logmess,'(a)')
     &       'extrude/cmoout/cmoin/min|const/offset/'
     &       // 'volume|bubble/[norm|x1,y1,z1]'
         call writloga('default',0,logmess,0,ierror)
         write(logmess,'(a)')
     &        'Continuing using a volume extrusion.'
         call writloga('default',0,logmess,0,ierror)
      endif
C
C#######################################################################
C
C     Initialize the Mesh Objects (Harder than it sounds)
C
C     ******************************************************************
C     Check if the incoming MO exists
C
      cmoin=cmsgin(3)
      call cmo_exist(cmoin,ierror)
      if(ierror.ne.0) then
         write(logmess,'(a)')
     &        'Error in subroutine extrude: input MO does not exist'
         call writloga('default',0,logmess,0,ierror)
         ierror = 1
         go to 9999
      endif
C
C     ******************************************************************
C     Get incoming MO information
C
      call cmo_get_info('nnodes',cmoin,nnodes,iout,lout,ierror)
      call cmo_get_info('nelements',cmoin,nelements,iout,lout,ierror)
      call cmo_get_info('ndimensions_topo',cmoin,nsdtopo,iout,lout,
     &     ierror)
      call cmo_get_info('ndimensions_geom',cmoin,nsdgeom,iout,lout,
     &     ierror)
      call cmo_get_info('nodes_per_element',cmoin,nen,iout,lout,ierror)
      call cmo_get_info('faces_per_element',cmoin,nef,iout,lout,ierror)
      call cmo_get_info('itp1',cmoin,ipitp1,iout,lout,ierror)
      call cmo_get_info('imt1',cmoin,ipimt1,iout,lout,ierror)
      call cmo_get_info('isn1',cmoin,ipisn1,iout,lout,ierror)
      call cmo_get_info('xic',cmoin,ipxic,iout,lout,ierror)
      call cmo_get_info('yic',cmoin,ipyic,iout,lout,ierror)
      call cmo_get_info('zic',cmoin,ipzic,iout,lout,ierror)
      call cmo_get_info('itetclr',cmoin,ipitetclr,iout,lout,ierror)
      call cmo_get_info('itettyp',cmoin,ipitettyp,iout,lout,ierror)
      call cmo_get_info('itet',cmoin,ipitet,iout,lout,ierror)
      call cmo_get_info('itetoff',cmoin,ipitetoff,iout,lout,ierror)
C
C     ******************************************************************
C     Check & see if the incoming MO is eligible for this transformation
C     (i.e., is it topologically <= 2D, and if it is points, lines, or
C     hybrid elements, that a normal vector is supplied)
C
      if((nsdtopo.gt.2).AND.(nen.ne.10)) then
         write(logmess,'(a)')
     &        'Error in subroutine extrude: cmoin is not <= 2D!'
         call writloga('default',0,logmess,0,ierror)
         ierror = 1
         go to 9999
      endif
      if(((nen.le.2).OR.(nen.eq.10)).AND.(nwds.ne.9)) then
         write(logmess,'(a)')
     &        'Error in subroutine extrude: You must specify a normal!'
         call writloga('default',0,logmess,0,ierror)
         ierror = 1
         go to 9999
      endif
C
C     ******************************************************************
C     Figure out what kind of extrusion we are doing (volume or bubble)
C     and set up the cmoout accordingly...
C
      cmofinal = cmsgin(2)
      if(cmsgin(6).eq.'bubble') then
C        We are doing a bubble extrusion, make sure that the incoming
C        MO is eligible.
         isbubble = .TRUE.
         if((nen.ne.3).AND.(nen.ne.4).AND.(nen.lt.9)) then
            write(logmess,'(a)')
     &           'Error: Option bubble requires input MO to be made up'
            call writloga('default',0,logmess,0,ierror)
            write(logmess,'(a)')
     &           ' of triangles, quads, or hybrid elements!'
            call writloga('default',0,logmess,0,ierror)
            ierror = 1
            go to 9999
         endif
C
C        Set up temporary names.
C        Part 1:
         cmoout = 'cmotmp1'
C     The length of 'cmotmp1' is 7 chars... (magic number)
C     The length of cmoout is 32 chars... (magic number)
         i=7
         call cmo_exist(cmoout,ierror)
         do while (ierror.eq.0)
            if(i.gt.32) then
               write(logmess, '(a)')
     &              'Error! All temporary mo names are in use!'
               call writloga('default',0,logmess,0,ierror)
               ierror = 1
               go to 9999
            endif
 
            cmoout(i:i)='%'
            i=i+1
 
            call cmo_exist(cmoout,ierror)
         enddo
C
C        Part 2
 
         cmoout2 = 'cmotmp2'
C     The length of 'cmotmp2' is 7 chars... (magic number)
C     The length of cmoout2 is 32 chars... (magic number)
         i=7
         call cmo_exist(cmoout2,ierror)
         do while (ierror.eq.0)
            if(i.gt.32) then
               write(logmess, '(a)')
     &              'Error! All temporary mo names are in use!'
               call writloga('default',0,logmess,0,ierror)
               ierror = 1
               go to 9999
            endif
 
            cmoout2(i:i)='%'
            i=i+1
            call cmo_exist(cmoout2,ierror)
         enddo
C
C     Figure out the proper hextotet conversion option.
         if(nen.eq.3) then
            httopt=3
         elseif(nen.eq.4) then
            httopt=5
         elseif(nen.ge.9) then
            write(logmess,'(a)')
     &           'Warning: hextotet may get confused, '
     &           // 'output may be garbled.'
            call writloga('default',0,logmess,0,ierror)
            httopt=5
         endif
 
      elseif((cmsgin(6).eq.'volume').OR.(cmsgin(6).eq.'p')
     &        .OR.(cmsgin(6).eq.'s')) then
         cmoout = cmofinal
      else
         write(logmess, '(a)')
     &        'Error! You must specify a volume or bubble extrusion!'
         call writloga('default',0,logmess,0,ierror)
         ierror = 1
         goto 9999
      endif
C
C     ******************************************************************
C     Begin setting up the output MO: topology and geometry
C
      if(nsdtopo.lt.3) then
         nsdtopo = nsdtopo+1
      endif
      if(nsdgeom.lt.3) then
         nsdgeom=nsdgeom+1
      endif
C
C     ******************************************************************
C     Create the output MO
C
C     Check if the output MO exists, if it does, remove it.
      call cmo_exist(cmoout,ierror)
      if(ierror.eq.0) call cmo_release(cmoout,ierror)
C
      call cmo_create(cmoout,ierror)
C
C     Set the information for the type of mesh object this happens to be
C
      call cmo_set_info('nnodes',cmoout,2*nnodes,1,1,ierror)
      call cmo_set_info('nelements',cmoout,nelements,1,1,ierror)
      call cmo_set_info('ndimensions_topo',cmoout,nsdtopo,1,1,ierror)
      call cmo_set_info('ndimensions_geom',cmoout,nsdgeom,1,1,ierror)
C
C     Differentiate between two groups:
C       hybrids vs. points, lines, triangles, and quads.
C
C     Hybrids
      if((nen.eq.10)) then
         neno=nen
         call cmo_set_info('nodes_per_element',cmoout,nen,1,1,ierror)
         call cmo_set_info('faces_per_element',cmoout,nef,1,1,ierror)
C     points, quads, triangles, and lines
      else
         neno=2*nen
         call cmo_set_info('nodes_per_element',cmoout,2*nen,1,1,ierror)
         call cmo_set_info('faces_per_element',cmoout,2+nef,1,1,ierror)
      endif
C
      if(nen.le.3) then
         call cmo_set_info('edges_per_element',cmoout,nen**2,1,1,ierror)
      else
         call cmo_set_info('edges_per_element',cmoout,12,1,1,ierror)
      endif
C
C     Reallocate memory.
      call cmo_newlen(cmoout,ierror)
C
C     ******************************************************************
C     Get output MO information
C
      call cmo_get_info('imt1',cmoout,ipimt1o,iout,lout,ierror)
      call cmo_get_info('itp1',cmoout,ipitp1o,iout,lout,ierror)
      call cmo_get_info('isn1',cmoout,ipisn1o,iout,lout,ierror)
      call cmo_get_info('xic',cmoout,ipxico,iout,lout,ierror)
      call cmo_get_info('yic',cmoout,ipyico,iout,lout,ierror)
      call cmo_get_info('zic',cmoout,ipzico,iout,lout,ierror)
      call cmo_get_info('itetclr',cmoout,ipitetclro,iout,lout,ierror)
      call cmo_get_info('itettyp',cmoout,ipitettypo,iout,lout,ierror)
      call cmo_get_info('itet',cmoout,ipiteto,iout,lout,ierror)
      call cmo_get_info('itetoff',cmoout,ipitetoffo,iout,lout,ierror)
C
C#######################################################################
C
C     Initialize local arrays
C
C     ******************************************************************
C     Allocate and Initialize memory for node information
C
      call mmgetblk('nodeidx',isubname,ipnodeidx,nen,1,ierror)
      if(ierror.ne.0) call x3d_error(isubname,'mmgetblk')
C
      do i=1,nen
         nodeidx(i)=0
      enddo
C
C#######################################################################
C
C     If the user wants to check for a planar surface, or the average
C     normal is to be used, check if that is feasiblie, and if so,
C     calculate the normal for each element.
C
      if((nwds.eq.6).OR.(cmsgin(7).eq.'norm'))then
C
         if((nen.ne.3).AND.(nen.ne.4)) then
            write(logmess,'(a)')
     &         'Error in subroutine extrude: cmoin must be tri or quad!'
            call writloga('default',0,logmess,0,ierror)
            ierror = 1
            go to 9999
         endif
C
C        Initialize the Normal variables.
         xvect=0.0
         yvect=0.0
         zvect=0.0
C
         do itri=1,nelements
C
C        Get the number of nodes, indicies, etc...
            do i=1,nelmnen(itettyp(itri))
               nodeidx(i)=itet(itetoff(itri)+i)
            enddo
C
            do i=1,nelmnen(itettyp(itri))
               id1=nodeidx(mod(i,nelmnen(itettyp(itri)))+1)
               id2=nodeidx(mod((i+1),nelmnen(itettyp(itri)))+1)
               id3=nodeidx(mod((i+2),nelmnen(itettyp(itri)))+1)
C
C              Calculate out the normals, and make sure they point in
C              the same direction.
               xnorm_curr=dbarea(yic(id1),zic(id1),
     &              yic(id2),zic(id2),yic(id3),zic(id3))
               ynorm_curr=dbarea(zic(id1),xic(id1),
     &              zic(id2),xic(id2),zic(id3),xic(id3))
               znorm_curr=dbarea(xic(id1),yic(id1),
     &              xic(id2),yic(id2),xic(id3),yic(id3))
               anorm=sqrt(xnorm_curr*xnorm_curr+
     &              ynorm_curr*ynorm_curr+
     &              znorm_curr*znorm_curr)
C
C              If average was selected, go for it...
               if((nwds.eq.6).OR.(cmsgin(7).eq.'norm')) then
                  xvect = xvect+xnorm_curr
                  yvect = yvect+ynorm_curr
                  zvect = zvect+znorm_curr
               endif
            enddo
         enddo
      endif
C
C#######################################################################
C
C     Now that all the loop-based pre-processing is done, get info
C     needed to create the new MO
C
C     ******************************************************************
C     Get the direction and magnitude of the extruding vector
C
C     Magnitude...
      if(msgtype(5).eq.1) then
         d=imsgin(5)
      elseif(msgtype(5).eq.2) then
         d=xmsgin(5)
      else
         write(logmess,'(a)')
     &        'Error in subroutine extrude: offset is not a number!'
         call writloga('default',0,logmess,0,ierror)
         ierror = 1
         go to 9999
      endif
C
C     Direction...
      if((nwds.ne.6).AND.(cmsgin(7).ne.'norm').AND.(nwds.ne.9)) then
         write(logmess,'(a)')
     &        'Error in subroutine extrude: invalid extruding vector!'
         call writloga('default',0,logmess,0,ierror)
         ierror = 1
         go to 9999
      elseif(nwds.eq.9) then
C     GET THE X PART OF THE EXTRUDING VECTOR
         if(msgtype(7).eq.1) then
            xvect=imsgin(7)
         elseif(msgtype(7).eq.2) then
            xvect=xmsgin(7)
         else
            write(logmess,'(a)')
     &          'Error in subroutine extrude: x vector is not a number!'
            call writloga('default',0,logmess,0,ierror)
            ierror = 1
            go to 9999
         endif
C     GET THE Y PART OF THE EXTRUDING VECTOR
         if(msgtype(8).eq.1) then
            yvect=imsgin(8)
         elseif(msgtype(8).eq.2) then
            yvect=xmsgin(8)
         else
            write(logmess,'(a)')
     &          'Error in subroutine extrude: y vector is not a number!'
            call writloga('default',0,logmess,0,ierror)
            ierror = 1
            go to 9999
         endif
C     GET THE Z PART OF THE EXTRUDING VECTOR
         if(msgtype(9).eq.1) then
            zvect=imsgin(9)
         elseif(msgtype(9).eq.2) then
            zvect=xmsgin(9)
         else
            write(logmess,'(a)')
     &          'Error in subroutine extrude: z vector is not a number!'
            call writloga('default',0,logmess,0,ierror)
            ierror = 1
            go to 9999
         endif
      endif
C
C     ******************************************************************
C     Normalize the direction
C
      anorm=sqrt(xvect*xvect+
     &     yvect*yvect+
     &     zvect*zvect)
      xvect = xvect/anorm
      yvect = yvect/anorm
      zvect = zvect/anorm
C
C     ******************************************************************
C     Figure out if the offset will be minimum or constant and react
C     accordingly
C
      if(cmsgin(4).eq.'min') then
         call minpt(xic,yic,zic,nnodes,xvect,yvect,zvect,d,pclose)
         refptx=xic(pclose)+d*xvect
         refpty=yic(pclose)+d*yvect
         refptz=zic(pclose)+d*zvect
      elseif(cmsgin(4).ne.'const') then
         write(logmess,'(a)')
     &        'Warning: argument 4 is not min or const, assuming const!'
         call writloga('default',0,logmess,0,ierror)
      endif
C
C#######################################################################
C
C     All the pre-processing is done...Start the process of creating the
C     new MO
C
C     ******************************************************************
C     Make copies of the nodes the appropriate distance away.
C
      do i=1,nnodes
         if(cmsgin(4).eq.'min') then
            d=(refptx-xic(i))*(xvect)+
     &           (refpty-yic(i))*(yvect)+
     &           (refptz-zic(i))*(zvect)
         endif
         xico(i)=xic(i)
         yico(i)=yic(i)
         zico(i)=zic(i)
         xico(i+nnodes)=xic(i)+d*xvect
         yico(i+nnodes)=yic(i)+d*yvect
         zico(i+nnodes)=zic(i)+d*zvect
         imt1o(i)=imt1(i)
         imt1o(i+nnodes)=imt1(i)
         if(isn1(i).ne.0) then
            isn1o(i)=isn1(i)
            isn1o(i+nnodes)=isn1(i)+nnodes
         endif
      enddo
C
C     ******************************************************************
C     Set up the attributes of the new MO
C
      do itri=1,nelements
C
C        ItetColor
         itetclro(itri)=itetclr(itri)
C
C        ItetOffset
         if(nen.gt.4) then
            itetoffo(itri)=itetoff(itri)
         else
            itetoffo(itri)=2*itetoff(itri)
         endif
C
C        ItetType
         if((itettyp(itri).le.2).OR.(itettyp(itri).eq.4)) then
            itettypo(itri)=2*itettyp(itri)
         elseif(itettyp(itri).eq.3) then
            itettypo(itri)=4+itettyp(itri)
         elseif(itettyp(itri).ge.9) then
            itettypo(itri)=10
         endif
C
         if(nen.gt.2) then
            xnorm_ref=0.0
            ynorm_ref=0.0
            znorm_ref=0.0
C
            do i=1,nelmnen(itettyp(itri))
               nodeidx(i)=itet(itetoff(itri)+i)
            enddo
C
            do i=1,nelmnen(itettyp(itri))
               id1=nodeidx(mod(i,nelmnen(itettyp(itri)))+1)
               id2=nodeidx(mod((i+1),nelmnen(itettyp(itri)))+1)
               id3=nodeidx(mod((i+2),nelmnen(itettyp(itri)))+1)
C
C              Calculate out the normals, and their magnitudes.
               xnorm_curr=dbarea(yic(id1),zic(id1),
     &              yic(id2),zic(id2),yic(id3),zic(id3))
               ynorm_curr=dbarea(zic(id1),xic(id1),
     &              zic(id2),xic(id2),zic(id3),xic(id3))
               znorm_curr=dbarea(xic(id1),yic(id1),
     &              xic(id2),yic(id2),xic(id3),yic(id3))
               anorm=sqrt(xnorm_curr*xnorm_curr+
     &              ynorm_curr*ynorm_curr+
     &              znorm_curr*znorm_curr)
C
C              Normalize and add the normalized normals together, giving
C              an idea of where the normals point
               xnorm_curr=xnorm_curr/anorm
               ynorm_curr=ynorm_curr/anorm
               znorm_curr=znorm_curr/anorm
               xnorm_ref=xnorm_ref+xnorm_curr
               ynorm_ref=ynorm_ref+ynorm_curr
               znorm_ref=znorm_ref+znorm_curr
            enddo
C
C           Normalize the reference normals.
            anorm=sqrt(xnorm_ref*xnorm_ref+
     &           ynorm_ref*ynorm_ref+
     &           znorm_ref*znorm_ref)
            xnorm_ref=xnorm_ref/anorm
            ynorm_ref=ynorm_ref/anorm
            znorm_ref=znorm_ref/anorm
C
C           Check the dotproduct of the elements pseudo normal vector
C           and the extruding vector direction if it's >= 0, react
C           accordingly.
            dotproduct=xnorm_ref*xvect+
     &           ynorm_ref*yvect+
     &           znorm_ref*zvect
C
            if(dotproduct.gt.0) then
               do idx=1,nelmnen(itettyp(itri))
                  iteto(2*itetoff(itri)+idx)=itet(itetoff(itri)+idx)
                  iteto(2*itetoff(itri)+idx+nelmnen(itettyp(itri)))=
     &                 itet(itetoff(itri)+idx)+nnodes
               enddo
            else
               do idx=1,nelmnen(itettyp(itri))
                  iteto(2*itetoff(itri)+idx)=
     &                 itet(itetoff(itri)+idx)+nnodes
                  iteto(2*itetoff(itri)+idx+nelmnen(itettyp(itri)))=
     &                 itet(itetoff(itri)+idx)
               enddo
            endif
         elseif(nen.eq.2) then
            iteto(2*itetoff(itri)+1)=itet(itetoff(itri)+1)
            iteto(2*itetoff(itri)+1+nelmnen(itettyp(itri)))=
     &           itet(itetoff(itri)+2)+nnodes
            iteto(2*itetoff(itri)+2)=itet(itetoff(itri)+1)+nnodes
            iteto(2*itetoff(itri)+2+nelmnen(itettyp(itri)))=
     &           itet(itetoff(itri)+2)
         elseif(nen.eq.1) then
            iteto(2*itetoff(itri)+1)=itet(itetoff(itri)+1)
            iteto(2*itetoff(itri)+1+nelmnen(itettyp(itri)))=
     &           itet(itetoff(itri)+1)+nnodes
         endif
      enddo
C
C     ****************************************************************
C     Set up the connectivity of the new MO and fill the jtetoff array.
C
      call set_jtetoff()
      call dotaskx3d('resetpts itp; finish',ierror)
C
C     ******************************************************************
C     See if we need to make the resulting MO a bubble.
      if (isbubble) then
C
C     HextoTet and Extract commands create the bubble
C
         write(cmdmess,35) httopt,cmoout2,cmoout
 35      format('hextotet/',I1,'/',A,'/',A,'; finish')
         call dotaskx3d(cmdmess,ierror)
         if(ierror.ne.0) then
            goto 9998
         endif
C
         write(cmdmess,40) cmofinal,cmoout2
 40      format('extract/intrface/-all-/1 0 0/',A,'/',A,'; finish')
         call dotaskx3d(cmdmess,ierror)
         if(ierror.ne.0) then
            goto 9998
         endif
C     Release Temporary cmos
C
 9998    continue
         call cmo_exist(cmoout,ierror)
         if(ierror.eq.0) call cmo_release(cmoout,ierror)
         call cmo_exist(cmoout2,ierror)
         if(ierror.eq.0) call cmo_release(cmoout2,ierror)
      endif
C
C     ******************************************************************
C     Release temporary memory and be done with it
C
 9999 continue
 9995 call mmrelprt(isubname,ierror)
      return
      end
C
C#####################################################################
C
C     Subroutine to calculate the closest point to the reference plane
C
C#####################################################################
C
      subroutine minpt(xin,yin,zin,nnodes,xvect,yvect,
     &     zvect,d,pout)
 
      implicit none
      integer nnodes, p1, p2
      integer pout
      real*8 xin(nnodes), yin(nnodes), zin(nnodes)
      real*8 xvect,yvect,zvect,d,d1
      real*8 dotproduct,p3x,p3y,p3z
 
      if(d.eq.0) then
         d1=.1
      else
         d1=d
      endif
 
      p1=1
      p3x=xin(p1)+d1*xvect
      p3y=yin(p1)+d1*yvect
      p3z=zin(p1)+d1*zvect
 
      do p2=2,nnodes
         dotproduct=(p3x-xin(p1))*(xin(p2)-xin(p1))+
     &        (p3y-yin(p1))*(yin(p2)-yin(p1))+
     &        (p3z-zin(p1))*(zin(p2)-zin(p1))
         if(dotproduct.gt.0) then
            p1=p2
            p3x=xin(p2)+d1*xvect
            p3y=yin(p2)+d1*yvect
            p3z=zin(p2)+d1*zvect
        endif
      enddo
 
      pout = p1
      return
      end
C
C#######################################################################
C
C Function DBArea:
C
C     Returns double the area of a triangle ordered (counterclockwise)
C     1,2,3 in the u-v plane. This means that for a triangle ordered
C     1,2,3 in x-y-z space, the (r.h. rule) vector normal to this
C     triangle
C     with magnitude equal to double the area is given by:
C                    < dbarea(y1,z1,y2,z2,y3,z3),
C                      dbarea(z1,x1,z2,x2,z3,x3),
C                      dbarea(x1,y1,x2,y2,x3,y3) >.
C
C#######################################################################
C
 
      real*8 function dbarea(u1,v1,u2,v2,u3,v3)
      implicit none
      real*8 u1,v1,u2,v2,u3,v3
      dbarea = (u2-u1)*(v3-v1)-(v2-v1)*(u3-u1)
      return
      end
 
