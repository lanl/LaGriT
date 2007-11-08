      subroutine intersect_elements(imsgin,xmsgin,cmsgin,
     &                                  msgtype,nwds,ierror)
C
C######################################################################
C
C     PURPOSE -
C
C        The Purpose of this subroutine is to take the intersection of
C        the two mesh objects and note in which elements of mesh object
C        one the elements of mesh object two intersect.
C
C     NOTES -
C
C        Syntax for this command:
C          intersect_elements/sink_mesh_object/source_mesh_object/
C          [attrib_name]
C        See web documentation for limitations and supported features.
C
C     INPUT ARGUMENTS -
C
C        xmsgin()  - REAL ARRAY OF COMMAND INPUT VALUES
C        cmsgin()  - CHARACTER ARRAY OF COMMAND INPUT VALUES
C        imsgin()  - INTEGER ARRAY OF COMMAND INPUT VALUES
C        msgtype() - INTEGER ARRAY OF COMMAND INPUT TYPE
C        nwds      - NO. OF WORDS OF COMMAND INPUT VALUES
C
C     $Log: intersect_elements.f,v $
C     Revision 2.00  2007/11/05 19:45:59  spchu
C     Import to CVS
C
CPVCS    
CPVCS       Rev 1.10   23 Jun 2006 08:27:16   tam
CPVCS    check for incoming attribute containing negative values
CPVCS    
CPVCS       Rev 1.9   12 Oct 2005 14:42:06   gable
CPVCS    Minor changes to log messages.
CPVCS    
CPVCS       Rev 1.8   23 Mar 2005 08:57:44   dcg
CPVCS    fix declarations for linux
CPVCS    
CPVCS       Rev 1.7   23 Feb 2005 08:32:00   tam
CPVCS    changed to implicit none
CPVCS    added code to report progress of the element search
CPVCS    
CPVCS       Rev 1.6   30 Sep 2004 11:18:48   dcg
CPVCS    make epsln double precision
CPVCS    
CPVCS       Rev 1.5   09 Nov 2001 12:11:16   gable
CPVCS    Fixed error in declaration if itetkid array. It was set
CPVCS    to real*8 when it should be integer. This caused errors
CPVCS    when the input mesh was an AMR type mesh.
CPVCS    
CPVCS       Rev 1.4   Tue Feb 22 14:33:10 2000   dcg
CPVCS    get rid of capital letters HP loader did something weird
CPVCS    
CPVCS       Rev 1.3   08 Feb 2000 08:37:32   dcg
CPVCS    remove comdict
CPVCS    
CPVCS       Rev 1.2   28 Jan 2000 10:03:36   gable
CPVCS    Code was not releasing the kdtree data structure. Now it does.
CPVCS    
CPVCS       Rev 1.1   18 Jan 2000 10:56:28   gable
CPVCS    Fixed header to insure log information goes into source code.
C
C     There is a name change from the old routine
C     xsectelementscmo.f to intersect_elements.f
C
C     Log:/pvcs.config/t3d/src/xsectelementscmo.f_a
CPVCS
CPVCS       Rev 1.3   07 Jan 2000 14:31:50   bap
CPVCS    Changed the default prefix for the intersection attribute from xsect_ to in_
CPVCS
CPVCS       Rev 1.2   06 Jan 2000 14:30:16   bap
CPVCS    Added support for X3D AMR grids. Removed extra diagnostic statements.
CPVCS
CPVCS       Rev 1.1   Wed Aug 04 10:44:32 1999   bap
CPVCS    Added PVCS Header information
C
C
C######################################################################
C
C
C     implicit real*8 (a-h, o-z)
      implicit none
C
      include "machine.h"
      include "local_element.h"
C
      integer lenptr
      parameter (lenptr=1000000)
      real*8 epsln
      parameter (epsln=1.0d-10)
C
C
C######################################################################
C
C Variable Declarations
C
C######################################################################
C
C     Subroutine Input Variables
C
      integer nwds
      character*(*) cmsgin(nwds)
      integer       imsgin(nwds), msgtype(nwds)
      real*8        xmsgin(nwds)
      integer ierror, ineg, init_attrib
C
C     Routine Variables and Message Variables
C
      character*32  isubname, cmoin, cmoout
      character*64  attribname
      character*132 logmess
      character*256 cmdmess
C
C
      integer i,j,k,l,m,n,orig_nwds,ipt,istep,iwrite,nwrite,
     * num_snk,totsrchd,totfind,ierror2,nnodesin,ilen,icmotype,
     * nelementsin,nenin,lenitettyp,lenitetoff,nnodesout,
     * nenout,num_src,totflag,iperc,iflag,numfound,ifound,iisrc,
     * ierrw,nelementsout,iomrgrid,ifoundstep,ireport,ierr
 
      integer icharlnf, strlen
 
      real*8 xperc, zero, xtrans,ytrans,ztrans,
     * xmin,ymin,zmin,xmax,ymax,zmax,local_epsilon
 
C     Variables used to store temporary info and indices
 
      real*8 xnodes1(12),ynodes1(12),znodes1(12)
      real*8 xnodes2(12),ynodes2(12),znodes2(12)
C
C     Pointers for incoming CMO
C
C     Node Based Attributes
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      real*8 xic(lenptr), yic(lenptr), zic(lenptr)
C
C     Element Based Attributes
C     The 8 is used to ensure the itet array can handle any element
      pointer (ipitettyp, itettyp)
      pointer (ipitetoff, itetoff)
      pointer (ipitet, itet)
      integer itettyp(lenptr), itetoff(lenptr), itet(8*lenptr)
C
C     Pointers for outgoing CMO
C
C     Node Based Attributes
      pointer (ipxico, xico)
      pointer (ipyico, yico)
      pointer (ipzico, zico)
      real*8 xico(lenptr), yico(lenptr), zico(lenptr)
C
C     Element Based Attributes
      pointer (ipitettypo, itettypo)
      pointer (ipiteto, iteto)
      pointer (ipitetoffo, itetoffo)
      integer itettypo(lenptr), iteto(8*lenptr), itetoffo(lenptr)
 
      pointer (ipitetkido, itetkido)
      pointer (ipitetxsecto, itetxsecto)
      integer itetkido(lenptr), itetxsecto(lenptr)
 
C
C     K-d tree type stuff
      pointer (iplinkto,linkto)
      pointer (ipitfound,itfound)
      integer   linkto(lenptr), itfound(lenptr)
 
      pointer (ipsboxo,sboxo)
      real*8  sboxo(2,3,lenptr)

      parameter(local_epsilon=1.0d-10)
C
C
C#####################################################################
C
C     Initialize Error Flag and other assorted goodies
C
      orig_nwds = nwds
      ierror = 0
      ineg = 0
      init_attrib = 0
      ierror2 = 0
      zero = 0.0
      cmoin = '-cmo-'
      cmoout = '-none-'
      isubname = 'intersect_elements'
 
 
C
C######################################################################
C
C     Check the gross syntax of the command entered
C
C     Check for optional -init- to initialize the intersect attribute
      if(msgtype(orig_nwds).eq.3 .and. orig_nwds.gt.3) then
         strlen=icharlnf(cmsgin(orig_nwds))
         if (cmsgin(orig_nwds)(1:strlen).eq.'-init-') then
             init_attrib = 1
             orig_nwds = orig_nwds-1
         endif
      endif
      if((orig_nwds.ne.4).and.(orig_nwds.ne.3)) then
         write(logmess,'(a)')
     &        'Error in subroutine intersect_elements: The Syntax is:'
         call writloga('default',0,logmess,0,ierror)
         write(logmess,'(a)')
     &  'intersect_elements/mesh_sink/mesh_source/[attrib_name][-init-]'
         call writloga('default',0,logmess,0,ierror)
         ierror = 1
         go to 9999
      endif
C
C######################################################################
C
C     Initialize the Mesh Objects (Harder than it sounds)
C
C     *****************************************************************
C     Ensure that the incoming MO name is a valid one, and if it is
C     the default, get the real name.
C
      strlen=icharlnf(cmsgin(3))
      cmoin=cmsgin(3)(1:strlen)
      if((cmoin.eq.'-cmo-').or.(cmoin.eq.'-def-')) then
         call cmo_get_name(cmoin,ierror)
      endif
C
C     *****************************************************************
C     Check if the incoming MO exists
C
      call cmo_exist(cmoin,ierror)
      if(ierror.ne.0) then
         write(logmess,'(a)')
     &        'Error in intersect_elements: input MO does not exist'
         call writloga('default',0,logmess,0,ierror)
         ierror = 1
         go to 9999
      endif
C
C     *****************************************************************
C     Check if the outgoing MO exists
C
      strlen=icharlnf(cmsgin(2))
      cmoout=cmsgin(2)(1:strlen)
      call cmo_exist(cmoout,ierror)
      if(ierror.ne.0) then
         write(logmess,'(a)')
     &        'Error in intersect_elements: output MO does not exist'
         call writloga('default',0,logmess,0,ierror)
         ierror = 1
         go to 9999
      endif
C
C     *****************************************************************
C     Ensure that the incoming and outgoing MOs aren't the same.
      if(cmoin.eq.cmoout) then
         write(logmess,'(a)')
     &        'Error in intersect_elements: input and output MOs'
     &        // ' are the same!'
         call writloga('default',0,logmess,0,ierror)
         ierror = 1
         goto 9999
      endif
C
C     *****************************************************************
C     Get the pointers for cmoin pointing to some valid data
      call cmo_get_info('nnodes',cmoin,nnodesin,ilen,icmotype,ierror)

      call cmo_get_info('nelements',cmoin,
     &     nelementsin,ilen,icmotype,ierror)
      call cmo_get_info('nodes_per_element',cmoin,
     &     nenin,ilen,icmotype,ierror)
      call cmo_get_info('xic',cmoin,ipxic,ilen,icmotype,ierror)
      call cmo_get_info('yic',cmoin,ipyic,ilen,icmotype,ierror)
      call cmo_get_info('zic',cmoin,ipzic,ilen,icmotype,ierror)
      call cmo_get_info('itettyp',cmoin,ipitettyp,lenitettyp,icmotype
     &     ,ierror)
      call cmo_get_info('itetoff',cmoin,ipitetoff,lenitetoff,icmotype
     &     ,ierror)
      call cmo_get_info('itet',cmoin,ipitet,ilen,icmotype,ierror)
C
C     *****************************************************************
C     Get the pointers for cmoout pointing to some valid data
      call cmo_get_info('nnodes',cmoout,nnodesout,ilen,icmotype,ierror)
      call cmo_get_info('nelements',cmoout,
     &     nelementsout,ilen,icmotype,ierror)
      call cmo_get_info('nodes_per_element',cmoin,
     &     nenout,ilen,icmotype,ierror)
      call cmo_get_info('xic',cmoout,ipxico,ilen,icmotype,ierror)
      call cmo_get_info('yic',cmoout,ipyico,ilen,icmotype,ierror)
      call cmo_get_info('zic',cmoout,ipzico,ilen,icmotype,ierror)
      call cmo_get_info('itettyp',cmoout,ipitettypo,lenitettyp,icmotype
     &     ,ierror)
      call cmo_get_info('itetoff',cmoout,ipitetoffo,lenitetoff,icmotype
     &     ,ierror)
      call cmo_get_info('itet',cmoout,ipiteto,ilen,icmotype,ierror)
C
C     *****************************************************************
C     Make sure that we got what we wanted.
      if(ierror.ne.0) then
         write(logmess,'(a)')
     &        'Error in intersect_elements: a cmo_get_info failed'
         call writloga('default',0,logmess,0,ierror)
         ierror = 1
         go to 9999
      endif
      if (nelementsout.lt.1) then
         write(logmess,'(a,a,a)')
     &   'Warning: ',cmoout(1:icharlnf(cmoout)),
     &   ' has no elements for intersect.'
         call writloga('default',0,logmess,0,ierror)
      endif
      if (nelementsin.lt.1) then
         write(logmess,'(a,a,a)')
     &   'Warning: ',cmoin(1:icharlnf(cmoin)),
     &   ' has no elements for intersect.'
         call writloga('default',0,logmess,0,ierror)
         ierror = -1
         goto 9999
      endif

C
C     *****************************************************************
C     Figure out if we have an AMR code (this is based on the itetkid
C     attribute)
      call cmo_get_info('itetkid',cmoout,ipitetkido,ilen,icmotype
     &     ,ierror)
      if(ierror.ne.0) then
         iomrgrid = 0
         ierror = 0
      else
         iomrgrid = 1
      endif
C
C
C     *****************************************************************
C     Create the appropriate attribute for intersection.
      if(orig_nwds.eq.4) then
         strlen = icharlnf(cmsgin(4))
         attribname = cmsgin(4)(1:strlen)
         strlen = icharlnf(attribname)
      else
         strlen = icharlnf(cmoin)
         attribname = 'in_' // cmoin(1:strlen)
         strlen = icharlnf(attribname)
      endif
 
c     setup for writing progress messeges
c     note, this routine does not check pntlim for mpno
      num_src = nelementsin
      num_snk = nelementsout
      iwrite = 0
      ireport = 0
      totsrchd = 0
      totfind = 0
      totflag = 0
      istep = 1
      if ( max(num_snk,num_src) .gt. 500000 ) then
        xperc = 50.
        iperc = 2
      elseif ( max(num_snk,num_src) .gt. 10000 ) then
        xperc = 20.
        iperc = 5
      else
        xperc = 4.
        iperc = 25
      endif
      nwrite = nint(dble(num_src)/ xperc )
 
C
C
C     *****************************************************************
C     Find out if the output cmo has a K-d tree built already, if it
C     doesn't, build one for it via the kdtree command.
      ierror = 0
      call cmo_get_info('linkt',cmoout,iplinkto,ilen,icmotype,ierror)
      call cmo_get_info('sbox',cmoout,ipsboxo,ilen,icmotype,ierror2)
      if((ierror.ne.0).or.(ierror2.ne.0)) then
         ierror = 0
         strlen = icharlnf(cmoout)
         cmdmess = 'cmo/select/' // cmoout(1:strlen) // '; finish'
         call dotaskx3d(cmdmess,ierror)
         cmdmess = 'kdtree/build; finish'
         call dotaskx3d(cmdmess,ierror)
      endif
C
C     *****************************************************************
C     Get the K-d tree attributes (again, if necessary)
      ierror = 0
      call cmo_get_info('linkt',cmoout,iplinkto,ilen,icmotype,ierror)
      call cmo_get_info('sbox',cmoout,ipsboxo,ilen,icmotype,ierror)
      if (ierror.ne.0) then
         write(logmess,'(a)')
     &    'Error in intersect_elements: subsidiary addatt commands'
     &    // ' failed'
         call writloga('default',0,logmess,0,ierror)
         ierror = 1
         goto 9999
      endif
C
C     Check to see if the attribute exists already, if it doesn't,
C     create it.
      call cmo_get_info(attribname(1:icharlnf(attribname)),
     &                  cmoout,ipitetxsecto,
     &                  ilen,icmotype,ierror)
      if (ierror.ne.0) then
         cmdmess = 'cmo/addatt/' // cmoout(1:icharlnf(cmoout))
     &             // '/' // attribname(1:icharlnf(attribname))
     &             // '/VINT/scalar/nelements/linear/permanent/agx/0/'
     &             // '; finish'
         ierror = 0
         call dotaskx3d(cmdmess,ierror)
         if(ierror.ne.0) then
            write(logmess,'(a)')
     &      'Error in intersect_elements: subsidiary addatt commands'
     &      // ' failed'
            call writloga('default',0,logmess,0,ierror)
            ierror = 1
            goto 9999
         endif
         call cmo_get_info(attribname(1:icharlnf(attribname)),
     &                     cmoout,ipitetxsecto,
     &                     ilen,icmotype,ierror)
      else
         if (init_attrib.eq.0) then
           write(logmess,'(a,a)')
     &      'intersect_elements using previously defined attribute: '
     &      // attribname(1:icharlnf(attribname)) 
            call writloga('default',0,logmess,0,ierror)
         else
           write(logmess,'(a,a)')
     &  'intersect_elements initializing previously defined attribute: '
     &      // attribname(1:icharlnf(attribname)) 
            call writloga('default',0,logmess,0,ierror)
            cmdmess = 'cmo/setatt/' // cmoout(1:icharlnf(cmoout))
     &             // '/' // attribname(1:icharlnf(attribname))
     &             // '/1,0,0/0/  ; finish'
            ierror = 0
            call dotaskx3d(cmdmess,ierror)
            if(ierror.ne.0) then
               write(logmess,'(a)')
     &         'Error in intersect_elements: subsidiary setatt command'
     &         // ' failed'
               call writloga('default',0,logmess,0,ierror)
               ierror = 1
               goto 9999
            endif
         endif
      endif
C
C     *****************************************************************
C     This is the QUICK AND DIRTY METHOD, NEEDS OPTIMIZATION
C     Now that we have the tree set up, run through the elements of
C     cmoin, and then have the intersect_elements subroutine tell us
C     whether or not the elements intersect, and how many times they do
C
C     Allocate memory for the kdtree search results
      call mmgetblk('itfound',isubname,ipitfound,nelementsout,1,iflag)
      ifound = 0
      ifoundstep = 0

C     loop through each element of cmoin
      do i=1,nelementsin
C
C        Run through the nodes in the current element and create a
C        bounding box that specifies the search area for the
C        k-D tree subroutine.
C
         do j = 1,nelmnen(itettyp(i))
            k = itet(itetoff(i)+j)
            xnodes1(j) = xic(k)
            ynodes1(j) = yic(k)
            znodes1(j) = zic(k)
         enddo
C
C        At this point, we have the bounding box, as well as the
C        element filled in; the next step is to find the elements that
C        are "close" to the element in question using the k-D tree.
         call kDtreeselect(itettyp(i),xnodes1,ynodes1,znodes1,
     &        linkto,sboxo,numfound,itfound,iflag)
 
         ifound = 0
         do l = 1,numfound
           if(iomrgrid.eq.1) then
              if(itetkido(itfound(l)).gt.0.0) then
                 goto 100
              endif
           endif
           do m = 1,nelmnen(itettypo(itfound(l)))
              n = iteto(itetoffo(itfound(l))+m)
              xnodes2(m) = xico(n)
              ynodes2(m) = yico(n)
              znodes2(m) = zico(n)
           enddo

C          Detirmine if the two elements intersect
           call xsectelements(itettyp(i),xnodes1,ynodes1,znodes1,
     &           itettypo(itfound(l)),xnodes2,ynodes2,znodes2,
     &           iflag)

C          increment attribute each time intersection is found
C          check for negative values in the added attribute
C          this can happen if an attribute is used for something other
C          than intersect and is not reset before being used again here
           if (iflag.ge.0) then
              if (itetxsecto(itfound(l)).eq. zero) ifound = ifound + 1
              itetxsecto(itfound(l)) = itetxsecto(itfound(l)) + 1
              if (itetxsecto(itfound(l)).lt.0 .and. ineg.eq.0) then
                write(logmess,'(a,a,a)')
     &         'Warning: ',attribname(1:icharlnf(attribname)),
     &         ' has negative value(s) assigned outside this routine.' 
                call writloga('default',0,logmess,0,ierror)
                ineg=1
              endif 
           endif
 100       continue
 
C        End loop through search elements
         enddo
         totsrchd = totsrchd + numfound
         totfind = totfind + ifound
         ifoundstep = ifoundstep + ifound
 
C        Report progress of element search
          num_snk = nelementsin
          ipt = i
          iisrc = l
          if(ipt.eq.1) then
            write(logmess,"(a)")
     *'         Element    Elems Searched  Elements Found  Percent Done'
           call writloga('default',1,logmess,0,ierrw)
          endif
          if((iwrite.eq.nwrite).and.(ipt.ne.num_snk)) then
            iwrite = 0
            write(logmess,"(i15,i17,i15,i9,a2)")
     *      ipt,totsrchd,ifoundstep,istep*iperc,' %'
            call writloga('default',0,logmess,0,ierrw)
            istep = istep + 1
            ifoundstep = 0
          elseif(ipt .eq. num_snk) then
            write(logmess,"(i15,i17,i15,a)")
     *      ipt,totsrchd,totfind,'    Total Done'
            call writloga('default',0,logmess,1,ierrw)
          endif
 
          iwrite = iwrite+1
 
c     End i Loop through all sink elements
      enddo
C
C     *****************************************************************
C     Deallocate Memory
C
C     *****************************************************************
C     Remove kdtree data structure from the CMO
C
         ierror = 0
         strlen = icharlnf(cmoout)
         cmdmess = 'cmo/select/' // cmoout(1:strlen) // '; finish'
         call dotaskx3d(cmdmess,ierror)
         cmdmess = 'kdtree/release; finish'
         call dotaskx3d(cmdmess,ierror)
C
C     *****************************************************************
C
 9995 call mmrelprt(isubname,ierror)
C
C     We're done...
 9999 continue
      if (ierror .eq. -1) then
        write(logmess,'(a,a)')
     &  'intersect_elements exiting with no search.'
         call writloga('default',0,logmess,1,ierror)
      else if (ierror .ne. 0) then
        write(logmess,'(a,a,i5)')
     &  'intersect_elements ',
     &  ' exiting with error: ',ierror
         call writloga('default',0,logmess,1,ierror)
      endif

      return
      end
 
 
