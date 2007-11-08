      subroutine mode_lg(imsgin,xmsgin,cmsgin,msgtype,nwds,ierr)
C
C
C #####################################################################
C
C     PURPOSE -
C
C        This routine sets up several optimization options
C        Currently implemented are:
C        (1) discrete optimization
C          mode/discrete/surface_cmo/tolldamage
C        refine, smooth, merge will require that any
C        operations that involves nodes on the
C        specified surface will result in a mesh
C        whose surface nodes are also members of the
C        surface_cmo
C        A mesh object attribute associated with the
C        3d mesh named 'discrete_optimize' will be
C        created and it's value will be the name
C        of the surface mesh object
C        (2) error_adaption
C          mode/adaption_field/field_name
C        optimization operations will be based on reducing
C        error.  A mesh object attribute associated with the
C        3d mesh named 'adaption_field' will be
C        created and it's value will be the name
C        of the field.
C
C     INPUT ARGUMENTS -
C
C        imsgin()  - Integer array of command input tokens
C        xmsgin()  - Real array of command input tokens
C        cmsgin()  - Character array of command input tokens
C        msgtype() - Integer array of command input token types
C        nwds      - Number of command input tokens
C
C     OUTPUT ARGUMENTS -
C
C        ierr - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C     CHANGE HISTORY -
C
C $Log:   /pvcs.config/t3d/src/mode_lg.f_a  $
CPVCS    
CPVCS       Rev 1.6   24 Apr 2003 17:11:20   dcg
CPVCS    add option to turn off recon (set ivoronoi to 5)
CPVCS    
CPVCS       Rev 1.5   14 Mar 2002 10:50:26   dcg
CPVCS    remove duplicate declaration
CPVCS    
CPVCS       Rev 1.4   25 Feb 2002 14:11:12   dcg
CPVCS    use massage.h include file
CPVCS    
CPVCS       Rev 1.3   31 Jan 2002 16:45:34   dcg
CPVCS    add recon mode option
CPVCS    
CPVCS       Rev 1.2   31 Jan 2002 13:12:38   dcg
CPVCS    add changes for adaption_field option
CPVCS    
CPVCS       Rev 1.1   07 Jan 2002 13:39:42   dcg
CPVCS    set toldamage
CPVCS    
CPVCS       Rev 1.0   20 Dec 2001 16:22:38   dcg
CPVCS    Initial revision.
C
C ######################################################################
C
      implicit none
      include 'consts.h'
      include 'massage.h'
      character*132 logmess,cbuff
      integer nwds, imsgin(nwds), msgtype(nwds)
      real*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
      integer ierr,imsgout(15),msgout(15),nwdsout,ilen,icmotype,ier,i,
     *   nelementss
      real*8 xmsgout(15)
      character*32 cmo,cmsgout(15)
      pointer (ipxics, xics)
      pointer (ipyics, yics)
      pointer (ipzics, zics)
      real*8 xics(*),yics(*),zics(*),field(*)
      pointer (ipitets,itets)
      integer itets(*)
      pointer (iplinkt, linkt)
      pointer (ipsbox, sbox),(ipfield,field)
      integer linkt(*)
      real*8 sbox(2,3,*),alargenumber
      parameter (alargenumber=1.0d+99)
c
C ######################################################################
C
c
c  check for discrete mode
c
      if(cmsgin(2).eq.'discrete') then
         call cmo_get_name(cmo,ierr)
         if(cmsgin(3).eq.'off') then
c
c  turn the option off
c
            isdiscrete=.false.
 
         else
c
c  save the surface mesh object name
c
            surfcmo_name=cmsgin(3)
            call cmo_exist(surfcmo_name,ierr)
            if(ierr.ne.0) then
               write(logmess,'(a,a)') 'Mesh object not found',cmsgin(3)
               call writloga('default',0,logmess,0,ierr)
               go to 9999
            endif
            isdiscrete=.true.
c
c  see if damage specfied - if so save it
c
            toldamage_discrete=zero
            if(nwds.ge.4) then
               toldamage_discrete=xmsgin(4)
            endif
 
c
C     ADD ATTRIBUTES FOR k-D TREE OF THE surface if needed
C
            call cmo_get_info('linkt',surfcmo_name,iplinkt,
     *      ilen,icmotype,ier)
            if(ier.ne.0) then
               cbuff='cmo/addatt/' //
     &         surfcmo_name //'/' //
     &         'v2' //
     &         '/INT' //
     &         '/scalar/scalar/constant/permanent//2.0' //
     &         ' ; finish'
               call dotaskx3d(cbuff,ierr)
               cbuff='cmo/addatt/' //
     &         surfcmo_name //'/' //
     &         'linkt' //
     &         '/VINT' //
     &         '/v2/nelements//permanent/x/0.0' //
     &         ' ; finish'
               call dotaskx3d(cbuff,ierr)
               call cmo_get_info('linkt',surfcmo_name,iplinkt,
     *         ilen,icmotype,ier)
            endif
C
            call cmo_get_info('sbox',surfcmo_name,ipsbox,
     *         ilen,icmotype,ier)
            if(ier.ne.0) then
               cbuff='cmo/addatt/' //
     &         surfcmo_name //'/' //
     &         'v12' //
     &         '/INT' //
     &         '/scalar/scalar/constant/permanent//12.0' //
     &         ' ; finish'
               call dotaskx3d(cbuff,ierr)
               cbuff='cmo/addatt/' //
     &         surfcmo_name //'/' //
     &         'sbox' //
     &         '/VDOUBLE' //
     &         '/v12/nelements/linear/permanent/x/0.0' //
     &         ' ; finish'
               call dotaskx3d(cbuff,ierr)
c
c  get info needed and make the kdtree
c
               call cmo_get_info('sbox',surfcmo_name,ipsbox,
     *         ilen,icmotype,ier)
               call cmo_get_intinfo('nelements',surfcmo_name,nelementss,
     *         ilen,icmotype,ier)
               call cmo_get_info('xic',surfcmo_name,ipxics,
     *         ilen,icmotype,ier)
               call cmo_get_info('yic',surfcmo_name,ipyics,
     *         ilen,icmotype,ier)
               call cmo_get_info('zic',surfcmo_name,ipzics,
     *         ilen,icmotype,ier)
               call cmo_get_info('itet',surfcmo_name,ipitets,
     *         ilen,icmotype,ier)
               call kdtree(xics,yics,zics,itets,nelementss,
     *         linkt,sbox,ier)
            endif
         endif
c
c  check for adaption_field mode
c  mode/adaption_field/field_name/field_scale_factor/
c    refine_length_4d/merge_length_4d/toldamage_4d/pc_refine/
c    error_refine
c  field_scale_factor will be used to divide the field value
c     when calculating the 4d distance d**2=dx**2+(fv/scale_factor)**2
c  refine_length_4d, merge_length_4d are 4d lengths
c  toldamage_4d is a 4d damage number (i.e. another 4d length)
c  pc_refine is the percent of edges to refine
c  error_refine says refine all edges with errors > error_refine
c  only one of pc_refine and error_refine should be non zero
c
      elseif(cmsgin(2).eq.'adaption_field') then
         if(cmsgin(3).eq.'off') then
            adaption_field_name=' '
            isafield=.false.
         else
            isafield=.true.
            adaption_field_name=cmsgin(3)
            call cmo_get_name(cmo,ierr)
            call cmo_get_info(adaption_field_name,cmo,ipfield,ilen,
     *         icmotype,ier)
            if(ier.ne.0) then
               write(logmess,'(3a)') adaption_field_name,
     *           ' does not exist in ',cmo
               call writloga('default',0,logmess,0,ierr)
               go to 9999
            endif
            field_scale_factor=one
            refine_length_4d=alargenumber
            merge_length_4d=alargenumber
            toldamage_4d=-one
            pc_refine=zero
            error_refine=zero
            if(nwds.ge.4) then
               field_scale_factor=xmsgin(4)
            endif
            if(nwds.ge.5) then
               refine_length_4d=xmsgin(5)
            endif
            if(nwds.ge.6) then
               merge_length_4d=xmsgin(6)
            endif
            if(nwds.ge.7) then
               toldamage_4d=xmsgin(7)
            endif
            if(nwds.ge.8) then
               pc_refine=xmsgin(8)
            endif
            if(nwds.ge.9) then
               error_refine=xmsgin(9)
            endif
            if(pc_refine.eq.zero.and.error_refine.eq.0) then
               pc_refine=10d0
            elseif(pc_refine.ne.zero.and.error_refine.ne.0)then
               error_refine=zero
            endif
c
c  also set recon flag to use geometric reconnection
c
            cmsgout(1)='cmo'
            cmsgout(2)='setatt'
            cmsgout(3)=cmo
            cmsgout(4)='ivoronoi'
            imsgout(5)=-2
            cmsgout(5)='-2'
            nwdsout=4
            do i=1,nwdsout
              imsgout(i)=0
              xmsgout(i)=0.0
              msgout(i)=3
            enddo
            nwdsout=5
            msgout(5)=1
            call cmo_setatt(imsgout,xmsgout,cmsgout,msgout,nwdsout,
     *                      ierr)
         endif
c
c  check for recon mode
c
      elseif (cmsgin(2)(1:5).eq.'recon') then
         call cmo_get_name(cmo,ierr)
         if(cmsgin(3)(1:3).eq.'del') then
            cmsgout(1)='cmo'
            cmsgout(2)='setatt'
            cmsgout(3)=cmo
            cmsgout(4)='ivoronoi'
            imsgout(5)=1
            cmsgout(5)='1'
         elseif (cmsgin(3)(1:4).eq.'geom') then
            cmsgout(1)='cmo'
            cmsgout(2)='setatt'
            cmsgout(3)=cmo
            cmsgout(4)='ivoronoi'
            imsgout(5)=-2
            cmsgout(5)='-2'
         elseif (cmsgin(3)(1:4).eq.'adap') then
            cmsgout(1)='cmo'
            cmsgout(2)='setatt'
            cmsgout(3)=cmo
            cmsgout(4)='ivoronoi'
            imsgout(5)=2
            cmsgout(5)='2'
         elseif (cmsgin(3)(1:4).eq.'none') then
            cmsgout(1)='cmo'
            cmsgout(2)='setatt'
            cmsgout(3)=cmo
            cmsgout(4)='ivoronoi'
            imsgout(5)=5
            cmsgout(5)='5'
         else
            write(logmess,'(3a)') 'Mode option not implemented ',
     *        cmsgin(2),cmsgin(3)
            call writloga('default',0,logmess,0,ierr)
            go to 9999
         endif
         nwdsout=4
         do i=1,nwdsout
           imsgout(i)=0
           xmsgout(i)=0.0
           msgout(i)=3
         enddo
         nwdsout=5
         msgout(5)=1
         call cmo_setatt(imsgout,xmsgout,cmsgout,msgout,nwdsout,ierr)
C
c  if get here illegal mode option
c
      else
         write(logmess,'(a,a)') 'Mode option not implemented ',cmsgin(2)
         call writloga('default',0,logmess,0,ierr)
         go to 9999
      endif
c
 9999 continue
      return
      end
