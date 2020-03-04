      subroutine dump_fehm_geom(ifile,ifileini)
C
C
C #####################################################################
C
C     PURPOSE -
C
C        THIS ROUTINE WRITES A DUMP FILE FOR FEHMN.
C
C     INPUT ARGUMENTS -
C
C        Character string for naming output file
C        ifile - root output file name
C        ifileini - ini file name skipped if empty
C                   ?? Not sure this is ever used ?? TAM
C
C     OUTPUT ARGUMENTS -
C
C        None
C
C     CHANGE HISTORY -
C
C        $Log: dump_fehm_geom.f,v $
C        Revision 2.00  2007/11/05 19:45:52  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.8   17 Jul 2007 08:41:54   gable
CPVCS    Added options to dump/elem_adj_node to create a node attribute, ean_num
CPVCS    that contains the number of elements adjacent to each node. There are now
CPVCS    options to just write a file (-def- or delatt), write a file and create
CPVCS    attribute (keepatt) and create attribute (attonly).
CPVCS    
CPVCS       Rev 1.7   23 Mar 2005 09:43:46   dcg
CPVCS    fix declarations for linux
CPVCS
CPVCS       Rev 1.6   17 Jun 2004 12:41:58   gable
CPVCS    Added two new output commands,
CPVCS    dump / elem_adj_node / file_name / mo_name / keepatt
CPVCS    dump / elem_adj_elem / file_name / mo_name / keepatt
CPVCS    The two new subroutines,
CPVCS    write_element_element, write_element_node_neigh
CPVCS    have been concatenated onto dump_fehm_geom.f
CPVCS
CPVCS       Rev 1.5   03 Oct 2000 09:54:40   dcg
CPVCS    remove unused references to ialias
CPVCS
CPVCS       Rev 1.4   Mon Apr 14 16:43:40 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.3   Wed Sep 18 11:49:08 1996   gable
CPVCS    Fixed error in header to connectivity list. Header for hex grid
CPVCS    was telling fehm there were 4 nodes. ns for hex grid changed to 8.
CPVCS
CPVCS       Rev 1.2   Wed Jun 05 09:44:54 1996   gable
CPVCS    Corrected end of elem list to end with a blank line
CPVCS    and a line with the string stop.
CPVCS
CPVCS       Rev 1.1   Mon May 13 16:20:32 1996   gable
CPVCS    Change output format. Add character string stop
CPVCS    to last line of the file.
CPVCS
CPVCS       Rev 1.0   Wed May 08 12:36:10 1996   gable
CPVCS    Initial revision.
C
C ######################################################################
C
      implicit real*8 (a-h,o-z)
C
      character*132 logmess
C
      character ifile*(*), ifileini*(*)
      integer nsd, nen, nef
C
      pointer (ipisetwd, isetwd)
      pointer (ipimt1, imt1)
      pointer (ipitp1, itp1)
      pointer (ipicr1, icr1)
      pointer (ipisn1, isn1)
      pointer (ipign1, ign1)
      integer isetwd(1000000)
      integer imt1(1000000), itp1(1000000),
     *        icr1(1000000), isn1(1000000), ign1(1000000)
C
C
C     *****************************************************************
C
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      REAL*8 xic(1000000), yic(1000000), zic(1000000)
C
C
C     *****************************************************************
C
      pointer (ipitetclr, itetclr)
      pointer (ipitettyp, itettyp)
      pointer (ipitetoff, itetoff)
      pointer (ipitet, itet1)
      pointer (ipjtet, jtet1)
      integer itetclr(1000000), itettyp(1000000), itetoff(1000000)
      integer itet1(4*1000000)
      integer jtet1(4*1000000)
C
C *****************************************************************
C
      pointer (ipxfield, xfield)
      real*8 xfield(10000000)
C
      character*32 wddl
      character*132 iline
C
      parameter (nentri=3, nfacetri=3)
      parameter (nentet=4, nfacetet=4)
      parameter (nenprism=6, nfaceprism=5)
      parameter (nenhex=8, nfacehex=6)
C
      dimension ihexface0(nfacehex), ihexface1(4,nfacehex)
C     top,bottom,front,right,back,left
      data ihexface0 / 4, 4, 4, 4, 4, 4 /
      data ihexface1 / 1, 2, 3, 4,
     *                 5, 8, 7, 6,
     *                 1, 5, 6, 2,
     *                 2, 6, 7, 3,
     *                 3, 7, 8, 4,
     *                 1, 4, 8, 5 /
      dimension iprismface0(nfaceprism), iprismface1(4,nfaceprism)
C     top,bottom,right,back,left
      data iprismface0 / 3, 3, 4, 4, 4 /
      data iprismface1 / 1, 2, 3, 0,
     *                   4, 6, 5, 0,
     *                   1, 4, 5, 2,
     *                   2, 5, 6, 3,
     *                   1, 3, 6, 4 /
C
      integer intpairhex(2,12)
      data intpairhex / 1,2, 2,3, 3,4, 4,1, 5,6, 6,7, 7,8,
     *                  8,5, 1,5, 2,6, 3,7, 4,8 /
 
      dimension itetface0(nfacetet), itetface1(4,nfacetet)
C     top,back,left,right
      data itetface0 / 3, 3, 3, 3 /
      data itetface1 / 2, 3, 4, 0,
     *                 1, 4, 3, 0,
     *                 1, 2, 4, 0,
     *                 1, 3, 2, 0 /
      dimension itriface0(nfacetri), itriface1(3,nfacetri)
C     top,back,left,right
      data itriface0 / 2, 2, 2 /
      data itriface1 / 2, 3, 1,
     *                 3, 1, 2,
     *                 1, 2, 3 /
      integer intpairtet(2,6)
      data intpairtet / 1,2, 1,3, 1,4, 2,3, 2,4, 3,4 /
C
C
      character*32 isubname, cmo, ifilename
C
      data ivoronoi2d / 1 /
      data ivoronoi3d / 0 /
C
C ######################################################################
C
C
      crosx(a,b,c,d,e,f)=b*f-c*e
      crosy(a,b,c,d,e,f)=c*d-a*f
      crosz(a,b,c,d,e,f)=a*e-b*d
C
C ######################################################################
C
c     insure that the icr array has the outside nodes correctly defined
c
c
      call cmo_get_name(cmo,ierror)
C
      call cmo_get_info('nnodes',cmo,
     *                  nnodes,length,icmotype,ierror)
      call cmo_get_info('nelements',cmo,
     *                  nelements,length,icmotype,ierror)
      call cmo_get_info('mbndry',cmo,
     *                  mbndry,length,icmotype,ierror)
      call cmo_get_info('ndimensions_topo',cmo,
     *                  nsdtopo,length,icmotype,ierror)
      call cmo_get_info('ndimensions_geom',cmo,
     *                  nsdgeom,length,icmotype,ierror)
      call cmo_get_info('nodes_per_element',cmo,
     *                  nen,length,icmotype,ierror)
      call cmo_get_info('faces_per_element',cmo,
     *                  nef,length,icmotype,ierror)
      call cmo_get_info('isetwd',cmo,
     *                  ipisetwd,ilen,ityp,ierr)
      call cmo_get_info('imt1',cmo,ipimt1,ilen,ityp,ierr)
      call cmo_get_info('itp1',cmo,ipitp1,ilen,ityp,ierr)
      call cmo_get_info('icr1',cmo,ipicr1,ilen,ityp,ierr)
      call cmo_get_info('isn1',cmo,ipisn1,ilen,ityp,ierr)
      call cmo_get_info('ign1',cmo,ipign1,ilen,ityp,ierr)
      call cmo_get_info('xic',cmo,ipxic,ilen,ityp,ierr)
      call cmo_get_info('yic',cmo,ipyic,ilen,ityp,ierr)
      call cmo_get_info('zic',cmo,ipzic,ilen,ityp,ierr)
      call cmo_get_info('itetclr',cmo,
     *                  ipitetclr,ilen,ityp,ierr)
      call cmo_get_info('itettyp',cmo,
     *                  ipitettyp,ilen,ityp,ierr)
      call cmo_get_info('itetoff',cmo,
     *                  ipitetoff,ilen,ityp,ierr)
      call cmo_get_info('itet',cmo,ipitet,ilen,ityp,ierr)
      call cmo_get_info('jtet',cmo,ipjtet,ilen,ityp,ierr)
C
      nsd=nsgtopo
C
      ihcycle=0
      time=0.0
      dthydro=0.0
C
      isubname='dump_geom_list'

C
      if(ifileini(1:1).eq.' '.or.icharlnf(ifileini).le.0) then

C         this is usually skipped
C         print*,'Skipping ifileini ',ifileini

      else
         ifilename=ifileini(1:icharlnf(ifileini))
         iread=-1
         call hassign(iread,ifilename,ierror)
         if (iread.lt.0 .or. ierror.lt.0) then
           call x3d_error(isubname,'hassign bad file unit')
           goto 9999
         endif

         read(iread,*)
         read(iread,*)
         read(iread,*) days
         read(iread,'(a4)') wddl(1:4)
         read(iread,'(a4)') wddl(5:8)
         read(iread,'(a4)') wddl(9:12)
         read(iread,'(a4)') wddl(13:16)
         read(iread,'(a4)') wddl(17:20)
         close(iread)
         ncount=nnodes
         if(wddl(13:16).eq.'dpdp') then
            write(logmess,'(a,a)') 'Cannot write this type of ',
     *                             'FEHMN.ini file'
            call writloga('default',1,logmess,1,ierrwrt)
         elseif(wddl(17:20).eq.'dual') then
            write(logmess,'(a,a)') 'Cannot write this type of ',
     *                             'FEHMN.ini file'
            call writloga('default',1,logmess,1,ierrwrt)
         else
C
            ifilename=ifile(1:icharlnf(ifile)) // '.ini'
            iunit=-1
            call hassign(iunit,ifilename,ierror)
            if (iunit.lt.0 .or. ierror.lt.0) then
              call x3d_error(isubname,'hassign bad file unit')
              goto 9999
            endif
C
            write(logmess,'(a,a,a,a)') 'Write FEHMN.ini file: ',
     *                                 ifilename(1:icharlnf(ifilename)),
     *                                 ' using header from ',
     *                                 ifileini(1:icharlnf(ifileini))
            call writloga('default',1,logmess,1,ierrwrt)
C
            ifilename=ifileini(1:icharlnf(ifileini))
            iread=-1
            call hassign(iread,ifilename,ierror)
            if (iread.lt.0 .or. ierror.lt.0) then
              call x3d_error(isubname,'hassign bad file unit')
              goto 9999
            endif

C
            read(iread,'(a132)') iline
               write(iunit,'(a)') iline(1:icharlnb(iline))
            read(iread,'(a132)') iline
               write(iunit,'(a)') iline(1:icharlnb(iline))
            read(iread,*) days
               write(iunit,*) 0.0d+00
C
            read(iread,'(a4)') wddl(1:4)
               write(iunit,'(a)') wddl(1:4)
            read(iread,'(a4)') wddl(5:8)
               write(iunit,'(a)') wddl(5:8)
            read(iread,'(a4)') wddl(9:12)
               write(iunit,'(a)') wddl(9:12)
            read(iread,'(a4)') wddl(13:16)
               write(iunit,'(a)') wddl(13:16)
            read(iread,'(a4)') wddl(17:20)
               write(iunit,'(a)') wddl(17:20)
C
            if(wddl(1:4).eq.'ngas') then
               call cmo_get_info('Temperature',cmo,
     *                           ipxfield,icmolen,icmotype,icscode)
               if(icscode.ne.0) call x3d_error(cmo,'cmo_get_info')
               write(iunit,'(6(1pe20.12))') (xfield(mi),mi=1,ncount)
               call cmo_get_info('Saturation',cmo,
     *                           ipxfield,icmolen,icmotype,icscode)
               if(icscode.ne.0) call x3d_error(cmo,'cmo_get_info')
               write(iunit,'(6(1pe20.12))') (xfield(mi),mi=1,ncount)
               call cmo_get_info('Vapor',cmo,
     *                           ipxfield,icmolen,icmotype,icscode)
               if(icscode.ne.0) call x3d_error(cmo,'cmo_get_info')
               write(iunit,'(6(1pe20.12))') (xfield(mi),mi=1,ncount)
               call cmo_get_info('Capillary',cmo,
     *                           ipxfield,icmolen,icmotype,icscode)
               if(icscode.ne.0) call x3d_error(cmo,'cmo_get_info')
               write(iunit,'(6(1pe20.12))') (xfield(mi),mi=1,ncount)
            elseif(wddl(1:4).eq.'h20 ') then
               call cmo_get_info('Temperature',cmo,
     *                           ipxfield,icmolen,icmotype,icscode)
               if(icscode.ne.0) call x3d_error(cmo,'cmo_get_info')
               write(iunit,'(6(1pe20.12))') (xfield(mi),mi=1,ncount)
               call cmo_get_info('Saturation',cmo,
     *                           ipxfield,icmolen,icmotype,icscode)
               if(icscode.ne.0) call x3d_error(cmo,'cmo_get_info')
               write(iunit,'(6(1pe20.12))') (xfield(mi),mi=1,ncount)
               call cmo_get_info('Vapor',cmo,
     *                           ipxfield,icmolen,icmotype,icscode)
               if(icscode.ne.0) call x3d_error(cmo,'cmo_get_info')
               write(iunit,'(6(1pe20.12))') (xfield(mi),mi=1,ncount)
            elseif(wddl(1:4).eq.'air ') then
               call cmo_get_info('Saturation',cmo,
     *                           ipxfield,icmolen,icmotype,icscode)
               if(icscode.ne.0) call x3d_error(cmo,'cmo_get_info')
               write(iunit,'(6(1pe20.12))') (xfield(mi),mi=1,ncount)
               call cmo_get_info('Vapor',cmo,
     *                           ipxfield,icmolen,icmotype,icscode)
               if(icscode.ne.0) call x3d_error(cmo,'cmo_get_info')
               write(iunit,'(6(1pe20.12))') (xfield(mi),mi=1,ncount)
            endif
            close(iread)
            close(iunit)
         endif
      endif
C
C     Write .fehm coord and elem ascii file

      ifilename=ifile(1:icharlnf(ifile)) // '.fehmn'
      iunit=-1
      call hassign(iunit,ifilename,ierror)
      if (iunit.lt.0 .or. ierror.lt.0) then
        call x3d_error(isubname,'hassign bad file unit')
        goto 9999
      endif

C
C   
      write(iunit,'(a4)') 'coor'
      write(iunit,'(i8)') nnodes
      do i=1,nnodes
         write(iunit,9040) i,xic(i),yic(i),zic(i)
 9040    format(i10,3(1pe20.12))
      enddo
      write(iunit,'(i10)') 0
      write(iunit,'(a4)') 'elem'
      if(nsdtopo.eq.2.and.nen.eq.3.and.nef.eq.3) then
         ns=3
         write(iunit,'(i8,1x,i12)') ns,nelements
         do it=1,nelements
            index=nen*(it-1)
            i1=itet1(index+1)
            i2=itet1(index+2)
            i3=itet1(index+3)
            write(iunit,9050) it,i1,i2,i3
 9050       format(10i12)
         enddo
      elseif(nsdtopo.eq.3.and.nen.eq.4.and.nef.eq.4) then
         ns=4
         write(iunit,'(i8,1x,i12)') ns,nelements
         do it=1,nelements
            index=nen*(it-1)
            i1=itet1(index+1)
            i2=itet1(index+2)
            i3=itet1(index+3)
            i4=itet1(index+4)
            write(iunit,9050) it,i1,i2,i4,i3
         enddo
      elseif(nen.eq.8.and.nef.eq.6) then
         ns=8
         write(iunit,'(i8,1x,i12)') ns,nelements
         do it=1,nelements
            index=nen*(it-1)
            i1=itet1(1+index)
            i2=itet1(2+index)
            i3=itet1(3+index)
            i4=itet1(4+index)
            i5=itet1(5+index)
            i6=itet1(6+index)
            i7=itet1(7+index)
            i8=itet1(8+index)
            write(iunit,9050) it,i5,i6,i7,i8,i1,i2,i3,i4
         enddo
      endif
      write(iunit,9074)
      logmess = 'stop'
      write(iunit,9075)logmess
 9074 format(' ')
 9075 format(a4)
      close(iunit)
C
 9999 continue

      return
      end

      subroutine write_element_element
     1          (imsgin,xmsgin,cmsgin,msgtype,nwds,ierror)
C
C #####################################################################
C
C     PURPOSE -
C
C     This routine takes as input the connectivity of a MO, and outputs a file
C     with the adjacent (across element faces) elements.
C
C     SYNTAX -
C
C     dump / elem_adj_elem / file_name / mo_name
C
C     File format:
C     elem_number number_of_adjacent_elem   e1 e2 ... en
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
C        ierror - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C     CHANGE HISTORY -
C
C   $Log: dump_fehm_geom.f,v $
C   Revision 2.00  2007/11/05 19:45:52  spchu
C   Import to CVS
C
C
C ######################################################################
C
      implicit none
      integer nwds, imsgin(nwds), msgtype(nwds), ierror
      real*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
C
      character*132 logmess
C
C     ipitp1 is an pointer pointing to itp1 array
 
      pointer (ipitp1, itp1(*))
      pointer (ipisn1, isn1(*))
      pointer (ipxic, xic(*))
      pointer (ipyic, yic(*))
      pointer (ipzic, zic(*))
      integer itp1,isn1
      real*8 xic,yic,zic
C
      pointer (ipitet, itet1)
      pointer (ipjtet, jtet1)
      integer itet1(*), jtet1(*)
C
      pointer (ipitettyp, itettyp)
      pointer (ipitetoff, itetoff)
      pointer (ipjtetoff, jtetoff)
      integer itettyp(*),itetoff(*), jtetoff(*)
 
c  array ielts(*) stores the element neighbors info
      pointer(ipielts,ielts)
      integer ielts(*)
C
      integer length,icscode,itype,npoints,ntets,ilen,mbndry,
     *   nef,i,it,ityp,kkk,nn,jt
      integer lu_number, ibuffer, strlen
      integer icharlnf
C
      include "local_element.h"
C
      character*32 cmo,isubname
      character*32 file_name
 
      isubname='write_element_element'
C
C     Assign and open file
C
      file_name = cmsgin(3)
      lu_number = -1
      call hassign(lu_number, file_name, ibuffer)
      if (lu_number.lt.0 .or. ibuffer.lt.0) then
        call x3d_error(isubname,'hassign bad file unit')
        goto 9999
      endif

 
C     *****************************************************************
C     Ensure that the incoming MO name is a valid one, and if it is
C     the default, get the real name.
C
      strlen=icharlnf(cmsgin(4))
      cmo=cmsgin(4)(1:strlen)
      if((cmo.eq.'-cmo-').or.(cmo.eq.'-def-')) then
         call cmo_get_name(cmo,ierror)
      endif
      call cmo_exist(cmo,ierror)
      if(ierror .ne. 0)then
        write(logmess,'(a)') 'WARNING: write_element_element'
        call writloga('default',0,logmess,0,ierror)
        write(logmess,'(a)') 'WARNING: MO does not exist'
        call writloga('default',0,logmess,0,ierror)
        return
      endif
 
C
C     *****************************************************************
c
 
c  get mesh object information
c  Scalar Info (assigned to integers)
      call cmo_get_info('nnodes',cmo,npoints,ilen,itype,icscode)
      call cmo_get_info('nelements',cmo,ntets,ilen,itype,icscode)
      call cmo_get_info('mbndry',cmo,mbndry,ilen,itype,icscode)
      call cmo_get_info('faces_per_element',cmo,nef,ilen,itype,icscode)
 
c  Vector Info (address of Vector[1] is assigened to pointers)
      call cmo_get_info('itp1',cmo,ipitp1,ilen,ityp,icscode)
      call cmo_get_info('isn1',cmo,ipisn1,ilen,ityp,icscode)
      call cmo_get_info('xic',cmo,ipxic,ilen,ityp,icscode)
      call cmo_get_info('yic',cmo,ipyic,ilen,ityp,icscode)
      call cmo_get_info('zic',cmo,ipzic,ilen,ityp,icscode)
      call cmo_get_info('itettyp',cmo,
     *                  ipitettyp,ilen,ityp,icscode)
      call cmo_get_info('itetoff',cmo,
     *                  ipitetoff,ilen,ityp,icscode)
      call cmo_get_info('jtetoff',cmo,
     *                  ipjtetoff,ilen,ityp,icscode)
      call cmo_get_info('itet',cmo,ipitet,ilen,ityp,icscode)
      call cmo_get_info('jtet',cmo,ipjtet,ilen,ityp,icscode)
c
c  get memory for seed array and parent array and tets surrounding node
 
c here, the maxiumum number of element neighbors per element are set to be 100!!!
      length=100
      call mmgetblk('ielts',isubname,ipielts, length,1,icscode)
 
       do it=1,ntets
           kkk=0
           do i=1,nelmnef(itettyp(it))
            kkk=kkk+1
c check if element face is on an external boundry end
            if(jtet1(jtetoff(it)+i).eq.mbndry) then
               jt=-99
c              jf=0
 
c check if element face is on an internal boundry (2D_JT grid does not contain them?)
            elseif(jtet1(jtetoff(it)+i).gt.mbndry)then
               jt=1+(jtet1(jtetoff(it)+i)-mbndry-1)/nef
c               jf=jtet1(jtetoff(it)+i)-mbndry-nef*(jt-1)
 
C Volume element
            else
               jt=1+(jtet1(jtetoff(it)+i)-1)/nef
c              jf=jtet1(jtetoff(it)+i)-nef*(jt-1)
            endif
          ielts(kkk)=jt
 
          end do
 
        write(lu_number,*) it, nelmnef(itettyp(it)),(ielts(nn),nn=1,
     *   nelmnef(itettyp(it)))
 
      enddo
 
      close(lu_number)

 9999 continue

      call mmrelprt(isubname,icscode)
      return
      end


      subroutine write_element_node_neigh
     1          (imsgin,xmsgin,cmsgin,msgtype,nwds,ierror)
C
C #####################################################################
C
C     PURPOSE -
C
C     This routine takes as input the connectivity of a MO, and outputs
C     the element numbers of elements adjacent to each node.
C
C     SYNTAX -
C
C     dump / elem_adj_node / file_name / mo_name
C
C     File format:
C     node_number number_of_adjacent_elem   e1 e2 ... en
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
C        ierror - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C     CHANGE HISTORY -
C
C $Log: dump_fehm_geom.f,v $
C Revision 2.00  2007/11/05 19:45:52  spchu
C Import to CVS
C
C
C
C ######################################################################
C
      implicit none
      integer nwds, imsgin(nwds), msgtype(nwds), ierror
      real*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
C
      character*132 logmess
      character*256 cmdmess

C
C     ipitp1 is an pointer pointing to itp1 array
 
      pointer (ipitp1, itp1(*))
      pointer (ipisn1, isn1(*))
      pointer (ipxic, xic(*))
      pointer (ipyic, yic(*))
      pointer (ipzic, zic(*))
      integer itp1,isn1
      real*8 xic,yic,zic
C
      pointer (ipitet, itet1)
      pointer (ipjtet, jtet1)
      integer itet1(*), jtet1(*)
C
      pointer (ipitettyp, itettyp)
      pointer (ipitetoff, itetoff)
      pointer (ipjtetoff, jtetoff)
      integer itettyp(*),itetoff(*), jtetoff(*)
 
      pointer (ipiseedtet, iseedtet)
      integer iseedtet(*)
      pointer (ipiparent, iparent)
      integer iparent(*)
 
c ielts(i) contains the element neibhors of any node i=1
      pointer (ipielts, ielts)
      integer ielts(*)
C
      pointer (ipean_num, ean_num)
      integer ean_num(*)
 
C
      integer length,icscode,itype,npoints,ntets,ilen,mbndry,
     *   nef,i,nf,it,k,kk,
     *   ityp,nn,nelts,itest,if_keepatt, num_max, node_max
      integer lu_number, ibuffer, strlen, local_debug
      integer icharlnf
C
      include "local_element.h"
C
      character*32 cmo,isubname
      character*32 file_name
 
      local_debug = 0

      isubname='write_element_node_neigh'
C
C     *****************************************************************
C     Ensure that the incoming MO name is a valid one, and if it is
C     the default, get the real name.
C
      strlen=icharlnf(cmsgin(4))
      cmo=cmsgin(4)(1:strlen)
      if((cmo.eq.'-cmo-').or.(cmo.eq.'-def-')) then
         call cmo_get_name(cmo,ierror)
      endif
      call cmo_exist(cmo,ierror)
      if(ierror .ne. 0)then
        write(logmess,'(a)') 'WARNING: write_element_node_neigh'
        call writloga('default',0,logmess,0,ierror)
        write(logmess,'(a)') 'WARNING: MO does not exist'
        call writloga('default',0,logmess,0,ierror)
        write(logmess,'(a)') 'WARNING: No Action'
        call writloga('default',0,logmess,0,ierror)
        write(logmess,'(a)') 'WARNING: RETURN'
        call writloga('default',0,logmess,0,ierror)
        return
      endif
 
C     *****************************************************************
C     if argument keepatt exist, check if attribute already exists, if it
C     does, get the pointer, if it does not, allocate the array and get the
C     pointer.
C
      if_keepatt = 0
      if(nwds .gt. 4)then
        if((cmsgin(5)(1:icharlnf(cmsgin(5))) .eq. 'keepatt') .or.
     *     (cmsgin(5)(1:icharlnf(cmsgin(5))) .eq. 'attonly')) then
         if_keepatt = 1
         call cmo_get_info('ean_num',cmo,ipean_num,ilen,ityp,icscode)
         if(icscode .ne. 0)then
           cmdmess='cmo/addatt/ / ean_num /vint/scalar/nnodes;finish'
           call dotask(cmdmess,icscode)
           call cmo_get_info('ean_num',cmo,ipean_num,ilen,ityp,icscode)
         endif
        endif
        if(cmsgin(5)(1:icharlnf(cmsgin(5))) .eq. 'attonly') then
           if_keepatt = 2
        endif
        if(cmsgin(5)(1:icharlnf(cmsgin(5))) .eq. 'delatt') then
           if_keepatt = 0
        endif
      endif
C
C     *****************************************************************
C     Assign and open file
C
      if((if_keepatt .eq. 0) .or. (if_keepatt .eq. 1))then
      file_name = cmsgin(3)
      lu_number = -1
      call hassign(lu_number, file_name, ibuffer)
      if (lu_number.lt.0 .or. ibuffer.lt.0) then
        call x3d_error(isubname,'hassign bad file unit')
        goto 9999
      endif
      endif
 
C     *****************************************************************
c
      if(local_debug .ne. 0) print *, 'get mesh object information'
C
c  get mesh object information
c  Scalar Info (assigned to integers)
      call cmo_get_info('nnodes',cmo,npoints,ilen,itype,icscode)
      call cmo_get_info('nelements',cmo,ntets,ilen,itype,icscode)
      call cmo_get_info('mbndry',cmo,mbndry,ilen,itype,icscode)
      call cmo_get_info('faces_per_element',cmo,nef,ilen,itype,icscode)
 
c  Vector Info (address of Vector[1] is assigened to pointers)
      call cmo_get_info('itp1',cmo,ipitp1,ilen,ityp,icscode)
      call cmo_get_info('isn1',cmo,ipisn1,ilen,ityp,icscode)
      call cmo_get_info('xic',cmo,ipxic,ilen,ityp,icscode)
      call cmo_get_info('yic',cmo,ipyic,ilen,ityp,icscode)
      call cmo_get_info('zic',cmo,ipzic,ilen,ityp,icscode)
      call cmo_get_info('itettyp',cmo,
     *                  ipitettyp,ilen,ityp,icscode)
      call cmo_get_info('itetoff',cmo,
     *                  ipitetoff,ilen,ityp,icscode)
      call cmo_get_info('jtetoff',cmo,
     *                  ipjtetoff,ilen,ityp,icscode)
      call cmo_get_info('itet',cmo,ipitet,ilen,ityp,icscode)
      call cmo_get_info('jtet',cmo,ipjtet,ilen,ityp,icscode)
c
c  get memory for seed array and parent array and tets surrounding node
      if(local_debug .ne. 0) print *, 'allocate mem, iseedtet, iparent'
 
      length = npoints
      call mmgetblk('iseedtet',isubname,ipiseedtet,length,1,icscode)
      call mmgetblk('iparent',isubname,ipiparent,length,1,icscode)
C 
C here, the maxiumum number of element neighbors are set to be 100 in
C in the temporary work array ielts. However, inside get_elements_around_node
C the array size may be increased and the new point value is returned. The
C algorithms will handle an arbitrary number of elements around a node.
C
      if(local_debug .ne. 0) print *, 'allocate mem, ielts'
      length=100
      call mmgetblk('ielts',isubname,ipielts,length,1,icscode)
 
c  get parents
      call unpackpc(npoints,itp1,isn1,iparent)
      if (local_debug.gt.0) call mmverify()

c  fill iseedtet
      do i=1,ntets
         ityp = itettyp(i)
         do nn = 1, nelmnen(ityp)
            k=itet1(itetoff(i)+nn)
            iseedtet(k)=i
            iseedtet(iparent(k))=i
         enddo
      enddo
 
      if (local_debug.gt.0) call mmverify()
c
c  find the element neighbors for each nodes
      num_max = 0
      node_max = 0
      do i=1,npoints
         it=iseedtet(i)
         ityp = itettyp(it)
c nn is the nodes within the same element of iseedtet(i) with i:
c nelts is the number of elements surrouding node nn
c ipielts is the pointer (integer address) pointing to the arrays of elements
         do nn = 1, nelmnen(ityp)
             itest=itet1(itetoff(it)+nn)
             if(itest .eq. i) then 
               call get_elements_around_node(
     1            it,nn,nelts,ipielts,itetoff,jtetoff,itet1,jtet1,
     2            itettyp,iparent,nef,mbndry)
                  if(nelts .gt . num_max)then
                    num_max  = nelts
                    node_max = i
                  endif
             endif
         end do
         if(if_keepatt .ne. 2)then
         write(lu_number,*) i, nelts, (ielts(kk),kk=1,nelts)
         endif
         if(if_keepatt .gt. 0)then
            ean_num(i) = nelts
         endif
      enddo

      if (local_debug.gt.0) call mmverify()
 
      if(if_keepatt .ne. 2) close(lu_number)

        write(logmess,'(a,i10)')
     1   'Maximum number of elements around a node num_max =', num_max
        call writloga('default',0,logmess,0,ierror)
        write(logmess,'(a,i10)') 
     1   'Node number                             node_max =', node_max
        call writloga('default',0,logmess,0,ierror)

 9999 continue 
      call mmrelprt(isubname,icscode)
      return
      end
C end file dump_fehm_geom.f
