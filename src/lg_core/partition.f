C
      subroutine partition(imsgin,xmsgin,cmsgin,msgtype,nwds,ierr2)
C
C ######################################################################
C
C        $Log: partition.f,v $
C        Revision 2.00  2007/11/05 19:46:03  spchu
C        Import to CVS
C
CPVCS
CPVCS       Rev 1.21   02 Oct 2007 12:40:28   spchu
CPVCS    original version
C
C#######################################################################
C
      implicit none
C
      include "local_element.h"
      character*32 cmo, isubname, cmoatt
C
      pointer (ipitet, itet1)
      integer itet1(1000000)
      pointer (ipjtet, jtet1)
      integer jtet1(1000000)
      pointer (ipitetoff, itetoff)
      integer itetoff(1000000)
      pointer (ipjtetoff, jtetoff)
      integer jtetoff(1000000)
      pointer (ipitettyp, itettyp)
      integer itettyp(1000000)
      pointer (ipitetclr, itetclr)
      integer itetclr(1000000)
      pointer (ippartitions, ipartitions)
      integer ipartitions(1000000)
      pointer (ipperm, perm)
      integer perm(1000000)
      pointer (ipinvperm, invperm)
      integer invperm(1000000)
      pointer (ipvwgts, vwgts)
      integer vwgts(1000000)
      pointer (ipxadj, xadj)
      integer xadj(1000000)
      pointer (ipadjncy, adjncy)
      integer adjncy(1000000)
      pointer (ipewgts, ewgts)
      integer ewgts(1000000)
C
      pointer (ipiface, iface)
      integer iface(2,1000000)
C
      pointer (ipxface, xface)
      pointer (ipyface, yface)
      pointer (ipzface, zface)
      real*8 xface(1000000), yface(1000000), zface(1000000)
C
      pointer (ipitcount, itcount)
      integer itcount(1000000)
C
      pointer (ipitalias1, italias1)
      pointer (ipitalias2, italias2)
      integer italias1(1000000), italias2(1000000)
C
      pointer (ipitreorder, itreorder)
      integer itreorder(1000000)
C
      pointer (ipittemp, ittemp)
      pointer (ipittemp1, ittemp1)
      integer ittemp(1000000), ittemp1(1000000)
C
      pointer (ipitetpar, itetpar)
      pointer (ipitetkid, itetkid)
      pointer (ipitetlev, itetlev)
      integer itetpar(1000000), itetkid(1000000), itetlev(1000000)
C
      integer nelements, mbndry
      integer ilen, ityp, icscode, length
      integer it, i, jt, jf, icount, jcount
      integer iweightflag, nparts, ioptions, numbering, iedgecut
      integer ierr2, lenout, nefcmo, nencmo, icskid, icspar, icslev,
     *        nface, itpar, iflag, it1, it2, iunit, ntreal, jtoff
C
      integer nwds, imsgin(nwds), msgtype(nwds)
      real*8 xmsgin(nwds)
      character*32 cmsgin(nwds)
      character*132 logmess
C
      integer ireorder
      data ireorder / 1 /
      integer icolortet
      data icolortet / 0 /

CCCCCCCCCCCC
C begin
      isubname='partition'
C
      if(nwds.le.2) then
         cmo=cmsgin(2)
      else
         call cmo_get_name(cmo,icscode)
      endif
C
      if(nwds.le.3) then
         nparts=imsgin(3)
      else
         nparts=16
      endif
C
C
      call cmo_get_info('nodes_per_element',cmo,nencmo,ilen,ityp,
     *      icscode)
      call cmo_get_info('faces_per_element',cmo,nefcmo,ilen,ityp,
     *      icscode)
      call cmo_get_info('mbndry',cmo,mbndry,ilen,ityp,icscode)
      call cmo_get_info('nelements',cmo,nelements,ilen,ityp,icscode)
      call cmo_get_info('itettyp',cmo,ipitettyp,ilen,ityp,icscode)
      call cmo_get_info('itetclr',cmo,ipitetclr,ilen,ityp,icscode)
      call cmo_get_info('itetoff',cmo,ipitetoff,ilen,ityp,icscode)
      call cmo_get_info('jtetoff',cmo,ipjtetoff,ilen,ityp,icscode)
      call cmo_get_info('jtet',cmo,ipjtet,ilen,ityp,icscode)
      call cmo_get_info('itet',cmo,ipitet,ilen,ityp,icscode)
C
      cmoatt='partitions'
      call mmfindbk(cmoatt,cmo,ippartitions,lenout,icscode)
      if(icscode.ne.0) then
         length=nelements
         call mmgetblk('ippartitions',isubname,
     *                 ippartitions,length,1,icscode)
         do it=1,nelements
            ipartitions(it)=0
         enddo
C*****         cbuff =     'cmo/addatt/-def-/partitions/VINT/scalar/' //
C*****     *               'nelements/linear/permanent/gax/0.0 ;  ' //
C*****     *               'finish'
C*****         call dotaskx3d(cbuff,icscode)
C*****         call cmo_get_info('partitions',cmo,
C*****     *                     ippartitions,ilen,ityp,icscode)
      endif
C
      length=nelements
      call mmgetblk('perm',isubname,ipperm,length,2,icscode)
      call mmgetblk('invperm',isubname,ipinvperm,length,2,icscode)
      call mmgetblk('vwgts',isubname,ipvwgts,length,2,icscode)
      length=nelements+1
      call mmgetblk('xadj',isubname,ipxadj,length,2,icscode)
      length=nefcmo*nelements
      call mmgetblk('adjncy',isubname,ipadjncy,length,2,icscode)
      call mmgetblk('ewgts',isubname,ipewgts,length,2,icscode)
C
      if(icolortet.eq.0) then
         do it=1,nelements
            ipartitions(it)=-1
            vwgts(it)=1
         enddo
      else
         do it=1,nelements
            ipartitions(it)=-1
            if(itetclr(it).eq.icolortet) then
               vwgts(it)=5
            else
               vwgts(it)=1
            endif
         enddo
      endif
      call mmfindbk('itetpar',cmo,ipitetpar,lenout,icspar)
      if(icspar.ne.0) then
         icspar=0
         length=nelements
         call mmgetblk('itetpar',isubname,ipitetpar,length,1,icscode)
         do it=1,nelements
            itetpar(it)=0
         enddo
      endif
      call mmfindbk('itetkid',cmo,ipitetkid,lenout,icskid)
      if(icskid.ne.0) then
         icskid=0
         length=nelements
         call mmgetblk('itetkid',isubname,ipitetkid,length,1,icscode)
         do it=1,nelements
            itetkid(it)=0
         enddo
      endif
      call mmfindbk('itetlev',cmo,ipitetlev,lenout,icslev)
      if(icslev.ne.0) then
         icslev=0
         length=nelements
         call mmgetblk('itetlev',isubname,ipitetlev,length,1,icscode)
         do it=1,nelements
            itetlev(it)=0
         enddo
      endif
      if(icskid.eq.0.and.icspar.eq.0) then
         length=2*nefcmo*nelements
         call mmgetblk('iface',isubname,ipiface,length,1,icscode)
         nface=0
         do it=1,nelements
            if(itetkid(it).eq.0) then
               do i=1,nelmnef(itettyp(it))
                  if(jtet1(jtetoff(it)+i).eq.mbndry) then
                     if(itetpar(it).eq.0) then
                     else
                        itpar=itetpar(it)
                        iflag=0
                        dowhile(iflag.eq.0)
                           if(itetpar(itpar).eq.0) then
                              iflag=1
                           elseif(jtet1(jtetoff(itpar)+i).lt.
     *                            mbndry) then
                              iflag=1
                           elseif(jtet1(jtetoff(itpar)+i).gt.
     *                            mbndry) then
                              iflag=1
                           else
                              itpar=itetpar(itpar)
                           endif
                        enddo
                        jtoff=jtetoff(itpar)+i
                        if(jtet1(jtoff).eq.mbndry) then
                        elseif(jtet1(jtoff).gt.mbndry) then
                           jt=1+(jtet1(jtoff)-mbndry-1)/nefcmo
                           jf=jtet1(jtoff)-mbndry-nefcmo*(jt-1)
                           if(itetkid(jt).eq.0) then
                              nface=nface+1
                              iface(1,nface)=it
                              iface(2,nface)=jt
                           endif
                        else
                           jt=1+(jtet1(jtoff)-1)/nefcmo
                           jf=jtet1(jtoff)-nefcmo*(jt-1)
                           if(itetkid(jt).eq.0) then
                              nface=nface+1
                              iface(1,nface)=it
                              iface(2,nface)=jt
                           endif
                        endif
                     endif
                  elseif(jtet1(jtetoff(it)+i).gt.mbndry) then
                     jt=1+(jtet1(jtetoff(it)+i)-mbndry-1)/nefcmo
                     jf=jtet1(jtetoff(it)+i)-mbndry-nefcmo*(jt-1)
                     if(itetkid(jt).eq.0) then
                        if(it.lt.jt) then
                           nface=nface+1
                           iface(1,nface)=it
                           iface(2,nface)=jt
                        endif
                     endif
                  else
                     jt=1+(jtet1(jtetoff(it)+i)-1)/nefcmo
                     jf=jtet1(jtetoff(it)+i)-nefcmo*(jt-1)
                     if(itetkid(jt).eq.0) then
                        if(it.lt.jt) then
                           nface=nface+1
                           iface(1,nface)=it
                           iface(2,nface)=jt
                        endif
                     endif
                  endif
               enddo
            endif
         enddo
         length=nelements
         call mmgetblk('italias1',isubname,ipitalias1,length,1,icscode)
         call mmgetblk('italias2',isubname,ipitalias2,length,1,icscode)
         do it=1,nelements
            italias1(it)=0
            italias2(it)=0
         enddo
         ntreal=0
         do it=1,nelements
            if(itetkid(it).eq.0) then
               ntreal=ntreal+1
               italias1(ntreal)=it
               italias2(it)=ntreal
            endif
         enddo
         do it=1,nelements+1
            xadj(it)=0.0
         enddo
         icount=icount+1
         do i=1,nface
            it1=italias2(iface(1,i))
            it2=italias2(iface(2,i))
            if((it1.lt.1.or.it1.gt.nelements) .or.
     *         (it2.lt.1.or.it2.gt.nelements)) then
               icount=icount+1
            endif
            xadj(it1)=xadj(it1)+1
            xadj(it2)=xadj(it2)+1
         enddo
         icount=1
         do it=1,ntreal
            jcount=xadj(it)
            xadj(it)=icount
            icount=icount+jcount
         enddo
         xadj(ntreal+1)=icount
         length=ntreal
         call mmgetblk('itcount',isubname,ipitcount,length,1,icscode)
         do it=1,ntreal
            itcount(it)=0
         enddo
         do i=1,nface
            it1=italias2(iface(1,i))
            it2=italias2(iface(2,i))
            itcount(it1)=itcount(it1)+1
            adjncy(xadj(it1)+itcount(it1)-1)=it2
            ewgts(xadj(it1)+itcount(it1)-1)=1
            itcount(it2)=itcount(it2)+1
            adjncy(xadj(it2)+itcount(it2)-1)=it1
            ewgts(xadj(it2)+itcount(it2)-1)=1
         enddo
C
         iweightflag=0
         ioptions=0
         numbering=1
         iedgecut=0
C
C        call pmetis(ntreal,
C    *               xadj,adjncy,
C    *               vwgts,ewgts,
C    *               iweightflag,
C    *               nparts,
C    *               ioptions,
C    *               numbering,
C    *               iedgecut,
C    *               ipartitions,
C    *               perm,invperm)
         length=nelements
         call mmgetblk('ittemp',isubname,
     *                 ipittemp,length,1,icscode)
         do it=1,nelements
            ittemp(it)=0
         enddo
         do i=1,ntreal
            ittemp(italias1(i))=ipartitions(i)
         enddo
         do it=1,nelements
            ipartitions(it)=ittemp(it)
         enddo
         call mmrelblk('ittemp',isubname,ipittemp,icscode)
         do i=1,nface
            print *,i,iface(1,i),iface(2,i)
         enddo
         do it=1,nelements
            icount=xadj(it+1)-xadj(it)
            print *,it,icount,(adjncy(i),i=xadj(it),xadj(it+1)-1)
         enddo
         length=2*nface
         call mmgetblk('xface',isubname,ipxface,length,2,icscode)
         call mmgetblk('yface',isubname,ipyface,length,2,icscode)
         call mmgetblk('zface',isubname,ipzface,length,2,icscode)
         iunit=-1
         call hassign(iunit,'gmvmatrix_before',icscode)
         if (iunit.lt.0 .or. icscode.lt.0) then
           call x3d_error(isubname,'hassign bad file unit')
           write(logmess,*)
     1     'WARNING: file gmvmatrix_before not written' 
           call writloga('default',0,logmess,0,icscode)
         else

         write(iunit,"('gmvinput ascii')")
         write(iunit,"('nodes   ',i10)") 2*nface
         do i=1,nface
            it1=iface(1,i)
            it2=iface(2,i)
            xface(2*(i-1)+1)=dble(it1)/dble(nelements)
            yface(2*(i-1)+1)=-dble(it2)/dble(nelements)
            zface(2*(i-1)+1)=0.0
            xface(2*(i-1)+2)=dble(it2)/dble(nelements)
            yface(2*(i-1)+2)=-dble(it1)/dble(nelements)
            zface(2*(i-1)+2)=0.0
         enddo
         write(iunit,"(10(1pe14.5e3))") (xface(i),i=1,2*nface)
         write(iunit,"(10(1pe14.5e3))") (yface(i),i=1,2*nface)
         write(iunit,"(10(1pe14.5e3))") (zface(i),i=1,2*nface)
         write(iunit,"('cells   ',i10)") 0
         write(iunit,"('endgmv')")
         close(iunit)
         call mmrelblk('xface',isubname,ipxface,icscode)
         call mmrelblk('yface',isubname,ipyface,icscode)
         call mmrelblk('zface',isubname,ipzface,icscode)
         call mmrelblk('iface',isubname,ipiface,icscode)

         endif
      else
         icount=0
         xadj(1)=1
         do it=1,nelements
            do i=1,nelmnef(itettyp(it))
               if(jtet1(jtetoff(it)+i).eq.mbndry) then
               elseif(jtet1(jtetoff(it)+i).gt.mbndry) then
                  jt=1+(jtet1(jtetoff(it)+i)-mbndry-1)/nefcmo
                  jf=jtet1(jtetoff(it)+i)-mbndry-nefcmo*(jt-1)
                  icount=icount+1
                  adjncy(icount)=jt
                  ewgts(icount)=1
               else
                  jt=1+(jtet1(jtetoff(it)+i)-1)/nefcmo
                  jf=jtet1(jtetoff(it)+i)-nefcmo*(jt-1)
                  icount=icount+1
                  adjncy(icount)=jt
                  ewgts(icount)=1
               endif
            enddo
            xadj(it+1)=icount+1
         enddo
C
         iweightflag=0
         ioptions=0
         numbering=1
         iedgecut=0
C
C*****         call pmetis(nelements,
C*****     *               xadj,adjncy,
C*****     *               vwgts,ewgts,
C*****     *               iweightflag,
C*****     *               nparts,
C*****     *               ioptions,
C*****     *               numbering,
C*****     *               iedgecut,
C*****     *               ipartitions,
C*****     *               perm,invperm)
      endif
C
      if(icskid.eq.0.and.icspar.eq.0) then
         do it=1,nelements
            ipartitions(it)=ipartitions(it)+1
            if(itetkid(it).eq.0) then
               itetclr(it)=ipartitions(it)
            else
               itetclr(it)=0
            endif
         enddo
C
         if(ireorder.eq.1) then
            length=nelements
            call mmgetblk('itreorder',isubname,
     *                    ipitreorder,length,1,icscode)
            call mmgetblk('ittemp',isubname,
     *                    ipittemp,length,1,icscode)
            length=nencmo*nelements
            call mmgetblk('ittemp1',isubname,
     *                    ipittemp1,length,1,icscode)
            icount=0
            do i=1,nparts
               do it=1,nelements
                  if(ipartitions(it).eq.i) then
                     icount=icount+1
                     itreorder(it)=icount
                  endif
               enddo
            enddo
C
            do it=1,nelements
               ittemp(itreorder(it))=itetclr(it)
            enddo
            do it=1,nelements
               itetclr(it)=ittemp(it)
            enddo
C
            do it=1,nelements
               ittemp(itreorder(it))=ipartitions(it)
            enddo
            do it=1,nelements
               ipartitions(it)=ittemp(it)
            enddo
C
            do it=1,nelements
               ittemp(itreorder(it))=itettyp(it)
            enddo
            do it=1,nelements
               itettyp(it)=ittemp(it)
            enddo
C
            do it=1,nelements
               ittemp(itreorder(it))=itetpar(it)
            enddo
            do it=1,nelements
               itetpar(it)=ittemp(it)
            enddo
C
            do it=1,nelements
               ittemp(itreorder(it))=itetkid(it)
            enddo
            do it=1,nelements
               itetkid(it)=ittemp(it)
            enddo
C
            do it=1,nelements
               ittemp(itreorder(it))=itetlev(it)
            enddo
            do it=1,nelements
               itetlev(it)=ittemp(it)
            enddo
C
            do it=1,nelements
               it1=itreorder(it)
               do i=1,nelmnen(itettyp(it))
                  ittemp1(itetoff(it1)+i)=itet1(itetoff(it)+i)
               enddo
            enddo
            do it=1,nelements
               do i=1,nelmnen(itettyp(it))
                  itet1(itetoff(it)+i)=ittemp1(itetoff(it)+i)
               enddo
            enddo
C
            do it=1,nelements
               if(itetpar(it).gt.0) then
                  ittemp(it)=itreorder(itetpar(it))
               else
                  ittemp(it)=0
               endif
            enddo
            do it=1,nelements
               itetpar(it)=ittemp(it)
            enddo
            do it=1,nelements
               if(itetkid(it).gt.0) then
                  ittemp(it)=itreorder(itetkid(it))
               else
                  ittemp(it)=0
               endif
            enddo
            do it=1,nelements
               itetkid(it)=ittemp(it)
            enddo
            call geniee_cmo(cmo)
C
            length=2*nefcmo*nelements
            call mmgetblk('iface',isubname,ipiface,length,1,icscode)
            nface=0
            do it=1,nelements
               if(itetkid(it).eq.0) then
                  do i=1,nelmnef(itettyp(it))
                     if(jtet1(jtetoff(it)+i).eq.mbndry) then
                        if(itetpar(it).eq.0) then
                        else
                           itpar=itetpar(it)
                           iflag=0
                           dowhile(iflag.eq.0)
                              if(itetpar(itpar).eq.0) then
                                 iflag=1
                              elseif(jtet1(jtetoff(itpar)+i).lt.
     *                               mbndry) then
                                 iflag=1
                              elseif(jtet1(jtetoff(itpar)+i).gt.
     *                               mbndry) then
                                 iflag=1
                              else
                                 itpar=itetpar(itpar)
                              endif
                           enddo
                           jtoff=jtetoff(itpar)+i
                           if(jtet1(jtoff).eq.mbndry) then
                           elseif(jtet1(jtoff).gt.mbndry) then
                              jt=1+(jtet1(jtoff)-mbndry-1)/nefcmo
                              jf=jtet1(jtoff)-mbndry-nefcmo*(jt-1)
                              if(itetkid(jt).eq.0) then
                                 nface=nface+1
                                 iface(1,nface)=it
                                 iface(2,nface)=jt
                              endif
                           else
                              jt=1+(jtet1(jtoff)-1)/nefcmo
                              jf=jtet1(jtoff)-nefcmo*(jt-1)
                              if(itetkid(jt).eq.0) then
                                 nface=nface+1
                                 iface(1,nface)=it
                                 iface(2,nface)=jt
                              endif
                           endif
                        endif
                     elseif(jtet1(jtetoff(it)+i).gt.mbndry) then
                        jt=1+(jtet1(jtetoff(it)+i)-mbndry-1)/nefcmo
                        jf=jtet1(jtetoff(it)+i)-mbndry-nefcmo*(jt-1)
                        if(itetkid(jt).eq.0) then
                           if(it.lt.jt) then
                              nface=nface+1
                              iface(1,nface)=it
                              iface(2,nface)=jt
                           endif
                        endif
                     else
                        jt=1+(jtet1(jtetoff(it)+i)-1)/nefcmo
                        jf=jtet1(jtetoff(it)+i)-nefcmo*(jt-1)
                        if(itetkid(jt).eq.0) then
                           if(it.lt.jt) then
                              nface=nface+1
                              iface(1,nface)=it
                              iface(2,nface)=jt
                           endif
                        endif
                     endif
                  enddo
               endif
            enddo
            length=2*nface
            call mmgetblk('xface',isubname,ipxface,length,2,icscode)
            call mmgetblk('yface',isubname,ipyface,length,2,icscode)
            call mmgetblk('zface',isubname,ipzface,length,2,icscode)

            iunit=-1
            call hassign(iunit,'gmvmatrix_after',icscode)
            if (iunit.lt.0 .or. icscode.lt.0) then
             call x3d_error(isubname,'hassign bad file unit')
             write(logmess,*)
     1       'WARNING: file gmvmatrix_before not written'
             call writloga('default',0,logmess,0,icscode)

           else

            write(iunit,"('gmvinput ascii')")
            write(iunit,"('nodes   ',i10)") 2*nface
            do i=1,nface
               it1=iface(1,i)
               it2=iface(2,i)
               xface(2*(i-1)+1)=dble(it1)/dble(nelements)
               yface(2*(i-1)+1)=-dble(it2)/dble(nelements)
               zface(2*(i-1)+1)=0.0
               xface(2*(i-1)+2)=dble(it2)/dble(nelements)
               yface(2*(i-1)+2)=-dble(it1)/dble(nelements)
               zface(2*(i-1)+2)=0.0
            enddo
            write(iunit,"(10(1pe14.5e3))") (xface(i),i=1,2*nface)
            write(iunit,"(10(1pe14.5e3))") (yface(i),i=1,2*nface)
            write(iunit,"(10(1pe14.5e3))") (zface(i),i=1,2*nface)
            write(iunit,"('cells   ',i10)") 0
            write(iunit,"('endgmv')")
            close(iunit)
            call mmrelblk('xface',isubname,ipxface,icscode)
            call mmrelblk('yface',isubname,ipyface,icscode)
            call mmrelblk('zface',isubname,ipzface,icscode)
            call mmrelblk('iface',isubname,ipiface,icscode)
           endif
         endif
C
      else
         do it=1,nelements
            ipartitions(it)=ipartitions(it)+1
            itetclr(it)=ipartitions(it)
         enddo
      endif
C
      goto 9999
 9999 continue
C
      call mmrelprt(isubname,icscode)
C
      return
      end
