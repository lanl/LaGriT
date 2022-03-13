*dk,getnoditet
      subroutine getnoditet(itet,itetoff,itettyp,mpary,mpno,
     &   nelements,nnodes,noditet,noditetoff,wtnoditet)
C #####################################################################
C
C     PURPOSE -
C
C     INPUT ARGUMENTS -
C
C     OUTPUT ARGUMENTS -
C
C     CHANGE HISTORY -
C $Log: getnoditet.f,v $
C Revision 2.00  2007/11/05 19:45:57  spchu
C Import to CVS
C
CPVCS    
CPVCS       Rev 1.0   31 Jan 2000 17:12:30   kuprat
CPVCS    Initial revision.
C
C ######################################################################
 
      implicit none
 
      include 'local_element.h'
 
      integer lenptr
      parameter (lenptr=1000000)
 
      integer noditet(2,lenptr),noditetoff(lenptr),itet(lenptr),
     &   itetoff(lenptr),itettyp(lenptr),mpary(lenptr),mpno,nelements
     &   ,nnodes
      real*8 wtnoditet(lenptr)
 
      pointer (ipinvmpary,invmpary)
      pointer (ipihead,ihead)
      pointer (iplink,link)
      pointer (ipnoditet1,noditet1)
      pointer (ipwtnoditet1,wtnoditet1)
      integer invmpary(lenptr),ihead(lenptr),link(lenptr),noditet1(2
     &   ,lenptr)
      real*8 wtnoditet1(lenptr)
 
      integer i,j,k,next1,next,linki,ntri,nqud,nsimp,icscode
      character*32 isubname
 
      integer i_,next3,iprev3,next4,iopp4,iprev4
 
      next3(i_)=mod(i_,3)+1
      iprev3(i_)=mod(i_+1,3)+1
      next4(i_)=mod(i_,4)+1
      iopp4(i_)=mod(i_+1,4)+1
      iprev4(i_)=mod(i_+2,4)+1
 
      isubname='getnoditet'
 
      ntri=0
      nqud=0
      do i=1,nelements
         if (itettyp(i).eq.ifelmtri) then
            ntri=ntri+1
         elseif (itettyp(i).eq.ifelmqud) then
            nqud=nqud+1
         endif
      enddo
 
      nsimp=12*nqud+3*ntri
 
      call mmgetblk('noditet1',isubname,ipnoditet1,2*nsimp,1,icscode)
      call mmgetblk('link',isubname,iplink,nsimp,1,icscode)
      call mmgetblk('wtnoditet1',isubname,ipwtnoditet1,nsimp,2,icscode)
      call mmgetblk('ihead',isubname,ipihead,mpno,1,icscode)
 
      call mmgetblk('invmpary',isubname,ipinvmpary,nnodes,1,icscode)
 
      do i=1,nnodes
         invmpary(i)=0
      enddo
      do i=1,mpno
         invmpary(mpary(i))=i
         ihead(i)=0
      enddo
      next1=1
      do i=1,nelements
         if (itettyp(i).eq.ifelmtri) then
            do j=1,3
               k=invmpary(itet(itetoff(i)+j))
               if (k.ne.0) then
                  if (ihead(k).eq.0) then
                     ihead(k)=next1
                  else
                     linki=ihead(k)
                     do while (link(linki).ne.0)
                        linki=link(linki)
                     enddo
                     link(linki)=next1
                  endif
 
                  noditet1(1,next1)=itet(itetoff(i)+next3(j))
                  noditet1(2,next1)=itet(itetoff(i)+iprev3(j))
                  wtnoditet1(next1)=1.d0
                  link(next1)=0
                  next1=next1+1
               endif
            enddo
         elseif (itettyp(i).eq.ifelmqud) then
            do j=1,4
               k=invmpary(itet(itetoff(i)+j))
               if (k.ne.0) then
                  if (ihead(k).eq.0) then
                     ihead(k)=next1
                  else
                     linki=ihead(k)
                     do while (link(linki).ne.0)
                        linki=link(linki)
                     enddo
                     link(linki)=next1
                  endif
                  noditet1(1,next1)=itet(itetoff(i)+next4(j))
                  noditet1(2,next1)=itet(itetoff(i)+iprev4(j))
                  wtnoditet1(next1)=0.5d0
                  link(next1)=next1+1
                  next1=next1+1
                  noditet1(1,next1)=itet(itetoff(i)+next4(j))
                  noditet1(2,next1)=itet(itetoff(i)+iopp4(j))
                  wtnoditet1(next1)=0.5d0
                  link(next1)=next1+1
                  next1=next1+1
                  noditet1(1,next1)=itet(itetoff(i)+iopp4(j))
                  noditet1(2,next1)=itet(itetoff(i)+iprev4(j))
                  wtnoditet1(next1)=0.5d0
                  link(next1)=0
                  next1=next1+1
               endif
            enddo
         endif
      enddo
 
      next=1
      do i=1,mpno
         noditetoff(i)=next-1
         if (ihead(i).ne.0) then
            noditet(1,next)=noditet1(1,ihead(i))
            noditet(2,next)=noditet1(2,ihead(i))
            wtnoditet(next)=wtnoditet1(ihead(i))
            next=next+1
            linki=link(ihead(i))
            do while (linki.ne.0)
               noditet(1,next)=noditet1(1,linki)
               noditet(2,next)=noditet1(2,linki)
               wtnoditet(next)=wtnoditet1(linki)
               next=next+1
               linki=link(linki)
            enddo
         endif
      enddo
      noditetoff(mpno+1)=next-1
 
 9999 continue
 
      call mmrelprt(isubname,icscode)
 
      return
      end
 
