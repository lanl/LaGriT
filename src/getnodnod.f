      subroutine getnodnod(ipnodnod,nnfirst,itet,jtet,mpary,mpno,
     &   nelements,nnodes,mbndry)
CPVCS $Log:   /pvcs.config/t3d/src/getnodnod.f_a  $
CPVCS    
CPVCS       Rev 1.7   04 Apr 2000 12:05:12   kuprat
CPVCS    Added MBNDRY into argument list.
CPVCS    
CPVCS       Rev 1.6   Mon Apr 14 16:49:52 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.5   Thu Mar 20 13:06:06 1997   kuprat
CPVCS    Check for nodes with topological inconsistencies and
CPVCS    remove them from the mass point array.
CPVCS    
CPVCS       Rev 1.4   Thu Nov 21 09:58:00 1996   kuprat
CPVCS    Got rid of second literals in argument list.
CPVCS    
CPVCS       Rev 1.3   11/07/95 17:18:10   dcg
CPVCS    change flag to 2 in mmgetblk calls
CPVCS    
CPVCS       Rev 1.2   05/30/95 17:15:02   kuprat
CPVCS    Changed the convention for jtet to 'mod 3' from 'mod 4'.
CPVCS    
      
c    Get the ordered 'node-node relation':
c    Given a node I in the mass point array MPARY, we list all the neighbouring
c    nodes J in counterclockwise order.  The relation 
c    actually consists of two arrays, NNFIRST and NODNOD.  
c    For node I=MPARY(K), the first neighour occurs at position NNFIRST(K) 
c    in NODNOD, and the last one occurs at NNFIRST(K+1)-1.  (Hence NNFIRST
c    must be dimensioned to have length at least MPNO+1.)
      
      implicit real*8 (a-h,o-z)
      pointer (ipnodnod,nodnod), (ipistart,istart), (ipic,ic),
     &   (ipicc,icc)

c This algorithm is meant for triangular grids.
      parameter (nen=3,nef=3)
      dimension nodnod(*),nnfirst(*),itet(nen,*),jtet(nef,*),mpary(*),
     &   istart(*),ic(*),icc(*)

      character*32 isubname, iblkname1, isubname1
      
      iprev3(ii)=mod(ii+1,3)+1
      next3(ii)=mod(ii,3)+1

      isubname='getnodnod'

      call mmgetnam(ipnodnod,iblkname1,isubname1,icscode)
      call mmgetlen(ipnodnod,lenorig,icscode)
      nnlen=lenorig
      call mmgetblk('istart',isubname,ipistart,nnodes,2,icscode)
      icclen=100
      call mmgetblk('icc',isubname,ipicc,icclen,2,icscode)
      iclen=100
      call mmgetblk('ic',isubname,ipic,iclen,2,icscode)
 
 100  continue

      do k=1,mpno
         istart(mpary(k))=0
      enddo
         
      do i=1,nelements
         do j=1,nen
            istart(itet(j,i))=i
         enddo
      enddo
      
      next=1
      do 10 k=1,mpno
         node=mpary(k)
         nnfirst(k)=next
         if (istart(node).eq.0) goto 10
         ictet=istart(node)
         if (node.eq.itet(1,ictet)) then
            lind=1
         elseif (node.eq.itet(2,ictet)) then
            lind=2
         else
            lind=3
         endif

         ind=next3(lind)
         icc(1)=itet(ind,ictet)
         ind=next3(ind)
         icc(2)=itet(ind,ictet)
         ncc=2
         nc=0
         ind=iprev3(ind)
         jadj=jtet(ind,ictet)
         do while(jadj.lt.mbndry.and.ncc.lt.1000)
            ictet=(jadj-1)/nef+1
            ind=mod(jadj-1,nef)+1
            nextn=itet(ind,ictet)
            if (nextn.eq.icc(1)) goto 20
            ncc=ncc+1
            if (ncc.ge.icclen) then
               inc=100
               icclen=icclen+inc
               call mmincblk('icc',isubname,ipicc,inc,icscode)
            endif
            icc(ncc)=nextn
            ind=iprev3(ind)
            jadj=jtet(ind,ictet)
         enddo

         if (ncc.ge.1000) then
            print*,'Corrupted topology for node ',node,'.  Dropping it.'
            do k1=k+1,mpno
               mpary(k1-1)=mpary(k1)
            enddo
            mpno=mpno-1
            goto 100
         endif

         ind=iprev3(lind)
         ictet=istart(node)
         jadj=jtet(ind,ictet)
         do while(jadj.lt.mbndry.and.nc.lt.1000)
            ictet=(jadj-1)/nef+1
            ind=mod(jadj-1,nef)+1
            nextn=itet(ind,ictet)
            nc=nc+1
            if (nc.ge.iclen) then
               inc=100
               iclen=iclen+inc
               call mmincblk('ic',isubname,ipic,inc,icscode)
            endif
            ic(nc)=nextn
            ind=next3(ind)
            jadj=jtet(ind,ictet)
         enddo

         if (nc.ge.1000) then
            print*,'Corrupted topology for node ',node,'.  Dropping it.'
            do k1=k+1,mpno
               mpary(k1-1)=mpary(k1)
            enddo
            mpno=mpno-1
            goto 100
         endif

 20      if (next+ncc+nc.ge.nnlen) then
            inc=( (next+ncc+nc)-nnlen) + mpno
            nnlen=nnlen+inc
            call mmincblk(iblkname1,isubname1,ipnodnod,inc,icscode)
         endif
         do j=1,nc
            nodnod(next+j-1)=ic(nc+1-j)
         enddo
         next=next+nc
         do j=1,ncc
            nodnod(next+j-1)=icc(j)
         enddo
         next=next+ncc
 10   continue

      nnfirst(mpno+1)=next
      call mmrelprt(isubname,icscode)
      return
      end
