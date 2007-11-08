*dk,ltripedge
      function ltripedge(i,iedg,itet,itetoff,itettyp,iparent,jtet,
     &   jtetoff,mbndry,nef_cmo,icr1,icontab)
C
C
C #####################################################################
C
C     PURPOSE -
C
C        This function is true iff the given edge is determined
C        to be a triple edge.
C
C     INPUT ARGUMENTS -
C
C     OUTPUT ARGUMENTS -
C
C     CHANGE HISTORY -
C
C        $Log:   /pvcs.config/t3d/src/ltripedge.f_a  $
CPVCS    
CPVCS       Rev 1.2   Fri Sep 03 10:47:12 1999   kuprat
CPVCS    Added documentation.
CPVCS    
CPVCS       Rev 1.1   Mon Nov 02 17:47:36 1998   kuprat
CPVCS    Corrected format statement.
CPVCS    
CPVCS       Rev 1.0   Fri Oct 23 16:17:16 1998   kuprat
CPVCS    Initial revision.

      implicit none

      include 'local_element.h'
      include 'chydro.h'
      
      integer maxelt
      parameter (maxelt=100)
      integer ieltlist(maxelt)
      integer i,iedg,iprevelt,icurrelt,icurredge,leneltlist,ic1,
     &   ic2,itet(*),itetoff(*),itettyp(*),ipar1,ipar2,iparent(*),
     &   imaxpar,iminpar,j,j1,jtetj,jtet(*),jtetoff(*),mbndry,
     &   nextelt,nef_cmo,nmat,ierrw,jedg,jc1,jc2,jpar1,jpar2,
     &   jmaxpar,jminpar,icra,icrb,icr1(*),nsc,nsa,nsb,icontab(50,*),
     &   n1,n2,njump
      character*132 logmess

      logical ltripedge,lhitoneboundary

      lhitoneboundary=.false.
      iprevelt=0
      icurrelt=i
      icurredge=iedg                  
      leneltlist=1
      ieltlist(1)=i

      ic1=itet(
     &   ielmedge1(1,iedg,itettyp(icurrelt))
     &   +itetoff(icurrelt))
      ic2=itet(
     &   ielmedge1(2,iedg,itettyp(icurrelt))
     &   +itetoff(icurrelt))
      ipar1=iparent(ic1)
      ipar2=iparent(ic2)
      imaxpar=max(ipar1,ipar2)
      iminpar=min(ipar1,ipar2)

c.... Compute the number of materials around the edge and
c.... the number of constraints of the edge.  If the sum of
c.... these numbers is greater than 3, we deem edge IEDG
c.... of element I to be a 'triple' edge.

c.... First transit around edge IEDG of element I and determine
c.... how many material jumps there are.  If the cycle around the
c.... edge is closed, the number of materials around the edge 
c.... (NMAT) is equal to the number of jumps around the edge,
c.... unless there are no jumps, in which case NMAT=1.
c.... If the cycle is not closed (the case of a boundary edge),
c.... the number of materials is equal to the number of jumps
c.... plus one.

      njump=0

 1500 continue

c.... Loop over faces of current element to see which face we
c.... will transit through.

      do 1510 j=1,nelmnef(itettyp(icurrelt))

c.... Loop over edges of current face to see if any are equal
c.... to the pivot edge, in which case we attempt to transit through the
c.... face.

         do j1=1,ielmface0(j,itettyp(icurrelt))
            if (ielmface2(j1,j,itettyp(icurrelt)).eq
     &         .icurredge) then

c.... We attempt to transit through face J.

               jtetj=jtet(j+jtetoff(icurrelt))

c.... If face J is a boundary face, check if we haven't hit
c.... a boundary face before.

               if (jtetj.eq.mbndry) then

c.... If we haven't hit a boundary face before, go to the 
c.... beginning of the cycle and transit in the opposite 
c.... direction.  We expect to hit a second boundary face
c.... eventually.

                  if (.not.lhitoneboundary) then
                     lhitoneboundary=.true.
                     if (leneltlist.ge.2) then
                        iprevelt=ieltlist(2)
                        icurrelt=i
                        icurredge=iedg
                        goto 1500
                     else
                        goto 1510
                     endif
                  else

c.... We have hit a boundary face before, so our transiting
c.... task is complete.  

                     nmat=njump+1
                     goto 10
                     
                  endif          

c.... If face J is an internal boundary face, we define NEXTELT to be
c.... the element on the other side of face J.  However we don't
c.... actually transit through J unless we aren't moving
c.... backwards (i.e. NEXTELT.NE.IPREVELT).  If this is true, we
c.... increment NJUMP.

               elseif (jtetj.gt.mbndry) then
                  nextelt=1+(jtetj-mbndry-1)/nef_cmo
                  if (nextelt.ne.iprevelt) njump=njump+1
               else

c.... Here face J is a regular (nonboundary) face and we 
c.... NEXTELT to be the element on the opposite side.

                  nextelt=1+(jtetj-1)/nef_cmo
               endif

c.... We only transit through J to NEXTELT if we aren't
c.... moving backwards.

               if (nextelt.ne.iprevelt) then 

c.... The next element is the beginning element, so we have
c.... closed the cycle.

                  if (nextelt.eq.i) then

c.... If NJUMP=0, there is only one material around the edge.

                     if (njump.eq.0) then
                        nmat=1

c.... If NJUMP=1, we have an error, because it is impossible to
c.... have a closed cycle with one material jump.

                     elseif (njump.eq.1) then
                        print*,'LTRIPEDGE: Top. error!'
                        stop
                     else

c.... Here the number of materials is equal to the number of jumps.

                        nmat=njump
                     endif 
                     goto 10

                  endif

c.... Here we have not closed the cycle, so we simply make the previous
c.... element equal to the current element and make the current 
c.... element equal to the next element, and write the new current
c.... element into the element list IELTLIST.

                  iprevelt=icurrelt
                  leneltlist=leneltlist+1
                  if (leneltlist.gt.maxelt) then
                     write(logmess,'(a,i6,a)') 'More than ', maxelt,
     &                  'elements sharing an edge!'
                     call writloga('default',0,logmess,0,ierrw)
                     stop
                  endif 
                  icurrelt=nextelt
                  ieltlist(leneltlist)=nextelt

c.... We now locate the pivot edge in the new current element.

                  do jedg=1,nelmnee(itettyp(icurrelt))
                     jc1=itet(
     &                  ielmedge1(1,jedg,itettyp(icurrelt))
     &                  +itetoff(icurrelt))
                     jc2=itet(
     &                  ielmedge1(2,jedg,itettyp(icurrelt))
     &                  +itetoff(icurrelt))
                     jpar1=iparent(jc1)
                     jpar2=iparent(jc2)
                     jmaxpar=max(jpar1,jpar2)
                     jminpar=min(jpar1,jpar2)
                     if (jminpar.eq.iminpar.and.jmaxpar.eq
     &                  .imaxpar) then
                        icurredge=jedg
                        goto 1500
                     endif
                  enddo
                  write(logmess,'(a)') 'LTRIPEDGE: Topological error!!'
                  call writloga('default',0,logmess,0,ierrw)
                  write(logmess,'(a,2i8)') 'iminpar/imaxpar=',iminpar,
     &               imaxpar
                  call writloga('default',0,logmess,0,ierrw)
                  write(logmess,'(a,i8)') 'cannot match element ',
     &               icurrelt
                  call writloga('default',0,logmess,0,ierrw)
                  stop
               endif
            endif
         enddo
 1510 continue

c.... At this point we did not bail out of the previous loop, meaning
c.... we did not (i) close the cycle and (ii) did not encounter
c.... two boundary faces.  This is a topological error.

      write(logmess,'(a)') 'LTRIPEDGE: Topological error!!'
      call writloga('default',0,logmess,0,ierrw)
      write(logmess,'(a,2i8)') 'iminpar/imaxpar=',iminpar,
     &   imaxpar
      call writloga('default',0,logmess,0,ierrw)
      write(logmess,'(a,i8)') 'last element ',
     &   icurrelt
      call writloga('default',0,logmess,0,ierrw)
      stop

 10   continue

c.... We now loop through the constraints for both endpoints and
c.... see how many they have in common.  This will be the number
c.... of constraints of the edge.

      icra=icr1(ic1)
      icrb=icr1(ic2)
      if(icra.eq.0.or.icrb.eq.0) then
         nsc=0
         go to 100
      endif
      nsc=0
      nsa=icontab(1,icra)
      nsb=icontab(1,icrb)
      do n1=1,nsa
         do n2=1,nsb
            if (icontab(2+n1,icra).eq.icontab(2+n2,icrb))
     *          then
                nsc=nsc+1
            endif
         enddo
      enddo

 100  continue

c.... The edge is a triple edge iff the sum of materials and constraints
c.... is at least three.

      if (nmat+nsc.ge.3) then
         ltripedge=.true.
      else
         ltripedge=.false.
      endif
 
      return
      end
