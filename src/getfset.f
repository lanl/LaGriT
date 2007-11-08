      subroutine getfset(name,ipiftet,ibitpos,num,ierr,
     *                   flen,ftet,mode,ivalue)
C
C
C ######################################################################
C
C     PURPOSE -
C
C        This routine extracts the boundary faces for a given set.
C
C     INPUT ARGUMENTS -
C
C        name    - the name of the set.
C        ipftet  - pointer to the array in which to put the set.
C
C     OUTPUT ARGUMENTS -
C
C        ibitpos - the position of the bit.
C        num     - the number of elements in the set.
C        ierr    - 0 => no error
C                  1 => error
C
C     CHANGE HISTORY -
C
C ######################################################################
C
      implicit none
C
      character*132 logmess
C
      include 'cmo_lg.h'
C
C
C ######################################################################
C
C
      integer nbitsmax,ibitpos,num,ierr,ndup,i1,icharlnf
      parameter (nbitsmax=32)
C
C 
      pointer( ipiftet  , iftet )
      integer iftet(1000000)
C
      integer itest(nbitsmax)
C
      character*32 name, iword, mode
      character*32 cmo, isubname
      integer ierror,ilen,ics,isetchg,len2,i,len1,len,flen,
     *  icscode,mask
      integer ftet(flen),nfsetcnt,ivalue
      pointer (ipfsetnames,fsetnames)
      character*32 fsetnames(*)
C
C
      integer shiftr
      data mask/ 1 /
C
      isubname='getfset'
      ierr=0
      num=0
C
C
C     ******************************************************************
C
C     get mesh object
C
      call cmo_get_name(cmo,ierror)
C
C     ******************************************************************
      isetchg=0
      if(mode.eq.name)then
        call mmfindbk('fsetnames',cmo,ipfsetnames,ilen,ics)
        isetchg=0
        len2=icharlnf(name)
C
        do 10 i=1,nbitsmax
           iword=fsetnames(i)
           len1=icharlnf(iword)
           len=max(len1,len2)
           itest(i)=0
           if(name(1:len).eq.iword(1:len))then
             itest(i)=i
             nfsetcnt=i
           endif
 10     continue
        call kmprsn(nbitsmax,itest,1,itest,1,itest,1,ndup)
        if(ndup.eq.0) then
           ierr=1
           write(logmess,1000) name
           call writloga('default',1,logmess,1,ierr)
 1000      format(' ERROR - THE NAME ',2x,a8,2x,' DOES NOT EXIST !')
           goto 9999
        endif
      else
        nfsetcnt=ivalue
      endif
C     
C     IFTET IS THE LOCAL ARRAY THAT HOLDS THE ON/OFF FLAG FOR THE 
C     LOCAL ELEMENT FACE CORRESPONDING TO THE GIVEN FSET
      do i=1,flen
        if(ftet(i).eq.nfsetcnt)then
           num=num+1
           iftet(i)=1
        else
           iftet(i)=0
        endif
      enddo
C     ******************************************************************
C
 9999 continue
      call mmrelprt(isubname,icscode)
      return
      end
 
 
