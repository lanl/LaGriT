      subroutine extract_isosurface(cmoout,cmoin,npoints,ntets,
     *                              fieldname,value,ierr1)
C
C #####################################################################
C
C     PURPOSE -
C
C        Sets up parameters for an isosurface extraction.
C
C     INPUT ARGUMENTS -
C
C        cmoout    - THE MESH OBJECT TO HOLD THE RESULT
C        cmoin     - THE INCOMING MESH OBJECT
C        npoints   - NUMBER OF POINTS
C        ntets     - NUMBER OF TETS
C        fieldname - NAME OF FIELD
C        value     - VALUE FOR ISOSURFACE
C
C     OUTPUT ARGUMENTS -
C
C        ierr1   - ERROR RETURNED (ZERO IF NO ERRORS)
C
C     CHANGE HISTORY -
C
C$Log: extract_isosurface.f,v $
CRevision 2.00  2007/11/05 19:45:54  spchu
CImport to CVS
C
CPVCS    
CPVCS       Rev 1.0   26 Jan 2000 16:39:22   dcg
CPVCS    Initial revision.
CPVCS    
CPVCS       Rev 1.8   Mon Apr 14 16:45:06 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.7   Fri Oct 18 10:55:38 1996   jxf
CPVCS    added integer field capability and fixed bug in call to mmgetblk
CPVCS
CPVCS       Rev 1.6   Thu Feb 15 09:13:18 1996   dcg
CPVCS    look for requested attribute of mesh object
CPVCS    for isosurface extraction
CPVCS
CPVCS       Rev 1.5   06/21/95 13:19:38   het
CPVCS    Replace the character literal by a character variable in the call to hgetprt.f and add the hybrid grid arrays
CPVCS
CPVCS       Rev 1.4   06/02/95 08:43:36   jxf
CPVCS    Changed multiple returns to one return
CPVCS
CPVCS       Rev 1.3   05/17/95 09:34:10   dcg
CPVCS    remove cmo_get_name for old cmo
CPVCS
CPVCS       Rev 1.2   02/27/95 11:26:46   jxf
CPVCS    Added cmoin.  Changed 2-region intrface to intrfac2.
CPVCS
CPVCS       Rev 1.1   02/13/95 09:18:36   jxf
CPVCS    New routine.
CPVCS
CPVCS       Rev 1.0   02/10/95 13:58:44   dcg
CPVCS
C
C ######################################################################
C
      implicit none
C
C   Definitions for incoming (existing) cmo
C
      pointer (ipfield,field)
      real*8 field(1000000)
      pointer (ipifield,ifield)
      integer ifield(1000000)
      pointer (ipsum,sum)
      real*8 sum(1000000)
C
      character*132 logmess
      character*(*) cmoin, cmoout
      character*32 isubname, fieldname, crank, ctype,
     *    clen,cinter,cpers,cio
      integer index,ierror_return,lnam,icharlnf,ier,lin,itin,
     *  nnodes,i,npoints,ntets,l,irank,ierr1
      real*8 value

C
      ierr1 = 0
C
C   Get the existing cmo
C
      lnam=icharlnf(fieldname)
      call cmo_get_attparam(fieldname,cmoin,index,ctype,crank,
     *    clen,cinter,cpers,cio,ierror_return)
 
C   Check whether this is an integr field.  If so, make a dummy real*8
C   field with the same values to pass on to isosurface.
 
      if( (ctype(1:4).eq.'VINT') .or. (ctype(1:4).eq.'VINT') ) then
 
C         integer type; make a real field to pass to isosurface.
 
          call cmo_get_info(fieldname(1:lnam),cmoin,ipifield,lin,itin,
     *                      ier)
         if(ier.ne.0) go to 9998
         call cmo_get_info('nnodes',cmoin,nnodes,lin,itin,ier)
         call mmgetblk('sum',isubname,ipsum,nnodes,2,ier)
         do i=1,nnodes
           sum(i) = 1.0*ifield(i)
         enddo
         call isosurface(ipsum,value,npoints,ntets,cmoin,cmoout,ierr1)
         call mmrelprt (isubname,ier)
         go to 9999
      else
         call cmo_get_info(fieldname(1:lnam),cmoin,ipfield,lin,itin,
     *                     ier)
         if(ier.ne.0) go to 9998
         if(crank(1:6).eq.'vector') then
C  gather magnitude for isosurface calculation
            call cmo_get_info('nnodes',cmoin,nnodes,lin,itin,ier)
            call mmgetblk('sum',isubname,ipsum,nnodes,2,ier)
            call cmo_get_info(crank,cmoin,irank,lin,itin,ier)
             do i=1,nnodes
               sum (i)=0.
               do l=1,irank
                 sum(i)=(field((i-1)*irank+l))**2
               enddo
                 sum(i)=sqrt(sum(i))
             enddo
            call isosurface(ipsum,value,npoints,ntets,cmoin,cmoout,
     *                      ierr1)
            call mmrelprt (isubname,ier)
            go to 9999
         else
C  must have rank 1
            call isosurface(ipfield,value,npoints,ntets,cmoin,cmoout,
     *                      ierr1)
            go to 9999
         endif
      endif
 9998 ierr1 = 1
      write(logmess,'(a,a)')
     *     'Invalid field name for isosurface:  ',fieldname
       call writloga('default',0,logmess,0,ierr1)
      goto 9999
C
C
 9999 continue
      return
      end
 
