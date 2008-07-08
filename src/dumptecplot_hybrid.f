C ######################################################################
*dk,dumptecplot_hybrid
       subroutine dumptecplot_hybrid(ifile,ifileini,iotype)
C
C ######################################################################
C 
C Initial implementation by: Daniel R Einstein, PNNL
C 
C This file writes contempory ASCII or BINARY TECPLOT format           #
C Tecplot write_binary functions are located in the static library     #
C tecio.a and can write to the following platforms:                    #
C                            sgix.65                                   #
C                            sgix64.65                                 #
C                            ibmx64.51                                 #
C                            hp7xx.11                                  #
C                            hp7xx64.11                                #
C                            sun4.57                                   #
C                            sun464.57                                 #
C                            linux.24                                  #
C                            linux64.24                                #
C The include file tecio.inc should be placed in $LAGRIT_HOME/src.     #
C The Makefile in $NWGRIDHOME/main should be modified to include:      #   
C             TECLIBDIR=$TEC100HOME/lib                                #            
C             INCDIR+=$TEC100HOME/include                              #            
C             EXTRALIBS=-lstdc++                                       #           
C             TECLIBNAME=tecio.a                                       #            
C             TECLIB=$TECLIBDIR/$TECLIBNAME                            #
C                                                                      #
C ######################################################################
C
C         $Log: dumptecplot_hybrid.f,v $
C         Revision 2.00  2007/11/05 19:45:53  spchu
C         Import to CVS
C
CPVCS    
CPVCS       Rev 1.4   15 May 2007 12:08:18   gable
CPVCS    Fixed typo in header information. 
CPVCS    
CPVCS       Rev 1.3   05 Mar 2007 16:35:36   gable
CPVCS    Modified rev 1.2 comments.
CPVCS    
CPVCS       Rev 1.2   05 Mar 2007 16:31:18   gable
CPVCS    Modified formatted write statements so that double 
CPVCS    quote is not escaped by a backslash character.
CPVCS    It was working for Solaris but Absoft Linux did not like it.
CPVCS    
CPVCS       Rev 1.1   28 Nov 2006 09:59:22   gable
CPVCS    Initial version written by Daniel R Einstein, PNNL.
CPVCS    Some changes were made to correct ascii output option.
CPVCS    binary output option is turned off but the code has been
CPVCS    left in. This implementation only outputs node attributes,
CPVCS    itetclr is not output and no other element attributes are
CPVCS    output. prism and pyrimide elements are output as degenerate
CPVCS    hex elements since Tecplot does not support this element
CPVCS    type. There is no special Tecplot ioflag. Attributes are
CPVCS    controled with the AVS ioflag variable.
CPVCS    All attributes are output as DOUBLE, no attempt is made to
CPVCS    maintain integer attribute types.
CPVCS    
CPVCS       Rev 1.0   28 Nov 2006 09:57:40   gable
CPVCS    Initial revision.
C
C ######################################################################
      implicit none
      include "local_element.h"
C
C     The following are dummy functions since the tecio.a library
C     is not incorporated into LaGriT.
C
      integer*4 function TECDAT100
      integer*4 function TECNOD100
      integer*4 function TECINI100
      integer*4 function TECEND100
      integer*4 function TECZNE100
C
C     Take out the five function definitions above and uncomment
C     the line below to use the tecio.a library.
C
c      include "tecio.inc"
C
      character*132 logmess
      character ifile*(*), ifileini*(*)
C
      character*32 cmo
      character iotype*(*)
      pointer (ipimt1, imt1)
      pointer (ipitp1, itp1)
      pointer (ipicr1, icr1)
      pointer (ipisn1, isn1)
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      pointer (ipitetclr, itetclr)
      pointer (ipitettyp, itettyp)
      pointer (ipitetoff, itetoff)
      pointer (ipjtetoff, jtetoff)
      pointer (ipitet, itet)
      integer nsd, nen, idumptype
      integer imt1(*), itp1(*), icr1(*), isn1(*)
      real*8 xic(*), yic(*), zic(*)
      integer itetclr(*), itettyp(*),
     *        itetoff(*), jtetoff(*)
      pointer (ipisetwd, isetwd)
      integer isetwd(*)
      integer itet(*)
      integer  itypout
C
      pointer (ipxvalues, xvalues)
      real*8 xvalues(*)
      pointer (ipcmo_pointer, icmo_pointer)
      pointer (ipcmo_pointer, xcmo_pointer)
      integer icmo_pointer(*)
      REAL*8 xcmo_pointer(*)
      pointer (iadr,dat)
      real*8 dat(*)
C
      integer  nmcmoatt
      pointer (ipcnames,cnames)
      pointer (ipranks,iranks)
      pointer (ipidx,idx)
      pointer (ipidxe,idxe)
      pointer (ipoffs,ioffs)
      pointer (ipoffse,ioffse)
      pointer (iplengths,clengths)
      pointer (ipioflags,cioflags)
      character*32 cnames(*),clengths(*),cioflags(*),filename
      integer iranks(*),ioffs(*),ioffse(*),idx(*),idxe(*)
C
      character*32 isubname
      character*32  ctype , cinterp, crank,cpers
      real*8 a,b,c,time,dthydro,cutoff,maxval
      real*8 cinterpolate
      integer icharlnf
      integer ilen,ityp,ierr,ivoronoi2d,ivoronoi3d,mmlength,
     *  ierror_return,irank,lent,nvalues1,length,len2,i8,i7,
     *  i6,i5,i4,i3,i2,i1,index,it,j,iflag,len1,i,nvalues,
     *  icscode,ierror,iunit,iopt_points,iopt_elements,
     *  iopt_values_node,iopt_values_elem,
     *  mbndry,nelements,nnodes,ihcycle,nvaluese,lvalues,
     *  lvaluese,izero,k,irowlen,irowlene, ncolumn_max
      integer nnodes_io,nelements_io,nvalues_node,nvalues_elem
C
      character*132 vchlist
      character*132 var
      real*8 temp(*)
      pointer (iptemp,temp)

      integer*4 TECINI100, TECZNE100, TECDAT100, TECNOD100, TECEND100
      integer vchoff,lenchar,chstop,q,len
C
C ****************************************************************
C     TECPLOT RELATED DECLARATIONS
      CHARACTER*32 datapacking
      INTEGER*4    Debug,L,III,NPts,NElm,DIsDouble,VIsDouble
      INTEGER*4    IMax,JMax,KMax,ind   
      CHARACTER*1  NULCHAR
      INTEGER*4    Zero,zonetype
      POINTER      (NullPtr,Null)
      integer*4    null
C ****************************************************************
      pointer(ipNM,NMlin)
      pointer(ipNM,NMtri)
      pointer(ipNM,NMqud)
      pointer(ipNM,NMtet)
      pointer(ipNM,NMpyr)
      pointer(ipNM,NMpri)
      pointer(ipNM,NMhex)
      integer NMlin(2,*)
      integer NMtri(3,*)
      integer NMqud(4,*)
      integer NMtet(4,*)
      integer NMpyr(8,*)
      integer NMpri(8,*)
      integer NMhex(8,*)
      pointer(iptempxic,tempxic)
      pointer(iptempyic,tempyic)
      pointer(iptempzic,tempzic)
      pointer(ipmpary,mpary)
      pointer(ipinvmpary,invmpary)
      real*8 tempxic(*), tempyic(*), tempzic(*)
      integer invmpary(*), mpary(*)
      integer yespnt,yeslin,yestri,yesqud,yestet,yespyr,yespri,yeshex
      integer mnel,mnpo,node,start,stop
      integer if_reorder_tecplot
      integer num_node_att, num_elem_att
C    
C ****************************************************************
C
C     The debug parameter is 2 for maximum verbosity and 0 for no echo
      Debug     = 1
      VIsDouble = 1
      DIsDouble = 0
      NULCHAR   = CHAR(0)
      Zero      = 0
      NullPtr   = 0
      III=1
C
      data ivoronoi2d / 1 /
      data ivoronoi3d / 0 /
      data cutoff/1.e-30/
      data maxval/1.e+30/
      data izero/0/
C ######################################################################
C
      isubname='dumptecplot_hybrid'
C     SYNTAX: dump/tecplot/filename/cmoname/binary|ascii
C
      if(iotype(1:5).eq.'ascii')then
        idumptype=0
        len=icharlnf(ifile(1:icharlnf(ifile)))
        if(ifile(len-3:len).eq.'.plt')then
           filename=ifile(1:icharlnf(ifile))
        else
           filename=ifile(1:icharlnf(ifile)) // '.plt'
        endif
      else
        idumptype=1
        len=icharlnf(ifile(1:icharlnf(ifile)))
        if(ifile(len-3:len).eq.'.plt')then
           filename=ifile(1:icharlnf(ifile))
        else
           filename=ifile(1:icharlnf(ifile)) // '.plt'
        endif
      endif
      len=icharlnf(filename(1:icharlnf(filename)))
      
      if (idumptype .eq. 1)then
C
C     Turn off binary Tecplot output option
C
      idumptype=0
      write(logmess,'(a)')'WARNING: binary output not supported.'
      call writloga('default',0,logmess,0,icscode)
      write(logmess,'(a)')'WARNING: Converting to ascii output.'
      call writloga('default',0,logmess,0,icscode)
      write(logmess,'(a)')'WARNING: See dumptecplot_hybrid.f source for'
      call writloga('default',0,logmess,0,icscode)
      write(logmess,'(a)')'WARNING: instruction on compiling in tecio.a'
      call writloga('default',0,logmess,0,icscode)
      write(logmess,'(a)')'WARNING: library for binary TECPLOT output.'
      call writloga('default',0,logmess,0,icscode)
      endif
C
C ****************** GET INFORMATION FROM MESH OBJECT ******************
C
      call cmo_get_name(cmo,ierror)

      call cmo_get_info('isetwd',cmo,
     *                        ipisetwd,ilen,ityp,ierr)
      call cmo_get_info('imt1',cmo,ipimt1,ilen,ityp,ierr)
      call cmo_get_info('itp1',cmo,ipitp1,ilen,ityp,ierr)
      call cmo_get_info('icr1',cmo,ipicr1,ilen,ityp,ierr)
      call cmo_get_info('isn1',cmo,ipisn1,ilen,ityp,ierr)
      call cmo_get_info('xic',cmo,ipxic,ilen,ityp,ierr)
      call cmo_get_info('yic',cmo,ipyic,ilen,ityp,ierr)
      call cmo_get_info('zic',cmo,ipzic,ilen,ityp,ierr)
      call cmo_get_info('itetclr',cmo,
     *                  ipitetclr,ilen,ityp,ierr)
      call cmo_get_info('itettyp',cmo,
     *                  ipitettyp,ilen,ityp,ierr)
      call cmo_get_info('itetoff',cmo,
     *                  ipitetoff,ilen,ityp,ierr)
      call cmo_get_info('jtetoff',cmo,
     *                  ipjtetoff,ilen,ityp,ierr)
      call cmo_get_info('itet',cmo,ipitet,ilen,ityp,ierr)
      call cmo_get_info('nodes_per_element',cmo,nen,ilen,ityp,ierr)
      call cmo_get_info('nnodes',cmo,nnodes,ilen,ityp,ierr)
      call cmo_get_info('nelements',cmo,nelements,ilen,ityp,ierr)
C
C ****************************************************************
C
      if((nnodes .eq. 0) .and. (nelements .eq. 0))then
          write(logmess,'(a)')'WARNING: dumptecplot'
          call writloga('default',0,logmess,0,icscode)
          write(logmess,'(a)')'WARNING: nnodes=0 nelements = 0'
          call writloga('default',0,logmess,0,icscode)
          write(logmess,'(a)')'WARNING: No output'
          call writloga('default',0,logmess,0,icscode)
          return
      endif
C
C
C *********************************************************************
C     Count the number of tecplot fields to write
C
      call cmo_get_info('number_of_attributes',cmo,nmcmoatt,ilen,ityp,
     *   icscode)
C
      nvalues=0
      nvaluese=0
      lvalues=0
      lvaluese=0
      irowlen=0
      irowlene=0
C
C     ALWAYS INCLUDE COORDINATE DATA
      vchlist = 'X Y Z '
      vchoff = 6
C
C     Hardwire turning off output of -def- field.
      call dotaskx3d
     1 ('cmo/modatt/-def-/-def-/ioflag/x;finish',icscode)
      call mmgetblk('cnames',isubname,ipcnames,nmcmoatt*8,1,icscode)
      call mmgetblk('clengths',isubname,iplengths,nmcmoatt*8,1,icscode)
      call mmgetblk('iranks',isubname,ipranks,nmcmoatt,1,icscode)
      call mmgetblk('idx',isubname,ipidx,nmcmoatt,1,icscode)
      call mmgetblk('idxe',isubname,ipidxe,nmcmoatt,1,icscode)
      call mmgetblk('ioffs',isubname,ipoffs,nmcmoatt,1,icscode)
      call mmgetblk('ioffse',isubname,ipoffse,nmcmoatt,1,icscode)
      call mmgetblk('cioflags',isubname,ipioflags,nmcmoatt*8,1,icscode)
c
      ioffs(1)=0
      ioffse(1)=0
c
      do i=1,nmcmoatt
C
C        NAME FIELD
         call cmo_get_attribute_name(cmo,i,
     *                     cnames(i),icscode)
         call cmo_get_attparam(cnames(i),cmo,index,ctype,crank,
     *    clengths(i),cinterp,cpers,cioflags(i),ierror_return)
         call cmo_get_info(crank,cmo,iranks(i),ilen,ityp,icscode)
         len1=icharlnf(cioflags(i))
         iflag=0
         ioffs(1)=0
         do j=1,len1
            if(cioflags(i)(j:j).eq.'a'.and.
     *         clengths(i)(1:icharlnf(clengths(i))).eq.'nnodes') then
               iflag=1
               nvalues=nvalues+1
C
                  lenchar=icharlnf(cnames(i))
                  do k=1,lenchar
                     vchlist(k+vchoff:k+vchoff)=cnames(i)(k:k)
                  enddo
                  vchlist(lenchar+1+vchoff:lenchar+1+vchoff) = ' '
                  vchoff=vchoff+lenchar+1
C
               idx(nvalues)=i
               lvalues=lvalues+iranks(i)
               ioffs(i)=irowlen
               irowlen=irowlen+iranks(i)
            endif
         enddo
      enddo
C
C
C
C ######################## HEADER #########################
C
C #################### ASCII HEADER #######################
      if(idumptype.eq.0)then
         iunit=-1
         call hassign(iunit,filename(1:len),ierror)
         write(iunit,*) 'TITLE = "LaGriT GENERATED GRID"'
         var(1:1)='"'
         write(iunit,*)'VARIABLES = '
         start=2
         stop=2
         do i=1,vchoff
           if(vchlist(i:i).eq.' ')then
             var(stop:stop)='"'
             write(iunit,*)var(1:stop)
             var(1:1)='"'
             start=2
             stop=2
           else
             var(stop:stop)=vchlist(i:i)
             stop=stop+1
           endif
         enddo
      else
C #################### BINARY HEADER #######################
        I = TECINI100('LaGriT GENERATED GRID'//NULCHAR,
     &              vchlist(1:vchoff)//NULCHAR,
     &              filename(1:len)//NULCHAR,
     &              '.'//NULCHAR,
     &               Debug,
     &               VIsDouble)
      endif  
C
C ###########################################################
C
C     FROM A TEMPORARY STORAGE ARRAY FOR THE INTERESTING
C     SUBSET OF ATTRIBUTES. STORE BY TYPE AND RANK (E.G. 
C     REAL OR INT, SCALAR OR VECTOR).
C
      num_node_att = lvalues
      length=lvalues*nnodes
      if ( num_node_att .ne. 0 ) then
         length=lvalues*nnodes
         call mmgetblk('xvalues',isubname,ipxvalues,length,2,icscode)
      else
         length=nnodes
         call mmgetblk('xvalues',isubname,ipxvalues,length,2,icscode)
      endif
      length=nnodes
      call mmgetblk('temp',isubname,iptemp,length,2,icscode)
      nvalues1=0
      do i=1,nmcmoatt
         len1=icharlnf(cioflags(i))
         iflag=0
         do j=1,len1
            if(cioflags(i)(j:j).eq.'a'.and.
     *         clengths(i)(1:icharlnf(clengths(i))).eq.'nnodes') then
               iflag=1
               nvalues1=nvalues1+1
            endif
         enddo
         if(iflag.eq.1) then
            call cmo_get_attparam(cnames(i),cmo,index,ctype,crank,
     *      clengths(i),cinterp,cpers,cioflags(i),ierror_return)
            lent=icharlnf(ctype)
            if(ctype(1:lent).eq.'VINT') then
               call cmo_get_length(cnames(i),cmo,length,irank,
     *                                ierror_return)
               mmlength=irank*length
               call mmfindbk(cnames(i),
     *                          cmo,
     *                          ipcmo_pointer,mmlength,
     *                          ierror_return)
C
               if(ierror_return.ne.0) then
                  call x3d_error(isubname,'mmfindbk')
               else
                  if(length.eq.nnodes) then
                     do j=1,length
                       do k=1,irank
                         xvalues(ioffs(i)+irowlen*(j-1)+k)=
     *                     icmo_pointer(irank*(j-1)+k)
                       enddo
                     enddo
                  elseif(length.eq.nelements) then
C                        write(logmess,9100) cnames(i),
C     *                                      length,irank
 9100                format("Invalid length for TECPLOT variable: ",
     *                         a,' length=',i10,' rank=',i10)
C                    call x3d_error(isubname,'Illegal cmo_name')
                  else
                     write(logmess,9100) cnames(i),
     *                                   length,irank
                     call x3d_error(isubname,'Illegal cmo_name')
                  endif
               endif
            elseif(ctype(1:lent).eq.'VDOUBLE') then
               call cmo_get_length(cnames(i),cmo,length,irank,
     *                                ierror_return)
               mmlength=irank*length
               call mmfindbk(cnames(i),
     *                          cmo,
     *                          ipcmo_pointer,mmlength,
     *                          ierror_return)
               if(ierror_return.ne.0) then
                  call x3d_error(isubname,'mmfindbk')
               else
                  if(length.eq.nnodes) then
                     do j=1,length
                       do k=1,irank
                         xvalues(ioffs(i)+irowlen*(j-1)+k)=
     *                     cinterpolate('function',cinterp,
     *                     xcmo_pointer(irank*(j-1)+k))
                       enddo
                     enddo
                  elseif(length.eq.nelements) then
C                    write(logmess,9100) cnames(i),
C     *                                  length,irank
C                    call x3d_error(isubname,'Illegal cmo_name')
                  else
                     write(logmess,9100) cnames(i),
     *                                   length,irank
                     call x3d_error(isubname,'Illegal cmo_name')
                  endif
               endif
            endif
         endif
      enddo
C
C      
      yespnt=0
      yeslin=0
      yestri=0
      yesqud=0
      yestet=0
      yespyr=0
      yespri=0
      yeshex=0
C

      do it=1,nelements
        if(itettyp(it).eq.ifelmpnt) then
          yespnt=1
        elseif(itettyp(it).eq.ifelmlin) then
          yeslin=1
        elseif(itettyp(it).eq.ifelmtri) then
          yestri=1
        elseif(itettyp(it).eq.ifelmqud) then
          yesqud=1
        elseif(itettyp(it).eq.ifelmtet) then
          yestet=1
        elseif(itettyp(it).eq.ifelmpyr) then
          yespyr=1
        elseif(itettyp(it).eq.ifelmpri) then
          yespri=1
        elseif(itettyp(it).eq.ifelmhex) then
          yeshex=1
        endif
      enddo
C
C     This sum is greater than 1 if the mesh is 'hybrid'.
C      
      if_reorder_tecplot = 
     1     yespnt+yeslin+yestri+yesqud+yestet+yespyr+yespri+yeshex

C
C ########################## ALLOCATE #########################
        length=nnodes
        call mmgetblk('tempxic',isubname,iptempxic,length,2,icscode)
        call mmgetblk('tempyic',isubname,iptempyic,length,2,icscode)
        call mmgetblk('tempzic',isubname,iptempzic,length,2,icscode)
        call mmgetblk('mpary',isubname,ipmpary,length,1,icscode)
        call mmgetblk('invmpary',isubname,ipinvmpary,length,1,icscode)
C ######################## POINT TYPE #########################
        if(yespnt.eq.1)then
           write(logmess,'(a)')
     *         'NOT WRITING UNSUPPORTED POINT TYPES'
          call writloga('default',0,logmess,0,ierr)
        endif
C ################### LINE SEGEMENT TYPE ######################
      if(yeslin.eq.1)then
        mnel=0
        mnpo=0
        do j=1,nnodes
           invmpary(j)=0
           mpary(j)=0
        enddo
        do it=1,nelements
          if(itettyp(it).eq.ifelmlin)then
C           zonetype='FELINESEG'
            zonetype=1
            mnel=mnel+1
            do j=1,2
              node=itet(itetoff(it)+j)
              if(invmpary(node).eq.0)then
                mnpo=mnpo+1
                mpary(mnpo)=node
                invmpary(node)=mnpo
              endif
            enddo 
          endif
        enddo 
C
        if(if_reorder_tecplot .eq. 1)then
           do j=1,nnodes
              invmpary(j) = j
              mpary(j)    = j
           enddo
        endif
C
        NPts = mnpo 
        NElm = mnel 
        KMax = 1  
        if(idumptype.eq.0)then
          write(iunit,*)'ZONE T="LINE Zone"'
          write(iunit,*)'N=',mnpo,',',' E=',mnel,',',' ZONETYPE=',
     *                   'FELINESEG' 
          write(iunit,*)'DATAPACKING=', 'BLOCK'
          vchlist(1:25) ='DT=(DOUBLE DOUBLE DOUBLE '
          vchoff=26
          do i=1,nvalues
             vchlist(vchoff:vchoff+7)='DOUBLE '
             vchoff=vchoff+7
          enddo
          vchlist(vchoff-1:vchoff)=')'
          write(iunit,*)vchlist(1:vchoff)  
        else
        I = TECZNE100('LINE Zone'//NULCHAR,
     &              zonetype,
     &              NPts,
     &              NElm,
     &              KMax,
     &              0,
     &              0,
     &              0,
     &              1,
     &              0,
     &              0,
     &              Null,
     &              Null,
     &              0)
        endif
C
C       WRITE OUT COORDINATE DATA
        do j=1,mnpo
           tempxic(j)=xic(mpary(j))
           tempyic(j)=yic(mpary(j))
           tempzic(j)=zic(mpary(j))
        enddo
        if(idumptype.eq.0)then
          write(iunit,9040)(tempxic(i),i=1,mnpo)
          write(iunit,9040)(tempyic(i),i=1,mnpo)
          write(iunit,9040)(tempzic(i),i=1,mnpo)
 9040     format(5(1pe16.8))
        else
           III       = mnpo
           DIsDouble = 1
           I = TECDAT100(III,tempxic,DIsDouble)
           I = TECDAT100(III,tempyic,DIsDouble)
           I = TECDAT100(III,tempzic,DIsDouble)
        endif 
C
C       WRITE OUT ATTRIBUTE DATA
        do q=1,nvalues
          do j=1,mnpo
            temp(j)=xvalues(q+nvalues1*(mpary(j)-1))
          enddo
          if(idumptype.eq.0)then
            write(iunit,9040)(temp(i),i=1,mnpo)
          else
            I = TECDAT100(III,temp,DIsDouble)
          endif
        enddo
C
C       WRITE OUT NODAL CONNECTIVITY LIST
        length=2*mnel
        call mmgetblk('NMlin',isubname,ipNM,length,1,icscode)
        j=0
        do k=1,nelements
          if(itettyp(k).eq.ifelmlin)then
          j=j+1
            NMlin(1,j)=invmpary(itet(itetoff(k)+1))
            NMlin(2,j)=invmpary(itet(itetoff(k)+2))
          endif
        enddo
        if(idumptype.eq.0)then
          do i=1,mnel
            write(iunit,9041) NMlin(1,i),NMlin(2,i)
 9041       format(2(i10,' '))
          enddo
        else
          I = TECNOD100(NMlin)
        endif
        call mmrelblk('NMlin',isubname,ipNM,icscode)
        do j=1,nnodes
           tempxic(j)=0.0
           tempyic(j)=0.0
           tempzic(j)=0.0
           temp(j)=0.0
           mpary(j)=0
           invmpary(j)=0
        enddo
      endif
C
C ####################### TRIANGLE TYPE ##########################
      if(yestri.eq.1)then
        mnel=0
        mnpo=0
        do j=1,nnodes
           invmpary(j)=0
           mpary(j)=0
        enddo
        do it=1,nelements
          if(itettyp(it).eq.ifelmtri)then
C           zonetype='FETRIANGLE'
            zonetype=2
            mnel=mnel+1
            do j=1,3
              node=itet(itetoff(it)+j)
              if(invmpary(node).eq.0)then
                mnpo=mnpo+1
                mpary(mnpo)=node
                invmpary(node)=mnpo
              endif
            enddo 
          endif
        enddo 
C
        if(if_reorder_tecplot .eq. 1)then
           do j=1,nnodes
              invmpary(j) = j
              mpary(j)    = j
           enddo
        endif
C
        NPts = mnpo 
        NElm = mnel 
        KMax = 1  
        if(idumptype.eq.0)then
          write(iunit,*)'ZONE T="TRAINGLE Zone"'
          write(iunit,*)'N=',mnpo,',',' E=',mnel,',',' ZONETYPE=',
     *                   'FETRIANGLE' 
          write(iunit,*)'DATAPACKING=', 'BLOCK'
          vchlist(1:25) ='DT=(DOUBLE DOUBLE DOUBLE '
          vchoff=26
          do i=1,nvalues
             vchlist(vchoff:vchoff+7)='DOUBLE '
             vchoff=vchoff+7
          enddo
          vchlist(vchoff-1:vchoff)=')'
          write(iunit,*)vchlist(1:vchoff)  
        else
          I = TECZNE100('TRAINGLE Zone'//NULCHAR,
     &              zonetype,
     &              NPts,
     &              NElm,
     &              KMax,
     &              0,
     &              0,
     &              0,
     &              1,
     &              0,
     &              0,
     &              Null,
     &              Null,
     &              0)
        endif
C
C       WRITE OUT COORDINATE DATA
        do j=1,mnpo
           tempxic(j)=xic(mpary(j))
           tempyic(j)=yic(mpary(j))
           tempzic(j)=zic(mpary(j))
        enddo
        if(idumptype.eq.0)then
          write(iunit,9040)(tempxic(i),i=1,mnpo)
          write(iunit,9040)(tempyic(i),i=1,mnpo)
          write(iunit,9040)(tempzic(i),i=1,mnpo)
        else 
          III       = mnpo
          DIsDouble = 1
          I = TECDAT100(III,tempxic,DIsDouble)
          I = TECDAT100(III,tempyic,DIsDouble)
          I = TECDAT100(III,tempzic,DIsDouble)
        endif
C
C       WRITE OUT ATTRIBUTE DATA
        do q=1,nvalues
          do j=1,mnpo
            temp(j)=xvalues(q+nvalues1*(mpary(j)-1))
          enddo
          if(idumptype.eq.0)then
            write(iunit,9040)(temp(i),i=1,mnpo)
          else
            I = TECDAT100(III,temp,DIsDouble)
          endif
        enddo
C
C       WRITE OUT NODAL CONNECTIVITY LIST
        length=3*mnel
        call mmgetblk('NMtri',isubname,ipNM,length,1,icscode)
        j=0
        do k=1,nelements
          if(itettyp(k).eq.ifelmtri)then
            j=j+1
            NMtri(1,j)=invmpary(itet(itetoff(k)+1))
            NMtri(2,j)=invmpary(itet(itetoff(k)+2))
            NMtri(3,j)=invmpary(itet(itetoff(k)+3))
          endif
        enddo
        if(idumptype.eq.0)then
          do i=1,mnel
            write(iunit,9042) NMtri(1,i),NMtri(2,i),NMtri(3,i)
 9042       format(3(i10,' '))
          enddo
        else
          I = TECNOD100(NMtri)
        endif
        call mmrelblk('NMtri',isubname,ipNM,icscode)
        do j=1,nnodes
           tempxic(j)=0.0
           tempyic(j)=0.0
           tempzic(j)=0.0
           temp(j)=0.0
           mpary(j)=0
           invmpary(j)=0
        enddo
      endif
C
C ####################### QUAD TYPE ##########################
      if(yesqud.eq.1)then
        mnel=0
        mnpo=0
        do j=1,nnodes
          invmpary(j)=0
          mpary(j)=0
        enddo
        do it=1,nelements
          if(itettyp(it).eq.ifelmqud)then
C           zonetype='FEQUAD'
            zonetype=3
            mnel=mnel+1
            do j=1,4
              node=itet(itetoff(it)+j)
              if(invmpary(node).eq.0)then
                mnpo=mnpo+1
                mpary(mnpo)=node
                invmpary(node)=mnpo
              endif
            enddo 
          endif
        enddo 
C
        if(if_reorder_tecplot .eq. 1)then
           do j=1,nnodes
              invmpary(j) = j
              mpary(j)    = j
           enddo
        endif
C
        NPts = mnpo 
        NElm = mnel 
        KMax = 1  
        if(idumptype.eq.0)then
          write(iunit,*)'ZONE T="QUAD Zone"'
          write(iunit,*)'N=',mnpo,',',' E=',mnel,',',' ZONETYPE=',
     *                   'FEQUADRILATERAL' 
          write(iunit,*)'DATAPACKING=', 'BLOCK'
          vchlist(1:25) ='DT=(DOUBLE DOUBLE DOUBLE '
          vchoff=26
          do i=1,nvalues
             vchlist(vchoff:vchoff+7)='DOUBLE '
             vchoff=vchoff+7
          enddo
          vchlist(vchoff-1:vchoff)=')'
          write(iunit,*)vchlist(1:vchoff)  
        else
        I = TECZNE100('QUAD Zone'//NULCHAR,
     &              zonetype,
     &              NPts,
     &              NElm,
     &              KMax,
     &              0,
     &              0,
     &              0,
     &              1,
     &              0,
     &              0,
     &              Null,
     &              Null,
     &              0)
       endif
C
C       WRITE OUT COORDINATE DATA
        do j=1,mnpo
           tempxic(j)=xic(mpary(j))
           tempyic(j)=yic(mpary(j))
           tempzic(j)=zic(mpary(j))
        enddo
        if(idumptype.eq.0)then
          write(iunit,9040)(tempxic(i),i=1,mnpo)
          write(iunit,9040)(tempyic(i),i=1,mnpo)
          write(iunit,9040)(tempzic(i),i=1,mnpo)
        else
          III       = mnpo
          DIsDouble = 1
          I = TECDAT100(III,tempxic,DIsDouble)
          I = TECDAT100(III,tempyic,DIsDouble)
          I = TECDAT100(III,tempzic,DIsDouble)
        endif
C
C       WRITE OUT ATTRIBUTE DATA
        do q=1,nvalues
          do j=1,mnpo
            temp(j)=xvalues(q+nvalues1*(mpary(j)-1))
          enddo
          if(idumptype.eq.0)then
            write(iunit,9040)(temp(i),i=1,mnpo)
          else
            I = TECDAT100(III,temp,DIsDouble)
          endif
        enddo
C
C       WRITE OUT NODAL CONNECTIVITY LIST
        length=4*mnel
        call mmgetblk('NMqud',isubname,ipNM,length,1,icscode)
        j=0
        do k=1,nelements
          if(itettyp(k).eq.ifelmqud)then
            j=j+1
            NMqud(1,j)=invmpary(itet(itetoff(k)+1))
            NMqud(2,j)=invmpary(itet(itetoff(k)+2))
            NMqud(3,j)=invmpary(itet(itetoff(k)+3))
            NMqud(4,j)=invmpary(itet(itetoff(k)+4))
          endif
        enddo
        if(idumptype.eq.0)then
          do i=1,mnel
            write(iunit,9043)NMqud(1,i),NMqud(2,i),NMqud(3,i),
     *                    NMqud(4,i)
 9043       format(4(i10,' '))
          enddo
        else
          I = TECNOD100(NMqud)
        endif
        call mmrelblk('NMqud',isubname,ipNM,icscode)
        do j=1,nnodes
           tempxic(j)=0.0
           tempyic(j)=0.0
           tempzic(j)=0.0
           temp(j)=0.0
           mpary(j)=0
           invmpary(j)=0
        enddo
      endif
C
C ####################### TETRAHEDRAL TYPE ##########################
      if(yestet.eq.1)then
        mnel=0
        mnpo=0
        do j=1,nnodes
           invmpary(j)=0
           mpary(j)=0
        enddo
        do it=1,nelements
          if(itettyp(it).eq.ifelmtet)then
C           zonetype='FETETRAHEDRON'
            zonetype=4
            mnel=mnel+1
            do j=1,4
              node=itet(itetoff(it)+j)
              if(invmpary(node).eq.0)then
                mnpo=mnpo+1
                mpary(mnpo)=node
                invmpary(node)=mnpo
              endif
            enddo 
          endif
        enddo 
C
        if(if_reorder_tecplot .eq. 1)then
           do j=1,nnodes
              invmpary(j) = j
              mpary(j)    = j
           enddo
        endif
C
        NPts = mnpo 
        NElm = mnel 
        KMax = 1  
        if(idumptype.eq.0)then
          write(iunit,*)'ZONE T="TETRAHEDRON Zone"'
          write(iunit,*)'N=',mnpo,',',' E=',mnel,',',' ZONETYPE=',
     *                   'FETETRAHEDRON' 
          write(iunit,*)'DATAPACKING=', 'BLOCK'
          vchlist(1:25) ='DT=(DOUBLE DOUBLE DOUBLE '
          vchoff=26
          do i=1,nvalues
             vchlist(vchoff:vchoff+7)='DOUBLE '
             vchoff=vchoff+7
          enddo
          vchlist(vchoff-1:vchoff)=')'
          write(iunit,*)vchlist(1:vchoff)  
          vchoff=0
        else
        I = TECZNE100('TETRA Zone'//NULCHAR,
     &              zonetype,
     &              NPts,
     &              NElm,
     &              KMax,
     &              0,
     &              0,
     &              0,
     &              1,
     &              0,
     &              0,
     &              Null,
     &              Null,
     &              0)
        endif
C
C       WRITE OUT COORDINATE DATA
        do j=1,mnpo
           tempxic(j)=xic(mpary(j))
           tempyic(j)=yic(mpary(j))
           tempzic(j)=zic(mpary(j))
        enddo
        if(idumptype.eq.0)then
          write(iunit,9040)(tempxic(i),i=1,mnpo)
          write(iunit,9040)(tempyic(i),i=1,mnpo)
          write(iunit,9040)(tempzic(i),i=1,mnpo)
        else
          III       = mnpo
          DIsDouble = 1
          I = TECDAT100(III,tempxic,DIsDouble)
          I = TECDAT100(III,tempyic,DIsDouble)
          I = TECDAT100(III,tempzic,DIsDouble)
        endif
C
C       WRITE OUT ATTRIBUTE DATA
        do q=1,nvalues
          do j=1,mnpo
            temp(j)=xvalues(q+nvalues1*(mpary(j)-1))
          enddo
          if(idumptype.eq.0)then
            write(iunit,9040)(temp(i),i=1,mnpo)
          else
            I = TECDAT100(III,temp,DIsDouble)
          endif
        enddo
C
C       WRITE OUT NODAL CONNECTIVITY LIST
        length=4*mnel
        call mmgetblk('NMtet',isubname,ipNM,length,1,icscode)
        j=0
        do k=1,nelements
          if(itettyp(k).eq.ifelmtet)then
            j=j+1
            NMtet(1,j)=invmpary(itet(itetoff(k)+1))
            NMtet(2,j)=invmpary(itet(itetoff(k)+2))
            NMtet(3,j)=invmpary(itet(itetoff(k)+3))
            NMtet(4,j)=invmpary(itet(itetoff(k)+4))
          endif
        enddo
        if(idumptype.eq.0)then
          do i=1,mnel
            write(iunit,9044) NMtet(1,i),NMtet(2,i),NMtet(3,i),
     *                        NMtet(4,i)
 9044       format(4(i10,' '))
          enddo
        else
          I = TECNOD100(NMtet)
        endif
        call mmrelblk('NMtet',isubname,ipNM,icscode)
        do j=1,nnodes
           tempxic(j)=0.0
           tempyic(j)=0.0
           tempzic(j)=0.0
           temp(j)=0.0
           mpary(j)=0
           invmpary(j)=0
        enddo
      endif
C
C ####################### PYRAMID TYPE ##########################
      if(yespyr.eq.1)then
        mnel=0
        mnpo=0
        do j=1,nnodes
           invmpary(j)=0
           mpary(j)=0
        enddo
        do it=1,nelements
          if(itettyp(it).eq.ifelmpyr)then
C           zonetype='FEPYRAMID'
            zonetype=5
            mnel=mnel+1
            do j=1,5
              node=itet(itetoff(it)+j)
              if(invmpary(node).eq.0)then
                mnpo=mnpo+1
                mpary(mnpo)=node
                invmpary(node)=mnpo
              endif
            enddo 
          endif
        enddo 
C
        if(if_reorder_tecplot .eq. 1)then
           do j=1,nnodes
              invmpary(j) = j
              mpary(j)    = j
           enddo
        endif
C
        NPts = mnpo 
        NElm = mnel 
        KMax = 1  
        if(idumptype.eq.0)then
          write(iunit,*)'ZONE T="PYRAMID (Degenerate HEX) Zone"'
          write(iunit,*)'N=',mnpo,',',' E=',mnel,',',' ZONETYPE=',
     *                   'FEBRICK' 
          write(iunit,*)'DATAPACKING=', 'BLOCK'
          vchlist(1:25) ='DT=(DOUBLE DOUBLE DOUBLE '
          vchoff=26
          do i=1,nvalues
             vchlist(vchoff:vchoff+7)='DOUBLE '
             vchoff=vchoff+7
          enddo
          vchlist(vchoff-1:vchoff)=')'
          write(iunit,*)vchlist(1:vchoff)  
        else
          I = TECZNE100('PYRAMID Zone'//NULCHAR,
     &              zonetype,
     &              NPts,
     &              NElm,
     &              KMax,
     &              0,
     &              0,
     &              0,
     &              1,
     &              0,
     &              0,
     &              Null,
     &              Null,
     &              0)
        endif
C
C       WRITE OUT COORDINATE DATA
        do j=1,mnpo
           tempxic(j)=xic(mpary(j))
           tempyic(j)=yic(mpary(j))
           tempzic(j)=zic(mpary(j))
        enddo
        if(idumptype.eq.0)then
          write(iunit,9040)(tempxic(i),i=1,mnpo)
          write(iunit,9040)(tempyic(i),i=1,mnpo)
          write(iunit,9040)(tempzic(i),i=1,mnpo)
        else
          III       = mnpo
          DIsDouble = 1
          I = TECDAT100(III,tempxic,DIsDouble)
          I = TECDAT100(III,tempyic,DIsDouble)
          I = TECDAT100(III,tempzic,DIsDouble)
        endif
C
C       WRITE OUT ATTRIBUTE DATA
        do q=1,nvalues
          do j=1,mnpo
            temp(j)=xvalues(q+nvalues1*(mpary(j)-1))
          enddo
          if(idumptype.eq.0)then
            write(iunit,9040)(temp(i),i=1,mnpo)
          else
            I = TECDAT100(III,temp,DIsDouble)
          endif
        enddo
C
C       WRITE OUT NODAL CONNECTIVITY LIST
        length=8*mnel
        call mmgetblk('NMpyr',isubname,ipNM,length,1,icscode)
        j=0
        do k=1,nelements
          if(itettyp(k).eq.ifelmpyr)then
            j=j+1
            NMpyr(1,j)=invmpary(itet(itetoff(k)+1))
            NMpyr(2,j)=invmpary(itet(itetoff(k)+1))
            NMpyr(3,j)=invmpary(itet(itetoff(k)+1))
            NMpyr(4,j)=invmpary(itet(itetoff(k)+1))
            NMpyr(5,j)=invmpary(itet(itetoff(k)+2))
            NMpyr(6,j)=invmpary(itet(itetoff(k)+3))
            NMpyr(7,j)=invmpary(itet(itetoff(k)+4))
            NMpyr(8,j)=invmpary(itet(itetoff(k)+5))
          endif
        enddo
        if(idumptype.eq.0)then
          do i=1,mnel
            write(iunit,9045) NMpyr(1,i),NMpyr(2,i),NMpyr(3,i),
     *         NMpyr(4,i),NMpyr(5,i),NMpyr(6,i),NMpyr(7,i),NMpyr(8,i)
 9045       format(8(i10,' '))
          enddo
        else
          I = TECNOD100(NMpyr)
        endif
        call mmrelblk('NMpyr',isubname,ipNM,icscode)
        do j=1,nnodes
           tempxic(j)=0.0
           tempyic(j)=0.0
           tempzic(j)=0.0
           temp(j)=0.0
           mpary(j)=0
           invmpary(j)=0
        enddo
      endif
C
C ####################### PRISM TYPE ##########################
      if(yespri.eq.1)then
        mnel=0
        mnpo=0
        do j=1,nnodes
          invmpary(j)=0
          mpary(j)=0
        enddo
        do it=1,nelements
          if(itettyp(it).eq.ifelmpri)then
C           zonetype='FEPPRISM'
            zonetype=5
            mnel=mnel+1
            do j=1,6
              node=itet(itetoff(it)+j)
              if(invmpary(node).eq.0)then
                mnpo=mnpo+1
                mpary(mnpo)=node
                invmpary(node)=mnpo
              endif
            enddo 
          endif
        enddo 
C
        if(if_reorder_tecplot .eq. 1)then
           do j=1,nnodes
              invmpary(j) = j
              mpary(j)    = j
           enddo
        endif
C
        NPts = mnpo 
        NElm = mnel 
        KMax = 1  
        if(idumptype.eq.0)then
          write(iunit,*)'ZONE T="PRISM (Degenerate HEX) Zone"'
          write(iunit,*)'N=',mnpo,',',' E=',mnel,',',' ZONETYPE=',
     *                   'FEBRICK' 
          write(iunit,*)'DATAPACKING=', 'BLOCK'
          vchlist(1:25) ='DT=(DOUBLE DOUBLE DOUBLE '
          vchoff=26
          do i=1,nvalues
             vchlist(vchoff:vchoff+7)='DOUBLE '
             vchoff=vchoff+7
          enddo
          vchlist(vchoff-1:vchoff)=')'
          write(iunit,*)vchlist(1:vchoff)  
        else
          I = TECZNE100('PRISM Zone'//NULCHAR,
     &              zonetype,
     &              NPts,
     &              NElm,
     &              KMax,
     &              0,
     &              0,
     &              0,
     &              1,
     &              0,
     &              0,
     &              Null,
     &              Null,
     &              0)
        endif
C
C       WRITE OUT COORDINATE DATA
        do j=1,mnpo
           tempxic(j)=xic(mpary(j))
           tempyic(j)=yic(mpary(j))
           tempzic(j)=zic(mpary(j))
        enddo
        if(idumptype.eq.0)then
          write(iunit,9040)(tempxic(i),i=1,mnpo)
          write(iunit,9040)(tempyic(i),i=1,mnpo)
          write(iunit,9040)(tempzic(i),i=1,mnpo)
        else
          III       = mnpo
          DIsDouble = 1
          I = TECDAT100(III,tempxic,DIsDouble)
          I = TECDAT100(III,tempyic,DIsDouble)
          I = TECDAT100(III,tempzic,DIsDouble)
        endif
C
C       WRITE OUT ATTRIBUTE DATA
        do q=1,nvalues
          do j=1,mnpo
            temp(j)=xvalues(q+nvalues1*(mpary(j)-1))
          enddo
          if(idumptype.eq.0)then
            write(iunit,9040)(temp(i),i=1,mnpo)
          else
            I = TECDAT100(III,temp,DIsDouble)
          endif
        enddo
C
C       WRITE OUT NODAL CONNECTIVITY LIST
        length=8*mnel
        call mmgetblk('NMpri',isubname,ipNM,length,1,icscode)
        j=0
        do k=1,nelements
          if(itettyp(k).eq.ifelmpri)then
            j=j+1
            NMpri(1,j)=invmpary(itet(itetoff(k)+1))
            NMpri(2,j)=invmpary(itet(itetoff(k)+1))
            NMpri(3,j)=invmpary(itet(itetoff(k)+1))
            NMpri(4,j)=invmpary(itet(itetoff(k)+2))
            NMpri(5,j)=invmpary(itet(itetoff(k)+3))
            NMpri(6,j)=invmpary(itet(itetoff(k)+4))
            NMpri(7,j)=invmpary(itet(itetoff(k)+5))
            NMpri(8,j)=invmpary(itet(itetoff(k)+6))
          endif
        enddo
        if(idumptype.eq.0)then
          do i=1,mnel
            write(iunit,9046)NMpri(1,i),NMpri(2,i),NMpri(3,i),
     *         NMpri(4,i),NMpri(5,i),NMpri(6,i),NMpri(7,i),NMpri(8,i)
 9046       format(8(i10,' '))
          enddo
        else
          I = TECNOD100(NMpri)
        endif
        call mmrelblk('NMpri',isubname,ipNM,icscode)
        do j=1,nnodes
           tempxic(j)=0.0
           tempyic(j)=0.0
           tempzic(j)=0.0
           temp(j)=0.0
           mpary(j)=0
           invmpary(j)=0
        enddo
      endif
C
C ####################### HEX TYPE ##########################
      if(yeshex.eq.1)then
        do j=1,nnodes
               invmpary(j)=0
               mpary(j)=0
            enddo
        mnel=0
        mnpo=0
        do it=1,nelements
          if(itettyp(it).eq.ifelmhex)then
C           zonetype='FEHEX'
            zonetype=5
            mnel=mnel+1
            do j=1,8
              node=itet(itetoff(it)+j)
              if(invmpary(node).eq.0)then
                mnpo=mnpo+1
                mpary(mnpo)=node
                invmpary(node)=mnpo
              endif
            enddo 
          endif
        enddo 
C
        if(if_reorder_tecplot .eq. 1)then
           do j=1,nnodes
              invmpary(j) = j
              mpary(j)    = j
           enddo
        endif
C
        NPts = mnpo 
        NElm = mnel 
        KMax = 1  
        if(idumptype.eq.0)then
          write(iunit,*)'ZONE T="HEX Zone"'
          write(iunit,*)'N=',mnpo,',',' E=',mnel,',',' ZONETYPE=',
     *                   'FEBRICK' 
          write(iunit,*)'DATAPACKING=', 'BLOCK'
          vchlist(1:25) ='DT=(DOUBLE DOUBLE DOUBLE '
          vchoff=26
          do i=1,nvalues
             vchlist(vchoff:vchoff+7)='DOUBLE '
             vchoff=vchoff+7
          enddo
          vchlist(vchoff-1:vchoff)=')'
          write(iunit,*)vchlist(1:vchoff)  
        else
        I = TECZNE100('HEX Zone'//NULCHAR,
     &              zonetype,
     &              NPts,
     &              NElm,
     &              KMax,
     &              0,
     &              0,
     &              0,
     &              1,
     &              0,
     &              0,
     &              Null,
     &              Null,
     &              0)
        endif
C
C       WRITE OUT COORDINATE DATA
        do j=1,mnpo
           tempxic(j)=xic(mpary(j))
           tempyic(j)=yic(mpary(j))
           tempzic(j)=zic(mpary(j))
        enddo
        if(idumptype.eq.0)then
          write(iunit,9040)(tempxic(i),i=1,mnpo)
          write(iunit,9040)(tempyic(i),i=1,mnpo)
          write(iunit,9040)(tempzic(i),i=1,mnpo)
        else
          III       = mnpo
          DIsDouble = 1
          I = TECDAT100(III,tempxic,DIsDouble)
          I = TECDAT100(III,tempyic,DIsDouble)
          I = TECDAT100(III,tempzic,DIsDouble)
        endif
C
C       WRITE OUT ATTRIBUTE DATA
        do q=1,nvalues
          do j=1,mnpo
            temp(j)=xvalues(q+nvalues1*(mpary(j)-1))
          enddo
          if(idumptype.eq.0)then
            write(iunit,9040)(temp(i),i=1,mnpo)
          else
            I = TECDAT100(III,temp,DIsDouble)
          endif
        enddo
C
C       WRITE OUT NODAL CONNECTIVITY LIST
        length=8*mnel
        call mmgetblk('NMhex',isubname,ipNM,length,1,icscode)
        j=0
        do k=1,nelements
          if(itettyp(k).eq.ifelmhex)then
            j=j+1
            NMhex(1,j)=invmpary(itet(itetoff(k)+1))
            NMhex(2,j)=invmpary(itet(itetoff(k)+2))
            NMhex(3,j)=invmpary(itet(itetoff(k)+3))
            NMhex(4,j)=invmpary(itet(itetoff(k)+4))
            NMhex(5,j)=invmpary(itet(itetoff(k)+5))
            NMhex(6,j)=invmpary(itet(itetoff(k)+6))
            NMhex(7,j)=invmpary(itet(itetoff(k)+7))
            NMhex(8,j)=invmpary(itet(itetoff(k)+8))
          endif
        enddo
        if(idumptype.eq.0)then
          do i=1,mnel
            write(iunit,9047)NMhex(1,i),NMhex(2,i),NMhex(3,i),
     *          NMhex(4,i),NMhex(5,i),NMhex(6,i),NMhex(7,i),NMhex(8,i)
 9047       format(8(i10,' '))
          enddo
        else
          I = TECNOD100(NMhex)
        endif
        call mmrelblk('NMhex',isubname,ipNM,icscode)
        do j=1,nnodes
           tempxic(j)=0.0
           tempyic(j)=0.0
           tempzic(j)=0.0
           temp(j)=0.0
           mpary(j)=0
           invmpary(j)=0
        enddo
      endif
C
      if(idumptype.eq.0)then
        close(iunit)
      else
        I = TECEND100() 
      endif
C
 9999 call mmrelprt(isubname,icscode)
C
      return
      end
      function TECDAT100(a01,a02,a03)
      implicit none
      integer*4 TECDAT100
      real*8 a01,a02,a03
      integer ierrw
      call writloga('default',0,'TECDAT100',0,ierrw)
      call writloga('default',0,
     1 'Binary IO not supported, see dumptecplot_hybrid.f',0,ierrw)
      call writloga('default',0,
     1 'for instructions on how to enable binary IO.',0,ierrw)
      TECDAT100 = 0
      return
      end
      function TECNOD100(a01)
      implicit none
      integer*4 TECNOD100
      real*8 a01,a02,a03
      integer ierrw
      call writloga('default',0,'TECNOD100',0,ierrw)
      call writloga('default',0,
     1 'Binary IO not supported, see dumptecplot_hybrid.f',0,ierrw)
      call writloga('default',0,
     1 'for instructions on how to enable binary IO.',0,ierrw)
      TECNOD100 = 0
      return
      end
      function TECINI100(a01,a02,a03,a04,a05,a06)
      implicit none
      integer*4 TECINI100
      real*8 a01,a02,a03,a04,a05,a06
      integer ierrw
      call writloga('default',0,'TECINI100',0,ierrw)
      call writloga('default',0,
     1 'Binary IO not supported, see dumptecplot_hybrid.f',0,ierrw)
      call writloga('default',0,
     1 'for instructions on how to enable binary IO.',0,ierrw)
      TECINI100 = 0
      return
      end
      function TECEND100()
      implicit none
      integer*4 TECEND100
      integer ierrw
      call writloga('default',0,'TECEND100',0,ierrw)
      call writloga('default',0,
     1 'Binary IO not supported, see dumptecplot_hybrid.f',0,ierrw)
      call writloga('default',0,
     1 'for instructions on how to enable binary IO.',0,ierrw)
      TECEND100 = 0
      return
      end
      function TECZNE100(a01,a02,a03,a04,a05,a06,a07,
     1                   a08,a09,a10,a11,a12,a13,a14)
      implicit none
      integer*4 TECZNE100
      real*8 a01,a02,a03,a04,a05,a06,a07,a08,a09,a10,a11,a12,a13,a14
      integer ierrw
      call writloga('default',0,'TECZNE100',0,ierrw)
      call writloga('default',0,
     1 'Binary IO not supported, see dumptecplot_hybrid.f',0,ierrw)
      call writloga('default',0,
     1 'for instructions on how to enable binary IO.',0,ierrw)
      TECZNE100 = 0
      return
      end
      
