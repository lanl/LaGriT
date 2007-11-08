      subroutine dump_geometries_lg(iunit,iomode,ierror)
C
C
C#######################################################################
C
C     PURPOSE -
C
C    This routine writes the geometry information, surfaces,
C    regions and mregions to a file
C
C
C     INPUT ARGUMENTS -
C
C     iunit  unit number of file
C
C     OUTPUT ARGUMENTS -
C
C
C
C     CHANGE HISTORY -
C
C        $Log:   /pvcs.config/t3d/src/dump_geometries_lg_nosb.f_a  $
CPVCS    
CPVCS       Rev 1.3   22 Mar 2002 16:41:42   dcg
CPVCS    allow for a mesh object with no geometry
CPVCS    
CPVCS       Rev 1.2   07 Feb 2000 16:49:22   dcg

C#######################################################################
C
      implicit none
c
      include 'geom_lg.h'
c
      integer iunit,i,j,ns,nr,nm,ls,lr,lm,len,ierror,length,
     *   icscode
      character *(*) iomode
      character*32 isubname,partname

C
      isubname='dumplggm'
      ierror=0

c
c  write out active info then list of geometry names
c  and general info for each geom
c
      partname='geom_lg'
      call mmfindbk ('geom_names',partname,
     *  ipgeom_names,len,ierror)
      call mmfindbk ('geom_info',partname,
     *  ipgeom_info,len,ierror)
      if(iomode(1:5).eq.'ascii') then
         write(iunit,10) number_of_geometries,nsurf,nregs,
     *   nmregs,maxdef,maxmdef,lastregdef,lastmregdef,
     *   lastsparam
         write(iunit,12)(geom_names(i),i=1,number_of_geometries)
         do i=1,number_of_geometries
           write(iunit,10)(geom_info(j,i),j=1,8)
         enddo
 10      format(9i10)
 12      format(4a32)

      else
         write(iunit) number_of_geometries,nsurf,nregs,
     *   nmregs,maxdef,maxmdef,lastregdef,lastmregdef,
     *   lastsparam,(geom_names(i),i=1,number_of_geometries),
     *   ((geom_info(j,i),j=1,8),
     *     i=1,number_of_geometries)
      endif
c
c  loop through the geometries
c
      do i=1,number_of_geometries
      partname=geom_names(i)
      if(i.ne.number_of_geometries) then
         ns=geom_info(1,i)
         nr=geom_info(2,i)
         nm=geom_info(3,i)
         lr=geom_info(6,i)
         lm=geom_info(7,i)
         ls=geom_info(8,i)
      else
         ns=nsurf
         nr=nregs 
         nm=nmregs
         lr=lastregdef
         lm=lastmregdef
         ls=lastsparam
      endif
c
c surface info for this geom - name, type, boundary type,
c definition, offset to definition
c
      call mmfindbk('csall',partname,ipcsall,length,ierror)
      if(ierror.ne.0) ns=0
      call mmfindbk('istype',partname,ipistype,length,ierror)
      call mmfindbk('ibtype',partname,ipibtype,length,ierror)
      call mmfindbk('sheetnm',partname,ipsheetnm,length,ierror)
      call mmfindbk('surfparam',partname,ipsurfparam,length,ierror)
      call mmfindbk('offsparam',partname,ipoffsparam,length,ierror)
      if(iomode(1:5).eq.'ascii') then
         if(ns.gt.0) then
            write(iunit,12) (csall(j),j=1,ns) 
            write(iunit,12) (istype(j),j=1,ns) 
            write(iunit,12) (ibtype(j),j=1,ns) 
            write(iunit,12) (sheetnm(j),j=1,ns) 
            write(iunit,20) (surfparam(j),j=1,ls) 
 20         format(6e20.13)
            write(iunit,10) (offsparam(j),j=1,ns)
         endif
      else
         if(ns.gt.0) then
            write(iunit)(csall(j),j=1,ns),(istype(j),j=1,ns),
     *      (ibtype(j),j=1,ns), (sheetnm(j),j=1,ns),
     *      (surfparam(j),j=1,ls),(offsparam(j),j=1,ns)
         else
            write(iunit)
         endif
      endif


C
C  write region storage blocks - name , offset to definition,
c  length of definition, definitions
c
      call mmfindbk('cregs',partname,ipcregs,length,ierror)
      call mmfindbk('offregdef',partname,ipoffregdef,length,ierror)
      call mmfindbk('ndefregs',partname,ipndefregs,length,ierror)
      call mmfindbk('regdef',partname,ipregdef,length,ierror)
      if(iomode(1:5).eq.'ascii') then
         if(nr.gt.0) then
            write(iunit,12) (cregs(j),j=1,nr)
            write(iunit,10) (offregdef(j),j=1,nr)
            write(iunit,10) (ndefregs(j),j=1,nr) 
            write(iunit,12) (regdef(j),j=1,lr) 
         endif
      else
         if(ns.gt.0)
     *     write(iunit)(cregs(j),j=1,nr),(offregdef(j),j=1,nr),
     *    (ndefregs(j),j=1,nr), (regdef(j),j=1,lr)   
      endif

C
C  write mregion storage blocks
      call mmfindbk('cmregs',partname,ipcmregs,length,ierror)
      call mmfindbk('offmregdef',partname,ipoffmregdef,length,ierror)
      call mmfindbk('ndefmregs',partname,ipndefmregs,length,ierror)
      call mmfindbk('mregdef',partname,ipmregdef,length,ierror)
      call mmfindbk('matregs',partname,ipmatregs,length,ierror)
      if(iomode(1:5).eq.'ascii') then
         if(nm.gt.0) then
            write(iunit,12) (cmregs(j),j=1,nm)
            write(iunit,10) (offmregdef(j),j=1,nm)
            write(iunit,10) (ndefmregs(j),j=1,nm) 
            write(iunit,12) (mregdef(j),j=1,lm) 
            write(iunit,10) (matregs(j),j=1,nm) 
         endif
      else
         if(nm.gt.0)
     *    write(iunit)(cmregs(j),j=1,nm),(offmregdef(j),j=1,nm),
     *    (ndefmregs(j),j=1,nm), (mregdef(j),j=1,lm),
     *    (matregs(j),j=1,nm)   
      endif
      enddo
         
C
 9999 call mmrelprt(isubname,icscode)
      return
      end
 
C
