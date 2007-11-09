      subroutine read_geometries_lg(iunit,iomode,ierror)
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
C        $Log:   /pvcs.config/t3d/src/read_geometries_lg_nosb.f_a  $
CPVCS    
CPVCS       Rev 1.5   15 May 2002 11:01:26   dcg
CPVCS    read a blank record if number of surfaces is zero
CPVCS    this matches the blank write of dump_geometries when type = binary
CPVCS
CPVCS       Rev 1.4   22 Mar 2002 16:42:16   dcg
CPVCS    allow for a mesh object with no geometry
CPVCS
CPVCS       Rev 1.3   Fri Apr 07 16:27:08 2000   dcg
CPVCS    allow for files with no geometry
CPVCS
CPVCS       Rev 1.0   Fri Apr 07 14:18:30 2000   dcg
CPVCS    Initial revision.
CPVCS
CPVCS       Rev 1.2   Mon Feb 14 08:47:36 2000   dcg
CPVCS    use number of mregions in mmgetblk calls where needed
CPVCS
CPVCS       Rev 1.1   07 Feb 2000 16:54:12   dcg
 
C#######################################################################
C
      implicit none
c
      include 'geom_lg.h'
c
      integer iunit,i,j,ns,nr,nm,ls,lr,lm,len,ierror,length,
     *   icscode,lrlen,lmlen,nmlen,nrlen,nslen,lslen
      character *(*) iomode
      character*32 isubname,partname
 
C
      isubname='dumplggm'
      ierror=0
c
c  read in active info then list of geometry names
c  and general info for each geom
c
      partname='geom_lg'
      call mmfindbk ('geom_names',partname,
     *  ipgeom_names,len,ierror)
      if(ierror.ne.0) call mmgetblk('geom_names',partname,
     *  ipgeom_names,number_of_geometries,3,ierror)
      call mmfindbk ('geom_info',partname,
     *  ipgeom_info,len,ierror)
      if(ierror.ne.0) call mmgetblk('geom_info',partname,
     *  ipgeom_names,8*number_of_geometries,1,ierror)
      if(iomode(1:5).eq.'ascii') then
         read(iunit,10) number_of_geometries,nsurf,nregs,
     *   nmregs,maxdef,maxmdef,lastregdef,lastmregdef,
     *   lastsparam
         read(iunit,12)(geom_names(i),i=1,number_of_geometries)
         do i=1,number_of_geometries
           read(iunit,10) (geom_info(j,i),j=1,8)
         enddo
 10      format(9i10)
 12      format(4a32)
 
      else
         read(iunit) number_of_geometries,nsurf,nregs,
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
      nslen=max(1,ns)
      nrlen=max(1,nr)
      nmlen=max(1,nm)
      lmlen=max(1,lm)
      lrlen=max(1,lr)
      lslen=max(1,ls)
c
c surface info for this geom - name, type, boundary type,
c definition, offset to definition
c
      call mmfindbk('csall',partname,ipcsall,length,ierror)
      if(ierror.ne.0) call mmgetblk('csall',partname,
     *  ipcsall,nslen,3,ierror)
      call mmfindbk('istype',partname,ipistype,length,ierror)
      if(ierror.ne.0) call mmgetblk('istype',partname,
     *  ipistype,nslen,3,ierror)
      call mmfindbk('ibtype',partname,ipibtype,length,ierror)
      if(ierror.ne.0) call mmgetblk('ibtype',partname,
     *  ipibtype,nslen,3,ierror)
      call mmfindbk('sheetnm',partname,ipsheetnm,length,ierror)
      if(ierror.ne.0) call mmgetblk('sheetnm',partname,
     *  ipsheetnm,nslen,3,ierror)
      call mmfindbk('surfparam',partname,ipsurfparam,length,ierror)
      if(ierror.ne.0) call mmgetblk('surfparam',partname,
     *  ipsurfparam,lslen,3,ierror)
      call mmfindbk('offsparam',partname,ipoffsparam,length,ierror)
      if(ierror.ne.0) call mmgetblk('offsparam',partname,
     *  ipoffsparam,nslen,1,ierror)
c
      if(iomode(1:5).eq.'ascii') then
         if(ns.ne.0) then
            read(iunit,12) (csall(j),j=1,ns)
            read(iunit,12) (istype(j),j=1,ns)
            read(iunit,12) (ibtype(j),j=1,ns)
            read(iunit,12) (sheetnm(j),j=1,ns)
            read(iunit,20) (surfparam(j),j=1,ls)
 20         format(6e20.13)
            read(iunit,10) (offsparam(j),j=1,ns)
        endif
      else
         if(ns.ne.0) then
            read(iunit)(csall(j),j=1,ns),(istype(j),j=1,ns),
     *     (ibtype(j),j=1,ns), (sheetnm(j),j=1,ns),
     *     (surfparam(j),j=1,ls),(offsparam(j),j=1,ns)
         else
            read(iunit)
         endif
      endif
 
 
C
C  write region storage blocks - name , offset to definition,
c  length of definition, definitions
c
      call mmfindbk('cregs',partname,ipcregs,length,ierror)
      if(ierror.ne.0) call mmgetblk('cregs',partname,
     *  ipcregs,nrlen,3,ierror)
      call mmfindbk('offregdef',partname,ipoffregdef,length,ierror)
      if(ierror.ne.0) call mmgetblk('offregdef',partname,
     *  ipoffregdef,nrlen,1,ierror)
      call mmfindbk('ndefregs',partname,ipndefregs,length,ierror)
      if(ierror.ne.0) call mmgetblk('ndefregs',partname,
     *  ipndefregs,nrlen,1,ierror)
      call mmfindbk('regdef',partname,ipregdef,length,ierror)
      if(ierror.ne.0) call mmgetblk('regdef',partname,
     *  ipregdef,lrlen,3,ierror)
      if(iomode(1:5).eq.'ascii') then
         if(nr.ne.0) then
            read(iunit,12) (cregs(j),j=1,nr)
            read(iunit,10) (offregdef(j),j=1,nr)
            read(iunit,10) (ndefregs(j),j=1,nr)
            read(iunit,12) (regdef(j),j=1,lr)
         endif
      else
         if(nr.ne.0)
     *     read(iunit)(cregs(j),j=1,nr),(offregdef(j),j=1,nr),
     *     (ndefregs(j),j=1,nr), (regdef(j),j=1,lr)
      endif
 
C
C  write mregion storage blocks
      call mmfindbk('cmregs',partname,ipcmregs,length,ierror)
      if(ierror.ne.0) call mmgetblk('cmregs',partname,
     *  ipcmregs,nmlen,3,ierror)
      call mmfindbk('offmregdef',partname,ipoffmregdef,length,ierror)
      if(ierror.ne.0) call mmgetblk('offmregdef',partname,
     * ipoffmregdef ,nmlen,1,ierror)
      call mmfindbk('ndefmregs',partname,ipndefmregs,length,ierror)
      if(ierror.ne.0) call mmgetblk('ndefmregs',partname,
     *  ipndefmregs,nmlen,1,ierror)
      call mmfindbk('mregdef',partname,ipmregdef,length,ierror)
      if(ierror.ne.0) call mmgetblk('mregdef',partname,
     *  ipmregdef,lmlen,3,ierror)
      call mmfindbk('matregs',partname,ipmatregs,length,ierror)
      if(ierror.ne.0) call mmgetblk('matregs',partname,
     *  ipmatregs,nmlen,1,ierror)
      if(iomode(1:5).eq.'ascii') then
         if(nm.ne.0) then
            read(iunit,12) (cmregs(j),j=1,nm)
            read(iunit,10) (offmregdef(j),j=1,nm)
            read(iunit,10) (ndefmregs(j),j=1,nm)
            read(iunit,12) (mregdef(j),j=1,lm)
            read(iunit,10) (matregs(j),j=1,nm)
         endif
      else
         if(nm.ne.0)
     *     read(iunit)(cmregs(j),j=1,nm),(offmregdef(j),j=1,nm),
     *    (ndefmregs(j),j=1,nm), (mregdef(j),j=1,lm),
     *    (matregs(j),j=1,nm)
      endif
      enddo
 
C
 9999 call mmrelprt(isubname,icscode)
      return
      end
 
C
