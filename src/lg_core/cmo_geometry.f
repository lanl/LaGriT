      subroutine cmo_geometry(cmo_name,geom_name,ierror_return)
C
C
C#######################################################################
C
C      PURPOSE -
C
C         This Routine Associates a mesh object with a geometry
c
c      INPUT -
c         cmo_name   mesh object name
c         geom_name  geometry name
c
c      OUTPUT
c         ierror_return  return flag (0= ok, 1=error)
c
c      CHANGE
C  $Log: cmo_geometry.f,v $
C  Revision 2.00  2007/11/05 19:45:48  spchu
C  Import to CVS
C
CPVCS    
CPVCS       Rev 1.2   Mon Mar 20 17:05:46 2000   dcg
CPVCS    fix mesh object/ geometry correspondence
CPVCS    add geometry/release option
CPVCS
CPVCS       Rev 1.1   Thu Feb 17 13:40:04 2000   dcg
CPVCS    add cmo_constraints subroutine
CPVCS
CPVCS       Rev 1.0   Tue Feb 15 10:37:26 2000   dcg
CPVCS    Initial revision.
C
C#######################################################################
C
      implicit none
      include 'geom_lg.h'
      character*32 cmo_name,geom_name,geom_name_temp,cmo_name_temp
      integer ierror_return
c
      character*132 cbuf
      integer i,len,ierror,icscode
      character*32 partname
c
c  set geom_name in the mesh object attribute list
c
      geom_name_temp=geom_name
      cmo_name_temp=cmo_name
      call cmo_select(cmo_name_temp,ierror_return)
      cbuf=
     *  'cmo/modatt/'//cmo_name_temp//
     *  '/geom_name/default/'//geom_name_temp//
     *  '/ ; finish'
      call dotask(cbuf,ierror)
c
c  see if geometry name is the current one - if not save the old
c  state and get the new one
c
      partname='geom_lg'
      call mmfindbk ('geom_names',partname,
     *  ipgeom_names,len,ierror)
      call mmfindbk ('geom_info',partname,
     *  ipgeom_info,len,ierror)
      if(geom_name.ne.current_geom_name) then
         do i=1,number_of_geometries
           if(current_geom_name.eq.geom_names(i)) then
              if(nsurf.ne.0) geom_info(1,i)=nsurf
              if(nregs.ne.0) geom_info(2,i)=nregs
              if(nmregs.ne.0) geom_info(3,i)=nmregs
              if(maxdef.ne.0) geom_info(4,i)=maxdef
              if(maxmdef.ne.0) geom_info(5,i)=maxmdef
              if(lastregdef.ne.0) geom_info(6,i)=lastregdef
              if(lastmregdef.ne.0) geom_info(7,i)=lastmregdef
              if(lastsparam.ne.0) geom_info(8,i)=lastsparam
              go to 10
           endif
         enddo
         write(cbuf,8) current_geom_name
 8       format ('cannot find geometry: ',a32)
         call writloga('default',0,cbuf,0,icscode)
         ierror_return=1
         go to 9999
 10      do i=1,number_of_geometries
           if(geom_name.eq.geom_names(i)) then
              nsurf=geom_info(1,i)
              nregs=geom_info(2,i)
              nmregs=geom_info(3,i)
              maxdef=geom_info(4,i)
              maxmdef=geom_info(5,i)
              lastregdef=geom_info(6,i)
              lastmregdef=geom_info(7,i)
              lastsparam=geom_info(8,i)
              ierror_return=0
              go to 20
           endif
         enddo
         write(cbuf,8) geom_name
         call writloga('default',0,cbuf,0,icscode)
         ierror_return=1
         go to 9999
 20      current_geom_name=geom_name
      endif
 9999 return
      end
 
      subroutine cmo_constraints(cmo_sink,cmo_src,ierror_return)
C
C
C#######################################################################
C
C      PURPOSE -
C
C         copy constraint attributes from src to sink mesh object
c
c      INPUT -
c         cmo_name   mesh object name
c         geom_name  geometry name
c
c      OUTPUT
c         ierror_return  return flag (0= ok, 1=error)
C#######################################################################
      implicit none
 
      pointer (ipicontab,icontab)
      integer icontab(*)
      pointer (ipicontab1,icontab1)
      integer icontab1(*)
      character*32 cmo_sink,cmo_src,cmoin,cmoout
      integer ierror_return,nconbnd,ilen,j,ncon,iconlen,icscode
      character*132 ibuff
C#######################################################################
C
      cmoin=cmo_src
      cmoout=cmo_sink
      call cmo_select(cmoout,ierror_return)
      if(ierror_return.ne.0) go to 9999
C
C   CHECK TO SEE IF ICONTAB EXISTS FOR src CMO
      call cmo_get_info('ncon',cmoout,ncon,ilen,j,icscode)
C
C   IF NOT THERE CREATE
C   ADD NCON50 VARIABLE TO CMO - LENGTH OF BLOCK
C   ADD ICONTAB TO CMO
C
      if(icscode.ne.0) then
        ibuff='cmo/addatt/'// cmoout //
     *                    '/ncon50/INT/' //
     *         'scalar/scalar/constant/permanent/x/0' //
     *         ' ; finish'
         call dotaskx3d(ibuff,icscode)
      endif
      call cmo_get_intinfo('ncon50',cmoin,ncon,ilen,
     *    j,ierror_return)
      if(ierror_return.ne.0) go to 9999
      call cmo_set_info('ncon50',cmoout,ncon,1,1,icscode)
c
      call cmo_get_info('nconbnd',cmoout,nconbnd,ilen,j,icscode)
      if(icscode.ne.0) then
         ibuff='cmo/addatt/'// cmoout //
     *                    '/nconbnd/INT/' //
     *         'scalar/scalar/constant/permanent/x/0' //
     *         ' ; finish'
         call dotaskx3d(ibuff,icscode)
      endif
      call cmo_get_intinfo('nconbnd',cmoin,nconbnd, ilen,
     *    j,ierror_return)
      if(ierror_return.ne.0) go to 9999
      call cmo_set_info('nconbnd',cmoout,nconbnd,1,1,icscode)
c
      call mmfindbk('icontab',cmoout,ipicontab,ilen,icscode)
      if(icscode.ne.0) then
         ibuff='cmo/addatt/'// cmoout //
     *                    '/icontab/VINT/' //
     *         'scalar/ncon50/constant/permanent/x/0.0' //
     *         ' ; finish'
         call dotaskx3d(ibuff,icscode)
         call mmfindbk('icontab',cmoout,ipicontab,ilen,ierror_return)
         if(ierror_return.ne.0) go to 9999
      endif
      call mmfindbk('icontab',cmoin,ipicontab1,iconlen,ierror_return)
      if (iconlen.gt.ilen) call mmnewlen('icontab',cmoout,
     *    ipicontab,iconlen,icscode)
      do j=1,iconlen
         icontab(j)=icontab1(j)
      enddo
 9999 return
      end
 
C
