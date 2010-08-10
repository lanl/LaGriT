        subroutine excavate
     &      (main_mo, surf_mo, ierror_return)
C
C######################################################################
C
C   PURPOSE - 
C
C       This routine takes as input the names of a mesh and a surface.
C       It then automatically excavates the mesh around the surface, and
C       inserts the surface into the mesh, saving the result into a new
C       mesh specified by the user. The surface mesh must be triangular.
C       If it is not, it will be converted to a triangular mesh. The
C       circumcircles of each element of the surface are computed. Then,
C       any point of the mesh that falls within the equivalent
C       circumsphere of an element of the surface is removed (making an
C       area for good tets to be formed).
C
C   NOTES -
C
C       Syntax for this command:
C       addmesh / excavate / sink_name / mesh_name / surface_name
C           / [bfs] / [connect]
C
C       If the optional 'connect' argument is given, the surface will be
C       addmesh/appended into the background mesh, and connect will be
C       called on the resulting point cloud. If the optional bfs
C       argument is specified, then that algorithm will be used to
C       search for points. By default, it chooses the KD-tree algorithm.
C
C######################################################################
C
C   CHANGE HISTORY - 
C
C       This is the first version.
C       6 / 23 / 2010
C       agable
C
C######################################################################
C
        implicit none
C
        include 'chydro.h'
        include 'local_element.h'
C
C       Subroutine input variables
        character*32    main_mo, surf_mo
        integer         ierror_return, bfs_flag
C
C       Local variables
        character*32    isubname, dual_mo
C       character*132   mess 
        character*256   cmd
        real*8          x1,x2,x3,y1,y2,y3,z1,z2,z3,ax4,ay4,az4,
     &                  ds1,ds2,ds3,farea,maxrad,
     &                  curr_crad,curr_dist
        integer         nmnodes,nmelems,nsnodes,nselems,ndnodes,
     &                  ncnodes,ncelems,
     &                  lnode1,lnode2,lnode3,
     &                  gdim,tdim,mbndry,
     &                  i,i2,i3,nrem,counter,
     &                  curr_cnode,curr_dnode,next_dnode,
     &                  ilen, itype,ierror
        character*32    cmaxrad, qname
        character*512   ndnodlist
        real*8          bboxx(8), bboxy(8), bboxz(8)
C
C       Variables for calls to subroutines
        pointer         (ipitets,   itets)
        integer         itets(*)
        pointer         (iptetoffs, itetoffs)
        integer         itetoffs(*)
        pointer         (ipitetm,   itetm)
        integer         itetm(*)
        pointer         (iptetoffm, itetoffm)
        integer         itetoffm(*)
        pointer         (ipxics,    xics)
        real*8          xics(*)
        pointer         (ipyics,    yics)
        real*8          yics(*)
        pointer         (ipzics,    zics)
        real*8          zics(*)
        pointer         (ipxicm,    xicm)
        real*8          xicm(*)
        pointer         (ipyicm,    yicm)
        real*8          yicm(*)
        pointer         (ipzicm,    zicm)
        real*8          zicm(*)
        pointer         (ipxicd,    xicd)
        real*8          xicd(*)
        pointer         (ipyicd,    yicd)
        real*8          yicd(*)
        pointer         (ipzicd,    zicd)
        real*8          zicd(*)   
        pointer         (ipcrad,    crad)
        real*8          crad(*)
        pointer         (ipxvcen,   xvcen)
        real*8          xvcen(*)
        pointer         (ipyvcen,   yvcen)
        real*8          yvcen(*)
        pointer         (ipzvcen,   zvcen)
        real*8          zvcen(*)
        pointer         (iplinkt,   linkt)
        integer         linkt(*)
        pointer         (ipsbox,    sbox)
        real*8          sbox(*)
        pointer         (ipfound,   elemfound)
        integer         elemfound(*)
        pointer         (iptettyp,  itettyp)
        integer         itettyp(*)
        pointer         (ipclosepts,closepts)
        integer         closepts(*)
        pointer         (ipdist,    dist)
        real*8          dist(*)
        pointer         (ipnearnode,nearnodes)
        integer         nearnodes(*)
        pointer         (ipitp1,    itp1)
        integer         itp1(*)
        pointer         (ipisetwd,  isetwd)
        integer         isetwd(*)
        pointer         (ipjtet,    jtet)
        integer         jtet(*)
        pointer         (ipjtetoff, jtetoff)
        integer         jtetoff(*)
        pointer         (ippushval, pushval)
        integer         pushval
        pointer         (ippopval,  popval)
        integer         popval
        pointer         (ipvisited, visited)
        integer         visited(*)
C
C
C######################################################################
C
        isubname = 'excavate'
C
        print *, 'IN EXCAVATE: ', main_mo, surf_mo
        
C   The flag for which algorithm to use is passed in through
C   ierror_return. Save it into a flag value, so we can use
C   ierror_return for output.
        bfs_flag = 0
        if (ierror_return .eq. 1) then
            bfs_flag = 1
            ierror_return = 0
        endif

C   Get access to all basic attributes of the primary and surface
C   meshes, for use later on.
        call cmo_get_info('nnodes',surf_mo,nsnodes,ilen,itype,ierror)
        call cmo_get_info('nelements',surf_mo,nselems,ilen,itype,ierror)
        call cmo_get_info('itet',surf_mo,ipitets,ilen,itype,ierror)
        call cmo_get_info('itetoff',surf_mo,iptetoffs,ilen,itype,ierror)
        call cmo_get_info('xic',surf_mo,ipxics,ilen,itype,ierror)
        call cmo_get_info('yic',surf_mo,ipyics,ilen,itype,ierror)
        call cmo_get_info('zic',surf_mo,ipzics,ilen,itype,ierror)
        call cmo_get_info('nnodes',main_mo,nmnodes,ilen,itype,ierror)
        call cmo_get_info('nelements',main_mo,nmelems,ilen,itype,ierror)
        call cmo_get_info('itet',main_mo,ipitetm,ilen,itype,ierror)
        call cmo_get_info('itetoff',main_mo,iptetoffm,ilen,itype,ierror)
        call cmo_get_info('xic',main_mo,ipxicm,ilen,itype,ierror)
        call cmo_get_info('yic',main_mo,ipyicm,ilen,itype,ierror)
        call cmo_get_info('zic',main_mo,ipzicm,ilen,itype,ierror)
        call cmo_get_info('itp1',main_mo,ipitp1,ilen,itype,ierror)



C   Add coordinates of voronoi centers to every element of the surface        
        cmd = 'cmo/addatt/' // surf_mo // '/voronoi/'
     &      // '-xvcen- -yvcen- -zvcen-; finish'
        call dotask(cmd, ierror)

C   Add circumradius attribute to surface mesh, to be filled manually
        cmd = 'cmo/addatt/' // surf_mo // '/-cradius-/'
     &      // 'VDOUBLE/scalar/nelements; finish'
        call dotask(cmd, ierror)

C   Loop through every element of surface. For each element:
C       * compute circumradius
C       * paint radius onto surface mesh 
C   IMPORTANT: Assumes surface mesh is triangular - this is necessary
C   for finding circumcircle, and makes other math easier.
        call cmo_get_info('-xvcen-',surf_mo,ipxvcen,ilen,itype,ierror)
       !print *, 'xvcen', ipxvcen, ilen, itype, ierror
        call cmo_get_info('-yvcen-',surf_mo,ipyvcen,ilen,itype,ierror)
       !print *, 'yvcen', ipyvcen, ilen, itype, ierror
        call cmo_get_info('-zvcen-',surf_mo,ipzvcen,ilen,itype,ierror)
       !print *, 'zvcen', ipzvcen, ilen, itype, ierror
        call cmo_get_info('-cradius-',surf_mo,ipcrad,ilen,itype,ierror)
       !print *, 'crad ', ipcrad, ilen, itype, ierror
        maxrad = 0
        do i=1,nselems
           !print *,
!    &  "--------------------------------------------------------------"
           !print *, "ELEMENT: ", i
C       Get information about locations of element vertices
            lnode1 = itets(itetoffs(i)+1)
            lnode2 = itets(itetoffs(i)+2)
            lnode3 = itets(itetoffs(i)+3)
           !print *, "LNODES: ", lnode1, lnode2, lnode3
            x1 = xics(lnode1)
            x2 = xics(lnode2)
            x3 = xics(lnode3)
            y1 = yics(lnode1)
            y2 = yics(lnode2)
            y3 = yics(lnode3)
            z1 = zics(lnode1)
            z2 = zics(lnode2)
            z3 = zics(lnode3)
           !print *, " Node1: ", x1, y1, z1
           !print *, " Node2: ", x2, y2, z2
           !print *, " Node3: ", x3, y3, z3
C       Calculate circumradius of element
            ax4 = (y3 - y1)*(z2 - z1)-(z3-z1)*(y2-y1)
            ay4 = -((x3 - x1)*(z2 - z1)-(z3 - z1)*(x2 - x1))
            az4 = (x3 - x1)*(y2 - y1)-(y3 - y1)*(x2 - x1)
            farea=.5*sqrt(ax4**2+ay4**2+az4**2)
            ds1 = sqrt((x3 - x2)**2 + (y3 - y2)**2 + (z3 - z2)**2)
            ds2 = sqrt((x1 - x3)**2 + (y1 - y3)**2 + (z1 - z3)**2)
            ds3 = sqrt((x2 - x1)**2 + (y2 - y1)**2 + (z2 - z1)**2)
            curr_crad = ds1*ds2*ds3/(4.0d0*farea)
           !print *, "RADIUS: ", curr_crad
C       Fill circumradius attribute
            crad(i) = curr_crad
            if (curr_crad > maxrad) maxrad = curr_crad
        enddo
        print *,"Done filling radii."
        call mmverify()



C############################
C   PRIMARY ALGORITHM: Compute a KD-tree on the main background mesh,
C   and use intersections of the KD elements with the circumspheres of
C   the elements in the surface to find candidate nodes. Then,
C   compute explicit distances from candidate nodes to the circumcenters
C   of the surface elements (the dual nodes) and remove any nodes whose
C   distance is less than the circumradius.
C
        if (bfs_flag .eq. 0) then

        print *,"IN KD-TREE SEARCH"

C   Create the KD-tree on the background mesh
        cmd = 'cmo/select/' // main_mo // '; finish'
        call dotask(cmd, ierror)
        cmd = 'kdtree/build; finish'
        call dotask(cmd, ierror)

C   Get access to the kd-tree attributes, so we can pass them into the
C   intersection function
        call cmo_get_info('linkt',main_mo,iplinkt,ilen,itype,ierror)
        call cmo_get_info('sbox',main_mo,ipsbox,ilen,itype,ierror)
        call mmgetblk('elemfound',isubname,ipfound,nmelems,1,ierror)
        call cmo_get_info('itettyp',main_mo,iptettyp,ilen,itype,ierror)
        
C   Loop through all the elements in the surface. For each element,
C   compute its bounding box, intersect this box with the kd-tree, and
C   examine the resulting elements for nodes that fall within the
C   circumradius of the element.
        nrem = 0
        do i=1,nselems
C   Get current circumcenter coordinates and radius
            x1 = xvcen(i)
            y1 = yvcen(i)
            z1 = zvcen(i)
            curr_crad = crad(i)
            
C   Construct bounding box of circumsphere
            do i2=1,8
                if (i2 .le. 4) then
                    bboxx(i2) = x1 - curr_crad
                else 
                    bboxx(i2) = x1 + curr_crad
                endif
                if (MOD(i2, 2) .eq. 1) then
                    bboxy(i2) = y1 - curr_crad
                else
                    bboxy(i2) = y1 + curr_crad
                endif
                if (i2 .ge. 3 .and. i2 .le. 6) then
                    bboxz(i2) = z1 - curr_crad
                else
                    bboxz(i2) = z1 + curr_crad
                endif
            enddo

C   Intersect the bounding cube with the kd-tree
            call kDtreeselect(8, bboxx, bboxy, bboxz, linkt, sbox,
     &                        ncelems, elemfound, ierror)

           !print *,"Candidate elements: ", ncelems
C   Loop through the found elements
            do i2=1,ncelems
C   Loop through the nodes of the element
                do i3=1,nelmnen(itettyp(elemfound(i2)))
                    curr_cnode = itetm(itetoffm(elemfound(i2))+i3)
                    x2 = xicm(curr_cnode)
                    y2 = yicm(curr_cnode)
                    z2 = zicm(curr_cnode)
                   !print *,"Node: ", curr_cnode, x2,y2,z2
                    curr_dist = sqrt((x1-x2)**2+(y1-y2)**2+(z1-z2)**2)
                   !print *,curr_dist, curr_crad
                    if (curr_dist .le. (curr_crad+curr_crad*1e-9)) then
C   Dud the node - 21 is a special value for itp that signifies dudding
                        if (itp1(curr_cnode) .ne. 21) then
                            itp1(curr_cnode) = 21
                            nrem = nrem + 1
                           !print *,"Dudded."
                        endif
                    endif
                enddo
            enddo
        enddo
        print *,"Number of nodes dudded total: ", nrem


C############################
C   SECONDARY ALGORITHM: (Used only if bfs_flag is set to 1). Get a set
C   of candidate nodes for deletion by finding all nodes within the
C   maximum circumradius of the surface. Then, for each of these nodes,
C   starting at the element to which it is closest, perform a
C   breadth-first search across the surface, searching for an element
C   whose circumradius is large enough to contain the candidate node.
C
        elseif (bfs_flag .eq. 1) then

        print *,"IN BFS SEARCH"

C   Create dual mesh to hold dual points of surface
        dual_mo = '-dual_mo-'
        call cmo_create(dual_mo,ierror)
        ndnodes = nselems
        call cmo_set_info('nnodes',dual_mo,ndnodes,1,1,ierror)
        call cmo_newlen(dual_mo, ierror)
        call cmo_get_info('ndimensions_geom',surf_mo,gdim,ilen,itype,
     &                    ierror)
        call cmo_set_info('ndimensions_geom',dual_mo,gdim,1,1,ierror)
        call cmo_get_info('ndimensions_topo',surf_mo,tdim,ilen,itype,
     &                    ierror)
        call cmo_set_info('ndimensions_topo',dual_mo,tdim,1,1,ierror)
       
C   Loop through every element of surface. For each element, create
C   corresponding element of dual mesh.
        call cmo_get_info('xic',dual_mo,ipxicd,ilen,itype,ierror)
        call cmo_get_info('yic',dual_mo,ipyicd,ilen,itype,ierror)
        call cmo_get_info('zic',dual_mo,ipzicd,ilen,itype,ierror)
        
        do i=1,nselems
C       Set coordinates of ith point from circumcenter coords of ith
C       element
            xicd(i) = xvcen(i)
            yicd(i) = yvcen(i)
            zicd(i) = zvcen(i)
        enddo

C   Compute distance field from every point of the main mesh to the
C   elements of the surface - i.e. the nodes of the dual mesh
        cmd = 'compute/distance_field/' // main_mo // '/' 
     &      // dual_mo // '/-dist-/keepatt; finish'
        call dotask(cmd, ierror)
C   Put the 'nearest element' attribute in a safe place.
        cmd = 'cmo/addatt/' // main_mo // '/-nearnode-/VINT; finish;'
        call dotask(cmd, ierror)
        cmd = 'cmo/copyatt/' // main_mo // '/' // main_mo // '/'
     &      // '-nearnode-/pt_gtg; finish'
        call dotask(cmd, ierror)
C   Create set of points with distance less than max cradius.
        write(cmaxrad, '(F20.10)') maxrad
       !print *, "cmaxrad: ", maxrad, cmaxrad
        cmd = 'pset/-close-/attribute/-dist-/1,0,0/' // cmaxrad
     &      // '/le; finish'
        call dotask(cmd, ierror)
        
C   Get array of nodes in this pset so we can iterate through it
        call cmo_get_info('isetwd',main_mo,ipisetwd,ilen,itype,ierror)
        call mmgetblk('closepts',isubname,ipclosepts,nmnodes,1,ierror)
        call pntlimc('pset','get','-close-',ipclosepts,ncnodes,nmnodes,
     &               isetwd,itp1)
                 
C   Get access to -nearnode- attribute array so we can get to the
C   dual_mo from the main_mo. Get access to -dist- attribute array so we
C   can test it against -cradius- values.
        call cmo_get_info('-nearnode-',main_mo,ipnearnode,
     &                    ilen,itype,ierror)
        call cmo_get_info('-dist-',main_mo,ipdist,ilen,itype,ierror)
                 
C   Get the necessary information to use jtet, so we can perform the BFS
        call cmo_get_info('jtet',surf_mo,ipjtet,ilen,itype,ierror)
        call cmo_get_info('jtetoff',surf_mo,ipjtetoff,ilen,itype,ierror)
        call cmo_get_info('mbndry',surf_mo,mbndry,ilen,itype,ierror)
                 
C   Initialize memory for breadth-first search. Need a place to put data
C   so it can be pushed onto the queue, we need a place to put the data
C   returned by pop calls, and we need an array to keep track of 
C   which nodes the BFS has visited.
        qname = 'is_bfs'
        call queue_c(qname,ndnodes,1,ierror)
        call mmgetblk('pushval',isubname,ippushval,1,1,ierror)
        call mmgetblk('popval',isubname,ippopval,1,1,ierror)
        call mmgetblk('visited',isubname,ipvisited,ndnodes,1,ierror)
       !call mmverify()
       !call mmprint() 
                 
C   Loop through all nodes in main mesh that are "close" (i.e. inside
C   the max_radius of a circumsphere. Test their distance against the
C   radius of the nearest sphere. If they fall inside the sphere, mark
C   the node for deletion. If the nearest node fails, perform a
C   breadth-first search outward from that node, to see if the point
C   falls within the radius of any other dual node.
        nrem = 0
        do i=1,ncnodes
           !print *,'----------------------------------------'
C   Clear the 'visited' array for use with the current node
            do i2=1,ndnodes
                visited(i2)=0
            enddo
C   Set get node numbers of current node and first dual node to check
            curr_cnode = closepts(i)
            curr_dnode = nearnodes(curr_cnode)
           !print *,"INDEX:", i, "CNODE:", curr_cnode
           !print *,"First dnode:", curr_dnode
           !print *,"Nearest distance: ", dist(curr_cnode)
C   Push first dual node onto queue to initialize the BFS
            pushval = curr_dnode
           !call mmverify()
           !call mmprint()
            call queue_push(qname,ippushval,1,ierror)
            visited(curr_dnode) = 1
C   This is the actual breadth-first search loop:
            counter = 0
            do
                call queue_pop(qname,ippopval,1,ierror)
C   Finish the search if the queue is empty - every node has been hit
                if (ierror .ne. 0) then
                   !print *,"QUEUE is empty."
                    exit
                endif
                curr_dnode = popval
                counter = counter + 1
               !print *,"DNODE:",curr_dnode
C   Compute the distance from the current node to the current dual node
                x1 = xicm(curr_cnode)
                y1 = yicm(curr_cnode)
                z1 = zicm(curr_cnode)
                x2 = xicd(curr_dnode)
                y2 = yicd(curr_dnode)
                z2 = zicd(curr_dnode)
               !print *,x1,y1,z1,x2,y2,z2,(x1-x2),(y1-y2),(z1-z2)
                curr_dist = sqrt((x1-x2)**2 + (y1-y2)**2 + (z1-z2)**2)
                curr_crad = crad(curr_dnode)
               !print *,"DIST: ", curr_dist, "CRAD: ", curr_crad
C   See if the current node is within the dual-node's circumradius
                if (curr_dist .le. (curr_crad+curr_crad*1e-9)) then
                   !print *,"This node will be removed."
C   Dud the node - 21 is a special value for itp that signifies dudding
                    itp1(curr_cnode) = 21
                    nrem = nrem + 1
                    exit
                else
C   Otherwise, push the dual-node's neighbors onto the queue
                    do i2=1,3
                        if (jtet(jtetoff(curr_dnode)+i2).lt.mbndry) 
     &                  then
                            next_dnode =
     &                      1+(jtet(jtetoff(curr_dnode)+i2)-1)/3
                        elseif (jtet(jtetoff(curr_dnode)+i2).gt.mbndry)
     &                  then
                            next_dnode =
     &                      1+(jtet(jtetoff(curr_dnode)+i2)-mbndry-1)/3
                        else
                            next_dnode = -1
                        endif
                        if (visited(next_dnode) .eq. 0 .and. 
     &                      next_dnode .ne. -1) then
                           !print *,counter,":About to push node",
C    &                  next_dnode,jtet(jtetoff(curr_dnode)+i2),
C    &                  1+(jtet(jtetoff(curr_dnode)+i2)-1)/3,
C    &                  visited(next_dnode)
                            pushval = next_dnode
                            call queue_push(qname,ippushval,1,ierror)
                            visited(next_dnode) = 1
                        endif
                    enddo
                endif
            enddo
           !print *,"Num nodes checked: ", counter
            if (counter .ne. ndnodes .and. 
     &          counter .ne. 1) then
C    &          itp1(curr_cnode) .ne. 21) then
               !print *,"INDEX:", i, "CNODE:", curr_cnode
               !print *,"Only ", counter, " nodes were visited!"
               !if (itp1(curr_cnode) .eq. 21) print *,"Removed!"
               !print *,"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
                ndnodlist = ''
                do i2=1,ndnodes
                    if (visited(i2) .eq. 0) then
                       !print *,i2
                       !write(ndnodlist, '(a, i4.4)') ndnodlist, i2
                    endif
                enddo
               !print *,ndnodlist
            endif
        enddo
C   Release the memory held by the queue
        call queue_d(qname,ierror)
        print *,"Number of nodes dudded total: ", nrem
        

C############################
C   Done with either the KD-Tree or the BFS method. Either way, we have
C   now dudded a selection of points that are too close to the surface.
C   Time to get rid of them once and for all.
        endif



        
C   Remove all the points we just dudded.
        cmd = 'cmo/select/' // main_mo // '; finish'
        call dotask(cmd, ierror)
        cmd = 'geniee; finish'
        call dotask(cmd, ierror)
        cmd = 'rmpoint/compress; finish'
        call dotask(cmd, ierror)

        return
        end
        
