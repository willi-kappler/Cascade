! nn_remove

!
!   nn_remove - Removes a node from the triangulation and updates
!           vertex and adjacency array using only local search.
!           Optionally re-numbers the nodes from 1 to np-1.
!
!   Input:  
!               node                    node to be removed 
!               np                      number of nodes
!           nt          number of triangles
!               points(2,np)            array of node co-ordinates
!           vertices(3,nt)      array of triangle vertices  
!       neighbour(3,nt)     adjacency array
!       nnpn_max        maximum number of neighbours per node
!                   (used to set array sizes)
!       vis_tlist       work array used by `delaun'
!       vis_elist       work array used by `delaun'
!       add_elist       work array used by `delaun'
!       nv_max          size of work arrays (<=nnpn_max)
!       c_list          work array used by `find_ctriangles'
!       nodelist        work array used by `find_ctriangles'
!       tlist           work array used by `update_vn_lists'
!       loc         a first guess at a triangle attached 
!                   to the input node. Can be any value
!                   between 1<= loc <= nt.
!       renumber_nodes      logical. If true then points are repacked
!
!   Output:
!       nt              number of triangles 
!           vertices(3,nt)      array of triangle vertices  
!       neighbour(3,nt)     adjacency array
!
!
!   Comments:
!        If the logical renumber_nodes is set to true then
!        the node numbers will also be renumbered from 1 to np-1.
!        This is done by 1) renaming node np as the input node if
!        node is not already equal to np, and 2) set np = np-1.
!        In this mode it is assumed that node np exists in the
!        current triangles. An error will occur if the node np
!        is removed without renumbering node numbers and subsequently
!        a call is made with node renumbering. i.e.  nn_remove is 
!        called with node=np and renumber_nodes = .false., and then 
!        a later call is made with renumber_nodes = .true.
!        Be careful in mixing calls with renumber_nodes changing.
!
!        Assumes input list of vertices in anticlockwise order
!
!        The arrays must be dimensioned in the following way:
!
!        real*8  points(np_max)
!        integer vertices(3,nt_max)
!        integer neighbour(3,nt_max)
!
!        integer nodelist(nnpn_max)    : work array
!        integer tlist(nnpn_max)       : work array
!        logical c_list(2*nnpn_max)    : work array
!        integer v_local(3,2*nnpn_max) : work array
!        integer n_local(3,2*nnpn_max) : work array
!            integer vis_tlist(nv_max)     : work array
!        integer vis_elist(nv_max)     : work array
!            integer add_tlist(nv_max)     : work array
!
!        where np_max = maximum number of points, nt_max = maximum
!        number of triangles (<2*np_max) and nnpn_max is the maximum 
!        number of neighbours about any node.
!
!        The work arrays vis_tlist, vis_elist,and add_tlist must be 
!        dimensioned size nv_max, (the maximum number of
!        triangles visible from any point from outside of the hull). 
!        In this case delaun is only being used for a maximum of 
!        nnpn_max points and so nv_max will always be <= nnpn_max.
!
!        Calls to find_ctriangles and update_vn_lists.
!
!                   M. Sambridge, RSES, Nov. 1995.
!
!------------------------------------------------------------------------
!

    module m_nn_remove
        contains

        Subroutine nn_remove &
                       (node,np,nt,points,vertices,neighbour,loc, &
                        nnpn_max,nv_max,vis_tlist,vis_elist,add_tlist, &
                        v_local,n_local,c_list,nodelist,tlist, &
                        renumber_nodes)

          common /vocal/ ivocal,iprofile

        integer(4) :: np,node,nnpn_max,loc,n_neighbours,n_triangles,np_local,nv_max
        integer(4) :: nt_local,nt,node_old,loc2,ivocal,iprofile

        integer     vertices(3,*)
        integer     neighbour(3,*)
        integer     nodelist(*)
        integer     tlist(*)
        integer         vis_tlist(*)
        integer         vis_elist(*)
        integer         add_tlist(*)
        integer     v_local(3,*)
        integer     n_local(3,*)
        logical         c_list(*)
        logical         on_hull
        logical         renumber_nodes
        real*8      points(2,*)
        logical     debug_mode
        common      /debug_nn_remove/debug_mode

    !                       Check error conditions
        if(np.eq.3)then
               write(*,100)
               stop
            end if
    !                       find list of triangles 
    !                       and nodes attached to input node
    !
        call find_neighbours_r &
                 (node,points,nnpn_max,vertices,neighbour, &
                  loc,on_hull,n_neighbours,n_triangles,nodelist,tlist)

    !                       find the circum-triangles
    !                       of the current node 

    !
            np_local = n_neighbours
        call find_ctriangles &
                 (node,np,points,nodelist, &
                  np_local,on_hull,nnpn_max,nv_max,vis_tlist, &
                  vis_elist,add_tlist,nt_local,v_local,n_local,c_list)

    !                       replace current node and
    !                       its attached triangles with
    !                       circum-triangles in
    !                       vertices and neighbour lists
    !
        call update_vn_lists &
                (node,nt,vertices,neighbour,tlist, &
                 on_hull,np_local, &
                 nt_local,v_local,n_local,c_list)
    !
        if(.not.renumber_nodes)return
    !
    !                       renumber nodes from 1 to np-1
    !
        if(node.eq.np)then
              np = np -1
              return
            end if
    !                       renumber node np to the
    !                       value of the input node in
    !                       all triangles attached to it 
            node_old = np
            loc2 = nt
        call renumber_node &
                 (node_old,node,points,vertices,neighbour, &
                  loc2,on_hull,n_neighbours)
            np = np - 1


     100    format(/'Error in subroutine nn_remove:'//, &
                   'Only three nodes in triangulation'/, & 
                   'Cannot remove a node'/) 

        return
        end subroutine
    !
    !------------------------------------------------------------------------
    !
    !   renumber_node  - renumbers node `node' by `node_new' in
    !            arrays vertices and neighbour and points.
    !
    !   Input:  
    !               node                    index of node to be removed
    !               node_new                index of node to be added 
    !               points(2,np)            array of node co-ordinates
    !           vertices(3,nt)      array of triangle vertices  
    !       neighbour(3,nt)     adjacency array
    !       loc         a first guess at a triangle
    !                   attached to input node `node'
    !
    !   Output:
    !           vertices(3,nt)      updated 
    !       n_neighbours        number of neighbours of `node' 
    !       n_triangles     number of triangles about `node'
    !       on_hull         logical = .true. if node is 
    !                   on the convex hull
    !
    !   Comments:
    !        This routine returns the vertex and adjacency arrays with
    !        all references to node `node' replaced by `node_new'.
    !        This is done using only local loops (i.e. no loop
    !        over all triangles). Extra information in the form
    !        of n_neighbours, n_triangles, on_hull is, returned and 
    !        may be useful. 
    !
    !        Calls are made to node_loc.
    !
    !                   M. Sambridge, RSES, Jan. 1996.
    !
    !------------------------------------------------------------------------
    !
        Subroutine renumber_node &
                      (node,node_new,points,vertices,neighbour, &
                       loc,on_hull,n_neighbours)

            use m_del_sub

            integer(4) :: loc,node,k,node_new,n_neighbours

            real*8      points(2,*)
            integer     vertices(3,*)
            integer     neighbour(3,*)
            integer     t,t0,tnew
            integer     p,p0,pnew
            integer     c1(3)
            integer     c2(3)
            logical     on_hull
            logical     debug_mode
            common      /debug_nn_remove/debug_mode
            data        c1/2,3,1/
            data        c2/3,1,2/

    !                   find a single triangle 
    !                   attached to the input node

            t0 = loc

            if(debug_mode)then
               write(*,*)' '
               write(*,*)' start renumber_node node = ',node
               write(*,*)' input to node_loc t0 =',t0
            end if

        call node_loc(node,points,vertices,neighbour,t0,p0)

            if(debug_mode) write(*,*)' output of node loc t0 =',t0,' pos =',p0

    !                   find remaining nodes and
    !                   triangles attached to input node
            on_hull = .false.
            k = 2
            t = t0
            p = p0
            vertices(p,t) = node_new
      5     tnew = neighbour(c1(p),t)
            if(debug_mode)then
               write(*,*)'first k',k
               write(*,*)'first tnew',tnew
            end if
            if(tnew.eq.0)then
               if(debug_mode)write(*,*)' reached hull on first sweep'
               on_hull = .true.
               t = t0
               p = p0
      6        tnew = neighbour(c2(p),t)
               if(debug_mode)then
                  write(*,*)'second k',k
                  write(*,*)'second tnew',tnew
                  if(tnew.eq.0)write(*,*)' reached hull on second sweep'
               end if
               if(tnew.eq.0)go to 10
               pnew = edg(tnew,node,vertices)
               k = k + 1
               t = tnew
               p = pnew
               vertices(p,t) = node_new
               go to 6
            else if(tnew.eq.t0)then
               k = k - 1
               go to 10
            end if
            pnew = edg(tnew,node,vertices)
            k = k + 1
            t = tnew
            p = pnew
            vertices(p,t) = node_new
            go to 5
     10     continue

            n_neighbours = k

    !                       renumber node in 
    !                       points array
            points(1,node_new) = points(1,node)
            points(2,node_new) = points(2,node)

            if(debug_mode)then
               write(*,*)' '
               write(*,*)' done renumber_node '
               write(*,*)' '
            end if

        return
        end subroutine
    !
    !------------------------------------------------------------------------
    !
    !   find_neighbours_r - finds the list of nodes that are neighbours
    !               of an input node and the triangles attached to
    !               that node.
    !
    !   Input:  
    !               node                    input node 
    !               points(2,np)            array of node co-ordinates
    !           vertices(3,nt)      array of triangle vertices  
    !       neighbour(3,nt)     adjacency array
    !       loc         a first guess at a triangle
    !                   attached to input node
    !
    !   Output:
    !       n_neighbours        number of neighbours of node 
    !       n_triangles     number of triangles attached to node
    !           nodelist        list of neighbours to input node 
    !           tlist               list of triangles attached to input node
    !       on_hull         logical = .true. if node is on the
    !                   convex hull
    !
    !   Comments:
    !        Arrays must be dimensioned in the following way
    !
    !        integer nodelist(nnpn_max)    
    !        integer tlist(nnpn_max)      
    !
    !        where nnpn_max is the maximum number of neighbours per node.
    !
    !        An error is reported if the number of neighbours of
    !        the input node is greater than nnpn_max. This is 
    !        necessary because in this case work arrays may not be 
    !        large enough.
    !
    !        Calls are made to node_loc.
    !
    !                   M. Sambridge, RSES, Dec. 1995.
    !
    !------------------------------------------------------------------------
    !
        Subroutine find_neighbours_r &
                       (node,points,nnpn_max,vertices,neighbour, &
                        loc,on_hull,n_neighbours,n_triangles,nodelist,tlist)

            use m_del_sub

            integer(4) :: loc,node,k,kk,n_neighbours,nnpn_max,n_triangles,i

            real*8      points(2,*)
            integer     vertices(3,*)
            integer     neighbour(3,*)
            integer     tlist(*)
            integer     nodelist(*)
            integer     t,t0,tnew
            integer     p,p0,pnew
            integer     c1(3)
            integer     c2(3)
            logical     on_hull
            data        c1/2,3,1/
            data        c2/3,1,2/
            logical     debug_mode
            common      /debug_nn_remove/debug_mode

    !                   find a single triangle 
    !                   attached to the input node

            t0 = loc
            if(debug_mode)then
               write(*,*)' '
               write(*,*)' start find_neighbours '
               write(*,*)' start find_neighbours node = ',node
               write(*,*)' input to node_loc t0 =',t0
            end if

        call node_loc(node,points,vertices,neighbour,t0,p0)

            if(debug_mode) write(*,*)' output of node loc t0 =',t0,' pos =',p0

    !                   find remaining nodes and
    !                   triangles attached to input node
            on_hull = .false.
        k = 1
            tlist(k) = t0
            nodelist(k) = vertices(c1(p0),t0)
            k = k + 1
            nodelist(k) = vertices(c2(p0),t0)
            t = t0
            p = p0
      5     tnew = neighbour(c1(p),t)
            if(debug_mode)then
               write(*,*)'first k',k,' tlist',(tlist(kk),kk=1,k-1)
               write(*,*)'first nodelist',(nodelist(kk),kk=1,k)
               write(*,*)'first tnew',tnew
            end if
            if(tnew.eq.0)then
               if(debug_mode)write(*,*)' reached hull on first sweep'
               on_hull = .true.
               t = t0
               p = p0
      6        tnew = neighbour(c2(p),t)
               if(debug_mode)then
                  write(*,*)'second k',k,' tlist',(tlist(kk),kk=1,k-1)
                  write(*,*)'second nodelist',(nodelist(kk),kk=1,k)
                  write(*,*)'second tnew',tnew
                  if(tnew.eq.0)write(*,*)' reached hull on second sweep'
               end if
               if(tnew.eq.0)go to 10
               tlist(k) = tnew
               pnew = edg(tnew,node,vertices)
               k = k + 1
               nodelist(k) = vertices(c1(pnew),tnew)
               t = tnew
               p = pnew
               go to 6
            else if(tnew.eq.t0)then
               nodelist(k) = 0
               k = k - 1
               go to 10
            end if
            tlist(k) = tnew
            pnew = edg(tnew,node,vertices)
            k = k + 1
            nodelist(k) = vertices(c2(pnew),tnew)
            t = tnew
            p = pnew
            go to 5
     10     continue

            n_neighbours = k
    !                       check size of nnpn_max

            call check_nnpn_max(n_neighbours,node,nnpn_max)

            if(on_hull)then
               n_triangles = n_neighbours-1
            else
               n_triangles = n_neighbours
            end if

            if(debug_mode)then
               write(*,*)' on_hull =',on_hull
               write(*,*)' n_neighbours =',n_neighbours
               write(*,*)' nodes:',(nodelist(i),i=1,n_neighbours)
               write(*,*)' triangles:',(tlist(i),i=1,n_triangles)
               write(*,*)' '
               write(*,*)' done find_neighbours '
               write(*,*)' '
            end if

        return
        end subroutine
    !
    !------------------------------------------------------------------------
    !
    !   Node_loc - locates a triangle attached to a particular node
    !
    !   Input:
    !       node            input node
    !       points(2,np)        array of node points    
    !           vertices(3,nt)      array of triangle vertices  
    !           neighbour(3,nt)     array of neighbouring triangles.    
    !                   Neighbour(i,j) is the triangle
    !                   opposite node i in triangle j,
    !                   stored counterclockwise about j.
    !           loc         first guess of triangle containing
    !                   input node
    !
    !   Output:
    !           loc         index of triangle containing 
    !                   input point.
    !           pos         position of node in triangle
    !
    !   Comments:
    !
    !        No calls to other routines.
    !
    !                   M. Sambridge, RSES, April 1994.
    !
    !------------------------------------------------------------------------
    !
        Subroutine node_loc(node,points,vertices,neighbour,loc,pos)
    !
            integer(4) :: i,k,loc,node,j

            real*8      points(2,*)
            integer     vertices(3,*)
            integer     neighbour(3,*)
            integer     p1,p2
            integer     pos
            real*8      x,y,del1,del2
            integer     c1(3)
            data        c1/2,3,1/
    !
    !                       check starting triangle
            do i=1,3
               k = vertices(i,loc)
               if(node.eq.k)then
                 pos = i
                 return
               end if
            end do

            x = points(1,node)
            y = points(2,node)

     10     continue

            do 20 i=1,3
           j = c1(i)
           k = c1(j)
               p1 = vertices(i,loc)
               p2 = vertices(j,loc)
           del1 = (points(2,p1)-y)*(points(1,p2)-x)
           del2 = (points(1,p1)-x)*(points(2,p2)-y)
           if(del1.gt.del2)then
              if(neighbour(k,loc).eq.0)then
                     write(*,*)' '
                     write(*,*)' Error in subroutine node_loc'
                     write(*,*)' '
                     write(*,*) ' Cannot find a triangle attached to node',node
                     write(*,*)' Node is possibly outside of convex hull'
                     write(*,*)' or already removed from the list ?'
                     stop
              else
                 loc = neighbour(k,loc)
              end if
              go to 10
           end if
     20     continue
        
    !                       find position in triangle
            do i=1,3
               k = vertices(i,loc)
               if(node.eq.k)then
                 pos = i
                 return
               end if
            end do

            write(*,*)' '
        write(*,*)' Error in subroutine node_loc'
            write(*,*)' '
            write(*,*)' Cannot find a triangle attached to node',node
            write(*,*)' Has node already removed from the list ?'
        write(*,*)' Last triangle encountered : ',loc
        write(*,*)' with vertex nodes         : ', (vertices(k,loc),k=1,3)

            stop
        end subroutine
    !
    !------------------------------------------------------------------------
    !
    !   find_ctriangles - finds the circum-triangles of input node among
    !             its set of neighbouring nodes (stored in nodelist).
    !
    !   Input:  
    !               node                    input node 
    !               np                      number of nodes
    !               points(2,np)            array of node co-ordinates
    !           nt          number of triangles
    !           vertices(3,nt)      array of triangle vertices  
    !       neighbour(3,nt)     adjacency array
    !           nodelist        list of neighbours to input node 
    !       vis_tlist       work array used by delaun
    !       vis_elist       work array used by delaun
    !       add_elist       work array used by delaun
    !       nv_max          size of work arrays
    !       np_local        number of neighbours of node 
    !       on_hull         logical: true if point is on convex hull
    !
    !   Output:
    !       v_local         array of triangle vertices between
    !                   neighbours of node
    !       n_local         local adjacency array 
    !       c_list          list of triangles in v_local
    !                   which are circumcircles of node
    !       nt_local        number of triangles attached to node
    !
    !   Comments:
    !        This routine calculates the Delaunay triangulation of
    !        the neighbours of the input node. The result is put into
    !        v_local and n_local. It also determines which of these
    !        local triangles are not circum-triangles of the input node
    !        and flags these in the logical array c_list.
    !        This information can be used to remove the input node
    !        from the vertex and neighbour lists of the original 
    !        triangulation.
    !
    !        Assumes input list of vertices in anticlockwise order
    !
    !        The arrays must be dimensioned in the following way:
    !
    !        integer nodelist(nnpn_max)    
    !        integer tlist(nnpn_max)      
    !        logical c_list(2*nnpn_max)   
    !        integer v_local(3,2*nnpn_max) 
    !        integer n_local(3,2*nnpn_max) 
    !            integer vis_tlist(nv_max)     : work array
    !        integer vis_elist(nv_max)     : work array
    !            integer add_tlist(nv_max)     : work array
    !
    !        where nnpn_max is the maximum number of neighbours per node. 
    !        (Note a strict upper bound on the number of triangles 
    !        attached to a node is 2*nnpn_max-2.) 
    !
    !        nv_max is the size of the work arrays vis_tlist,
    !        vis_elist, add_tlist, used by delaun. In this case
    !        delaun is only being used for a maximum of nnpn_max points
    !        and so nv_max <= nnpn_max.
    !
    !        Calls are made to delaun.
    !
    !                   M. Sambridge, RSES, Jan. 1996.
    !
    !------------------------------------------------------------------------
    !
        Subroutine find_ctriangles (node,np,points,nodelist, &
                   np_local,on_hull,nnpn_max,nv_max,vis_tlist,vis_elist, &
                   add_tlist,nt_local,v_local,n_local,c_list)

                use m_delaun

            integer(4) :: node,np,nt_local_max,nnpn_max,np_local,nt_local
            integer(4) :: nv_max,i,j,k

            integer     nodelist(*)
            integer     v_local(3,*)
            integer     n_local(3,*)
            integer         vis_tlist(*)
            integer         vis_elist(*)
            integer         add_tlist(*)
            logical         c_list(*)
            logical         on_hull
            real*8      points(2,*)
            real*8      circle(3)
            real*8      x,y,dx,dy,dist
            real*8      eps
            logical     debug_mode
            logical     ldummy(np_local)
            common      /debug_nn_remove/debug_mode
            common          /roundoff/eps
    !                       check error conditions 
            if(node.gt.np)then
               write(*,100)node,np
               stop
            end if
            if(node.lt.1)then
               write(*,200)node
               stop
            end if

            if(eps.eq.0.d0)eps = 1.E-9_8
            nt_local_max = 2*nnpn_max

    !                       calculate Delaunay 
    !                       triangulation of neighbours

            if(np_local.gt.2)then
           call delaun &
                        (points,np_local,n_local,v_local,nt_local, &
                         nt_local_max,vis_tlist,vis_elist, &
                         add_tlist,eps,nv_max,2,ldummy,0,0,nodelist)
    !
    !      call delaun_subset 
    !    &              (points,np_local,n_local,v_local,nt_local,
    !    &               nt_local_max,nodelist,vis_tlist,vis_elist,
    !    &               add_tlist,eps,nv_max)
            else
               nt_local = 0
            end if

    !                                               write local Delaunay and nodes
            if(debug_mode)then
           write(*,*)' node = ',node
           write(*,*)' np_local',np_local,' nt_local',nt_local
           write(*,*)' neighbours '
           write(*,*)(nodelist(i),i=1,np_local)
               do j=1,nt_local
              write(*,*)' v:',(v_local(k,j),k=1,3), ' n:',(n_local(k,j),k=1,3)
               end do

    !                                               draw local Delaunay and nodes
    !          call xpen(2)
    !          call draw_nodes_sub
    !    &          (np_local,nodelist,points,0.0075)
    !          call xpen(3)
    !          call xthick(2.)
    !          call draw_delaunay
    !    &          (np_local,points,nt_local,v_local)
            end if

    !                       If the number of triangles
    !                       differs from np-2 then
    !                       we must have a convex
    !                       set of neighbours and we
    !                       need to remove some
    !
        if(nt_local.eq.np_local-2.and..not.on_hull)then
               do i=1,nt_local
                  c_list(i) = .true.
               end do
               if(debug_mode)then
                  write(*,*)' No extra triangles to remove'
                  write(*,*)' The set of neighbours is convex'
               end if
            else
    !                       find local triangles 
    !                       whose circumcircles do not
    !                       contain node node
               do j=1,nt_local
    !                       determine if node node is
    !                       inside circum-circle of
    !                       current triangle

                  call circum_centre(j,v_local,points,circle)
                  x = points(1,node)
                  y = points(2,node)
                  dx = circle(1)-x
                  dy = circle(2)-y
                  dist = dx*dx + dy*dy
          
                  if(dist.gt.circle(3))then
                     if(debug_mode)write(*,*)j,' not circum-triangle'
                     c_list(j) = .false.
                  else
                     c_list(j) = .true. 
                     if(debug_mode)write(*,*)j,' circum-triangle'
                  end if
               end do
    !                                               write local Delaunay and nodes
           if(debug_mode)then
                  write(*,*)' reduced triangles'
                  do j=1,nt_local
                     if(c_list(j))then
                    write(*,*)' v:',(v_local(k,j),k=1,3), ' n:',(n_local(k,j),k=1,3)
                     end if
                  end do
           end if

        end if

     100    format(/' Error in subroutine find_ctriangles:'//,  &
                    ' Node number is greater than the number of nodes'/,  &
                    ' Input node:',i8,' Number of nodes:',i8/) 

     200    format(/' Error in subroutine find_ctriangles:'//, &
                    ' Node number is negative '/, &
                    ' Input node:',i8/)

        return
        end subroutine
    !
    !------------------------------------------------------------------------
    !
    !   update_vn_lists - Removes the triangles attached to node `node'
    !             from vertices and replaces them with those in 
    !             v_local. It also updates the adjacency array 
    !             neighbour using n_local.
    !
    !   Input:  
    !               node                    input node 
    !           nt          number of triangles
    !           vertices(3,nt)      array of triangle vertices  
    !       neighbour(3,nt)     adjacency array
    !       v_local         array of triangle vertices between
    !                   neighbours of node
    !       n_local         adjacency array for v_local 
    !       c_list          list of triangles in v_local
    !                   which are circumcircles of node
    !       nt_local        number of local triangles
    !           tlist               list of triangles attached to input node
    !       nnpn_max        maximum number of neighbours per node
    !
    !   Output:
    !           nt          updated number of triangles
    !           vertices(3,nt)      updated array of triangle vertices  
    !       neighbour(3,nt)     updated adjacency array
    !
    !   Comments:
    !        This routine updates the vertex and adjacency lists
    !        by removing the triangles in v_local. It is used in
    !        conjunction with the routine find_ctriangles to remove
    !        a node from a 2-D Delaunay triangulation.
    !
    !        The algorithm updates vertices and neighbour using only 
    !        local loops, i.e. without a global loop over all triangles. 
    !        Only triangles in c_list are considered, because these are 
    !        the only ones which are circum_circles of the input node. 
    !
    !        Assumes input list of vertices in anticlockwise order
    !
    !        The arrays must be dimensioned in the following way:
    !
    !        integer tlist(nnpn_max) 
    !        logical c_list(2*nnpn_max)    
    !        integer v_local(3,2*nnpn_max)
    !        integer n_local(3,2*nnpn_max) 
    !
    !        It is assumed that the maximum size of the local arrays
    !        v_local(3,*), n_local(3,*) and c_list(*) 
    !                (which store local triangles) is 2*nnpn_max, 
    !        where nnpn_max is the maximum number of 
    !        neighbours per node. (Note: a strict upper bound on 
    !        the number of triangles attached to a node is 2*nnpn_max-2) 
    !
    !        Only uses local loops
    !
    !                Calls no other routines.
    !
    !                   M. Sambridge, RSES, Dec. 1995.
    !
    !------------------------------------------------------------------------
    !
        Subroutine update_vn_lists &
                (node,nt,vertices,neighbour,tlist, &
                 on_hull,np_local, &
                 nt_local,v_local,n_local,c_list)

            use m_del_sub

            integer(4) :: node,nt,i,j,k,nt_local,j1,j2,np_local,nt_old
            integer(4) :: n1,n2,ip,jt,m1,m2,nn,m,ii,nt_done,nt_rem,k1,k2,k3
            integer(4) :: it

            integer     vertices(3,*)
            integer     neighbour(3,*)
            integer     v_local(3,*)
            integer     n_local(3,*)
            logical         c_list(*)
            integer         tlist(*)
            integer     c1(3)
            integer     c2(3)
            integer     t
            logical         on_hull
            data        c1/2,3,1/
            data        c2/3,1,2/
            logical         debug_mode
            common      /debug_nn_remove/debug_mode
    !                       write out v and n lists
            if(debug_mode)then
               write(*,*)' '
               write(*,*)' Inside update_vn_lists'
               write(*,*)' '
               write(*,*)' node = ',node
               write(*,*)' nt =',nt
               do j=1,nt
              write(*,*)' v:',(vertices(k,j),k=1,3), ' n:',(neighbour(k,j),k=1,3)
               end do
               write(*,*)' '
               write(*,*)' nt_local =',nt_local
               do j=1,nt_local
              write(*,*)' vl:',(v_local(k,j),k=1,3), ' nl:',(n_local(k,j),k=1,3)
               end do
               write(*,*)' '
            end if
    !
    !                       renumber local triangles 
    !                       to negative values to avoid
    !                       confusion with global triangles
    !                       having the same number
        do i=1,nt_local
               n_local(1,i) = -n_local(1,i)
               n_local(2,i) = -n_local(2,i)
               n_local(3,i) = -n_local(3,i)
            end do

    !
    !                       Procedure to update neighbour
    !                       and vertices lists in 3 stages
    !                       
    !                       stage 1:
    !                       update n_local and neighbour
    !                       for exterior edges of
    !                       local triangulation
    !                       (using negative local indices)
            j1 = 1
            j2 = np_local
            if(on_hull)j2 = np_local-1
            nt_old = j2-j1+1
            if(debug_mode) write(*,*)' nt_old=',nt_old,' nt_local =',nt_local

        do i = 1,nt_local
               if(c_list(i))then
                  do j=1,3
                     t = -n_local(j,i) 
                     if(t.eq.0.or..not.c_list(t))then
                       n1 = v_local(c1(j),i)
                       n2 = v_local(c2(j),i)
                       do ip = j1,j2
                          it = tlist(ip)
                          do jt = 1,3
                             m1 = vertices(c1(jt),it)
                             m2 = vertices(c2(jt),it)
                             if(m1.eq.n1.and.m2.eq.n2)then
                                nn = neighbour(jt,it)
                                n_local(j,i) = nn
                                if(nn.ne.0)then
                                   if(debug_mode) &
                                   write(*,*)' found a match',m1,m2,n1,n2, &
                                   ' local t',i,' old t',it, &
                                   ' neighbour',n_local(j,i)
                                   neighbour(edg(nn,it,neighbour),nn) = -i
                                end if
                             else if(m1.eq.n2.and.m2.eq.n1)then
                                nn = neighbour(jt,it)
                                n_local(j,i) = nn
                                if(nn.ne.0)then
                                   if(debug_mode) &
                                   write(*,*)' found a match',m1,m2,n1,n2, &
                                   ' local t',i,' old t',it, &
                                   ' neighbour',n_local(j,i)
                                   neighbour(edg(nn,it,neighbour),nn) = -i
                                end if
                             end if
                          end do
                       end do
                     end if
                  end do
               end if
            end do
    !                       for points on the hull
    !                       set to zero the adjacency
    !                       entries for all neighbours of
    !                       triangles attached to input 
    !                       node that are not already
    !                       attached to new triangles 
            if(on_hull)then
               do i=1,np_local-1 
                  t = tlist(i)
                  j = neighbour(edg(t,node,vertices),t)
                  do k=1,3
                     if(neighbour(k,j).eq.t)then
                        if(debug_mode) &
                        write(*,*)' reseting triangle ',j, &
                        ' neighbour',t,' to zero' 
                        neighbour(k,j)=0
                        go to 6
                     end if
                  end do
     6            continue
               end do
            end if

        if(debug_mode)then
               write(*,*)' after stage 1'
               do j=1,nt
              write(*,*)' v:',(vertices(m,j),m=1,3), ' n:',(neighbour(m,j),m=1,3)
               end do
               do j=1,nt_local
              write(*,*)' vl:',(v_local(m,j),m=1,3), ' nl:',(n_local(m,j),m=1,3)
               end do
            end if
    !                       stage 2:
    !                       replace old triangles with new          
            k = j1
        do i = 1,nt_local
               if(c_list(i))then
                  t = tlist(k)
                  k = k + 1
                  if(debug_mode) write(*,*)' triangle',-i,' becomes triangle',t
                  do j=1,3
                     vertices(j,t) = v_local(j,i)
                     neighbour(j,t) = n_local(j,i)
                     ii = n_local(j,i)
                     if(ii.lt.0)then
                        n_local(edg(-ii,-i,n_local),-ii) = t
                     else if(ii.gt.0)then
                        neighbour(edg(ii,-i,neighbour),ii) = t
                     end if
                  end do
                end if
            end do
            nt_done = k-1
            nt_rem = nt_old-k+j1

            if(debug_mode)then
               write(*,*)' Number of triangles removed =', k-j1,' nt_old=',nt_old
               write(*,*)' Number remaining =',nt_rem
           write(*,*)' after stage 2'
               do j=1,nt
              write(*,*)' v:',(vertices(m,j),m=1,3), ' n:',(neighbour(m,j),m=1,3)
               end do
            end if
     
    !                       stage 3:
    !                       swap last triangles (nt,nt-1,..)
    !                       into remaining vacant triangle 
    !                       positions 
    !                       (but do not do so if they are
    !                        already in the vacant list)
            k1 = k
            k2 = nt_rem+(k1-1)
            k3 = nt-nt_rem
            do 10 i=1,nt_rem
               it = nt-(i-1)
    !                       determine if triangle to be
    !                       moved is already in vacant list 
    !                       If so then ignore
               do j=k1,k2
                  t = tlist(j)
                  if(it.eq.t)go to 10
               end do
    !                       If not then find 
    !                       next vacant slot
               do j = k,k2
                  t = tlist(j)
                  if(t.le.k3)then
                     if(debug_mode) write(*,*)' triangle',it,' becomes triangle',t
                     do m=1,3
                        vertices(m,t) = vertices(m,it)
                        neighbour(m,t) = neighbour(m,it)
                        ii = neighbour(m,it)
                        if(ii.ne.0)neighbour(edg(ii,it,neighbour),ii) = t
                     end do
                     k = j+1
                     go to 10
                  end if
               end do
               write(*,*)' Error in subroutine update_vn_lists'
               write(*,*)' Error no vacant slots'
               write(*,*)' t =',t,' k=',k,' k2=',k2
               stop
     10     continue

    !                       update number of triangles
            nt = it-1
    !                       write out v and n lists

            if(debug_mode)then
               write(*,*)' after stage 3: New nt = ',nt
               do j=1,nt
              write(*,*)' v:',(vertices(m,j),m=1,3), ' n:',(neighbour(m,j),m=1,3)
               end do
               write(*,*)' '
            end if

        return
        end subroutine
    !
    !------------------------------------------------------------------------
    !
    !       circum_centre - calculates circum-centre of triangle t 
    !
    !
    !       Input:
    !               t                       triangle 
    !               points(2,np)            array of node points
    !               vertices(3,nt)          array of triangle vertices
    !
    !       Output:
    !               centre(3)               centres(j,i) (j=1,2) contains the
    !                                       co-ordinates of the centre of
    !                                       circumcircle about Delaunay
    !                                       triangle i, (i=1,...,nt),
    !                                       j=3 contains squared radius of circle.
    !       Comments:
    !
    !                No calls to other routines.
    !
    !                                       M. Sambridge, RSES, April 1994.
    !
    !------------------------------------------------------------------------
    !
            Subroutine circum_centre(t,vertices,points,centre)
    !
            real*8          points(2,*)
            real*8          centre(3)
            real*8          x1,x2,x3,y1,y2,y3,x,y
            real*8          dx2m1,dx2p1,dy2m1,dy2p1
            real*8          dx3m1,dx3p1,dy3m1,dy3p1
            integer         vertices(3,*)
            integer         t
    !                                               Find centres of 
    !                                               Delaunay circumcircle
            x1 = points(1,vertices(1,t))
            x2 = points(1,vertices(2,t))
            x3 = points(1,vertices(3,t))
            y1 = points(2,vertices(1,t))
            y2 = points(2,vertices(2,t))
            y3 = points(2,vertices(3,t))
     
            dx2m1 = x2-x1
            dx2p1 = x2+x1
            dy2m1 = y2-y1
            dy2p1 = y2+y1
            dx3m1 = x3-x1
            dx3p1 = x3+x1
            dy3m1 = y3-y1
            dy3p1 = y3+y1
            x = ((dx2m1*dx2p1 + dy2m1*dy2p1)*dy3m1*0.5d0 &
                -(dx3m1*dx3p1 + dy3m1*dy3p1)*0.5d0*dy2m1)/ &
                (dx2m1*dy3m1-dx3m1*dy2m1)
     
            y = (dx2m1*(dx3m1*dx3p1 + dy3m1*dy3p1)*0.5d0 &
                -dx3m1*(dx2m1*dx2p1 + dy2m1*dy2p1)*0.5d0)/ &
                (dx2m1*dy3m1-dx3m1*dy2m1)
     
            centre(1) = x
            centre(2) = y
            x1 = x - x1
            y1 = y - y1
            centre(3) = x1*x1 + y1*y1
     
            return
            end subroutine
    !
    !------------------------------------------------------------------------
    !
    !   check_nnpn_max - performs an error check on the size of nnpn_max
    !
    !   Input:  
    !           numn            number of neighbours            
    !           node            node index 
    !       nnpn_max        maximum number of neighbours for 
    !                   all nodes
    !   Output:
    !
    !   Comments:
    !
    !        No calls to other routines.
    !
    !                   M. Sambridge, RSES, Jan. 1996.
    !
    !------------------------------------------------------------------------
    !
        Subroutine check_nnpn_max(numn,node,nnpn_max)

            integer(4) :: numn,node,nnpn_max

        if(numn.gt.nnpn_max)then
               write(*,*)' '
               write(*,*)' Error - maximum number of neighbours'
               write(*,*)'         per node is not large enough'
               write(*,*)' '
               write(*,*)' Node',node,' has ',numn,' neighbours'
               write(*,*)' nnpn_max currently set to',nnpn_max
               write(*,*)' Work arrays are not big enough'
               write(*,*)' '
               write(*,*)' Remedy - increase size of parameter'
               write(*,*)'          nnpn_max in main program'
               write(*,*)' '
               stop
            end if

        return
        end subroutine
    end module m_nn_remove

