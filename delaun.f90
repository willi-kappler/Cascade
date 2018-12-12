        module m_delaun
            contains

!
!------------------------------------------------------------------------
!
!   delaun - calculates delaunay triangulation incrementally 
!        for a set of points in 2-D using a variation of
!        Lawson's algorithm.
!
!   Input:
!       points(2,np)        array of node co-ordinates
!       num         number of nodes to be used
!               vis_tlist(nv_max)       List of triangles visible from 
!                   current point.
!               vis_elist(nv_max)       List of edges visible from 
!                   current point.
!               add_tlist(nv_max)       work array used by routine addpoint
!               eps                     distance from an interface for a
!                                       a point to be considered on an
!                                       interface (real*8). Prevents zero
!                                       area triangles resulting from rounding
!                                       error when nodes are co-linear.
!       nv_max          size of work arrays
!       mode            (=0,1,2,3) operation mode (see below)
!       inactive(np)        logical array. If mode=1 then the i-th
!                   node is ignored if active(i) = .true.
!       nfirst          If mode=3 then nfirst is the first
!                   node added to an existing triangulation
!       itstart         If mode=3 then itstart is a first
!                   guess triangle containing node first node
!       subset(np)      logical array. If mode=2 then only
!                   the nodes (subset(i),i=1,num) are used.
!
!   Output:
!               v(3,*)              array of triangle vertices
!               numtri                  number of triangles in current
!                                       triangulation.
!               e(3,*)                  adjacency matrix of neighbouring
!                                       triangles. e(i,j) is the triangle
!                                       which shares the face containing
!                                       node (mod(i,3)+1) and (mod(i,3)+2) 
!                   in triangle j, stored counterclockwise 
!                   about j.  
!                                       (This is the `opposite' definition)
!
!   Comments:
!
!       This routine calculates the Delaunay triangulation of a set of nodes 
!   using a variation of Lawson's method.  Each node is added sequentially 
!   and the Delaunay triangulation is updated. If the new node is inside 
!   the convex hull of the existing triangulation then the standard Lawson 
!   method is used. If it is outside then the list of triangle edges 
!   which are visible from the new point is calculated using routine 
!   visiblelist and each of these is used as the start of the swapping 
!   routine addpoint.
!
!   Four different operation modes are allowed.
!
!   MODE = 0:
!   The `standard' mode. All nodes from 1 to num are included. The arrays 
!   `subset' and `inactive' are unused and may be set to dummy variables
!       (saving memory). The variables nfirst and itstart are also unused.
!
!   MODE = 1:
!   All nodes from 1 to num are included except those for which
!   inactive(i) is set to true. The array `subset' is unused and may
!   be set to a dummy variable (saving memory). The variables nfirst 
!   and itstart are also unused.
!
!   MODE = 2:
!   Only nodes from subset(1) to subset(num) are included. The array
!   `inactive' is unused and may be set to a dummy variable
!   (saving memory). The variables nfirst and itstart are also unused.
!
!   MODE = 3:
!   Used to add nodes from nfirst to num to an existing triangulation.
!   Nodes for which inactive(i) is set to true are ignored.
!   The array `subset' is unused and may be set to a dummy variable.
!
!       The performance may be sensitive to the order in which the nodes are
!       added so these can be sorted before calling this routine if desired.
!
!   This routine was converted to use the `opposite' definition of
!   the adjacency matrix on 30/1/96.
!
!
!   Calls are made to Triloc_del,visiblelist,insert_point,addpoint.
!
!                            M. Sambridge, Dec. 1994.
!                   Modified by J. Braun, Sept. 1995.
!                   (last change 30/1/96: multiple modes,
!                    and uses opposite definition of
!                    adjacency matrix)
!
!------------------------------------------------------------------------
!
    Subroutine delaun (points,num,e,v,numtri,numtri_max, &
                           vis_tlist,vis_elist,add_tlist,eps,nv_max, &
                           mode,inactive,nfirst,itstart,subset)

        use m_del_sub

    real*8      points(2,*)
    real*8      x,y
    real*8          eps,del1,del2,del
    integer     vis_tlist(*),vis_elist(*),add_tlist(*)
    integer     v(3,*)
    integer     e(3,*)
    integer(4) ::     subset(*),mode,i1,i2,nodestart,i,num,istart,k,itemp,numtri,nfirst
    integer(4) :: ipos,iface,nvis,nv_max,jpos,numtri_max,numtrimax_max,j,itstart
    integer  t,p
    logical         out
    logical     newpoint
    logical     inactive(*)
    istart = 1
    nodestart = 1

        if (mode.eq.0.or.mode.eq.1.or.mode.eq.2) then

!                   We are calculating Delaunay 
!                   of all input points or a
!                   subset of all input points

!                   find first two active nodes
           if(mode.eq.0)then
              i1=1
              i2=2
              nodestart=3
           else if(mode.eq.1)then
              i1=0
              i2=0
              do i=1,num
                 if (i2.ne.0) goto 2222
                 if (i1.ne.0.and..not.inactive(i)) i2=i
                 if (i1.eq.0.and..not.inactive(i)) i1=i
              end do
 2222         continue
              nodestart = i2+1
           else if(mode.eq.2)then
              i1 = subset(1)
              i2 = subset(2)
              nodestart = 3
           end if
!                                       Find three non-colinear points
!                                       to form the first triangle 
           v(1,1) = i1
           v(2,1) = i2
           do 10 j=nodestart,num
              i = j
              if(mode.eq.2)then
                  i = subset(j)
              else if(mode.eq.1.and.inactive(i))then
                  go to 10
              end if
              istart=i
          del1 = (points(2,i1)-points(2,i))*(points(1,i2)-points(1,i))
          del2 = (points(1,i1)-points(1,i))*(points(2,i2)-points(2,i))
              del = del1-del2
              if(dabs(del).gt.eps) goto 11111
 10        continue
           stop 'all input data are in a line...'
11111      v(3,1) = istart

!                   Initialize adjacency matrix
       e(1,1) = 0
       e(2,1) = 0
       e(3,1) = 0
!                   Ensure initial triangle 
!                   is in ccw order
!                   
       if(ccw(points(1,v(1,1)),points(1,v(2,1)),points(1,v(3,1)),k).eq.-1)then
                  itemp = v(1,1)
                  v(1,1) = v(2,1)
                  v(2,1) = itemp
!             write(*,*)' initial triangle was cw'
        end if
    
!                   Initialize variables
        numtri = 1
        t = 1

        else if (mode.eq.3) then
!                   We are adding nodes to an 
!                   existing triangulation
!                   Perform initialization
           nodestart=nfirst
           t = itstart
           istart = 0
           if(t.le.0.or.t.gt.numtri)t=1

        end if 
!                   Incrementally update the 
!                   Delaunay triangulation

    do 100 j=nodestart,num


           p = j
           if(mode.eq.1.or.mode.eq.3)then
             if (inactive(j)) goto 100
           else if(mode.eq.2)then
         p = subset(j)
           end if
             
           if(p.eq.istart)go to 100

       x = points(1,p)
       y = points(2,p)

!                   locate triangle 
!                   containing current node

       call Triloc_del(x,y,points,v,e,t,eps,out,ipos,iface)


       if(out)then

!                   point is outside of convex hull, 
!                   so find list of edges that are 
!                   visible from current point 

          call visiblelist(points,e,v,x,y,t,ipos,eps,vis_tlist,vis_elist,nvis)

!                   for each visible edge 
!                   start swapping algorithm

              newpoint = .true.

          if(nvis.gt.nv_max)then
                 write(*,*)' Error in subroutine delaun:'
                 write(*,*)' Too many visible triangles from current point'
                 write(*,*)' Remedy: increase size of parameter nv_max'
                 write(*,*)'         in calling program'
                 write(*,*)'         Number of visible triangles '
                 write(*,*)'         for this point             =',nvis
                 write(*,*)'         Current value of nv_max    =',nv_max
                 stop
              end if

          do 60 i=1,nvis
                 t = vis_tlist(i)
                 ipos = vis_elist(i)
                 jpos = mod(vis_elist(i),3)+1
!            write(6,*)' visible t =',t,' node',v(ipos,t),v(jpos,t)
             call addpoint (points,e,v,p,t,ipos,numtri,newpoint,add_tlist) 
                 newpoint = .false.
 60           continue

       else
 
!         write(6,*)' point located in triangle',t

!                   add node to inside of convex hull
!                   using swapping algorithm

          call insertpoint(points,e,v,p,t,numtri,iface) 

           end if

           if (numtri.gt.numtri_max) then
              write (*,*) 'Error in subroutine delaun:'
              write (*,*) 'Too many triangles'
              write(*,*)' Remedy: increase size of parameter numtri_max'
              write(*,*)'         in calling program'
              write(*,*)'         Number of triangles '
              write(*,*)'         for this point             =',numtri
              write(*,*)'         Current value of numtri_max    =', numtrimax_max
              stop
           endif

 100    continue

            return
            end subroutine delaun
        end module m_delaun

