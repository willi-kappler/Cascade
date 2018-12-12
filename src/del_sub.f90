    module m_del_sub
        contains

    !
    !------------------------------------------------------------------------
    !
    !   Function edg - finds edge in triangle l which is adjacent 
    !              to triangle k.
    !
    !              (From Sloan 1987)
    !
    !------------------------------------------------------------------------
    !
        integer Function edg(l,k,e)
    !
        integer     l,k,i,e(3,*)
    !
        do 10 i=1,3
           if(e(i,l).eq.k)then
                  edg = i
                  return
               end if
     10     continue

        write(*,*)' ***Error in function edg***'
        write(*,*)' ***Triangles not adjacent***'
        write(*,*)' triangle = ',l,' looking for triangle',k

        stop
        end function edg
    !
    !------------------------------------------------------------------------
    !
    !   Function visible - determines whether the triangle t is visible
    !                  from the point p on edge tpos.
    !   Input:
    !       points(2,*)     array of node co-ordinates  
    !           vertices(3,*)       array of triangle vertices  
    !           t           Triangle to be tested
    !           tpos            Edge to be tested in triangle t 
    !       eps         distance from an interface for a
    !                   a point to be considered on an 
    !                   interface (real*8). Prevents zero
    !                   area triangles resulting from rounding
    !                   error when nodes are co-linear.
    !
    !   Output:
    !           visible         Logical: = true if edge is visible  
    !                                            = false if edge is not visible
    !
    !   Comments:
    !        Assumes point p is outside of the convex hull and vertices
    !        are in ccw order. Uses Sloan's definition of adjacency matrix.
    !       
    !   Calls no other routines.
    !
    !                   M. Sambridge, RSES, Nov 1994.
    !                   (Last updated 30/1/96)
    !
    !------------------------------------------------------------------------
    !
        logical Function visible(x,y,points,vertices,t,tpos,eps)

        real*8 ::     points(2,*)
        real*8 ::     del1,del2
        real*8 ::     x,y
        integer(4) ::    vertices(3,*)
        integer(4) ::    t,tpos
        real*8 ::     eps,del
        integer(4) ::     c1(3),j,i1,i2
        save            c1
        data        c1/2,3,1/

            j = c1(tpos)
    !                       test edge tpos in triangle t
            i1 = vertices(tpos,t)
            i2 = vertices(j,t)
            del1 = (points(2,i1)-y)*(points(1,i2)-x)
            del2 = (points(1,i1)-x)*(points(2,i2)-y)
        del = del1-del2
            if(del.gt.eps)then
               visible = .true.
        else
               visible = .false.
        end if

        return
        end function visible
    !
    !------------------------------------------------------------------------
    !
    !   Function ccw - used to test the orientation of three points
    !
    !       Input :     points p1,p2 and p3 (vectors 2x1)
    !               (e.g. p(1,2) = x co-ordinate of p2)
    !
    !       Output:     ccw,I
    !
    !       ccw    k
    !        1     0    :The direction p1,p2,p3 is ccw (+ve)   
    !       -1     0    :The direction p1,p2,p3 is  cw (-ve)   
    !        1     1    :p1,p2,p3 are colinear & p2 in middle  
    !       -1     1    :p1,p2,p3 are colinear & p1 in middle
    !        0     1    :p1,p2,p3 are colinear & p3 in middle 
    !
    !
    !               Calls no other routines.
    !
    !                   M. Sambridge, RSES, April 1994.
    !
    !------------------------------------------------------------------------
    !
          Integer Function ccw(p1,p2,p3,k)
    !     
          real*8        p1(2),p2(2),p3(2)
          real*8        dx1,dx2,dy1,dy2,a,b
          integer(4) :: k
    !      integer      ccw
    !
          dx1 = p2(1) - p1(1)
          dx2 = p3(1) - p1(1)
          dy1 = p2(2) - p1(2)
          dy2 = p3(2) - p1(2)
          a = dx1*dy2
          b = dy1*dx2
          if (a.gt.b)then
             k = 0
             ccw = 1
          else if(a.lt.b)then
             k = 0
             ccw = -1
          else if(dx1*dx2.lt.0.0_8.or.dy1*dy2.lt.0.0_8)then
             k = 1
             ccw = -1 
          else if((dx1*dx1+dy1*dy1).lt.(dx2*dx2+dy2*dy2))then
             k = 1
             ccw = 1 
          else
             k = 1
             ccw = 0 
          end if
          return
          end function ccw

!------------------------------------------------------------------------
!
!   Subroutine visiblelist - calculates all sides of triangles 
!                        visible from the point (x,y), which is
!                    outside of the convex hull.
!           
!   Input:
!       points(2,*)     array of node co-ordinates  
!           vertices(3,*)       array of triangle vertices  
!           neighbour(3,*)      array of neighbouring triangles.    
!                                       neighbour(i,j) is the triangle
!                                       which shares the face containing
!                                       node (mod(i,3)+1) and (mod(i,3)+2)
!                                       in triangle j, stored counterclockwise
!                                       about j.
!                                       (This is the `opposite' definition)
!       x,y         Co-ordinates of test point p
!       t           Index of any triangle on hull 
!                   that is visible from point p.
!                   (Usually given by routine Triloc_del.)
!       tpos            Position of edge in triangle t
!                   (using Sloan's adjacency convention)
!       eps         distance from an interface for a
!                   a point to be considered on an 
!                   interface (real*8). Prevents zero
!                   area triangles resulting from rounding
!                   error when nodes are co-linear.
!
!   Output:
!       nvis            Number of triangles visible from point p
!       vis_tlist       List of triangles visible from p
!       vis_elist       List of edges visible from p
!
!   Comments:
!        Assumes point p is outside of the convex hull and vertices
!        are in ccw order. Uses Sloan's definition of adjacency matrix.
!       
!        This routine was converted from using Sloan's definition of
!        the adjacency matrix to the `opposite' definition on 30/1/96.
!
!   Calls routine visible.
!
!                   M. Sambridge, RSES, Nov 1994.
!                   (Last updated 30/1/96)
!
!------------------------------------------------------------------------
!       

        Subroutine visiblelist (points,neighbour,vertices,x,y,t,tpos,eps, vis_tlist,vis_elist,nvis)

        real*8 ::     points(2,*)
        real*8 ::     x,y
        real*8 ::     eps
        integer(4) ::    vertices(3,*)
        integer(4) ::    neighbour(3,*)
        integer(4) ::    vis_tlist(*),vis_elist(*)
        integer(4) ::    t,tpos,pos,t1,t2,tnew
        logical     special
        integer(4) ::    c1(3),c2(3),nvis,inode,jnode
        save        c1,c2
        data        c1/2,3,1/
        data        c2/3,1,2/

        nvis = 1
        vis_tlist(1) = t
        vis_elist(1) = tpos
            inode = vertices(tpos,t)
    !d      write(6,100)t,inode,vertices(mod(tpos,3)+1,t)
            pos = c1(tpos)
            jnode = vertices(pos,t)
            t1 = neighbour(c2(pos),t)
            special = .false.
        if(t1.eq.0)then
          t1 = t
              tnew = 0
              special = .true.
        end if

      5     continue
            if(.not.special)then
               pos = edg(t1,jnode,vertices)
               tnew = neighbour(c2(pos),t1)
    !d         write(6,*)' tnew =',tnew,' t1',t1,' jnode',jnode
            end if
            special = .false.
            if(tnew.eq.0)then
      6        continue
           if(visible(x,y,points,vertices,t1,pos,eps))then
              nvis = nvis + 1
              vis_tlist(nvis) = t1
              vis_elist(nvis) = pos
    !d            write(6,100)t1,jnode,vertices(mod(pos,3)+1,t1)
           else
    !d        write(6,200)t1,jnode,vertices(mod(pos,3)+1,t1)
                  go to 10
           end if
               pos = c1(pos)
           jnode = vertices(pos,t1)
               tnew = neighbour(c2(pos),t1)
           if(tnew.eq.0) go to 6
               t1 = tnew
           go to 5 
            else
    !d     write(6,300)t1,jnode,vertices(mod(pos,3)+1,t1)
           t1 = tnew
           go to 5 
        end if

      10    jnode = inode
            pos = c2(tpos)
            t2 = neighbour(c2(pos),t)
            special = .false.
        if(t2.eq.0)then
          t2 = t
              tnew = 0
              special = .true.
        end if

      15    continue
            if(.not.special)then
               pos = c2(edg(t2,jnode,vertices))
               tnew = neighbour(c2(pos),t2)
            end if
            special = .false.
            if(tnew.eq.0)then
      16       continue
           if(visible(x,y,points,vertices,t2,pos,eps))then
              nvis = nvis + 1
              vis_tlist(nvis) = t2
              vis_elist(nvis) = pos
    !d            write(6,100)t2,vertices(pos,t2),vertices(mod(pos,3)+1,t2)
           else
    !d        write(6,200)t2,vertices(pos,t2),vertices(mod(pos,3)+1,t2)
                  go to 20
           end if
           jnode = vertices(pos,t2)
               pos = c2(pos)
               tnew = neighbour(c2(pos),t2)
           if(tnew.eq.0)go to 16
           t2 = tnew
           go to 15
            else
    !d     write(6,300)t2,jnode,vertices(mod(pos,3)+1,t2)
           t2 = tnew
           go to 15
        end if

     20 continue

        return
        end subroutine visiblelist
    !
    !------------------------------------------------------------------------
    !
    !   addpoint - inserts a point into an existing delaunay triangulation 
    !          when point is outside of triangulation (but attached to
    !          triangle t) using the stacking procedure of Sloan.
    !
    !   Input:
    !       points(2,np)        array of node co-ordinates  
    !           v(3,*)          array of triangle vertices  
    !           e(3,*)              array of neighbouring triangles.    
    !                                       e(i,j) is the triangle
    !                                       which shares the face containing
    !                                       node (mod(i,3)+1) and (mod(i,3)+2)
    !                                       in triangle j, stored counterclockwise
    !                                       about j.
    !                                       (This is the `opposite' definition)
    !       p           index of input point
    !       t           triangle on convex hull visible 
    !                   from input point 
    !       numtri          number of triangles in current
    !                   triangulation.
    !       tpos            position of start node in triangle t
    !       tri         list of triangles visible from point p
    !       newpoint        logical = true if t is the first
    !                   triangle on the hull visible from p
    !
    !   Output:
    !               v           updated
    !               e           updated
    !       numtri          updated
    !
    !   Comments:
    !
    !   The input nodes are in the form of a subset of an existing set
    !   of points. This is so that extra arrays do not need to be used.
    !   On input the vertices are assumed to be in ccw order.
    !
    !   When newpoint = false then there are multiple triangles
    !   from the new point to the convex hull, and addpoint must
    !   be called once for each attached triangle. In this case
    !   the initialization of the adjacency list includes the
    !   neighouring triangles already processed by addpoint, i.e.
    !   those from point p to the hull.
    !
    !   This routine was converted from using Sloan's definition of
    !   the adjacency matrix to the `opposite' definition on 30/1/96.
    !
    !                   M. Sambridge, RSES, Nov. 1994.
    !                   (Last updated 30/1/96)
    !
    !------------------------------------------------------------------------
    !
        Subroutine addpoint (points,e,v,p,t,tpos,numtri,newpoint,tri) 

        real*8 ::     points(2,*)
        integer(4) ::    v(3,*)
        integer(4) ::    e(3,*)
        integer(4) ::    erl,era,erb,v1,v2,v3,a,b,c,l,r
        integer(4) ::    p,t,tpos
        integer(4) ::    tri(*)
        logical     newpoint
        save        ip,lp
        integer(4) ::    c1(3)
        integer(4) ::    c2(3),lp,numtri,k,itemp,j,ip
        save            c1,c2
        data        c1/2,3,1/
        data        c2/3,1,2/

        if(newpoint)then
           ip = 0
           lp = 0
            end if

    !           Add new node to existing triangulation

    !                   create new triangle
            numtri = numtri + 1
            v1 = v(tpos,t)
            v2 = v(c1(tpos),t)
            if(ccw(points(1,v1), points(1,v2), points(1,p),k).eq.-1)then
                   itemp = v1
                   v1 = v2
                   v2 = itemp
            end if
            v(1,numtri) = p
            v(2,numtri) = v1
            v(3,numtri) = v2

    !                   initialize adjacency list including
    !                   neighbouring triangles attached
    !                   from the point to the hull.

            e(c2(1),numtri) = 0
            e(c2(2),numtri) = t
            e(c2(3),numtri) = 0

    !               
            if(.not.newpoint)then
               do 10 j=1,lp
                  k = tri(j)
                  if(v(2,k).eq.v1)then
    !                write(6,*)' v1 match with node 2'
    !                write(6,*)' current triangle',numtri,' new',k
    !                write(6,*)' nodes:',v(1,k),v(2,k),v(3,k) 
    !                write(6,*)' e mat:',e(c2(1),k),e(c2(2),k),e(c2(3),k) 
                     e(c2(1),numtri) = k
                     e(c2(1),k) = numtri
                  else if(v(3,k).eq.v1)then
                     e(c2(1),numtri) = k
                     e(c2(3),k) = numtri
                  end if
                  if(v(2,k).eq.v2)then
                     e(c2(3),numtri) = k
                     e(c2(1),k) = numtri
                  else if(v(3,k).eq.v2)then
                     e(c2(3),numtri) = k
                     e(c2(3),k) = numtri
                  end if
     10        continue
            end if

    !
    !                   initialize stack

        call stackinit

    !                                       update adjacency list
    !                                       for triangle on old boundary
            e(c2(tpos),t) = numtri


    !                   add new triangle on stack

        call push(numtri)

    !                   loop while stack is not empty

     50     continue

        call pop(L)
        r = e(c2(2),l)
    !
    !                   check if new point is in circumcircle
    !
        erl=edg(r,l,e)
            erl = c1(erl) 
        era=c1(erl)
        erb=c1(era)
        v1 = v(erl,r)
        v2 = v(era,r)
        v3 = v(erb,r)
        
        if(swap(points(1,v1),points(1,v2), points(1,v3),points(1,p)))then

    !                   new point is inside circumcircle
    !                   for triangle r so swap diagonal
               a=e(c2(era),r)
               b=e(c2(erb),r)
               c=e(c2(3),l)
    !                   update adjacency list for triangle l
           v(3,l) = v3
           e(c2(2),l) = a
           e(c2(3),l) = r

    !                   update adjacency list for triangle r
           v(1,r)=p
           v(2,r)=v3
           v(3,r)=v1
           e(c2(1),r)=l
           e(c2(2),r)=b
           e(c2(3),r)=c

    !                   put edges l-a and r-b on stack
    !                   update adjacency list for 
    !                   triangles a and c
           if(a.ne.0)then
              e(edg(a,r,e),a)=l
                  call push(l)
           else
    !                   record triangles 
    !                   attached to new point
                  ip = ip + 1
                  tri(ip) = l
           end if

           if(b.ne.0)then
                  call push(r)
           else
    !                   record triangles 
    !                   attached to new point
                  ip = ip + 1
                  tri(ip) = r
               end if
           if(c.ne.0) e(edg(c,l,e),c)=r

        else

    !                   record triangle attached to p
           ip = ip + 1
               tri(ip) = l

        end if
        call stackempty(k)
        if(k.ne.1)go to 50
            call stackflush()

        lp = ip

    !       write(6,*)' Number of triangles attached to last point',ip
    !1  write(6,*)(tri(i),i=1,ip)
    !   write(6,*)' triangles attached to last point on hull'
    !       do 100 i=1,ip
    !          it=tri(i)
    !          do 101 k=1,3
    !             l=mod(k,3)+1
    !             if(e(k,it).eq.0)then
    !                write(6,*)' t',it,' edge ',v(k,it),v(l,it)
    !             end if
    ! 101      continue 
    ! 100   continue 
    !

        return
        end subroutine addpoint
    !
    !------------------------------------------------------------------------
    !
    !   insertpoint - inserts a point into an existing delaunay triangulation 
    !             (when new point is inside triangle t) using the stacking 
    !             procedure of Sloan.
    !
    !   Input:
    !       points(2,np)        array of node co-ordinates  
    !           v(3,*)          array of triangle vertices  
    !           e(3,*)              array of neighbouring triangles.    
    !                                       e(i,j) is the triangle
    !                                       which shares the face containing
    !                                       node (mod(i,3)+1) and (mod(i,3)+2)
    !                                       in triangle j, stored counterclockwise
    !                                       about j.
    !                                       (This is the `opposite' definition)
    !       p           index of input point
    !       t           triangle containing input point
    !       numtri          number of triangles in current
    !                   triangulation.
    !           iface           index of the face containing the
    !                   input point in triangle loc
    !                   (if point is on a face)
    !
    !   Output:
    !
    !               v           updated
    !               e           updated
    !       numtri          updated
    !
    !   Comments:
    !
    !   The new point is assumed to be inside the convex hull of the
    !   existing triangulation.
    !
    !   The input nodes are in the form of a subset of an existing set
    !   of points. This is so that extra arrays do not need to be used.
    !   On input the vertices are assumed to be in ccw order.
    !
    !   This routine was converted from using Sloan's definition of
    !   the adjacency matrix to the `opposite' definition on 30/1/96.
    !
    !                   M. Sambridge, RSES, Nov. 1994.
    !                   (Last updated 30/1/96)
    !
    !------------------------------------------------------------------------
    !
        Subroutine insertpoint (points,e,v,p,t,numtri,iface) 

        real*8 ::     points(2,*)
        integer(4) ::    v(3,*)
        integer(4) ::    e(3,*)
        integer(4) ::    erl,era,erb,v1,v2,v3,a,b,c,l,r
        integer(4) ::    p,t
        integer(4) ::    c1(3)
        integer(4) ::    c2(3),iface,numtri,j,k,i
        save            c1,c2
        data        c1/2,3,1/
        data        c2/3,1,2/


    !                   add new node to existing triangulation
            if(iface.eq.0)then
           a = e(c2(1),t)
           b = e(c2(2),t)
           c = e(c2(3),t)
           v1 = v(1,t)
           v2 = v(2,t)
           v3 = v(3,t)
           v(1,t) = p   
           v(2,t) = v1  
           v(3,t) = v2  
           e(c2(1),t) = numtri+2    
           e(c2(2),t) = a   
           e(c2(3),t) = numtri+1    
            

    !                   create new triangles
    !
               numtri = numtri + 1
           v(1,numtri)=p
           v(2,numtri)=v2
           v(3,numtri)=v3
           e(c2(1),numtri)=t
           e(c2(2),numtri)=b
           e(c2(3),numtri)=numtri+1
           numtri = numtri + 1
           v(1,numtri)=p
           v(2,numtri)=v3
           v(3,numtri)=v1
           e(c2(1),numtri)=numtri-1
           e(c2(2),numtri)=c
           e(c2(3),numtri)=t
            else
               j = iface
               k = c1(j)
               i = c1(k)
           a = e(c2(i),t)
           b = e(c2(j),t)
           c = e(c2(k),t)
           v1 = v(i,t)
           v2 = v(j,t)
           v3 = v(k,t)
           v(1,t) = p   
           v(2,t) = v1  
           v(3,t) = v2  
           e(c2(1),t) = numtri+1
           e(c2(2),t) = a   
           e(c2(3),t) = 0   

    !                   create new triangle
    !
               numtri = numtri + 1
           v(1,numtri)=p
           v(2,numtri)=v3
           v(3,numtri)=v1
           e(c2(1),numtri)=0
           e(c2(2),numtri)=c
           e(c2(3),numtri)=t
            end if
      
    !
    !                   initialize stack

        call stackinit

    !                   add new triangles on stack
    !                   and update adjacency list

        if(a.ne.0)call push(t)

        if(b.ne.0)then
              e(edg(b,t,e),b)=numtri-1
              call push(numtri-1)
            end if

        if(c.ne.0)then
              e(edg(c,t,e),c)=numtri
              call push(numtri)
            end if
    !                   loop while stack is not empty

            if(a.eq.0.and.b.eq.0.and.c.eq.0)go to 100

     50     continue

        call pop(L)
        r = e(c2(2),l)
    !
    !                   check if new point is in circumcircle
    !
        erl=edg(r,l,e)
            erl = c1(erl)
        era=c1(erl)
        erb=c1(era)
        v1 = v(erl,r)
        v2 = v(era,r)
        v3 = v(erb,r)
        
        if(swap(points(1,v1),points(1,v2), points(1,v3),points(1,p)))then

    !                   new point is inside circumcircle
    !                   for triangle r so swap diagonal
               a=e(c2(era),r)
               b=e(c2(erb),r)
               c=e(c2(3),l)
    !                   update adjacency list for triangle l
           v(3,l) = v3
           e(c2(2),l) = a
           e(c2(3),l) = r

    !                   update adjacency list for triangle r
           v(1,r)=p
           v(2,r)=v3
           v(3,r)=v1
           e(c2(1),r)=l
           e(c2(2),r)=b
           e(c2(3),r)=c

    !                   put edges l-a and r-b on stack
    !                   update adjacency list for 
    !                   triangles a and c
           if(a.ne.0)then
              e(edg(a,r,e),a)=l
                  call push(l)
           end if
           if(b.ne.0) call push(r)
           if(c.ne.0) e(edg(c,l,e),c)=r

        end if
        call stackempty(k)
        if(k.ne.1)go to 50
     100    continue
            call stackflush()

        return
        end subroutine insertpoint
    !
    !------------------------------------------------------------------------
    !
    !   logical function swap - checks to see if point p lies 
    !                   inside circumcircle about points p1,p2,p3
    !               using the algorithm of Cline and Renka
    !               (see Sloan 1987).
    !
    !------------------------------------------------------------------------
    !
        logical Function swap(p1,p2,p3,p)

        real*8      p(2),p1(2),p2(2),p3(2)
        real*8      x13,y13,x23,y23,x1p,y1p,x2p,y2p
        real*8      cosa,cosb,sina,sinb

        x13=p1(1)-p3(1)
        y13=p1(2)-p3(2)
        x23=p2(1)-p3(1)
        y23=p2(2)-p3(2)
        x1p=p1(1)-p(1)
        y1p=p1(2)-p(2)
        x2p=p2(1)-p(1)
        y2p=p2(2)-p(2)

        cosa = x13*x23 + y13*y23
        cosb = x2p*x1p + y1p*y2p

        if((cosa.ge.0.d0).and.(cosb.ge.0.d0))then
                swap = .false.
            else if((cosa.lt.0.d0).and.(cosb.lt.0.d0))then
                swap = .true.
            else
                sina=x13*y23-x23*y13
                sinb=x2p*y1p-x1p*y2p
            if((sina*cosb+sinb*cosa).lt.0.d0)then
                    swap = .true.
                else
                    swap = .false.
                end if
            end if

        return
        end function swap
    !
    !------------------------------------------------------------------------
    !
    !   Triloc_del - locates the triangle containing point x,y
    !
    !   Input:
    !       x,y         co-ordinates of input points    
    !       points(2,np)        array of node co-ordinates  
    !           vertices(3,nt)      array of triangle vertices  
    !           neighbour(3,*)      array of neighbouring triangles.    
    !                                       neighbour(i,j) is the triangle
    !                                       which shares the face containing
    !                                       node (mod(i,3)+1) and (mod(i,3)+2)
    !                                       in triangle j, stored counterclockwise
    !                                       about j.
    !                                       (This is the `opposite' definition)
    !           loc         first guess of triangle containing
    !                   (x, y).
    !       eps         distance from an interface for a
    !                   a point to be considered on an 
    !                   interface (real*8). Prevents zero
    !                   area triangles resulting from rounding
    !                   error when nodes are co-linear.
    !
    !   Output:
    !           loc         index of triangle containing 
    !                   input point.
    !           outside         =true if (x,y) is outside of
    !                   the convex hull, otherwise = false. 
    !           k           index of face through which the
    !                   algorithm last passed (used by
    !                   routine visbilelist if outside = .true.)
    !           iface           index of the face containing the
    !                   input point in triangle loc
    !                   (if point is on a face)
    !
    !   Comments:
    !        If (x,y) is outside convex hull loc is a visible triangle
    !        on the hull, outside is set to .true., and k is set to the
    !        index of the face of triangle loc visible from the input point
    !        (used as a starting point by the routine visiblelist)
    !
    !        This version also returns the parameter iface. 
    !        If iface .ne. 0 then the input point is on the face of 
    !        triangle t between nodes iface and mod(iface,3)+1 and 
    !        it is also on the convex hull.
    !
    !        A point is assumed to be on the edge (or its extension)
    !        between two nodes if it is inside the triangle at a 
    !        distance >= eps.
    !
    !        Can be extended to higher dimensions using a similar
    !        stepping mechanism but without angular test.
    !
    !            This routine was converted from using Sloan's definition of
    !            the adjacency matrix to the `opposite' definition on 30/1/96.
    !
    !        No calls to other routines.
    !
    !                   M. Sambridge, RSES, Nov. 1994.
    !                   (Last updated 30/1/96)
    !
    !------------------------------------------------------------------------
    !
        Subroutine Triloc_del (x,y,points,vertices,neighbour,loc,eps,outside,k,iface)
    !
        real*8 ::     points(2,*)
        integer(4) ::    vertices(3,*)
        integer(4) ::    neighbour(3,*)
        integer(4) ::    p1,p2
        logical     outside
        real*8 ::     x,y,del1,del2,del
        real*8 ::     eps
        integer(4) ::    c1(3),c2(3),ic,iface,i,j,k,loc
        integer(4) :: loc1,loc2,loc3
        save            c1,c2
        data        c1/2,3,1/
        data        c2/3,1,2/
        logical         new

        outside = .false.
            new = .true.
            ic = 0
            loc1 = 1
            loc2 = 1
            loc3 = 1

     10     continue
    !                   point is outside convex hull
            if(outside)return
            iface = 0

            do 20 i=1,3
           j = c1(i)
    !      k = c1(j)
    !                   definition of adjacency matrix

    !                   use Sloan's 
    !                   definition of adjacency matrix
           k = i

               p1 = vertices(i,loc)
               p2 = vertices(j,loc)
           del1 = (points(2,p1)-y)*(points(1,p2)-x)
           del2 = (points(1,p1)-x)*(points(2,p2)-y)
               del = del1-del2
           if(dabs(del).le.eps)then
                  iface = i
           else if(del.gt.0.d0)then
              if(neighbour(c2(k),loc).eq.0)then
                     outside = .true.
              else
                 loc = neighbour(c2(k),loc)
              end if
                  if(.not.new.and.loc.eq.loc1)then
                     write(*,100) 
                     write(*,*)' Current triangle:',loc,' last three:',loc1,loc2,loc3
                     write(*,*)' New point      x:',x,' y:',y
                     write(*,*)' Triangle ',loc,' v:',(vertices(j,loc),j=1,3),' n:',(neighbour(c2(j),loc),j=1,3)
    !                write(*,*)' del',del,' del1',del1,' del2',del2
                     write(*,101) 
                     stop
                  end if
                  if(new)then
                    ic = ic + 1
                    if(ic.eq.3)new = .false.
                  end if
                  loc1 = loc2
                  loc2 = loc3
                  loc3 = loc
              go to 10
           end if
     20     continue
        
    !                       check if input point is
    !                       on the convex hull
    !
    !        print *, "loc: ", loc
    !        print *, "iface: ", iface
    !        print *, "c2(iface < 3): ", c2(iface)
    !        print *, "neighbour(c2(iface),loc)  (3, 452403)"

            if (loc > 452403) then
                print *, "loc: ", loc
            end if

            if(neighbour(c2(iface),loc).ne.0) then
    !            print *,'iface:',iface
                iface = 0
            end if

    !       if(iface.ne.0)then
    !          j = mod(iface,3)+1
    !          jj = vertices(iface,loc)
    !          kk = vertices(j,loc)
    !          write(*,*)' point on triangle between nodes ',
    !    &               jj,' and',kk
    !          write(*,*)' point is on the convex hull'
    !       end if

     100    format(/'Error in subroutine Triloc_del:',// &
            ' Infinite loop detected in walking triangle algorithm',/, &
            ' Probably due to rounding error creating a flat triangle'/)

     101    format(/1x,'Remedy: '/ &
            ' Either increase size of parameter eps in calling routine '/ &
            ' or re-order input points by running program nn_hull '/)

        return
        end subroutine Triloc_del
    !
    !------------------------------------------------------------------------
    !
    !   function theta - returns a real number between 0 and 360
    !                which has the same ordering as the angle
    !            between the line (a,b) and the horizontal.
    !
    !------------------------------------------------------------------------
    !
        real(8) function theta(a,b)

        real*8      a(2),b(2)
        real(8) :: dx,dy,ax,ay,d

        dx = b(1) - a(1)  
        ax = abs(dx)
        dy = b(2) - a(2)  
        ay = abs(dy)

        theta = 0.0_8
        d = ax+ay
        if(d.ne.0.0_8)theta=dy/d

        if(dx.lt.0.0_8)then
               theta = 2.0_8-theta
        else if(dy.lt.0.0_8)then
               theta = 4.0_8+theta
        end if
        theta = theta*90.0_8

        return
        end function theta
    end module m_del_sub

