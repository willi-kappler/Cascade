!
!------------------------------------------------------------------------
!
!   nn2Dfd - calculates natural neighbour interpolation and 
!        1st derivatives at point x when point is inside convex hull 
!        (using method based on a closed 2-D formula)
!
!   Input:
!       x(2)            co-ordinates of input point
!       points(2,np)        array of node points    
!           vertices(3,nt)      array of triangle vertices  
!           neighbour(3,nt)     array of neighbouring triangles.    
!                   Neighbour(i,j) is the triangle
!                   opposite node i in triangle j,
!                   stored counterclockwise about j.
!               centres(3,nt)           centres(j,i) (j=1,2) contains the
!                                       co-ordinates of the centre of 
!                                       circumcircle about Delaunay 
!                                       triangle i, (i=1,...,nt),
!                                       j=3 contains squared radius of circle.
!           loc         index of triangle containing 
!                   input point.
!       data            data values at each node
!       nnpn_max        maximum number of neighbours per node
!                   (depends on the point distribution,
!                                        set to ~50 in calling program)
!       tstore          integer work array of size nnpn_max
!       index           integer work array of size nnpn_max
!       vpairs          integer work array of size 2*nnpn_max
!       angles          real work array of size nnpn_max
!       pverts          real(8) work array of size 2*nnpn_max
!       vverts          real(8) work array of size 2*nnpn_max
!       dx_verts        real(8) work array of size 2*nnpn_max
!       dy_verts        real(8) work array of size 2*nnpn_max
!       ltri            logical work array of size nt_max
!       lnode           logical work array of size nt_max
!
!   Output:
!       f           interpolated data value
!       df(1)           interpolated derivative df/dx
!       df(2)           interpolated derivative df/dy
!       v           area of voronoi cell about x
!
!   Comments:
!        On input point x must be in triangle loc. 
!        Note: The input triangle contains x and so it's 
!        circumcircle must also contain x.
!
!        In order to use the direct formula to
!        calculate the nn co-ordinate of the input point with
!        respect to its ith neighbour we must find the set of
!        nodes that are both neighbours of i and x, where 
!        the neighbours of i are determined before addition of x.
!        This is done using a LIFO stack to avoid an 
!        expensive global search over all triangles.
!
!        It is assumed that the logical arrays ltri(nt_max) 
!        and lnode are initialized to false on input.
!
!        This version does not calculate derivatives.
!
!        Calls are made to: plot_tc, stackpairinit,
!                   poppair,pushpair & stackpairempty 
!                   indexx & second_v_area_d.
!
!                   M. Sambridge, RSES, May 1996.
!
!------------------------------------------------------------------------
!

    module m_nn2
        contains

        Subroutine nn2Dfd &
           (x,points,vertices,neighbour,centres,loc,data,nnpn_max, &
            pverts,vverts,tstore,vpairs,angles,index,dx_verts,dy_verts, &
            ltri,lnode,f,df,v)

        integer(4) :: nnpn_max,lud,loc,i,j,k,new1,new2,nv,node,it,j1,j2
        integer(4) :: na,nb,iopp,ncount,in,ip,im,km,nverts,jt,jtri,jn
        integer(4) :: jnode,kp
        real(8) :: area

        real(8)      points(2,*)
        real(8)      centres(3,*)
        real(8)      data(*)
        real(8)      x(2),dist,dx,dy
        real(8)      f,v,df(2),deriv(2),dvx,dvy
        real(8)          pverts(2,nnpn_max)
        real(8)          vverts(2,nnpn_max)
        real(8)          dx_verts(2,nnpn_max)
        real(8)          dy_verts(2,nnpn_max)
        real(8)          dpverts(4,2)
        real(8)            angles(nnpn_max)
        integer     vertices(3,*)
        integer     neighbour(3,*)
        integer     tri,pos,tcount
        integer     tstore(nnpn_max)
        integer     index(nnpn_max)
        integer     vpairs(2,nnpn_max)
        logical     nnwrite
        logical*1   ltri(*)
        logical*1   lnode(*)
    !
        integer     cyc(3)
            data        cyc/2,3,1/

            

        common/nnswitches/nnwrite,lud

    !                   Initialize variables
        f = 0.d0
        df(1) = 0.d0
        df(2) = 0.d0
        v = 0.d0
        dvx = 0.d0
        dvy = 0.d0
            tcount = 1

        if(nnwrite)write(lud,*)loc
    !                   plot input circum-triangle 
    !                   and circum-circle

        call plot_tc()

    !                   record circum-triangle 
            tstore(tcount) = loc
            ltri(loc) = logical(.true., 1)
    !
    !                   Find all other circumcircles 
    !                   containing input point using a 
    !                   directed walk
    !
    !                   initialize stack
        call stackpairinit


    !                   put input triangle's neighbouring 
    !                   triangles on LIFO stack
    !                   together with position of input
    !                   triangle in their neighbour list

        i = neighbour(1,loc)
        j = neighbour(2,loc)
        k = neighbour(3,loc)
        if(i.ne.0)then
           if(neighbour(1,i).eq.loc)then
                  call pushpair(i,1)
           else if(neighbour(2,i).eq.loc)then
                  call pushpair(i,2)
           else if(neighbour(3,i).eq.loc)then
                  call pushpair(i,3)
               end if
        end if
        if(j.ne.0)then
           if(neighbour(1,j).eq.loc)then
                  call pushpair(j,1)
           else if(neighbour(2,j).eq.loc)then
                  call pushpair(j,2)
           else if(neighbour(3,j).eq.loc)then
                  call pushpair(j,3)
               end if
        end if
        if(k.ne.0)then
           if(neighbour(1,k).eq.loc)then
                  call pushpair(k,1)
           else if(neighbour(2,k).eq.loc)then
                  call pushpair(k,2)
           else if(neighbour(3,k).eq.loc)then
                  call pushpair(k,3)
               end if
        end if

     10 call stackpairempty(k)
    !                   if stack empty then finish
        if(k.eq.1)go to 100
    !                   take triangle from stack
        call poppair(tri,pos)

    !                   test if x is in circumcircle
            dx = x(1)-centres(1,tri)
            dy = x(2)-centres(2,tri)
        dist = dx*dx + dy*dy 

        if(dist.lt.centres(3,tri))then
               if(nnwrite)write(lud,*)tri

    !                   plot circum-triangle and circum-circle

           call plot_tc()

           if(pos.eq.1)then
              new1 = neighbour(2,tri)
              new2 = neighbour(3,tri)
           else if(pos.eq.2)then
              new1 = neighbour(1,tri)
              new2 = neighbour(3,tri)
           else 
              new1 = neighbour(1,tri)
              new2 = neighbour(2,tri)
           end if
    !                   record current circum-triangle
    !                   and pseudo angle to x
               tcount = tcount + 1
               tstore(tcount) = tri
               ltri(tri) = logical(.true., 1)

           if(new1.ne.0)then
              if(neighbour(1,new1).eq.tri)then
                     call pushpair(new1,1)
              else if(neighbour(2,new1).eq.tri)then
                     call pushpair(new1,2)
              else if(neighbour(3,new1).eq.tri)then
                     call pushpair(new1,3)
                  end if
           end if
           if(new2.ne.0)then
              if(neighbour(1,new2).eq.tri)then
                     call pushpair(new2,1)
              else if(neighbour(2,new2).eq.tri)then
                     call pushpair(new2,2)
              else if(neighbour(3,new2).eq.tri)then
                     call pushpair(new2,3)
                  end if
           end if
        end if
        go to 10
     
     100    continue
            call stackpairflush()

    !                       check size of tcount
            if(tcount+2.gt.nnpn_max)then
               write(*,*)' '
               write(*,*)' Error: work arrays in subroutine nn2Dfd', &
                        ' not big enough.'
               write(*,*)' Too many circum-triangles for this node'
               write(*,*)' Remedy: Increase size of parameter'
               write(*,*)'         nnpn_max in calling program'
               write(*,*)'         current value = ',nnpn_max
               write(*,*)'         required value >= ',tcount+2
           stop
            end if
    !                       find list of vertices
    !                       of new voronoi cell about
    !                       x and store the pair of
    !                       nodes associated with each
    !                       vertex
    !       write(*,*)' Circum-triangles of x:'
            nv = 0
            do 110 i=1,tcount
               it = tstore(i)
    !          write(*,*)it
               do j=1,3
                  node = vertices(j,it)
                  j1 = cyc(j)
                  j2 = cyc(j1)
                  na = vertices(j1,it)
                  nb = vertices(j2,it)
                  iopp = neighbour(j,it)
                  if(iopp.eq.0.or..not.logical(ltri(iopp),4))then
                     nv = nv + 1
    !
    !                       find circum-centre of
    !                       x, node, and nodeb 
    !
                     call circum_d (x,points(1,na),points(1,nb), &
                                   vverts(1,nv), &
                                   dx_verts(1,nv),dy_verts(1,nv))

                     vpairs(1,nv) = na
                     vpairs(2,nv) = nb
                     angles(nv) = pangle(x,vverts(1,nv))
    !
    !                write(*,*)' na =',na,' nb=',nb
    !                write(*,*)' dx_verts =',dx_verts(1,nv)
    !                write(*,*)' dx_verts =',dx_verts(2,nv)
    !                write(*,*)' dy_verts =',dy_verts(1,nv)
    !                write(*,*)' dy_verts =',dy_verts(2,nv)
    !
                  end if
               end do
               
     110    continue
            
    !                       sort vertices 
    !
            call indexx(nv,angles,index)
    !                       debug I/O
    !       write(*,*)' Number of vertices = ',nv
    !       write(*,*)' index '
    !   do i=1,nv
    !          write(*,*)' i',i,' index',index(i),' a',angles(i)
    !       end do
    !
    !                       find vertices of each 
    !                       second-order Voronoi cell
    !
        ncount = 0
    !                       loop over neighbours of x
            do 120 it=1,tcount
           tri = tstore(it)
               do 130 in = 1,3
                  node = vertices(in,tri)
                  if(lnode(node))go to 130
                  lnode(node) = logical(.true., 1)
    !             write(*,*)' Neighbour node =',node
    !
    !                       find pair of `external' vertices
    !                       of second-order voronoi cell  
    !
                  do i=1,nv
                     k = index(i)
                     if(vpairs(1,k).eq.node.or. &
                       vpairs(2,k).eq.node)then
                        pverts(1,1) = vverts(1,k) - x(1)
                        pverts(2,1) = vverts(2,k) - x(2)
                        dpverts(1,1) = dx_verts(1,k)
                        dpverts(2,1) = dx_verts(2,k)
                        dpverts(3,1) = dy_verts(1,k)
                        dpverts(4,1) = dy_verts(2,k)
                        ip = i+1
                        im = i-1
                        if(i.eq.nv)ip=1
                        if(i.eq.1)im=nv
                        kp = index(ip)
                        km = index(im)
                        if(vpairs(1,kp).eq.node.or. &
                          vpairs(2,kp).eq.node)then
                           pverts(1,2) = vverts(1,kp) - x(1)
                           pverts(2,2) = vverts(2,kp) - x(2)
                           dpverts(1,2) = dx_verts(1,kp)
                           dpverts(2,2) = dx_verts(2,kp)
                           dpverts(3,2) = dy_verts(1,kp)
                           dpverts(4,2) = dy_verts(2,kp)
                        else if(vpairs(1,km).eq.node.or. &
                               vpairs(2,km).eq.node)then
                           pverts(1,1) = vverts(1,km) - x(1)
                           pverts(2,1) = vverts(2,km) - x(2)
                           pverts(1,2) = vverts(1,k) - x(1)
                           pverts(2,2) = vverts(2,k) - x(2)
                           dpverts(1,1) = dx_verts(1,km)
                           dpverts(2,1) = dx_verts(2,km)
                           dpverts(3,1) = dy_verts(1,km)
                           dpverts(4,1) = dy_verts(2,km)
                           dpverts(1,2) = dx_verts(1,k)
                           dpverts(2,2) = dx_verts(2,k)
                           dpverts(3,2) = dy_verts(1,k)
                           dpverts(4,2) = dy_verts(2,k)
                        else
                           write(*,*)' Error in nn2Dfd'
                           write(*,*) &
                          ' Can not find neighbouring vertices' 
                           write(*,*)' node=',node,' i =',i
                        end if
    !
                        go to 150
                     end if
                  end do
     150          continue
                  nverts = 2
    !
    !                       find `internal' vertices of 
    !                       second-order voronoi cell  
              do 140 jt = 1,tcount
                     jtri = tstore(jt)
                     do jn = 1,3
                        jnode = vertices(jn,jtri)
                        if(jnode.eq.node)then
                           nverts = nverts + 1
    !
    !                       record circum-centre
    !
                           pverts(1,nverts) = centres(1,jtri) - x(1)
                           pverts(2,nverts) = centres(2,jtri) - x(2)

    !                      write(*,*)' found common triangle',jtri

                           go to 140
                        end if
                     end do
     140          continue
    !                       Calculate volume of 
    !                       second-order voronoi cell 
    !                       for current node using
    !                       direct 2-D formula 
    !
                  call second_v_area_d &
                 (nverts,pverts,dpverts,angles,area,deriv)
    !
                  v = v + area
                  f = f + area*data(node)
                  df(1) = df(1) + data(node)*deriv(1)
                  df(2) = df(2) + data(node)*deriv(2)
                  dvx = dvx + deriv(1)
                  dvy = dvy + deriv(2)

     130       continue
     120    continue
        if(v.ne.0.0_8)then
               f = f/v
               df(1) = (df(1) - f*dvx)/v
               df(2) = (df(2) - f*dvy)/v
            end if
            v = v*0.5_8
    !
    !                       reset logical arrays
            do i=1,tcount
               tri = tstore(i)
               ltri(tri) = logical(.false., 1)
               do j = 1,3
                  lnode(vertices(j,tri)) = logical(.false., 1)
               end do
            end do 

        return
        end subroutine
    !
    !------------------------------------------------------------------------
    !
    !   nn2Dfdd - calculates natural neighbour interpolation 
    !         and 1st & 2nd  derivatives at point x when 
    !         point is inside convex hull 
    !         (using method based on a closed 2-D formula)
    !
    !   Input:
    !       x(2)            co-ordinates of input point
    !       points(2,np)        array of node points    
    !           vertices(3,nt)      array of triangle vertices  
    !           neighbour(3,nt)     array of neighbouring triangles.    
    !                   Neighbour(i,j) is the triangle
    !                   opposite node i in triangle j,
    !                   stored counterclockwise about j.
    !               centres(3,nt)           centres(j,i) (j=1,2) contains the
    !                                       co-ordinates of the centre of 
    !                                       circumcircle about Delaunay 
    !                                       triangle i, (i=1,...,nt),
    !                                       j=3 contains squared radius of circle.
    !           loc         index of triangle containing 
    !                   input point.
    !       data            data values at each node
    !       nnpn_max        maximum number of neighbours per node
    !                   (depends on the point distribution,
    !                                        set to ~50 in calling program)
    !       tstore          integer work array of size nnpn_max
    !       index           integer work array of size nnpn_max
    !       vpairs          integer work array of size 2*nnpn_max
    !       angles          real work array of size nnpn_max
    !       pverts          real(8) work array of size 2*nnpn_max
    !       vverts          real(8) work array of size 2*nnpn_max
    !       dx_verts        real(8) work array of size 2*nnpn_max
    !       dy_verts        real(8) work array of size 2*nnpn_max
    !       dxx_verts       real(8) work array of size 2*nnpn_max
    !       dyy_verts       real(8) work array of size 2*nnpn_max
    !       dxy_verts       real(8) work array of size 2*nnpn_max
    !       ltri            logical work array of size nt_max
    !       lnode           logical work array of size nt_max
    !
    !   Output:
    !       f           interpolated data value
    !       df(1)           interpolated derivative df/dx
    !       df(2)           interpolated derivative df/dy
    !       ddf(1)          interpolated derivative d2f/dxx
    !       ddf(2)          interpolated derivative d2f/dyy
    !       ddf(3)          interpolated derivative d2f/dxy
    !       v           area of voronoi cell about x
    !
    !   Comments:
    !        On input point x must be in triangle loc. 
    !        Note: The input triangle contains x and so it's 
    !        circumcircle must also contain x.
    !
    !        In order to use the direct formula to
    !        calculate the nn co-ordinate of the input point with
    !        respect to its ith neighbour we must find the set of
    !        nodes that are both neighbours of i and x, where 
    !        the neighbours of i are determined before addition of x.
    !        This is done using a LIFO stack to avoid an 
    !        expensive global search over all triangles.
    !
    !        It is assumed that the logical arrays ltri(nt_max) 
    !        and lnode are initialized to false on input.
    !
    !        This version does not calculate derivatives.
    !
    !        Calls are made to: plot_tc, stackpairinit,
    !                   poppair,pushpair & stackpairempty 
    !                   indexx & second_v_area_dd.
    !
    !                   M. Sambridge, RSES, May 1996.
    !
    !------------------------------------------------------------------------
    !
        Subroutine nn2Dfdd &
           (x,points,vertices,neighbour,centres,loc,data,nnpn_max, &
            pverts,vverts,tstore,vpairs,angles,index,dx_verts,dy_verts, &
            dxx_verts,dyy_verts,dxy_verts,ltri,lnode,f,df,ddf,v)

        integer(4) :: nnpn_max,lud,loc,i,j,k,new1,new2,nv,it,node,j1,j2
        integer(4) :: na,nb,iopp,ncount,in,ip,im,kp,km,nverts,jt
        integer(4) :: jtri,jn,jnode
        real(8) :: area

        real(8)      points(2,*)
        real(8)      centres(3,*)
        real(8)      data(*)
        real(8)      x(2),dist,dx,dy
        real(8)      f,v,df(2),ddf(3)
        real(8)      deriv(2),dderiv(3),dvx,dvy,dvxx,dvyy,dvxy
        real(8)          pverts(2,nnpn_max)
        real(8)          dpverts(4,2)
        real(8)          ddpverts(6,2)
        real(8)          vverts(2,nnpn_max)
        real(8)          dx_verts(2,nnpn_max)
        real(8)          dy_verts(2,nnpn_max)
        real(8)          dxx_verts(2,nnpn_max)
        real(8)          dyy_verts(2,nnpn_max)
        real(8)          dxy_verts(2,nnpn_max)
        integer     vertices(3,*)
        integer     neighbour(3,*)
        integer     tri,pos,tcount
        integer     tstore(nnpn_max)
        integer     index(nnpn_max)
        integer     vpairs(2,nnpn_max)
        logical     nnwrite
        logical*1   ltri(*)
        logical*1   lnode(*)
        real(8)            angles(nnpn_max)
        integer     cyc(3)
        data        cyc/2,3,1/

        common/nnswitches/nnwrite,lud
    !                   Initialize variables
        f = 0.d0
        df(1) = 0.d0
        df(2) = 0.d0
        ddf(1) = 0.d0
        ddf(2) = 0.d0
        ddf(3) = 0.d0
        v = 0.d0
        dvx = 0.d0
        dvy = 0.d0
        dvxx = 0.d0
        dvyy = 0.d0
        dvxy = 0.d0
            tcount = 1

        if(nnwrite)write(lud,*)loc
    !                   plot input circum-triangle 
    !                   and circum-circle

        call plot_tc()

    !                   record circum-triangle 
            tstore(tcount) = loc
            ltri(loc) = logical(.true., 1)
    !
    !                   Find all other circumcircles 
    !                   containing input point using a 
    !                   directed walk
    !
    !                   initialize stack
        call stackpairinit


    !                   put input triangle's neighbouring 
    !                   triangles on LIFO stack
    !                   together with position of input
    !                   triangle in their neighbour list

        i = neighbour(1,loc)
        j = neighbour(2,loc)
        k = neighbour(3,loc)
        if(i.ne.0)then
           if(neighbour(1,i).eq.loc)then
                  call pushpair(i,1)
           else if(neighbour(2,i).eq.loc)then
                  call pushpair(i,2)
           else if(neighbour(3,i).eq.loc)then
                  call pushpair(i,3)
               end if
        end if
        if(j.ne.0)then
           if(neighbour(1,j).eq.loc)then
                  call pushpair(j,1)
           else if(neighbour(2,j).eq.loc)then
                  call pushpair(j,2)
           else if(neighbour(3,j).eq.loc)then
                  call pushpair(j,3)
               end if
        end if
        if(k.ne.0)then
           if(neighbour(1,k).eq.loc)then
                  call pushpair(k,1)
           else if(neighbour(2,k).eq.loc)then
                  call pushpair(k,2)
           else if(neighbour(3,k).eq.loc)then
                  call pushpair(k,3)
               end if
        end if

     10 call stackpairempty(k)
    !                   if stack empty then finish
        if(k.eq.1)go to 100
    !                   take triangle from stack
        call poppair(tri,pos)

    !                   test if x is in circumcircle
            dx = x(1)-centres(1,tri)
            dy = x(2)-centres(2,tri)
        dist = dx*dx + dy*dy 

        if(dist.lt.centres(3,tri))then
               if(nnwrite)write(lud,*)tri

    !                   plot circum-triangle and circum-circle

           call plot_tc()

           if(pos.eq.1)then
              new1 = neighbour(2,tri)
              new2 = neighbour(3,tri)
           else if(pos.eq.2)then
              new1 = neighbour(1,tri)
              new2 = neighbour(3,tri)
           else 
              new1 = neighbour(1,tri)
              new2 = neighbour(2,tri)
           end if
    !                   record current circum-triangle
    !                   and pseudo angle to x
               tcount = tcount + 1
               tstore(tcount) = tri
               ltri(tri) = logical(.true., 1)

           if(new1.ne.0)then
              if(neighbour(1,new1).eq.tri)then
                     call pushpair(new1,1)
              else if(neighbour(2,new1).eq.tri)then
                     call pushpair(new1,2)
              else if(neighbour(3,new1).eq.tri)then
                     call pushpair(new1,3)
                  end if
           end if
           if(new2.ne.0)then
              if(neighbour(1,new2).eq.tri)then
                     call pushpair(new2,1)
              else if(neighbour(2,new2).eq.tri)then
                     call pushpair(new2,2)
              else if(neighbour(3,new2).eq.tri)then
                     call pushpair(new2,3)
                  end if
           end if
        end if
        go to 10
     
     100    continue
            call stackpairflush()

    !                       check size of tcount
            if(tcount+2.gt.nnpn_max)then
               write(*,*)' '
               write(*,*)' Error: work arrays in subroutine nn2Dfdd', &
                        ' not big enough.'
               write(*,*)' Too many circum-triangles for this node'
               write(*,*)' Remedy: Increase size of parameter'
               write(*,*)'         nnpn_max in calling program'
               write(*,*)'         current value = ',nnpn_max
               write(*,*)'         required value >= ',tcount+2
           stop
            end if
    !
    !                       find list of vertices
    !                       of new voronoi cell about
    !                       x and store the pair of
    !                       nodes associated with each
    !                       vertex
    !       write(*,*)' Circum-triangles of x:'
            nv = 0
            do 110 i=1,tcount
               it = tstore(i)
    !          write(*,*)it
               do j=1,3
                  node = vertices(j,it)
                  j1 = cyc(j)
                  j2 = cyc(j1)
                  na = vertices(j1,it)
                  nb = vertices(j2,it)
                  iopp = neighbour(j,it)
                  if(iopp.eq.0.or..not.logical(ltri(iopp), 4))then
                     nv = nv + 1
    !
    !                       find circum-centre of
    !                       x, node, and nodeb 
    !
                     call circum_dd (x,points(1,na),points(1,nb), &
                                   vverts(1,nv), &
                                   dx_verts(1,nv),dy_verts(1,nv), &
                                   dxx_verts(1,nv),dyy_verts(1,nv), &
                                   dxy_verts(1,nv))

                     vpairs(1,nv) = na
                     vpairs(2,nv) = nb
                     angles(nv) = pangle(x,vverts(1,nv))
    !                write(*,*)' na =',na,' nb=',nb
     
                  end if
               end do
               
     110    continue
            
    !                       sort vertices 
    !
            call indexx(nv,angles,index)
    !                       debug I/O
    !       write(*,*)' Number of vertices = ',nv
    !   do i=1,nv
    !          write(*,*)' i',i,' index',index(i),' a',angles(i)
    !       end do
    !
    !                       find vertices of each 
    !                       second-order Voronoi cell
    !
        ncount = 0
    !                       loop over neighbours of x
            do 120 it=1,tcount
           tri = tstore(it)
               do 130 in = 1,3
                  node = vertices(in,tri)
                  if(lnode(node))go to 130
                  lnode(node) = logical(.true., 1)
    !             write(*,*)' Neighbour node =',node
    !
    !                       find pair of `external' vertices
    !                       of second-order voronoi cell  
    !
                  do i=1,nv
                     k = index(i)
                     if(vpairs(1,k).eq.node.or. &
                       vpairs(2,k).eq.node)then
                        pverts(1,1) = vverts(1,k) - x(1)
                        pverts(2,1) = vverts(2,k) - x(2)
                        dpverts(1,1) = dx_verts(1,k)
                        dpverts(2,1) = dx_verts(2,k)
                        dpverts(3,1) = dy_verts(1,k)
                        dpverts(4,1) = dy_verts(2,k)
                        ddpverts(1,1) = dxx_verts(1,k)
                        ddpverts(2,1) = dxx_verts(2,k)
                        ddpverts(3,1) = dyy_verts(1,k)
                        ddpverts(4,1) = dyy_verts(2,k)
                        ddpverts(5,1) = dxy_verts(1,k)
                        ddpverts(6,1) = dxy_verts(2,k)
                        ip = i+1
                        im = i-1
                        if(i.eq.nv)ip=1
                        if(i.eq.1)im=nv
                        kp = index(ip)
                        km = index(im)
                        if(vpairs(1,kp).eq.node.or. &
                          vpairs(2,kp).eq.node)then
                           pverts(1,2) = vverts(1,kp) - x(1)
                           pverts(2,2) = vverts(2,kp) - x(2)
                           dpverts(1,2) = dx_verts(1,kp)
                           dpverts(2,2) = dx_verts(2,kp)
                           dpverts(3,2) = dy_verts(1,kp)
                           dpverts(4,2) = dy_verts(2,kp)
                           ddpverts(1,2) = dxx_verts(1,kp)
                           ddpverts(2,2) = dxx_verts(2,kp)
                           ddpverts(3,2) = dyy_verts(1,kp)
                           ddpverts(4,2) = dyy_verts(2,kp)
                           ddpverts(5,2) = dxy_verts(1,kp)
                           ddpverts(6,2) = dxy_verts(2,kp)
                        else if(vpairs(1,km).eq.node.or. &
                               vpairs(2,km).eq.node)then
                           pverts(1,1) = vverts(1,km) - x(1)
                           pverts(2,1) = vverts(2,km) - x(2)
                           pverts(1,2) = vverts(1,k) - x(1)
                           pverts(2,2) = vverts(2,k) - x(2)
                           dpverts(1,1) = dx_verts(1,km)
                           dpverts(2,1) = dx_verts(2,km)
                           dpverts(3,1) = dy_verts(1,km)
                           dpverts(4,1) = dy_verts(2,km)
                           dpverts(1,2) = dx_verts(1,k)
                           dpverts(2,2) = dx_verts(2,k)
                           dpverts(3,2) = dy_verts(1,k)
                           dpverts(4,2) = dy_verts(2,k)
                           ddpverts(1,1) = dxx_verts(1,km)
                           ddpverts(2,1) = dxx_verts(2,km)
                           ddpverts(3,1) = dyy_verts(1,km)
                           ddpverts(4,1) = dyy_verts(2,km)
                           ddpverts(5,1) = dxy_verts(1,km)
                           ddpverts(6,1) = dxy_verts(2,km)
                           ddpverts(1,2) = dxx_verts(1,k)
                           ddpverts(2,2) = dxx_verts(2,k)
                           ddpverts(3,2) = dyy_verts(1,k)
                           ddpverts(4,2) = dyy_verts(2,k)
                           ddpverts(5,2) = dxy_verts(1,k)
                           ddpverts(6,2) = dxy_verts(2,k)
                        else
                           write(*,*)' Error in nn2Dfdd'
                           write(*,*) &
                          ' Can not find neighbouring vertices' 
                           write(*,*)' node=',node,' i =',i
                        end if
    !
                        go to 150
                     end if
                  end do
     150          continue
                  nverts = 2
    !
    !                       find `internal' vertices of 
    !                       second-order voronoi cell  
              do 140 jt = 1,tcount
                     jtri = tstore(jt)
                     do jn = 1,3
                        jnode = vertices(jn,jtri)
                        if(jnode.eq.node)then
                           nverts = nverts + 1
    !
    !                       record circum-centre
    !
                           pverts(1,nverts) = centres(1,jtri) - x(1)
                           pverts(2,nverts) = centres(2,jtri) - x(2)

    !                      write(*,*)' found common triangle',jtri

                           go to 140
                        end if
                     end do
     140          continue
    !                       Calculate volume of 
    !                       second-order voronoi cell 
    !                       for current node using
    !                       direct 2-D formula 
    !
                  call second_v_area_dd &
                      (nverts,pverts,dpverts,ddpverts, &
                       angles,area,deriv,dderiv)
    !
                  v = v + area
                  f = f + area*data(node)
                  df(1) = df(1) + data(node)*deriv(1)
                  df(2) = df(2) + data(node)*deriv(2)
                  dvx = dvx + deriv(1)
                  dvy = dvy + deriv(2)
                  dvxx = dvxx + dderiv(1)
                  dvyy = dvyy + dderiv(2)
                  dvxy = dvxy + dderiv(3)
                  ddf(1) = ddf(1) + data(node)*dderiv(1)
                  ddf(2) = ddf(2) + data(node)*dderiv(2)
                  ddf(3) = ddf(3) + data(node)*dderiv(3)

    !             write(*,*)' Second order Voronoi-cell between x and ',node,
    !    &                  ' has ',nverts,' vertices'
     130       continue
     120    continue
        if(v.ne.0.0_8)then
               f = f/v
               df(1) = (df(1) - f*dvx)/v
               df(2) = (df(2) - f*dvy)/v
               ddf(1) = (ddf(1) - 2.0_8*dvx*df(1) - dvxx*f)/v
               ddf(2) = (ddf(2) - 2.0_8*dvy*df(2) - dvyy*f)/v
               ddf(3) = (ddf(3) - dvy*df(1) - dvx*df(2) - dvxy*f)/v
            end if
            v = v*0.5_8
    !
    !                       reset logical arrays
            do i=1,tcount
               tri = tstore(i)
               ltri(tri) = logical(.false., 1)
               do j = 1,3
                  lnode(vertices(j,tri)) = logical(.false., 1)
               end do
            end do 

        return
        end subroutine
    !
    !------------------------------------------------------------------------
    !
    !   first_voronoi - calculates the area or volume of the voronoi    
    !           polygon (polyhedron) about point x.
    !
    !   Input:
    !       x(n)            input point 
    !       p(n,m)          m neighbouring points about x
    !       n           number of dimensions
    !       m           number of neighbours
    !       nmax            maximum number of dimensions
    !       mmax            maximum number of neighbours
    !       a(m,n)          work array 
    !       b(m)            work array 
    !
    !   Output:
    !       vol         volume (n=3) or area (n=2) of 
    !                   voronoi cell about x.
    !
    !   Comments:
    !        The first-order voronoi cell is defined by the perpendicular
    !        bisectors of x and each of its neighbours p(n,i).
    !        Each bisector forms a linear inequality constraint and 
    !        the voronoi cell is the region which satisfies all 
    !        constraints.
    !
    !        This routine determines the matrix a(i,j) and vector b(j)   
    !        of the linear system and calls the recursive C routine
    !        `volume' to calculate the volume (area) of the voronoi cell.
    !
    !
    !                   J. Braun,
    !                   & M. Sambridge, RSES, 1995.
    !
    !------------------------------------------------------------------------
    !
          Subroutine first_voronoi (x,p,n,m,mmax,nmax,a,b,vol)
                implicit none

            integer(4) :: n, m, mmax, nmax, i, j
            real(8) :: x(nmax),p(nmax,mmax)
            real(8) :: a(mmax,nmax),b(mmax)
            real(8) :: vol

    !                   Set up coefficients 
    !                   of linear system

          do 5 j=1,m
             do 6 i=1,n
                a(j,i)=p(i,j)-x(i)
     6       continue
             b(j)=0.0_8
             do 7 i=1,n
                b(j)=b(j)+p(i,j)*p(i,j)-x(i)*x(i)
     7       continue
             b(j)=b(j)/2.0_8
     5    continue


          call volume (a,b,m,n,mmax,nmax,vol)

          if(vol.eq.-1.0_8)then
    !         write(*,*)' Warning in routine first_voronoi'
    !         write(*,*)' Voronoi cell is unbounded'
    !         write(*,*)' volume is set to zero'
             vol = 0.0_8
          else if(vol.eq.0.0_8)then
             write(*,*)' Warning in routine first_voronoi'
             write(*,*)' An inconsistent set of constraints has been'
             write(*,*)' detected in calculating the volume of '
             write(*,*)' a Voronoi cell.'
             write(*,*)' volume is set to zero'
          end if

          return
          end subroutine
    !
    !------------------------------------------------------------------------
    !
    !   second_voronoi - calculates the area or volume of the second-order
    !            voronoi polygon (polyhedron) between x and its
    !            neighbour p(n,1).
    !
    !   Input:
    !       x(n)            input point 
    !       p(n,m)          neighbours shared by x and p(n,1)
    !       n           number of dimensions
    !       m           number of neighbours
    !       nmax            maximum number of dimensions
    !       mmax            maximum number of neighbours
    !       xo(n)           a local origin point.
    !       a(m,n)          work array 
    !       b(m)            work array 
    !
    !   Output:
    !       vol         volume (n=3) or area (n=2) of 
    !                   voronoi cell about x.
    !
    !   Comments:
    !        The second-order voronoi cell between x and its neighbour p 
    !        is defined by the perpendicular bisector between x and p, 
    !        and the bisectors between p and the nodes which are neighbours 
    !        of both p and x. Each bisector forms a linear inequality 
    !        constraint and the second-order voronoi cell is the region 
    !        which satisfies all constraints.
    !
    !        This routine determines the matrix a(i,j) and vector b(j)   
    !        of the linear system and calls the recursive C routine
    !        `volumef' to calculate the volume (area) of the voronoi cell.
    !
    !        The point p is stored in the first row of input array p 
    !        (p(i,1),i=1,n). xo is any `nearby' point used as the origin
    !        for the calculation. It's use is only to help reduce
    !        roundoff error. For perfect arithmetic it will not affect  
    !        the calculated voronoi volume.
    !
    !                   J. Braun, 
    !                   & M. Sambridge, RSES, 1995.
    !
    !------------------------------------------------------------------------
    !
          Subroutine second_voronoi (x,p,n,m,mmax,nmax,xo,a,b,vol)

          integer(4) :: nmax,mmax,i,n,j,m
          real(8) :: vol

          real(8)    x(nmax)
          real(8)    p(nmax,mmax)
          real(8)    xo(nmax)
          real(8)      a(mmax,nmax)
          real(8)  b(mmax)
          real(8)    dx1,dx2

    !                   Set up coefficients 
    !                   of linear system

          do 5 i=1,n
             a(1,i)=dble(p(i,1)-x(i))
     5    continue
          b(1)=0.0_8
          do 6 i=1,n
             dx1 = p(i,1)-xo(i)
             dx2 = x(i)-xo(i)
             b(1)=b(1) + dble(dx1*dx1 - dx2*dx2)
     6    continue
          b(1)=b(1)/2.0_8

          do 7 j=2,m
             do 8 i=1,n
                a(j,i)=dble(p(i,j)-p(i,1))
     8       continue 
             b(j)=0.0_8
             do 9 i=1,n
                dx1 = p(i,j)-xo(i)
                dx2 = p(i,1)-xo(i)
                b(j)=b(j) + dble(dx1*dx1 - dx2*dx2)
     9       continue
             b(j)=b(j)/2.0_8
     7    continue


          call volumef (a,b,m,n,mmax,nmax,vol)

          if(vol.eq.-1.0_8)then
             write(*,*)' Warning in routine second_voronoi'
             write(*,*)' Second order Voronoi cell is unbounded'
             write(*,*)' volume is set to zero'
             vol = 0.0_8
          else if(vol.eq.0.0_8)then
             write(*,*)' Warning in routine second_voronoi'
             write(*,*)' An inconsistent set of constraints has been'
             write(*,*)' detected in calculating the volume of '
             write(*,*)' the second-order Voronoi cell.'
             write(*,*)' volume is set to zero'
          end if

          return
          end subroutine
    !
    !------------------------------------------------------------------------
    !
    !   second_voronoi_d - calculates the area or volume of the second-order
    !              voronoi polygon (polyhedron) between x and its
    !              neighbour p(n,1). It also calculates the
    !              derivatives of the area (volume) with respect to
    !              the co-ordinates of x. 
    !
    !   Input:
    !       x(n)            input point 
    !       p(n,m)          neighbours shared by x and p(n,1)
    !       n           number of dimensions
    !       m           number of neighbours
    !       nmax            maximum number of dimensions
    !       mmax            maximum number of neighbours
    !       xo(n)           a local origin point.
    !       a(m,n)          work array 
    !       b(m)            work array 
    !
    !   Output:
    !       vol         volume (n=3) or area (n=2) of 
    !                   voronoi cell about x.
    !       deriv(n)        first derivatives dv/dx_i (i=1,n)
    !
    !   Comments:
    !        See comments for routine second_voronoi.
    !
    !        Calls C routines volumef, volumeb and dvdaf.
    !
    !                   J. Braun, 
    !                   & M. Sambridge, RSES, 1995.
    !
    !------------------------------------------------------------------------
    !
          subroutine second_voronoi_d (x,p,n,m,mmax,nmax,xo,a,b,vol,deriv)

          integer(4) :: nmax,mmax,i,n,j,m,k,icode
          real(8) :: vol,dvdb,deriva

          real(8)    x(nmax)
          real(8)    p(nmax,mmax)
          real(8)    xo(nmax)
          real(8)      a(mmax,nmax)
          real(8)  b(mmax)
          real(8)  deriv(nmax)
          real(8)    dx1,dx2

    !                   Set up coefficients 
    !                   of linear system

          do 5 i=1,n
             a(1,i)=dble(p(i,1)-x(i))
     5    continue
          b(1)=0.0_8
          do 6 i=1,n
             dx1 = p(i,1)-xo(i)
             dx2 = x(i)-xo(i)
             b(1)=b(1) + dble(dx1*dx1 - dx2*dx2)
     6    continue
          b(1)=b(1)/2.0_8

          do 7 j=2,m
             do 8 i=1,n
                a(j,i)=dble(p(i,j)-p(i,1))
     8       continue 
             b(j)=0.0_8
             do 9 i=1,n
                dx1 = p(i,j)-xo(i)
                dx2 = p(i,1)-xo(i)
                b(j)=b(j) + dble(dx1*dx1 - dx2*dx2)
     9       continue
             b(j)=b(j)/2.0_8
     7    continue

    !     write(6,*)' A and b'
    !     do 100 j=1,m
    !        write(6,*)(a(j,i),i=1,n),' : ',b(j)
    !100  continue
    !     write(6,*)' '
    !                       calculate volume and
    !                       derivative d(vol)/db(1)

          call volumeb (a,b,m,n,mmax,nmax,1,vol,dvdb)

          if(vol.eq.-1.0_8)then
             write(*,*)' Warning in routine second_voronoi_d'
             write(*,*)' Second order Voronoi cell is unbounded'
             write(*,*)' volume and derivatives set to zero'
             vol = 0.0_8
             do 10 k=1,n
                deriv(k) = 0.0_8
     10      continue
          else if(vol.eq.0.0_8)then
             write(*,*)' Warning in routine second_voronoi_d'
             write(*,*)' An inconsistent set of constraints has been'
             write(*,*)' detected in calculating the volume of '
             write(*,*)' the second-order Voronoi cell.'
             write(*,*)' volume and derivatives set to zero'
             do 11 k=1,n
                deriv(k) = 0.0_8
     11      continue
          else
    !                       calculate derivatives
    !                       d(vol)/da(1,j), (j=1,n),
    !                       and use formulae to find
    !                       d(vol)/dx(k).
             do 20 k=1,n
       
               call dvdaf (a,b,m,n,mmax,nmax,k,deriva,icode)

               if(icode.eq.-1)then
                  write(*,*)' Warning in routine second_voronoi_d'
                  write(*,*)' Second order Voronoi cell is unbounded'
                  write(*,*)' derivatives set to zero'
               else if(icode.eq.1)then
                  write(*,*)' Warning in routine second_voronoi_d'
                  write(*,*)' An inconsistent set of constraints has been'
                  write(*,*)' detected in calculating the volume of '
                  write(*,*)' the second-order Voronoi cell.'
                  write(*,*)' All derivatives set to zero'
                  do 30 i=1,n
                     deriv(i) = 0.0_8
     30           continue
               end if

               deriv(k) = -1.0_8*(dvdb*(x(k)-xo(k)) + deriva)
     20      continue 
          end if

          return
          end subroutine
    !
    !------------------------------------------------------------------------
    !
    !   Heapsort - Sorts an array into ascending order.
    !          Modified from numerical recipes 2, to use a 
    !          two dimensional double precision array.
    !
    !          Sorting is done on index ka (ka=1 or 2).
    !
    !------------------------------------------------------------------------
    !
    !
          SUBROUTINE hpsort_d(n,ka,ra)
          INTEGER n,kb
    !     REAL ra(n)
          REAL*8 ra(2,n)
          INTEGER i,ir,j,l,ka
    !     REAL rra
          REAL*8 rra(2)
          kb = 1
          if(ka.eq.1)kb = 2
          if (n.lt.2) return
          l=n/2+1
          ir=n
    10    continue
            if(l.gt.1)then
              l=l-1
              rra(ka)=ra(ka,l)
              rra(kb)=ra(kb,l)
            else
              rra(ka)=ra(ka,ir)
              rra(kb)=ra(kb,ir)
              ra(ka,ir)=ra(ka,1)
              ra(kb,ir)=ra(kb,1)
              ir=ir-1
              if(ir.eq.1)then
                ra(ka,1)=rra(ka)
                ra(kb,1)=rra(kb)
                return
              endif
            endif
            i=l
            j=l+l
    20      if(j.le.ir)then
              if(j.lt.ir)then
                if(ra(ka,j).lt.ra(ka,j+1))j=j+1
              endif
              if(rra(ka).lt.ra(ka,j))then
                ra(ka,i)=ra(ka,j)
                ra(kb,i)=ra(kb,j)
                i=j
                j=j+j
              else
                j=ir+1
              endif
            goto 20
            endif
            ra(ka,i)=rra(ka)
            ra(kb,i)=rra(kb)
          goto 10
          END subroutine
    !
    !------------------------------------------------------------------------
    !
    !   pangle - pseudo angle routine 
    !
    !        returns a number between 0 and 360 which is NOT the
    !        angle made by the line from p1 to p2 with the horizontal
    !        but which has the same order properties as that angle,
    !        i.e. has the same order of angles as arctan dy/dx.
    !        This function involves only simple products and quotients.
    !
    !        From Sedgewick (1990) `Algorithms in C' (Addison Wesley)
    !
    !                       M. Sambridge 1996.
    !
    !------------------------------------------------------------------------
    !
        Function pangle(p1,p2)

        real(8)      p1(2)
        real(8)      p2(2)
        real(8) :: pangle,dx,ax,dy,ay,t,a

        dx = p2(1) - p1(1)
            ax = abs(dx)
        dy = p2(2) - p1(2)
            ay = abs(dy)
            t = 0.0_8
            a = ax+ay
        if(a.ne.0.0_8)then
               t = dy/a
            end if
            if(dx.lt.0.0_8)then
              t = 2.0_8-t
            else if(dy.lt.0.0_8)then 
              t = 4.0_8+t
            end if
            pangle = t*90.0_8

        return
        end function
    !
    !------------------------------------------------------------------------
    !
    !   Circum - calculates circum-centre of three points
    !
    !   Input:
    !       pa,pb,pc        array of input points   
    !
    !   Output:
    !               centre(3)               centre(j) (j=1,2) contains the
    !                                       co-ordinates of the centre of 
    !                                       circle passing through the
    !                   three input points. 
    !   Comments:
    !
    !        Solves 3x3 linear system of equations.
    !
    !        No calls to other routines.
    !
    !                   M. Sambridge, RSES, April 1996.
    !
    !------------------------------------------------------------------------
    !
        Subroutine circum(pa,pb,pc,centre)
    !
        real(8)      pa(2),pb(2),pc(2)
        real(8)      centre(2)
        real(8)      x1,x2,x3,y1,y2,y3
        real(8)      dx2m1,dx2p1,dy2m1,dy2p1
        real(8)      dx3m1,dx3p1,dy3m1,dy3p1
        real(8)      denom
    !                       Find centre of circum-circle
           x1 = pa(1)
           x2 = pb(1)
           x3 = pc(1)
           y1 = pa(2)
           y2 = pb(2)
           y3 = pc(2)

               dx2m1 = x2-x1
               dx2p1 = x2+x1
               dy2m1 = y2-y1
               dy2p1 = y2+y1
               dx3m1 = x3-x1
               dx3p1 = x3+x1
               dy3m1 = y3-y1
               dy3p1 = y3+y1
               denom = dx2m1*dy3m1-dx3m1*dy2m1

           centre(1) = ((dx2m1*dx2p1 + dy2m1*dy2p1)*dy3m1 &
                  -(dx3m1*dx3p1 + dy3m1*dy3p1)*dy2m1)/ &
                  (denom)

           centre(2) = (dx2m1*(dx3m1*dx3p1 + dy3m1*dy3p1)  &
                  -dx3m1*(dx2m1*dx2p1 + dy2m1*dy2p1))/  &
                  (denom)
    !
               centre(1) = centre(1)*0.5d0
               centre(2) = centre(2)*0.5d0

        return
        end subroutine
    !
    !------------------------------------------------------------------------
    !
    !   Circum_d - calculates circum-centre of three points and 
    !              derivatives of circum-centre with respect to 
    !          co-ordinates of first point.
    !
    !   Input:
    !       pa,pb,pc        array of input points   
    !
    !   Output:
    !               centre(3)               centre(j) (j=1,2) contains the
    !                                       co-ordinates of the centre of 
    !                                       circle passing through the
    !                   three input points. 
    !       vx(2)           derivative of centre with respect
    !                   to x-component of pa
    !       vy(2)           derivative of centre with respect
    !                   to y-component of pa
    !   Comments:
    !
    !        Solves 3x3 linear system of equations and calculates
    !        derivatives of circum-centre with respect to co-ordinates
    !        of input vector pa(2).
    !
    !        No calls to other routines.
    !
    !                   M. Sambridge, RSES, May 1996.
    !
    !------------------------------------------------------------------------
    !
        Subroutine circum_d(pa,pb,pc,centre,vx,vy)
    !
        real(8)      pa(2),pb(2),pc(2)
        real(8)      centre(2)
        real(8)      x1,x2,x3,y1,y2,y3
        real(8)      dx2m1,dx2p1,dy2m1,dy2p1
        real(8)      dx3m1,dx3p1,dy3m1,dy3p1
        real(8)      denom
        real(8)      vx(2),vy(2)
        real(8) :: DCXM1,DCYM1,DENUM1,DENUM2
    !                       Find centre of circum-circle
           x1 = pa(1)
           x2 = pb(1)
           x3 = pc(1)
           y1 = pa(2)
           y2 = pb(2)
           y3 = pc(2)

               dx2m1 = x2-x1
               dx2p1 = x2+x1
               dy2m1 = y2-y1
               dy2p1 = y2+y1
               dx3m1 = x3-x1
               dx3p1 = x3+x1
               dy3m1 = y3-y1
               dy3p1 = y3+y1
               denom = dx2m1*dy3m1-dx3m1*dy2m1

           centre(1) = ((dx2m1*dx2p1 + dy2m1*dy2p1)*dy3m1 &
                  -(dx3m1*dx3p1 + dy3m1*dy3p1)*dy2m1)/ &
                  (denom)

           centre(2) = (dx2m1*(dx3m1*dx3p1 + dy3m1*dy3p1)  &
                  -dx3m1*(dx2m1*dx2p1 + dy2m1*dy2p1))/  &
                  (denom)
    !
               centre(1) = centre(1)*0.5d0
               centre(2) = centre(2)*0.5d0
    !                       X-derivative
    !
               dcxm1 = centre(1) - x1
               dcym1 = centre(2) - y1

               denum1 = (dy3m1 - dy2m1)/denom
               denum2 = (dx2m1 - dx3m1)/denom

           vx(1) = dcxm1*denum1
           vx(2) = dcxm1*denum2

    !                       Y-derivative
    !
           vy(1) = dcym1*denum1
           vy(2) = dcym1*denum2

        return
        end subroutine
    !
    !------------------------------------------------------------------------
    !
    !   Circum_dd - calculates circum-centre of three points and 
    !           1st and 2nd derivatives of circum-centre with 
    !           respect to co-ordinates of first point.
    !
    !   Input:
    !       pa,pb,pc        array of input points   
    !
    !   Output:
    !               centre(3)               centre(j) (j=1,2) contains the
    !                                       co-ordinates of the centre of 
    !                                       circle passing through the
    !                   three input points. 
    !       vx(2)           derivative of centre with respect
    !                   to x-component of pa
    !       vy(2)           derivative of centre with respect
    !                   to y-component of pa
    !   Comments:
    !
    !        Solves 3x3 linear system of equations and calculates
    !        derivatives of circum-centre with respect to co-ordinates
    !        of input vector pa(2).
    !
    !        No calls to other routines.
    !
    !                   M. Sambridge, RSES, May 1996.
    !
    !------------------------------------------------------------------------
    !
        Subroutine circum_dd(pa,pb,pc,centre,vx,vy,vxx,vyy,vxy)
    !
        real(8)      pa(2),pb(2),pc(2)
        real(8)      centre(2)
        real(8)      x1,x2,x3,y1,y2,y3
        real(8)      dx2m1,dx2p1,dy2m1,dy2p1
        real(8)      dx3m1,dx3p1,dy3m1,dy3p1
        real(8)      denom
        real(8)      vx(2),vy(2),vxx(2),vyy(2),vxy(2)
        real(8) :: DCXM1,DCYM1,DENUM1,DENUM2,f11,f22,f12
    !
    !                       Find centre of circum-circle
           x1 = pa(1)
           x2 = pb(1)
           x3 = pc(1)
           y1 = pa(2)
           y2 = pb(2)
           y3 = pc(2)

               dx2m1 = x2-x1
               dx2p1 = x2+x1
               dy2m1 = y2-y1
               dy2p1 = y2+y1
               dx3m1 = x3-x1
               dx3p1 = x3+x1
               dy3m1 = y3-y1
               dy3p1 = y3+y1
               denom = dx2m1*dy3m1-dx3m1*dy2m1

           centre(1) = ((dx2m1*dx2p1 + dy2m1*dy2p1)*dy3m1 &
                  -(dx3m1*dx3p1 + dy3m1*dy3p1)*dy2m1)/ &
                  (denom)

           centre(2) = (dx2m1*(dx3m1*dx3p1 + dy3m1*dy3p1)  &
                  -dx3m1*(dx2m1*dx2p1 + dy2m1*dy2p1))/  &
                  (denom)
    !
               centre(1) = centre(1)*0.5d0
               centre(2) = centre(2)*0.5d0
    !                       X-derivative
    !
               dcxm1 = centre(1) - x1
               dcym1 = centre(2) - y1

               denum1 = (dy3m1 - dy2m1)/denom
               denum2 = (dx2m1 - dx3m1)/denom

           vx(1) = dcxm1*denum1
           vx(2) = dcxm1*denum2
    !                       Y-derivative
    !
           vy(1) = dcym1*denum1
           vy(2) = dcym1*denum2
    !                       Second derivatives
           f11 = 2.0_8*vx(1) - 1.0_8
           f22 = 2.0_8*vy(2) - 1.0_8
           f12 = vy(1) + vx(2)

               vxx(1) = f11*denum1
               vxx(2) = f11*denum2
               vyy(1) = f22*denum1
               vyy(2) = f22*denum2
               vxy(1) = f12*denum1
               vxy(2) = f12*denum2

        return
        end subroutine
    !
    !------------------------------------------------------------------------
    !
    !                       heapsort modified to 
    !                       sort two arrays  
    !
    !------------------------------------------------------------------------
    !
          SUBROUTINE hpsort_two(n,ra,rb)
          INTEGER n
          REAL(8) :: ra(n)
          REAL*8 rb(2,n)
          INTEGER i,ir,j,l
          REAL(8) :: rra
          REAL*8 rrb1,rrb2
          if (n.lt.2) return
          l=n/2+1
          ir=n
    10    continue
            if(l.gt.1)then
              l=l-1
              rra=ra(l)
              rrb1=rb(1,l)
              rrb2=rb(2,l)
            else
              rra=ra(ir)
              rrb1=rb(1,ir)
              rrb2=rb(2,ir)
              ra(ir)=ra(1)
              rb(1,ir)=rb(1,1)
              rb(2,ir)=rb(2,1)
              ir=ir-1
              if(ir.eq.1)then
                ra(1)=rra
                rb(1,1)=rrb1
                rb(2,1)=rrb2
                return
              endif
            endif
            i=l
            j=l+l
    20      if(j.le.ir)then
              if(j.lt.ir)then
                if(ra(j).lt.ra(j+1))j=j+1
              endif
              if(rra.lt.ra(j))then
                ra(i)=ra(j)
                rb(1,i)=rb(1,j)
                rb(2,i)=rb(2,j)
                i=j
                j=j+j
              else
                j=ir+1
              endif
            goto 20
            endif
            ra(i)=rra
            rb(1,i)=rrb1
            rb(2,i)=rrb2
          goto 10
          END subroutine
    !
    !------------------------------------------------------------------------
    !
    !                   Numerical Recipes routine index
    !
    !------------------------------------------------------------------------
    !
          SUBROUTINE indexx(n,arr,indx)
          INTEGER n,indx(n),M,NSTACK
          REAL(8) :: arr(n)
          PARAMETER (M=7,NSTACK=50)
          INTEGER i,indxt,ir,itemp,j,jstack,k,l,istack(NSTACK)
          REAL(8) :: a
          do 11 j=1,n
            indx(j)=j
    11    continue
          jstack=0
          l=1
          ir=n
    1     if(ir-l.lt.M)then
            do 13 j=l+1,ir
              indxt=indx(j)
              a=arr(indxt)
              do 12 i=j-1,1,-1
                if(arr(indx(i)).le.a)goto 2
                indx(i+1)=indx(i)
    12        continue
              i=0
    2         indx(i+1)=indxt
    13      continue
            if(jstack.eq.0)return
            ir=istack(jstack)
            l=istack(jstack-1)
            jstack=jstack-2
          else
            k=(l+ir)/2
            itemp=indx(k)
            indx(k)=indx(l+1)
            indx(l+1)=itemp
            if(arr(indx(l+1)).gt.arr(indx(ir)))then
              itemp=indx(l+1)
              indx(l+1)=indx(ir)
              indx(ir)=itemp
            endif
            if(arr(indx(l)).gt.arr(indx(ir)))then
              itemp=indx(l)
              indx(l)=indx(ir)
              indx(ir)=itemp
            endif
            if(arr(indx(l+1)).gt.arr(indx(l)))then
              itemp=indx(l+1)
              indx(l+1)=indx(l)
              indx(l)=itemp
            endif
            i=l+1
            j=ir
            indxt=indx(l)
            a=arr(indxt)
    3       continue
              i=i+1
            if(arr(indx(i)).lt.a)goto 3
    4       continue
              j=j-1
            if(arr(indx(j)).gt.a)goto 4
            if(j.lt.i)goto 5
            itemp=indx(i)
            indx(i)=indx(j)
            indx(j)=itemp
            goto 3
    5       indx(l)=indx(j)
            indx(j)=indxt
            jstack=jstack+2
            if(jstack.gt.NSTACK) then
                print *, 'NSTACK too small in indexx'
            endif
            if(ir-i+1.ge.j-l)then
              istack(jstack)=ir
              istack(jstack-1)=i
              ir=j-1
            else
              istack(jstack)=j-1
              istack(jstack-1)=l
              l=i
            endif
          endif
          goto 1
          END subroutine
    !  (C) Copr. 1986-92 Numerical Recipes Software '%1&9p#!.
    !
    !------------------------------------------------------------------------
    !
    !   second_v_area - calculates the area of a second-order Voronoi 
    !           cell using an un-ordered list of vertices and a
    !               a closed formula. 
    !
    !   Input:
    !       x(2)            input point 
    !       p(2,n)          vertices of polygon.
    !       n           number of vertices
    !       a(n)            pseudo angles of nodes with 
    !                   respect to input point
    !
    !   Output:
    !       area            area of polygon X 2
    !
    !   Comments:
    !        The vertices may be input in any order but they are 
    !        re-ordered anti-clockwise upon output.
    !       
    !        Calls are made to pangle, hpsort_two.
    !
    !                   M. Sambridge, RSES, May 1996.
    !
    !------------------------------------------------------------------------
    !
          Subroutine second_v_area(n,p,a,area)

          real(8)    p(2,*)
          real(8)    theta
          real(8)    a(*)
          integer(4) :: i,n,j
          real(8) :: area
    !                           calculate pseudo angles
    !                       from first node
    !
          do i=2,n
            a(i) = pangle(p(1,1),p(1,i))
          end do
          a(1) = -1.0_8
    !
          theta = a(2)
          do i=2,n
             a(i) = a(i) - theta
            if(a(i).lt.0.0_8)a(i) = a(i) + 360.0_8
          end do
          theta = a(2)
    !
    !     write(*,*)' unsorted angles'
    !     do i=1,n
    !        write(*,*)i,' :',a(i),p(1,i),p(2,i)
    !     end do
    !                       sort these nodes by 
    !                       pseudo angle from first edge
          call hpsort_two(n,a,p)

          if(a(2).ne.theta)write(*,*)' ERROR: first angle moved ?'

    !     write(*,*)' sorted angles'
    !     do i=1,n
    !        write(*,*)i,' :',a(i),p(1,i),p(2,i)
    !     end do
    !                       calculate area of 
    !                       second-order Voronoi cell.
          area = 0.0_8
          do i=1,n-1
             j = i+1
             area = area + p(1,i)*p(2,j) - p(2,i)*p(1,j)
          end do
          area = area + p(1,n)*p(2,1) - p(2,n)*p(1,1)
          area = abs(area)
          
          return
          end subroutine
    !
    !------------------------------------------------------------------------
    !
    !   second_v_area_d - calculates the area of a second-order Voronoi 
    !             cell using an un-ordered list of vertices and a
    !                 a closed formula. 
    !
    !   Input:
    !       x(2)            input point 
    !       p(2,n)          vertices of polygon.
    !       dp(4,2)         derivatives of first two vertices 
    !                   of polygon.
    !       n           number of vertices
    !       a(n)            pseudo angles of nodes with 
    !                   respect to input point
    !
    !   Output:
    !       area            area of polygon X 2
    !       df(2)           first derivatives of area X 2
    !                         df(1) = df/dx,
    !                         df(2) = df/dy
    !
    !   Comments:
    !        The first two vertices are assumed to be the vertices
    !        dependent on x, i.e. the vertices on the voronoi cell about x.
    !        These two vertices must be in clockwise order.
    !        The remaining vertices may be input in any order but they are 
    !        re-ordered anti-clockwise upon output.
    !       
    !        Calls are made to pangle, hpsort_two.
    !
    !                   M. Sambridge, RSES, April 1996.
    !
    !------------------------------------------------------------------------
    !
          Subroutine second_v_area_d(n,p,dp,a,area,df)

          real(8)    p(2,*)
          real(8)    dp(4,2)
          real(8)    df(2)
          real(8)    theta
          real(8)    a(*)
          integer(4) :: i,n,j
          real(8) :: area,D222N,D1N12,D2321,D1113
    !
    !                           calculate pseudo angles
    !                       from first node
    !
          do i=2,n
            a(i) = pangle(p(1,1),p(1,i))
          end do
          a(1) = -1.0_8
    !
    !     write(*,*)' first pseudo angle =',theta 
    !
          theta = a(2)
          do i=2,n
             a(i) = a(i) - theta
            if(a(i).lt.0.0_8)a(i) = a(i) + 360.0_8
          end do
          theta = a(2)
    !
    !     write(*,*)' unsorted angles'
    !     do i=1,n
    !        write(*,*)i,' :',a(i),p(1,i),p(2,i)
    !     end do
    !                       sort these nodes by 
    !                       pseudo angle from first edge
          call hpsort_two(n,a,p)

          if(a(2).ne.theta)write(*,*)' ERROR: first angle moved ?'

    !                       plot nodes and v-cell
    !     write(*,*)' sorted angles'
    !     do i=1,n
    !        write(*,*)i,' :',a(i),p(1,i),p(2,i)
    !     end do
    !                       calculate area of 
    !                       second-order Voronoi cell.
          area = 0.0_8
          do i=1,n-1
             j = i+1
             area = area + p(1,i)*p(2,j) - p(2,i)*p(1,j)
          end do
          area = area + p(1,n)*p(2,1) - p(2,n)*p(1,1)
    !
    !                       calculate 1st derivatives
          d222n = p(2,2) - p(2,n) 
          d1n12 = p(1,n) - p(1,2) 
          d2321 = p(2,3) - p(2,1)
          d1113 = p(1,1) - p(1,3)
          df(1) =   dp(1,1)*d222n + dp(2,1)*d1n12  &
                 + dp(1,2)*d2321 + dp(2,2)*d1113
          df(2) =   dp(3,1)*d222n + dp(4,1)*d1n12  &
                 + dp(3,2)*d2321 + dp(4,2)*d1113

          if(area.lt.0.0_8)then
            df(1) = -df(1)
            df(2) = -df(2)
          end if

          area = abs(area)
           
          return
          end subroutine
    !
    !------------------------------------------------------------------------
    !
    !   second_v_area_dd - calculates the area of a second-order Voronoi 
    !              cell using an un-ordered list of vertices and a
    !                  a closed formula. 
    !
    !   Input:
    !       x(2)            input point 
    !       p(2,n)          vertices of polygon.
    !       dp(4,2)         derivatives of first 
    !                   two vertices of polygon.
    !       n           number of vertices
    !       a(n)            pseudo angles of nodes with 
    !                   respect to input point
    !
    !   Output:
    !       area            area of polygon X 2
    !       df(2)           first derivatives of area X 2
    !                         df(1) = df/dx
    !                         df(2) = df/dy
    !       ddf(3)          second derivatives of area X 2
    !                         ddf(1) = d2f/dxx
    !                         ddf(2) = d2f/dyy
    !                         ddf(3) = d2f/dxy
    !
    !   Comments:
    !        The first two vertices are assumed to be the vertices
    !        dependent on x, i.e. the vertices on the voronoi cell about x.
    !        These two vertices must be in clockwise order.
    !        The remaining vertices may be input in any order but they are 
    !        re-ordered anti-clockwise upon output.
    !       
    !        Calls are made to pangle, hpsort_two and xplot routines.
    !
    !                   M. Sambridge, RSES, April 1996.
    !
    !------------------------------------------------------------------------
    !
          Subroutine second_v_area_dd(n,p,dp,ddp,a,area,df,ddf)

          real(8)    p(2,*)
          real(8)    dp(4,2)
          real(8)    ddp(6,2)
          real(8)    df(2),ddf(3)
          real(8)    theta
          real(8)    a(*)
          integer(4) :: i,n,j
          real(8) :: area,D222N,D1N12,D2321,D1113
    !                           calculate pseudo angles
    !                       from first node
    !
          do i=2,n
            a(i) = pangle(p(1,1),p(1,i))
          end do
          a(1) = -1.0_8
    !
    !     write(*,*)' first pseudo angle =',theta 
    !
          theta = a(2)
          do i=2,n
            a(i) = a(i) - theta
            if(a(i).lt.0.0_8)a(i) = a(i) + 360.0_8
          end do
          theta = a(2)
    !
    !     write(*,*)' unsorted angles'
    !     do i=1,n
    !        write(*,*)i,' :',a(i),p(1,i),p(2,i)
    !     end do
    !                       sort these nodes by 
    !                       pseudo angle from first edge
          call hpsort_two(n,a,p)

          if(a(2).ne.theta)write(*,*)' ERROR: first angle moved ?'

    !                       plot nodes and v-cell
    !     write(*,*)' sorted angles'
    !     do i=1,n
    !        write(*,*)i,' :',a(i),p(1,i),p(2,i)
    !     end do
    !                       calculate area of 
    !                       second-order Voronoi cell.
          area = 0.0_8
          do i=1,n-1
             j = i+1
             area = area + p(1,i)*p(2,j) - p(2,i)*p(1,j)
          end do
          area = area + p(1,n)*p(2,1) - p(2,n)*p(1,1)
    !
    !                       calculate 1st derivatives
          d222n = p(2,2) - p(2,n) 
          d1n12 = p(1,n) - p(1,2) 
          d2321 = p(2,3) - p(2,1)
          d1113 = p(1,1) - p(1,3)
          df(1) =   dp(1,1)*d222n + dp(2,1)*d1n12  &
                 + dp(1,2)*d2321 + dp(2,2)*d1113
          df(2) =   dp(3,1)*d222n + dp(4,1)*d1n12  &
                 + dp(3,2)*d2321 + dp(4,2)*d1113

    !                       calculate 2nd derivatives
         
          ddf(1) =   ddp(1,1)*d222n + ddp(2,1)*d1n12  &
                  + ddp(1,2)*d2321 + ddp(2,2)*d1113 &
                  + 2.0_8*(dp(1,1)*dp(2,2) - dp(2,1)*dp(1,2))
          ddf(2) =   ddp(3,1)*d222n + ddp(4,1)*d1n12  &
                  + ddp(3,2)*d2321 + ddp(4,2)*d1113 &
                  + 2.0_8*(dp(3,1)*dp(4,2) - dp(4,1)*dp(3,2))
          ddf(3) =   ddp(5,1)*d222n + ddp(6,1)*d1n12  &
                  + ddp(5,2)*d2321 + ddp(6,2)*d1113 &
                  + dp(1,1)*dp(4,2) - dp(2,1)*dp(3,2) &
                  + dp(3,1)*dp(2,2) - dp(4,1)*dp(1,2)

          if(area.lt.0.0_8)then
            df(1) = -df(1)
            df(2) = -df(2)
            ddf(1) = -ddf(1)
            ddf(2) = -ddf(2)
            ddf(3) = -ddf(3)
          end if

          area = abs(area)
           
          return
          end subroutine

    end module m_nn2

