! check_mesh

        module m_check_mesh
            contains

              subroutine check_mesh (configData)
                    use rt_param
                    use cascade_globals
                    use m_debug
                    use m_check_for_removal
                    use m_find_surface
                    use m_delaun
                    use m_nn_remove

                    implicit none
!      subroutine check_mesh (x,y,h,h0,hi,memory,nmemory,itime, &
!                             param,nparam, &
!                             nn,nb,nnode, &
!                             nbmax,nnodemax,ntmax, &
!                             points,vertices,neighbour,nodes, &
!                             vis_tlist,vis_elist,add_tlist,nt, &
!                             slope,water, &
!                             xx,pp,aa,bb, &
!                             nadd,itadd,jtadd, &
!                             dt,surfmin, &
!                             kcon,nkcon,ndon, &
!                             nnode0, &
!                             nodelist,tlist,c_list,v_local,n_local, &
!                             inactive,eps,surfscale,delta,bdry, &
!                             nn2,nb2,cell)

! This routine checks the mesh to update it during dynamic remeshing
! The flux (dh*surface) of material removed by river erosion is used
! in this version but any measure could be used to improve the resolution
! locally.
! It is important that, if this routine is used, that is if dynamic 
! remeshing is turned on, all nodal parameters that you have added (such
! as a new nodal property) be passed here for what is called permutation.
! During permutation, nodes are renumbered (see near bottom of the
! subroutine) and nodal properties have to be updated accordingly.
!  In this new version all the properties and parameters that have to
! be permuted are stored in memory and param

! param(*,1)=fluvial erosion constant
! param(*,2)=bedrock erosion length scale
! param(*,3)=diffusion erosion constant

! memory(*,1)=dhcrit
! memory(*,2)=dhfluvial
! memory(*,3)=dhdiff
! memory(*,4)=hiso
! memory(*,5)=fix
! memory(*,6)=newsurface
! memory(*,7)=surface
! memory(*,8)=dhls 2/7/2

! INPUT: x,y      = x-, y-coordinates of the nodes
!        h        = topography
!        h0       = bedrock-alluvion interface
!        hi       = initial topography
!        memory   = properties to be memorized (and thus properly
!                   carried through) over a time step
!        nmemory  = number of properties to be memorized
!        itime    = current time step
!        param    = erosional parameters
!        nparam   = number of erosional parameters
!        nn       = neighbour matrix
!        nb       = number of neighbours per node
!        nnode    = number of nodes
!        nbmax    = maximum number of neighbours per node
!        nnodemax = maximum number of nodes
!        ntmax    = maximum number of Delaunay triangles
!        points   = real*8 array used in Delaunay routine
!        vertices = triangulation array used in Delaunay routine
!        neighbour= neighbour array used in Delaunay routine
!        nodes    = working array used in Delaunay routine
!        vis_tlist= working array used in Delaunay routine
!        vis_elist= working array used in Delaunay routine
!        add_tlist= working array used in Delaunay routine
!        nt       = number of Delaunay triangles
!        slope    = "nodal" slopes (not used in this version)
!        water    = nodal discharge (not used in this version)
!        xx       = working array used in Voronoi cell volume calculation
!        pp       = working array used in Voronoi cell volume calculation
!        aa       = working array used in Voronoi cell volume calculation
!        bb       = working array used in Voronoi cell volume calculation
!        nadd     = number of nodes added
!        itadd    = working array used in the adding algorithm
!        jtadd    = working array used in the adding algorithm
!        dt       = time step
!        surfmin  = minimum surface allowed (to prevent run away)
!        kcon     = connectivity array (list of elements attached to
!                   a given node)
!        nkcon    = number of elements attached to a given node
!        ndon     = donor array
!        nnode0   = initial number of nodes (these nodes cannot be
!                   removed)
!        nodelist = working array used in the node remove algorithm
!        tlist    = working array used in the node remove algorithm
!        c_list   = working array used in the node remove algorithm
!        v_local  = working array used in the node remove algorithm
!        n_local  = working array used in the node remove algorithm
!        inactive = working array used in Delaunay routine
!        eps      = working array used in Delaunay routine
!        surfscale= mean surface attached to a node
!        bdry     = flag to identify moving boundary nodes (added 11/28/1 DS)

! OUTPUT: updated mesh
!         the following arrays and variables are updated:
!             memory, param, nnode, nt, x, y, h, h0, hi, vertices
!             neighbour, nn, nb, kcon, nkcon, nadd

! subroutines called:
! - debug
! - check_for_removal
! - nn_remove
! - find_surface
! - delaun

!      real              x(*),y(*)
!      real              h(*),h0(*),hi(*)
!      real              water(*),slope(*)
!      integer           nn(nbmax,*)
!      integer           nb(*)

!      real              param(nnodemax,nparam)
!      real              memory(nnodemax,nmemory)

!      real*8        points(2,*)
!      real*8            eps
!      integer       vertices(3,*)
!      integer       neighbour(3,*)
!      integer           nodes(*)
!      integer           vis_tlist(*)
!      integer           vis_elist(*)
!      integer           add_tlist(*)
!      integer           nodelist(*)
!      integer           tlist(*)
!      logical           c_list(*)
!      integer           v_local(3,*)
!      integer           n_local(3,*)
!      logical           inactive(*)

!      real              xx(2),pp(2,nbmax),aa(nbmax,2),bb(nbmax)
!      integer           nkcon(*),ndon(*)
!      integer           kcon(ntmax,*),itadd(*),jtadd(*)
!      integer           bdry(*)

!      integer           nn2(nbmax,*),nb2(*),cell(nnodemax,nbmax,2)





            type(config) :: configData


! variables used by boundary node addition
            integer(4) :: bcnt

! variables used for height interpolation
            real(8), dimension(3) :: s

! variable used but not declared, WK
!            integer np, itstart

            integer(4), dimension(:,:), allocatable :: nn3
            integer(4), dimension(:), allocatable :: subset
            real(8) :: dhcritmin, fluxmax_erosion, tsurf, tsurfm
            real(8) :: x1, x2, x3, y1, y2, y3, MinimumNodeDistance, TriangleEdge1, TriangleEdge2, TriangleEdge3
            real(8) :: x1_remove, x2_remove, x3_remove, y1_remove, y2_remove, y3_remove, i_remove1, i_remove2, i_remove3
            integer(4) :: i, i0, i1, i2, i3, ic, ie, iess, ii, iloc, it
            integer(4) :: iremove, itstart, j, ja, jess, jess2, k, kmem, kpar
            integer(4) :: jb, mode, newn, np, nrem, numtri, nv_max
            integer(4) :: nadd
            real(8) :: Distance_x1, Distance_x2, Distance_x3, Distance_y1, Distance_y2, Distance_y3


        allocate(nn3(nbmax,nnodemax))
        allocate(subset(nnodemax))

!
! done with variable declaration
!


      if (ivocal) write (22,*) 'time step : ',itime

! fluxmax_erosion is the maximum flux allowed before a new node must
! be inserted

      fluxmax_erosion=configData%surfscale/global_cascade_dt*1.e-2_8

      nv_max=nnodemax

        do i=1,nnodemax
            memory(i,6)=0.0_8
            inactive(i)=.false.
            if (memory(i,1).gt.0.5_8) memory(i,1)=memory(i,1)+1.0_8
        enddo

! dhcritmin is the number of time steps that an "added"
! node (from the original distribution) is forced to remain active before
! being considered for removal

      dhcritmin=0.0_8

! first remove nodes where they are not needed any longer

      iloc=1
      numtri=nt
      nrem=0

! note that the first nnode0 nodes cannot be removed
! they form a sort of minimum set

12    continue
        if (ivocal) call debug ('check_for_removal$')
        do i=nnode0+1,configData%nnode
!        call check_for_removal (memory(i,7),memory(i,2), &
!                                configData%surfmin,global_cascade_dt,iremove)
!! ***** Method 1 *****
!! Calculate minimum surface area and triangle surface area (see code from Sean Willett further down), added by PRE Nov2017
!!   set maximum triangle size to delta squared
!
!        tsurfm=1.0e-2_8*delta*delta
!        i1=vertices(1,i)
!        i2=vertices(2,i)
!        i3=vertices(3,i)
!        x1=x(i1)
!        y1=y(i1)
!        x2=x(i2)
!        y2=y(i2)
!        x3=x(i3)
!        y3=y(i3)
!!    calculate triangle surface area
!        tsurf=(x1*y2+x2*y3+x3*y1-y1*x2-y2*x3-y3*x1)/2.0_8
!        if (tsurf.lt.tsurfm) then
!            iremove = 1
!        endif
!! *****

!! ***** Method 2 *****
!! Calculate length of triangle edges. If length is below the minimum requirement, remove node. PRE Nov2017
!! Set minimum requirement for triangle edge length
!        MinimumNodeDistance = ((configData%sidex / configData%nx + configData%sidey / configData%ny) / 2.0_8) !/ 10.0_8
!
!        i_remove1=vertices(1,i)
!        i_remove2=vertices(2,i)
!        i_remove3=vertices(3,i)
!        x1_remove=x(i_remove1)
!        y1_remove=y(i_remove1)
!        x2_remove=x(i_remove2)
!        y2_remove=y(i_remove2)
!        x3_remove=x(i_remove3)
!        y3_remove=y(i_remove3)
!
!! Calculate triangle edge lengths
!        TriangleEdge1 = sqrt((x2_remove-x1_remove)**2 + (y2_remove-y1_remove)**2)
!        TriangleEdge2 = sqrt((x2_remove-x3_remove)**2 + (y2_remove-y3_remove)**2)
!        TriangleEdge3 = sqrt((x1_remove-x3_remove)**2 + (y1_remove-y3_remove)**2)
!        if ((TriangleEdge1 < MinimumNodeDistance) &
!            .or. (TriangleEdge2 < MinimumNodeDistance) &
!            .or. (TriangleEdge3 < MinimumNodeDistance)) then
!            iremove = 1
!        end if
!! *****

! ***** Method 3 *****
! Calculate distance to neighbouring points. If distance is below the minimum requirement, remove node. PRE Nov2017
! Set minimum requirement for triangle edge length
        MinimumNodeDistance = ((configData%sidex / configData%nx + configData%sidey / configData%ny) / 2.0_8) / 2.0_8

        i_remove1=vertices(1,i)
        i_remove2=vertices(2,i)
        i_remove3=vertices(3,i)
        x1_remove=x(i_remove1)
        y1_remove=y(i_remove1)
        x2_remove=x(i_remove2)
        y2_remove=y(i_remove2)
        x3_remove=x(i_remove3)
        y3_remove=y(i_remove3)

! Calculate distances to triangle nodes and assign removal if any of them in the x-y plane are smaller than the minimum requirement
        Distance_x1 = abs(x2_remove-x1_remove)
        Distance_x2 = abs(x2_remove-x3_remove)
        Distance_x3 = abs(x1_remove-x3_remove)
								Distance_y1 = abs(x2_remove-x1_remove)
        Distance_y2 = abs(x2_remove-x3_remove)
        Distance_y3 = abs(x1_remove-x3_remove)
        if ((Distance_x1 < MinimumNodeDistance) &
            .or. (Distance_x2 < MinimumNodeDistance) &
            .or. (Distance_x3 < MinimumNodeDistance) &
            .or. (Distance_y1 < MinimumNodeDistance) &
            .or. (Distance_y2 < MinimumNodeDistance) &
            .or. (Distance_y3 < MinimumNodeDistance)) then
            iremove = 1
        end if
! ********************

          if(iremove.eq.1) then
          nrem=nrem+1
          np=configData%nnode
          if (ivocal) write(22,*) -i
            do k=1,nb(i)
            memory(nn(k,i),6)=1.0_8
            enddo
          call nn_remove (i,np,numtri,points,vertices,neighbour,iloc,nbmax,nv_max, &
                          vis_tlist,vis_elist,add_tlist,&
                          v_local,n_local,c_list,nodelist,tlist,.false.)
            do j=i+1,configData%nnode
            points(1,j-1)=points(1,j)
            points(2,j-1)=points(2,j)
            x(j-1)=x(j)
            y(j-1)=y(j)
            h(j-1)=h(j)
            h0(j-1)=h0(j)
            hi(j-1)=hi(j)
              do kpar=1,nparam
                  param(j-1,kpar)=param(j,kpar)
              enddo
              do kmem=1,nmemory
                  memory(j-1,nmemory)=memory(j,nmemory)
              enddo
            enddo
            do it=1,numtri
              do k=1,3
                  if (vertices(k,it).gt.i) vertices(k,it)=vertices(k,it)-1
              enddo
            enddo
          configData%nnode=configData%nnode-1
            do j=1,configData%nnode
                nb(j)=0
            enddo
            do it=1,numtri
                i1=vertices(1,it)
                i2=vertices(2,it)
                i3=vertices(3,it)
                nb(i1)=nb(i1)+1
                  if (nb(i1).gt.nbmax) then
                    print *, "nbmax: ", nbmax, ", nb: ", nb(i1)
                    stop 'nbmax too small...1'
                  endif
                nn(nb(i1),i1)=i2
                nb(i2)=nb(i2)+1
                  if (nb(i2).gt.nbmax) then
                    print *, "nbmax: ", nbmax, ", nb: ", nb(i2)
                    stop 'nbmax too small...2'
                  endif
                nn(nb(i2),i2)=i3
                nb(i3)=nb(i3)+1
                  if (nb(i3).gt.nbmax) then
                    print *, "nbmax: ", nbmax, ", nb: ", nb(i3)
                    stop 'nbmax too small...3'
                  endif
                nn(nb(i3),i3)=i1
            enddo
          goto 12
          endif
        enddo

      if (ivocal) call debug ('check_mesh$')

      if (nrem.gt.0) then

!
! find nn2,nb2,cell
!
       do i=1,configData%nnode
        nb2(i) = 0
       enddo
 
       do it=1,numtri
        i1=vertices(1,it)
        i2=vertices(2,it)
        i3=vertices(3,it)
 
! i1
        nb2(i1)=nb2(i1)+1
        nn2(nb2(i1),i1)=i2
        cell(i1,int((1.+real(nb2(i1)))/2.),1)=i2
 
        nb2(i1)=nb2(i1)+1
        nn2(nb2(i1),i1)=i3
        cell(i1,int((1.+real(nb2(i1)))/2.),2)=i3
        if (nb2(i1).gt.nbmax) then
            print *, "nbmax: ", nbmax, ", nb2: ", nb2(i1)
            stop 'nbmax too small...4'
        endif
 
! i2
        nb2(i2)=nb2(i2)+1
        nn2(nb2(i2),i2)=i3
        cell(i2,int((1.+real(nb2(i2)))/2.),1)=i1
 
        nb2(i2)=nb2(i2)+1
        nn2(nb2(i2),i2)=i1
        cell(i2,int((1.+real(nb2(i2)))/2.),2)=i3
        if (nb2(i2).gt.nbmax) then
            print *, "nbmax: ", nbmax, ", nb2: ", nb2(i2)
            stop 'nbmax too small...5'
        endif
 
! i3
        nb2(i3)=nb2(i3)+1
        nn2(nb2(i3),i3)=i1
        cell(i3,int((1.+real(nb2(i3)))/2.),1)=i1
 
        nb2(i3)=nb2(i3)+1
        nn2(nb2(i3),i3)=i2
        cell(i3,int((1.+real(nb2(i3)))/2.),2)=i2
        if (nb2(i3).gt.nbmax) then
            print *, "nbmax: ", nbmax, ", nb2: ", nb2(i3)
            stop 'nbmax too small...6'
        endif
 
       enddo

!
! operate on nn2,nb2: added directly from find_neighbours.f
!
        do i=1,configData%nnode
 
! classement de nn2 de i le plus gd au plus pt
         do ie=1,nb2(i)-1
          ja=nn2(ie,i)
          jb=ie
          do jess=ie+1,nb2(i)
           jess2=nn2(jess,i)
           if(ja.lt.jess2) then
            ja=jess2
            jb=jess
           endif
          enddo
          nn2(jb,i)=nn2(ie,i)
          nn2(ie,i)=ja
         enddo
 
! elimination des pts doubles de enl
         i0=1
         nn3(1,i)=nn2(1,i)
         do iess=2,nb2(i)-1
          if(nn2(iess,i).ne.nn3(i0,i)) then
           nn3(i0+1,i)=nn2(iess,i)
           i0=i0+1
          endif
         enddo
         do j=1,i0
          nn2(j,i)=nn3(j,i)
         enddo
         nb2(i)=i0
        enddo
!
! back to regular code
! 

! recompute surfaces around removed nodes

      if (ivocal) call debug ('find_surface$')
!      call find_surface (nn,nb,memory(1,7),nbmax,nnode,x,y,xx,pp,aa,bb,memory(1,6),surfscale)
      call find_surface (configData)
      if (ivocal) call debug ('check_mesh$')

      do i=1,configData%nnode
          if (memory(i,7).eq.0.0_8) then
          print*,'surface nil at ',i,memory(i,6)
          print*,nb(i),(nn(k,i),k=1,nb(i))
            do k=1,nb(i)
                print*,x(nn(k,i)),y(nn(k,i))
            enddo
          endif
      enddo

! recompute connectivities around removed nodes

        do i=1,configData%nnode
        nkcon(i)=0
        enddo
        do it=1,numtri
        if (vertices(1,it).le.configData%nnode.and. vertices(2,it).le. &
            configData%nnode.and. vertices(3,it).le.configData%nnode) then
          do i=1,3
          ic=vertices(i,it)
          nkcon(ic)=nkcon(ic)+1
            if (nkcon(ic).gt.ntmax) then
            print *, "ic, nkcon(ic): " ,ic,nkcon(ic)
            print *,(kcon(ii,ic),ii=1,nkcon(ic)-1)
            stop 'check_mesh.f90: too tight connectivity...'
            endif
          kcon(nkcon(ic),ic)=it
          enddo
        endif
        enddo

      nt=numtri

      endif

! compute in which triangles points are to be added
! and location of new points

      nadd=0
        do it=1,nt
            jtadd(it)=0
        enddo
!
!   flux criteria
!
!        do i=1,nnode
!        surf=memory(i,7)
!        if (surf.gt.surfmin) then
!        flux=min(0.,surf*memory(i,2)/dt)
!          if(flux.lt.-fluxmax_erosion) then
!          do j=1,nkcon(i)
!          it=kcon(j,i)
!          i1=vertices(1,it)
!          i2=vertices(2,it)
!          i3=vertices(3,it)
!          if (i1.le.nnode.and.i2.le.nnode.and.i3.le.nnode .and.
!     &        jtadd(it).eq.0) then
!          jtadd(it)=1
!          nadd=nadd+1
!            if (nnode+nadd.gt.nnodemax) then
!            print*,nnode,nadd,nnode+nadd,nnodemax
!            print*,'Too many nodes in routine check_mesh/1'
!            stop
!       endif
!          itadd(nadd)=it
!     newn=nnode+nadd
!          if (ivocal) write (22,*) newn
!          x(newn)=(x(i1)+x(i2)+x(i3))/3.
!          y(newn)=(y(i1)+y(i2)+y(i3))/3.
!          h(newn)=(h(i1)+h(i2)+h(i3))/3.
!          h0(newn)=(h0(i1)+h0(i2)+h0(i3))/3.
!          hi(newn)=(hi(i1)+hi(i2)+hi(i3))/3.
!         memory(newn,1)=1.
!     memory(newn,2)=(memory(i1,2)+memory(i2,2)+memory(i3,2))/3.
!     memory(newn,3)=(memory(i1,3)+memory(i2,3)+memory(i3,3))/3.
!     memory(newn,4)=(memory(i1,4)+memory(i2,4)+memory(i3,4))/3.
!          memory(newn,5)=1.
!          if (memory(i1,5).eq.0. .and. memory(i2,5).eq.0.
!     &        .and. memory(i3,5).eq.0.) memory(newn,5)=0.
!            do kpar=1,nparam
!            param(newn,kpar)=
!     &      (param(i1,kpar)+param(i2,kpar)+param(i3,kpar))/3.
!            enddo
!          points(1,newn)=dble(x(newn))
!          points(2,newn)=dble(y(newn))
!          memory(newn,1)=1.
!          endif
!          enddo
!          endif
!          endif
!        enddo

!
!  add nodes based on triangle area criterion  sean willett
!
!   set maximum triangle size to delta squared

        tsurfm=2.0_8*delta*delta
        do it=1,nt
        i1=vertices(1,it)
        i2=vertices(2,it)
        i3=vertices(3,it)
        x1=x(i1)
        y1=y(i1)
        x2=x(i2)
        y2=y(i2)
        x3=x(i3)
        y3=y(i3)
!    calculate triangle surface area
        tsurf=(x1*y2+x2*y3+x3*y1-y1*x2-y2*x3-y3*x1)/2.0_8
        if (tsurf.gt.tsurfm) then
          jtadd(it)=1
          nadd=nadd+1
            if (configData%nnode+nadd.gt.nnodemax) then
            print*,configData%nnode,nadd,configData%nnode+nadd,nnodemax
            print*,'Too many nodes in routine check_mesh/1'
            stop
        endif
        itadd(nadd)=it
        newn=configData%nnode+nadd
        if (ivocal) write (22,*) newn
!
! adds point randomly inside of the triangle
!  uses isoparametric basis functions to interpolate
!  position, height, etc. to new point
!
          call random_number(s)
! scale random to avoid putting the new point too close to a vertex
          do ii=1,3
           s(ii) = 0.8_8*s(ii) + 0.1_8
          enddo

          bcnt = bdry(i1) + bdry(i2) + bdry(i3)
          if (bcnt.ne.2) then
           s(2) = (1.0_8-s(1))*s(2)
           s(3) = 1.0_8-s(1)-s(2)
           bdry(newn) = 0
          else
           if (bdry(i1)+bdry(i2).eq.2) then
            s(2) = 1.0_8-s(1)
            s(3) = 0.0_8
           elseif (bdry(i2)+bdry(i3).eq.2) then
            s(3) = 1.0_8-s(2)
            s(1) = 0.0_8
           else
            s(1) = 1.0_8-s(3)
            s(2) = 0.0_8
           endif
           bdry(newn) = 1
          endif

! interpolate values using linear interp:  h(x,y) = N1h1 + N2h2 + N3h3
!  for linear triangles N1=s(1) N2=s(2) N3=s(3)
!  the values of s(i) vary depending if the triangle is on the moving bdry
!
       x(newn) = s(1)*x(i1)+s(2)*x(i2)+s(3)*x(i3)
       y(newn) = s(1)*y(i1)+s(2)*y(i2)+s(3)*y(i3)
       h(newn) = s(1)*h(i1)+s(2)*h(i2)+s(3)*h(i3)
       h0(newn) = s(1)*h0(i1)+s(2)*h0(i2)+s(3)*h0(i3)
       hi(newn) = s(1)*hi(i1)+s(2)*hi(i2)+s(3)*hi(i3)
       memory(newn,1)=1.0_8
       memory(newn,2) = s(1)*memory(i1,2)+s(2)*memory(i2,2)+s(3)*memory(i3,2)
       memory(newn,3) = s(1)*memory(i1,3)+s(2)*memory(i2,3)+s(3)*memory(i3,3)
       memory(newn,4) = s(1)*memory(i1,4)+s(2)*memory(i2,4)+s(3)*memory(i3,4)
       memory(newn,8) = s(1)*memory(i1,8)+s(2)*memory(i2,8)+s(3)*memory(i3,8)

       memory(newn,6) = 0.0_8
       memory(newn,7) = 0.0_8

       memory(newn,5)=1.0_8
       if (memory(i1,5).eq.0.0_8 .and. memory(i2,5).eq.0.0_8 .and. memory(i3,5).eq.0.0_8) memory(newn,5)=0.0_8
       do kpar=1,nparam
        param(newn,kpar)=s(1)*param(i1,kpar)+s(2)*param(i2,kpar)+s(3)*param(i3,kpar)
       enddo

       points(1,newn)=dble(x(newn))
       points(2,newn)=dble(y(newn))
       memory(newn,1)=1.0_8
       endif
      enddo

      if (nadd.eq.0) then
       return
      endif

! compute new Delaunay triangulation

      mode=3
      np=configData%nnode+nadd
      itstart=1
      numtri=nt
      if (ivocal) call debug ('delaun$')
      call delaun (points,np,neighbour,vertices,numtri,2*np, &
                  vis_tlist,vis_elist,add_tlist,eps,nv_max, &
                  mode,inactive,configData%nnode+1,itstart,subset)
      if (ivocal) call debug ('check_mesh$')

      nt=numtri

      configData%nnode=configData%nnode+nadd

!
! compute neighbour list
!
 
        do i=1,configData%nnode
            nb(i)  = 0
            nb2(i) = 0
        enddo
 
!
! find nn,nb, nb2,nn2,cell
!
        do it=1,nt
        i1=vertices(1,it)
        i2=vertices(2,it)
        i3=vertices(3,it)
 
! i1
        nb(i1)=nb(i1)+1
        nn(nb(i1),i1)=i2
        if (nb(i1).gt.nbmax) then
            print *, "nbmax: ", nbmax, ", nb: ", nb(i1)
            stop 'nbmax too small...7'
        endif
 
        nb2(i1)=nb2(i1)+1
        nn2(nb2(i1),i1)=i2
        cell(i1,int((1.+real(nb2(i1)))/2.),1)=i2
 
        nb2(i1)=nb2(i1)+1
        nn2(nb2(i1),i1)=i3
        cell(i1,int((1.+real(nb2(i1)))/2.),2)=i3
        if (nb2(i1).gt.nbmax) then
            print *, "nbmax: ", nbmax, ", nb2: ", nb2(i1)
            stop 'nbmax too small...8'
        endif
 
! i2
        nb(i2)=nb(i2)+1
        nn(nb(i2),i2)=i3
        if (nb(i2).gt.nbmax) then
            print *, "nbmax: ", nbmax, ", nb: ", nb(i2)
            stop 'nbmax too small...9'
        endif
 
        nb2(i2)=nb2(i2)+1
        nn2(nb2(i2),i2)=i3
        cell(i2,int((1.+real(nb2(i2)))/2.),1)=i1
 
        nb2(i2)=nb2(i2)+1
        nn2(nb2(i2),i2)=i1
        cell(i2,int((1.+real(nb2(i2)))/2.),2)=i3
        if (nb2(i2).gt.nbmax) then
            print *, "nbmax: ", nbmax, ", nb2: ", nb2(i2)
            stop 'nbmax too small...10'
        endif
 
! i3
        nb(i3)=nb(i3)+1
        nn(nb(i3),i3)=i1
        if (nb(i3).gt.nbmax) then
            print *, "nbmax: ", nbmax, ", nb: ", nb(i3)
            stop 'nbmax too small...11'
        endif
 
        nb2(i3)=nb2(i3)+1
        nn2(nb2(i3),i3)=i1
        cell(i3,int((1.+real(nb2(i3)))/2.),1)=i1
 
        nb2(i3)=nb2(i3)+1
        nn2(nb2(i3),i3)=i2
        cell(i3,int((1.+real(nb2(i3)))/2.),2)=i2
        if (nb2(i3).gt.nbmax) then
            print *, "nbmax: ", nbmax, ", nb2: ", nb2(i3)
            stop 'nbmax too small...12'
        endif
 
        enddo

!
! operate on nn2,nb2 (added directly from find_neighbours.f)
!
        do i=1,configData%nnode
 
! classement de nn2 de i le plus gd au plus pt
         do ie=1,nb2(i)-1
          ja=nn2(ie,i)
          jb=ie
          do jess=ie+1,nb2(i)
           jess2=nn2(jess,i)
           if(ja.lt.jess2) then
            ja=jess2
            jb=jess
           endif
          enddo
          nn2(jb,i)=nn2(ie,i)
          nn2(ie,i)=ja
         enddo
 
! elimination des pts doubles de enl
         i0=1
         nn3(1,i)=nn2(1,i)
         do iess=2,nb2(i)-1
          if(nn2(iess,i).ne.nn3(i0,i)) then
           nn3(i0+1,i)=nn2(iess,i)
           i0=i0+1
          endif
         enddo
         do j=1,i0
          nn2(j,i)=nn3(j,i)
         enddo
         nb2(i)=i0
        enddo
!
! back to regular code
!


        do i=configData%nnode-nadd+1,configData%nnode
            memory(i,6)=1.0_8
              do j=1,nb(i)
                  memory(nn(j,i),6)=1.0_8
              enddo
        enddo

! finds new surfaces where needed

      if (ivocal) call debug ('find_surfacE$')
!      call find_surface (nn,nb,memory(1,7),nbmax,nnode,x,y,xx,pp,aa,bb,memory(1,6),surfscale)
      call find_surface (configData)
      if (ivocal) call debug ('check_mesh$')

        do i=1,configData%nnode
            nb(i)=nb(i)+1
              if (nb(i).gt.nbmax) then
                print *, "nbmax: ", nbmax, ", nb: ", nb(i)
                stop 'nbmax too small...12'
              endif
            nn(nb(i),i)=i
        enddo

        deallocate(nn3)
        deallocate(subset)


              return
              end subroutine check_mesh

end module m_check_mesh
