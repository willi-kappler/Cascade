! tectonic_movement


        module m_tectonic_movement
            contains
              subroutine tectonic_movement (configData)
                    use rt_param
                    use cascade_globals
                    use m_debug
                    use m_find_surface
                    use m_del_flip

                    implicit none


!      subroutine tectonic_movement (x,y,nnode,fix,dt,itime,points,vertices,neighbour, &
!                                    surface,newsurface,nt,nn,nb,nbmax,nswaps, &
!                                    mask,mask_e,xy,pp,aa,bb,surfscale,delta,tcheck,sidex,sidey, &
!                                    advec_vel,nn2,nb2,cell)

! this routine allows the points of the grid to be moved around
! due to horizontal tectonic movements

! INPUT: x, y      = x- and y-nodal coordinates
!        nnode     = number of nodes
!        fix       = boundary conditions
!        dt        = time step length
!        itime     = time step
!        points    = working array
!        vertices  = connectivity list
!        neighbour = triangle neighbour list
!        surface   = surface attached to each node
!        newsurface= to determine whether nodal surface has to be reestimated
!        nt        = number of triangles
!        nn        = neighbour list
!        nb        = number of neighbour per node
!        nbmax     = maximum number of neighbours
!        mask      = working array
!        mask_e    = working array
!        xy        = working array
!        pp        = working array
!        aa        = working array
!        bb        = working array
!
! DS 7/27/1
!        delta     = reference grid spacing [km]
!        tcheck    = time since last check [yrs]
!        sidex     = length in x direction [km]
!        sidey     = length in y direction [km]
!        advec_vel = maximum advection velocity [km/yr]

! OUTPUT: x,y       = updated nodal coordinates
!         nt        =number of triangles
!        vertices  = connectivity list
!        neighbour = triangle neighbour list
!        surface   = surface attached to each node
!        nn        = neighbour list
!        nb        = number of neighbour per node
!        nswaps    = number of triangle side swaps operated during the
!                    grid reorganisation

! subroutines called:
! - debug
! - del_flip
! - find_surface


!      real*4    x(nnode),y(nnode)
!      real*8    points(2,*)
!      real      fix(nnode)
!      integer   vertices(3,*),neighbour(3,*)
!      real*4    surface(nnode)
!      real      newsurface(nnode)
!      integer   nb(nnode),nn(nbmax,nnode)
!      logical   mask(*),mask_e(3,*)
!      real      xy(2),pp(2,nbmax),aa(nbmax,2),bb(nbmax)

!      integer   nb2(nnode),nn2(nbmax,nnode),cell(nnode,nbmax,2)



            type(config) :: configData
            integer(4) :: i, i0, i1, i2, i3, ie, iess, it
            integer(4) :: ja, jess, jess2, j, jb, nswaps
            integer(4), dimension(:,:), allocatable :: nn3

            allocate(nn3(nbmax,configData%nnode))


! This is pure shear shortening in Y with velocity (advec_vely) D.S 7/27/1
      do i=1,configData%nnode
       if(memory(i, 5).gt.0.5_8)then
        y(i)=y(i)-configData%advec_vely*(y(i)/configData%sidey)*global_cascade_dt
       endif
      enddo

! This is pure shear shortening in X with velocity (advec_velx) D.S 7/27/1
      do i=1,configData%nnode
       if(memory(i, 5).gt.0.5_8)then
        x(i)=x(i)-configData%advec_velx*(x(i)/configData%sidex)*global_cascade_dt
       endif
      enddo



! this is the spinning landscape
!      omega=2.*3.1415/1.e6
!        do i=1,nnode
!        x0=x(i)-50.
!        y0=y(i)-50.
!          if (x0**2+y0**2.lt.25.**2) then
!          x(i)=x(i)-omega*dt*y0
!          y(i)=y(i)+omega*dt*x0
!          endif
!        enddo

! perform a check to see if mesh needs to be updated DS 11/27/1 
!  remember endif statement at end if commenting out
      if ((configData%advec_vely*tcheck.gt.delta/10.0_8).or. &
          (configData%advec_velx*tcheck.gt.delta/10.0_8)) then
 
! reset tcheck
      tcheck = 0.0_8

! from here on you should not change anything...
      do i=1,configData%nnode
       points(1,i)=x(i)
       points(2,i)=y(i)
      enddo

      do i=1,configData%nnode
        memory(i, 6)=0.0_8
      enddo

      if (ivocal) call debug ('del_flip$')
      call del_flip (points,neighbour,vertices,nt,mask,mask_e,nswaps)
        do it=1,nt
            if (mask(it)) then
                memory(vertices(1,it), 6)=1.0_8
                memory(vertices(2,it), 6)=1.0_8
                memory(vertices(3,it), 6)=1.0_8
            endif
        enddo
      if (ivocal) call debug ('tectonic_movement$')

! if there were any swapping the natural neighbours and surfaces need to be 
! recalculated...

      if (nswaps.ne.0) then

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
                stop 'nbmax too small...1'
            endif
     
            nb2(i1)=nb2(i1)+1
            nn2(nb2(i1),i1)=i2
            cell(i1,int((1.+real(nb2(i1)))/2.),1)=i2
     
            nb2(i1)=nb2(i1)+1
            nn2(nb2(i1),i1)=i3
            cell(i1,int((1.+real(nb2(i1)))/2.),2)=i3
            if (nb2(i1).gt.nbmax) then
                print *, "nbmax: ", nbmax, ", nb2: ", nb2(i1)
                stop 'nbmax too small...2'
            endif
     
        ! i2
            nb(i2)=nb(i2)+1
            nn(nb(i2),i2)=i3
            if (nb(i2).gt.nbmax) then
                print *, "nbmax: ", nbmax, ", nb: ", nb(i2)
                stop 'nbmax too small...3'
            endif
     
            nb2(i2)=nb2(i2)+1
            nn2(nb2(i2),i2)=i3
            cell(i2,int((1.+real(nb2(i2)))/2.),1)=i1
     
            nb2(i2)=nb2(i2)+1
            nn2(nb2(i2),i2)=i1
            cell(i2,int((1.+real(nb2(i2)))/2.),2)=i3
            if (nb2(i2).gt.nbmax) then
                print *, "nbmax: ", nbmax, ", nb2: ", nb2(i2)
                stop 'nbmax too small...4'
            endif
     
        ! i3
            nb(i3)=nb(i3)+1
            nn(nb(i3),i3)=i1
            if (nb(i3).gt.nbmax) then
                print *, "nbmax: ", nbmax, ", nb: ", nb(i3)
                stop 'nbmax too small...5'
            endif
     
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
! added directly from find_neighbours.f
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

      if (ivocal) call debug ('find_surface$')
      call find_surface (configData)
!      call find_surface (nn,nb,surface,nbmax,nnode,x,y,xy,pp,aa,bb,newsurface,surfscale)
      if (ivocal) call debug ('tectonic_movement$')

        do i=1,configData%nnode
        nb(i)=nb(i)+1
          if (nb(i).gt.nbmax) then
            print *, "nbmax: ", nbmax, ", nb: ", nb(i)
            stop 'nbmax too small...7'
          endif
        nn(nb(i),i)=i
        enddo

      endif

      endif

      deallocate(nn3)

              return

              end subroutine tectonic_movement
        end module m_tectonic_movement

