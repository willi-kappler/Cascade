! diffusion_erosion

        module m_diffusion_erosion
            contains
              subroutine diffusion_erosion (configData)
                    use rt_param
                    use cascade_globals
                    use m_debug
                    use m_build_a
                    use m_solve_diffusion

                    implicit none
!      subroutine diffusion_erosion (xkdiff,x,y,h,hp,dh,nnode,ntri,ntmax,vertices,kcon,jcon,nkcon,&
!                                    a1,a2,bel,fix,diag,dt,dhmindiff,dhmaxdiff,sea_level,outflux,surface,glacier)

! subroutine to calculate diffusion erosion

! INPUT: xkdiff     = nodal diffusivity = param(1,3)
!        x, y       = x- and y- nodal coordinates
!        h          = topography
!        hp         = working array (initial topography)
!        dh         = total height removed/added = memory(1,3)
!        nnode      = number of nodes
!        ntri       = number of triangles (here finite elements)
!        ntmax      = maximum number of elements attached to a node
!        vertices       = connectivity matrix
!        kcon       = list of elements attached to a node
!        jcon       = node number in the equivalent element
!        nkcon       = number of elements attached to a node
!        a1         = elemental conductivity matrix
!        a2         = elemental mass matrix
!        bel          = right hand side vector
!        fix        = boundary condition array
!        diag       = working array (diagonal of matrix to be solved
!                     by Gauss-siedel method)
!        dt         = time step length (in yrs)
!        dhmindiff      = minimum amount eroded/deposited
!        dhmaxdiff      = maximum amount eroded/deposited
!        sea_level  = sea-level in meters
!        outflux    = flux out of the system through base level
!        surface    = array containing the surface attached to each node
!	glacier     = integer array of the location of glaciers (1=glacier, 0=no glacier)
! OUTPUT: a range of arrays/variables are updated:
!        h, dh, dhmindiff, dhmaxdiff, outflux
!         working arrays are modified

! subroutines calles:
! - debug
! - build_a
! - solve_diffusion



            type(config) :: configData
            integer(4) :: i, i1, i2, i3, ic, it, ii
            real(8) :: alpha, xk

            ic = 1

!      real    x(nnode),y(nnode),h(nnode),hp(nnode),dh(nnode)
!      real    xkdiff(nnode)
!      integer vertices(3,ntri)
!      real    fix(nnode)
!      integer kcon(ntmax,nnode),jcon(ntmax,nnode),nkcon(nnode)
!      real    a1(6,ntri),a2(6,ntri),bel(nnode),diag(nnode)
!      real    surface(nnode)
!      integer 	 glacier(nnode)
! stores initial height

        do i=1,configData%nnode
            hp(i)=h(i)
            memory(i,3)=h(i)
        enddo

! finds inverse connectivities

        do i=1,configData%nnode
            nkcon(i)=0
        enddo

        do it=1,nt
        if (vertices(1,it).le.configData%nnode .and. vertices(2,it).le. &
            configData%nnode.and. vertices(3,it).le.configData%nnode) then
          do i=1,3
          ic=vertices(i,it)
          nkcon(ic)=nkcon(ic)+1
            if (nkcon(ic).gt.ntmax) then
            print*,ic,nkcon(ic)
            print*,(kcon(ii,ic),ii=1,nkcon(ic)-1)
            stop 'too tight connectivity...'
            endif
          kcon(nkcon(ic),ic)=it
          jcon(nkcon(ic),ic)=i
          enddo
        endif
        enddo

! builds matrices and right-hand sides vectors

!        alpha      = time integration parameter (0 = explicit)
!                                                (1 = implicit)
      alpha=1.0_8
      if (ivocal) call debug ('build_a$')
        do it=1,nt
        i1=vertices(1,it)
        i2=vertices(2,it)
        i3=vertices(3,it)
        xk=(param(i1,3)+param(i2,3)+param(i3,3))*(global_cascade_dt/3.0_8)
          if (i1.le.configData%nnode.and.i2.le.configData%nnode.and.i3.le.configData%nnode) then
          call  build_a (x(i1),x(i2),x(i3),y(i1),y(i2),y(i3),xk,alpha,ael1(1,it),ael2(1,it))
          endif
        enddo
      if (ivocal) call debug ('diffusion_erosion$')

        if (ivocal) call debug ('solve_diffusion$')
        call solve_diffusion(configData)
!        call solve_diffusion(ael1,ael2,ntri,configData%nnode,bel,h,hp,vertices,&
!                             kcon,jcon,nkcon,ntmax,diag,&
!                             configData%sea_level,outflux,glacier)
        if (ivocal) call debug ('diffusion_erosion$')

! dh contains original heights
        dhmindiff=h(1)-memory(1,3)
        dhmaxdiff=h(1)-memory(1,3)
        do i=1,configData%nnode

! this lines don't enforce any bc's
         memory(i,3)=memory(i,5)*(h(i)-memory(i,3))

         dhmindiff=min(dhmindiff,memory(i,3))
         dhmaxdiff=max(dhmaxdiff,memory(i,3))
!         dhmindiff=amin1(dhmindiff,memory(i,3))
!         dhmaxdiff=amax1(dhmaxdiff,memory(i,3))
        enddo

      return
      end subroutine diffusion_erosion
      end module m_diffusion_erosion

