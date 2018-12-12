        module m_solve_diffusion
          contains
          subroutine solve_diffusion (configData)
            use rt_param
            use cascade_globals
            implicit none
!      subroutine solve_diffusion (a1,a2,ntri,nnode,b,h,hp,icon,jc,kc,nc,ncmax,diag,&
!                                  sea_level,outflux,glacier)

! this subroutine solves iteratively (using Gauss-Siedel + overrelaxation)
! an algebraic system. The matrix is stored EBE in condensed form (a1 and a2)
! the right hand-side vector (b) is formed at the begining of iterations
! as a2*h0
! the solution is returned in h
! fix stores the b.c. (fix=0 means fixed height; fix=1 means variable height)
! 
! jc(ncmax,i) is an array giving the list of triangles to which node i
! belongs; there is a maximum of ncmax triangles to which any node may belong;
! node i belongs to nc(i) triangles
! for each node, kc(ncmax,i) gives the number (1-3) that node i has in triangle
! jc(ncmax,i).

! INPUT: a1        = elemental conductivity matrix
!        a2        = elemental mass matrix
!        ntri      = number of triangles (elements)
!        nnode     = number of nodes
!        b         = right hand-side vector
!        hp        = working array
!        icon      = connectivity matric (nodes to elements) = vertices
!        fix       = boundary conditions
!        jc        = working array
!        kc        = working array
!        nc        = working array
!        ncmax     = maximum number of elements to which a node may be attached
!        diag      = working array
!        sea_level = sea level
!        surface   = surface associated to each node
!   glacier     = integer array of the location of glaciers (1=glacier, 0=no glacier)

! OUPUT: h         = updated topography
!        outflux   = updated flux through the boundaries

! subroutines called:
! NONE

            type(config) :: configData
            real(8) :: beta, dh, dhcont, dhmax, dhmin, htol, sup, tol
            integer(4) :: i, i1, i2, i3, it, k, mcalc, ncalc, itmax, jt
			integer(4) :: ProblemCounter
		

!      real     ael1(6,ntri),ael2(6,ntri),b(nnode)
!      integer  vertices(3,ntri)
!      real     fix(nnode)
!      real     h(nnode),hp(nnode),diag(nnode)
!      integer  jc(ncmax,nnode),kc(ncmax,nnode),nc(nnode)
!      real     surface(nnode)
!      integer   glacier(nnode)

        beta=1.2_8
        tol=1.e-4_8
        itmax=1000

        ncalc = 0
        mcalc = 0
        dhmin = 0.0_8
        dhmax = 0.0_8
        htol = 0.0_8
        dh = 0.0_8
        sup = 0.0_8

      if (ivocal) open (19,file='iteration.out',status='unknown')

        hmax=0.0_8
        do i=1,configData%nnode
            bel(i)=0.0_8
            hp(i)=h(i)
            diag(i)=1.0_8
            hmax=max(hmax,abs(h(i)))
        enddo

        do it=1,nt
            i1=vertices(1,it)
            i2=vertices(2,it)
            i3=vertices(3,it)
            if ((i1 <= configData%nnode) .and. &
                (i2 <= configData%nnode) .and. &
                (i3 <= configData%nnode)) then
                bel(i1)=bel(i1)+ael2(1,it)*hp(i1)+ael2(2,it)*hp(i2)+ael2(4,it)*hp(i3)
                bel(i2)=bel(i2)+ael2(2,it)*hp(i1)+ael2(3,it)*hp(i2)+ael2(5,it)*hp(i3)
                bel(i3)=bel(i3)+ael2(4,it)*hp(i1)+ael2(5,it)*hp(i2)+ael2(6,it)*hp(i3)
                diag(i1)=diag(i1)+ael1(1,it)
                diag(i2)=diag(i2)+ael1(3,it)
                diag(i3)=diag(i3)+ael1(6,it)
            endif
        enddo

      htol=max(hmax,1.0_8)*tol

      ncalc=0
110   mcalc=0
      ncalc=ncalc+1
!      if (ncalc.gt.itmax) goto 999
ProblemCounter=0
      dhmin=hmax
      dhmax=0.0_8
        do i=1,configData%nnode
        sup=bel(i)
! loop thru all triangles connected to node i
          do it=1,nkcon(i)

! get id of triangle
          jt=kcon(it,i)

! get position of node i in triangle
          k=jcon(it,i)
          if (k.eq.1) then
          sup=sup-(ael1(1,jt)*h(vertices(1,jt))+ael1(2,jt)*h(vertices(2,jt))+ael1(4,jt)*h(vertices(3,jt)))
          elseif (k.eq.2) then
          sup=sup-(ael1(2,jt)*h(vertices(1,jt))+ael1(3,jt)*h(vertices(2,jt))+ael1(5,jt)*h(vertices(3,jt)))
          else
          sup=sup-(ael1(4,jt)*h(vertices(1,jt))+ael1(5,jt)*h(vertices(2,jt))+ael1(6,jt)*h(vertices(3,jt)))
          endif

        enddo

! this dh is the dh during the solution iteration
!   bjy 101109 added 1-glacier to mask ice
        dhcont=(1.0_8-dble(glacier(i)))*beta*sup/diag(i)
!        call check_var(2, "solve_diffusion, dhcont, beta, sup 3$", dhcont, i, beta, sup)
!   dhcont=beta*sup/diag(i)
!        dh=memory(i,5)*dhcont

        if (memory(i,5) .gt. 0.5) then
			dh = dhcont
		else
			dh = 0.0_8
		end if

!        call check_var(2, "solve_diffusion, dhm dhcont 2$", dh, i, dhcont)

        if (h(i).lt.configData%sea_level) dh=0.0_8

!        if (ncalc.gt.itmax) then
!			dh = 0.0_8
!		if (ncalc.gt.itmax) then
!			dh = abs(htol)
! 		if (ncalc.gt.itmax) dh = htol ! Not tried yet
		if (ncalc .gt. itmax) then
		ProblemCounter = ProblemCounter + 1
			if (ProblemCounter > 100) then
				stop "Too many nodes without solution in solve_diffusion.f90"
			end if
!		if (abs(dh) .gt. abs(htol)) then
			dh = htol
			print*, "Problem node detected in solve_diffusion.f90: ", i, &
			"x: ", x(i), " y: ", y(i), " dh: ",dh, " h: ",h(i), " hp: ", hp(i), " outflux: ", outflux, " dhcont: ", dhcont
		end if
! enforce constant elevation boundary conditions

        h(i)=hp(i)+dh
!        call check_var(2, "solve_diffusion, h(i), hp(i), dh 1$", h(i), i, hp(i), dh)

        if (ivocal) then
         dhmin=min(dhmin,abs(dh))
         dhmax=max(dhmax,abs(dh))
        endif

		if (memory(i,5) .lt. 0.5) then
			outflux = outflux - dhcont * memory(i,7)
		end if

!        outflux=outflux+(memory(i,5)-1.0_8)*dhcont*memory(i,7)

        if (abs(dh).gt.abs(htol)) mcalc=mcalc+1

        enddo

! update height working array
        do i=1,configData%nnode
            hp(i)=h(i)
        enddo

      if (ivocal) write (19,*) ncalc,mcalc,dhmin,dhmax,htol

      if (mcalc.ne.0) goto 110

      if (ivocal) close (19)

      return

999   if (ivocal) print*,'Have a look in iteration.out'
      close (19)
      stop 'too many iterations...'

            end subroutine solve_diffusion
        end module m_solve_diffusion

