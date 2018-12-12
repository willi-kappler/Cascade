! find_surface

!      subroutine find_surface (nn,nb,surface,nbmax,nnode,x,y,xx,pp,a,b,newsurface,surfscale)
        module m_find_surface
          contains
          subroutine find_surface (configData)
                use rt_param
                use cascade_globals
                use m_debug
                use m_nn2

                implicit none


! subroutine to calculate the surface area (part of the landscape)
! that is attached to each node.
! note that some voronoi cell are unbounded (because their node is
! on the convex hull) and others are abnomalously large (because)
! their node is within one "row" of being on the convex hull
! I have imbeded a simple test that checks for infinite voronoi cells
! and for voronoi cells that are bigger than 10 times the average.
! Both types of nodes are given a "mean" surface area.

! INPUT:  nn         = natural neighbour list
!         nb         = number of natural neighbours per node
!         nbmax      = maximum number of natural neighbours
!         nnode      = number of nodes
!         x,y        = x- and y- coordinates of nodes
!         xx         = working array
!         pp         = working array
!         a          = working array (= aa)
!         b          = working array (= bb)
!         newsurface = which nodal surface area has to be updated = memory(1,6)
!         surfscale  = mean nodal surface = memory(1,7)

! OUTPUT: surface   = surface area attahced to each node
!         newsurface is reset to 0.

! subroutines called:
! - debug
! - first_voronoi

                type(config) :: configData
                real(8) :: vol
                integer(4) :: i, j, m

! surface = memory(1,7)

!      real    x(nnode),y(nnode),surface(nnode)
!      integer nb(nnode),nn(nbmax,nnode)
!      real    xx(2),pp(2,nbmax),a(nbmax,2),b(nbmax)
!      real    newsurface(nnode)

          if (ivocal) call debug ('first_voronoi$')
            do i=1,configData%nnode
                if (memory(i,6).gt.0.5_8) then
                    xy(1)=x(i)
                    xy(2)=y(i)
                    do j=1,nb(i)
                        pp(1,j)=x(nn(j,i))
                        pp(2,j)=y(nn(j,i))
                    enddo
                    m=nb(i)

                    ! WK: two dimensions for function volume, n = 2, nmax = 2
                    call first_voronoi (xy,pp,2,m,nbmax,2,aa,bb,vol)

! I have decided to remove the extra row of nodes around the perimeter
! of the real landscape; instead I check here for voronoi cells that
! have been given a volume of 0 (because they were unbounded) or
! for voronoi cells that are larger than 10 times the average value
! In both cases, the surface area attached to the node is set to some
! average value (surfscale=sidex*sidey/nnode)
! this modification may cause some unwanted results and needs to be
! fully tested
! alternatively I can come up with a better idea to remove the spurious
! voronoi cells that are generated for the nodes on the convex hull
! (infinite voronoi cells) or in its vicinity (very large voronoi cells)
                  if (vol.gt.configData%surfscale*10.0_8) then
                    memory(i,7)=configData%surfscale
                  elseif (vol.gt.0.0_8) then
                    memory(i,7)=vol
                  else
                    memory(i,7)=configData%surfscale
                  endif
                endif
            enddo
          if (ivocal) call debug ('find_surface$')

          return
          end subroutine find_surface
        end module m_find_surface


