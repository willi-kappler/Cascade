! tectonic_uplift
        module m_tectonic_uplift
          contains
          subroutine tectonic_uplift(configData)
                use rt_param
                use cascade_globals
                implicit none

!      subroutine tectonic_uplift(x,y,h,h0,hi,nnode,fix,dt,time,influx,surface,uplift_rate,shelf)

! this routine defines the tectonic uplift function
! it may vary in space and time

! INPUT: x,y       = x- and y-nodal coordinates
!        h         = present topography
!        h0        = bedrock-alluvials interface
!        hi        = original topograpy
!        nnode     = number of nodes
!        fix       = boundary conditions
!        dt        = time step length
!        time      = current time
!        surface   = surface associated with each node
!        uplift_rate = rate in m/yr - defined w/ general params
!       shelf       =array controlling where uplift occurs
! OUTPUT:  h        = update topography
!          hi       = updated original topograpy
!          h0       = updated bedrock-alluvials interface
!          influx   = updated influx of material by tectonic uplift

! subroutines called:
! NONE

!      real     x(nnode),y(nnode),h(nnode),h0(nnode),hi(nnode)
!      real     fix(nnode),shelf(nnode)
!      real     surface(nnode),influx
!      real     uplift_rate

!      real     dij

                type(config) :: configData
                integer(4) :: inode
                real(8) :: dh



! the example is a square topography that stops growing after a while
! note that you can use the array fix to prevent the base level to move
! with tectonic uplift

! commented out 6/15/1, now in initialize_general_parameters.f
!      uplift_rate=2.E-3

          do inode=1,configData%nnode

! uplift the topography keeping the boundary pinned
           if (memory(inode, 5) > 0.5_8) then

            dh=configData%uplift_rate*global_cascade_dt*shelf(inode)

! uplift interior 
            h(inode) = h(inode) + (dh*shelf(inode))
!        if (shelf(inode) > 0.0 ) then
!            print *, "h(inode), dh, uplift_rate, dt, shelf: ", &
!            h(inode), dh, configData%uplift_rate, global_cascade_dt, shelf(inode)
!        endif

!       print*,dh,shelf(inode),h(inode),dt,uplift_rate
! added 6/15/1
!  rectangular uplift pattern
!        if (x(inode).gt.3.0.and.x(inode).lt.30.-3.0.and.
!     &      y(inode).gt.3.0.and.y(inode).lt.30.-3.0) then
!         h(inode)=h(inode)+dh
!        endif
!

!  rectangular uplift pattern
!      if (x(inode).gt.2.0.and.x(inode).lt.8.0) then
!       h(inode) = h(inode) + dh
!      endif

! changes added 6/14/1
!     circular uplift pattern
!     dij = sqrt((x(inode)-25.)**2+(y(inode)-25.)**2)
!      if (dij.lt.10.) then
!       h(inode)=h(inode)+dh
!      endif
      
           else
                dh = 0.0_8
           endif
! do not touch the following lines
! they update h0, hi and calculate the influx of material into the
! landscape
            h0(inode)=h0(inode)+(dh)
            hi(inode)=hi(inode)+(dh)
            influx=influx+dh*memory(inode,7)

           enddo

          return
          end subroutine tectonic_uplift
        end module m_tectonic_uplift

