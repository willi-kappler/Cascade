
        module m_check_for_removal
            contains
            subroutine check_for_removal (surface,dh,surfmin,dt, iremove)

! subroutine where the condition for node removal is defined

! INPUT: surface     = surface attached to current node
!        dh          = incremental erosion over the current time step
!        nb          = number of neighbours per node
!        nn          = list of neighbours
!        fluxmax_erosion = maximum erosive flux permitted
!        surfmin     = minimum surface permitted
!        dt          = current time step length

! OUTPUT:
!        iremove     = 0 keep it
!                    = 1 remove it

! subroutines called:
! NONE

          integer(4) :: iremove
          real(8) :: flux, surface, dh, dt, surfmin


          iremove=0
          flux=min(0.0_8,surface*dh/dt)
!        do j=1,nb
!        jj=nn(j)
!        flux=flux+min(0.,surface*dh/dt)
!        enddo

! original code:
!      if(flux.gt.-fluxmax_erosion/10. .or.
!     &   surface.lt.surfmin/2.) iremove=1

          if (surface.lt.surfmin/4.0_8) iremove=1
          return
          end subroutine check_for_removal
        end module m_check_for_removal

