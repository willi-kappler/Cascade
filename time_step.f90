        module m_time_step
            contains
            subroutine time_step (dhmax,dhdt_allowed,dtice,lastdh,chmax)
            implicit none

            real(8) :: dhdt_allowed,dtice,dhmax,lastdh,chmax
! estimate new time step
!       dh_min=1
            chmax=abs(dhmax-lastdh)/lastdh
            if (dhmax.gt.dhdt_allowed) then
                dtice=dtice*0.1_8
!/(dhmax/dh_allowed)
!        elseif (dhmax.lt.dh_min) then
            elseif (dhmax.lt.0.1_8) then
                dtice=dtice*1.011_8
!        dt=min(dt,dtmax)

            elseif (chmax.lt.0.01_8) then
                dtice=dtice*1.011_8
            endif
!      dt=max(dt,0.09)
!dt=dt0

            lastdh=dhmax
          return
          end subroutine time_step
        end module m_time_step

