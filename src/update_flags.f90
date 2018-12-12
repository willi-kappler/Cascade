        module m_update_flags
            contains
            subroutine update_flags(configData)
                use rt_param
                use cascade_globals
                implicit none

!     subroutine update_flags(iflexure,iceflag,time,writetime,flexon,&
!     			iceon,h,hi,nnode,writeice,ice_time,calc_ice,shorttime,&
!     			calc_rain)
!!! subroutine created by BJY 112709 to allow flags such as isostasy and ice to change mid model run

!     integer iflexure,iceflag,nnode
!     real time,writetime,flexon,iceon,h(nnode),hi(nnode)
!     real writeice,ice_time,calc_ice,calc_rain
                type(config) :: configData
                integer(4) :: i

          if ((time > configData%flexon) .and. (.not. configData%iflexure)) then
              configData%iflexure = .TRUE.
              print *,'Turning Flexure on'
              do i=1,configData%nnode
                  hi(i)=h(i)
              enddo
          endif


            if ((configData%iceflag) .and. (time > configData%iceon) .and. (.not. global_iceIsRunning)) then
!          if((time.ge.configData%iceon).and.(.not.configData%iceflag)) then

!!  this avoids trying ice when climate is too warm, make sure time scale is approapriate, etc

            global_iceIsRunning = .TRUE.
            configData%writetime=configData%writeice

            shorttime=0.0_8
            ice_time=dble(configData%calc_ice+1)
            configData%calc_rain=5000.0_8
            print *,'Turning Ice On, writetime: ',configData%writeice, ", time: ", time, ", configData%iceon: ", configData%iceon
          endif


          return
          end subroutine update_flags
        end module m_update_flags


