! change_sea_level
        module m_change_sea_level
          contains
          subroutine change_sea_level (configData)
                use rt_param
                use cascade_globals
                implicit none

                type(config) :: configData

                configData%sea_level = 0.0_8

                return
            end subroutine change_sea_level
        end module m_change_sea_level


