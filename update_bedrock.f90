! update_bedrock
        module m_update_bedrock
          contains
          subroutine update_bedrock (configData)
                use rt_param
                use cascade_globals
                implicit none

! subroutine to update bedrock position in case of bedrock incision

! INPUT: h       = present topography
!        h0      = bedrock-alluvial interface
!        nnode   = number of nodes

! OUTPUT: h0      = updated bedrock-alluvial interface

! subroutines called:
! NONE

                type(config) :: configData
                integer(4) :: i

            do i=1,configData%nnode
                if (h(i).lt.h0(i)) h0(i)=h(i)+1.e-12_8
            enddo

          return
          end subroutine update_bedrock
        end module m_update_bedrock

