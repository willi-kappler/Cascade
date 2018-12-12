!      subroutine terosion(nnode,dhfluv,dhdiff,dhland,dhglac,totalerosion)

        module m_terosion
          contains
          subroutine terosion()
                use rt_param
                use cascade_globals
                implicit none

!       integer nnode
!       real dhfluv(nnode),dhdiff(nnode),dhland(nnode)
!       real dhglac(nnode),totalerosion(nnode)
! dhfluv = memory(1, 2)
! dhdiff = memory(1, 3)
! dhland = memory(1, 8)
! dhglac = dhg



           totalerosion=totalerosion + memory(1:,2) + memory(1:,3) + dhg - memory(1:,8)
!       print*,maxval(totalerosion),maxval(dhfluv),maxval(dhdiff),maxval(dhland),maxval(dhglac)
!       print*,minval(totalerosion),minval(dhfluv),minval(dhdiff),minval(dhland),minval(dhglac)
!


           return

           end subroutine terosion
        end module m_terosion

