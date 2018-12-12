        module m_total_topography
            contains
            subroutine total_topography (nx,ny,h_tmp,t,ht)

          integer(4) :: nx, ny
          real(8) :: h_tmp(nx,ny),t(nx,ny),ht(nx,ny) ! [km]
          integer(4) :: i,j

! calculating total topography

          do i=1,nx
            do j=1,ny
                ht(i,j)=t(i,j)
                if (h_tmp(i,j) > 0.0_8) then
                    ht(i,j) = h_tmp(i,j) + t(i,j)
                endif
            enddo
          enddo

          return
          end subroutine total_topography
      end module m_total_topography

