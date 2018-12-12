        module m_write_int
          contains
          subroutine write_int (nx,ny,h_tmp,tempm, &
                                ice_mass_balance,time,dt, &
                                t,sliding,dx,dy)

          implicit none

          integer(4) :: nx, ny
          real(8) :: time, dt

          real(8) :: h_tmp(nx,ny),tempm(nx,ny)
          real(8) :: ice_mass_balance(nx,ny)
          real(8) :: t(nx,ny),sliding(nx,ny),dx,dy

! printing intermediate results
!       dtemp=-100.
!       where (h_tmp>0.) dtemp=tempb-tempm
          print*,'>>>>>>>>>>>',' time=',time,' dt=',dt
!      print*,'Time needed to finish run: ',remaining_time/3600.,' hrs'
!      print*,'Temp0:',temp0,'degrees C'
          print*,'Ice thickness ',minval(h_tmp),maxval(h_tmp)
          print*,'Topography',minval(t),maxval(t)
          print*,'Cell size x y', dx, dy

! COMMENTED OUT BJY 100909-  I DON"T SEE THE POINT???.....

! 	geros=0.
! 	ic=0
! 	do j=1,ny2
! 	 do i=1,nx2
! 	  if (h2(i,j).gt.0.) then
! 	  geros=dtdtg2(i,j)+geros
! 	  ic=ic+1
! 	  endif
! 	 enddo
! 	enddo
! 	geros=geros/float(ic)/dt
!      print*,'Delta T ',minval(tempb-temps),maxval(tempb-temps)
          print*,'Melting temperature ',minval(tempm),maxval(tempm)
!      print*,'Peclet number ',minval(Pe),maxval(Pe)
          print*,'Mass Balance ',minval(ice_mass_balance),maxval(ice_mass_balance)
          print*,'Percentage sliding ', &
                 dble(count(logical(sliding > 0.0_8)))/dble(nx)/dble(ny)*100.0_8,'%'
          print*,'Min/Max sliding',minval(sliding),maxval(sliding)
          return
          end subroutine write_int
        end module m_write_int

