        module m_surface_temperature
            contains
            subroutine surface_temperature (configData)
                use rt_param
                use cascade_globals
                implicit none

!      subroutine surface_temperature (temp0min,temp0max,xp,temperature,At,time,iceon)


!      real temps(nx,ny),tempb(nx,ny),ht(nx,ny),t(nx,ny)
!      real temp0min,temp0max,xp,temperature,time,At,iceon
!      real temp0mn,temp0mx

                type(config) :: configData
                real(8) :: xint
                integer(4) :: io_error
                integer(4) :: read_error
                integer(4) :: i
                integer(4) :: k
                real(8)         :: a
                real(8)         :: b
                real(8)         :: temp_scr
                real(8)         :: low_t
                integer(4)         :: inde
                real(8),dimension(30031)        :: age
                real(8),dimension(30031)        :: d_O_norm
                real(8),dimension(30031)        :: temp_array
                real(8),dimension(30031)        :: diff_array

!!!!!!!!!!!!!UNCOMMENT THE FOLLOWING TO HAVE A TIME DEPENDENCE FOR CLIMATE, I.E. CENOZOIC COOLING

!!  added 112609 BJY, Move parameter inputs at some point
!    temp0mx=(-time/(1.e6))+16
!   temp0mn=(-time/(1.e6))+10
!   temp0mx=(-0.5*time/(10.e6))+20
!   temp0mn=(-time/(10.e6))+15
!   xint=temp0mx-temp0mn

!       xp=100.e3
!          temperature=temp0mn+xint*(sin(2.*pi*time/xp-pi/2.)+1.)/2.

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        
!  annual swing in mean daily temperatures.  Irrelavent here, but is eventually passed into mass balance routine mb_ice.f90.  Move at some point!       
                if (.not.configData%temp_from_file) then
!                        print *, 'surface_temperature.f90: temperaturefile is false'

                        xint=configData%temp0max-configData%temp0min
!       xp=100.e3
                        temperature=configData%temp0min+xint*(sin(2.0_8*GLOBAL_PI*time/configData%xp-GLOBAL_PI/2.0_8)+1.0_8)/2.0_8

                else
!                        print *, 'surface_temperature.f90: temperaturefile is true'

                        !Calculate factors a and b for linear equation Temp = a*dO18 + b
                        low_t = configData%mean_t - configData%amp_temp
                        a = (low_t - configData%mean_t) / (configData%LGM_O18 - configData%IG_O18)
                        b = low_t - (a*configData%LGM_O18)
                        open(unit=20,file=configData%temp_file_name,status='old',action='read',iostat=io_error)
                        if (io_error /= 0) then
                                write(*,*)'There has been a problem with the Delta O18 Data'
                                stop
                        end if
                        do i = 1,30031
                                read(20,*,iostat=read_error) age(i),d_O_norm(i)
                                temp_scr = (a * d_O_norm(i)) + b
                                
                                !Create the .txt file.
                                !Doesn't work yet. Wrong loop. creates gigantic file with values.

                                !open(unit=7,file = "Temp_Data_d18.txt")
                                !write(7,*) age(i) , temp_scr
                                !create the temperature array
                                
                                temp_array(i) = temp_scr
                        end do
                        

                        !Brace yourself, Here comes the time-temperature correlation.
                        
                        forall(k=1:30031) diff_array(k) = abs(age(k) - time)
                        inde = minloc(diff_array,1)
                        temperature = temp_array(inde)
                end if
                                
                        
!                print *, "surface_temperature.f90, temperature: ", temperature
!      temp0=temp0min
!      print*
!      tempout=temp0
!
!      temps=temp0-xlapse_rate*ht !-0.7576*xlatitude
!      tempb=temp0-xlapse_rate*t  !-0.7576*xlatitude
!       elseif (time.lt.iceon) then
!           temperature=10
              return
          end subroutine surface_temperature
        end module m_surface_temperature

