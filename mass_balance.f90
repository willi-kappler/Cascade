    module m_mass_balance
        contains
            subroutine mass_balance(configData)
                    use rt_param
                    use cascade_globals
                    implicit none
!      subroutine mass_balance(nnode,th,At,T0,sea_level,xlapse_rate,&
!                  prec,kmelt,gbalance)
!  Subroutine created by BJY 101109 to calculate the mass balance on a glaciel landscape given rainmaker's orographic precip output.  Takes fraction of days that temp is below zero to calculate precipitation (note for now, we are ignoring rain that refreezes, future versions should have a factor to account for this).  Melt is calculated using a positive degree day equation (see Kessler et al., 2006 and Braithwaite, 1995 for further explanation.

!input
! lapse     -'lapse' rate of temperature with elevation
!  At       -amplitude of temperature shifts throughout year.
!  temp0min      -tempeature at sea-level
!  sea_level    - elevation of temp0min
! kmelt     -constant for positive degree day melting function

!output
!   gbalance: the mass balance per year for each Cascade node.  To be interpreted to a grid for use in Ice

!working
!      real mb_Ts(nnode),mb_Ttg0(nnode),mb_wz(nnode),mb_Tgw(nnode),At
!      real xlapse_rate,sea_level,th(nnode),T0,temperature
!      integer mb_Dg0(nnode),mb_Wd0(nnode),i,nnode
!      real prec(nnode),gbalance(nnode),mb_win(nnode),mb_wout(nnode)
!      real kmelt, mb_btemp(nnode)
!      real, parameter :: pi=3.141592

!   T0=temperature
            type(config) :: configData
            real(8), dimension(:), allocatable :: mb_Ttg0, mb_Ts, mb_Tgw, mb_win
            real(8), dimension(:), allocatable :: mb_wout, mb_btemp
            integer(4), dimension(:), allocatable :: mb_Wd0, mb_Dg0
            real(8) :: dtday, Tau, time0, t
            integer(4) :: i, j

            allocate(mb_Ttg0(configData%nnode))
            allocate(mb_Ts(configData%nnode))
            allocate(mb_Tgw(configData%nnode))
            allocate(mb_win(configData%nnode))
            allocate(mb_wout(configData%nnode))
            allocate(mb_btemp(configData%nnode))
            allocate(mb_Wd0(configData%nnode))
            allocate(mb_Dg0(configData%nnode))

!    print *, "mass_balance: step 0"

    Tau=365.25_8
    dtday=Tau / 365.0_8
    time0=0.0_8

    do i=1,configData%nnode
        mb_Ttg0(i)=0.0_8
        mb_Dg0(i)=0
        mb_Tgw(i)=0.0_8
        mb_Wd0(i)=0
        mb_Ts(i)=0.0_8
    enddo

!    print *, "mass_balance: step 1"

! step through each day and calculate surface temperature (may be a more formal, analytical way to do this)
    do i=1,365
          t=dtday*dble(i)
    !    print *, "mass_balance: step 2, i = ", i
          do j=1,configData%nnode
            ! xlapse_rate is in C / m
            ! sea_level is in km
            ! tott is in m
            mb_Ts(j)=temperature + (configData%At*sin(2.0_8*GLOBAL_PI*((t-time0)/Tau))) + &
                           (-configData%xlapse_rate *(tott(j)-(configData%sea_level * 1000.0_8)))
          enddo

    !    print *, "mass_balance: step 3"

    !     get array where value is mb_Ts if temperature is greater than zero and where it is zero otherwise.  uses dim function
    !     rewrote loop to avoid dim function
          do j=1,configData%nnode
            if (mb_Ts(j) > 0.0_8) then
                mb_Tgw(j) = mb_Ts(j)
            else
                mb_Tgw(j) = 0.0_8
            endif
    !        mb_Tgw(j)=dim(mb_Ts(j),mb_wz(j))
          enddo
    !    print *, "mass_balance: step 4"
    !     add to total temperature above zero for each node
          do j=1,configData%nnode
            mb_Ttg0(j)=mb_Ttg0(j)+mb_Tgw(j)
          enddo
    !    print *, "mass_balance: step 5"
    !     find where mb_Tgw is >0 and add the number of days
          do j=1,configData%nnode
            if (mb_Tgw(j) > 0.0_8) then
                mb_Wd0(j) = 1
            else
                mb_Wd0(j) = 0
            end if
          end do
    !        tempa0=mb_Tgw.gt.0
    !        mb_Wd0=tempa0
    !    print *, "mass_balance: step 6"
          do j=1,configData%nnode
            mb_Dg0(j)=mb_Dg0(j)+mb_Wd0(j)
          enddo
    !    print *, "mass_balance: step 7"

    enddo
! calculate fraction of precip that goes into ice.  May be best to just use all precip since we need to account for refreezing.  Look into this  
    do j=1,configData%nnode
        mb_win(j)=prec(j)*(365.0_8-dble(mb_Dg0(j)))/365.0_8
    enddo
!    print *, "mass_balance: step 8"

!uncomment following line if you remove fractional >0 days.  This will remind you what is going on in terms of mass balance assumptions
!    print*,'All precip is snow'
    
! calculate melt with positive degree day calculation
    do j=1,configData%nnode
        mb_wout(j)=configData%kmelt*mb_Ttg0(j) ! mb_wout is in meter
    enddo
!    print *, "mass_balance: step 9"
!mass balance is difference between the two

    do j=1,configData%nnode
        mb_btemp(j)=mb_win(j)-mb_wout(j)
    enddo
!    print *, "mass_balance: step 10"
    do j=1,configData%nnode
        gbalance(j)=mb_btemp(j)
    enddo
!    print *, "mass_balance: step 11"

!    call flush()

!    print *, "mass_balance: finished"

    deallocate(mb_Ttg0)
    deallocate(mb_Ts)
    deallocate(mb_Tgw)
    deallocate(mb_win)
    deallocate(mb_wout)
    deallocate(mb_btemp)
    deallocate(mb_Wd0)
    deallocate(mb_Dg0)

    return
    end subroutine mass_balance
    end module m_mass_balance

