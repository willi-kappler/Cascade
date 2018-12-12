    module m_mb_ice
        contains
        subroutine mb_ice(nx, ny, ht, T0, mass_balance, configData)
!    subroutine mb_ice(nxice,nyice,ht,prec_gr,At,xlapse_rate,sea_level,T0,a,kmelt)
!  Subroutine created by BJY 101109 to calculate the mass balance on a glaciel landscape given rainmaker's orographic precip output.  Takes fraction of days that temp is below zero to calculate precipitation (note for now, we are ignoring rain that refreezes, future versions should have a factor to account for this).  Melt is calculated using a positive degree day equation (see Kessler et al., 2006 and Braithwaite, 1995 for further explanation.

!input
! lapse     -'lapse' rate of temperature with elevation
!  At       -amplitude of temperature shifts throughout year.
!  temp0min      -tempeature at sea-level
!  sea_level    - elevation of temp0min
! kmelt     -constant for positive degree day melting function

!output
!   gbalance: the mass balance per year for each Cascade node.  To be interpreted to a grid for use in Ice

! working
        use rt_param
        use cascade_globals
        implicit none

        type(config) :: configData

        integer(4) :: nx, ny
        real(8), dimension(nx,ny) :: mass_balance, ht
        real(8) :: T0

        real(8), dimension(:,:), allocatable :: mbi_Ts,mbi_Ttg0,mbi_Tgw
        real(8), dimension(:,:), allocatable :: mbi_win,mbi_wout
        integer, dimension(:,:), allocatable :: mbi_Wd0,mbi_Dg0
        logical, dimension(:,:), allocatable :: mbi_tempa0
        real(8), dimension(:,:), allocatable :: mbi_btemp

        real(8) :: dtday, tau, tq
        integer(4) :: i, j, k
        real(8) :: mbi_temp1, mbi_temp2, mbi_temp3, time0

        allocate(mbi_Ts(nxice,nyice))
        allocate(mbi_Ttg0(nxice,nyice))
        allocate(mbi_Tgw(nxice,nyice))
        allocate(mbi_win(nxice,nyice))
        allocate(mbi_wout(nxice,nyice))
        allocate(mbi_Wd0(nxice,nyice))
        allocate(mbi_Dg0(nxice,nyice))
        allocate(mbi_tempa0(nxice,nyice))
        allocate(mbi_btemp(nxice,nyice))


!       print*,'mb_ice',sea_level,xlapse_rate,kmelt

    dtday=365.25_8/365.0_8
    Tau=365.25_8
    time0=0.0_8
    mbi_Ttg0=0.0_8
    mbi_Dg0=0
    mbi_Ts=0.0_8
    mbi_Tgw=0.0_8
    mbi_win=0.0_8
    mbi_wout=0.0_8
    mbi_Wd0=0
    mbi_tempa0=.FALSE.
    mbi_btemp=0.0_8

    print *, "mb_ice.f90, temperature (T0): ", T0

! WK: use configData%At
!    At=7.5

! step through each day and calculate surface temperature (may be a more formal, analytical way to do this)
    do i=1,365
          tq=dtday*dble(i)
          do j=1,nxice
            do k=1,nyice
                mbi_temp1 = (configData%At*sin(2.0_8*GLOBAL_PI*((tq-time0)/Tau)))
                ! xlapse_rate is in C / m
                ! sea_level is in km
                ! ht is in km
                mbi_temp2 = (-configData%xlapse_rate*(ht(j,k)-(configData%sea_level*1000.0_8)))
                mbi_temp3 = mbi_temp1 + mbi_temp2
                mbi_Ts(j,k)=T0 + mbi_temp3
            enddo
          enddo
      

    !     get array where value is mbi_Ts if temperature is greater than zero and where it is zero otherwise.  uses dim function
          do j=1,nxice
            do k=1,nyice
                if (mbi_Ts(j,k) > 0.0_8) then
                    mbi_Tgw(j, k) = mbi_Ts(j,k)
                else
                    mbi_Tgw(j, k) = 0.0_8
                endif

    !            mbi_Tgw(j,k)=dim(mbi_Ts(j,k),mbi_wz(j,k))
            enddo
          enddo

    !     add to total temperature above zero for each node
          do j=1,nxice
            do k=1,nyice
                mbi_Ttg0(j,k)=mbi_Ttg0(j,k)+mbi_Tgw(j,k)
            enddo
          enddo

    !      print*,mbi_Ttg0
    !     find where mbi_Tgw is >0 and add the number of days
            do j=1,nxice
                do k=1,nyice
                    if (mbi_Tgw(j,k) > 0.0_8) then
                        mbi_Wd0(j,k) = 1
                    else
                        mbi_Wd0(j,k) = 0
                    end if
                enddo
            enddo
    !      mbi_tempa0=mbi_Tgw.gt.0
    !      mbi_Wd0=mbi_tempa0

          do j=1,nxice
            do k=1,nyice
                mbi_Dg0(j,k)=mbi_Dg0(j,k)+mbi_Wd0(j,k)   
            enddo
         enddo
    enddo

    print *, "mbi_Tgw: ", minval(mbi_Tgw), maxval(mbi_Tgw)

    print *, "mbi_Ttg0: ", minval(mbi_Ttg0), maxval(mbi_Ttg0)

    print *, "mbi_Dg0: ", minval(mbi_Dg0), maxval(mbi_Dg0)

! calculate fraction of precip that goes into ice.  May be best to just use all precip since we need to account for refreezing.  Look into this. For now keeping as if only ppt on days <0 go into ice.

    do j=1,nxice
        do k=1,nyice
            mbi_win(j,k)=prec_gr(j,k)*(365.0_8-dble(mbi_Dg0(j,k)))/365.0_8
        enddo
    enddo

    print *, "mbi_win: ", minval(mbi_win), maxval(mbi_win)

!uncomment follombi_wing line if you remove fractional >0 days.  This will remind you what is going on in terms of mass balance assumptions
!    print*,'All precip is snow'
    
! calculate melt with positive degree day calculation
    do j=1,nxice
        do k=1,nyice
            mbi_wout(j,k)=configData%kmelt*mbi_Ttg0(j,k)
        enddo
    enddo

    print *, "mbi_wout: ", minval(mbi_wout), maxval(mbi_wout)

!mass balance is difference between the two

    do j=1,nxice
        do k=1,nyice
            mass_balance(j,k)=mbi_win(j,k)-mbi_wout(j,k)
        enddo
    enddo
!   print*,maxval(a),minval(a)
    if (configData%use_max_accu) then
        where(mass_balance > configData%xpmax) mass_balance=configData%xpmax
    end if

        deallocate(mbi_Ts)
        deallocate(mbi_Ttg0)
        deallocate(mbi_Tgw)
        deallocate(mbi_win)
        deallocate(mbi_wout)
        deallocate(mbi_Wd0)
        deallocate(mbi_Dg0)
        deallocate(mbi_tempa0)
        deallocate(mbi_btemp)

        return
        end subroutine mb_ice
    end module m_mb_ice

