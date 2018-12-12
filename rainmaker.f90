!-----------------------------------------------------------------
    module m_rainmaker
        contains

      subroutine rainmaker(configData)
            use rt_param
            use cascade_globals

            implicit none
!      subroutine rainmaker(nnode,x,y,h,water,surface, &
!                prec,iflag_oro,iflag_uni,T0,sidex,sidey, &
!                nxs,nys,del_gr,z,prec_gr,hforice)
!-----------------------------------------------------------------
!
! NOTE SOME USER DEFINED INPUT IS REQUIRED IN THE TOP 64 LINES
! OF THIS SUBROUTINE 
! This routine was written by Gerard Roe and implemented by Todd
! Todd Ehlers, 9/2001, Univ. Washington, Quaternary Research Center
!
            type(config) :: configData

!      integer nnode
!      integer iflag_oro
!      integer iflag_uni
!      integer i,j

! domain sizes [km]
!      real sidex, sidey
!  Change on 110209 by BJY so that sidex and sidey are read in with other variables.  Required also moving defination of nxs and nys out of rainmaker and into cascade (see the few lines above the rainmaker call)
!      parameter(sidex = 400.)
!      parameter(sidey = 240.)

! grid size [km] 
!      real del_gr
!      parameter(del_gr = 2)

! regular grid dimension
!      integer nxs, nys
!      parameter (nxs = sidex/del_gr + 1)
!      parameter (nys = sidey/del_gr + 1)

! Precipitation Parameters
!     T0 = climate temperature (default is 0C)
!   -changed from default 10 to 0 on 102009, BJY-- may have to redo Ellie's SS topography with this temperature to avoid major orographic effects
!     a0 [m/yr] &  a1 [m/yr per (m/s)]
!     alf =  reciprocal vertical velocity variance [1/(m/s)].
!     wnd = wind speed [m/s], 
!     angle = clockwise angle rel. to y=0 line [degrees]
!      real T0,a0,a1,alf,wnd,angle
!      parameter(T0 = 4.0)
! Note no need to change a0 or a1.
!      change a0 is same as changing To (approx)
!      changing a1 is approx the same as changing wnd
!      parameter(a0=0.3, a1=110.0)
!      parameter(alf = 1./0.01)
!      parameter(wnd=.6)
!      parameter(angle = 90.)

! smoothing scales [km].
! xwind_s = cross wind smoothing scale
! upwnd_s = upwind smoothing scale
! if less than grid scale, NO smoothing is applied in that direction
!      real xwnd_s,upwnd_s
!      parameter(xwnd_s = 50.0, upwnd_s = 50.0)

! subsampling to speed up precip-- changed to 2 on 102309 bjy to speed up rainmaker
!      integer subsam
!      parameter(subsam=2)

!      real x_gr(nxs), y_gr(nys), prec_gr(nxs,nys)
!      real prec(nnode),water(nnode),surface(nnode)
!      real z(nxs,nys),hforice(nxs,nys)
!      real x(nnode), y(nnode),h(nnode)

! Save prec_gr so it can be used when iflag_oro == 0
!      save prec_gr

!      nxs = sidex/del_gr + 1
!      nys = sidey/del_gr + 1

      call oro_prec(configData)
!      call oro_prec(nnode,x,y,h,surface,prec,water, &
!          prec_gr,x_gr,y_gr, del_gr, &
!          xwnd_s,upwnd_s,subsam,T0, &
!          a0,a1,alf,wnd,angle, &
!          iflag_oro,iflag_uni,nxs,nys,z,hforice)
!       print*,maxval(prec_gr),minval(prec_gr)
!       z=z-hforice
          end subroutine

!*****************************************************************
! NO USER DEFINED INPUT NEEDED BELOW HERE
!*****************************************************************
!-----------------------------------------------------------------
      subroutine oro_prec(configData)
            use rt_param
            use cascade_globals
            use m_check_var

            implicit none
!      subroutine oro_prec(nnode,x,y,h,surface,prec,water, &
!          prec_gr,x_gr,y_gr, del_gr, &
!          xwnd_s,upwnd_s,subsam,T0, &
!          a0,a1,alf,wnd, angle, &
!          iflag_oro,iflag_uni,nxs,nys,z,hforice)
!-----------------------------------------------------------------
! code to produce precip param that works for CASCADE...
!     inputs: nnode             no. of nodes
!              x,y,h             x,y, and h of nodes
!              surface           surface area assoc. with each node
!              del_gr            grid scale for prec. calc.
!              nxs,nys           no xs and ys 
!              xwnd_s,upwnd_s    cross winds and upwind smooth. scales
!              subsam            node subsampling interval to 
!                                speed up interpolation 
!              T0,a0,a1,alf         precip parameters.
!              wnd,angle         wnd strength (m/s) and direction.
!                                angle = clockwise angle 
!                                from x=0 line.
!              iflag_oro         if = 1, recalc. precip.
!                                if = 0, just interpolate onto nodes.
!              iflag_uni         if = 1, uniform precip.
!                                if = 0, full calculation.
! outputs:
!              prec              precipitation rate at each node
!              water             precip *area at each node
!              x_gr,y_gr,prec_gr x,y,precip on reg. grid


! grid size [km]
!      real del_gr
!      real xwnd_s,upwnd_s

! subsampling to speed up precip
!      integer subsam

!      integer nnode
!      integer iflag_oro, iflag_uni

! Precip parameters
! wind speed [m/s]
!      real T0,a0,a1,alf
!      real wnd,angle

!      real x(nnode), y(nnode),h(nnode)
!      real prec(nnode),water(nnode),surface(nnode)

! declare variables & define parameters
!      integer i,j,k
!      real dum

! regular grid dimension
!      integer nxs, nys

! field variables
!      real x_gr(nxs), y_gr(nys), prec_gr(nxs,nys)
!      real z(nxs,nys), divQ(nxs,nys),hforice(nxs,nys)
!      real dzdx(nxs,nys), dzdy(nxs,nys),zint(nxs,nys)

!      real bckgr

! begin routine.

            type(config) :: configData
            integer(4) :: i, j
            real(8) :: bckgr
            real(8), dimension(:,:), allocatable :: divq
            real(8), dimension(:,:), allocatable :: dzdx
            real(8), dimension(:,:), allocatable :: dzdy
            real(8), dimension(:,:), allocatable :: zint

!            real(8) :: z_value

        allocate(divq(nxs,nys))
        allocate(dzdx(nxs,nys))
        allocate(dzdy(nxs,nys))
        allocate(zint(nxs,nys))

! WK: init memory:

        do i=1,nxs
            do j=1,nys
                divq(i,j) = 0.0_8
                dzdx(i,j) = 0.0_8
                dzdy(i,j) = 0.0_8
                zint(i,j) = 0.0_8
            enddo
        enddo

! get background precip rate for h=w=gradh=0
! but including prob. dist. of vert velo. given by alf param.
         call back_div(temperature,configData%a0,configData%a1, &
              configData%alf,bckgr)

! if uniform precip requested, set it equal to the 
! background rate and (sorry) go to bottom of routine.

!      print *,'Number of nodes according to rainmaker.f',nnode

      if (configData%iflag_uni) then
         do i = 1,configData%nnode
!            print *,'i = ',i
!            print *,'nnode = ',nnode
!            prec(i) = 1.
            prec(i) = bckgr
!            call check_var(2, "rainmaker, prec(i), 1$", prec(i), i)
!            print *,'prec = ',prec(i)
         enddo
         goto 100
      endif


! if update of precip on regular grid required
      if (configData%iflag_oro) then

! define regular grid.
      do i  = 1,nxs
         x_gr(i) = dble((i-1)*configData%del_gr)
      enddo
      do j  = 1,nys
         y_gr(j) = dble((j-1)*configData%del_gr)
      enddo

! Interpolate from nodal points to regular grid.
! uses bivar.f in this case.
! WK: values from h are going to be interpolated to z
      !call check_array_2d_real("rainmaker, z, 2$", z, nxs, nys)
      print *, "rainmaker.f90, h 1: ", minval(h), maxval(h)
      print *, "rainmaker.f90, z 1: ", minval(z), maxval(z)
!      call grid(configData%nnode,x,y,h,x_gr,y_gr,nxs,nys,configData%subsam,z)
      call grid2(configData%nnode,x,y,h,x_gr,y_gr,nxs,nys,z)
      print *, "rainmaker.f90, z 2: ", minval(z), maxval(z)
      zint=z
!      print *, "rainmaker.f90, z 3: ", minval(z), maxval(z)
      z=z+hforice
!      print *, "rainmaker.f90, z 4: ", minval(z), maxval(z)

!      open(50, file="z_out.dat")
!      do i=1,nys
!        do j=1,nxs
!            z_value = z(j,i)
!            if (z_value /= z_value) then
!                write (50, "(F16.8) ", advance='no') 0.0
!            else
!                write (50, "(F16.8) ", advance='no') z_value
!            endif
!        enddo
!        write(50, "(a)") ""
!      enddo
!      close(50)


      !call check_array_2d_real("rainmaker, hforice, 1$", hforice, nxs, nys)
      !call check_array_2d_real("rainmaker, z, 1$", z, nxs, nys)
! output topography
!      open(unit=21, file = 'bivar.dat', status = 'new')
!      do i = 1,nxs
!         write(21,1000) (z(i,j),j=1,nys)
!      enddo
!      close(21)

! calculate slopes perpendicular to prevailing wind direction.
! normal to front in this case.
! note have to shift horiz. units to m
!      call xderiv(dAdx,A,delx,imx,jmx)
      call xderiv(dzdx,z,1000*configData%del_gr,nxs,nys)
      call yderiv(dzdy,z,1000*configData%del_gr,nxs,nys)

! output moist. conv.
!      open(unit=21, file = 'grady.dat', status = 'new')
!      do i = 1,nxs
!         write(21,1000) (dzdy(i,j),j=1,nys)
!      enddo
!      close(21)
!      open(unit=21, file = 'gradx.dat', status = 'new')
!      do i = 1,nxs
!         write(21,1000) (dzdx(i,j),j=1,nys)
!      enddo
!      close(21)

! calculate moisture convergence.
! use standard routine form my thesis.
      call moist_div(z,dzdy,dzdx,nxs,nys,configData%wnd, &
            configData%angle,temperature,configData%a0,configData%a1, &
            configData%alf,divq)

! output moist conv.
!      open(unit=21, file = 'divq.dat', status = 'new')
!      do i = 1,nxs
!         write(21,1000) (divq(i,j),j=1,nys)
!      enddo
!      close(21)

! average divQ over defined smoothing scales.
! gaussian upwind weighting applied in this case.
      call smooth(divq,nxs,nys,configData%xwnd_s, &
                  configData%upwnd_s, configData%del_gr, &
                  configData%angle,bckgr,prec_gr)
! end conditional
      endif

! do linear interpolation back onto nodes
      call linear(configData%nnode,configData%del_gr)

! multiply precip rate by nodal area to get rate of water input.
 100   continue
       do i =1,configData%nnode
         water(i) = memory(i, 7)*prec(i)
!          call check_var(2, "rainmaker, water(i), 1$", water(i), i)
!         water(i) = surface(i)*1.0
      enddo
        z=zint

! 1000 format(500(E15.6))
! er, stop, end
!        deallocate(divq)
!        deallocate(dzdx)
!        deallocate(dzdy)
!        deallocate(zint)

      return 
      end subroutine

      subroutine grid2(nnode,x,y,h,x_gr,y_gr,nxs,nys,z)
      implicit none
!     inputs: 
!             nnode     number of nodes 
!             x,y        co-ords of nodes 
!             h          elevation of nodes 
!             x_gr, y_gr co-ords of reg grid 
!             nxs, nys   number of values in x and y direction
!             subsam   subsampling of input nodes.
!
!      outputs: 
!             z       elevation on regular grid 
!

        integer(4), intent(in) :: nnode, nxs, nys
        real(8), intent(in) :: x(nnode), y(nnode), h(nnode), x_gr(nxs), y_gr(nys)
        real(8), intent(out) :: z(nxs, nys)

        integer(4) :: i, j, k, n1, n2, n3, n4
        real(8) :: radius, d, d1, d2, d3, d4
        real(8) :: factor1, factor2, factor3, factor4, factor_sum

        ! consider a radius of 10 km
        radius = 10.0_8
        n1 = 1
        n2 = 1
        n3 = 1
        n4 = 1

        print *, "x min: ", minval(x), ", x max: ", maxval(x), ", y min: ", minval(y), ", y max: ", maxval(y)
        print *, "x_gr min: ", minval(x_gr), ", x_gr max: ", maxval(x_gr)
        print *, "y_gr min: ", minval(y_gr), ", y_gr max: ", maxval(y_gr)

        do i=1,nxs
            do j=1,nys

                ! now look for the 4 nearest neighbors
                d1 = huge(d1)
                d2 = huge(d2)
                d3 = huge(d3)
                d4 = huge(d4)

                do k=1,nnode
                    d = sqrt((x_gr(i) - x(k))**2 + (y_gr(j) - y(k))**2)

                    if (d < d1) then
                        d2 = d1
                        d3 = d2
                        d4 = d3
                        d1 = d

                        n2 = n1
                        n3 = n2
                        n4 = n3
                        n1 = k
                    else if (d < d2) then
                        d3 = d2
                        d4 = d3
                        d2 = d

                        n3 = n2
                        n4 = n3
                        n2 = k
                    else if (d < d3) then
                        d4 = d3
                        d3 = d

                        n4 = n3
                        n3 = k
                    else if (d < d4) then
                        d4 = d

                        n4 = k
                    endif
                enddo

                factor1 = exp(-d1/radius)
                factor2 = exp(-d2/radius)
                factor3 = exp(-d3/radius)
                factor4 = exp(-d4/radius)

                factor_sum = factor1 + factor2 + factor3 + factor4

                z(i, j) = ((h(n1)*factor1) + (h(n2)*factor2) + (h(n3)*factor3) + (h(n4)*factor4)) / factor_sum

!                print *, "grid2:"
!                print *, "factor1: ", factor1, ", factor2: ", factor2, ", factor3: ", factor3, ", factor4: ", factor4
!                print *, "factor_sum: ", factor_sum
!                print *, "z(i,j): ", z(i,j), ", n1: ", n1, ", n2: ", n2, ", n3: ", n3, ", n4: ", n4
!                print *, "h(n1): ", h(n1), ", h(n2): ", h(n2), ", h(n3): ", h(n3), ", h(n4): ", h(n4)
!                print *, "x(n1): ", x(n1), ", x(n2): ", x(n2), ", x(n3): ", x(n3), ", x(n4): ", x(n4)
!                print *, "y(n1): ", y(n1), ", y(n2): ", y(n2), ", y(n3): ", y(n3), ", y(n4): ", y(n4)
!                print *, "d1: ", d1, ", d2: ", d2, ", d3: ", d3, ", d4: ", d4
!                print *, "i: ", i, ", j: ", j, ", x_gr(i): ", x_gr(i), ", y_gr(j): ", y_gr(j)

!                stop

            enddo
        enddo

      end subroutine grid2


!----------------------------------------------------------------
      subroutine moist_div(z,dzdy,dzdx,nxs,nys,wnd,angle,Tbck,a0,a1, &
          alf,divq)
!----------------------------------------------------------------
! code to calculate moisture divergence from the w and T fields passed 
! calculated in the main program.
        use m_check_var
      implicit none
! inputs:  h, dhdy/dhdx   height, gradient in x and y directions
!                            (calc. on reg. grid)
!          nxs,nys       number of grid points.
!          Tbck          background temperature 
!          a0,a1         background and slope term, respectively.
!          wnd           prescribed prevailing wind speed.
!          angle         prescribed wind direction
!          alf           prescribed variance in atmospheric vert. velocity.
!
! outputs: divq          moisture divergence at each point.
!
! other:   a,b,e0        parameters in Clasius-Clapeyron relation.
!          p,a1,a2,a3    parameters in error function approximation.
!          gamma         atmospheric lapse rate. 

! Other variables etc.
      integer i,j,nxs,nys
      real(8) :: divq(nxs,nys),w,T,Tbck,wnd, angle
      real(8) :: z(nxs,nys), dzdy(nxs,nys), dzdx(nxs,nys)

! Precipitation parameters taken from my thesis.
      real(8) :: a0,a1
      real(8) :: a,b,e0,satfac
      real(8) :: p,c1,c2,c3

! Other stuff for precipitation.
      real(8) :: alf,alf_p
      real(8) :: x_0
      real(8) :: erf,t0
      real(8) :: I1,I2

      real(8) :: pi
      parameter(pi=3.1415927_8) ! FIX_PI

! lapse rate
      real(8) :: gamma
      parameter(gamma = -6.5E-03_8)
! Sat. vap. pres. params...  
! and error function parameters for esat distribution.
      parameter(a=17.67_8,b=243.5_8,e0=611.2_8)
      parameter(p=0.47047_8,c1=0.3480242_8,c2=-0.0958798_8,c3=0.7478556_8)

! loop over grid
      do i = 1,nxs
         do j = 1,nys

! Probability distribution of velocities around the time-mean
! upslope velocity integrated over time to give an effective upslope
! velocity. See my thesis...

! transform for precip.
           w = wnd*sin(3.14159_8*angle/180.0_8)*dzdy(i,j) &
             - wnd*cos(3.14159_8*angle/180.0_8)*dzdx(i,j)
!           call check_var(1, "rainmaker, wnd, 1$", wnd, i)
!           call check_var(1, "rainmaker, angle, 1$", angle, i)
!           call check_var(1, "rainmaker, dzdy(i,j), 1$", dzdy(i,j), i)
!           call check_var(1, "rainmaker, dzdx(i,j), 1$", dzdx(i,j), i)
!           call check_var(1, "rainmaker, w, 2$", w, i)
           alf_p = alf/a1
           x_0   = a1*w + a0
!           call check_var(1, "rainmaker, a1, 1$", a1, i)
!           call check_var(1, "rainmaker, a0, 1$", a0, i)
!           call check_var(1, "rainmaker, w, 1$", w, i)
!           call check_var(1, "rainmaker, x_0, 2$", x_0, i)

! analytic expression for precip. integral.
           t0=1.0_8/(1.0_8+p*alf_p*abs(x_0))
           erf=1.0_8-(c1*t0+c2*t0**2.0_8+c3*t0**3)*exp(-alf_p**2*x_0**2)
           I1=x_0/2.0_8*(1.0_8+sign(1.0_8,x_0)*erf)
!           call check_var(1, "rainmaker, erf, 1$", erf, i)
!           call check_var(1, "rainmaker, x_0, 1$", x_0, i)
!           call check_var(1, "rainmaker, I1, 2$", I1, i, x_0, erf)
           I2=1.0_8/(2.0_8*alf_p*sqrt(pi))*exp(-alf_p**2*x_0**2)
!           write(6,*) alf_p,erf,x_0,I1,I2
          
! divq=esat*(I1+I2)
! change temperatue by changing 0.0 here.
           T = Tbck+gamma*z(i,j)
           if (T < -100.0_8) then
                print *, "rainmaker: T=", T
                print *, "z(i,j)=", z(i,j), ", i, j=", i, j
                print *, "Tbck=", Tbck
!                call check_var(1, "rainmaker, Tbck, 1$", Tbck, i)
           endif
! sat. vap. factor
           satfac=exp(a*T/(b+T))
           divq(i,j)=satfac*(I1+I2)
!           call check_var(1, "rainmaker, I1, 1$", I1, i)
!           call check_var(1, "rainmaker, I2, 1$", I2, i)
!           call check_var(1, "rainmaker, Tbck, 1$", Tbck, i)
!           call check_var(1, "rainmaker, a, 1$", a, i)
!           call check_var(1, "rainmaker, b, 1$", b, i)
!           call check_var(1, "rainmaker, T, 1$", T, i)
!           call check_var(1, "rainmaker, divq(i,j), 2$", divq(i,j), i)
        enddo
      enddo
! er, return, end
      return
      end subroutine

!-----------------------------------------
      subroutine back_div(Tbck,a0,a1,alf,bckgr)
!-----------------------------------------
! code to calculate background moisture divergence (ok, convergence) 
! at z=0 in absence of surface slopes. Uses same prob. distribution of
! vertical velocities as divq
       implicit none
!          a0,a1         background and slope term, respectively.
!          alf           prescribed variance in atmospheric vert. velocity.
!          Tbck             background temperature.
!
! outputs: bckgr          moisture divergence at each point.
!
! other:   a,b,e0        parameters in Clasius-Clapeyron relation.
!          p,c1,c2,c3    parameters in error function approximation.
!          gamma         atmospheric lapse rate. 

! Other variables etc.
      real(8) :: bckgr,Tbck

! Precipitation parameters taken from my thesis.
      real(8) :: a0,a1
      real(8) :: a,b,e0,satfac
      real(8) :: p,c1,c2,c3

! Other stuff for precipitation.
      real(8) :: alf,alf_p
      real(8) :: x_0
      real(8) :: erf,t0
      real(8) :: I1,I2

      real(8) :: pi
      parameter(pi=3.1415927_8) ! FIX_PI

! lapse rate.
      real(8) :: gamma
      parameter(gamma = -6.5E-03_8)

! Sat. vap. pres. params...  
! and error function parameters for esat distribution.
      parameter(a=17.67_8,b=243.5_8,e0=611.2_8)
      parameter(p=0.47047_8,c1=0.3480242_8,c2=-0.0958798_8,c3=0.7478556_8)

! transform for precip.
      alf_p = alf/a1
      x_0   = a0

! analytic expression for precip. integral.
      t0=1.0_8/(1.0_8+p*alf_p*abs(x_0))
      erf=1.0_8-(c1*t0+c2*t0**2.0_8+c3*t0**3)*exp(-alf_p**2*x_0**2)
      I1=x_0/2.0_8*(1.0_8+sign(1.0_8,x_0)*erf)
      I2=1.0_8/(2.0_8*alf_p*sqrt(pi))*exp(-alf_p**2*x_0**2)
! divq=esat*(I1+I2)

! sat. vap. factor
      satfac=exp((a*Tbck)/(b+Tbck))
      bckgr=satfac*(I1+I2)
! er, return, end
      return
      end subroutine

!------------------------------------------------------
      subroutine smooth(divq,nxs,nys,xwnd_s,upwnd_s, &
          del_gr,angle,bckgr,prec_gr)
!------------------------------------------------------
        use m_check_var

      implicit none
! subroutine to smooth moisture divergence field to get
! precipitation field
!
!     inputs:
!             divq           moisture conv. on reg. grid
!             x_gr,y_gr            reg. grid
!             nxs,nys        kinda obvious
!             xwnd_s, upwnd_s assumed smoothing scales across 
!                            and along the prevailing wind directions
!             del_gr         grid size
!             bckgr          background precip rate in [m/yr]
!             angle          prescribed angle of prevailing wind
!
!     outputs:
!             prec_gr        also kinda obvious

! others:
!             phi =          distance from gd. pt. perp. to 
!                            prevailing wind direction
!             lam =          distance from gd. pt. parallel. to 
!                            prevailing wind direction

! delare variables
      integer i,j,i1,j1, ixwnd_s, iupwnd_s
      integer nxs,nys
      integer iwk1,iwk2,jwk1,jwk2
      integer(4) :: del_gr
      real(8) :: xwnd_s, upwnd_s
      real(8) :: divq(nxs,nys),prec_gr(nxs,nys)
      real(8) :: wt1,wt2, sum_wt
      real(8) :: phi,lam
      real(8) :: bckgr,angle

      real(8) :: pi
      parameter(pi = 3.1415926_8)

! smoothing scales in terms of number grid points
      ixwnd_s = int(xwnd_s)/del_gr
      iupwnd_s = int(upwnd_s)/del_gr

! Determine boundaries of smoothing region.
! Ensure at least 2 smoothing widths upwind and
! 2 smoothing widths either side.
      jwk1 = abs(2*int(dble(ixwnd_s)*cos(angle*pi/180.0_8)  &
          + dble(iupwnd_s)*sin(angle*pi/180.0_8)))
      jwk2 = abs(2*int(dble(ixwnd_s)*cos(angle*pi/180.0_8)))
      if (angle.le.90.0_8) then
         iwk1 = abs(2*int(dble(ixwnd_s)*sin(angle*pi/180.0_8)))
         iwk2 = abs(2*int(dble(ixwnd_s)*sin(angle*pi/180.0_8)+ &
                     dble(iupwnd_s)*cos(angle*pi/180.0_8)))
      else
         iwk1 = abs(2*int(dble(ixwnd_s)*sin(angle*pi/180.0_8)- &
             dble(iupwnd_s)*cos(angle*pi/180.0_8)))
         iwk2 = abs(2*int(dble(ixwnd_s)*sin(angle*pi/180.0_8)))
      endif
!      write(6,*) iwk1,iwk2,jwk1,jwk2
    
! begin loop over variables.
      do i = 1,nxs
         do j = 1,nys
            prec_gr(i,j) = 0.0_8
            sum_wt = 0.0_8

            do j1 = j-jwk1,j+jwk2
               do i1 = i-iwk1, i+iwk2
! phi = cross wind distance
! lam  = upwind distance
                  phi=dble(i1-i)*sin(angle*pi/180.0_8) &
                      +dble(j1-j)*cos(angle*pi/180.0_8)
                  lam=-dble(i1-i)*cos(angle*pi/180.0_8)  &
                      +dble(j1-j)*sin(angle*pi/180.0_8)

! define weighting factors, allow for zero smoothing.
! if smoothing scale is less than xwnd or upwind then only
! count points within 1 grid pt of the wind direction.
                  if (ixwnd_s.eq.0) then
                     if(abs(phi).lt.1.0_8) then
                        wt1=1.0_8
                     else
                        wt1=0.0_8
                     endif
                  else
                     wt1 = exp(-1.0_8*(phi/dble(ixwnd_s))**2) 
                  endif

                  if (iupwnd_s.eq.0) then
                     if(abs(lam).lt.1.0_8) then
                        wt2=1.0_8
                     else
                        wt2=0.0_8
                     endif
                  else
                     wt2 = exp(-1.0_8*(lam/dble(iupwnd_s))**2) 
                  endif


                  if ((i1.ge.1).and.(i1.le.nxs)) then
                     if (j1.lt.1) then
                        prec_gr(i,j) = prec_gr(i,j) + bckgr*wt1*wt2
!                        call check_var(1, "rainmaker, prec_gr(i,j), 3$", prec_gr(i, j), i)
                        sum_wt = sum_wt+wt1*wt2
                     elseif (j1.le.nys) then
                        prec_gr(i,j) = prec_gr(i,j) +  &
                            divq(i1,j1)*wt1*wt2
!                        call check_var(1, "rainmaker, wt1, 1$", wt1, i)
!                        call check_var(1, "rainmaker, wt2, 1$", wt2, i)
!                        call check_var(1, "rainmaker, divq(i,j), 1$", divq(i1, j1), i)
!                        call check_var(1, "rainmaker, prec_gr(i,j), 4$", prec_gr(i, j), i)
                        sum_wt = sum_wt+wt1*wt2
                     endif  
                  endif
               enddo
            enddo

! finally divide by weighting factor.
            prec_gr(i,j) = prec_gr(i,j)/sum_wt
!            call check_var(1, "rainmaker, prec_gr(i,j), 5$", prec_gr(i, j), i)

         enddo
      enddo

!, er return, end.
      return
      end subroutine


!-----------------------------------------------------------------------
      subroutine linear(nnode,del_gr)
!      subroutine linear(nnode,prec_gr,x,y,nxs,nys,del_gr,prec)
!-----------------------------------------------------------------------
      use cascade_globals
      use m_check_var

      implicit none
! subroutine to do linear interpolation from regular grid back onto nodes
!     inputs: 
!            nnode             number of nodes
!            prec_gr           precipitation on regular grid
!            x,y               coords at nodes
!            del_gr            regular grid spacing
!     outputs:
!            prec              precipitation on nodes

      integer(4) :: i,nnode,i0,j0
      integer(4) :: del_gr,iwk,jwk,t,u

! begin loop over nodes
      do i = 1,nnode
         iwk = int(x(i))/del_gr + 1
         jwk = int(y(i))/del_gr + 1
         i0 = int(x(i))/del_gr + 1
         j0 = int(y(i))/del_gr + 1
         
         if (i0 < 1) then
            i0 = 1
         else if (i0 > (nxs - 1)) then
            i0 = nxs - 1
         endif

         if (j0 < 1) then
            j0 = 1
         else if (j0 > (nys - 1)) then
            j0 = nys - 1
         endif

        
! working numbers.
         t = (iwk-i0)
         u = (jwk-j0)
! linear interpolation.
         prec(i) = dble((1-t)*(1-u))*prec_gr(i0,j0) + &
                   dble(t*(1-u))*   prec_gr(i0+1,j0) + &
                   dble(t*u)*       prec_gr(i0+1,j0+1) + &
                   dble((1-t)*u)*   prec_gr(i0,j0+1)
!          call check_var(1, "rainmaker, prec_gr(i0,j0), 2$", prec_gr(i0,j0), i0)
!          call check_var(2, "rainmaker, prec(i), 2$", prec(i), i)
      enddo

! er, return, end
      return
      end subroutine

!------------------------------------------------------------
      subroutine xderiv(dAdx,A,delx,imx,jmx)
!-------------------------------------------------------------
      implicit none
! subroutine to take a real 2-d matrix and take a deriv in the 
! x direction or the first array address. 2nd order accurate.
      integer i,j,imx,jmx
      real(8) :: A(imx,jmx), dAdx(imx,jmx)
      integer(4) :: delx

      do j = 1,jmx
         dAdx(1,j) = (-3.0_8*A(1,j)+4.0_8*A(2,j)-A(3,j))/(2.0_8*dble(delx))
         do i = 2,imx-1
            dAdx(i,j) = (A(i+1,j)-A(i-1,j))/(2.0_8*dble(delx))
         enddo
         dAdx(imx,j) = (3.0_8*A(imx,j)-4.0_8*A(imx-1,j)+A(imx-2,j))/(2.0_8*dble(delx))
      enddo

      return
      end subroutine

!------------------------------------------------------------
      subroutine yderiv(dAdy,A,dely,imx,jmx)
!-------------------------------------------------------------
      use m_check_var
      implicit none
! subroutine to take a real 2-d matrix and take a deriv in the 
! y direction or the second array address. 2nd order accurate.
      integer i,j,imx,jmx
      real(8) :: A(imx,jmx), dAdy(imx,jmx)
      integer(4) :: dely

      !call check_var(1, "rainmaker, dely, 1$", dely,i)

      do i = 1,imx
         dAdy(i,1) = (-3.0_8*A(i,1)+4.0_8*A(i,2)-A(i,3))/(2.0_8*dble(dely))
!         call check_var(1, "rainmaker, dAdy(i,1), 1$", dAdy(i,1),i)
         do j = 2,jmx-1
            dAdy(i,j) = (A(i,j+1)-A(i,j-1))/(2.0_8*dble(dely))
!             call check_var(1, "rainmaker, A(i,j+1), 1$", A(i,j+1),i)
!             call check_var(1, "rainmaker, A(i,j-1), 2$", A(i,j-1),i)
             !call check_var(1, "rainmaker, dAdy(i,j), 2$", dAdy(i,j),i, A(i,j+1), A(i,j-1), dely)
         enddo
         dAdy(i,jmx) = (3.0_8*A(i,jmx)-4.0_8*A(i,jmx-1)+A(i,jmx-2))/(2.0_8*dble(dely))
!         call check_var(1, "rainmaker, dAdy(i,jmx), 3$", dAdy(i,jmx),i)
      enddo

      return
      end subroutine

    end module m_rainmaker

