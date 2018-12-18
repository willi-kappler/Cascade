! This version of CASCADE is a fork from the original version published by Braun and Sambridge, 1997 Basin Research. The modifications made to the original version of CASCADE are described in Yanites and Ehlers, 2012, 2016 EPSL, and Eizenhoefer et al., 2019 JGR-ES.
! If you published results from this version of the program we would appreciate that you reference:
! 1. Publications listed by J. Braun in the cascade.f90 file.
! 2. Yanites, B.J., and Ehlers, T.A., 2012, Global climate and tectonic controls on the denudation of glaciated mountains, Earth and Planetary Science Letters, v. 325-326, pp. 63-75. doi.org/10.1016/j.epsl.2012.01.030.
! 3. Yanites, B.J., and Ehlers, T.A., 2016, Intermittent glacial sliding velocities explain variations in long-timescale denudation, SW British Columbia. Earth and Planetary Science Letters, 450, pp. 52-61. 
! 3. Eizenhoefer, P.R., McQuarrie, N., Shelef, E., and Ehlers, T.A., 2019 (in press), Fluvial responses to horizontal displacements in convergent orogens over geologic time scales, Journal of Geophysical Research - Earth Surface.

! Please direct all inquiries concerning this fork of the source code to Todd Ehlers (Univ. Tuebingen, Germany) tehlers@icloud.com


! cascade_globals.f90
! This module contains all global variables and constants used in icecascade

        module cascade_globals
            implicit none
            save

!
! nx and ny are the linear dimensions of the initial rectangular grid
! note that there is no reason for the grid to be rectangular
! this is done here because it is still the most popular way of
! designing numerical meshes

! nnodemax is the maximum number of nodes that the grid is allowed to grow
! into. If you decide not to use the idynamic=1 option you should chose
! nnodemax as close as possible to nnode

! nbmax is the maximum number of "natural neighbours" that any point can have
! for a general set of points nbmax=20 seems appropriate. However it is
! possible that for arbitrary sets of points, nbmax could be > 20. If it is
! the case, a warning message will be produced by cascade

! ntmax is the maximum nuber of triangles the circumcircles of which contain
! a common point

! Note that it will be assumed that the maximum number of triangles is
! three times the maximum number of nodes (this is very safe as the number
! of triangles is usually of the order of twice the number of nodes).

! nparam is the number of geomorphic parameters to be carried
! by the nodes

! nmemory is the number of working arrays carried by the nodes

! nflex is the discretization used to solve the flexural problem
! it must be a power of 2 

!  nxi is the number of gridded x nodes for ice
!  nyi is the number of gridded y nodes for ice be sure they match the .in file

!      parameter (nnodemax=205*79,nparam=3,nmemory=8,
!     &           nbmax=70,ntmax=30,nflex=256)

! nodemax boosted up to run higher resolution models on petrarch cluster
! dwhipp 04/07

            integer(4), parameter :: nnodemax = 501*301
            integer(4), parameter :: nparam = 3
            integer(4), parameter :: nmemory = 9
            integer(4), parameter :: nbmax = 600
            integer(4), parameter :: ntmax = 30
            integer(4), parameter :: nflex = 256
            integer(4), parameter :: nxi = 200
            integer(4), parameter :: nyi = 120

! gridsize is no longer used and will be deleted
!            integer(4), parameter :: gridsize = 1000

! global constants, TODO: add more constants, replace
! magic numbers in code with constants

            real(8), parameter :: GLOBAL_PI = 3.141592653589793239_8
            ! SEC_IN_YR was "secinyr"
            real(8), parameter :: SEC_IN_YR = 365.25_8 * 24.0_8 * 3600.0_8


! global variables:

            real(8) :: global_cascade_dt
            logical :: global_iceIsRunning

! x and y are the x- and y-coordinates of the nodes in km
! h is the current topography
! hi is the initial topography
! h0 is the location of the bedrock interface
! all h's are in m,iceth(nnodemax)
            real(8) :: ice_time,lastice_dh

            real(8), dimension (:), allocatable :: x
            real(8), dimension (:), allocatable :: y
            real(8), dimension (:), allocatable :: hi
            real(8), dimension (:), allocatable :: h
            real(8), dimension (:), allocatable :: h0
            real(8), dimension (:), allocatable :: isodh
            real(8), dimension (:), allocatable :: xd
            real(8), dimension (:), allocatable :: yd
            real(8), dimension (:), allocatable :: hd
            real(8), dimension (:), allocatable :: xl
            real(8), dimension (:), allocatable :: yl
            real(8), dimension (:), allocatable :: hl
            real(8), dimension (:), allocatable :: xu
            real(8), dimension (:), allocatable :: yu
            real(8), dimension (:), allocatable :: hu
            real(8), dimension (:), allocatable :: xr
            real(8), dimension (:), allocatable :: yr
            real(8), dimension (:), allocatable :: hr
            real(8), dimension (:), allocatable :: dhg
            real(8), dimension (:), allocatable :: hicerem
            real(8), dimension (:), allocatable :: lastice_h
            real(8), dimension (:), allocatable :: ldh
            real(8), dimension (:), allocatable :: gerode_term

!
! param are geomorphic parameters attached to each nodes
! there are nparam of them
!

! param(*,1)=fluvial erosion constant
! param(*,2)=bedrock erosion length scale
! param(*,3)=diffusion erosion constant

            real(8), dimension (:,:), allocatable :: param


! memory are variables that have to be stored from one step to the next
! for each node
! there are nmemory of them
!

! memory(*,1)=dhcrit
! memory(*,2)=dhfluvial
! memory(*,3)=dhdiff
! memory(*,4)=hiso
! memory(*,5)=fix
! memory(*,6)=newsurface
! memory(*,7)=surface
! memory(*,8)=dhlandslide (added by Ehlers 6/01)
! memory(*,9)=dhglacier (added by Yanites 10/09)

            real(8), dimension (:,:), allocatable :: memory


! work is a working array
!
            real(8), dimension (:), allocatable :: work

! water is the amount of water that drains down the landscape
! it is equivalent to the discharge
! sediment is the sediment load in the rivers

            real(8), dimension (:), allocatable :: water
            real(8), dimension (:), allocatable :: sediment
            real(8), dimension (:), allocatable :: orwater

! slope is the slope between a node and its donor neighbour
! note that slopes are in meter per kilometer as our horizontal
! length unit is a kilometer while the horizontal unit is the meter

            real(8), dimension (:), allocatable :: slope
            real(8), dimension (:), allocatable :: length

! ndon is the name of the donor neighbour node
! nn is the list of neighbours
!  nb is the number of neighbours for each node

            integer(4), dimension (:), allocatable :: ndon
            integer(4), dimension (:), allocatable :: nb
            integer(4), dimension (:), allocatable :: nb2
            integer(4), dimension (:,:), allocatable :: nn2
            integer(4), dimension (:,:), allocatable :: nn

! ibucket is a working array that is used in the
! "pass the bucket" algorithm on which the cascade method is based
! to define the river network (the ndon array)

            integer(4), dimension (:), allocatable :: ibucket

! the following arrays are also used in the cascade algorithm

            integer(4), dimension (:), allocatable :: iorder
            integer(4), dimension (:), allocatable :: itype_node

! nwork is a working array used in determing the catchment to which each
! node belongs; that is stored in the ncat array which has the name
! of the exiting node of the catchment (that the way catchments are named)

            integer(4), dimension (:), allocatable :: nwork
            integer(4), dimension (:), allocatable :: ncat

! nsill and nempty are used in the algorithm that looks for
! sill nodes in case of local minima
! nlake is a flag that determined whether a node belong to a
! lake or not

            integer(4), dimension (:), allocatable :: nsill
            integer(4), dimension (:), allocatable :: nempty
            integer(4), dimension (:), allocatable :: nlake

! influx is the flux of material into the landscape
! brought in the system by the tectonic uplift
! outflux is the flux out of the system, ie through the nodes
! where fix=0.

            real(8) :: influx,outflux

!
! initialize precip. array - added by DW 10/06
! TAE 7/01
! NOTE: if you unccomment the following line the program will execute, but
! if start to pass prec to other subroutines you will seg fault.  I'm not
! sure why at this point? TAE
            real(8), dimension (:), allocatable :: prec
            real(8), dimension (:), allocatable :: y_gr
            real(8), dimension (:), allocatable :: x_gr
            integer(4) :: nxs,nys
            real(8), dimension (:,:), allocatable :: prec_gr
            real(8), dimension (:,:), allocatable :: z

! the following arrays are needed in the natural neighbour routines
! have a look inside the library routienes to figure out what
! their purpose is

            real(8), dimension (:,:), allocatable :: points
            real(8) :: eps
            integer(4), dimension (:,:), allocatable :: vertices
            integer(4), dimension (:,:), allocatable :: neighbour
            integer(4), dimension (:), allocatable :: nodes
            integer(4), dimension (:), allocatable :: vis_tlist
            integer(4), dimension (:), allocatable :: vis_elist
            integer(4), dimension (:), allocatable :: add_tlist
            integer(4), dimension (:), allocatable :: nodelist
            integer(4), dimension (:), allocatable :: tlist
            logical, dimension (:), allocatable :: c_list
            integer(4), dimension (:,:), allocatable :: v_local
            integer(4), dimension (:,:), allocatable :: n_local
            logical, dimension (:), allocatable :: mask
            logical, dimension (:,:), allocatable :: mask_e
            logical, dimension (:), allocatable :: inactive

! the following arrays are used to calculate the surface of
! voronoi cells

! WK: needs to be changed to 8 bytes later
! WK: c code currently also uses float instead of double
            real(8) :: xy(2),pp(2,nbmax),aa(nbmax,2),bb(nbmax)

! the following arrays are used to solve the diffusion equation iteratively

            real(8) :: hp(nnodemax)
            integer(4) :: nkcon(nnodemax)
            real(8) :: ael1(6,nnodemax*3),ael2(6,nnodemax*3)
            real(8) :: bel(nnodemax),diag(nnodemax)
            integer(4), dimension (:,:), allocatable :: kcon
            integer(4), dimension (:,:), allocatable :: jcon

! itadd and jtadd are used when dynamic remeshing is turned
! on they are used to determine where resolution has to be increased

            integer(4) :: itadd(nnodemax),jtadd(nnodemax*3)

! the following arrays are used in the flexural isostasy
! calculations
! nflex is the resolution at which the FFT are done to calculate the
! flexural response; nflex has to be a power of 2

            real(8), dimension (:,:), allocatable :: flex
            real(8), dimension (:,:), allocatable :: work_flex


! Variables needed for landsliding routine landslide.f
! The following lines were added by Ehlers 6/01
            real(8) :: smax(nnodemax),tt(nnodemax)
            integer(4), dimension (:,:,:), allocatable :: cell


! landslide time series variables DS 6/15/1
!  these store info between write_output calls
!  might be problems if there are more than nnodemax slides
!  during a period

! dslope:  downstream slope found after all erosion DS 11/18/1
            real(8) :: dslope(nnodemax)

            integer(4) :: bdry(nnodemax)

! timeint: needed for naming tecplot output files
            integer(4) :: timeint

! ice grids and arrays
            real(8), dimension (:,:), allocatable :: zpt
            real(8), dimension (:,:), allocatable :: bipt
            real(8), dimension (:,:), allocatable :: bface
            real(8), dimension (:,:), allocatable :: hforice
            real(8), dimension (:,:), allocatable :: tbipt
            real(8), dimension (:,:), allocatable :: htoc
            real(8), dimension (:,:), allocatable :: httoc
            real(8), dimension (:,:), allocatable :: constoc
            real(8), dimension (:,:), allocatable :: slidetoc
            real(8), dimension (:,:), allocatable :: iceftoc

            integer(4), dimension(:,:), allocatable :: antitoc
            integer(4), dimension(:), allocatable :: anthi
            integer(4), dimension(nnodemax) :: glacier

            real(8) :: iceth(nnodemax),tott(nnodemax),slide(nnodemax)
            real(8) :: gbalance(nnodemax),strict(nnodemax)
            real(8) :: delx, dely
            real(8) :: shelf(nnodemax),sort(nnodemax),th(nnodemax)

            real(8)  totalerosion(nnodemax)

            integer(4) :: nt, dye, dxe, nexd, neyd, ned, nel, ner, neu, nexl, nexr, nextra, nexu
            integer(4) :: neyl, neyr, neyu, remeshflag = 0

            integer(4) :: tsys0, tsysT, cascade_ioStatus, system_Result

            integer(4) :: iseed, ice_topotime, cascade_istep, itime, nnode0, nxice, nyice
            logical :: ivocal, cascade_FileExists

            real(8) :: dhmaxglac, dtold, hmax, hmedian, hmin, oro_time, shorttime, meshtime, tcheck, temperature
            real(8) :: time, delta, dhmaxfluv, dhminfluv, dhminglac, GLOBAL_C, GLOBAL_CS
            real(8) :: diffusivity, dtdzb, dhmaxdiff, dhmindiff, dtmin, dtmax

        end module cascade_globals
