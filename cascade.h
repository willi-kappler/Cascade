! cascade.h
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

!      parameter (nnodemax=501*301,nparam=3,nmemory=9, &
!                 nbmax=50,ntmax=30,nflex=256, &
!                 nxi=200,nyi=120)

      integer nnode, nnodemax, nparam, nmemory
      integer nbmax, ntmax, nflex
      integer nxi, nyi, gridsize

      parameter (nnodemax=501*301,nparam=3,nmemory=9, &
                 nbmax=50,ntmax=30,nflex=256, &
                 nxi=200,nyi=120,gridsize=1000)

! x and y are the x- and y-coordinates of the nodes in km
! h is the current topography
! hi is the initial topography
! h0 is the location of the bedrock interface
! all h's are in m,iceth(nnodemax)

!      real(8)       x(nnodemax),y(nnodemax),hi(nnodemax)
!      real(8)       h(nnodemax),h0(nnodemax),isodh(nnodemax)
!      real(8)       xd(nnodemax),yd(nnodemax),hd(nnodemax)
!      real(8)       xl(nnodemax),yl(nnodemax),hl(nnodemax)
!      real(8)       xu(nnodemax),yu(nnodemax),hu(nnodemax)
!      real(8)       xr(nnodemax),yr(nnodemax),hr(nnodemax)
!      real(8)       dhg(nnodemax),hicerem(nnodemax)
!      real(8)       lastice_h(nnodemax),ldh(nnodemax)
      real(8)           ice_time,lastice_dh
!      real(8)        gerode_term(nnodemax)

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
!      real(8)       param(nnodemax,nparam)
! use dynamic arrays, WK
      real(8), dimension (:,:), allocatable :: param

!
! param(*,1)=fluvial erosion constant
! param(*,2)=bedrock erosion length scale
! param(*,3)=diffusion erosion constant
!
! memory are variables that have to be stored from one step to the next
! for each node
! there are nmemory of them
!
!      real(8)       memory(nnodemax,nmemory)
! use dynamic arrays, WK
      real(8), dimension (:,:), allocatable :: memory

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


! work is a working array
!
!      real(8)       work(nnodemax)
      real(8), dimension (:), allocatable :: work

! water is the amount of water that drains down the landscape
! it is equivalent to the discharge
! sediment is the sediment load in the rivers

!      real(8)       water(nnodemax),sediment(nnodemax)
      real(8), dimension (:), allocatable :: water
      real(8), dimension (:), allocatable :: sediment
!      real(8)       orwater(nnodemax)
      real(8), dimension (:), allocatable :: orwater

! slope is the slope between a node and its donor neighbour
! note that slopes are in meter per kilometer as our horizontal
! length unit is a kilometer while the horizontal unit is the meter

!      real(8)       slope(nnodemax),length(nnodemax)
      real(8), dimension (:), allocatable :: slope
      real(8), dimension (:), allocatable :: length

! ndon is the name of the donor neighbour node
! nn is the list of neighbours
!  nb is the number of neighbours for each node

!      integer    ndon(nnodemax)
!      integer    nb(nnodemax)
!      integer    nb2(nnodemax)
      integer, dimension (:), allocatable :: ndon
      integer, dimension (:), allocatable :: nb
      integer, dimension (:), allocatable :: nb2

!      integer    nn(nbmax,nnodemax)
!      integer    nn2(nbmax,nnodemax)
! use dynamic arrays, WK

      integer, dimension (:,:), allocatable :: nn2
      integer, dimension (:,:), allocatable :: nn

! ibucket is a working array that is used in the
! "pass the bucket" algorithm on which the cascade method is based
! to define the river network (the ndon array)

!      integer    ibucket(nnodemax)
      integer, dimension (:), allocatable :: ibucket

! the following arrays are also used in the cascade algorithm

!      integer    iorder(nnodemax)
!      integer    itype_node(nnodemax)
      integer, dimension (:), allocatable :: iorder
      integer, dimension (:), allocatable :: itype_node

! nwork is a working array used in determing the catchment to which each
! node belongs; that is stored in the ncat array which has the name
! of the exiting node of the catchment (that the way catchments are named)

!      integer    nwork(nnodemax),ncat(nnodemax)
      integer, dimension (:), allocatable :: nwork
      integer, dimension (:), allocatable :: ncat

! nsill and nempty are used in the algorithm that looks for
! sill nodes in case of local minima
! nlake is a flag that determined whether a node belong to a
! lake or not

!      integer    nsill(nnodemax),nempty(nnodemax),nlake(nnodemax)
      integer, dimension (:), allocatable :: nsill
      integer, dimension (:), allocatable :: nempty
      integer, dimension (:), allocatable :: nlake

! influx is the flux of material into the landscape
! brought in the system by the tectonic uplift
! outflux is the flux out of the system, ie through the nodes
! where fix=0.

      real(8)       influx,outflux

! rain_vel is the precipitation rate when considering a uniform rainfall
! (non orographic).

       real(8)      rain_vel

!
! initialize precip. array - added by DW 10/06
! TAE 7/01
! NOTE: if you unccomment the following line the program will execute, but
! if start to pass prec to other subroutines you will seg fault.  I'm not
! sure why at this point? TAE
!      real(8)  prec(nnodemax)
!      real(8)  x_gr(nnodemax)
!      real(8)  y_gr(nnodemax)
      real(8), dimension (:), allocatable :: prec
      real(8), dimension (:), allocatable :: y_gr
      real(8), dimension (:), allocatable :: x_gr
      integer nxs,nys

!      real(8)  prec_gr(nnodemax,nnodemax)
!      real(8)  z(nnodemax,nnodemax)
! use dynamic arrays, WK
      real(8), dimension (:,:), allocatable :: prec_gr
      real(8), dimension (:,:), allocatable :: z

! the following arrays are needed in the natural neighbour routines
! have a look inside the library routienes to figure out what
! their purpose is

!      real(8)*8     points(2,nnodemax)
      real(8)*8, dimension (:,:), allocatable :: points
      real(8)*8     eps
!      integer    vertices(3,nnodemax*3)
      integer, dimension (:,:), allocatable :: vertices
!      integer    neighbour(3,nnodemax*3)
      integer, dimension (:,:), allocatable :: neighbour
!      integer    nodes(nnodemax)
      integer, dimension (:), allocatable :: nodes
!      integer    vis_tlist(nnodemax)
      integer, dimension (:), allocatable :: vis_tlist
!      integer    vis_elist(nnodemax)
      integer, dimension (:), allocatable :: vis_elist
!      integer    add_tlist(nnodemax)
      integer, dimension (:), allocatable :: add_tlist
!      integer    nodelist(nbmax)
      integer, dimension (:), allocatable :: nodelist
!      integer    tlist(nbmax)
      integer, dimension (:), allocatable :: tlist
!      logical    c_list(2*nbmax)
      logical, dimension (:), allocatable :: c_list
!      integer    v_local(3,2*nbmax)
      integer, dimension (:,:), allocatable :: v_local
!      integer    n_local(3,2*nbmax)
      integer, dimension (:,:), allocatable :: n_local
!      logical    mask(nnodemax*3),mask_e(3,nnodemax*3)
      logical, dimension (:), allocatable :: mask
      logical, dimension (:,:), allocatable :: mask_e
!      logical    inactive(nnodemax)
      logical, dimension (:), allocatable :: inactive

! the following arrays are used to calculate the surface of
! voronoi cells

      real(8)       xy(2),pp(2,nbmax),aa(nbmax,2),bb(nbmax)

! the following arrays are used to solve the diffusion equation iteratively

      real(8)       hp(nnodemax)
      integer    nkcon(nnodemax)
      real(8)       ael1(6,nnodemax*3),ael2(6,nnodemax*3)
      real(8)       bel(nnodemax),diag(nnodemax)
!      integer    kcon(ntmax,nnodemax),jcon(ntmax,nnodemax)
! use dynamic arrays, WK
      integer, dimension (:,:), allocatable :: kcon
      integer, dimension (:,:), allocatable :: jcon

! itadd and jtadd are used when dynamic remeshing is turned
! on they are used to determine where resolution has to be increased

      integer    itadd(nnodemax),jtadd(nnodemax*3)

! the following arrays are used in the flexural isostasy
! calculations
! nflex is the resolution at which the FFT are done to calculate the
! flexural response; nflex has to be a power of 2

!      real(8)       flex(nflex,nflex),work_flex(nflex,nflex)
! use dynamic arrays, WK
      real(8), dimension (:,:), allocatable :: flex
      real(8), dimension (:,:), allocatable :: work_flex


      character  run_name*256
! Variables needed for landsliding routine landslide.f
! The following lines were added by Ehlers 6/01
      real(8)        smax(nnodemax),tt(nnodemax)
!   integer  cell(nnodemax,nbmax,2)
! use dynamic arrays, WK
      integer, dimension (:,:,:), allocatable :: cell


! landslide time series variables DS 6/15/1
!  these store info between write_output calls
!  might be problems if there are more than nnodemax slides
!  during a period
!      integer    snum
!      real(8)       stime(nnodemax),sx(nnodemax),sy(nnodemax)
!      real(8)       svol(nnodemax),sarea(nnodemax)

! dslope:  downstream slope found after all erosion DS 11/18/1
       real(8)       dslope(nnodemax)

       integer    bdry(nnodemax)

! timeint: needed for naming tecplot output files
       integer    timeint

! ice grids and arrays
!       real(8) zpt(nnodemax,nnodemax),htoc(nnodemax,nnodemax)
!       real(8) httoc(nnodemax,nnodemax),slidetoc(nnodemax,nnodemax)
!       real(8) iceftoc(nnodemax,nnodemax),t(nxi,nyi),a(nxi,nyi),t(nxi,nyi),tface(nxi,nyi)
!        real(8) zpt(nxi,nyi),bipt(nxi,nyi)
! use dynamic arrays, WK
       real(8), dimension (:,:), allocatable :: zpt
       real(8), dimension (:,:), allocatable :: bipt

!        real(8) tzpt(nxi,nyi),tbipt(nxi,nyi)
       real(8) kmelt,xlapse_rate,At
!       real(8) tface(nxi,nyi),bface(nxi,nyi),hforice(nxi,nyi)
! use dynamic arrays, WK
      real(8), dimension (:,:), allocatable :: tface
      real(8), dimension (:,:), allocatable :: bface
      real(8), dimension (:,:), allocatable :: hforice
      real(8), dimension (:,:), allocatable :: tzpt
      real(8), dimension (:,:), allocatable :: tbipt


!       real(8) htoc(nxi,nyi),httoc(nxi,nyi),constoc(nxi,nyi)
! use dynamic arrays, WK
      real(8), dimension (:,:), allocatable :: htoc
      real(8), dimension (:,:), allocatable :: httoc
      real(8), dimension (:,:), allocatable :: constoc

!       real(8) slidetoc(nxi,nyi),iceftoc(nxi,nyi)
! use dynamic arrays, WK
      real(8), dimension (:,:), allocatable :: slidetoc
      real(8), dimension (:,:), allocatable :: iceftoc

!       integer antitoc(nxi,nyi),anthi(nnodemax)
! use dynamic arrays, WK
integer, dimension(:,:), allocatable :: antitoc
integer, dimension(:), allocatable :: anthi

       real(8) iceth(nnodemax),tott(nnodemax),slide(nnodemax)
       real(8) glacier(nnodemax),gbalance(nnodemax),strict(nnodemax)
       real(8) delx, dely
        real(8) shelf(nnodemax),sort(nnodemax),th(nnodemax)
!       integer pnx,pny




        real(8)  totalerosion(nnodemax)
