! This version of CASCADE is a fork from the original version published by Braun and Sambridge, 1997 Basin Research. The modifications made to the original version of CASCADE are described in Yanites and Ehlers, 2012, 2016 EPSL, and Eizenhoefer et al., 2019 JGR-ES.
! If you published results from this version of the program we would appreciate that you reference:
! 1. Publications listed by J. Braun in the cascade.f90 file.
! 2. Yanites, B.J., and Ehlers, T.A., 2012, Global climate and tectonic controls on the denudation of glaciated mountains, Earth and Planetary Science Letters, v. 325-326, pp. 63-75. doi.org/10.1016/j.epsl.2012.01.030.
! 3. Yanites, B.J., and Ehlers, T.A., 2016, Intermittent glacial sliding velocities explain variations in long-timescale denudation, SW British Columbia. Earth and Planetary Science Letters, 450, pp. 52-61. 
! 3. Eizenhoefer, P.R., McQuarrie, N., Shelef, E., and Ehlers, T.A., 2019 (in press), Fluvial responses to horizontal displacements in convergent orogens over geologic time scales, Journal of Geophysical Research - Earth Surface.

! Please direct all inquiries concerning this fork of the source code to Todd Ehlers (Univ. Tuebingen, Germany) tehlers@icloud.com

! This module contains the type definition for the runtime parameter configuration
! it also contains the load_config subroutine, that loads the parameters form the
! configuration file

        module rt_param
            use cascade_globals
            implicit none
            save

            integer(4), private, parameter :: global_fileUnit = 10
            integer(4), private, parameter :: typeReal = 1
            integer(4), private, parameter :: typeInteger = 2
            integer(4), private, parameter :: typeLogical = 3

            integer(4), private :: global_lineNumber = 0
            integer(4), private :: global_ioStatus = 0

            character(255), private :: global_line
            character(255), private :: global_last_comment
            character(255), private, parameter :: global_fileName = "input/IceCascade/icecascade.in"

            type :: config
                ! parameters from initialize_general_parameters.f90

                ! MODEL PROCESSES
                ! INPUT A
                ! -------------------------------------------------------------

                ! Fluvial erosion
                !   INPUT A1
                logical :: fluvial_erosion

                ! Enable fluvial depostion
                !   INPUT A2
                logical :: ideposition

                ! Hillslope diffusion parameters
                ! Enable hillslope diffusion
                !   INPUT A3
                logical :: idiffusion

                ! Enable landslide erosion parameters
                !   INPUT A4
                logical :: ilandslide

                ! Flag to permanently turn on/off Ice. T = (true) on, F = (false) off
                ! If set to F it will never call ice/grow a glacier.
                !   INPUT A5
                logical :: iceflag

                ! Enable glacial erosion
                !   INPUT A6
                logical :: glacial_erosion

                ! flag to permit flexural isostasy (=0 no flexure)
                !   INPUT A7
                logical iflexure

                ! Allow horizontal advection of nodes and topography.
                ! Only required if vy and vx are non-zero.
                !   INPUT A8a
                logical :: ihorizontal
                
                ! Select mode for horizontal movement (added by PRE Jan2017)
                ! 1 = Global value applied throughout the model domain as defined in input 
                !      M2a and M2b
                ! 2 = Variable horizontal movement as given by a MOVE/Pecube velocity file
                integer(4) :: ihorizontal_mode 

                ! Enable orographic precipitation
                ! If iflag_oro = F, the iflag_uni is set to T (uniform precipitation)
                !   INPUT A9
                logical :: iflag_oro
                logical :: iflag_uni

                ! Higher-Order Ice Physics Switch (using COMSOL)
                ! .FALSE. = just use update_height from BY (modified SIA)
                ! .TRUE. = use update_height_comsol
                !   INPUT A10
                logical :: use_comsol

                ! MODEL OUTPUT
                ! INPUT B
                ! -------------------------------------------------------------

                ! .FALSE. = All output files written
                ! .TRUE. = Only the tecplot formatted output files and the
                !          geometry and topography files are written
                !   INPUT B1
                logical :: tecflag


                ! Frequency of screen output [time steps]
                !   INPUT B2
                integer(4) :: nshortwrite

                ! Frequency of flux outputs [time steps]
                ! (=0 no flux save otherwise frequency in time steps)
                ! this output is of total flux through the fixed nodes
                !   INPUT B3
                real(8) :: iflux ! not needed, remove

                ! Frequency of writing model output for pure cascade and when ice is called [y]
                ! When only a cascade simulation is run the writetime varialbe is what used.
                ! When a cascade and ice simulation are run then the writeice variable is used.
                ! If it is a mixed simulation (e.g. 2 Myr Cascade only, then ice), the writetime
                ! variable will be used until ice is called.
                !   INPUT B4
                integer(4) :: writetime
                integer(4) :: writeice

                ! name of this run; must also be the name
                ! of an existing folder where all output files will be stored.
                ! If the folder does not exits, it will be created
                !   INPUT B5
                character(256) :: run_name



                ! MODEL SET UP: TIME DOMAIN
                ! INPUT C
                ! -------------------------------------------------------------

                ! Enable dynamic time step calculation
                !   INPUT C1
                logical :: iadjust

                ! Initial time step length [y]
                !   INPUT C2
                real(8) :: dt0

                ! Total model run time [y]
                !   INPUT C3
                real(8) :: endtime



                ! MODEL SET UP: SPATIAL DOMAIN
                ! INPUT D
                ! -------------------------------------------------------------

                ! Number of nodes along x and y edges of model, note these will be changed if
                ! addrim=1 after addrim subroutine.  If readmesh is on, make sure these values
                ! still equal the tecplot file.  If creating SS fluvial, be sure to turn addrim off.
                !   INPUT D1
                integer(4) :: nx
                integer(4) :: ny

                ! Length of x and y sides of model [km]
                !   INPUT D2
                real(8) :: sidex
                real(8) :: sidey

                ! Enable adaptive remeshing
                !   INPUT D3
                logical :: iadapt

                ! Allow Cascade to read in mesh with dimensions the same as above
                !   INPUT D4
                logical :: meshread

                ! parameter from readmesh.f90
                ! Name of the initial topography to read in [file_name].
                ! This is only used if meshread = T.
                !   INPUT D5
                character(255) :: meshname

                ! The format of the meshfile
                ! 1 = tecplot formatted file: 
                !       File needs to include number of nodes, and then colums for x,y,h,h0 for each node.
                !       h and h0 are the surface and bedrock elevations (?),
                !       the difference between the two is the sediment thickness.
                ! 2 = ascii file:
                !       The file should contain nx by ny elevation points
                !       with the elevations of each point in [m].
                !       File should be 3 columns with row=nx*ny
                !       Column 1: x coordinate (m); column 2: y coordinate (m); column 3: elevation (m)
				            ! 3 = 2DMOVE ascii file export (added by PRE Jan2017)
				            !		     2DMOVE export in [x y z Point_ID] format, where [x y z] describe the location of nodes in MOVE and
				            !     		[Point_ID] the MOVE line identifier (not used in Cascade).
			            	!		     Only x and z will be used and y extrapolated to 3D space in Cascade.
				            ! 4 = 3DMOVE ascii file export (added by PRE Jan2017) - !NOT YET IMPLEMENTED!
				            !       3DMOVE export in [x y z Point_ID] format, where [x y z] describe the location of nodes in MOVE and
                !       [Point_ID] the MOVE line identifier (not used in Cascade).
                !   INPUT D6
                integer(4) :: meshformat

                ! Add 'shelf' to boundary of model domain to allow glaciers to flow out of valleys
                ! and not disappear into space, 0,0 is on lower left so Exl is extended rim left,
                ! Edy is extended rim down [km]
                ! The 'shelf' in reality could be a below modern sea level continental shelf
                ! or an above sea-level coastal plain or a piedmont.
                ! Exr = distance shelf will be created to the RIGHT of the domain y-axis specified above. (optional)
                ! Exl = distance shelf will be created to the LEFT of the domain y-axis specified above. (optional)
                ! Exd = distance shelf will be created to the DOWN/BELOW of the domain x-axis specified above. (optional)
                ! Exu = distance shelf will be created to the UP/ABOVE of the domain x-axis specified above. (optional)
                !   INPUT D7
                logical :: addshelf
                real(8) :: Exr
                real(8) :: Exl
                real(8) :: Eyd
                real(8) :: Eyu

                ! shelf/piedmon slopes: positivite values slope away from original topography
                !   INPUT D8
                real(8) :: slopexr
                real(8) :: slopexl
                real(8) :: slopeyd
                real(8) :: slopeyu

                ! number of extra nodes to add in x and y direction.
                ! These nodes will only be added to the 'shelf' area.
                ! Suggestion is to add both distances (e.g. Exl+Exr) from above and use an average
                ! node spacing of your liking.  This method is best used when reading a mesh.
                ! In that scenarios, you're plain can have different nodal spacings.
                ! If you are generating new topograpy, make sure nxe, nye will generate
                ! the spacing you prefer (i.e. what nx and ny give you in INPUT D1).
                ! Essentially if a new mesh is generated, spacing in the model domain as well as on the
                ! shelf will be the same, so make sure nxe and nye (in relation to Exl,Exr,Eyd,Eyl) will
                ! give the proper nodal spacing.  Initialize_nodal_geometry.f90 does not differentiate between
                ! shelf and regular nodes.  Read_nodal_geometry.f90 does differentiate, so a different
                ! mesh spacing is possible if reading in a mesh
                ! Note: you can run IceCascade for one time step and use that topography as an input mesh in order to
                ! use a different node spacing
                !   INPUT D9
                integer(4) :: nxe
                integer(4) :: nye


                ! EROSION: FLUVIAL
                ! INPUT E
                ! -------------------------------------------------------------

                ! Fluvial erosion constant [unitless]
                !   INPUT E1
                real(8) :: xkf

                ! Bedrock erosion length scale [m]
                !   INPUT E2
                real(8) :: xlf_BR

                ! Coefficient controling channel width as function of discharge [sqrt(yr/m)]
                !   INPUT E3
                real(8) :: width_c

                ! Discharge threshold for channel formation [m km2/yr]
                !   INPUT E4
                real(8) :: thresh

                ! Fluvial erosion length scale for alluvial material [m]
                ! NOTE (TAE) xlf_AL set to 100.0 in our old version
                !   INPUT E5
                real(8) :: xlf_AL

                ! Define sea level elevation; no erosion below sea level [km]
                !   INPUT E6
                real(8) :: sea_level



                ! EROSION: HILLSLOPES
                ! INPUT F
                ! -------------------------------------------------------------

                ! Diffusion constant [km2/yr]
                !   INPUT F1
                real(8) :: xkdiff

                ! Landslide method to be used
                ! added by DS 11/13/1
                ! 1: probabilistic method from pvdb
                ! 2: simple slope threshold
                !    pmax is the critical slope
                !    distmax is the maximum distance
                ! 3: simpler slope threshold in landslide_simple.f
                !    pmax is the critical slope DS 11/17/1
                ! Landslide method 1 and 2 use subroutine landslide.f
                ! Landslide method 3 uses subroutine landslide_simple.f
                !   INPUT F2
                integer(4) :: lsmeth

                ! Threshold hillslope angle for landsliding (used for lsmeth=1-3)
                ! For method 2 and 3, this value dictates if a landslide occurs or not (if slope is greater
                ! than this value, then a landslide occurs, if it's less, no landslide)
                ! For method 1, this value is the maximum possible slope, but landslides can occur on slopes
                ! less than this value.  As slope steepens, the probability increases that a landslide will occur.
                ! If the slope equals pmax, then the probability is 1.  Probability also increases as a function of time
                ! since last landslide (see xk0).
                !   INPUT F3
                real(8) :: pmax

                ! Maximum transport distance (used for lsmeth=1,2) [m]
                ! (changed by DS 6/15/1)
                !   INPUT F4
                real(8) :: distmax

                ! Parameters for lsmeth=1
                ! Hillslope effective cohesion
                ! I (Brian) think that a large value will give you hillslope angles that cluster near pmax
                ! whereas a small value will give you hillslope angles of a greater distribution.
                ! Used by landslide method: 1
                !   INPUT F5
                real(8) :: cohes

                ! Density of hillslope material [kg/m3]
                !   INPUT F6
                real(8) :: rho

                ! Acceleration due to gravity [m/s2]
                !   INPUT F7
                real(8) :: grav

                ! Landslide frequency scaling parameters for lsmethod 1, suggested (?) values
                ! xk0, a coefficient used along with time since last landslide for a given node to calculate
                ! probability of a landslide occurring at that node.  Can be thought of as a 'preparation' timescale
                ! i.e. if you have a landslide, it will take some time (related to xk0) before another landslide
                ! will occur. I (Brian) guess this means that if you set to 0, landslide probability will not depend
                ! on time since last landslide.
                ! xk1, is the number of landslides per timescale set by dtc (below) for the average deluany area per node.
                ! So if xk1=1 and dtc=100, then on average 1 landslide will occur every 100 yrs at a given node.
                ! dtc is a measure of landslide frequency used in the probabilistic calculations.  Used to divide
                ! both the current timestep length as well as the time since last landslide to calculate a
                ! landslide probability for a given node.
                ! xk0=0.01, xk1=1., dtc = 100. yrs
                ! Used by landslide method: 1
                !   INPUT F8
                real(8) :: xk0
                real(8) :: xk1
                real(8) :: dtc



                ! ICE TIME SETUP
                ! INPUT G
                ! -------------------------------------------------------------

                ! parameters from update_flags.f90
                ! After ice is turned on, this parameter controls how often rainmaker is called.
                ! The one set in initialize_general_parameters.f90 is used before ice is turned on.
                !   INPUT G1
                real(8) :: calc_rain_ice

                ! Shallow Ice Approximation time step (yr)
                ! was dt0 in initialize_ice.f90 (input 6 in ice.in), dtice0 in cascade.f
                !   INPUT G2
                real(8) :: shallow_ice

                ! Maximum dh/dt allowed for each timestep in the ice thickness.
                ! This prevents run-away glacier sizes.
                !   INPUT G3
                real(8) :: dh_allowed

                ! the new parameter, added 2011.05.20, Willi Kappler

                ! Erosion time step (in yrs)
                ! was dt in initialize_ice.f90 (input 6 in ice.in), icetime in cascade.f
                !   INPUT G4
                real(8) :: dt_icetime

                ! End of time stepping
                ! was tfinal in ICE.f90
                !   INPUT G5
                real(8) :: ice_tfinal



                ! ICE FLAGS
                ! INPUT H
                ! -------------------------------------------------------------

                ! Flag to determin if you have polythermal or isothermal ice.
                ! T = polythermal, calculate glacier geotherms.
                ! F = isothermal, glacier temperature = surface temperature.
                !   INPUT H1
                logical :: itemp




                ! ICE MECHANICS
                ! INPUT I
                ! -------------------------------------------------------------

                ! Ice-flow constant,(1/(sPa**3)), it is good to use Paterson's value but not necessary...
                ! ice_flow was b
                !   INPUT I1
                real(8) :: ice_flow

                ! Sliding law constant, [units depend on value of the exponent below, e.g. 1/shearstress**n]
                ! sliding_law was bs
                !   INPUT I2
                real(8) :: sliding_law

                ! Power law exponent,
                ! power_law_exp was n
                !   INPUT I3
                real(8) :: power_law_exp

                ! Exponent sliding law, (usually set between 2 and 3)
                !   INPUT I4
                ! exp_sliding_law was xns
                real(8) :: exp_sliding_law

                ! Density of ice (kg/m**3)
                ! was 'rho', now rho_ice
                !   INPUT I5
                real(8) :: rho_ice

                ! Constriction constant
                ! was 'gamma' now 'constriction'
                !   INPUT I6
                real(8) :: constriction




                ! ICE EROSIOIN
                ! INPUT J
                ! -------------------------------------------------------------

                ! Erosion rate constant for glacial erosion
                ! Sliding velocity is multiplied by this value. [unitless]
                ! was 'alpha' now 'erosion_rate'
                !   INPUT J1
                real(8) :: erosion_rate

                ! The power of the relationship between ice velocity and erosion
                !   INPUT J2
                real(8) :: ice_ero_pow




                ! ICE THERMAL FIELD
                ! INPUT K
                ! -------------------------------------------------------------

                ! Basal heat flux [Wm-2]
                ! basal_heat_flux was qb
                !   INPUT K1
                real(8) :: basal_heat_flux

                ! Ice conductivity [Wm-1k-1]
                !   INPUT K1
                real(8) :: conductivity




                ! MISCELLANEOUS ICE
                ! INPUT L
                ! -------------------------------------------------------------

                ! parameters from avalanche.f90
                ! The threshold snow angle [degrees].
                ! Any hillslope steeper than this angle will experience snow avalanching
                !   INPUT L1
                real(8) :: critangle

                ! parameters from calve.f90
                ! Coefficient to calculate the rate of calving velocity [1/yr].
                ! This value times the depth of ice below water gives the rate.
                !   INPUT L2
                real(8) :: calvecoef




                ! TECTONICS (cascade code)
                ! INPUT M
                ! -------------------------------------------------------------
                ! Uplift modified by PRE Jan2017
                ! Uplift mode:
                ! 1 = Global uplift rate:
                !    Uplift rate is constant over the entire model domain and run
                ! 2 = MOVE uplift based on MOVE velocity input; time-invariant:
                !    Uplift rate is variable over the model domain based on MOVE input but
                !    overall does not change with time
                ! 3 = MOVE uplift based on MOVE velocity input; time-variant:
                !    Uplift rate is variable in space and time based on a given set of 
                !    MOVE velocity input files NOT YET IMPLEMENTED!
                !    INPUT M1
                integer(4) :: uplift_mode
                
                ! Global uplift rate [mm/y], vertical rock uplift in z direction 
                !   INPUT M1a 
                real(8) :: uplift_rate
                
                ! MOVE velocity file name for time-invariant uplift            
                !   INPUT M1b
                character(255) :: MOVE_velocity_file

                ! MOVE time steps
                !   INPUT M1c
                integer(4) :: timesteps
                
                ! File with MOVE velocity file names and time line
!                character(255) :: MOVE_velocity_summary 
                
                ! Maximum advection velocity in x direction [mm/y ????]
                !   INPUT M2a
                real(8) :: advec_velx

                ! Maximum advection velocity in y direction [mm/y ????]
                !   INPUT M2b
                real(8) :: advec_vely

                ! size (in km) of the square mesh on which the thin elastic plate calculations are done
                !   INPUT M3
                real(8) :: hflex

                ! flag to permit flexure in the x-direction
                !   INPUT M4
                logical :: ixflex

                ! flag to permit flexure in the y-direction
                !   INPUT M5
                logical :: iyflex

                ! elastic thickness in km
                !   INPUT M6
                real(8) :: thickflex

                ! young modulus (in Pa)
                ! was 'ym', now 'young_mod'
                !   INPUT M7
                real(8) :: young_mod

                ! poisson's ratio
                !   INPUT M8
                real(8) :: pratio

                ! density of crustal rocks (in kg/m3)
                !   INPUT M9
                real(8) :: rhocflex

                ! asthenospheric density (in kg/m3)
                !   INPUT M10
                real(8) :: rhoaflex

                ! added BJY 112709 to remove flexure effects early on in landscape development.
                ! If you want flexure on the whole time, just set to 0
                !   INPUT M11
                real(8) :: flexon





                ! CLIMATE
                ! INPUT N
                ! -------------------------------------------------------------

                ! Time between updates of precipitation field in orographic precip model [yr]
                !   INPUT N1
                real(8) :: calc_rain

                ! Grid size of square grid cells used in orographic precipitation model to calculate
                ! precipitation on cascade topography [km]
                !   INPUT N2
                integer(4) :: del_gr

                ! Precipitation rate for a uniform rainfall model [m/yr]
                !   INPUT N3
                real(8) :: rain_vel

                ! how often do you want to update Ice if topography hasn't changed significantly.
                ! Best to make this a multiple of the rainmaker calls
                !   INPUT N4
                integer(4) :: calc_ice

                ! If you want to not call Ice for a certain time period (i.e. climate will
                ! be too warm anyway, or you want a step change into a glacial period, etc).
                ! For Ice to be turned on at begining, set to 0
                !   INPUT N5
                real(8) :: iceon

                ! a0 [m/yr] &  a1 [m/yr per (m/s)]
                !   INPUT N6
                real(8) :: a0
                real(8) :: a1

                ! alf =  reciprocal vertical velocity variance [1/(m/s)]
                !   INPUT N7
                real(8) :: alf

                ! wnd = wind speed [m/s]
                !   INPUT N8
                real(8) :: wnd

                ! angle = clockwise angle rel. to y=0 line [degrees]
                !   INPUT N9a
                real(8) :: angle

                ! y_flip = flip coordinates and advection along y-axis (y_min = y_max). Temporary fix that creates correct (?)
                !     precipitation patterns in the direction of convergence towards y = 0 (wind blowing towards y = y_max). As a
                !     result, advection will be towards y = y_max (instead of y = 0 as in MOVE)
                !   INPUT N9b
                logical :: y_flip

                ! xwind_s = cross wind smoothing scale
                !   INPUT N10
                real(8) :: xwnd_s

                ! upwnd_s = upwind smoothing scale
                !   INPUT N11
                real(8) :: upwnd_s

                ! subsampling to speed up precip-- changed to 2 on 102309 bjy to speed up rainmaker
                !   INPUT N12
                integer(4) :: subsam

                ! lapse rate, xlapse_rate (C/m)
                !   INPUT N13
                real(8) :: xlapse_rate

                ! parameters from mb_ice.f90
                ! At = amplitude of temperature shifts throughout year
                ! Annual variation in daily temperature.
                ! Used to calculate a positive degree day melting algorithm in glacial calculations
                !   INPUT N14
                real(8) :: At

                ! accummulution (m/yr)
                !   INPUT N15
                real(8) :: xpmax

                ! Flag to enable max out accumulation
                !   INPUT N16
                logical :: use_max_accu

                ! Positive degree day melting constant (kmelt) (m/C)
                !   INPUT N17
                real(8) :: kmelt

                ! Min and max sea level temperatures
                !   INPUT N18
                real(8) :: temp0min
                real(8) :: temp0max

                ! Period of surface oscillation (in yrs)
                !   INPUT N19
                real(8) :: xp


                !#******************************
                !#****Temperature Input
                !# INPUT O
                !#******************************

                !# INPUT O1 *** Use temperature data from file (T = yes , F = No)
                !# temperature_file
                logical :: temp_from_file

                !# INPUT O2 *** Filename for temperature data
                !# temp_file_name
                character(255) :: temp_file_name

                !# INPUT O3 *** Amplitude which is used for calculating the temperature swing (integer)
                !# amp_temp
                real(8) :: amp_temp

                !# INPUT O4 *** Normalised O18 Values for present and LGM
                !# IG_O18
                !# LGM_O18
                real(8) :: IG_O18
                real(8) :: LGM_O18

                !# INPUT O5 *** MAT at target latitude (Zuerich)
                !# mean_t
                real(8) :: mean_t

                ! number of nodes, will be calculated: nnode = nx * ny
                integer(4) :: nnode

                ! nrun_name = length of string 'run_name', will be calculated
                integer(4) :: nrun_name

                ! average nodel surface in km^2, will be calculated: surfscale = sidex * sidey / nnode
                real(8) :: surfscale

                ! minimum surface allowed (to prevent run away)
                ! will be calculated: surfscale / 4
                real(8) :: surfmin

            end type config

            contains
                subroutine readLine()
                    global_line(1:255) = ' '

                    do
                        global_lineNumber = global_lineNumber + 1
                        read(global_fileUnit, "(a)", iostat=global_ioStatus) global_line
                        if (global_ioStatus /= 0) then
                            print *, "readLine: error while reading file <" // trim(global_fileName) // ">"
                            print *, "lineNumber: ", global_lineNumber
                            print *, "line: ", trim(global_line)
                            print *, "last comment: ", trim(global_last_comment)
                            stop
                        end if

                        if (global_line(1:1) == '#') then ! ignore comments
                            global_last_comment = global_line
                            cycle
                        else if (global_line(1:1) == ' ') then ! ignore blank lines
                            cycle
                        else
                            exit
                        end if
                    end do
                end subroutine readLine

                subroutine checkData(typeOfData)
                    integer(4), intent(in) :: typeOfData
                    character(255) :: dataName

                    if (typeOfData == typeReal) then
                        dataName = "real"
                    else if (typeOfData == typeInteger) then
                        dataName = "integer"
                    else if (typeOfData == typeLogical) then
                        dataName = "logical"
                    else
                        dataName = "unknown"
                    end if

                    if (global_ioStatus /= 0) then
                        print *, "error while reading file <" // trim(global_fileName) // ">"
                        print *, "expecting <" // trim(dataName) // "> value"
                        print *, "lineNumber: ", global_lineNumber
                        print *, "line: <", trim(global_line), ">"
                        print *, "last comment: ", trim(global_last_comment)
                        stop
                    end if
                end subroutine checkData

                subroutine readReal(data)
                    real(8), intent(out) :: data

                    call readLine()
                    read(global_line, *, iostat=global_ioStatus) data
                    call checkData(typeReal)

!                    print *, "readReal: <", data, ">, line: ", global_lineNumber
                end subroutine readReal

                subroutine readInteger(data)
                    implicit none
                    integer(4), intent(out) :: data

                    call readLine()
                    read(global_line, *, iostat=global_ioStatus) data
                    call checkData(typeInteger)

!                    print *, "readInteger: <", data, ">, line: ", global_lineNumber
                end subroutine readInteger

                subroutine readLogical(data)
                    implicit none
                    logical, intent(out) :: data

                    call readLine()
                    read(global_line, *, iostat=global_ioStatus) data
                    call checkData(typeLogical)

!                    print *, "readLogical: <", data, ">, line: ", global_lineNumber
                end subroutine readLogical

                subroutine readString(data)
                    implicit none
                    character(255), intent(out) :: data

                    call readLine()
                    data = global_line

!                    print *, "readString: <", trim(data), ">, line: ", global_lineNumber
                end subroutine readString

                subroutine readConfig(configData)
                    implicit none
                    type(config), intent(out) :: configData

                    ! for debugging purpose
                    real(8) :: Ne
                    real(8) :: D

                    print *, "reading configuration file: <" // trim(global_fileName) // ">"

                    open(global_fileUnit, file=global_fileName, iostat=global_ioStatus, &
                         status='old', action='read')

                    if (global_ioStatus /= 0) then
                        print *, "could not open file <" // trim(global_fileName) // ">"
                        stop
                    end if

                    ! read each parameter line by line

                    ! MODEL PROCESSES
                    ! INPUT A
                    call readLogical(configData%fluvial_erosion)

                    call readLogical(configData%ideposition)

                    call readLogical(configData%idiffusion)

                    call readLogical(configData%ilandslide)

                    call readLogical(configData%iceflag)

                    call readLogical(configData%glacial_erosion)

                    call readLogical(configData%iflexure)

                    call readLogical(configData%ihorizontal)
                    
                    call readInteger(configData%ihorizontal_mode)

                    call readLogical(configData%iflag_oro)
                    configData%iflag_uni = .not.configData%iflag_oro

                    call readLogical(configData%use_comsol)

                    ! MODEL OUTPUT
                    ! INPUT B
                    call readLogical(configData%tecflag)

                    call readInteger(configData%nshortwrite)

                    call readReal(configData%iflux)

                    call readInteger(configData%writetime)
                    call readInteger(configData%writeice)

                    configData%run_name(1:255) = ' '

                    call readString(configData%run_name)
                    configData%run_name = 'output/' // trim(configData%run_name)
                    configData%nrun_name = len_trim(configData%run_name)

                    ! MODEL SET UP: TIME DOMAIN
                    ! INPUT C
                    call readLogical(configData%iadjust)

                    call readReal(configData%dt0)

                    call readReal(configData%endtime)


                    ! MODEL SET UP: SPATIAL DOMAIN
                    ! INPUT D
                    call readInteger(configData%nx)
                    call readInteger(configData%ny)
                    configData%nnode = configData%nx * configData%ny

                    call readReal(configData%sidex)
                    call readReal(configData%sidey)
                    configData%surfscale = configData%sidex * configData%sidey / dble(configData%nnode)

                    if (configData%surfscale < 0.0_8) then
                        print *, "surfscale must be > 0!"
                        print *, "surfscale: ", configData%surfscale, ", sidex: ", configData%sidex, &
                                 ", sidey: ", configData%sidex, ", nnode: ", configData%nnode
                        stop
                    endif

                    configData%surfmin = configData%surfscale / 4.0_8

                    call readLogical(configData%iadapt)

                    call readLogical(configData%meshread)

                    call readString(configData%meshname)

                    call readInteger(configData%meshformat)

                    call readLogical(configData%addshelf)
                    call readReal(configData%Exr)
                    call readReal(configData%Exl)
                    call readReal(configData%Eyd)
                    call readReal(configData%Eyu)

                    call readReal(configData%slopexr)
                    call readReal(configData%slopexl)
                    call readReal(configData%slopeyd)
                    call readReal(configData%slopeyu)

                    call readInteger(configData%nxe)
                    call readInteger(configData%nye)


                    ! EROSION: FLUVIAL
                    ! INPUT E
                    call readReal(configData%xkf)

                    call readReal(configData%xlf_BR)

                    call readReal(configData%width_c)

                    call readReal(configData%thresh)

                    call readReal(configData%xlf_AL)

                    call readReal(configData%sea_level)


                    ! EROSION: HILLSLOPES
                    ! INPUT F
                    call readReal(configData%xkdiff)

                    call readInteger(configData%lsmeth)

                    call readReal(configData%pmax)
                    configData%pmax = configData%pmax * GLOBAL_PI / 180.0_8

                    call readReal(configData%distmax)

                    call readReal(configData%cohes)

                    call readReal(configData%rho)

                    call readReal(configData%grav)

                    call readReal(configData%xk0)
                    call readReal(configData%xk1)
                    call readReal(configData%dtc)
                    


                    ! ICE TIME SETUP
                    ! INPUT G
                    call readReal(configData%calc_rain_ice)

                    call readReal(configData%shallow_ice)

                    call readReal(configData%dh_allowed)

                    call readReal(configData%dt_icetime)

                    call readReal(configData%ice_tfinal)


                    ! ICE FLAGS
                    ! INPUT H
                    call readLogical(configData%itemp)


                    ! ICE MECHANICS
                    ! INPUT I
                    call readReal(configData%ice_flow)

                    call readReal(configData%sliding_law)

                    call readReal(configData%power_law_exp)

                    call readReal(configData%exp_sliding_law)

                    call readReal(configData%rho_ice)

                    call readReal(configData%constriction)


                    ! ICE EROSIOIN
                    ! INPUT J
                    call readReal(configData%erosion_rate)

                    call readReal(configData%ice_ero_pow)


                    ! ICE THERMAL FIELD
                    ! INPUT K
                    call readReal(configData%basal_heat_flux)

                    call readReal(configData%conductivity)


                    ! MISCELLANEOUS ICE
                    ! INPUT L
                    call readReal(configData%critangle)
                    configData%critangle = configData%critangle * GLOBAL_PI / 180.0_8

                    call readReal(configData%calvecoef)


                    ! TECTONICS (cascade code)
                    ! INPUT M
                    call readInteger(configData%uplift_mode)
                    
                    call readReal(configData%uplift_rate)
                    ! convert uplift_rate from [mm/y] to [m/y]
                    configData%uplift_rate = configData%uplift_rate * 1e-3_8

                    call readString(configData%MOVE_velocity_file)

                    call readInteger(configData%timesteps)

                    call readReal(configData%advec_velx)
                    ! convert advec_vel from [mm/y] to [km/y]
                    configData%advec_velx = configData%advec_velx * 1e-6_8

                    call readReal(configData%advec_vely)
                    ! convert advec_vel from [mm/y] to [km/y]
                    configData%advec_vely = configData%advec_vely * 1e-6_8

                    call readReal(configData%hflex)

                    call readLogical(configData%ixflex)

                    call readLogical(configData%iyflex)

                    call readReal(configData%thickflex)

                    call readReal(configData%young_mod)

                    call readReal(configData%pratio)

                    call readReal(configData%rhocflex)

                    call readReal(configData%rhoaflex)

                    call readReal(configData%flexon)


                    ! CLIMATE
                    ! INPUT N
                    call readReal(configData%calc_rain)

                    call readInteger(configData%del_gr)

                    call readReal(configData%rain_vel)

                    call readInteger(configData%calc_ice)

                    call readReal(configData%iceon)

                    call readReal(configData%a0)
                    call readReal(configData%a1)

                    call readReal(configData%alf)

                    call readReal(configData%wnd)

                    call readReal(configData%angle)

                    call readLogical(configData%y_flip)

                    call readReal(configData%xwnd_s)

                    call readReal(configData%upwnd_s)

                    call readInteger(configData%subsam)

                    call readReal(configData%xlapse_rate)

                    call readReal(configData%At)

                    call readReal(configData%xpmax)

                    call readLogical(configData%use_max_accu)

                    call readReal(configData%kmelt)

                    call readReal(configData%temp0min)

                    call readReal(configData%temp0max)

                    call readReal(configData%xp)

                    !
                    !Temperature Input O
                    !! If something is not working just comment this section till the !!End!!

                    call readLogical(configData%temp_from_file)

                    call readString(configData%temp_file_name)

                    call readReal(configData%amp_temp)

                    call readReal(configData%IG_O18)

                    call readReal(configData%LGM_O18)

                    call readReal(configData%mean_t)

                    !!End!!

                    GLOBAL_C = 2.0_8 * configData%ice_flow * &
                               (configData%rho_ice * configData%grav)**configData%power_law_exp / &
                               dble(configData%power_law_exp + 2.0_8) * SEC_IN_YR

                    GLOBAL_CS = configData%sliding_law * &
                                (configData%rho_ice * configData%grav)**configData%exp_sliding_law / 0.8_8

                    print *,"GLOBAL_C = ", GLOBAL_C
                    print *,"GLOBAL_CS = ", GLOBAL_CS

                    diffusivity = configData%conductivity / configData%rho_ice / 2115.0_8

                    dtdzb = configData%basal_heat_flux / configData%conductivity

                    ! print out Ne and D
                    Ne = (configData%xkf/configData%xlf_BR)*(1000.0_8 * configData%sidex)/ &
                            (configData%width_c * sqrt(configData%uplift_rate / 1000.0_8))
                    D  = configData%xkdiff / (configData%sidex * (configData%uplift_rate / 1000.0_8))
                    print *,'Ne = ',Ne, ', log10(Ne) = ',log10(Ne)
                    print *,'D  = ',D,  ', log10(D)  = ',log10(D)

                    print *, 'Input O values:'
                    print *, 'read from file: ', configData%temp_from_file
                    print *, 'file name: ', configData%temp_file_name
                    print *, 'amp: ', configData%amp_temp
                    print *, 'ig_o18, lgm_o18, mean: ', configData%IG_O18, configData%LGM_O18, configData%mean_t


                    close(global_fileUnit)
                end subroutine readConfig
        end module rt_param
