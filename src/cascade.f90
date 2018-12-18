        program cascade
!  -----------------------------------------------------
!  |                                                   |
!  | CCCCC     A     SSSSS CCCCC     A     DDDD  EEEEE |
!  | C        A A    S     C        A A    D   D E     |
!  | C       AAAAA   SSSSS C       AAAAA   D   D EEEE  |
!  | C      A     A      S C      A     A  D   D E     |
!  | CCCCC A       A SSSSS CCCCC A       A DDDD  EEEEE |
!  |                                                   |
!  -----------------------------------------------------

! The program cascade was developed by:

!        Jean Braun
!        Research School of Earth Sciences
!        Australian National University
!        Canberra, ACT, 0200
!        Australia
!        Tel: +61-2-6249-5512
!        Fax: +61-2-6249-5443
!        email: Jean.Braun@anu.edu.au

! (Canberra, June 1st, 1995)
! (Present version August 28, 1998)

! Please report any problem and certainly any improvement you bring to
! cascade.

! cascade is a geomorphic program to compute the evolution of landscapes
! by erosion/deposition and tectonic uplift/subsidence. Two types of processes
! are included: 1) short-range (hillslope) processes, modeled by a simple linear
!                  diffusion equation
!               2) long-range (river) processes, modeled by an equation of
!                  reaction between water flowing in a river network and the
!                  substratum

! The main advantage/difference between cascade and other geomorphic models
! is its ability to handle arbitrary, non-rectangular, grids at the corners
! of which the computations are done. This opens opportunities to vary spatial
! discretization in various parts of the model or to adjust spatial discretiza-
! tion to the results of the model (evolving discretization).

! cascade uses the theory of Delaunay triangulation and Voronoi diagrams
! to compute the possible "connections" between neighbouring nodes on the
! arbitrary grid and the surface area of the part of the landscape the height
! of which is slave to any node of the grid.

! The river network is calculated from the "bucket passing" algorithm (BPA)
! that does not require a complete ordering of the nodes according to their
! heights but rather a local-only ordering that depends on the position of the
! nodes with respect to local maxima in heights. In BPA, each node is given a
! bucket full of water and asked to pass it to its lowest neighbour. After that
! operation, all nodes that have not received anything are local maxima and
! are put at the top of a stack. After the next bucket passing step, those that
! have not received anything are put on the stack, and so on until all nodes
! have been put on the stack. The stack contains an ordering of the nodes that
! is appropriate to calculate the effect of river flow on the landscape. 

! note that because this version of the bucket algorithm can handle local
! minima, runs tend to be slower at the beginning when many local minima
! exist. As the minima disappear due to river erosion and deposition, one
! should notice an increase efficiency per time step

! In this version, the diffusion equation is solved by using a "classical"
! 3 node linear finite element code with an iterative solver (Gauss-Seidel).
! The diffusion equation solver is the slow part of the code and future versions
! of cascade will be improved in this regard.

! cascade has the possibility of including a stratified crust with
! regards to erosional parameters
! Developed by Peter van der Beek, April '96

! IMPORTANT NOTE:
!****************
! Please, note that this software CANNOT be freely distributed. You must
! obtain Jean Braun's permission to use it (or part of it) or to give to other
! potential users. Please, respect this condition of use. I am trying to 
! protect parts of the Delaunay/Voronoi algorithms that we are using in a
! commercial venture with Malcolm Sambridge. This means that some of our
! "clients" had to pay to use this software commercially.

! Good luck.

! NOTE from check_mesh.f on requirements for remeshing ...
! It is important that, if this routine is used, that is if dynamic 
! remeshing is turned on, all nodal parameters that you have added (such
! as a new nodal property) be passed here for what is called permutation.
! During permutation, nodes are renumbered (see near bottom of the
! subroutine) and nodal properties have to be updated accordingly.
!  In this new version all the properties and parameters that have to
! be permuted are stored in memory and param


! This version of CASCADE is a fork from the original version published by Braun and Sambridge, 1997 Basin Research. The modifications made to the original version of CASCADE are described in Yanites and Ehlers, 2012, 2016 EPSL, and Eizenhoefer et al., 2019 JGR-ES.
! If you published results from this version of the program we would appreciate that you reference:
! 1. Publications listed by J. Braun in the cascade.f90 file.
! 2. Yanites, B.J., and Ehlers, T.A., 2012, Global climate and tectonic controls on the denudation of glaciated mountains, Earth and Planetary Science Letters, v. 325-326, pp. 63-75. doi.org/10.1016/j.epsl.2012.01.030.
! 3. Yanites, B.J., and Ehlers, T.A., 2016, Intermittent glacial sliding velocities explain variations in long-timescale denudation, SW British Columbia. Earth and Planetary Science Letters, 450, pp. 52-61. 
! 3. Eizenhoefer, P.R., McQuarrie, N., Shelef, E., and Ehlers, T.A., 2019 (in press), Fluvial responses to horizontal displacements in convergent orogens over geologic time scales, Journal of Geophysical Research - Earth Surface.

! Please direct all inquiries concerning this fork of the source code to Todd Ehlers (Univ. Tuebingen, Germany) tehlers@icloud.com


!--------------------------------------------------------------------------

! subroutines called:
! - debug
! - initialize_general_parameters
! - initialize_nodal_geometry
! - erosional_properties
! - update_time_step
! - find_neighbours
! - write_output
! - change_sea_level
! - erosional_properties
! - tectonic_uplift
! - tectonic_movement
! - orography
! - find_donors
! - find_catchment
! - fluvial_erosion
! - diffusion_erosion
! - flexure
! - show
! - check_mesh
! - update_bedrock
! - write_output
! - mdian2
! - update_time_step
! - landslide or
! - landslide_simple

! the module rt_param contains the data structure for the run time parameters
! and the functions to read those parameters from file
        use rt_param
        use cascade_globals

! for each file there is a module:
        use m_change_sea_level
        use m_update_bedrock
        use m_terosion
        use m_debug
        use m_find_dslope
        use m_update_flags
        use m_surface_temperature
        use m_erosional_properties
        use m_interp_ice
        use m_find_donors
        use m_tectonic_uplift
        use m_gerode_node
        use m_diffusion_erosion
        use m_mass_balance
        use m_update_time_step
        use m_find_catchment
        use m_find_neighbours
        use m_tectonic_movement
        use m_write_output
        use m_initialize_nodal_geometry
        use m_landslide_simple
        use m_fluvial_erosion
        use m_read_nodal_geometry
        use m_flexure
        use m_landslide
        use m_ice
        use m_check_mesh
        use m_rainmaker
        use m_MOVEtoCascade
        use m_checkMeshResolution
        implicit none

! define global data structure for configuration items
        type(config) :: ConfigData

! only used in this context / scope. (= not global)
        integer(4) :: i, k, system

        iseed = 1



! initialize variables, WK
        nt = ntmax

! system time variables
        print *,'**********************************'
        print *,'Start time:'
        tsys0 = system('date')
        print *,'**********************************'

! use dynamic arrays, WK

! one dimension
        allocate(x(nnodemax))
        allocate(y(nnodemax))
        allocate(hi(nnodemax))
        allocate(h(nnodemax))
        allocate(h0(nnodemax))
        allocate(isodh(nnodemax))
        allocate(xd(nnodemax))
        allocate(yd(nnodemax))
        allocate(hd(nnodemax))
        allocate(xl(nnodemax))
        allocate(yl(nnodemax))
        allocate(hl(nnodemax))
        allocate(xu(nnodemax))
        allocate(yu(nnodemax))
        allocate(hu(nnodemax))
        allocate(xr(nnodemax))
        allocate(yr(nnodemax))
        allocate(hr(nnodemax))
        allocate(dhg(nnodemax))
        allocate(hicerem(nnodemax))
        allocate(lastice_h(nnodemax))
        allocate(ldh(nnodemax))
        allocate(gerode_term(nnodemax))
        allocate(work(nnodemax))
        allocate(water(nnodemax))
        allocate(sediment(nnodemax))
        allocate(orwater(nnodemax))
        allocate(slope(nnodemax))
        allocate(length(nnodemax))
        allocate(ndon(nnodemax))
        allocate(nb(nnodemax))
        allocate(nb2(nnodemax))
        allocate(ibucket(nnodemax))
        allocate(iorder(nnodemax))
        allocate(itype_node(nnodemax))
        allocate(nwork(nnodemax))
        allocate(ncat(nnodemax))
        allocate(nsill(nnodemax))
        allocate(nempty(nnodemax))
        allocate(nlake(nnodemax))
        allocate(prec(nnodemax))
        allocate(y_gr(nnodemax))
        allocate(x_gr(nnodemax))
        allocate(nodes(nnodemax))
        allocate(vis_tlist(nnodemax))
        allocate(vis_elist(nnodemax))
        allocate(add_tlist(nnodemax))
        allocate(nodelist(nbmax))
        allocate(tlist(nbmax))
        allocate(c_list(2*nbmax))
        allocate(mask(3*nnodemax))
        allocate(inactive(nnodemax))
        allocate(anthi(nnodemax))

! two dimensions
        allocate(memory(nnodemax,nmemory))
        allocate(param(nnodemax,nparam))
        allocate(nn2(nbmax,nnodemax))
        allocate(nn(nbmax,nnodemax))
        allocate(kcon(ntmax,nnodemax))
        allocate(jcon(ntmax,nnodemax))
        allocate(flex(nflex,nflex))
        allocate(work_flex(nflex,nflex))
        allocate(zpt(nxi,nyi))
        allocate(bipt(nxi,nyi))
        allocate(bface(nxi,nyi))
        allocate(tbipt(nxi,nyi))
        allocate(antitoc(nxi,nyi))
        allocate(points(2,nnodemax))
        allocate(vertices(3,nnodemax*3))
        allocate(neighbour(3,nnodemax*3))
        allocate(v_local(3,nbmax*2))
        allocate(n_local(3,nbmax*2))
        allocate(mask_e(3,nnodemax*3))

! three dimensions
        allocate(cell(nnodemax,nbmax,2))

! initializes memory
        do k=1,nmemory
            do i=1,nnodemax
                memory(i,k)=0.0_8
            enddo
        enddo

        do k=1,nnodemax
            do i=1,nparam
                param(k,i)=0.0_8
            enddo
        enddo

        do k=1,nbmax
            do i=1,nnodemax
                nn2(k,i)=0
                nn(k,i)=0
            enddo
        enddo

        do k=1,ntmax
            do i=1,nnodemax
                kcon(k,i)=0
                jcon(k,i)=0
            enddo
        enddo

        do k=1,nflex
            do i=1,nflex
                flex(k,i)=0.0_8
                work_flex(k,i)=0.0_8
            enddo
        enddo

        do k=1,nxi
            do i=1,nyi
                zpt(k,i)=0.0_8
                bipt(k,i)=0.0_8
                bface(k,i)=0.0_8
                tbipt(k,i)=0.0_8
                antitoc(k,i)=0
            enddo
        enddo

        do k=1,2
            do i=1,nnodemax
                points(k,i)=0.0_8
            enddo
        enddo

        do k=1,3
            do i=1,nnodemax
                mask_e(k,i)=.FALSE.
            enddo
        enddo

        do k=1,3
            do i=1,nbmax*2
                v_local(k,i)=0
                n_local(k,i)=0
            enddo
        enddo

        do k=1,nnodemax
            x(k) = 0.0_8
            y(k) = 0.0_8
            hi(k) = 0.0_8
            h(k) = 0.0_8
            h0(k) = 0.0_8
            isodh(k) = 0.0_8
            xd(k) = 0.0_8
            yd(k) = 0.0_8
            hd(k) = 0.0_8
            xl(k) = 0.0_8
            yl(k) = 0.0_8
            hl(k) = 0.0_8
            xu(k) = 0.0_8
            yu(k) = 0.0_8
            hu(k) = 0.0_8
            xr(k) = 0.0_8
            yr(k) = 0.0_8
            hr(k) = 0.0_8
            dhg(k) = 0.0_8
            hicerem(k) = 0.0_8
            lastice_h(k) = 0.0_8
            ldh(k) = 0.0_8
            gerode_term(k) = 0.0_8
            work(k) = 0.0_8
            water(k) = 0.0_8
            sediment(k) = 0.0_8
            orwater(k) = 0.0_8
            slope(k) = 0.0_8
            length(k) = 0.0_8
            ndon(k) = 0
            nb(k) = 0
            nb2(k) = 0
            ibucket(k) = 0
            iorder(k) = 0
            itype_node(k) = 0
            nwork(k) = 0
            ncat(k) = 0
            nsill(k) = 0
            nempty(k) = 0
            nlake(k) = 0
            prec(k) = 0.0_8
            y_gr(k) = 0.0_8
            x_gr(k) = 0.0_8
            nodes(k) = 0
            vis_tlist(k) = 0
            vis_elist(k) = 0
            add_tlist(k) = 0
            inactive(k) = .FALSE.
            anthi(k) = 0

        enddo

        do k=1,3
            do i=1,nnodemax*3
                vertices(k,i) = 0
                neighbour(k,i) = 0
            enddo
        enddo

! ivocal = .TRUE. means CASCADE will save debugging information in debug.out
! should be set to ivocal = .FALSE. in production runs

        ivocal = .TRUE.

        if (.not.ivocal) then
            open (89,file='debug.out',status='unknown')
            write (89,'(a)') 'Debugger not activated'
            write (89,'(a)') 'To activate set ivocal to 1 in cascade'
            close (89)
        else
            open (89,file='debug.out',status='unknown')
            rewind (89)
        endif

! if ivocal=1 opens a log file to record which nodes are being
! added/removed from the mesh

        if (ivocal) open (22,file='checkMeshResolution.out',status='unknown')

! read configuration from file "input/IceCascade/icecascade.in"
! fortran uses call by reference, so the data structure is not copied and no
! pointer is needed here!
        call readConfig(ConfigData)

!        print *, "configData:"
!        print *, configData

!        print *, "end of test readConfig"

        call flush()

! this is no longer needed and can be removed later:
! initialize general parameters
        if (ivocal) call debug ('cascade$')

! prep intialize geometry if adding a shelf.
! Takes variables defined in initialize nodal geometry and resets nnode.
! Done in cascade main becasue I was getting seg faults otherwise.  Should move this at some point
        if (configData%addshelf) then

           print *, "sidex: ", configData%sidex, ", sidey: ", configData%sidey

           ! new width and height of domain
           configData%sidex = configData%sidex + configData%Exl + configData%Exr
           configData%sidey = configData%sidey + configData%Eyu + configData%Eyd

           print *, "new sidex: ", configData%sidex, ", new sidey: ", configData%sidey

           ! new grid step size
           dye = int(configData%Eyd + configData%Eyu) / configData%nye
           dxe = int(configData%Exl + configData%Exr) / configData%nxe

           if (dxe == 0) then
              dxe=dye
           end if

           if (dye == 0) then
              dye=dxe
           end if

           if ((dxe > 0) .and. (dye > 0)) then
              print *, "dxe: ", dxe, ", dye: ", dye

                nexd=int(configData%sidex)/dxe
                neyd=int(configData%Eyd)/dye - 1

                if (neyd < 0) then
                   neyd = 0
                end if

                ned=nexd*neyd

                print *, "nexd: ", nexd, ", neyd: ", neyd, ", ned: ", ned

                nexu=int(configData%sidex)/dxe
                neyu=int(configData%Eyu)/dye - 1

                if (neyu < 0) then
                   neyu = 0
                end if

                neu=nexu*neyu

                print *, "nexu: ", nexu, ", neyu: ", neyu, ", neu: ", neu

                neyl=int(configData%sidey-configData%Eyd-configData%Eyu)/dye
                nexl=int(configData%Exl)/dxe
                nel=neyl*nexl

                print *, "nexl: ", nexl, ", neyl: ", neyl, ", nel: ", nel

                neyr=int(configData%sidey-configData%Eyd-configData%Eyu)/dye
                nexr=int(configData%Exr)/dxe
                ner=neyr*nexr

                print *, "nexr: ", nexr, ", neyr: ", neyr, ", ner: ", ner

                ! nextra: number of additional shell nodes
                nextra=ned+neu+nel+ner
                print *, "cascade, add shelf:"
                print *, "nextra: ", nextra
                print *, "nnode: ", configData%nnode
                print *, "nx, ny: ", configData%nx, configData%ny
                configData%nnode = configData%nnode + nextra
                print *, "nnode: ", configData%nnode

           end if
        end if

!  Prep rainmaker
        nxs = int(configData%sidex)/configData%del_gr + 1
        nys = int(configData%sidey)/configData%del_gr + 1
        nxice=nxs
        nyice=nys
        delx=configData%sidex/dble(nxice-1)
        dely=configData%sidey/dble(nyice-1)

! allocate memory once
! we meed to do it here, because we need the values nxs and nys

        allocate(prec_gr(nxs,nys))
        allocate(z(nxs,nys))
        allocate(hforice(nxs,nys))
        allocate(slidetoc(nxs,nys))
        allocate(htoc(nxs,nys))
        allocate(httoc(nxs,nys))
        allocate(iceftoc(nxs,nys))
        allocate(constoc(nxs,nys))

        do k=1,nys
            do i=1,nxs
                z(i,k) = 0.0_8
                hforice(i,k) = 0.0_8
                prec_gr(i,k) = 0.0_8
                slidetoc(i,k) = 0.0_8
                htoc(i,k) = 0.0_8
                httoc(i,k) = 0.0_8
                iceftoc(i,k) = 0.0_8
                constoc(i,k) = 0.0_8
            enddo
        enddo

! initialize the node geometry
        if (configData%meshread) then
            if (configData%meshformat == 1) then
                if (ivocal) then 
                call debug ('read_nodal_geometry$')
                call read_nodal_geometry(configData)
                end if
            end if
            if (configData%meshformat == 2) then
            ! WK: TODO
            end if
            if (configData%meshformat == 3) then ! added by PRE Jan2017
                call MOVEtoCascadeTopo(configData)
            end if
        else
            if (ivocal) call debug ('initialize_nodal_geometry$')
                call check_h(1)
                call initialize_nodal_geometry(configData)
                call check_h(2)
        end if
        tott=h
        if (ivocal) call debug ('cascade$')
        nnode0 = configData%nnode

        do i=1,configData%nnode
            if (.not.configData%meshread) then
                h0(i) = h(i)
            end if
            hi(i) = h(i)
        enddo

        temperature = configData%temp0min


! opens various output files

        if (configData%nrun_name == 0) then
            print *,'No run name available'
            stop
        endif

        inquire (file=configData%run_name(1:configData%nrun_name), &
                exist=cascade_FileExists, iostat=cascade_ioStatus)

        if (.not.cascade_FileExists) then
            print *, 'creating folder: ', configData%run_name(1:configData%nrun_name)
            system_Result = system('mkdir ' // configData%run_name(1:configData%nrun_name))
        end if

        open (7, file=configData%run_name(1:configData%nrun_name) // '/topography', &
              status='unknown', iostat=cascade_ioStatus)

        if (cascade_ioStatus /= 0) then
            print *,'error, could not open file: ', (configData%run_name(1:configData%nrun_name) // 'topography')
            stop
        end if

        open (10,file=configData%run_name(1:configData%nrun_name)//'/geometry', status='unknown')

        if (.not.configData%tecflag) then
            open (8,file=configData%run_name(1:configData%nrun_name)//'/connectivity',status='unknown')
            open (9,file=configData%run_name(1:configData%nrun_name)//'/donors',status='unknown')
            open (11,file=configData%run_name(1:configData%nrun_name)//'/properties',status='unknown')
            open (12,file=configData%run_name(1:configData%nrun_name)//'/discharge',status='unknown')
            open (13,file=configData%run_name(1:configData%nrun_name)//'/erosion_rate',status='unknown')
            open (14,file=configData%run_name(1:configData%nrun_name)//'/catchments',status='unknown')
            open (15,file=configData%run_name(1:configData%nrun_name)//'/lakes',status='unknown')
            open (16,file=configData%run_name(1:configData%nrun_name)//'/slides',status='unknown')
            open (17,file=configData%run_name(1:configData%nrun_name)//'/slopearea',status='unknown')
            open (77,file=configData%run_name(1:configData%nrun_name)//'/precip',status='unknown')
        endif

! initialize erosional nodal properties
        if (ivocal) call debug ('erosional_properties$')
        call check_h(3)
        call erosional_properties (configData)
        call check_h(4)
        if (ivocal) call debug ('cascade$')

! initialize time step
        if (ivocal) call debug ('update_time_step$')
        call check_h(5)
        call update_time_step (configData, .FALSE.)
        call check_h(6)
        if (ivocal) call debug ('cascade$')

      
! writes the initial conditions

        if (ivocal) call debug ('write_output$')
        timeint=-1
        call check_h(7)
        call write_output(configData, .TRUE.)
        call check_h(8)
        if (ivocal) call debug ('cascade$')

! WK: DEBUG
!        stop

! finds Delaunay triangulation and voronoi cell surface areas for initial
! set of nodes
        if (ivocal) call debug ('find_neighbours$')

!        print *, "x: ", x(1:10)
!        print *, " y: ", y(1:10)
!        print *, "nn: ", nn(1:10, 1)
!        print *, "nb: ", nb(1:10)
!        print *, "nnode: ", configData%nnode
!        print *, "nbmax: ", nbmax
!        print *, "nn2: ", nn2(1:10, 1)
!        print *, "nb2: ", nb2(1:10)
!        print *, "points: ", points(1:10, 1)
!        print *, "vertices: ", vertices(1:10, 1)
!        print *, "neighbour: ", neighbour(1:10, 1)
!        print *, "nodes: ", nodes(1:10)
!        print *, "vis_tlist: ", vis_tlist(1:10)
!        print *, "vis_elist: ", vis_elist(1:10)
!        print *, "add_tlist: ", add_tlist(1:10)
!        print *, "nt: ", nt
!        print *, "memory(1, 7): ", memory(1, 7)
!        print *, "memory(1, 6): ", memory(1, 6)
!        print *, "eps: ", eps
!        print *, "xy: ", xy
!        print *, "pp: ", pp
!        print *, "aa: ", aa(1:10, 1)
!        print *, "bb: ", bb(1:10)
!        print *, "surfscale: ", configData%surfscale
!        print *, "cell: ", cell(1:10, 1, 1)
!        print *, "finish"

!        print *, "nnodemax: ", nnodemax
!        print *, "nmemory: ", nmemory

        call check_h(9)
        call find_neighbours (configData)
        call check_h(10)

        if (ivocal) call debug ('cascade$')

! writes the initial conditions
        if (ivocal) call debug ('write_output$')
        timeint=-1
        call check_h(11)
        call write_output(configData, .TRUE.)
        call check_h(12)
        if (ivocal) call debug ('cascade$')

! start of time stepping
        cascade_istep=0

! initalize orographic time counter (via TE code 10/06)
        oro_time=configData%calc_rain + 1.0_8
      
! intialize ice time counter BJY 101109
        ice_time=dble(configData%calc_ice + 1)
        global_iceIsRunning = .FALSE.

! initialize time since last addition of nodes in tectonic_movement.f
        tcheck = 0.0_8

        do while (time < configData%endtime)
            cascade_istep=cascade_istep+1
      
! update flags for ice and flexure
            call check_h(13)
            call update_flags(configData)
            call check_h(14)

! change in sea level
            if (ivocal) call debug ('change_sea_level$')
            call check_h(15)
            call change_sea_level (configData)
            call check_h(16)
            if (ivocal) call debug ('cascade$')

! initializes the flux of material entering and leaving the grid
! for this time step
            influx=0.0_8
            outflux=0.0_8

! distributes erosional properties to rocks according to the amount eroded
! i.e. makes layered crust (Peter, April '96)
            if (ivocal) call debug ('erosional_properties$')
            call check_h(17)
            call erosional_properties (configData)
            call check_h(18)
            if (ivocal) call debug ('cascade$')


! add tectonic uplift/subsidence component to landscape height (modified by PRE Jan2017)
! (note that heights are in km)

            if (configData%uplift_mode == 1) then
                if (ivocal) call debug ('tectonic_uplift$')
                    call check_h(19)
                    call tectonic_uplift (configData)
                    call check_h(20)
                if (ivocal) call debug ('cascade$')
                    call check_h(20)
                if (ivocal) call debug ('cascade$')
            end if
            if (configData%uplift_mode == 2) then
                call MOVEtoCascadeUplift(configData) ! Added by PRE Jan2017
            end if
            if (configData%uplift_mode == 3) then ! Added by PREFeb2017
                call MOVEtoCascadeVelocityField(configData)
                call MOVEtoCascadeUplift(configData)
            end if


! add tectonic horizontal movement (modified by PRE Jan2017)
            if (configData%ihorizontal) then
                tcheck = tcheck + global_cascade_dt
                if (configData%ihorizontal_mode == 1) then
                    if (ivocal) call debug ('tectonic_movement$')
                    call check_h(21)
                    call tectonic_movement (configData)
                    call check_h(22)
                    if (ivocal) call debug ('cascade$')
                end if
                if (configData%ihorizontal_mode == 2) then ! added by PRE Jan2017
                    if (configData%uplift_mode == 2) then
                        call MOVEtoCascadeHmove(configData)
                    elseif (configData%uplift_mode == 3) then
                        call MOVEtoCascadeHmove(configData)
                    end if
                end if
            end if

!  Following subroutine implements a variable climate that is fed into both rainmaker and mass_balance
            call check_h(23)
            call surface_temperature (configData)
            call check_h(24)
 
! Calculate Precipitation - added by DW 10/06 (using TE code)
            if (ivocal) call debug ('rainmaker$')

! call new routine.
! call every time step.
! if update time exceeded then set oro_flag to one
! which cases reinterpolation of the nodal topography
! on to the regular grid in routine orography_new ghr 07/01

! DEBUG MODE 10/11/01 on next line
            water=orwater
            oro_time = oro_time+global_cascade_dt
            if ((oro_time > configData%calc_rain).or.(cascade_istep == 1)) then
                print*,'rainmaker: total topography (surface +ice)',oro_time, configData%calc_rain
                oro_time = 0.0_8
     
!  NOTE:  CHanged h from 'tott' total topography because weird things were happening with ice now that rainmaker builds ice topography       
                call check_h(25)
                call rainmaker(configData)
                call check_h(26)
                orwater=water
            end if

            if (ivocal) call debug ('cascade$')


! find the donor for each node (its lowest neighbour)

            if (ivocal) call debug ('find_donors$')
!         write(6,*) 'finding donors... '
            call check_h(27)
            call find_donors (configData)
            call check_h(28)
            if (ivocal) call debug ('cascade$')

! find rain function (water) - commented out by DW 10/06

!      if (ivocal.eq.1) call debug ('orography$')
!      call orography (x,y,h,water,memory(1,7),length,work,
!     &                ndon,nn,nb,nbmax,
!     &                nwork,
!     &                oro_length,oro_height,oro_scale,
!     &                nnode,wind_direction,rain_vel)
!      if (ivocal.eq.1) call debug ('cascade$')

! find catchment for each node

            if (ivocal) call debug ('find_catchment$')
            call check_h(29)
            call find_catchment (configData)
            call check_h(30)
            if (ivocal) call debug ('cascade$')

      
!  CALL ICE HERE
            if (global_iceIsRunning) then
                ice_time=ice_time+global_cascade_dt
                ice_topotime=ice_topotime+int(global_cascade_dt)
       
! checking maximum change in topography for a node since the last time Ice was called
                ldh=(h-lastice_h)
                ldh=abs(ldh)
                lastice_dh=maxval(ldh)
        

!  Add more requirements such as (maybe): max(gbalance)>0  or max(iceth>0) or  topography changes by 25 m or more

                if ((ice_time > dble(configData%calc_ice)).or.(cascade_istep == 1)) then
                    ice_time=0.0_8

!      print*,'--converting Cascade to Ice--',lastice_dh


                    lastice_h=h

                    print *,'*******Updating Ice, Ice, baby*******'
                    ice_topotime=0
      
!   call ice, returns ice
                    hicerem=iceth

                    if (ivocal) call debug ('ice, interp_ice$')
                    call check_h(31)
                    call ice(configData)
                    call check_h(32)
                    call interp_ice(configData)
                    call check_h(33)

                    print *, "ice finished!"
                end if ! ice_time
            end if ! end global_iceIsRunning

!   Find where to erodoe by glacial erosion and erode there

            if (configData%glacial_erosion) then
                if (ivocal) call debug ('gerode_node$')
                call check_h(34)
                call gerode_node(configData)
                call check_h(35)
            endif
! river erosion

!       print*,dt,time,cascade_istep
        
            if (configData%fluvial_erosion) then
                if (ivocal) call debug ('fluvial_erosion$')
                call check_h(36)
                call fluvial_erosion (configData)
                call check_h(37)
            end if

            if (ivocal) call debug ('cascade$')

! diffusion erosion
            if (configData%idiffusion) then
                if (ivocal) call debug ('diffusion_erosion$')
                call check_h(38)
                call diffusion_erosion (configData)
                call check_h(39)
            endif
            if (ivocal) call debug ('cascade$')

! landsliding
            if (configData%ilandslide) then
                if ((configData%lsmeth == 1).or.(configData%lsmeth == 2)) then
                    if (ivocal) call debug ('landslide$')
                    do i=1,configData%nnode
                        smax(i)=0.0_8
                        tt(i)=tt(i)+global_cascade_dt
                    enddo
                    call check_h(40)
                    call landslide(configData)
                    call check_h(41)
                elseif (configData%lsmeth == 3) then
                    if (ivocal) call debug ('landslide_simple$')
                    call check_h(42)
                    call landslide_simple(configData)
                    call check_h(43)
                endif
            endif

            if (ivocal) call debug ('cascade$')


! calculate isostatic rebound
            if (configData%iflexure) then
!      print *,'Calculating Flexure',configData%iflexure
                if (ivocal) call debug ('flexure$')
                call check_h(44)
                call flexure(configData)
                call check_h(45)
                if (ivocal) call debug ('cascade$')
            endif


! check the grid (in case adaptive grid is allowed)
      
            if (configData%iadapt) then
                if (ivocal) call debug ('check_mesh$')
                call check_h(46)
                call check_mesh (configData)
                call check_h(47)
                if (ivocal) call debug ('cascade$')
            endif

! If horizontal tectonic movement is turned on, mesh needs to be remeshed regularly PRE NOV2017
            if (configData%ihorizontal) then
                if (meshtime >= dble((delta / 2e-4_8))) then
                    call check_h(150)
                    print*, 'Updating mesh...'
                    call checkMeshResolution(configData)
                    call check_h(151)
                    meshtime = 0.0_8
!																    if (dtold /= 0.0_8) then
!                        global_cascade_dt=dtold
!                        dtold=0.0_8
!                    end if
                    remeshflag = 1
                end if
												end if

! check for bedrock incision
      
            if (ivocal) call debug ('update_bedrock$')
            call check_h(48)
            call update_bedrock (configData)
            call check_h(49)
            if (ivocal) call debug ('cascade$')

! update time
            time=time+global_cascade_dt
            shorttime=shorttime+global_cascade_dt
            meshtime=meshtime+global_cascade_dt ! Added by PRE Nov2017
         
            call check_h(50)
            call terosion()
            call check_h(51)

! writing output

            if (shorttime == dble(configData%writetime)) then
                if (ivocal) call debug ('write_output$')

! find downstream slope
                call check_h(52)
                call find_dslope(configData)
                call check_h(53)
                call mass_balance(configData)
                call check_h(54)
  
! added h0 to the parameter list 
                call flush(6)
     
                call check_h(55)
                call write_output(configData, .FALSE.)
                call check_h(56)
     
                if (ivocal) call debug ('cascade$')

                shorttime=0.0_8
                if (dtold /= 0.0_8) then
                    global_cascade_dt=dtold
                    dtold=0.0_8
                endif
            endif

! write a short line to the screen
            if (mod(itime, configData%nshortwrite) == 0) then
!            if ((itime/configData%nshortwrite*configData%nshortwrite) == itime) then
                hmin=h(1)
                hmax=h(1)
                do i=1,configData%nnode
                    hmin=min(hmin,h(i))
                    hmax=max(hmax,h(i))
                enddo
                hmedian=(hmax-hmin)/2.0_8

! commenting out mdian2 as it seems to give an error, some 
! h(i) values are being set to infinity...
    
                print *, 'TIME ',cascade_istep,' (',time,'), ', 100.0_8 * time / configData%endtime
                tsys0 = system('date')
                print *, 'hrange=[',hmin,',',hmedian,',',hmax,']'
                print *, 'h', maxval(h),minval(h),global_cascade_dt,dhmaxglac,maxval(slide)
                print *, "nnode: ", configData%nnode
                print *, "nx, ny: ", configData%nx, configData%ny
                print *, "sidex, sidey: ", configData%sidex, configData%sidey
!     &     maxval(h-h0)
            endif

! update time step
            if (ivocal) call debug ('update_time_step$')
            call check_h(57)
            call update_time_step (configData, .TRUE.)
            call check_h(58)
            if (ivocal) call debug ('cascade$')

! end of time stepping 

! WK: for gprof:
!        exit

        enddo

! get stop time
        print *,'**********************************'
        print *,'End time:'
        tsys0 = system('date')
        print *,'**********************************'

    contains
        subroutine check_h(num)
            use cascade_globals
            use m_check_mesh
            implicit none

            integer(4) :: i, num

            do i=1,nnodemax
                if (h(i) /= h(i)) then
                    print *, "num: ", num
                    print *, "h(i) is NaN: ", i, h(i)
                    print *, "memory(i,5): ", memory(i,5)
                    stop
                end if
            end do

        end subroutine check_h
    end
