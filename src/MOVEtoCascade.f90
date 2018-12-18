! This version of CASCADE is a fork from the original version published by Braun and Sambridge, 1997 Basin Research. The modifications made to the original version of CASCADE are described in Yanites and Ehlers, 2012, 2016 EPSL, and Eizenhoefer et al., 2019 JGR-ES.
! If you published results from this version of the program we would appreciate that you reference:
! 1. Publications listed by J. Braun in the cascade.f90 file.
! 2. Yanites, B.J., and Ehlers, T.A., 2012, Global climate and tectonic controls on the denudation of glaciated mountains, Earth and Planetary Science Letters, v. 325-326, pp. 63-75. doi.org/10.1016/j.epsl.2012.01.030.
! 3. Yanites, B.J., and Ehlers, T.A., 2016, Intermittent glacial sliding velocities explain variations in long-timescale denudation, SW British Columbia. Earth and Planetary Science Letters, 450, pp. 52-61. 
! 3. Eizenhoefer, P.R., McQuarrie, N., Shelef, E., and Ehlers, T.A., 2019 (in press), Fluvial responses to horizontal displacements in convergent orogens over geologic time scales, Journal of Geophysical Research - Earth Surface.

! Please direct all inquiries concerning this fork of the source code to Todd Ehlers (Univ. Tuebingen, Germany) tehlers@icloud.com

! Module that contains all subroutines that are required to implement 2DMOVE input in IceCascade consisting of:

! 1. A sequence of exported MOVE grids in format [x y z ColorID PointID]
! 2. A sequence of exported MOVE topographies in format [x y z MOVEID]
! 3. A summary file (format [Time MOVEGrid MOVETopo]) as is used in Pecube

! Written and modifications made from previous Cascade subroutines (e.g. tectonic_movement.f90; tectonic_uplift.f90) 
! by PRE2017; peizen@pitt.edu

module m_MOVEtoCascade

    use rt_param
    use cascade_globals
    use m_initialize_nodal_geometry
    use m_check_mesh

    contains

!##################################################################################################################################!
!                              Subroutine that updates inital topography based on 2DMOVE topography
!##################################################################################################################################!
        subroutine MOVEtoCascadeTopo(configData)
            use rt_param
            use cascade_globals
            use m_initialize_nodal_geometry
        
            implicit none

            type(config) :: configData

            integer :: i, Lines, ios
            integer(4), dimension(:), allocatable :: MOVE_ID
            real(8), dimension(:), allocatable :: x_MOVE, y_MOVE, z_MOVE
            real(8) :: inputData
        
            print*, 'read MOVE file: input/IceCascade/', configData%meshname
        
            ! Count lines of MOVE input file and read variables
            call LengthofFile('input/IceCascade/'//configData%meshname, Lines)

            allocate(x_MOVE(Lines), y_MOVE(Lines), z_MOVE(Lines), MOVE_ID(Lines))
        
            open(55, file='input/IceCascade/'//configData%meshname, status='unknown')
                do i = 1,Lines
                    read(55, *, iostat=ios) x_MOVE(i), y_MOVE(i), z_MOVE(i), MOVE_ID(i)
                    if (ios/=0) exit
					! Performing y-flip switch for orographic precipitation to work properly
					if (configData%y_flip) then
						x_MOVE(i) = configData%sidey - x_MOVE(i)
					end if

                end do
            close(55)

            call initialize_nodal_geometry(configData)
 
            ! Modify elevations of nodes based on MOVE input; MOVE input is in 2D, z is extrapolated
            ! along the y-axis and some noise (+- 10 m) added (does noise have any effect here?!)
            do i = 1,configData%nnode
                !use h array slice h(i:i) instead of h(i), otherwise rank is incompatible
                h(i:i) = h(i:i) + z_MOVE(minloc(abs(y(i)-x_MOVE)))*1000.0_8 +&
                (ran(i) * 10.0_8)
            end do
        end subroutine MOVEtoCascadeTopo

!##################################################################################################################################!
!                      Subroutine that checks existence of velocity files and creates them if not available
!##################################################################################################################################!
        subroutine MOVEtoCascadeVelocityField(configData)
            implicit none

            type(config) :: configData

            real(8), dimension(:), allocatable :: InputTimeStep, TimeStep
            character(255), dimension(:), allocatable :: GridFiles
            integer(4) :: FileUnit1 = 83, FileUnit2 = 84, FileLines, i, j, l, m, k, &
                NumberOfGrids, ios, NumberOfVelocityFiles, PointID_loc(1)
            real(8), dimension(:), allocatable :: Grid_x1, Grid_x2, Grid_y1, Grid_y2, Grid_z1, Grid_z2, Vel_x, Vel_y, Vel_z
            integer(4), dimension(:), allocatable :: Grid_Color1, Grid_Color2, Grid_ID1, Grid_ID2
            character(255) :: FileName_List, FileName_Vel, FileName_Grid1, FileName_Grid2
            logical :: Test

            ! Regular grid input/output
            real(8), dimension(:), allocatable :: RegGrid_X, RegGrid_Z, RegGrid_Xvel, RegGrid_Zvel
            real(8) :: RegGrid_Resolution
            integer(4) :: RegGrid_Nodes
            integer(4), dimension(:), allocatable :: LocArray

            ! Read text file with list of MOVE point grids and their corresponding time in the format [time grid topography]
            FileName_List = trim('input/IceCascade/'//configData%MOVE_velocity_file)

                ! The input file must have the following format:

                ! first line: depth in y dimension in km
                ! second line: number of nodes in y dimension
                ! third line: number of samples to take for interpolation
                ! fourth line: step size in x direction for interpolation
                ! fifth line to end of file: time in Myrs and filename
                !
                ! example:
                !
                ! 10.0
                ! 5
                ! 10
                ! 1.0
                ! 50.0 file1.dat
                ! 25.0 file2.dat
                ! 12.56 file3.dat
                ! 7.1 file4.dat
                ! 1.8 file5.dat
                ! 0.0 file6.dat
                !
                ! The first four lines will be ignored here, but are used in Pecube and should be kept to ensure compatibility with
                ! Pecube

            call LengthOfFile(FileName_List, FileLines)

            open(FileUnit1, file=FileName_List, status='old')
                ! Skip first four lines
                do i = 1,4
                    read (FileUnit1, *)
                end do

                NumberOfGrids = FileLines - 4
                NumberOfVelocityFiles = NumberOfGrids - 1
                FileLines = 0

                ! Read time steps and input file names; reverse time line
                allocate (InputTimeStep(NumberOfGrids), GridFiles(NumberOfGrids), TimeStep(NumberOfGrids))

                do i = 1, NumberOfGrids
                    read(FileUnit1, *, iostat=ios), InputTimestep(i), GridFiles(i)

                    TimeStep(i) = (InputTimeStep(1) - InputTimeStep(i)) * 1e6_8

                    call ShortenString(GridFiles(i), 4)
                end do
            close(FileUnit1)

            ! Create velocity files
            do i = 1, NumberOfVelocityFiles
                FileName_Vel = trim('input/IceCascade/'//GridFiles(i))//'_vel_new.dat'

                ! Test whether file already exists
                inquire(file=FileName_Vel, exist=Test)

                if (Test) then
                    cycle
                end if

                ! Initial Grid
                FileName_Grid1 = 'input/IceCascade/'//trim(GridFiles(i))//'.dat'

                call LengthOfFile(FileName_Grid1, FileLines)

                allocate (Grid_x1(FileLines), Grid_y1(FileLines), Grid_z1(FileLines), Grid_Color1(FileLines), Grid_ID1(FileLines))

                call Read_MOVEGrid(FileName_Grid1, FileLines, Grid_y1, Grid_x1, Grid_z1, Grid_Color1, Grid_ID1)

				! Performing y-flip switch for orographic precipitation to work properly
                if (configData%y_flip) then
					do j=1, FileLines
						Grid_y1(j) = configData%sidey - Grid_y1(j)
					end do
                end if

                ! New/Deformed Grid
                FileName_Grid2 = 'input/IceCascade/'//trim(GridFiles(i+1))//'.dat'

                call LengthOfFile(FileName_Grid2, FileLines)

                allocate (Grid_x2(FileLines), Grid_y2(FileLines), Grid_z2(FileLines), Grid_Color2(FileLines), Grid_ID2(FileLines))

                call Read_MOVEGrid(FileName_Grid2, FileLines, Grid_y2, Grid_x2, Grid_z2, Grid_Color2, Grid_ID2)

				! Performing y-flip switch for orographic precipitation to work properly
                if (configData%y_flip) then
					do j=1, FileLines
						Grid_y2(j) = configData%sidey - Grid_y2(j)
					end do
                end if

                ! Create velocity files
                allocate (Vel_x(FileLines), Vel_y(FileLines), Vel_z(FileLines))

				print*, 'Writing velocity file for time step ', nint(TimeStep(i)), 'years ...'

				do j=1,FileLines
					PointID_loc = minloc(abs(Grid_ID1(j) - Grid_ID2))

					Vel_x(j) = (Grid_x2(PointID_loc(1)) - Grid_x1(j)) / ((TimeStep(i + 1) - TimeStep(i)) / 1e6_8)
					Vel_y(j) = (Grid_y2(PointID_loc(1)) - Grid_y1(j)) / ((TimeStep(i + 1) - TimeStep(i)) / 1e6_8)
					Vel_z(j) = (Grid_z2(PointID_loc(1)) - Grid_z1(j)) / ((TimeStep(i + 1) - TimeStep(i)) / 1e6_8)
				end do

            ! Create regular velocity grid
            ! Currently considering y=0 (e.g. x-z plane)
                open(FileUnit2, file = FileName_Vel, status='new')
                    write(FileUnit2, *), "REGULAR GRID # particle id,x,y,z,vx,vy,vz,color; units: km, mm/year"

                    RegGrid_Resolution = configData%sidey / configData%ny

                    call CreateGrid(0.0_8, configData%sidey, -5.0_8, 20.0_8, RegGrid_Resolution / 4.0_8, RegGrid_X, RegGrid_Z, &
                        RegGrid_Nodes)

                    allocate(RegGrid_Xvel(RegGrid_Nodes), RegGrid_Zvel(RegGrid_Nodes))

                    do k=1,RegGrid_Nodes
                        ! Find MOVE velocities within resolution range of regular grid point
                        allocate(LocArray(FileLines))
                        do l=1,FileLines
                            if ((Grid_y1(l) <= (RegGrid_X(k) + RegGrid_Resolution)) .and. &
                                (Grid_y1(l) > (RegGrid_X(k) - RegGrid_Resolution)) .and. &
                                (Grid_z1(l) <= (RegGrid_Z(k) + RegGrid_Resolution)) .and. &
                                (Grid_z1(l) > (RegGrid_Z(k) - RegGrid_Resolution))) then
                                LocArray(l) = l
                            else
                                LocArray(l) = 0.0_8
							end if
                        end do
                        LocArray = pack(LocArray, LocArray /= 0.0_8 .and. LocArray < FileLines)

						! Calculate average vertical and horizontal velocities of found record.
						! If record is empty, set velocities to zero.
                        if (size(LocArray) == 0) then
                            RegGrid_Xvel(k) = 0.0_8
                            RegGrid_Zvel(k) = 0.0_8
                            allocate(LocArray(1))
                        else
							RegGrid_Xvel(k) = sum(Vel_y(LocArray)) / size(LocArray)
                            RegGrid_Zvel(k) = sum(Vel_z(LocArray)) / size(LocArray)
                        end if

                        ! Write output to file
                        write(FileUnit2,*), k,  0.0_8, RegGrid_X(k), RegGrid_Z(k), 0.0_8, RegGrid_Xvel(k), RegGrid_Zvel(k), 7
                        deallocate(LocArray)
                    end do

					deallocate(Grid_x1, Grid_x2, Grid_y1, Grid_y2, Grid_z1, Grid_z2, Grid_Color1, Grid_Color2, Grid_ID1, Grid_ID2, &
                    Vel_x, Vel_y, Vel_z, RegGrid_X, RegGrid_Z, RegGrid_Xvel, RegGrid_Zvel)
                close(FileUnit2)

				print*, 'DONE!'
			end do
        end subroutine MOVEtoCascadeVelocityField

!##################################################################################################################################!
!            Subroutine that uplifts nodes based on a MOVE velocity grid created in subroutine MOVEtoCascadeVelocityField
!##################################################################################################################################!
		subroutine MOVEtoCascadeUplift(configData)

			use rt_param
			use cascade_globals
			use m_MOVEtoCascadeVelocityField

			implicit none

			type(config) :: configData

			integer(4) :: inode, FileUnit1 = 56, NumberOfGrids, NumberOfVelocityFiles, Lines = 0, ios, j, i, Loc1(1), Loc2(1)
			integer(4), allocatable :: LocArray1(:),LocArray2(:), index1(:), index2(:), MOVE_ID(:), MOVE_Color(:)

			real(8) :: inputData, dh
			real(8), allocatable :: x_MOVEv(:), y_MOVEv(:), z_MOVEv(:), x_vel(:), y_vel(:), z_vel(:), &
				z_MOVEv_temp(:), temp_x(:), temp_y(:), temp_z(:), InputTimeStep(:), TimeStep(:), location_x(:), location_y(:), &
				location_z(:), x_MOVETopo(:), y_MOVETopo(:), z_MOVETopo(:), MOVELine_ID(:)

			character(255), dimension(:), allocatable :: GridFiles
			character(255) :: FileName1, FileName2, FileName3, FileName4

			logical :: Test

			! Select correct active MOVE velocity file
			if (configData%uplift_mode == 3) then ! Reading from an input list of grids and selecting the appropriate velocity file
				FileName1 ='input/IceCascade/'//configData%MOVE_velocity_file

				call LengthOfFile(FileName1, Lines)

				open(FileUnit1, file=FileName1, status='old')
					! Skip first four lines
					do i = 1,4
						read (FileUnit1, *)
					end do

					NumberOfGrids = Lines - 4
					NumberOfVelocityFiles = NumberOfGrids - 1

					! Read time steps and input file names; reverse time line
					allocate (InputTimeStep(NumberOfGrids), GridFiles(NumberOfGrids), TimeStep(NumberOfGrids))

					do i = 1, NumberOfGrids
						read(FileUnit1, *, iostat=ios), InputTimestep(i), GridFiles(i)
						TimeStep(i) = (InputTimeStep(1) - InputTimeStep(i)) * 1e6_8

						call ShortenString(GridFiles(i), 4)
					end do

					Lines = 0
				close(FileUnit1)

				do i = 1,NumberOfGrids
					if (TimeStep(i) >= time .and. i > 1) then
						! Velocities over entire sections
						FileName2 = 'input/IceCascade/'//trim(GridFiles(i-1))//'_vel_new.dat'
						! Velocities over surface of entire section
						FileName3 = 'input/IceCascade/'//trim(GridFiles(i-1))//'_vel_new_surface.dat'
						exit
					else if (i == 1) then
						FileName2 = 'input/IceCascade/'//trim(GridFiles(i))//'_vel_new.dat'
						FileName3 = 'input/IceCascade/'//trim(GridFiles(i))//'_vel_new_surface.dat'
					end if
				end do
			else if (configData%uplift_mode == 2) then ! Reading a single velocity file
				FileName2 = trim('input/IceCascade/'//configData%MOVE_velocity_file)
			end if

			! Read MOVE velocity file and corresponding MOVE topography
			! Velocity File
			call LengthOfFile(FileName2, Lines)

			allocate(MOVE_ID(Lines), MOVE_Color(Lines), &
					x_MOVEv(Lines), y_MOVEv(Lines), z_MOVEv(Lines), x_vel(Lines), y_vel(Lines), z_vel(Lines))

			call Read_Velocities(FileName2, Lines, MOVE_ID, x_MOVEv,  y_MOVEv, z_MOVEv, &
					x_vel,  y_vel,  z_vel, MOVE_Color)

		!            ! Topography File
		!            call LengthofFile(FileName4, Lines)
		!
		!            open(FileUnit1, file=FileName4, status='old')
		!
		!                allocate(x_MOVETopo(Lines), y_MOVETopo(Lines), z_MOVETopo(Lines), MOVELine_ID(Lines))
		!
		!                do j = 1,Lines
		!                    read(FileUnit1, *, iostat=ios) x_MOVETopo(j), y_MOVETopo(j), z_MOVETopo(j), MOVELine_ID(j)
		!                        if (ios/=0) then
		!                            exit
		!                        end if
		!                end do
		!
		!            close(FileUnit1)

		! Uplift the topography
			allocate(temp_x(configData%nnode), temp_y(configData%nnode), temp_z(configData%nnode), index1(configData%nnode), &
				location_x(configData%nnode), location_y(configData%nnode), location_z(configData%nnode))

			inquire(file=FileName3, exist=Test)
			if (Test .and. (remeshflag == 0)) then
				open(FileUnit1, file=FileName3, status='old')
					! Skip header
					do i = 1,1
						read (FileUnit1, *)
					end do

					do i = 1,configData%nnode
						read(FileUnit1, *, iostat=ios), index1(i), location_x(i), location_y(i), location_z(i), &
						temp_x(i), temp_y(i), temp_z(i)

						if (ios/=0) then
							exit
						end if
					end do
				close(FileUnit1)
			else
				print*, 'Updating surface velocities and saving them in ', trim(FileName3), ' ...'
				remeshflag = 0

				open(FileUnit1, file=FileName3, status='replace')

					write(FileUnit1, *), '# node, node_x, node_y, node_z, node_vx, node_vy, node_vz; units: km, mm/year'

					do inode=1,configData%nnode
						allocate(LocArray1(Lines))
						do i = 1,Lines
							if ((y_MOVEv(i) <= (y(inode) + delta)) .and. &
								(y_MOVEv(i) > (y(inode) - delta)) .and. &
								(z_MOVEv(i) <= (h(inode) / 1e3_8 + delta)) .and. &
								(z_MOVEv(i) > (h(inode) / 1e3_8 - delta))) then
								LocArray1(i) = i
							else
								LocArray1(i) = 0.0_8
							end if
						end do
						LocArray1 = pack(LocArray1, LocArray1 /= 0.0_8 .and. LocArray1 < Lines)
						if (size(LocArray1) == 0) then
							temp_x(inode) = 0.0_8
							temp_y(inode) = 0.0_8
							temp_z(inode) = 0.0_8
							allocate(LocArray1(1))
						else
							temp_x(inode) = 0.0_8 * 1e-6_8
							temp_y(inode) = - (sum(y_vel(LocArray1)) / size(LocArray1)) * 1e-6_8
							temp_z(inode) = sum(z_vel(LocArray1)) / size(LocArray1) * 1e-3_8
						end if

						deallocate(LocArray1)

						write(FileUnit1, *), inode, x(inode), y(inode), h(inode), temp_x(inode), temp_y(inode), temp_z(inode)
					end do
				close(FileUnit1)

				print*, 'DONE!'
			end if

			print*, 'Vertical velocities from: ', trim(FileName3)

			do inode=1,configData%nnode
				dh = temp_z(inode) * global_cascade_dt * shelf(inode)

				h(inode) = h(inode) + (dh*shelf(inode))

				! Keeps topography above zeros (and might prevent 'Can't find sill' error)
				if (h(inode) < 0) then
					h(inode) = 0.0_8 + (ran(i) * 10.0_8)
				end if

				! Do not touch the following lines
				! They update h0, hi and calculate the influx of material into the landscape
				h0(inode)=h0(inode)+(dh)

				hi(inode)=hi(inode)+(dh)

				influx=influx+dh*memory(inode,7)
			 end do

			 return
		end subroutine MOVEtoCascadeUplift

!##################################################################################################################################!
!   Subroutine that adds horizontal movement component based on velocity file calculated in subroutine MOVEtoCascadeVelocityField
!##################################################################################################################################!
		! This subroutine implements spatially variable displacement extracted from MOVE, PRE Jan2017
		subroutine MOVEtoCascadeHmove(configData)
				use rt_param
				use cascade_globals
				use m_debug
				use m_find_surface
				use m_del_flip
				use m_MOVEtoCascadeVelocityField
				use m_check_mesh

				implicit none

				integer :: Lines = 0, ios, k
				integer(4), allocatable :: LocArray(:), index1(:), MOVE_ID(:), MOVE_Color(:)

				type(config) :: configData
				integer(4) :: inode1, inode2, FileUnit1 = 56, NumberOfGrids, NumberOfVelocityFiles
				real(8) :: inputData
				real(8), allocatable :: x_MOVEv(:), y_MOVEv(:), z_MOVEv(:), x_vel(:), y_vel(:), z_vel(:), &
					z_MOVEv_temp(:), InputTimeStep(:), TimeStep(:), location_x(:), location_y(:), location_z(:), x_MOVETopo(:), &
					y_MOVETopo(:), z_MOVETopo(:), MOVELine_ID(:)
				real(8), allocatable :: temp_x(:), temp_y(:), temp_z(:)

				integer(4) :: i, i0, i1, i2, i3, ie, iess, it
				integer(4) :: ja, jess, jess2, j, jb, nswaps
				integer(4), dimension(:,:), allocatable :: nn3

				character(255) :: FileName1, FileName2, FileName3, FileName4
				character(255), dimension(:), allocatable :: GridFiles

				allocate(nn3(nbmax,configData%nnode))

				! Select correct active MOVE velocity file
				! Reading from an input list of grids and selecting the appropriate velocity file
				if (configData%uplift_mode == 3) then
					FileName1 ='input/IceCascade/'//configData%MOVE_velocity_file

					call LengthOfFile(FileName1, Lines)

					open(FileUnit1, file=FileName1, status='old')
						! Skip first four lines
						do i = 1,4
							read (FileUnit1, *)
						end do

						NumberOfGrids = Lines - 4
						NumberOfVelocityFiles = NumberOfGrids - 1

						! Read time steps and input file names; reverse time line
						allocate (InputTimeStep(NumberOfGrids), GridFiles(NumberOfGrids), TimeStep(NumberOfGrids))

						do i = 1, NumberOfGrids
							read(FileUnit1, *, iostat=ios), InputTimestep(i), GridFiles(i)
							TimeStep(i) = (InputTimeStep(1) - InputTimeStep(i)) * 1e6_8

							call ShortenString(GridFiles(i), 4)
						end do

						Lines = 0
					close(FileUnit1)

					do i = 1,NumberOfGrids
						if (TimeStep(i) >= time .and. i > 1) then
							FileName2 = 'input/IceCascade/'//trim(GridFiles(i-1))//'_vel_new.dat'
							FileName3 = 'input/IceCascade/'//trim(GridFiles(i-1))//'_vel_new_surface.dat'
							exit
						elseif (i == 1) then
							FileName2 = 'input/IceCascade/'//trim(GridFiles(i))//'_vel_new.dat'
							FileName3 = 'input/IceCascade/'//trim(GridFiles(i))//'_vel_new_surface.dat'
						end if
					end do
				! Reading a single velocity file
				else if (configData%uplift_mode == 2) then
					FileName2 = 'input/IceCascade/'//configData%MOVE_velocity_file
				end if

				! Read MOVE velocity (and topography files???)
				! Velocity file
				call LengthOfFile(FileName2, Lines)

				allocate(MOVE_ID(Lines), MOVE_Color(Lines), &
					x_MOVEv(Lines), y_MOVEv(Lines), z_MOVEv(Lines), x_vel(Lines), y_vel(Lines), z_vel(Lines))

				call Read_Velocities(FileName2, Lines, MOVE_ID, x_MOVEv,  y_MOVEv, z_MOVEv, &
								x_vel,  y_vel,  z_vel, MOVE_Color)

		!        ! Topography file
		!        call LengthofFile(FileName4, Lines)
		!
		!            open(FileUnit1, file=FileName4, status='old')
		!
		!                allocate(x_MOVETopo(Lines), y_MOVETopo(Lines), z_MOVETopo(Lines), MOVELine_ID(Lines))
		!
		!                do j = 1,Lines
		!                    read(FileUnit1, *, iostat=ios) x_MOVETopo(j), y_MOVETopo(j), z_MOVETopo(j), MOVELine_ID(j)
		!                        if (ios/=0) then
		!                            exit
		!                        end if
		!                end do
		!
		!            close(FileUnit1)

				allocate(temp_x(configData%nnode), temp_y(configData%nnode), temp_z(configData%nnode), index1(configData%nnode), &
				location_x(configData%nnode), location_y(configData%nnode), location_z(configData%nnode))

				open(FileUnit1, file=FileName3, status='old')
					! Skip header
					do i = 1,1
						read (FileUnit1, *)
					end do

					do i = 1,configData%nnode
						read(FileUnit1, *, iostat=ios), index1(i), location_x(i), location_y(i), location_z(i), &
						temp_x(i), temp_y(i), temp_z(i)

						if (ios/=0) then
							exit
						end if
					end do
				close(FileUnit1)
				print*, 'Horizontal velocities from: ', trim(FileName3)

				! Applying spatially variable horizontal displacement
				! This applies a 2-delta buffer zone (~1 km on a 0.5 km * 0.5 km mesh) along the boundaries where displacement rates
				! will decline towards zero. Width of buffer zone might need to be changed. PRE Mar2018
				do i=1,configData%nnode
					! Moving nodes along y-axis
					if (y(i) .lt. (2 * delta)) then
						y(i) = y(i) - temp_y(i) * (y(i) / (2 * delta)) * global_cascade_dt
					else if (y(i) .gt. (configData%sidey - (2 * delta))) then
						y(i) = y(i) - temp_y(i) * ((configData%sidey - y(i)) / (2 * delta)) * global_cascade_dt
					else
						y(i) = y(i) - temp_y(i) * global_cascade_dt
					end if
					! Moving nodes along x-axis
					if (x(i) .lt. (2 * delta)) then
						x(i) = x(i) - temp_x(i) * (x(i) / (2 * delta)) * global_cascade_dt
					else if (x(i) .gt. (configData%sidex - (2 * delta))) then
						x(i) = x(i) - temp_x(i) * ((configData%sidex - x(i)) / (2 * delta)) * global_cascade_dt
					else
						x(i) = x(i) - temp_x(i) * global_cascade_dt
					end if
				enddo

				! perform a check to see if mesh needs to be updated DS 11/27/1
				!  remember endif statement at end if commenting out
				!
				if ((configData%advec_vely*tcheck.gt.delta/10.0_8).or. &
				  (configData%advec_velx*tcheck.gt.delta/10.0_8)) then

				! reset tcheck
				tcheck = 0.0_8

				! from here on you should not change anything...
				do i=1,configData%nnode
				points(1,i)=x(i)
				points(2,i)=y(i)
				enddo

				do i=1,configData%nnode
				memory(i, 6)=0.0_8
				enddo

				if (ivocal) call debug ('del_flip$')
				call del_flip (points,neighbour,vertices,nt,mask,mask_e,nswaps)
				do it=1,nt
					if (mask(it)) then
						memory(vertices(1,it), 6)=1.0_8
						memory(vertices(2,it), 6)=1.0_8
						memory(vertices(3,it), 6)=1.0_8
					endif
				enddo
				if (ivocal) call debug ('tectonic_movement$')

				! if there were any swapping the natural neighbours and surfaces need to be
				! recalculated...

				if (nswaps.ne.0) then

				do i=1,configData%nnode
					nb(i)  = 0
					nb2(i) = 0
				enddo

				!
				! find nn,nb, nb2,nn2,cell
				!
				do it=1,nt
					i1=vertices(1,it)
					i2=vertices(2,it)
					i3=vertices(3,it)

				! i1
					nb(i1)=nb(i1)+1
					nn(nb(i1),i1)=i2
					if (nb(i1).gt.nbmax) then
						print *, "nbmax: ", nbmax, ", nb: ", nb(i1)
						stop 'nbmax too small...1'
					endif

					nb2(i1)=nb2(i1)+1
					nn2(nb2(i1),i1)=i2
					cell(i1,int((1.+real(nb2(i1)))/2.),1)=i2

					nb2(i1)=nb2(i1)+1
					nn2(nb2(i1),i1)=i3
					cell(i1,int((1.+real(nb2(i1)))/2.),2)=i3
					if (nb2(i1).gt.nbmax) then
						print *, "nbmax: ", nbmax, ", nb2: ", nb2(i1)
						stop 'nbmax too small...2'
					endif

				! i2
					nb(i2)=nb(i2)+1
					nn(nb(i2),i2)=i3
					if (nb(i2).gt.nbmax) then
						print *, "nbmax: ", nbmax, ", nb: ", nb(i2)
						stop 'nbmax too small...3'
					endif

					nb2(i2)=nb2(i2)+1
					nn2(nb2(i2),i2)=i3
					cell(i2,int((1.+real(nb2(i2)))/2.),1)=i1

					nb2(i2)=nb2(i2)+1
					nn2(nb2(i2),i2)=i1
					cell(i2,int((1.+real(nb2(i2)))/2.),2)=i3
					if (nb2(i2).gt.nbmax) then
						print *, "nbmax: ", nbmax, ", nb2: ", nb2(i2)
						stop 'nbmax too small...4'
					endif

				! i3
					nb(i3)=nb(i3)+1
					nn(nb(i3),i3)=i1
					if (nb(i3).gt.nbmax) then
						print *, "nbmax: ", nbmax, ", nb: ", nb(i3)
						stop 'nbmax too small...5'
					endif

					nb2(i3)=nb2(i3)+1
					nn2(nb2(i3),i3)=i1
					cell(i3,int((1.+real(nb2(i3)))/2.),1)=i1

					nb2(i3)=nb2(i3)+1
					nn2(nb2(i3),i3)=i2
					cell(i3,int((1.+real(nb2(i3)))/2.),2)=i2
					if (nb2(i3).gt.nbmax) then
						print *, "nbmax: ", nbmax, ", nb2: ", nb2(i3)
						stop 'nbmax too small...6'
					endif

				enddo

				!
				! added directly from find_neighbours.f
				!
				do i=1,configData%nnode

				! classement de nn2 de i le plus gd au plus pt
				 do ie=1,nb2(i)-1
				  ja=nn2(ie,i)
				  jb=ie
				  do jess=ie+1,nb2(i)
				   jess2=nn2(jess,i)
				   if(ja.lt.jess2) then
					ja=jess2
					jb=jess
				   endif
				  enddo
				  nn2(jb,i)=nn2(ie,i)
				  nn2(ie,i)=ja
				 enddo

				! elimination des pts doubles de enl
				 i0=1
				 nn3(1,i)=nn2(1,i)
				 do iess=2,nb2(i)-1
				  if(nn2(iess,i).ne.nn3(i0,i)) then
				   nn3(i0+1,i)=nn2(iess,i)
				   i0=i0+1
				  endif
				 enddo
				 do j=1,i0
				  nn2(j,i)=nn3(j,i)
				 enddo
				 nb2(i)=i0
				enddo
				!
				! back to regular code
				!

				if (ivocal) call debug ('find_surface$')
				call find_surface (configData)
				!      call find_surface (nn,nb,surface,nbmax,nnode,x,y,xy,pp,aa,bb,newsurface,surfscale)
				if (ivocal) call debug ('tectonic_movement$')

				do i=1,configData%nnode
				nb(i)=nb(i)+1
				  if (nb(i).gt.nbmax) then
					print *, "nbmax: ", nbmax, ", nb: ", nb(i)
					stop 'nbmax too small...7'
				  endif
				nn(nb(i),i)=i
				enddo

				endif

				endif

				deallocate(nn3)

				return
			end subroutine MOVEtoCascadeHmove

!##################################################################################################################################!
!                                                   SUPPLEMENTAL SUBROUTINES
!##################################################################################################################################!
			! Subroutine that determines the length of a file
			subroutine LengthOfFile(FileName, LineCounter)
				implicit none

				character(255), intent(in) :: FileName
				integer(4) :: FileUnit = 11, ios
				integer(4), intent(out) :: LineCounter

				open(FileUnit, file=FileName, status='old')

					LineCounter = 0

					do
						read(FileUnit, *, iostat=ios)
						if (ios/=0) then
							exit
						end if
						LineCounter = LineCounter + 1
					end do
				close(FileUnit)
			end subroutine LengthOfFile

			! Subroutine that reads in MOVE grids
			subroutine Read_MOVEGrid(FileName, Lines, x, y, z, Color, PointID)
				implicit none

				character(255), intent(in) :: FileName
				integer(4) :: FileUnit = 12, ios, i
				integer(4), intent(in) :: Lines
				real(8), dimension(:), intent(out) :: x, y, z
				integer(4), dimension(:), intent(out) :: Color, PointID

				open(FileUnit, file=FileName, status='old')
					do i = 1,Lines
						read(FileUnit, *, iostat=ios), x(i), y(i), z(i), Color(i), PointID(i)
					end do
				close(FileUnit)
			end subroutine Read_MOVEGrid

			! Shortens a given string by an integer number of
			subroutine ShortenString(FileName, ReduceBy)
				implicit none

				character(255), intent(out) :: FileName
				integer(4), intent(in) :: ReduceBy
				integer(4) :: FileName_StringLength

				FileName_StringLength = len_trim(FileName)

				if (FileName_StringLength < ReduceBy) then
					print*, 'Subroutine ShortenString: String is too short to be shortened by ', ReduceBy, ' characters.'
				else
					FileName = FileName(1:(FileName_StringLength - ReduceBy))
				end if
			end subroutine ShortenString

			! Reads a velocity file
			subroutine Read_Velocities(FileName, Lines, MOVE_ID, loc_x, loc_y, loc_z, vel_x, vel_y, vel_z, MOVE_ColorID)
				implicit none

				character(255), intent(in) :: FileName
				integer(4) :: FileUnit = 13, ios, j
				integer, intent(in) :: Lines
				real(8), dimension(:), intent(out) :: loc_x, loc_y, loc_z, vel_x, vel_y, vel_z
				integer(4), dimension(:), intent(out) :: MOVE_ID, MOVE_ColorID

				open(FileUnit, file=FileName, status='old')

				read(FileUnit,*)

					do j = 1,Lines
						read(FileUnit, *, iostat=ios),  MOVE_ID(j), loc_x(j),  loc_y(j), loc_z(j), &
							vel_x(j),  vel_y(j),  vel_z(j), MOVE_ColorID(j)
						if (ios/=0) then
							exit
						end if
					end do

				close(FileUnit)
			end subroutine Read_Velocities

			! Creates regular grid
			subroutine CreateGrid(XRangeLower, XRangeUpper, YRangeLower, YRangeUpper, Resolution, NodeLocationX, NodeLocationY, &
				TotalNodes)
				implicit none

				real(8), intent(in) :: XRangeUpper, XRangeLower, YRangeUpper, YRangeLower, Resolution
				real(8), dimension(:), allocatable, intent(out) :: NodeLocationX, NodeLocationY
				integer(4) :: i, j, k, XNodes, YNodes
				integer(4), intent(out) :: TotalNodes

				XNodes = nint((XRangeUpper-XRangeLower) / Resolution)
				YNodes = nint((YRangeUpper-YRangeLower) / Resolution)
				TotalNodes = XNodes * YNodes

				allocate(NodeLocationX(TotalNodes), NodeLocationY(TotalNodes))

				k = 1
				do i=1,XNodes
					do j=1,YNodes
						NodeLocationX(k) = (i-1) * Resolution + XRangeLower
						NodeLocationY(k) = (j-1) * Resolution + YRangeLower
						k = k + 1
					end do
				end do
			end subroutine
end
