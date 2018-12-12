! Written by PRE Jan2017; peizen@pitt.edu
! Uses MOVE velocity files as input; modifies original tectonic_uplift module   
 

module m_MOVEtoCascadeUplift
contains 
    subroutine MOVEtoCascadeUplift(configData)

        use rt_param
        use cascade_globals
        use m_MOVEtoCascadeVelocityField
        
        implicit none    

        type(config) :: configData

        integer(4) :: inode, FileUnit1 = 56, NumberOfGrids, NumberOfVelocityFiles, Lines = 0, ios, j, i, Loc(1)
        integer(4), allocatable :: LocArray(:), index1(:), index2(:)

        real(8) :: inputData, dh
        real(8), allocatable :: x_MOVEv(:), y_MOVEv(:), z_MOVEv(:), x_vel(:), y_vel(:), z_vel(:), MOVE_ID(:), MOVE_Color(:), &
            z_MOVEv_temp(:), temp_x(:), temp_y(:), temp_z(:), InputTimeStep(:), TimeStep(:), location_x(:), location_y(:), &
            location_z(:), x_MOVETopo(:), y_MOVETopo(:), z_MOVETopo(:), MOVELine_ID(:)

        character(255), dimension(:), allocatable :: GridFiles, TopoFiles
        character(255) :: FileName1, FileName2, FileName3, FileName4

        logical :: Test

        real(8) :: x_avg_min, x_avg_max, GridResolution_x, Cumulative_h
        integer(4) :: Counter

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
                    allocate (InputTimeStep(NumberOfGrids), GridFiles(NumberOfGrids), TimeStep(NumberOfGrids), &
                        TopoFiles(NumberOfGrids))

                    do i = 1, NumberOfGrids
                        read(FileUnit1, *, iostat=ios), InputTimestep(i), GridFiles(i), TopoFiles(i)
                        TimeStep(i) = (InputTimeStep(1) - InputTimeStep(i)) * 1e6_8
                    end do

                    Lines = 0

            close(FileUnit1)

            do i = 1,NumberOfGrids

                if (TimeStep(i) >= time .and. i > 1) then
                    ! Velocities over entire sections
                    FileName2 = 'input/IceCascade/'//trim(GridFiles(i-1))//'_vel_new.dat'
                    ! Velocities over surface of entire section
                    FileName3 = 'input/IceCascade/'//trim(GridFiles(i-1))//'_vel_new_temp.dat'
                    ! Associated MOVE topography
                    FileName4 = 'input/IceCascade/'//trim(Topofiles(i-1))
                    exit
                elseif (i == 1) then
                    FileName2 = 'input/IceCascade/'//trim(GridFiles(i))//'_vel_new.dat'
                    FileName3 = 'input/IceCascade/'//trim(GridFiles(i))//'_vel_new_temp.dat'
                    FileName4 = 'input/IceCascade/'//trim(Topofiles(i))
                end if

            end do

        else if (configData%uplift_mode == 2) then ! Reading a single velocity file

            FileName2 = trim('input/IceCascade/'//configData%MOVE_velocity_file)

        end if

        print*, 'Reading MOVE velocity file ', FileName2, ' for vertical movement component...'

        ! Read MOVE velocity file and corresponding MOVE topography

            ! Velocity File
            call LengthOfFile(FileName2, Lines)

            open(FileUnit1, file=FileName2, status='old')

                allocate(MOVE_ID(Lines), MOVE_Color(Lines), &
                    x_MOVEv(Lines), y_MOVEv(Lines), z_MOVEv(Lines), x_vel(Lines), y_vel(Lines), z_vel(Lines))

                read(FileUnit1,*)

                    do j = 1,Lines
                        read(FileUnit1, *, iostat=ios) MOVE_ID(j), x_MOVEv(j), y_MOVEv(j), z_MOVEv(j), &
                            x_vel(j),  y_vel(j),  z_vel(j), MOVE_Color(j)
                        if (ios/=0) then
                            exit
                        end if
                    end do

                Lines = 0

            close(FileUnit1)

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

        print*, 'DONE!'

! Uplift the topography keeping the boundary pinned
        allocate(temp_x(configData%nnode), temp_y(configData%nnode), temp_z(configData%nnode), index1(configData%nnode), &
            index2(configData%nnode), location_x(configData%nnode), location_y(configData%nnode), location_z(configData%nnode))

        call LengthOfFile(FileName2, Lines)

        inquire(file=FileName3, exist=Test)

        if (Test) then

            print*, FileName3, 'already exists, reading velocities...'

            open(FileUnit1, file=FileName3, status='old')

                ! Skip header
                do i = 1,1
                    read (FileUnit1, *)
                end do

                do i = 1,configData%nnode
                
                    read(FileUnit1, *, iostat=ios), index1(i), index2(i), &
                        location_x(i), location_y(i), location_z(i), temp_x(i), temp_y(i), temp_z(i)

                    if (ios/=0) then
                        exit
                    end if

                    GridResolution_x = configData%sidex / configData%nx

                    ! Assumes a mean topography for the boundaries along the x-axis
                    if (y(i) <= GridResolution_x .or. y(i) >= (configData%sidex - GridResolution_x)) then

!                        temp_x(i) = 0.0_8
!                        temp_y(i) = 0.0_8
!                        temp_z(i) = 0.0_8

                        ! Determine range of x-slice through which topography will be averaged

                        x_avg_min = x(i) - GridResolution_x
                        x_avg_max = x(i) + GridResolution_x

                        Counter = 0
                        Cumulative_h = 0

                        do j = 1,configData%nnode

                            if (x_avg_min <= x(j) .and. x(j) < x_avg_max) then

                                Counter = Counter + 1
                                Cumulative_h = Cumulative_h + h(i)

                            end if

                        end do

                        ! Assign average topography to border node
                        h(i:i) = Cumulative_h / Counter

                    end if

!                    ! Uses simplified critical taper MOVE topography file to constrain model boundaries
!                    if (x(i) == 0 .or. x(i) == configData%sidex .or. y(i) == 0 .or. y(i) == configData%sidey) then
!
!                        temp_x(i) = 0.0_8
!                        temp_y(i) = 0.0_8
!                        temp_z(i) = 0.0_8
!
!                        h(i:i) = z_MOVETopo(minloc(abs(x(i) - x_MOVETopo))) * 1000.0_8
!
!                    end if

                end do

            close(FileUnit1)

            print*, 'DONE!'

        else

            print*, 'Finding surface velocities and saving them in ', FileName3, '...'

            open(FileUnit1, file=FileName3, status='new')

                write(FileUnit1, *), '# node, MOVEGrid_index, node_x,node_y,node_z,node_vx,node_vy,node_vz; units: km, mm/year'

                    GridResolution_x = configData%sidex / configData%nx

                do inode=1,configData%nnode

                    allocate(LocArray(Lines))

                    ! Find all input entries that are in the resolution range of x(inode): +/- half the Cascade grid resolution (2D MOVE)
                    do i = 1,Lines

                        if ((x_MOVEv(i) - GridResolution_x) < x(inode) .and. x(inode) <= (x_MOVEv(i) + GridResolution_x)) then
                            LocArray(i) = i
                        else
                            LocArray(i) = 0.0_8
                        end if

!                        print*, 'ALL GOOD AT LEAST UNTIL HERE!'
!                        print*, 'Current node: ', inode
!                        print*, 'Line in velocity file: ', i
!                        print*, 'Index test: ', LocArray

                    end do

                    LocArray = pack(LocArray, LocArray /= 0.0_8 .and. LocArray < Lines) ! Index locations that correspond to x(inode) along the z-axis

                    allocate(z_MOVEv_Temp(size(LocArray)))

                    do i = 1,size(LocArray)

                        z_MOVEv_temp(i) = z_MOVEv(LocArray(i))

                    end do

                    Loc = LocArray(minloc(abs(h(inode)-z_MOVEv_Temp))) ! Index that is closest to current node elevation

                    deallocate (LocArray, z_MOVEv_Temp)

                    temp_x(inode) = -x_vel(Loc(1)) * 1e-6_8
                    temp_y(inode) = -y_vel(Loc(1)) * 1e-6_8
                    temp_z(inode) = z_vel(Loc(1)) * 1e-3_8

                    write(FileUnit1, *), inode, Loc(1), x(inode), y(inode), h(inode), temp_x(inode), temp_y(inode), temp_z(inode)

                end do

            close(FileUnit1)

            print*, 'DONE!'

        end if

        do inode=1,configData%nnode

!            if (memory(inode,5) > 0.5_8) then

                dh=temp_z(inode)*global_cascade_dt*shelf(inode)

! Uplift interior                  
                h(inode) = h(inode) + (dh*shelf(inode))
!            else
!
!                dh = 0.0_8
!
!            end if

! Do not touch the following lines
! They update h0, hi and calculate the influx of material into the landscape
! 
            h0(inode)=h0(inode)+(dh)

            hi(inode)=hi(inode)+(dh)

            influx=influx+dh*memory(inode,7)
 
         enddo
         
         return

     end subroutine MOVEtoCascadeUplift

end module m_MOVEtoCascadeUplift
