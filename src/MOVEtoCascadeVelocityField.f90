! This version of CASCADE is a fork from the original version published by Braun and Sambridge, 1997 Basin Research. The modifications made to the original version of CASCADE are described in Yanites and Ehlers, 2012, 2016 EPSL, and Eizenhoefer et al., 2019 JGR-ES.
! If you published results from this version of the program we would appreciate that you reference:
! 1. Publications listed by J. Braun in the cascade.f90 file.
! 2. Yanites, B.J., and Ehlers, T.A., 2012, Global climate and tectonic controls on the denudation of glaciated mountains, Earth and Planetary Science Letters, v. 325-326, pp. 63-75. doi.org/10.1016/j.epsl.2012.01.030.
! 3. Yanites, B.J., and Ehlers, T.A., 2016, Intermittent glacial sliding velocities explain variations in long-timescale denudation, SW British Columbia. Earth and Planetary Science Letters, 450, pp. 52-61. 
! 3. Eizenhoefer, P.R., McQuarrie, N., Shelef, E., and Ehlers, T.A., 2019 (in press), Fluvial responses to horizontal displacements in convergent orogens over geologic time scales, Journal of Geophysical Research - Earth Surface.

! Please direct all inquiries concerning this fork of the source code to Todd Ehlers (Univ. Tuebingen, Germany) tehlers@icloud.com

! Written by PRE Mar2017
! Module that reads a given list of exported MOVE point clouds and calculates a 2D velocity profile to be used in horizontal and
! vertical movement modules from the point clouds. There's an existing more elaborate module to calculate velocity grids written by
! WK implemented into Pecube which should replace this module in a future version to more efficiently integrate Cascade with Pecube

module m_MOVEtoCascadeVelocityField

    use rt_param
    use cascade_globals

    contains


        ! Main subroutine that checks existence of velocity files and creates them if not available
        subroutine MOVEtoCascadeVelocityField(configData)

            implicit none

            type(config) :: configData

            real(8), dimension(:), allocatable :: InputTimeStep, TimeStep
            character(255), dimension(:), allocatable :: GridFiles
            integer(4) :: FileUnit1 = 83, FileLines, i, j, NumberOfGrids, ios, NumberOfVelocityFiles, PointID_loc(1)
            real(8), dimension(:), allocatable :: Grid_x1, Grid_x2, Grid_y1, Grid_y2, Grid_z1, Grid_z2, Vel_x, Vel_y, Vel_z
            integer(4), dimension(:), allocatable :: Grid_Color1, Grid_Color2, Grid_ID1, Grid_ID2
            character(255) :: FileName
            logical :: Test

            ! Read text file with list of MOVE point grids and their corresponding time in the format [time grid]
            FileName = 'input/IceCascade/'//configData%MOVE_velocity_file

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

            call LengthOfFile(FileName, FileLines)

            open(FileUnit1, file='input/IceCascade/'//configData%MOVE_velocity_file, status='old')
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
                end do

            close(FileUnit1)

            ! Create velocity files
            do i = 1, NumberOfVelocityFiles

                ! Initial Grid
                FileName = 'input/IceCascade/'//GridFiles(i)

                call LengthOfFile(FileName, FileLines)

                allocate (Grid_x1(FileLines), Grid_y1(FileLines), Grid_z1(FileLines), Grid_Color1(FileLines), Grid_ID1(FileLines))

                call Read_MOVEGrid(FileName, FileLines, Grid_x1, Grid_y1, Grid_z1, Grid_Color1, Grid_ID1)

                ! New/Deformed Grid
                FileName = 'input/IceCascade/'//GridFiles(i+1)

                call LengthOfFile(FileName, FileLines)

                allocate (Grid_x2(FileLines), Grid_y2(FileLines), Grid_z2(FileLines), Grid_Color2(FileLines), Grid_ID2(FileLines))

                call Read_MOVEGrid(FileName, FileLines, Grid_x2, Grid_y2, Grid_z2, Grid_Color2, Grid_ID2)

                ! Create velocity files
                allocate (Vel_x(FileLines), Vel_y(FileLines), Vel_z(FileLines))

                FileName = trim('input/IceCascade/'//GridFiles(i))//'_vel_new.dat'

                ! Test whether file already exists
                inquire(file=FileName, exist=Test)

                if (Test) then
                    print*, 'File ', FileName, 'already exists... Skipping.'
                    deallocate(Grid_x1, Grid_x2, Grid_y1, Grid_y2, Grid_z1, Grid_z2, Grid_Color1, Grid_Color2, Grid_ID1, Grid_ID2, &
                    Vel_x, Vel_y, Vel_z)
                    cycle
                end if

                open(FileUnit1, file = FileName, status='new')

                    write(FileUnit1, *), "# particle id,x,y,z,vx,vy,vz,color; units: km, mm/year"

                    print*, 'Writing velocity file for time step...', TimeStep(i), 'years'

                do j=1,FileLines

                    PointID_loc = minloc(abs(Grid_ID1(j) - Grid_ID2))

                    Vel_x(j) = (Grid_x2(PointID_loc(1)) - Grid_x1(j)) / ((TimeStep(i + 1) - TimeStep(i)) / 1e6_8)
                    Vel_y(j) = (Grid_y2(PointID_loc(1)) - Grid_y1(j)) / ((TimeStep(i + 1) - TimeStep(i)) / 1e6_8)
                    Vel_z(j) = (Grid_z2(PointID_loc(1)) - Grid_z1(j)) / ((TimeStep(i + 1) - TimeStep(i)) / 1e6_8)

                    write(FileUnit1, *), Grid_ID1(j), Grid_x1(j), Grid_y1(j), Grid_z1(j), Vel_x(j), Vel_y(j), Vel_z(j), &
                        Grid_Color1(j)

                end do

                    print*, 'DONE!'

                close(FileUnit1)

                deallocate(Grid_x1, Grid_x2, Grid_y1, Grid_y2, Grid_z1, Grid_z2, Grid_Color1, Grid_Color2, Grid_ID1, Grid_ID2, &
                    Vel_x, Vel_y, Vel_z)

            end do

        end subroutine MOVEtoCascadeVelocityField


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
        subroutine Read_MOVEGrid(FileName, LineCounter, x, y, z, Color, PointID)

            implicit none

            character(255), intent(in) :: FileName
            integer(4) :: FileUnit = 12, ios, i
            integer(4), intent(in) :: LineCounter
            real(8), dimension(:), intent(out) :: x, y, z
            integer(4), dimension(:), intent(out) :: Color, PointID

            open(FileUnit, file=FileName, status='old')

                do i = 1,LineCounter
                    read(FileUnit, *, iostat=ios), x(i), y(i), z(i), Color(i), PointID(i)
                end do

            close(FileUnit)

        end subroutine Read_MOVEGrid

        SUBROUTINE FindInVector(n,TF,npos,pos)
            ! Inlet variables
            INTEGER,INTENT(IN):: n      ! Dimension of logical vector
            LOGICAL,INTENT(IN):: TF(n)  ! Logical vector (True or False)
            ! Outlet variables
            INTEGER npos                ! number of "true" conditions
            INTEGER pos(n)              ! position of "true" conditions
            ! Internal variables
            INTEGER i                   ! counter
            INTEGER v(n)                ! vector of all positions

            pos = 0                     ! Initialize pos
            FORALL(i=1:n)   v(i) = i    ! Enumerate all positions
            npos  = COUNT(TF)           ! Count the elements of TF that are .True.
            pos(1:npos)= pack(v, TF)    ! With Pack function, verify position of true conditions

        ENDSUBROUTINE FindInVector

end module
