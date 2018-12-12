        module m_import_from_comsol
            contains
            subroutine import_from_comsol(nx, ny, grid_spacing, h_tmp, sliding, com_run)
            implicit none

            ! arguments:
            integer(4), intent(in) :: nx, ny, grid_spacing
            integer(4), intent(inout) :: com_run
            real(8), dimension(:,:), intent(inout) :: h_tmp, sliding

            ! local variables:
            integer(4) :: file_id, line_counter, io_status, i, j, k, x_index, y_index
            integer(4) :: sys_res ! x_min_index, x_max_index, y_min_index, y_max_index, index_radius these are never called
            integer(4) :: x_index_min, x_index_max, y_index_min, y_index_max
            integer(4) :: num_of_neighbours
            character(300) :: input_line
            real(8) :: x_min, y_min, x_max, y_max, x_pos, y_pos, dist
            real(8) :: radius, factor_sum, h_tmp_value, sliding_value
            real(8), dimension(:,:), allocatable :: h_tmp_old
            real(8), dimension(:,:), allocatable :: delh

            
            type :: comsol_data_type
                real(8) :: x, y, z, h_tmp, sliding
            end type comsol_data_type

            type(comsol_data_type), dimension(:), allocatable :: comsol_data
            type(comsol_data_type) :: data_line

            type :: node_inde_type
                real(8) :: distance, factor, h_tmp, sliding
            end type node_inde_type

            type(node_inde_type), dimension(:), allocatable :: node_info
            
            ! set the starting max and min (so that it actually builds from something instead of just starting each at 0)
            x_max = 0
            x_min = nx*grid_spacing
            y_max = 0
            y_min = ny*grid_spacing

            allocate(h_tmp_old(nx,ny))

            h_tmp_old = h_tmp;
            
            file_id = 100

            open(file_id, file="output/comsol/import_from_comsol.txt", status="old", iostat=io_status)

            if (io_status /= 0) then
                print *, "import_from_comsol.f90: could not open comsol file: output/IceCascade/import_from_comsol.txt"
                return
            endif

            line_counter = 0

            do
                read(file_id, *, iostat=io_status) input_line
                if (io_status < 0) then
                    exit ! end of file reached
                endif
                line_counter = line_counter + 1
            enddo

            print *, "import_from_comsol.f90: number of lines in cascade file: ", line_counter

            ! I need to make sure that this somewhere retains the time information, so I can better troubleshoot
            ! the comsol files. -RH
            if (line_counter < 3) then
                print *, "import_from_comsol.f90: Comsol may have crashed. Using values from update_height"
                sys_res =  system("date") ! This will print out the current date and time so this event can be logged
                close(file_id)
                com_run = 0
                return
            endif

            allocate(comsol_data(line_counter))

            rewind(file_id)

            do i=1,line_counter
                read (file_id, *) data_line

                !print *, "data_line: ", data_line

                comsol_data(i)%x = 0.0_8
                comsol_data(i)%y = 0.0_8
                comsol_data(i)%z = 0.0_8
                comsol_data(i)%sliding = 0.0_8
                comsol_data(i)%h_tmp = 0.0_8

                !print *, "comsol_data: ", comsol_data(i)

                comsol_data(i)%x = data_line%x
                comsol_data(i)%y = data_line%y
                comsol_data(i)%z = data_line%z
                comsol_data(i)%sliding = data_line%sliding
                comsol_data(i)%h_tmp = data_line%h_tmp

                if (x_min > data_line%x) then
                    x_min = data_line%x
                endif

                if (x_max < data_line%x) then
                    x_max = data_line%x
                endif

                if (y_min > data_line%y) then
                    y_min = data_line%y
                endif

                if (y_max < data_line%y) then
                    y_max = data_line%y
                endif
            enddo ! i=1,line_counter

            print *, "import_from_comsol.f90: x_min, x_max, y_min, y_max: ", x_min, x_max, y_min, y_max
            
            print *, 'Size of h_tmp', nx, ny, grid_spacing
            
            do i=1,line_counter,100
            print *, "sliding: ", comsol_data(i)%sliding
            enddo
            
            x_index_min = int(x_min / dble(grid_spacing)) + 1
            x_index_max = int(x_max / dble(grid_spacing)) + 1
            y_index_min = int(y_min / dble(grid_spacing)) + 1
            y_index_max = int(y_max / dble(grid_spacing)) + 1
            
            print *, 'Indices from COMSOL', x_index_min, x_index_max, y_index_min, y_index_max

            num_of_neighbours = 4
            radius = 10.0_8 ! this is the exponential fall of radius, needs to be set in the inputfile in the future
            
            !allocate(delh(x_index_max-x_index_min+1+2, y_index_max-y_index_min+1+2))
            allocate(delh(nx,ny))
            delh = 0
            allocate(node_info(num_of_neighbours))
! within this loop, I need to find out if the change to h_tmp is minimized (must choose some criteria)
            do x_index = x_index_min - 1, x_index_max + 1
                do y_index = y_index_min - 1, y_index_max + 1
                    x_pos = dble(x_index * grid_spacing)
                    y_pos = dble(y_index * grid_spacing)

                    ! for each point on the regular cascade grid we find the four nearest
                    ! neighbours on the iregular comsol grid and interpolate
                    
                    do i=1,num_of_neighbours
                        ! initialize values
                            node_info(i)%distance = huge(0.0_8)
                            node_info(i)%h_tmp = 0.0_8
                            node_info(i)%sliding = 0.0_8
                            node_info(i)%factor = 0.0_8
                    enddo ! i=1,num_of_neighbours
                   
                    do i=1,line_counter
                        ! distance from current comsol point to current grid point
                                          
                        dist = sqrt((x_pos - comsol_data(i)%x)**2 + (y_pos - comsol_data(i)%y)**2)
                    
                        ! look through all neighbours, if this point is closer
                        do j=1,num_of_neighbours
                            if (dist < node_info(j)%distance) then ! yes it is closer
                                ! move the other neighbours up in the list and though out the last one
                                do k=j+1,num_of_neighbours
                                    node_info(k) = node_info(k-1)
                                enddo
                                
                                ! this is the new nearest neighbour for position j
                                node_info(j)%distance = dist
                                node_info(j)%h_tmp = comsol_data(i)%h_tmp
                                node_info(j)%sliding = comsol_data(i)%sliding

                                exit ! from loop: do j
                            endif
                        enddo ! j=1,num_of_neighbours
                    enddo ! i=1,line_counter
                    
                    factor_sum = 0.0_8
                    h_tmp_value = 0.0_8
                    sliding_value = 0.0_8
                    do j=1,num_of_neighbours
                        node_info(j)%factor = exp(-node_info(j)%distance / radius) ! exponential fall off for the values
                        factor_sum = factor_sum + node_info(j)%factor
                        h_tmp_value = h_tmp_value + (node_info(j)%factor * node_info(j)%h_tmp)
                        sliding_value = sliding_value + (node_info(j)%factor * node_info(j)%sliding)
                    enddo ! j=1,num_of_neighbours
                    !print *, 'Line 182'
                    !h_tmp(x_index, y_index) = h_tmp_value / factor_sum
                    ! the last column in my output file is now delh and not h_tmp. 
                    ! I haven't renamed all values yet, however
                    delh(x_index, y_index) = h_tmp_value / factor_sum
                    
                    sliding(x_index, y_index) = (sliding_value / factor_sum)

                enddo ! y_index = y_index_min - 1, y_index_max + 1
            enddo ! x_index = x_index_min - 1, x_index_max + 1

            
            do i = 0, x_index_max-x_index_min
                do j = 0, y_index_max-y_index_min
            !        delh(i+1,j+1) = h_tmp(i+x_index_min, j+y_index_min)-h_tmp_old(i+x_index_min, j+y_index_min)
                     h_tmp(i+x_index_min, j+y_index_min) = h_tmp_old(i+x_index_min, j+y_index_min) + delh(i+1,j+1)
                enddo
            enddo
            print *, 'Minimizing Value', maxval(delh)
            !if (maxval(abs(delh)) <= 2) then
                com_run = 2
            !endif
            ! add code to find if the change in ice thickness is minimized (how much?)
            ! if this is true, set com_run = 2

            deallocate(h_tmp_old)
            deallocate(node_info)
            deallocate(comsol_data)
            deallocate(delh)

            close(file_id)
        end subroutine import_from_comsol
    end module m_import_from_comsol
