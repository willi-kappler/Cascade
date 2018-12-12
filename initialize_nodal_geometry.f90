! initialize_nodal_geometry

        module m_initialize_nodal_geometry
            contains

            subroutine initialize_nodal_geometry(configData)
                use rt_param
                use cascade_globals
                use m_iread_but_skip_comment
                use m_read_but_skip_comment
                use m_debug

                implicit none



! subroutine to read in initial nodal information

! INPUT: nnodemax      = maximum number of nodes that can be read
!        run_name      = name of the subdirectory where the run output
!                        will be stored
!        nrun_name     = length of the character string run_name

! OUTPUT: nnode        = number of nodes read
!         x(nnode)     = x-location of nodes (in km)
!         y(nnode)     = y-location of nodes (in km)
!         h(nnode)     = height of nodes (in m)
!         fix(nnode)   = boundary condition (=0 means node height is fixed;
!                        =1 means node height is free)
!         delta        = mean nodal spacing (in km)
!         configData%surfscale    = mean nodal surface (a nodal surface is the surface
!                        of the part of the landscape associated with a node)
!                        (in km**2)
!         configData%surfmin
!         bdry         = flag to identify moving bdry nodes (added 11/28/1 DS)
!                        0: not on bdry, 1: on bdry


! subroutines called:
! - debug
! - random
! - iread_but_skip_comment
! - read_but_skip_comment,iceth(*)


            type(config) :: configData
            integer(4) :: nodeCounter

            real(8) :: stepX, stepY, topBottomNodes, leftRightNodes
            real(8) :: topArea, bottomArea, leftArea, rightArea, totalArea

!            allocate(fix(nnodemax))

            ! fix = memory(1:, 5)

          if (configData%nnode > nnodemax) then
              stop 'Too many nodes...'
          endif

          delta=configData%sidex/dble(configData%nx-1)
          configData%surfscale=configData%sidex*configData%sidey/dble(configData%nnode)
          configData%surfmin=configData%surfscale/4.0_8

! this is a bit of random noise put on the initial grid
! note that the noise is not used for the nodes along the boundary
!       print*,configData%nx,configData%ny,configData%sidex,configData%sidey
          if (ivocal) call debug ('random$')



! WK: work around for find_catchment:
! allow algorithm to find a sill.

!        do i=1, configData%nnode
!            h(i) = h(i) * 0.001
!        end do


            print *, "initialize_nodal_geometry: "
            print *, "nnode: ", configData%nnode
            print *, "nx, ny: ", configData%nx, configData%ny
            print *, "sidex, sidey: ", configData%sidex, configData%sidey


!      call random (iceh,nnode)
            if (ivocal) call debug ('initialize_nodal_geometry$')


! 2012.01.31, WK: This is the new version of the init code
! Here we use the additional nodes from cascade.f90, so that the node spacing can be different

            call random_number(h)

            if (configData%addshelf) then
                nodeCounter = 1

                ! fill inner area
                stepX = (configData%sidex - configData%Exl - configData%Exr) / (configData%nx - 1)
                stepY = (configData%sidey - configData%Eyu - configData%Eyd) / (configData%ny - 1)
                call fill_area(nodeCounter, configData%Exl, configData%Eyd, &
                               configData%sidex - configData%Exr, &
                               configData%sidey - configData%Eyu, stepX, stepY, configData)
                print *, "nodeCounter (shelf inner area): ", nodeCounter

                topArea = configData%sidex * configData%Eyu
                bottomArea = configData%sidex * configData%Eyd
                leftArea = (configData%sidey - configData%Eyu - configData%Eyd) * configData%Exl
                rightArea = (configData%sidey - configData%Eyu - configData%Eyd) * configData%Exr

                totalArea = topArea + bottomArea + leftArea + rightArea
                topBottomNodes = (dble(nextra) * (topArea + bottomArea)) / totalArea
                leftRightNodes = (dble(nextra) * (leftArea + rightArea)) / totalArea

                print *, "nextra, topBottomNodes, leftRightNodes: ", nextra, topBottomNodes, leftRightNodes
                print *, "total nodes: ", topBottomNodes + leftRightNodes

                if (topBottomNodes > 0.0_8) then
                    ! fill top shelf
                    stepX = sqrt((topArea + bottomArea) / topBottomNodes)
                    stepY = stepX

                    print *, "stepX, stepY: ", stepX, stepY

                    call fill_area(nodeCounter, 0.0_8, configData%sidey + (stepY / 2.0) - configData%Eyu, &
                                   configData%sidex, configData%sidey, stepX, stepY, configData)
                    print *, "nodeCounter (top shelf): ", nodeCounter

                    ! fill bottom shelf
                    call fill_area(nodeCounter, 0.0_8, 0.0_8, configData%sidex, &
                                   configData%Eyd - (stepY / 2.0_8), stepX, stepY, configData)
                    print *, "nodeCounter (bottom shelf): ", nodeCounter
                endif

                if (leftRightNodes > 0.0_8) then
                    ! fill left shelf
                    stepX = sqrt((leftArea + rightArea) / leftRightNodes)
                    stepY = stepX

                    print *, "stepX, stepY: ", stepX, stepY

                    call fill_area(nodeCounter, 0.0_8, configData%Eyd, configData%Exl, &
                                   configData%sidey - configData%Eyu, stepX, stepY, configData)
                    print *, "nodeCounter (left shelf): ", nodeCounter

                    ! fill right shelf
                    call fill_area(nodeCounter, configData%sidex - configData%Exr, configData%Eyd, &
                                   configData%sidex, configData%sidey - configData%Eyu, stepX, &
                                   stepY, configData)
                    print *, "nodeCounter (right shelf): ", nodeCounter
                endif

            else
                nodeCounter = 1
                stepX = configData%sidex / (configData%nx - 1)
                stepY = configData%sidey / (configData%ny - 1)
                call fill_area(nodeCounter, 0.0_8, 0.0_8, configData%sidex, configData%sidey, &
                               stepX, stepY, configData)
                print *, "nodeCounter (no shelf): ", nodeCounter
            endif

        configData%nnode = nodeCounter - 1
        print *, "new configData%nnode: ", configData%nnode

        if (configData%nnode < 3) then
            stop 'nnode too small...'
        endif

        if (delta.le.0.0_8) then
            stop 'delta must be greater than 0...'
        endif

        if (configData%surfscale.le.0.0_8) then
            stop 'configData%surfscale must be greater than 0...'
        endif

        if (configData%sidex.le.0.0_8) then
            stop 'configData%sidex must be greater than 0...'
        endif

        if (configData%sidey.le.0.0_8) then
            stop 'configData%sidey must be greater than 0...'
        endif

                ! copy modified values back
!                memory(1:, 5) = fix

!                deallocate(fix)

                return
            end subroutine initialize_nodal_geometry

            subroutine fill_area(i, startx, starty, endx, endy, stepx, stepy, configData)
                use rt_param
                use cascade_globals

                implicit none

                integer(4), intent(inout) :: i
                type(config) :: configData

                real(8), intent(in) :: startx, starty, endx, endy, stepx, stepy
                real(8) :: px, py, randomValue

                px = startx
                py = starty

                print *, "fill_area, enter. i, startx, starty: ", i, startx, starty
                print *, "endx, endy, stepx, stepy: ", endx, endy, stepx, stepy

                do
                    if ((px == 0.0_8) .or. (py == 0.0_8) .or. (px > configData%sidex - stepx) .or. & 
                        (py > configData%sidey - stepy)) then ! point is on the border
                        x(i) = px
                        y(i) = py
                        memory(i, 5) = 0.0_8 ! fix
                        h(i) = 0.0_8
                        shelf(i) = 0.0_8
                    else ! point is inside the area
                        call random_number(randomValue)
                        x(i) = px + ((randomValue - 0.5_8) * stepx)

                        call random_number(randomValue)
                        y(i) = py + ((randomValue - 0.5_8) * stepy)

                        memory(i, 5) = 1.0_8 ! fix

                        if (configData%addshelf) then
                            shelf(i) = 1.0_8
                            ! TODO: add slope for height h(i)
                            ! configData%slope xr, xl, yu, yd
                        else
                            shelf(i) = 0.0_8
                        endif

                    endif

                    i = i + 1
                    px = px + stepx

                    if (px > endx) then
                        px = startx
                        py = py + stepy
                        if (py > endy) then
                            print *, "fill_area, exit. px, py: ", px, py
                            exit
                        endif
                    endif ! px > endx
                enddo ! py < endy
            end subroutine fill_area
        end module m_initialize_nodal_geometry

