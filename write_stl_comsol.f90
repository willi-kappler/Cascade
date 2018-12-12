! This module contains subroutines to write out the STL file of the model
! There are two subroutines: 1. write a regular grid, 2. write an iregular grid
!
! 2012.01.17, Willi Kappler
!
    module m_write_stl_comsol
        contains
        subroutine write_stl_comsol_regular(file_id, time_step, nx, ny, grid_space, z_bottom, z_top)
            implicit none

            ! input values
            integer(4), intent(in) :: file_id
            integer(4), intent(in) :: time_step
            integer(4), intent(in) :: nx, ny, grid_space

            real(8), intent(in), dimension(:,:) :: z_bottom, z_top

            ! local variables
            character(4) :: file_counter

            integer(4) :: i, j

            real(8) :: x1, x2, y1, y2
            real(8) :: zb1, zb2, zb3, zb4
            real(8) :: zt1, zt2, zt3, zt4

            write (file_counter,'(I4.4)') time_step

            open(file_id, file='output/IceCascade/reg_stl_comsol_'//file_counter//'.stl',status='unknown')


! STL format:

! solid name
!
!  facet normal n1 n2 n3
!   outer loop
!    vertex p1x p1y p1z
!    vertex p2x p2y p2z
!    vertex p3x p3y p3z
!   endloop
!  endfacet
!
!  facet normal n1 n2 n3
!   outer loop
!    vertex p1x p1y p1z
!    vertex p2x p2y p2z
!    vertex p3x p3y p3z
!   endloop
!  endfacet
!
! ...
!
! endsolid name


! Write STL header

            write(file_id, "(a)") "solid IceCascade_STL_Comsol"

! Loop over all points to build up triangles

            do j=1,ny-1
                do i=1,nx-1

! Define points of a triangle clock-wise
! We write out two triangles here since we are using a regular grid!
!
!   x1    x2
! y1*-----*
!   | t1 /|
!   |   / |
!   |  /  |
!   | /   |
!   |/ t2 |
! y2*-----*
!
! t1 contains: (x1, y1, zb1), (x2, y1, zb2), (x1, y2, zb3)
! t2 contains: (x2, y1, zb2), (x2, y2, zb4), (x1, y2, zb3)
!


                    x1 = dble(i * grid_space)
                    x2 = dble((i + 1) * grid_space)
                    y1 = dble(j * grid_space)
                    y2 = dble((j + 1) * grid_space)

                    zb1 = z_bottom(i, j)
                    zb2 = z_bottom(i + 1, j)
                    zb3 = z_bottom(i, j + 1)
                    zb4 = z_bottom(i + 1, j + 1)

                    zt1 = z_top(i, j)
                    zt2 = z_top(i + 1, j)
                    zt3 = z_top(i, j + 1)
                    zt4 = z_top(i + 1, j + 1)

! First bottom triangle:
                    call write_triangle(file_id, x1, y1, zb1, & ! first point
                                                 x2, y1, zb2, & ! second point
                                                 x1, y2, zb3 & ! third point
                                                 )

! Second bottom triangle:
                    call write_triangle(file_id, x2, y1, zb2, & ! first point
                                                 x2, y2, zb4, & ! second point
                                                 x1, y2, zb3 & ! third point
                                                 )

! First top triangle:
                    call write_triangle(file_id, x1, y1, zt1, & ! first point
                                                 x2, y1, zt2, & ! second point
                                                 x1, y2, zt3 & ! third point
                                                 )

! Second top triangle:
                    call write_triangle(file_id, x2, y1, zt2, & ! first point
                                                 x2, y2, zt4, & ! second point
                                                 x1, y2, zt3 & ! third point
                                                 )

                    if (i == 1) then
                        ! left side
                        call write_triangle(file_id, x1, y1, zt1, & ! first point
                                                     x1, y2, zt3, & ! second point
                                                     x1, y2, zb3 & ! third point
                                                     )

                        call write_triangle(file_id, x1, y1, zt1, & ! first point
                                                     x1, y2, zb3, & ! second point
                                                     x1, y1, zb1 & ! third point
                                                     )
                    else if (i == nx-1) then
                        ! right side
                        call write_triangle(file_id, x2, y1, zt2, & ! first point
                                                     x2, y2, zt4, & ! second point
                                                     x2, y2, zb4 & ! third point
                                                     )

                        call write_triangle(file_id, x2, y1, zt2, & ! first point
                                                     x2, y2, zb4, & ! second point
                                                     x2, y1, zb2 & ! third point
                                                     )
                    endif

                    if (j == 1) then
                        ! front side
                        call write_triangle(file_id, x1, y1, zt1, & ! first point
                                                     x2, y1, zt2, & ! second point
                                                     x2, y1, zb2 & ! third point
                                                     )

                        call write_triangle(file_id, x1, y1, zt1, & ! first point
                                                     x2, y1, zb2, & ! second point
                                                     x1, y1, zb1 & ! third point
                                                     )
                    else if (j == ny-1) then
                        ! back side
                        call write_triangle(file_id, x1, y2, zt3, & ! first point
                                                     x2, y2, zt4, & ! second point
                                                     x2, y2, zb4 & ! third point
                                                     )

                        call write_triangle(file_id, x1, y2, zt3, & ! first point
                                                     x2, y2, zb4, & ! second point
                                                     x1, y2, zb3 & ! third point
                                                     )
                    endif
                enddo
            enddo

! End of STL file

            write(file_id, "(a)") "endsolid IceCascade_STL_Comsol"

            close(file_id)
        end subroutine write_stl_comsol_regular


        ! the iregular version may not be needed any more and can be deleted in the future
        subroutine write_stl_comsol_iregular(file_id, time_step, x_pos, y_pos, z_pos, num_of_triangles, triangles)
            implicit none

            ! input values
            integer(4), intent(in) :: file_id
            integer(4), intent(in) :: time_step
            integer(4), intent(in) :: num_of_triangles

            integer(4), intent(in), dimension(:,:) :: triangles

            real(8), intent(in), dimension(:) :: x_pos, y_pos, z_pos

            ! local variables
            character(4) :: file_counter

            integer(4) :: i, p1, p2, p3

            real(8) :: nx, ny, nz

            write (file_counter,'(I4.4)') time_step

            open(file_id, file='output/IceCascade/ireg_stl_'//file_counter//'.stl',status='unknown')

! Write STL header

            write(file_id, "(a)") "solid IceCascade_STL_Comsol"

! Write all triangles

            do i=1,num_of_triangles

                    p1 = triangles(1, i)
                    p2 = triangles(2, i)
                    p3 = triangles(3, i)

                    call calc_normal(x_pos(p1), y_pos(p1), z_pos(p1), x_pos(p2), y_pos(p2), z_pos(p2), &
                            x_pos(p3), y_pos(p3), z_pos(p3), nx, ny, nz)

                    write(file_id, "('    facet normal ', F16.8, F16.8, F16.8)") nx, ny, nz
                    write(file_id, "(a)") "        outer loop"
                    write(file_id, "('            vertex ', F16.8, F16.8, F16.8)") x_pos(p1), y_pos(p1), z_pos(p1) / 1000.0_8
                    write(file_id, "('            vertex ', F16.8, F16.8, F16.8)") x_pos(p2), y_pos(p2), z_pos(p2) / 1000.0_8
                    write(file_id, "('            vertex ', F16.8, F16.8, F16.8)") x_pos(p3), y_pos(p3), z_pos(p3) / 1000.0_8
                    write(file_id, "(a)") "        endloop"
                    write(file_id, "(a)") "    endfacet"
            enddo

            write(file_id, "(a)") "endsolid IceCascade_STL_Comsol"

            close(file_id)
        end subroutine write_stl_comsol_iregular

        subroutine calc_normal(x1, y1, z1, x2, y2, z2, x3, y3, z3, nx, ny, nz)
            implicit none

            real(8), intent(in) :: x1, y1, z1, x2, y2, z2, x3, y3, z3
            real(8), intent(out) :: nx, ny, nz
            real(8) :: ux, uy, uz, vx, vy, vz

            ux = x2 - x1
            uy = y2 - y1
            uz = z2 - z1

            vx = x3 - x1
            vy = y3 - y1
            vz = z3 - z1

            nx = (uy * vz) - (uz * vy)
            ny = (uz * vx) - (ux * vz)
            nz = (ux * vy) - (uy * vx)
        end subroutine calc_normal

        subroutine write_triangle(file_id, x1, y1, z1, x2, y2, z2, x3, y3, z3)
            implicit none

            integer(4), intent(in) :: file_id

            real(8), intent(in) :: x1, y1, z1
            real(8), intent(in) :: x2, y2, z2
            real(8), intent(in) :: x3, y3, z3

            real(8) :: nx, ny, nz

            call calc_normal(x1, y1, z1, x2, y2, z2, x3, y3, z3, nx, ny, nz)

            write(file_id, "('    facet normal ', F16.8, F16.8, F16.8)") nx, ny, nz
            write(file_id, "(a)") "        outer loop"
            write(file_id, "('            vertex ', F16.8, F16.8, F16.8)") x1, y1, z1
            write(file_id, "('            vertex ', F16.8, F16.8, F16.8)") x2, y2, z2
            write(file_id, "('            vertex ', F16.8, F16.8, F16.8)") x3, y3, z3
            write(file_id, "(a)") "        endloop"
            write(file_id, "(a)") "    endfacet"

        end subroutine write_triangle

    end module m_write_stl_comsol

