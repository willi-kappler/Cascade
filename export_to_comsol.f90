        module m_export_to_comsol
            contains
            subroutine export_to_comsol(nx, ny, grid_spacing, ht, tempb, ice_mass_balance, h_tmp_comsol, t)

            implicit none

            ! arguments:
            integer(4), intent(in) :: nx, ny, grid_spacing
            real(8), dimension(:,:), intent(in) :: ht, tempb, ice_mass_balance, h_tmp_comsol, t

            ! local variables:
            integer(4) :: i, j, file_id_1, file_id_2, file_id_3, file_id_4, file_id_5, file_id_6
            real(8) :: x, y

            file_id_1 = 100
            file_id_2 = 101
            file_id_3 = 102
            file_id_4 = 103
            file_id_5 = 104

            open(file_id_1, file="output/comsol/export_to_comsol_isurf.txt", status="unknown")
            open(file_id_2, file="output/comsol/export_to_comsol_tempb.txt", status="unknown")
            open(file_id_3, file="output/comsol/export_to_comsol_ice_mass_balance.txt", status="unknown")
            open(file_id_4, file="output/comsol/export_to_comsol_ithick.txt", status="unknown")
            open(file_id_5, file="output/comsol/export_to_comsol_bed.txt", status="unknown")

            do i=1,nx
                do j=1,ny
                    x = dble((i-1) * grid_spacing)*1000
                    ! [m]
                    y = dble((j-1) * grid_spacing)*1000
                    ! [m]

                    write(file_id_1, "(F16.8, F16.8, F16.8)") x, y, ht(i,j)
                    ! [m]
                    write(file_id_2, "(F16.8, F16.8, F16.8)") x, y, tempb(i,j)
                    ! [degrees C; make sure comsol knows this]
                    write(file_id_3, "(F16.8, F16.8, F16.8)") x, y, ice_mass_balance(i,j)
                    ! [m/yr]
                    write(file_id_4, "(F16.8, F16.8, F16.8)") x, y, h_tmp_comsol(i,j)
                    ! [m]
                    write(file_id_5, "(F16.8, F16.8, F16.8)") x, y, t(i,j)
                    ! [m]
                enddo
            enddo

            close(file_id_1)
            close(file_id_2)
            close(file_id_3)
            close(file_id_4)
            close(file_id_5)

            ! File_id_5 writes a one line w/ zero to file output/IceCascade/import_from_comsol.txt
            ! if comsol doesn't work, this ensures that the regular update_height values are used instead
            file_id_6 = 105
            open(file_id_6, file="output/comsol/import_from_comsol.txt", status="unknown")
            write(file_id_6, "(F16.8)") 0.00
            close(file_id_6)

          end subroutine export_to_comsol
      end module m_export_to_comsol

