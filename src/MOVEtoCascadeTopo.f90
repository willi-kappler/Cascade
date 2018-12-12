! Written by PRE, peizen@pitt.edu
! Module to import 2DMOVE topography as initial topography for Cascade if meshread == T

module m_MOVEtoCascadeTopo
contains 
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
        Lines = 0
        open(54, file='input/IceCascade/'//configData%meshname, status='unknown')
            do
                read(54, *, iostat=ios) inputData
                if (ios/=0) exit
                Lines = Lines + 1
            end do
        close(54)
        
        open(55, file='input/IceCascade/'//configData%meshname, status='unknown')
        allocate(x_MOVE(Lines), y_MOVE(Lines), z_MOVE(Lines), MOVE_ID(Lines))
        
            do i = 1,Lines
                read(55, *, iostat=ios) x_MOVE(i), y_MOVE(i), z_MOVE(i), MOVE_ID(i)
                if (ios/=0) exit
            end do
        close(55)     

        call initialize_nodal_geometry(configData)
 
! Modify elevations of nodes based on MOVE input; MOVE input is in 2D, z is extrapolated 
! along the y-axis and some noise (+- 10 m) added (does noise have any effect here?!)                  
        do i = 1,configData%nnode
           !use h array slice h(i:i) instead of h(i), otherwise rank is incompatible
           h(i:i) = h(i:i) + z_MOVE(minloc(abs(x(i)-x_MOVE)))*1000.0_8 +& 
           (ran(i) * 10.0_8)
            
        end do

    end subroutine MOVEtoCascadeTopo
end module m_MOVEtoCascadeTopo
