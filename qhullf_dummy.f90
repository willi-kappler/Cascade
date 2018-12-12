!
!-----------------------------------------------------------------------------
!
!   Subroutine qhullf (np,i,j,nt_max,k,points,nt,vertices)
!
!-----------------------------------------------------------------------------
!
    Subroutine qhullf ()
!

    write(*,*)' '
    write(*,*)' Error in subroutine nn2d_setup'
    write(*,*)' qhull is not installed'
    write(*,*)' Delaunay triangulation must be either'
    write(*,*)' calculated with routine delaun (dmode=-1 or -2)'
    write(*,*)' or read in from a file (dmode>0; logical unit=dmode)'
    write(*,*)' '

    stop
    end
