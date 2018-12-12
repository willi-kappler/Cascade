! iread_but_skip_comment
        module m_iread_but_skip_comment
          contains
          subroutine iread_but_skip_comment (iunit,n,x)

! to read in an array of integer*4 numbers of length n
! while skipping potential comment lines starting with the character '#'

! INPUT: n             = length of array to be read
!        iunit         = unit number where data is read from

! OUTPUT: x(n)         = array of length n read from unit iunit

! subroutines called:
! NONE

          character*1 comment
          integer(4) :: n, iunit
          integer x(n)

        1 read (iunit,'(a1)',end=999,err=998) comment
          if (comment.eq.'#') goto 1
          backspace (iunit)
          read (iunit,*,end=999,err=998) x

          return

999       print*,'End of file reached on unit ',iunit
          stop '(iread_but_skip_comment)'

998       print*,'Wrong data type on unit ',iunit
          stop '(iread_but_skip_comment)'

          end subroutine iread_but_skip_comment
        end module m_iread_but_skip_comment


