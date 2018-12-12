! debug

        module m_debug
          contains
          subroutine debug (message)

! subroutine to help debug cascade
! it keeps track of the subroutine cascade is in when the program
! is stopped (by the user or by a run-time error)
! to use the debugger you must set ivocal to 1 in cascade
! the information is stored in a file called debug.out

! INPUT: message       = character string containing the message to be 
!                        saved in debug.out; the last character must
!                        be a dollar sign ($)
!        iflag         = if = 1 the message is added to debug.out
!                        if = 0 debug.out is first rewound

! subroutines called:
! NONE

          character(*) :: message
          integer(4) :: nmessage, i

          nmessage=0
            do i=1,256
              if (message(i:i) == '$') then
                  nmessage=i-1
                  exit
              endif
            enddo

            write (89, "(a)") message(1:nmessage)

          call flush(89)

          return
          end subroutine debug
        end module m_debug

