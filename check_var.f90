        module m_check_var
            contains

                logical function invalid_real(r)
                    implicit none

                    real(8) :: r

                    invalid_real = ((r /= r).or.(1.0_8/r == 0.0_8))

                    return
                end function invalid_real

            subroutine check_var(mode, message, variable, i, opt1, opt2, opt3)
                use cascade_globals

                implicit none

                integer(4) :: mode, i, msg_len, zero
                real(8) :: variable
                real(8), optional :: opt1, opt2, opt3
                character(*) :: message

                do msg_len=1,255
                    if (message(msg_len:msg_len) == '$') then
                        exit
                    end if
                end do

                msg_len = msg_len - 1

                ! check for NaN or Infinity
                ! if mode == 4 then check for 0
                if (invalid_real(variable).or.((mode == 4).and.(variable == 0.0_8))) then
                    print *, "variable <" // message(1:msg_len) // "> is invalid: ", variable
                    print *, "mode, i: ", mode, i
                    if (mode == 1) then ! check only a single variable
                        print *, "" ! do nothing for now, will be added later
                    else if (mode == 2) then ! check global variables
                        print *, "h(i): ", h(i)
                        print *, "h0(i): ", h0(i)
                        print *, "hp(i): ", hp(i)
                        print *, "water(i): ", water(i)
                        print *, "prec(i): ", prec(i)
                        print *, "param(i,1): ", param(i,1)
                        print *, "param(i,2): ", param(i,2)
                        print *, "length(i): ", length(i)
                        print *, "slope(i): ", slope(i)
                        print *, "glacier(i): ", glacier(i)
                        print *, "sediment(i): ", sediment(i)
                        print *, "shelf(i): ", shelf(i)
                        print *, "diag(i): ", diag(i)
                        print *, "bel(i): ", bel(i)
                        print *, "memory(i,1): ", memory(i,1)
                        print *, "memory(i,2): ", memory(i,2)
                        print *, "memory(i,3): ", memory(i,3)
                        print *, "memory(i,4): ", memory(i,4)
                        print *, "memory(i,5): ", memory(i,5)
                        print *, "memory(i,6): ", memory(i,6)
                        print *, "memory(i,7): ", memory(i,7)
                        print *, "ael1(1,i): ", ael1(1,i)
                        print *, "ael1(2,i): ", ael1(2,i)
                        print *, "ael1(3,i): ", ael1(3,i)
                        print *, "ael1(4,i): ", ael1(4,i)
                        print *, "ael1(5,i): ", ael1(5,i)
                        print *, "ael1(6,i): ", ael1(6,i)
                        print *, "ael2(1,i): ", ael2(1,i)
                        print *, "ael2(2,i): ", ael2(2,i)
                        print *, "ael2(3,i): ", ael2(3,i)
                        print *, "ael2(4,i): ", ael2(4,i)
                        print *, "ael2(5,i): ", ael2(5,i)
                        print *, "ael2(6,i): ", ael2(6,i)
                    else if (mode == 3) then ! check all global variables
                        print *, "" ! do nothing for now, will be added later
                    end if
                    if (present(opt1).and.mode==5) then
                        print *, "opt1: ", opt1
                    end if
                    if (present(opt2).and.mode==5) then
                        print *, "opt2: ", opt2
                    end if
                    if (present(opt3).and.mode==5) then
                        print *, "opt3: ", opt3
                    end if
                    zero = 0
                    print *, "now you can use the debugger...", 1 / zero
                end if

            end subroutine check_var

            subroutine check_array_2d_real(text, ar, xmax, ymax)
                implicit none

                integer(4) :: xmax, ymax, i, j
                real(8), dimension(xmax,ymax) :: ar
                character(255) :: text

                do i=1,xmax
                    do j=1,ymax
                        call check_var(1, text, ar(i,j), i)
                    end do
                end do
            end subroutine check_array_2d_real
        end module m_check_var
