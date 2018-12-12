! find greatest downstream slope for each node
!
!      subroutine find_dslope(x,y,h,nbmax,nnode,nn,nb,dslope)

        module m_find_dslope
          contains
          subroutine find_dslope(configData)
                use rt_param
                use cascade_globals
                implicit none

!       real x(nnode),y(nnode),h(nnode)
!       real dslope(nnode)

!       integer nn(nbmax,nnode),nb(nnode)

                type(config) :: configData
                real(8) :: s,dij

! iterative variables
                integer i,j,k

! **********************************
! done with variable declaration
! **********************************

           do i=1, configData%nnode

            dslope(i) = 0.0_8

            do k=1,nb(i)

             j=nn(k,i)
             if (j.ne.i) then
              dij=(x(i)-x(j))**2.0_8+(y(i)-y(j))**2.0_8
              dij=1000.0_8*sqrt(dij)
              s=(h(j)-h(i))/dij

              if (s.lt.dslope(i)) then
               dslope(i) = s
              endif

             endif
            enddo
           enddo

           return

           end subroutine find_dslope
        end module m_find_dslope

