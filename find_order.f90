! find_order

        module m_find_order
          contains
          subroutine find_order (ibucket,ndon,iorder, nnode,norder)

! subroutine to calculate the proper order in which the nodes must be
! stored to perform the river erosion calculations

! INPUT: ibucket     = working array
!        ndon        = donor array
!        nnode       = total number of nodes

! OUTPUT: iorder      = node ordering
!         norder      = number of nodes in the ordering

! subroutines called:
! NONE

          integer(4) :: ibucket(*),ndon(*),iorder(*)
          integer(4) :: norder, i, nnode, ib
          logical :: more

          norder=0

! find self-donors
          do i=1,nnode
           ibucket(i)=1
           if (ndon(i).eq.i) ibucket(i)=0
          enddo

! beginning of pseudo time stepping

! neat
        1 continue

          do i=1,nnode
           if (ibucket(i).ne.0) ibucket(ndon(i))=-1
          enddo

          more=.false.
          do i=1,nnode
           ib=ibucket(i)
           if (ib.gt.0) then
            norder=norder+1
            iorder(norder)=i
            ib=0
            more=.true.
           endif
           ibucket(i)=ib**2
          enddo

          if (more) goto 1

! puts self-donors at the end of the list
          do i=1,nnode
           if (ndon(i).eq.i) then
            norder=norder+1
            iorder(norder)=i
           endif
          enddo

          return
          end subroutine find_order
        end module m_find_order

