! find_donors
        module m_find_donors
          contains
          subroutine find_donors (configData)
                use rt_param
                use cascade_globals
                implicit none
!      subroutine find_donors (x,y,h,slope,length,nnode,ndon,nb,nn,nbmax,delta)

! this subroutine simply finds the lowest neighbour of each point among
! its natural neighbours.
! It also calculates the length of the connection and the slope.
! Note that in the case one has made a periodic
! boundary condition, one needs to check the length of rivers across
! the whole grid.

! INPUT: x,y     = x- and y-nodal locations
!        h       = present topography
!        nnode   = number of nodes
!        nb      = number of neighbours per node
!        nn      = list of neighbours per node
!        nbmax   = maxiumum number of neighbours per node
!        delta   = mean nodal spacing (in km)

! OUTPUT: slope   = slope of stream segment originating at node i
!                    (downstream slope)
!         length  = length of stream segment originating at node i
!         ndon    = donor of node i (ie where the segment ends)

! subroutines called:
! NONE

                type(config) :: configData
                integer(4) :: inode, jnode, i, nnn, nbb
                real(8) :: dd, dh, dhmin, dx, dy

!      integer ndon(nnode),nb(nnode),nn(nbmax,nnode)
!      real    x(nnode),y(nnode),h(nnode)
!      real    slope(nnode),length(nnode)

! loop to find donors

! loop over the nodes
            do inode=1,configData%nnode
                dhmin=0.0_8
                ndon(inode)=inode
                nbb=nb(inode)
! loop over the neighbours
              do jnode=1,nbb
                  nnn=nn(jnode,inode)
                  if (nnn <= configData%nnode) then
                    if (nnn.ne.inode) then
                      dh=h(nnn)-h(inode) ! if dh is negative, node nnn is a lower neighbour of node inode (thus, flow recipient)

!! ########## Added by PRE Feb2017 ############
!! Makes boundary nodes donors in order to prevent flow out of a closed model boundary ("X_bnd = 3"). If a neighbour is detected as
!! lower than inode and it lies on the boundary then it will not be treated as a recipient/donor.
!! (Not sure if that really works though...)
!
!                    if (dh < 0.0_8) then
!                      if (memory(nnn,5) == 0.0_8) then ! node is on a boundary
!                        if (y(nnn) == configData%sidey .and. configData%E_bnd == 3) then ! node is on the western boundary
!                                dh = 0.0_8
!                        else if (y(nnn) == 0.0_8 .and. configData%W_bnd == 3) then ! node is on the eastern boundary
!                                dh = 0.0_8
!                        else if (x(nnn) == configData%sidex .and. configData%N_bnd == 3) then ! node is on the northern boundary
!                                dh = 0.0_8
!                        else if (x(nnn) == 0.0_8 .and. configData%S_bnd == 3) then ! node is on the southern boundary
!                                dh = 0.0_8
!                        end if
!                      end if
!                    end if
!
!! #############################################

                      dx=x(nnn)-x(inode)
                      dy=y(nnn)-y(inode)
                      dd=sqrt(dx**2+dy**2) ! if dd is small the node is near
                      if (dh/dd < dhmin) then ! then dh / dd is smaller, since dh is negative
                        ndon(inode)=nnn
                        dhmin=dh/dd

!! ########## Added by PRE Feb2017 ############
!! Makes boundary nodes donors in order to prevent flow out of a closed model boundary ("X_bnd = 3")
!! (Not really working it seems...)
!
!                        if (memory(nnn,5) == 0.0_8) then ! node is on a boundary
!                            if (y(nnn) == configData%sidey .and. configData%E_bnd == 3) then ! node is on the western boundary
!                                ndon(inode)=nnn
!                                dhmin=dh/dd
!                            else if (y(nnn) == 0.0_8 .and. configData%W_bnd == 3) then ! node is on the eastern boundary
!                                ndon(inode)=nnn
!                                dhmin=dh/dd
!                            else if (x(nnn) == configData%sidex .and. configData%N_bnd == 3) then ! node is on the northern boundary
!                                ndon(inode)=nnn
!                                dhmin=dh/dd
!                            else if (x(nnn) == 0.0_8 .and. configData%S_bnd == 3) then ! node is on the southern boundary
!                                ndon(inode)=nnn
!                                dhmin=dh/dd
!                            end if
!                        end if
!
!! #############################################
                      endif

                    endif
                  else
                    print *, "find_donors: node index out of range: ", nnn, ", inode: ", inode, ", jnode: ", jnode, ", nbb: ", nbb
                  endif
              enddo ! jnode
            enddo ! inode

! loop to calculate lengths and slopes

            do i=1,configData%nnode
                if (ndon(i).eq.i) then
                    slope(i)=0.0_8
                    length(i)=0.0_8
                else
                    dx=x(i)-x(ndon(i))
                    dy=y(i)-y(ndon(i))
                    length(i)=sqrt(dx**2+dy**2)

                    slope(i)=(h(ndon(i))-h(i))/length(i)

                endif
            enddo

          return
          end subroutine find_donors
        end module m_find_donors

