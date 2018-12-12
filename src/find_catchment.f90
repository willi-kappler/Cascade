! find_catchment
        module m_find_catchment
            contains
              subroutine find_catchment (configData)
                    use rt_param
                    use cascade_globals
                    implicit none
!      subroutine find_catchment (ndon,nwork,ncat,nsill,nempty,nlake, &
!                                 nn,nb,nbmax,h,nnode,nself,nselfu,fix, &
!                                 x,y,slope,length)

! subroutine to find the distribution of catchment areas, their sills
! and the distribution of "lakes"

! a catchment area is characterized by a minimum in topography

! every node is part of a catchment area
! ncat(i) is the node number of the minimum in which node i
! is draining

! nsill(i) is 0 unless i is a minimum (giving its name to a catchment area)
! in which case nsill(i) gives the number of the node at the sill of
! the catchment of name i

! nlake(i) is 0 if node i is not in a lake
!             1 if node i is in a lake

! INPUT: ndon      = donor array
!        nwork     = working array
!        nn        = neighbour array
!        nb        = number of neighbour per node
!        nbmax     = maximum number of neighbour per node
!        h         = present topography
!        nnode     = number of nodes
!        fix       = boundary condition array
!        x,y       = x- and y-nodal corrdinates 
!        slope      = slope associated to each node/stream segment
!        length     = length associated to each node/stream segment

! OUTPUT:  ndon is modified
!        ncat      = catchement array
!        nsill     = sill array
!        nempty    = array containing where a sill empties
!        nlake     = lake array
!        nself     = number of nodes presenting a problem (no output was found)
!        nselfu    = number of local minima that could not be resolved

            type(config) :: configData
            integer(4) :: i, nnn, j, jjj, k, kk, mmm, nc, nd, nl
            real(8) :: xlength, xslope, xslopemin

! subroutines called:
! NONE

!      integer    ndon(nnode),nwork(nnode),ncat(nnode),nsill(nnode)
!      integer    nlake(nnode),nempty(nnode)
!      integer    nb(nnode),nn(nbmax,nnode)
!      real       fix(nnode)
!      real       h(nnode),x(nnode),y(nnode),slope(nnode),length(nnode)

      hmax=h(1)
        do i=1,configData%nnode
            nlake(i)=0
            hmax=max(hmax,h(i)) ! find highest node on the grid
            ncat(i)=0
            nwork(i)=i
        enddo

!9999  continue

! finds catchment names

11111 nnn=0
! loop over all nodes
        do i=1,configData%nnode
! if one has not yet found the catchment number for node i
         if (ncat(i).eq.0) then
          nl=nwork(i) ! current node for node i we are examining
          nd=ndon(nl) ! nd = lowest direct neighbour of node nl
          if (nd.eq.nl) then
           ncat(i)=nl ! current node nl is his own lowest direct neighbour
                      ! so we found the lowest node for node i (= catchment)
          else
           nwork(i)=nd ! replace current node with lowest neighbour
           nnn=nnn+1 ! set a flag that we are not finished yet
          endif
         endif
        enddo
      if (nnn.ne.0) goto 11111

! check if a catchment area is touching a fixed node
! in which case there is no need to find a sill

9999  continue

        do i=1,configData%nnode
            nwork(i)=0 ! reset working area
        enddo
        do i=1,configData%nnode
          ! memory(*,5) = fix
          if (memory(i, 5).lt.0.5_8) then
              nc=ncat(i) ! nc = lowest node (= catchment) for node i
              nwork(nc)=1 ! set flag: the node nc is finished
          endif
        enddo

! finds sills
        do i=1,configData%nnode
          nc=ncat(i) ! nc = catchment for node i
          if (nwork(nc).eq.0) then ! flag for node nc not set: node nc not finished
! look for the lowest point on the catchment boundary that has a donor below it
          hmin=hmax
          jjj=0
            do j=1,configData%nnode
              if (ncat(j).eq.nc) then ! node i and node j have the same lowest neighbour (= catchment), assuming i != j
                do k=1,nb(j) ! nb = number of neighbours for node j
                  nnn=nn(k,j) ! nn(*,j) = list of neighbours of node j, nn(1,j) = first neighbour, nn(2,j) = second neighbour, ...
                  if (ncat(nnn).ne.nc .and.h(nnn).lt.h(j)) then
                    if (h(j).lt.hmin) then
                        hmin=h(j)
                        jjj=j ! jjj = node with a sill
                    endif
                  endif
                enddo ! k, all neighbours of node j
              endif ! node i and node j have the same catchment
            enddo ! j
! sometimes one has to look at a second ring of nodes
! ie neighbours of neighbours
          if (jjj.eq.0) then
            do j=1,configData%nnode
              if (ncat(j).eq.nc) then
                do k=1,nb(j)
                nnn=nn(k,j)
                  if (ncat(nnn).ne.nc) then
                    do kk=1,nb(nnn)
                    mmm=nn(kk,nnn)
                      if (ncat(mmm).ne.nc .and. h(mmm).lt.h(nnn)) then
                        if (h(nnn).lt.hmin) then
                        hmin=h(nnn)
                        jjj=nnn
                        endif
                      endif
                    enddo ! kk
                  endif
                enddo ! k
              endif
            enddo ! j
          endif
! otherwise give up...
          if (jjj.eq.0) then
              print *, "find_catchment: i ", i, ", nc: ", nc
              print *, "x(i): ", x(i), ", y(i): ", y(i), ", h(i): ", h(i)
              print *, "x(nc): ", x(nc), ", y(nc): ", y(nc), ", h(nc): ", h(nc)
              stop 'cannot find a sill...'
          endif
! a sill has been found in node jjj
! we now find the one of its neighbours that is in another catchment
! and has the steepest slope
          xslopemin=0.0_8
            do k=1,nb(jjj)
              nnn=nn(k,jjj)
              if (ncat(nnn).ne.nc .and. nnn.ne.jjj) then
                xlength=sqrt((x(nnn)-x(jjj))**2+(y(nnn)-y(jjj))**2)
                if (xlength.eq.0.0_8) print*,nnn,jjj
                xslope=(h(nnn)-h(jjj))/xlength
                if (xslope.lt.xslopemin) then
                    xslopemin=xslope
                    ndon(nc)=nnn
                    length(nc)=xlength
                    slope(nc)=xslope
                endif
              endif
            enddo ! k
! find lake nodes in this catchment
            do k=1,configData%nnode
                if (ncat(k).eq.nc .and. h(k).lt.h(jjj)) nlake(k)=1
            enddo ! k
! reset catchment names
            do k=1,configData%nnode
                if (ncat(k).eq.nc) ncat(k)=ncat(ndon(nc))
            enddo ! k
          goto 9999
          endif
        enddo ! i

      return
              end subroutine find_catchment
        end module m_find_catchment

