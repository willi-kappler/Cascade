! initialize_nodal_geometry

        module m_read_nodal_geometry
            contains
            subroutine read_nodal_geometry(configData)
                use rt_param
                use cascade_globals
                use m_readmesh

                implicit none

! subroutine to read in initial nodal information

! INPUT: nnodemax      = maximum number of nodes that can be read
!        run_name      = name of the subdirectory where the run output
!                        will be stored
!        nrun_name     = length of the character string run_name

! OUTPUT: nnode        = number of nodes read
!         x(nnode)     = x-location of nodes (in km)
!         y(nnode)     = y-location of nodes (in km)
!         h(nnode)     = height of nodes (in m)
!         fix(nnode)   = boundary condition (=0 means node height is fixed;
!                        =1 means node height is free) = memory(i,5)
!         delta        = mean nodal spacing (in km)
!         surfscale    = mean nodal surface (a nodal surface is the surface
!                        of the part of the landscape associated with a node)
!                        (in km**2)
!         surfmin
!         bdry         = flag to identify moving bdry nodes (added 11/28/1 DS)
!                        0: not on bdry, 1: on bdry
!         shelf         an array of 1's and 0's denoting if a node is a contenintl shelf (shelf=0) or
!                       regular tectonically active topogrpahy (shelf=1)

! subroutines called:
! - debug
! - random
! - iread_but_skip_comment
! - read_but_skip_comment,iceth(*)

        type(config) :: configData

! # of valleys in initial topography, amplitude of initial valleys
        real(8) :: nv, A, snode, xmax, xmin, ymax, ymin
        real(8) :: diff1, diff2

        integer(4) :: i, ii, inode, ishake, j, jj, jnode, k, kk, l, ll
        integer(4) :: penx, peny, pnnode

! Number of valleys requested
      nv = 1.0_8
! Ridge-valley relief [m]
      A = 5000.0_8

! read in mesh and overwrite nodal geometry and bedrock elevations h0

      print *, "read nodal geometry"
     
      call readmesh(configData)

      penx=configData%nx
      peny=configData%ny

! 2013.06.14, WK: find out where the boundary points are

      print *, "maxval x: ", maxval(x), ", maxval y: ", maxval(y)

      if (maxval(x) > configData%sidex) then
         print *, "maximum x value is bigger than sidex in configuration file!"
         stop
      end if

      if (maxval(y) > configData%sidey) then
         print *, "maximum y value is bigger than sidey in configuration file!"
         stop
      end if

      ! difference in first row = grid step size in y-direction
      ! should be (near) zero is orientation is correct
      diff1 = abs(y(2) - y(1))

      ! difference end of first row, start of second row = size of domain in y direction
      diff2 = abs(y(configData%ny) - y(configData%ny + 1))

      if (diff2 > 3.0 * diff1) then
         print *, "x,y orientation is wrong and must be corrected"

         ! TODO new order for values:
         ! do i = 1, configData%ny
         !    do j = 0, configData%nx - 1
         !        new_x(j + 1 + ((i - 1) * nx)) = x(i + (configData%ny * j))
         !        new_y(j + 1 + ((i - 1) * nx)) = y(i + (configData%ny * j))
         !        new_htemp(j + 1 + ((i - 1) * nx)) = htemp(i + (configData%ny * j))
         !        new_iceth(j + 1 + ((i - 1) * nx)) = iceth(i + (configData%ny * j))

         stop
      end if

! add shelf to outer rim of model domain

       if (configData%addshelf) then
           pnnode=configData%nnode-nextra
    !       call random(nsx,pnnode)
    !       call random(nsy,pnnode)
           do j=1,peny
                  do i=1,penx
                        inode=(j-1)*penx+i

                        memory(inode,5)=1.0_8
                        x(inode)=x(inode)+configData%Exl
                        y(inode)=y(inode)+configData%Eyd
                        shelf(inode)=1.0_8
            !           if (i.eq.1 .or.i.eq.nx .or.j.eq.1.or.j.eq.ny) then


                        if (i.eq.1 .or.i.eq.penx) then
                             memory(inode,5)=0.0_8
                             h(inode)=0.0_8
                        endif



            !           sort(inode)=inode+ned



                        if (configData%Exl.eq.0.0_8.and.i.eq.1) memory(inode,5)=0.0_8
                        if (configData%Exr.eq.0.0_8.and.i.eq.penx) memory(inode,5)=0.0_8
                        if (configData%Eyd.eq.0.0_8.and.j.eq.1) memory(inode,5)=0.0_8
                        if (configData%Eyu.eq.0.0_8.and.j.eq.peny) memory(inode,5)=0.0_8




            !       c   if (j.eq.1.or.j.eq.ny) then
            !             x(inode)=x(inode)+nsx(inode)
            !             y(inode)=y(inode)+nsy(inode)
            !           endif
                  end do
           end do
    !       call random(xd,ned)
    !       call random(yd,ned)
    !       call random(hd,ned)
    !       call random(xl,nel)
    !       call random(yl,nel)
    !       call random(hl,nel)
    !       call random(xu,neu)
    !       call random(yu,neu)
    !       call random(hu,neu)
    !       call random(xr,ner)
    !       call random(yr,ner)
    !       call random(hr,ner)

           call random_number(xd)
           call random_number(yd)
           call random_number(hd)
           call random_number(xl)
           call random_number(yl)
           call random_number(hl)
           call random_number(xu)
           call random_number(yu)
           call random_number(hu)
           call random_number(xr)
           call random_number(yr)
           call random_number(hr)

           print *, "read_nodal_geometry, neyd: ", neyd, ", nexd: ", nexd

           if (neyd == 0) then
              ! TODO
           else
              do i=1,neyd
                 do ii=1,nexd
                    inode=(i-1)*nexd + ii
                    jnode=pnnode+inode
                    memory(jnode,5)=1.0_8
                    shelf(jnode)=0.0_8
                    ishake=1
                    if (i.eq.1.or.ii.eq.1 .or.ii.eq.nexd) then
                       memory(jnode,5)=0.0_8
                       h(jnode)=0.0_8
                       ishake=0
                    endif


                    x(jnode)=((xd(inode)-0.5_8)/(1.0_8*dble(nexd-1))*dble(ishake)+1.0_8*dble(ii-1)/dble(nexd-1)) &
                         *configData%sidex
                    y(jnode)=((yd(inode)-0.5_8)/(1.0_8*dble(neyd-1))*dble(ishake)+1.0_8*dble(i-1)/dble(neyd-1)) &
                         *(configData%Eyd-dble(dye))
                    if (x(jnode).eq.0.0_8.and.y(jnode).eq.70.0_8) then
                       y(jnode)=y(jnode)-0.1_8
                    endif
                    if (x(jnode).eq.400.0_8.and.y(jnode).eq.70.0_8) then
                       y(jnode)=y(jnode)-0.1_8
                    endif

                    !       sort(jnode)=inode
                    h(jnode)=(hd(inode)-0.5_8)-(1000.0_8*configData%slopeyd*(configData%Eyd-y(jnode)))
                    if (x(jnode).gt.configData%Exl.and.x(jnode).lt.(configData%sidex-configData%Exr)) then
                       if (i.eq.neyd) then
                          y(jnode)=y(jnode)-0.51_8
                          h(jnode)=h(jnode)-0.5_8
                       end if
                    endif
                    if (memory(jnode,5).eq.0.0_8) h(jnode)=(-1000.0_8*configData%slopeyd*(configData%Eyd-y(jnode)))
                    !        if (memory(jnode,5).eq.0) h(jnode)=0.  

                 end do
              end do
           end if

           do j=1,neyl
              do jj=1,nexl ! nexl can be zero !!!
                 inode=(j-1)*nexl + jj
                 jnode=pnnode+ned+inode
                 memory(jnode,5)=1.0_8
                 ishake=1
                 if (jj.eq.1) then
                    memory(jnode,5)=0.0_8
                    h(jnode)=0.0_8
                    shelf(jnode)=0.0_8
                    ishake=0
                 endif
                 
                 
                 x(jnode)=((xl(inode)-0.5_8)/(1.0_8*dble(nexl-1))*dble(ishake)+1.0_8*dble(jj-1)/dble(nexl-1)) &
                      *configData%Exl
                 y(jnode)=configData%Eyd+((yl(inode)-0.5_8)/(1.0_8*dble(neyl-1))*dble(ishake) &
                      +1.0_8*dble(j-1)/dble(neyl-1))*(configData%sidey-configData%Eyu-configData%Eyd)
                 
                 h(jnode)=(hl(inode)-0.5_8)-(1000.0_8*configData%slopexl*(configData%Exl-x(jnode)))
                 
                 if (y(jnode).gt.configData%Eyd.and.y(jnode).lt.(configData%sidey-configData%Eyu)) then
                    if (jj.eq.nexl) then
                       x(jnode)=x(jnode)-0.51_8
                       h(jnode)=h(jnode)-0.5_8
                    end if
                 endif
                 
                 !       if (memory(jnode,5).eq.0) h(jnode)=-1000*slopexl*(Exl-x(jnode))
                 if (memory(jnode,5).eq.0.0_8) h(jnode)=0.0_8
              end do
           end do
           
           if (neyu == 0) then

              ! TODO

           else
              do k=1,neyu
                 do kk=1,nexu
                    inode=(k-1)*nexu + kk
                    jnode=pnnode+ned+nel+inode
                    memory(jnode,5)=1.0_8
                    ishake=1
                    if (kk.eq.1.or.kk.eq.nexu.or.k.eq.neyu) then
                       memory(jnode,5)=0.0_8
                       h(jnode)=0.0_8
                       shelf(jnode)=0.0_8
                       ishake=0
                       !        sort(jnode)=inode+ned+pnnode
                    endif


                    x(jnode)=((xu(inode)-0.5_8)/(1.0_8*dble(nexu-1))*dble(ishake)+1.0_8*dble(kk-1)/dble(nexu-1))*configData%sidex
                    y(jnode)=(configData%sidey-dble(configData%Eyu)+dble(dye))+((yu(inode)-0.5_8)/(1.0_8*dble(neyu-1)) &
                         *dble(ishake)+1.0_8* dble(k-1)/dble(neyu-1))*(dble(configData%Eyu)-dble(dye))
                    if (x(jnode).eq.0.0_8.and.y(jnode).eq.220.0_8) then
                       y(jnode)=y(jnode)+0.1_8
                    endif
                    if (x(jnode).eq.400.0_8.and.y(jnode).eq.220.0_8) then
                       y(jnode)=y(jnode)+0.1_8
                    endif

                    h(jnode)=(hu(inode)-0.5_8)-(1000.0_8*configData%slopeyu*(dble(configData%Eyu)+(y(jnode)-configData%sidey)))
                    if (x(jnode).gt.dble(configData%Exl).and.x(jnode).lt.(configData%sidex-dble(configData%Exr))) then
                       if (k.eq.1) then
                          y(jnode)=y(jnode)+0.51_8
                          h(jnode)=h(jnode)-0.5_8
                       end if
                    endif

                    if (memory(jnode,5).eq.0.0_8) then
                       h(jnode)=-1000.0_8*configData%slopeyu*(dble(configData%Eyu)+(y(jnode)-configData%sidey))
                    endif
                    !       if (memory(jnode,5).eq.0) h(jnode)=0 

                 end do
              end do
           end if

           do l=1,neyr
              do ll=1,nexr ! nexr can be zero !!!
                 inode=(l-1)*nexr + ll
                 jnode=pnnode+ned+nel+neu+inode
                 ishake=1
                 memory(jnode,5)=1.0_8
                 if (ll.eq.nexr) then
                    memory(jnode,5)=0.0_8
                    h(jnode)=0.0_8
                    shelf(jnode)=0.0_8
                    ishake=0
                 endif
                 
                 
                 x(jnode)=(configData%sidex-configData%Exr)+((xr(inode)-0.5_8)/(1.0_8*dble(nexr-1)) &
                      *dble(ishake)+1.0_8* dble(ll-1)/dble(nexr-1))*configData%Exr
                 y(jnode)=configData%Eyd+(((yr(inode)-0.5_8)/(1.0_8*dble(neyr-1))*dble(ishake))+1.0_8* &
                      dble(l-1)/dble(neyr-1))*(configData%sidey-configData%Eyu-configData%Eyd)
                 
                 
                 h(jnode)=(hr(inode)-0.5_8)-(1000.0_8*configData%slopexr*(configData%Exr+(x(jnode)-configData%sidex)))
                 if (y(jnode).gt.configData%Eyd.and.y(jnode).lt.(configData%sidey-configData%Eyu)) then
                    if (ll.eq.1) then
                       x(jnode)=x(jnode)+0.51_8
                       h(jnode)=h(jnode)-0.5_8
                    end if
                 endif
                 !
                 if (memory(jnode,5).eq.0.0_8) h(jnode)=-1000.0_8*configData%slopexr &
                      *(dble(configData%Exr)+x(jnode)-configData%sidex)
                 !       if (memory(jnode,5).eq.0) h(jnode)=0     
                 if (h(jnode).gt.0.0_8) print*,'above sea level'
              end do
           end do
       endif ! if addshelf

      xmin=x(1)
      xmax=x(1)
      ymin=y(1)
      ymax=y(1)
      do i=1,configData%nnode
          xmin=min(xmin,x(i))
          xmax=max(xmax,x(i))
          ymin=min(ymin,y(i))
          ymax=max(ymax,y(i))
      enddo
      configData%sidex=xmax-xmin
      configData%sidey=ymax-ymin
      snode=sqrt(dble(configData%nnode))
      delta=(configData%sidex/snode+configData%sidey/snode)/2.0_8
      configData%surfscale=configData%sidex*configData%sidey/dble(configData%nnode)

      close (7)


        if (configData%nnode < 3) then
            stop 'nnode too small...'
        endif

        if (delta < 0.0_8) then
            stop 'delta must be greater than 0...'
        endif

        if (configData%surfscale < 0.0_8) then
            stop 'surfscale must be greater than 0...'
        endif

        if (configData%sidex < 0.0_8) then
            stop 'sidex must be greater than 0...'
        endif

        if (configData%sidey < 0.0_8) then
            stop 'sidey must be greater than 0...'
        endif

           return
           end subroutine read_nodal_geometry
        end module m_read_nodal_geometry

