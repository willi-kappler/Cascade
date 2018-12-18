! This version of CASCADE is a fork from the original version published by Braun and Sambridge, 1997 Basin Research. The modifications made to the original version of CASCADE are described in Yanites and Ehlers, 2012, 2016 EPSL, and Eizenhoefer et al., 2019 JGR-ES.
! If you published results from this version of the program we would appreciate that you reference:
! 1. Publications listed by J. Braun in the cascade.f90 file.
! 2. Yanites, B.J., and Ehlers, T.A., 2012, Global climate and tectonic controls on the denudation of glaciated mountains, Earth and Planetary Science Letters, v. 325-326, pp. 63-75. doi.org/10.1016/j.epsl.2012.01.030.
! 3. Yanites, B.J., and Ehlers, T.A., 2016, Intermittent glacial sliding velocities explain variations in long-timescale denudation, SW British Columbia. Earth and Planetary Science Letters, 450, pp. 52-61. 
! 3. Eizenhoefer, P.R., McQuarrie, N., Shelef, E., and Ehlers, T.A., 2019 (in press), Fluvial responses to horizontal displacements in convergent orogens over geologic time scales, Journal of Geophysical Research - Earth Surface.

! Please direct all inquiries concerning this fork of the source code to Todd Ehlers (Univ. Tuebingen, Germany) tehlers@icloud.com
! Written by PRE Jan2017; peizen@pitt.edu
! Uses MOVE velocity files as input; modifies original tectonic_movement module

module m_MOVEtoCascadeHmove
contains
        subroutine MOVEtoCascadeHmove(configData)

        use rt_param
        use cascade_globals
        use m_debug
        use m_find_surface
        use m_del_flip
        use m_MOVEtoCascadeVelocityField
        
        implicit none    

        integer :: Lines = 0, ios, k, loc(1)
        integer(4), allocatable :: LocArray(:), index1(:), index2(:)

        type(config) :: configData
        integer(4) :: inode1, inode2, FileUnit1 = 56, NumberOfGrids, NumberOfVelocityFiles
        real(8) :: inputData
        real(8), allocatable :: x_MOVEv(:), y_MOVEv(:), z_MOVEv(:), x_vel(:), y_vel(:), z_vel(:), MOVE_ID(:), MOVE_Color(:), &
            z_MOVEv_temp(:), InputTimeStep(:), TimeStep(:), location_x(:), location_y(:), location_z(:), x_MOVETopo(:), &
            y_MOVETopo(:), z_MOVETopo(:), MOVELine_ID(:)
        real(8), allocatable :: temp_x(:), temp_y(:), temp_z(:)

        integer(4) :: i, i0, i1, i2, i3, ie, iess, it
        integer(4) :: ja, jess, jess2, j, jb, nswaps
        integer(4), dimension(:,:), allocatable :: nn3

        character(255) :: FileName1, FileName2, FileName3, FileName4
        character(255), dimension(:), allocatable :: GridFiles, TopoFiles

        logical :: Test

        real(8) :: GridResolution_x

!        configData%idiffusion = .FALSE.
        ! Apparently, implementation of spatially variable horizontal movement doesn't always find solutions for diffusion. Thus,
        ! it's hard-coded turned off here when MOVEtoCascadeHmove is active; NO IT'S A SCALING ISSUE!.

        allocate(nn3(nbmax,configData%nnode))

        ! Select correct active MOVE velocity file
        if (configData%uplift_mode == 3) then ! Reading from an input list of grids and selecting the appropriate velocity file

            FileName1 ='input/IceCascade/'//configData%MOVE_velocity_file

            call LengthOfFile(FileName1, Lines)

            open(FileUnit1, file=FileName1, status='old')

                    ! Skip first four lines
                    do i = 1,4
                        read (FileUnit1, *)
                    end do

                    NumberOfGrids = Lines - 4
                    NumberOfVelocityFiles = NumberOfGrids - 1

                    ! Read time steps and input file names; reverse time line
                    allocate (InputTimeStep(NumberOfGrids), GridFiles(NumberOfGrids), TimeStep(NumberOfGrids), &
                        TopoFiles(NumberOfGrids))

                    do i = 1, NumberOfGrids
                        read(FileUnit1, *, iostat=ios), InputTimestep(i), GridFiles(i), TopoFiles(i)
                        TimeStep(i) = (InputTimeStep(1) - InputTimeStep(i)) * 1e6_8
                    end do

                    Lines = 0

            close(FileUnit1)

            do i = 1,NumberOfGrids

                if (TimeStep(i) >= time .and. i > 1) then
                    FileName2 = 'input/IceCascade/'//trim(GridFiles(i-1))//'_vel_new.dat'
                    FileName3 = 'input/IceCascade/'//trim(GridFiles(i-1))//'_vel_new_temp.dat'
                    exit
                elseif (i == 1) then
                    FileName2 = 'input/IceCascade/'//trim(GridFiles(i))//'_vel_new.dat'
                    FileName3 = 'input/IceCascade/'//trim(GridFiles(i))//'_vel_new_temp.dat'
                end if

            end do

        else if (configData%uplift_mode == 2) then ! Reading a single velocity file

            FileName2 = 'input/IceCascade/'//configData%MOVE_velocity_file

        end if

        print*, 'Reading MOVE velocity file ', FileName2, ' for horizontal movement component...'

        ! Read MOVE velocity and topography files
        ! Velocity file
        call LengthOfFile(FileName2, Lines)

        open(FileUnit1, file=FileName2, status='old')

            allocate(MOVE_ID(Lines), MOVE_Color(Lines), &
                    x_MOVEv(Lines), y_MOVEv(Lines), z_MOVEv(Lines), x_vel(Lines), y_vel(Lines), z_vel(Lines))

            read(FileUnit1,*)

                do j = 1,Lines
                    read(FileUnit1, *, iostat=ios)  MOVE_ID(j), x_MOVEv(j),  y_MOVEv(j), z_MOVEv(j), &
                        x_vel(j),  y_vel(j),  z_vel(j), MOVE_Color(j)
                    if (ios/=0) then
                        exit
                    end if
                end do

                Lines = 0

            close(FileUnit1)

!        ! Topography file
!        call LengthofFile(FileName4, Lines)
!
!            open(FileUnit1, file=FileName4, status='old')
!
!                allocate(x_MOVETopo(Lines), y_MOVETopo(Lines), z_MOVETopo(Lines), MOVELine_ID(Lines))
!
!                do j = 1,Lines
!                    read(FileUnit1, *, iostat=ios) x_MOVETopo(j), y_MOVETopo(j), z_MOVETopo(j), MOVELine_ID(j)
!                        if (ios/=0) then
!                            exit
!                        end if
!                end do
!
!            close(FileUnit1)

        print*, 'DONE!'

! This is pure shear shortening in Y with velocity (advec_vely) D.S 7/27/1
! Instead of using a global value for Y, it uses one given by the MOVE velocity file (spatially variable), PRE Jan2017

        allocate(temp_x(configData%nnode), temp_y(configData%nnode), temp_z(configData%nnode), index1(configData%nnode), &
            index2(configData%nnode), location_x(configData%nnode), location_y(configData%nnode), location_z(configData%nnode))

        call LengthOfFile(FileName2, Lines)

        inquire(file=FileName3, exist=Test)

        if (Test) then

            print*, FileName3, 'already exists, reading velocities...'

            open(FileUnit1, file=FileName3, status='old')

                ! Skip header
                do i = 1,1
                    read (FileUnit1, *)
                end do

                do i = 1,configData%nnode

                    read(FileUnit1, *, iostat=ios), index1(i), index2(i), &
                        location_x(i), location_y(i), location_z(i), temp_x(i), temp_y(i), temp_z(i)

                    if (ios/=0) then
                        exit
                    end if

                end do

            close(FileUnit1)

            print*, 'DONE!'

        else

            print*, 'Finding surface velocities and saving them in ', FileName3, '...'

            open(FileUnit1, file=FileName3, status='new')

                write(FileUnit1, *), "# node, MOVEGrid_index, node_x,node_y,node_z,node_vx,node_vy,node_vz; units: km, mm/year"

                GridResolution_x = configData%sidex / configData%nx

                do inode1=1,configData%nnode

                    allocate(LocArray(Lines))

                    ! Find all input entries that are in the resolution range of x(inode): +/- half the Cascade grid resolution (2D MOVE)
                    do i = 1,Lines

                        if ((x_MOVEv(i) - GridResolution_x) < x(inode1) .and. x(inode1) <= (x_MOVEv(i) + GridResolution_x)) then
                            LocArray(i) = i
                        else
                            LocArray(i) = 0.0_8
                        end if

                    end do

                    LocArray = pack(LocArray, LocArray /= 0.0) ! Index locations that correspond to x(inode) along the z-axis

                    allocate(z_MOVEv_Temp(size(LocArray)))

                    do i = 1,size(LocArray)

                        z_MOVEv_temp(i) = z_MOVEv(LocArray(i))

                    end do

                    Loc = LocArray(minloc(abs(h(inode1)-z_MOVEv_Temp))) ! Index that is closest to current node elevation

                    deallocate (LocArray, z_MOVEv_Temp)

                    temp_x(inode1) = -x_vel(Loc(1)) * 1e-6_8
                    temp_y(inode1) = -y_vel(Loc(1)) * 1e-6_8
                    temp_z(inode1) = z_vel(Loc(1)) * 1e-3_8

                    write(FileUnit1, *), inode1, Loc(1), x(inode1), y(inode1), h(inode1), temp_x(inode1), temp_y(inode1), &
                        temp_z(inode1)

                end do

            close(FileUnit1)

        end if

!        do inode1=1,configData%nnode
!            loc = minloc(abs(x(inode1)-x_MOVEv))
!            temp1(inode1) = -y_vel(loc(1)) * 1e-6_8
!!            print*, 'Velocity in y is: ', temp1(inode1)
!        end do

      do i=1,configData%nnode
       if(memory(i, 5).gt.0.5_8)then
        y(i)=y(i)-temp_y(i)*(y(i)/configData%sidey)*global_cascade_dt
       endif
      enddo

! This is pure shear shortening in X with velocity (advec_velx) D.S 7/27/1
! Instead of using a global value for Y, it uses one given by the MOVE velocity file (spatially variable), PRE Jan2017

!        do inode2=1,configData%nnode
!            loc = minloc(abs(x(inode2)-x_MOVEv))
!            temp2(inode2) = -x_vel(loc(1)) * 1e-6_8 ! Factor 1e-6 arbitrarily changed for testing
!!            print*, 'Velocity in x is: ', temp2(inode2)
!        end do

      do i=1,configData%nnode
       if(memory(i, 5).gt.0.5_8)then
!       print*, 'x at position i BEFORE horizontal movement: ', x(i)
        x(i)=x(i)-temp_x(i)*(x(i)/configData%sidex)*global_cascade_dt
!       print*, 'x at position i AFTER horizontal movement: ', x(i)
!        print*, x(i)
       endif
      enddo

! this is the spinning landscape
!      omega=2.*3.1415/1.e6
!        do i=1,nnode
!        x0=x(i)-50.
!        y0=y(i)-50.
!          if (x0**2+y0**2.lt.25.**2) then
!          x(i)=x(i)-omega*dt*y0
!          y(i)=y(i)+omega*dt*x0
!          endif
!        enddo

! perform a check to see if mesh needs to be updated DS 11/27/1 
!  remember endif statement at end if commenting out
      if ((configData%advec_vely*tcheck.gt.delta/10.0_8).or. &
          (configData%advec_velx*tcheck.gt.delta/10.0_8)) then

! reset tcheck
      tcheck = 0.0_8

! from here on you should not change anything...
      do i=1,configData%nnode
       points(1,i)=x(i)
       points(2,i)=y(i)
      enddo

      do i=1,configData%nnode
        memory(i, 6)=0.0_8
      enddo

      if (ivocal) call debug ('del_flip$')
      call del_flip (points,neighbour,vertices,nt,mask,mask_e,nswaps)
        do it=1,nt
            if (mask(it)) then
                memory(vertices(1,it), 6)=1.0_8
                memory(vertices(2,it), 6)=1.0_8
                memory(vertices(3,it), 6)=1.0_8
            endif
        enddo
      if (ivocal) call debug ('tectonic_movement$')

! if there were any swapping the natural neighbours and surfaces need to be 
! recalculated...

      if (nswaps.ne.0) then

        do i=1,configData%nnode
            nb(i)  = 0
            nb2(i) = 0
        enddo
 
!
! find nn,nb, nb2,nn2,cell
!
        do it=1,nt
            i1=vertices(1,it)
            i2=vertices(2,it)
            i3=vertices(3,it)
     
        ! i1
            nb(i1)=nb(i1)+1
            nn(nb(i1),i1)=i2
            if (nb(i1).gt.nbmax) then
                print *, "nbmax: ", nbmax, ", nb: ", nb(i1)
                stop 'nbmax too small...1'
            endif
     
            nb2(i1)=nb2(i1)+1
            nn2(nb2(i1),i1)=i2
            cell(i1,int((1.+real(nb2(i1)))/2.),1)=i2
     
            nb2(i1)=nb2(i1)+1
            nn2(nb2(i1),i1)=i3
            cell(i1,int((1.+real(nb2(i1)))/2.),2)=i3
            if (nb2(i1).gt.nbmax) then
                print *, "nbmax: ", nbmax, ", nb2: ", nb2(i1)
                stop 'nbmax too small...2'
            endif
     
        ! i2
            nb(i2)=nb(i2)+1
            nn(nb(i2),i2)=i3
            if (nb(i2).gt.nbmax) then
                print *, "nbmax: ", nbmax, ", nb: ", nb(i2)
                stop 'nbmax too small...3'
            endif
     
            nb2(i2)=nb2(i2)+1
            nn2(nb2(i2),i2)=i3
            cell(i2,int((1.+real(nb2(i2)))/2.),1)=i1
     
            nb2(i2)=nb2(i2)+1
            nn2(nb2(i2),i2)=i1
            cell(i2,int((1.+real(nb2(i2)))/2.),2)=i3
            if (nb2(i2).gt.nbmax) then
                print *, "nbmax: ", nbmax, ", nb2: ", nb2(i2)
                stop 'nbmax too small...4'
            endif
     
        ! i3
            nb(i3)=nb(i3)+1
            nn(nb(i3),i3)=i1
            if (nb(i3).gt.nbmax) then
                print *, "nbmax: ", nbmax, ", nb: ", nb(i3)
                stop 'nbmax too small...5'
            endif
     
            nb2(i3)=nb2(i3)+1
            nn2(nb2(i3),i3)=i1
            cell(i3,int((1.+real(nb2(i3)))/2.),1)=i1
     
            nb2(i3)=nb2(i3)+1
            nn2(nb2(i3),i3)=i2
            cell(i3,int((1.+real(nb2(i3)))/2.),2)=i2
            if (nb2(i3).gt.nbmax) then
                print *, "nbmax: ", nbmax, ", nb2: ", nb2(i3)
                stop 'nbmax too small...6'
            endif

        enddo
 
!
! added directly from find_neighbours.f
!
        do i=1,configData%nnode
 
! classement de nn2 de i le plus gd au plus pt
         do ie=1,nb2(i)-1
          ja=nn2(ie,i)
          jb=ie
          do jess=ie+1,nb2(i)
           jess2=nn2(jess,i)
           if(ja.lt.jess2) then
            ja=jess2
            jb=jess
           endif
          enddo
          nn2(jb,i)=nn2(ie,i)
          nn2(ie,i)=ja
         enddo
 
! elimination des pts doubles de enl
         i0=1
         nn3(1,i)=nn2(1,i)
         do iess=2,nb2(i)-1
          if(nn2(iess,i).ne.nn3(i0,i)) then
           nn3(i0+1,i)=nn2(iess,i)
           i0=i0+1
          endif
         enddo
         do j=1,i0
          nn2(j,i)=nn3(j,i)
         enddo
         nb2(i)=i0
        enddo
!
! back to regular code
!

      if (ivocal) call debug ('find_surface$')
      call find_surface (configData)
!      call find_surface (nn,nb,surface,nbmax,nnode,x,y,xy,pp,aa,bb,newsurface,surfscale)
      if (ivocal) call debug ('tectonic_movement$')

        do i=1,configData%nnode
        nb(i)=nb(i)+1
          if (nb(i).gt.nbmax) then
            print *, "nbmax: ", nbmax, ", nb: ", nb(i)
            stop 'nbmax too small...7'
          endif
        nn(nb(i),i)=i
        enddo

      endif

      endif

      deallocate(nn3)

              return



    end subroutine MOVEtoCascadeHmove
end module m_MOVEtoCascadeHmove

