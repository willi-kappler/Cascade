! write_tecplot_output
        module m_write_tecplot_output
            contains
                subroutine writeFloat(value)
                    implicit none
                    real(8) :: value

                    if (value == 0.0_8) then
                      write(47, "(F4.1) ", advance='no') 0.0
                    else
                      write(47, "(E32.8) ", advance='no') value
                    endif
                end subroutine
                subroutine write_tecplot_output(configData, temp, catch)
                    use rt_param
                    use cascade_globals

                    implicit none

!      subroutine write_tecplot_output(h,h0,x,y,configData%nnode,vertices,nt,prec,time,timeint, &
!                                      dhfluvial,dhdiffusion,dhls,global_cascade_dt,temp,catch, &
!                                      iceth,tott,slide,dhglacier,gbalance,sediment, &
!                                      isodh,slope,hiso,strict,totalerosion,surface, &
!                                      gerode_term)

! subroutine to write out files in Tecplot format

! INPUT: h         = current topography
!        h0        = height of the bedrock-alluvium interface
!        x,y       = x- and y-nodal coordinates
!        configData%nnode     = number of nodes
!        vertices  = triangles connectivity
!        nt        = number of triangles
!        prec      = precip
!        time      = current time


            type(config) :: configData
            real(8) :: dttol
            integer(4) :: i, k
            character(4) :: timenice
            integer(4), dimension(configData%nnode) :: temp, catch


!      real     h(configData%nnode),h0(configData%nnode),x(configData%nnode),y(configData%nnode)
!      real     prec(configData%nnode),dttol,isodh(configData%nnode),slope(configData%nnode)
!      real     dhfluvial(configData%nnode),dhdiffusion(configData%nnode),dhls(configData%nnode),global_cascade_dt
!      integer  vertices(3,nt),timeint
!      character*4 timenice
!      integer  temp(configData%nnode),catch(configData%nnode)
!      real gbalance(configData%nnode),sediment(configData%nnode),hiso(configData%nnode)
!      real dhglacier(configData%nnode),iceth(configData%nnode),tott(configData%nnode),slide(configData%nnode)
!      real strict(configData%nnode),totalerosion(configData%nnode),surface(configData%nnode)
!      real gerode_term(configData%nnode)
      

! round current time to something nice for filename
      timeint=timeint+1
!      print *,'timeint: ',timeint
      write (timenice,'(I4.4)') timeint
!      if (timeint.ge.1000) then
!        write (timenice,'I4') timeint
!      else if (timeint.ge.100) then
!        write (timenice,'I3') timeint
!      else if (timeint.ge.10) then
!        write (timenice,'I2') timeint
!      else if (timeint.ge.0) then
!        write (timenice,'I1') timeint
!      else
!        print *,'Unusual timesteps for tecplot output.  Exiting...'
!        stop
!      endif

! open files - hard-coded for now (11/06)
      print *, "write_tecplot_output.f90, timenice: ", timenice
      open (47,file='output/IceCascade/topo_tec_'//timenice//'.dat',status='unknown')
! Write header(s)
      write(47,'(A)') 'TITLE = "Cascade model topography"'
      write(47,'(A)') 'VARIABLES = "1: x [km]" "2: y [km]" "3: z [km]" "4: node" "5: Precipitation [m/y]" &
      & "6: Fluvial Erosion Rate [m/y]" "7: Diffusion Erosion Rate [m/y]" &
      & "8: Landslide Erosion Rate [m/y]" "9: Total Erosion Rate [m/y]" "10: Catchment Color" &
      & "11: Catchment Number" "12: Glacial Erosion Rate [m/y]" "13: Ice Thickness [m]" &
      & "14: Mass Balance [1/y]" "15: Total Topography [m]" "16: Sliding Velocity [m/y]"  &
      & "17: Gerode Term [m/y]" "18: Rock/Alluvium Contact [km]" "19: Isostatic deflection [m/y]" &
      & "20: Slope [m/km]" "21: Totalflexiso [m]" "22: Constriction" "23: Cumulative Erosion [m]" &
      & "24: Surface Area [km*km]"'

      write(47,'(A)') 'ZONE T = "Cascade"'
      write(47,900) 'n=',configData%nnode,', e=',nt,', et=triangle, f=fepoint'

! Write topography
      do i=1,configData%nnode
          dttol=memory(i,2)+memory(i,3)+memory(i,9)-memory(i,8)

          call writeFloat(x(i))                          ! 1: x
          call writeFloat(y(i))                          ! 2: y
          call writeFloat(h(i)/1000.0_8)                 ! 3: z
          write(47,"(i12)",advance='no') i               ! 4: node id
          call writeFloat(prec(i))                       ! 5: Precipitation
          call writeFloat(memory(i,2)/global_cascade_dt) ! 6: fluvial erosion rate
          call writeFloat(memory(i,3)/global_cascade_dt) ! 7: diffusion erosion rate
          call writeFloat(memory(i,8)/global_cascade_dt) ! 8: landslide erosion rate
          call writeFloat((dttol)/global_cascade_dt)     ! 9: total erosion rate
          write(47,"(i12)",advance='no') temp(i)         ! 10: catchment color
          write(47,"(i12)",advance='no') catch(i)        ! 11: catchment number
          call writeFloat(dhg(i)/global_cascade_dt)      ! 12: glacial erosion rate
          call writeFloat(iceth(i))                      ! 13: ice thickness
          call writeFloat(gbalance(i))                   ! 14: mass balance
          call writeFloat(tott(i))                       ! 15: total topography
          call writeFloat(slide(i))                      ! 16: sliding velocity
          call writeFloat(gerode_term(i))                ! 17: gerode term
          call writeFloat(h0(i)/1000.0_8)                ! 18: rock / alluvium contact
          call writeFloat(isodh(i))                      ! 19: isostatic deflection
          call writeFloat(slope(i))                      ! 20: slope
          call writeFloat(memory(i,4))                   ! 21: total flexiso
          call writeFloat(strict(i))                     ! 22: constriction
          call writeFloat(totalerosion(i))               ! 23: cumulative erosion
          call writeFloat(memory(i,7))                   ! 24: surface area
          write(47, "(a)") ""

!      write(47,901) x(i),&
!        y(i),&
!        h(i)/1000,&
!        i,&
!        prec(i),&
!        memory(i,2)/global_cascade_dt,&
!        memory(i,3)/global_cascade_dt,&
!        memory(i,8)/global_cascade_dt,&
!        (dttol)/global_cascade_dt,&
!        temp(i),&
!        catch(i),&
!        memory(i,9)/global_cascade_dt,&
!        iceth(i),&
!        gbalance(i),&
!        tott(i),&
!        slide(i),&
!        gerode_term(i),&
!        h0(i)/1000,&
!        isodh(i),&
!        slope(i),&
!        memory(i,4),&
!        strict(i),&
!        totalerosion(i),&
!        memory(i,7)
      enddo

! Write connectivities
      do k=1,nt
      write (47,902) (vertices(i,k),i=1,3)
      enddo

! Formatting
 900  format (A2,I12,A4,I12,A24)
! 901  format (F16.8,F16.8,F16.8,I12,F16.8,F16.8,F16.8,F16.8,F16.8,&
!              I12,I12,F16.8,F16.8,F16.8,F16.8,F16.8,F16.8,F16.8,&
!              F16.8,F16.8,F16.8,F16.8,F16.8,F16.8)
 902  format (I12,I12,I12)

! close files
      close(47)

          end subroutine write_tecplot_output
        end module m_write_tecplot_output


