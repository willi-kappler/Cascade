! update_time_step
        module m_update_time_step
            contains
            subroutine update_time_step(configData, iaction)
                use rt_param
                use cascade_globals
                implicit none

!       subroutine update_time_step (global_cascade_dt,configData%shallow_ice,dtold,xkf,configData%nnode,configData%iadjust,delta,iaction, &
!                                    dhminfluv,dhmaxfluv,dhmindiff,dhmaxdiff,time, &
!                                    shorttime,configData%writetime,configData%endtime,itime,dtmin,dtmax,&
!                                    dhminglac,dhmaxglac)

! subroutine to update time step based on Peter's modifications

! INPUT: global_cascade_dt           = current time step length
!        configData%shallow_ice          = original time step length
!        dtold        = used to avoid missing a writing time step
!        xkf          = fluvial constant [] = param(1,1)
!        configData%nnode        = number of nodes
!        configData%iadjust      = if configData%iadjust=0, global_cascade_dt=configData%shallow_ice always
!        delta        = mean nodal spacing
!        iaction      = 0 initialization phase
!                       1 update pahse
!        dhminfluv    = minimum erosion by fluvial action
!        dhmaxfluv    = maximum erosion by fluvial action
!        dhmindiff    = minimum erosion by diffusion
!        dhmaxdiff    = maximum erosion by diffusion
!        time         = current time
!        shorttime    = used to avoid missing a writing time step
!        configData%writetime    = time at which the next saving is required
!        configData%endtime      = time at which this run terminates
!        itime        = current time step

! OUTPUT:  global_cascade_dt          = new time step length
 
! subroutines called:
! NONE

            type(config) :: configData
            logical, intent(in) :: iaction
            real(8) :: xkfmax
            integer(4) :: i

! initialization phase
      if (.not.iaction) then

        xkfmax=param(1,1)
        do i=1,configData%nnode
          xkfmax=max(param(i,1),xkfmax)
        enddo
        
     
            dtmax=100.0_8*(0.5_8*delta)/xkfmax
            print *, "delta: ", delta
            print *, "xkfmax: ", xkfmax
            print *, "configData%xkf: ", configData%xkf
            print *, "dtmax: ", dtmax
!        dtmin=dtmax/1.e4

        global_cascade_dt = configData%dt0

        dtold=0.0_8
        time=0.0_8
        shorttime=0.0_8
        itime=1
      

! update phase
      else
    
!    adjust minimum timescale to catch potential glacial issues
      if ((dhminglac.lt.-1.e-3_8).or.(dhmaxglac.gt.1.e-3_8)) then
!   dtmax=100.*(0.5*delta)/xkfmax
!        dtmin=1
!   dtmax/5.e5
!   dtmax/1.e6
!        print*,'update global_cascade_dt min',dtmin

      else
            dtmin=dtmax/1.e4_8
      end if


        if ((dhminfluv.lt.-1.e-3_8).or.(dhmindiff.lt.-1e-3_8).or.(dhminglac.lt.-1.e-3_8)) then
            global_cascade_dt=global_cascade_dt/2.0_8
!            call check_var(4, "update_time_step, global_cascade_dt, 2$", global_cascade_dt, 0)
        else
          if ((dhmaxfluv.lt.1.e-5_8).and.(dhmaxdiff.lt.1e-5_8).and. &
             (dhmaxglac.lt.1.e-5_8)) then
                global_cascade_dt=global_cascade_dt*2.0_8
!                call check_var(4, "update_time_step, global_cascade_dt, 3$", global_cascade_dt, 0)
          end if
        endif

        if ((dhmaxfluv.gt.1.e-3_8).or.(dhmaxdiff.gt.1e-3_8).or.(dhmaxglac.gt.1.e-3_8)) then
          global_cascade_dt=global_cascade_dt/2.0_8
!          call check_var(4, "update_time_step, global_cascade_dt, 4$", global_cascade_dt, 0)
        else
          if ((dhminfluv.gt.-1.e-5_8).and.(dhmindiff.gt.-1e-5_8).and. &
              (dhminglac.gt.-1.e-5_8)) then
                global_cascade_dt=global_cascade_dt*2.0_8
!                call check_var(4, "update_time_step, global_cascade_dt, 5$", global_cascade_dt, 0)
          end if
        endif

! global_cascade_dt must be bound to some finite values otherwise you may run into trouble

        if (global_cascade_dt > dtmax) then
            global_cascade_dt=dtmax
!            call check_var(4, "update_time_step, global_cascade_dt, 6$", global_cascade_dt, 0)
        endif

        if (global_cascade_dt < dtmin) then
            global_cascade_dt=dtmin
!            call check_var(4, "update_time_step, global_cascade_dt, 7$", global_cascade_dt, 0)
        endif
!   print*,global_cascade_dt,dtmax,dtmin,dhmaxglac,dhminglac
! shortcircuit the dynamic time step adjustment if configData%iadjust=0
        if (.not.configData%iadjust) then
            global_cascade_dt=configData%dt0
!            call check_var(4, "update_time_step, global_cascade_dt, 8$", global_cascade_dt, 0)
        end if

! updates time step to make sure that we dont miss an output time
! or the end of the run

        if ((shorttime+global_cascade_dt) > dble(configData%writetime)) then
            dtold=global_cascade_dt
            global_cascade_dt=dble(configData%writetime)-shorttime
!            call check_var(4, "update_time_step, global_cascade_dt, 9$", global_cascade_dt, 0)
        endif

        if ((time+global_cascade_dt) > configData%endtime) then
            global_cascade_dt=configData%endtime-time
!            call check_var(4, "update_time_step, global_cascade_dt, 10$", global_cascade_dt, 0)
        end if

        itime=itime+1

!        print *, "itime: ", itime
        print *, "time: ", time
        print *, "cascade_istep: ", cascade_istep
        print *, "dtmin: ", dtmin
!        print *, "dtmax: ", dtmax
!        print *, "shorttime: ", shorttime
        print *, "global_cascade_dt: ", global_cascade_dt
!        print *, "configData%writetime: ", configData%writetime
!        print *, "configData%endtime: ", configData%endtime
!        print *, "configData%nshortwrite: ", configData%nshortwrite
!        print *, ""

        call flush()

      endif

      return

            end subroutine update_time_step
        end module m_update_time_step

