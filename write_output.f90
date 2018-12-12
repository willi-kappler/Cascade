! write_output

        module m_write_output
            contains

            subroutine write_output(configData, ifirst)
                use rt_param
                use cascade_globals
                use m_write_tecplot_output
                implicit none

!      subroutine write_output &
!           (h,h0,x,y,fix,configData%nnode,nnodemax,iadapt,ifirst,vertices,nt, &
!            ndon,param,nparam,water,dhfluvial,dhdiffusion,dhls, &
!            ncat,nlake,time,global_cascade_dt,prec,slope,dslope,timeint,configData%tecflag, &
!            iceth,tott,slide,dhglacier,gbalance,sediment,isodh, &
!            hiso,strict,totalerosion,surface,gerode_term)

! subroutine to write output to a series of files in ASCII

! INPUT: h         = current topography
!        h0        = height of the bedrock-alluvium interface
!        x,y       = x- and y-nodal coordinates
!        fix       = boundary conditions = memory(1,5)
!        configData%nnode     = number of nodes
!        nnodemax  = maximum number of nodes
!        iadapt    = flag to allow dynamic remeshing (=0 no; =1 yes)
!        ifirst    = =1 means first zeroth time step (used to store
!                    initial geometry and parameters)
!        vertices  = triangles connectivity
!        nt        = number of triangles
!        ndon      = donor array
!        param     = erosion parameters
!        nparam    = number of erosion parameters
!        water     = discharge
!        slope     = slope [m/km]
!        dhfluvial = fluvial erosion increment over the time step = memory(1,2)
!        dhdiffusion= diffusion erosion increment over the time step = memory(1,3)
!        ncat      = catchment names
!        nlake     = lake flag
!        time      = current time
!        slope     = downstream slope found at beginning of time step
!        dslope    = downstream slope found after all erosion [m/m]
!        dhls = memory(1,8)

! subroutines called:
! NONE

!      real     h(configData%nnode),h0(configData%nnode),x(configData%nnode),y(configData%nnode),isodh(configData%nnode)
!      real     dhfluvial(configData%nnode),dhdiffusion(configData%nnode),dhls(configData%nnode)
!      real     param(nnodemax,nparam),water(configData%nnode),prec(configData%nnode)
!      real     slope(configData%nnode),dslope(configData%nnode),hiso(configData%nnode)
!      integer  vertices(3,nt),ndon(configData%nnode),ncat(configData%nnode),nlake(configData%nnode)
!      real     fix(configData%nnode),gerode_term(configData%nnode)
!      integer  timeint,configData%tecflag
!      integer  temp(configData%nnode),catch(configData%nnode)
!      real     gbalance(configData%nnode),sediment(configData%nnode)
!      real     dhglacier(configData%nnode),iceth(configData%nnode),tott(configData%nnode),slide(configData%nnode)
!      real  strict(configData%nnode),totalerosion(configData%nnode),surface(configData%nnode)
      

            type(config) :: configData
            integer(4), dimension(configData%nnode) :: temp, catch
            integer(4) :: i, j, k, lcat, mcat, ncat0
            logical :: ifirst

! the 'temp' variable stores the catchment color value of each node for use
! in the tecplot output
! The 'catch' variable stores the catchment number for each node
 
! initialize memory

        do i=1,configData%nnode
            temp(i) = 0
            catch(i) = 0
        enddo
 
! topography 
      write (7,*) 'TIME ',time,configData%nnode     
        do i=1,configData%nnode
        write (7,*) i,h(i),h(i)-h0(i)
        enddo

! connections
! write connections at all time steps DS 8/14/1
!        if (iadapt.eq.1 .or. ifirst.eq.1) then
      if (.not.configData%tecflag) then
        write (8,*) 'TIME ',time,nt
          do k=1,nt
          write (8,*) k,(vertices(i,k),i=1,3)
          enddo
!        endif
      endif
! donors
      if (.not.configData%tecflag) then
        if (.not.ifirst) then
        write (9,*) 'TIME ',time,configData%nnode
          do i=1,configData%nnode
          write (9,*) i,ndon(i)
          enddo
        endif
      endif

! geometry
! write geometry at all time steps DS 8/14/1
!        if (iadapt.eq.1 .or. ifirst.eq.1) then
        write (10,*) 'TIME ',time,configData%nnode
          do i=1,configData%nnode
          write (10,*) i,y(i),x(i),memory(i, 5)
          enddo
!        endif

! parameters
      if (.not.configData%tecflag) then
      write (11,*) 'TIME ',time,nparam,configData%nnode
        do i=1,configData%nnode
        write (11,*) (param(i,k),k=1,nparam)
        enddo
      endif

! discharge
      if (.not.configData%tecflag) then
        if (.not.ifirst) then
        write (12,*) 'TIME ',time,configData%nnode
          do i=1,configData%nnode
          write (12,*) i,water(i)
          enddo
        endif
      endif

! precipitation - added by DW 10/06
      if (.not.configData%tecflag) then
        if (.not.ifirst) then
        write (77,*) 'TIME ',time,configData%nnode
          do i=1,configData%nnode
          write (77,*) i,prec(i)
          enddo
        endif
      endif

! erosion rate
      if (.not.configData%tecflag) then
        if (.not.ifirst) then
        write (13,*) 'TIME ',time,configData%nnode
          do i=1,configData%nnode
          write (13,*) i, memory(i,2)/global_cascade_dt, &
                          memory(i,3)/global_cascade_dt, &
                          memory(i,8)/global_cascade_dt, &
                          (memory(i,2) + memory(i,3) + memory(i,8)) / global_cascade_dt
          enddo
        endif
      endif

! catchements - the 'temp' variable is used to store the catchment values
! of each node and then is sent to the write_tecplot_output.f function
! to be writin to the tecplot formatted output files
      if (.not.configData%tecflag) then
        if (.not.ifirst) then
        mcat=0
        write (14,*) 'TIME ',time,configData%nnode
1111    lcat=0
          do i=1,configData%nnode
            if (ncat(i).gt.0) then
            lcat=1
            ncat0=ncat(i)
            mcat=mcat+1
            write (14,*) 'Cat ',mcat
              do j=i,configData%nnode
                if (ncat(j).eq.ncat0) then
                write (14,*) j
                ncat(j)=-ncat(j)
                catch(j)=mcat
                if (MOD(mcat,10) .eq. 0) then
                  temp(j)=10
                else
                  temp(j)=MOD(mcat,10)
                endif
                endif
              enddo
            endif
          if (lcat.eq.1) goto 1111
          enddo
          do i=1,configData%nnode
          ncat(i)=-ncat(i)
          enddo
        endif
      endif

! lakes
      if (.not.configData%tecflag) then
        if (.not.ifirst) then
        write (15,*) 'TIME ',time,configData%nnode
          do i=1,configData%nnode
          if (nlake(i).eq.1) write (15,*) i
          enddo
        endif
      endif

! slope-area
      if (.not.configData%tecflag) then
        if (.not.ifirst) then
         write (17,*) 'TIME',time,configData%nnode
         do i=1,configData%nnode
          write (17,*) i,-dslope(i),water(i)
         enddo
        endif
      endif

! regolith
      
!      write (16,*) 'TIME ',time,configData%nnode
!        do i=1,configData%nnode
!        reg_thick(i) = h(i)-h0(i)
!       if (reg_thick(i).lt.1.E-8) reg_thick(i) = 1.E-8
!        write (16,*) i, h(i)-h0(i) 
!        enddo

! Make tecplot formatted output files (dwhipp 11/06)
      call write_tecplot_output (configData, temp, catch)
!      call write_tecplot_output (h,h0,x,y,configData%nnode,vertices,nt,prec,time, &
!                                 timeint,dhfluvial,dhdiffusion,dhls,global_cascade_dt, &
!                                 temp,catch,iceth,tott,slide, &
!                                 dhglacier,gbalance,sediment,isodh,slope, &
!                                 hiso,strict,totalerosion,surface,gerode_term)

! Make topography files to be loaded into Pecube (dwhipp 02/07)
!      call write_pecube_topo (h,configData%nnode,timeint,run_name,nrun_name)

          return
          end subroutine write_output
        end module m_write_output

