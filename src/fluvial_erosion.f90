! fluvial_erosion

        module m_fluvial_erosion
            contains

              subroutine fluvial_erosion (configData)
                    use rt_param
                    use cascade_globals
                    use m_debug
                    use m_find_order
                    use m_check_var

                    implicit none
!      subroutine fluvial_erosion (xkf,xlf_BR,xlf_AL,x,y,h,h0,hi,ndon,nnode, &
!                                  surface,slope,length,water,sediment,&
!                                  ibucket,dt,fix,dhh,nb,nn,nbmax,iorder,itype_node, &
!                                  dhminfluv,dhmaxfluv,nlake, &
!                                  sea_level,outflux,ideposition, &
!                                  width_c,thresh,glacier,dhglacier)

! subroutine to calculate fluvial erosion

! INPUT: xkf         = fluvial erosion constant [] = param(1,1)
!        xlf_BR      = bedrock erosion length scale [m] = param(1,2)
!        xlf_AL      = alluvials erosion length scale [m?]
!        x,y         = x- and y-nodal positions [km]
!        h           = present topography [m]
!        h0          = bedrock-alluvion interface
!        hi          = initial topography (at time 0)
!        ndon        = donor array
!        nnode       = number of nodes
!        surface     = surface associated with each node, [km2] = memory(1,7)
!        slope       = slope associated with each nodal link (stream) [m/km]
!        length      = length associated with each nodal link (stream) [km]
!        ibucket     = working array
!        dt          = time step length [yr]
!        fix         = boundary conditoin array = memory(1,5)
!        dhh         = amount of material eroded/deposited over the time step = memory(1,2)
!        nb          = number of natural neighbours per node
!        nn          = list of natural neighbours
!        nbmax       = maximum of natural neighbours
!        iorder      = working array containing the proper sequence in node
!                      number in which to perform the river erosion operations
!        itype_node  = type associated to each node (see later in code)
!        dhminfluv   = minimum amount removed by river erosion
!        dhmaxfluv   = maximum amount removed by river erosion
!        nlake       = determines whether a node is part of a lake or not
!        sea_level   = sea level
!        ideposition = to prevent deposition by rivers (=0)
!        width_c     = coef controling channel width as fnct of discharge [sqrt(yr/m)]
!        width_ch    = effective channel width [km]
!        thresh      = theshold for discharge for channel formation [m km2/yr]
!    glacier     = integer array of the location of glaciers (1=glacier, 0=no glacier)
!   dhglacier    =array of eroded depth from glacier routine
! OUTPUT:  several arrays are updated:
!        h           = new current topography [m]

!          the contribution from river incision to outflux is calculated

!          the following arrays are filled:
!        water       = water discharge at each point
!        sediment    = sediment load at each point

! subroutines called:
! - debug
! - find_order

! changed elevation update statements to include fixed BC's DS 11/18/1

            type(config) :: configData
            integer(4) :: i, i_sea_dep, jorder, norder
            real(8), dimension(:), allocatable :: h1
            real(8) :: slop, dh, dh1, dhmax, dsed_al_cell, dsed_al_ch
            real(8) :: dsed_ch, dsediment, sedeq, sedeqb, width_ch

!      real*4     x(nnode),y(nnode),h(nnode),h0(nnode),hi(nnode)
!      real*4     xkf(nnode),xlf_BR(nnode)
!      real*4     surface(nnode),dhh(nnode)
!      real*4     water(nnode),sediment(nnode)
!      real*4     slope(nnode),length(nnode)
!      integer*4  ndon(nnode)
!      integer*4  ibucket(nnode)
!      real       fix(nnode),dhglacier(nnode)
!      integer    glacier(nnode)
!      integer*4  nb(*),nn(nbmax,*)
!      integer*4  iorder(nnode),itype_node(nnode)
!      real  Sedcap(nnode),h1(nnode)
!      integer*4  nlake(nnode)
!      integer i_sea_dep
! sets all ibucket to 0
! sets all water to 1 and all sediment to 0
! note that water(i) can be different for every node; this is where a more
!  "complex" orographic model should be included...

        allocate(h1(configData%nnode))


      do i=1,configData%nnode
       ibucket(i)=0
       sediment(i)=0.0_8
       memory(i,2)=0.0_8
       h1(i)=h(i)
      enddo

      i_sea_dep=0
      dh = 0.0_8

! finds proper ordering (cascade algorithm)

      if (ivocal) call debug ('find_order$')
      call find_order (ibucket,ndon,iorder,configData%nnode,norder)
      if (ivocal) call debug ('fluvial_erosion$')

! itype_node determines the type of node:
!   -1:   local minima next to (or on) a fixed boundary
!    0:   diffusion only
!    1:   channel because one of its parents was a channel
        do i=1,configData%nnode
            itype_node(i)=0
            call check_var(1, "fluvial_erosion, sediment, 1$", sediment(i), i)
        enddo

        do jorder=1,norder
          i=iorder(jorder)

          if (i < 1 .or. i > configData%nnode) then
            print *, "fluvial_erosion: i out of bounds: ", i
          endif

          call check_var(1, "fluvial_erosion, sediment, 2$", sediment(i), i)

          slop=-slope(i)
! special treatment for self donors
          if (ndon(i).eq.i) then
              dh=0.0_8
              itype_node(i)=-2
              outflux=outflux+sediment(i)*(1.0_8-memory(i,5))*(1.0_8-dble(glacier(i)))
              sediment(i)=0.0_8
              water(i)=0.0_8
              call check_var(1, "fluvial_erosion, sediment, 3$", sediment(i), i)

! special treatment for nodes below sea level
          elseif (h(i).le.configData%sea_level) then
              dh=sediment(i)*(1.0_8-dble(glacier(i)))/memory(i,7)
              call check_var(1, "fluvial_erosion, sediment, 4$", sediment(i), i)
              if (i_sea_dep.eq.0) dh=0.0_8
              h(i)=h(i)+dh*memory(i,5)
              call check_var(2, "fluvial_erosion, h(i), 1$", h(i), i)
              itype_node(i)=-1
              sediment(i)=0.0_8
          else

! normal nodes
         if (itype_node(i).ne.0) itype_node(i)=1

         sedeqb=param(i,1)*slop*water(i)*global_cascade_dt

         width_ch=configData%width_c*sqrt(water(i))

         if(water(i).lt.configData%thresh) width_ch=999999999.0_8



! special treatment for lake nodes
         if (nlake(i).eq.1) then
          dh=sediment(i)*(1.0_8-dble(glacier(i)))/memory(i,7)
          h(i)=h(i)+dh*memory(i,5)
          call check_var(2, "fluvial_erosion, h(i), 2$", h(i), i)
          sediment(i)=0.0_8
         elseif (sediment(i).ge.sedeqb.and.configData%ideposition) then
          dh=(sediment(i)-sedeqb)*(1.0_8-dble(glacier(i)))/memory(i,7)
          if (water(i).ne.0.0_8) then
           dhmax=sediment(i)*length(i)/param(i,1)/water(i)/global_cascade_dt+h(ndon(i))-h(i)
          call check_var(2, "fluvial_erosion, dhmax, 2$", dhmax, i, global_cascade_dt, h(ndon(i)), h(i))
          else
           dhmax=dh
          call check_var(1, "fluvial_erosion, dhmax, 3$", dhmax, i)
          endif

          if (dh.le.dhmax) then
           sediment(i)=sedeqb
           h(i)=h(i)+dh*memory(i,5)
          call check_var(2, "fluvial_erosion, h(i), 3$", h(i), i)
          else
           dh=dhmax
          call check_var(1, "fluvial_erosion, dhmax, 1$", dhmax, i)
           sediment(i)=sediment(i)-dhmax*memory(i,7)
          call check_var(2, "fluvial_erosion, sediment(i), 1$", sediment(i), i)
           h(i)=h(i)+dh*memory(i,5)
          call check_var(1, "fluvial_erosion, dh, 1$", dh, i)
          call check_var(2, "fluvial_erosion, h(i), 4$", h(i), i)
          endif

! erosion
         else

! first case: eroding bedrock only
          if (h(i).le.h0(i)) then
           dsed_ch=(sedeqb)*(length(i)/param(i,2))
           dh=-(dsed_ch)*(1.0_8-dble(glacier(i)))/(width_ch*length(i))
           dsediment=-dh*memory(i,7)

           h(i)=h(i)+dh*memory(i,5)
          call check_var(2, "fluvial_erosion, h(i), 5$", h(i), i)

           sediment(i)=sediment(i)+dsediment

! second case: eroding alluvions only
          else
           dsed_ch=(sedeqb)*(length(i)/configData%xlf_AL)
           dh=-(dsed_ch)*(1.0_8-dble(glacier(i)))/(width_ch*length(i))
            if (dh.ge.h0(i)-h(i)) then
             dsediment=-dh*memory(i,7)

             h(i)=h(i)+dh*memory(i,5)
          call check_var(2, "fluvial_erosion, h(i), 6$", h(i), i)

             sediment(i)=sediment(i)+dsediment

! third case: eroding alluvions and bedrock
            else
             dh1=(h(i)-h0(i))*(1.0_8-dble(glacier(i)))
             dsed_al_ch=dh1*length(i)*width_ch

!   added by BJY 101509 because sedeq wasn't defined.  This is now consistent with a linear cover model.
             sedeq=sedeqb-dsed_al_ch
             dsed_ch=sedeq*(length(i)/param(i,2))

             dh=-(dsed_ch)*(1.0_8-dble(glacier(i)))/(width_ch*length(i))
             dsediment=-dh*memory(i,7)
             dsed_al_cell=dh1*memory(i,7)

             h(i)=h0(i)+dh*memory(i,5)
          call check_var(2, "fluvial_erosion, h(i), 7$", h(i), i)
!   had to move 1-glacier term to dsed_al_cell= satement above because it was too long (>72 columns) and the continuation wasn't working
             sediment(i)=sediment(i)+dsediment+dsed_al_cell
            endif
          endif

         endif

         dhminfluv=min(dhminfluv,dh)
         dhmaxfluv=max(dhmaxfluv,dh)
!         dhminfluv=amin1(dhminfluv,dh)
!         dhmaxfluv=amax1(dhmaxfluv,dh)
          
!  add glacier sediment
         sediment(i)=sediment(i)+(-dhg(i)*memory(i,7))
!  The following is where sediment and water is passed to the next downstream node.
         if (ndon(i).ne.i.and.h(i).gt.configData%sea_level) then
          water(ndon(i))=water(ndon(i))+water(i)
          sediment(ndon(i))=sediment(ndon(i))+sediment(i)
          if (slop.ne.0.0_8) itype_node(ndon(i))=1

         endif
         endif
!   Sedcap(i)=sedeqb
    
!         dhh(i)=dh*real(fix(i))
         memory(i,2)=(h(i)-h1(i))*memory(i,5)

! re-setting the height of the border nodes to zero, in case they have
! been changed within this routine
!         if (fix(i).eq.0) h(i)=0.

               enddo

            deallocate(h1)

          return

          end subroutine fluvial_erosion
        end module m_fluvial_erosion

