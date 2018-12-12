    module m_gerode_node
        contains
         subroutine gerode_node(configData)
                    use rt_param
                    use cascade_globals
                    implicit none
 !subroutine gerode_node(nnode,h,iceth,slide,alpha,dt,glacier,&
 !           dhminglac,dhmaxglac,dhg,sea_level,slope,x,y,nb,nn,nbmax,h0,&
 !           gerode_term)

! subroutine created by BJY 100809 to erode cascade nodes by glaciers generated in Ice

! input 
! nnode
! x
! y
! z
! iceth     ice thickness array created in interp_ice
! alpha
! dt



! output
! glacier  integer array that denotes if a glacier covers a node or not (1=glacier is there, 0=no glacier, operate on node as normal)


!Declaration

            type(config) :: configData
            real(8) :: rhoi, rhow, dhmin, noerode, wd,critdepth, dh, dht
            real(8) :: dx, dy, dd, slp_n, sedthick, slpt_n, srat
            real(8), dimension(:), allocatable :: ht
            integer(4) :: nmin, nbb, jnode, nnn, i


!    integer glacier(nnode)
!    real iceth(nnode),dhg(nnode),slide(nnode),alpha,dt,ht(nnode),slpt_n
!    real h(nnode),dhminglac,dhmaxlac,sea_level,wd,critdepth,slp_n,dhmin
!    real rhoi, rhow, noerode, slope(nnode),dh,dx,dy,dd,x(nnode),y(nnode),h0(nnode)
!    real sedthick, srat
!    integer nbb, jnode,nnn,nb(nnode),nn(nbmax,nnode),nmin
!    real gerode_term(nnode)

        allocate(ht(configData%nnode))

    rhoi=910.0_8
    rhow=1000.0_8
    where(iceth.lt.0.0_8) iceth=0.0_8
    do i=1,configData%nnode
        ht(i)=h(i)+iceth(i)
    enddo
! find where there is enough ice for glacial erosion 
      dhg=0.0_8
      dhminglac=0.0_8
      dhmaxglac=0.0_8
      do i=1,configData%nnode
      dhmin=0.0_8
      nmin=0
        if (iceth(i)>100.0_8) then
            memory(i,9)=1.0_8
        else
            memory(i,9)=0.0_8
        endif
         noerode=1.0_8
         wd=configData%sea_level-h(i)
       
         if (wd.lt.0.0_8) wd=0.0_8
         critdepth=iceth(i)*rhoi/rhow
!  added **3 on 112609 to try and reign in fjord growth.haven't runn it like this yet
         if (critdepth.gt.0.0_8) noerode=(critdepth-wd)**3/critdepth**3
         
         
         if (wd.gt.critdepth) noerode=0.0_8
!         if (slope(i).lt.-550) noerode=0.
         
! check if any neighbors are too steep
            nbb=nb(i)
! loop over the neighbours
          do jnode=1,nbb
            nnn=nn(jnode,i)
           dh=h(nnn)-h(i)
           dht=ht(nnn)-ht(i)
          dx=x(nnn)-x(i)
          dy=y(nnn)-y(i)
          dd=sqrt(dx**2+dy**2)
          slp_n=dh/dd
!          if (slp_n.lt.-550) noerode=0.
          if (slp_n.gt.700.0_8) noerode=0.0_8
          
          slpt_n=dht/dd
          if (slpt_n.lt.dhmin) then
          dhmin=slpt_n
          nmin=nnn
          
          endif
          enddo
          if (nmin.gt.0) then
            dh=h(nmin)-h(i)
           dht=ht(nmin)-ht(i)
          dx=x(nmin)-x(i)
          dy=y(nmin)-y(i)
          dd=sqrt(dx**2+dy**2)
          slp_n=dh/dd
            slpt_n=dht/dd
            srat=slp_n/slpt_n
            if (srat.lt.-0.5_8) then
!               print*,slpt_n,slp_n
               noerode=0.0_8
             endif
          endif
          
          
!          print*,slp_n
          
          
          
          
          
         
!         if (noerode.ne.1.and.noerode.ne.0) then    
!       print*,wd,critdepth,noerode,sea_level,h(i)
!       endif
      dhg(i)=-configData%erosion_rate*abs(slide(i)**configData%ice_ero_pow)*global_cascade_dt*memory(i,9)*noerode
! Frederic limits erosion to a 'maximum allowed' with a line like the following (commented out by BJY 100909  because I think it's a beat way to avoid numerical instabilities)
! dhgmax=0.05
! dhg=min(dhg,dhgmax*dt)

!  erode
     sedthick=h(i)-h0(i)
     if (sedthick.gt.dhg(i)) then
       h(i)=h(i)+dhg(i)
     elseif (sedthick.le.dhg(i)) then
       h(i)=h(i)+dhg(i)
       h0(i)=h(i)
     
     endif
      
       gerode_term(i)=noerode
      
      dhminglac=min(dhminglac,dhg(i))
      dhmaxglac=max(dhmaxglac,dhg(i))
    end do

        deallocate(ht)


! added for debugging
! 2012.07.12, Willi Kappler

    print *, "minErosionRate: ", minval(memory(:, 9))
    print *, "maxErosionRate: ", maxval(memory(:, 9))
    print *, "avgErosionRate: ", sum(memory(:, 9)) / dble(configData%nnode)

    return
    end subroutine gerode_node
    end module m_gerode_node

