    module m_calve
        contains
        subroutine calve(nx,ny,h_tmp,t,ht,sea_level,dx,dy,dtice)

! subroutine created by BJY 102609 to calve the front of glaciers with a bed below sea level.  based on highly empirical formulation in Patterson


!inputs
!nx    number of cells in x direction
!ny    number of cells in y direction
!h_tmp     ice thickness [km]
!t     land elevation
!sea_level   sea level
!dx    grid spacing in x direction [m]
!dy    grid spacing in y direction [m]
!dtice   timestep for the ice calcultation

!output 
! h_tmp    updated ice thickness [km]
! calvedh  height calved to check for mass balance

!working
!  iceavail  amount of ice available to calve in a margin cell
!  icemargin  integer array of 1's and 0's, 1 if the cell is an ice margin out at sea (i.e. susceptible to calving)
!   icebelow  integer array denoting if a cell is covered in ice AND has a bedrock elevation below sea level


        integer(4), dimension(:,:), allocatable :: icemargin,icebelow,iceloc,zbelow,sumiz
        real(8), dimension(:,:), allocatable :: waterdepth,calverate,calveflux,calveheight
        real(8), dimension(:,:), allocatable :: iceavail,timeemp
        integer(4) :: nx,ny,i,j
        real(8) :: calvecoef,dtCalve,subdt,emptycelltime
        real(8) :: h_tmp(nx,ny),t(nx,ny),ht(nx,ny),sea_level,dx,dy,dtice

        allocate(icemargin(nx,ny))
        allocate(icebelow(nx,ny))
        allocate(iceloc(nx,ny))
        allocate(zbelow(nx,ny))
        allocate(sumiz(nx,ny))
        allocate(waterdepth(nx,ny))
        allocate(calverate(nx,ny))
        allocate(calveflux(nx,ny))
        allocate(calveheight(nx,ny))
        allocate(iceavail(nx,ny))
        allocate(timeemp(nx,ny))

! value=17 taken from Patterson, should move to ice.in at some point.  Hooke's book reports a value of 2 for freshwater, 27 for Alaska marine   
calvecoef=2.0_8
icemargin=0
! calving timestep can become variable (see below)
dtCalve=dtice
!print*,dtCalve,dtice
 do while (dtCalve.gt.0.0_8)
! need to find...
    ! 1, where there are glaciers
        icebelow=0
        iceloc=0
        where(h_tmp.gt.1.0_8) iceloc=1
    !2 where the land surface is belwo sealevel
        zbelow=1
        where(t.lt.sea_level) zbelow=0
    !3 where glaciers have a bed below sealevel
        where(iceloc.eq.1.and.zbelow.eq.0) icebelow=1
        
    !4 the ice margin:  so check neighbors:it is a margin if one of its neigbors is below sea-level AND there is no ice on that neighbor.  There may be a faster way to do this with cshift or something
        sumiz=iceloc+zbelow
        do j=1,ny
            do i=1,nx
                if (icebelow(i,j).eq.1) then
                    if (sumiz(i+1,j).eq.0.or.sumiz(i-1,j).eq.0.or.sumiz(i,j+1).eq.0.or.sumiz(i,j-1).eq.0) then
                        ! ice margin
                        
                        icemargin(i,j)=1            
!                       print*,h_tmp(i,j),t(i,j),i,j
                    end if
                endif
            enddo
       enddo    
                
        waterdepth=(sea_level-t)*dble(icemargin)
!       print*,'waterdepth',maxval(waterdepth),minval(waterdepth)
        where (waterdepth.lt.0.0_8) waterdepth=0.0_8
        calverate=calvecoef*waterdepth
!       print*,'calverate',maxval(calverate),minval(calverate)
!       print*,'dxdy',dx,dy
        ! the 10e-10 is to avoid a divide by zero later.  Is fixed below at calveheight, make sure dx and dy are equal, if not, calving algorithm needs to be changed.
        calveflux=(dx*h_tmp*calverate)+(10e-10_8)
!       print*,'calveflux',maxval(calveflux),minval(calveflux)
        iceavail=dx*dy*h_tmp*dble(icemargin)
!       print*,'iceavail',maxval(iceavail),minval(iceavail)
! make sure you don't remove more ice than available in a timestep...
        timeemp=(iceavail/calveflux)
        where (h_tmp.le.1.0_8) timeemp=dtCalve
        emptycelltime=minval(timeemp)
        ! the following line is implemented if there is no ice in the grid
        if (emptycelltime.eq.0.0_8) emptycelltime=dtCalve+1.0_8
!       print*,'line76',emptycelltime,dtCalve
        subdt=min(dtCalve,emptycelltime)
        
        calveheight=dble(icemargin)*(calveflux-(10e-10_8))*subdt/(dx*dy)
! ensure that calving doesn't ever add ice becuase that would be stupid...
        where(calveheight.lt.0.0_8) calveheight=0.0_8
!       if (maxval(calveheight).gt.0) print*,'calvheight',maxval(calveheight), subdt,&
!           maxval(calveflux), calvecoef,maxval(h_tmp),maxval(calverate),maxval(waterdepth),&
!           maxval(icemargin),minval(t),sea_level
!       print*,'predh',maxval(icemargin*h_tmp),minval(icemargin*h_tmp)
        h_tmp=h_tmp-calveheight
        ht=ht-calveheight
!       print*,'postdh',maxval(icemargin*h_tmp),minval(icemargin*h_tmp)
        dtCalve=dtCalve-subdt
!       print*,'updated',dtCalve,subdt

end do 
        deallocate(icemargin)
        deallocate(icebelow)
        deallocate(iceloc)
        deallocate(zbelow)
        deallocate(sumiz)
        deallocate(waterdepth)
        deallocate(calverate)
        deallocate(calveflux)
        deallocate(calveheight)
        deallocate(iceavail)
        deallocate(timeemp)

        return
        end subroutine calve
    end module m_calve

