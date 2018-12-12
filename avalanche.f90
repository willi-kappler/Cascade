        module m_avalanche
            contains
            subroutine avalanche(ht,dx,dy,h_tmp,dhdt_allowed,nx,ny,dt)
!  subroutine created by BJY 110509 to avalanche snow off of steep slopes.  It essentially cycles (only once per timestep right now) through nodes and find the steepest and if that is greater than critical, it moves enough material to the downslope node so that it is at critical
!!! inputs
! ht    total topogrpah
! h_tmp     ice thickness
! dx    
!dy
!nx,ny
!dhdt_allowed  maximum change in ice thickness allowed per yr.
!  critangle  input below, 'angle of repose' for snow. Lit searches suggest 25-60 degrees depending on properties of the snow.

!Outputs
!  the model just updates ht and t


        integer(4) :: nx,ny,i,j,oneway
        real(8) :: ht(nx,ny),h_tmp(nx,ny),dx,dy,dt,dhdt_allowed
        real(8) :: adh,dhcrity,dhcritx,dhcritxy
        real(8) :: dhu,dhl,dhul,dhur,dhll,dhlr,dhr,dhd
        real(8) :: critangle,dhdt_max,thu,tdhmax,thd,thl,thr,thul,thur
        real(8) :: thll,thlr,dxy
        
        
        !  calculate maximum height difference between neighbors and corners.  If heigh difference is > critical height (for crit slope) then move material
        
        critangle=30.0_8*3.14159265_8/180.0_8
        
        dhcritx=dx*(tan(critangle))
        dhcrity=dy*(tan(critangle))
        dhdt_max=dhdt_allowed*dt
        dxy=sqrt(dx**2 +dy**2)
        dhcritxy=dxy*(tan(critangle))

        do j=2,ny-1
            do i=2,nx-1 
            
                oneway=0    
                !only avalanche if there's snow there
                if (h_tmp(i,j).gt.0.0_8) then
                     ! get height difference in all directions and remeber max
                    dhu =ht(i,j)-ht(i,j+1)
                    thu=dhu/dy
                    tdhmax=thu
                    dhd =ht(i,j)-ht(i,j-1)
                    thd=dhd/dy
                    tdhmax=max(tdhmax,thd)
                    dhl =ht(i,j)-ht(i-1,j)
                    thl=dhl/dx
                    tdhmax=max(tdhmax,thl)
                    dhr =ht(i,j)-ht(i+1,j)
                    thr=dhr/dx
                    tdhmax=max(tdhmax,thr)
                    
                    dhul=ht(i,j)-ht(i-1,j+1)
                    thul=dhul/dxy
                    tdhmax=max(tdhmax,thul)
                    
                    
                    dhur=ht(i,j)-ht(i+1,j+1)
                    thur=dhur/dxy
                    tdhmax=max(tdhmax,thur)
                    
                    dhll=ht(i,j)-ht(i-1,j-1)
                    thll=dhll/dxy
                    tdhmax=max(tdhmax,thll)
                    
                    dhlr=ht(i,j)-ht(i+1,j-1)
                    thlr=dhlr/dxy
                    tdhmax=max(tdhmax,thlr)
                    
                    !only avalanche in the maximum dh direction.  the variable 'oneway' makes sure that you only avalanche in one direction and retains mass balance. (just in case two dh's are equal)
                    
                    if ((thu.eq.tdhmax).and.(dhu.ge.dhcrity).and.(oneway.eq.0)) then
                        adh=(dhu-dhcrity)/2.0_8
                        !print*,'1',adh, dhu, dhcrity
                        if (adh.gt.h_tmp(i,j)) adh=h_tmp(i,j)
                        !if (adh.gt.dhdt_max) adh=dhdt_max
                        !print*,'2',adh, dhu, dhcrity
                        h_tmp(i,j)=h_tmp(i,j)-adh
                        ht(i,j)=ht(i,j)-adh
                        
                        h_tmp(i,j+1)=h_tmp(i,j+1)+adh
                        ht(i,j+1)=ht(i,j+1)+adh         
                        
                        !print*,'avalanching up',adh, dhu, dhcrity
                        oneway=1
!                        avalanche_counter = avalanche_counter + 1
                    endif
                    
                    
                    if ((thd.eq.tdhmax).and.(dhd.ge.dhcrity).and.(oneway.eq.0)) then
                    
                        adh=(dhd-dhcrity)/2.0_8
                        if (adh.gt.h_tmp(i,j)) adh=h_tmp(i,j)
                        !if (adh.gt.dhdt_max) adh=dhdt_max
                        
                        h_tmp(i,j)=h_tmp(i,j)-adh
                        ht(i,j)=ht(i,j)-adh
                        
                        h_tmp(i,j-1)=h_tmp(i,j-1)+adh
                        ht(i,j-1)=ht(i,j-1)+adh 
                        
                        
                        
                        
                        !print*,'avalanching dw'
                        
                        oneway=1
!                        avalanche_counter = avalanche_counter + 1
                    endif



                    if ((thl.eq.tdhmax).and.(dhl.ge.dhcritx).and.(oneway.eq.0)) then
                    
                        adh=(dhl-dhcritx)/2.0_8
                        if (adh.gt.h_tmp(i,j)) adh=h_tmp(i,j)
                        !if (adh.gt.dhdt_max) adh=dhdt_max
                        
                        h_tmp(i,j)=h_tmp(i,j)-adh
                        ht(i,j)=ht(i,j)-adh
                        
                        h_tmp(i-1,j)=h_tmp(i-1,j)+adh
                        ht(i-1,j)=ht(i-1,j)+adh 
                        
                        !print*,'avalanching lf'
                        
                        oneway=1
!                        avalanche_counter = avalanche_counter + 1
                    endif

                    if ((thr.eq.tdhmax).and.(dhr.ge.dhcritx).and.(oneway.eq.0)) then
                    
                        adh=(dhr-dhcritx)/2.0_8
                        if (adh.gt.h_tmp(i,j)) adh=h_tmp(i,j)
                        !if (adh.gt.dhdt_max) adh=dhdt_max
                        
                        h_tmp(i,j)=h_tmp(i,j)-adh
                        ht(i,j)=ht(i,j)-adh
                        
                        h_tmp(i+1,j)=h_tmp(i+1,j)+adh
                        ht(i+1,j)=ht(i+1,j)+adh 
                        
                        !print*,'avalanching rt'
                        
                        oneway=1
!                        avalanche_counter = avalanche_counter + 1
                    endif

                    
                    
                    if ((thul.eq.tdhmax).and.(dhul.ge.dhcritxy).and.(oneway.eq.0)) then
                        
                        adh=(dhul-dhcritxy)/2.0_8
                        if (adh.gt.h_tmp(i,j)) adh=h_tmp(i,j)
                        
                        
                        !if (adh.gt.dhdt_max) adh=dhdt_max
                        
                        h_tmp(i,j)=h_tmp(i,j)-adh
                        ht(i,j)=ht(i,j)-adh
                        
                        h_tmp(i-1,j+1)=h_tmp(i-1,j+1)+adh
                        ht(i-1,j+1)=ht(i-1,j+1)+adh 
                        
                        !print*,'avalanching ul'
                        oneway=1
!                        avalanche_counter = avalanche_counter + 1
                    endif


                    if ((thur.eq.tdhmax).and.(dhur.ge.dhcritxy).and.(oneway.eq.0)) then
                    
                        adh=(dhur-dhcritxy)/2.0_8
                        if (adh.gt.h_tmp(i,j)) adh=h_tmp(i,j)
                        
                        
                        !if (adh.gt.dhdt_max) adh=dhdt_max
                        
                        h_tmp(i,j)=h_tmp(i,j)-adh
                        ht(i,j)=ht(i,j)-adh
                        
                        h_tmp(i+1,j+1)=h_tmp(i+1,j+1)+adh
                        ht(i+1,j+1)=ht(i+1,j+1)+adh 
                        
                        !print*,'avalanching ur'
                        oneway=1
!                        avalanche_counter = avalanche_counter + 1
                    endif
                    
                    
                
                    if ((thlr.eq.tdhmax).and.(dhlr.ge.dhcritxy).and.(oneway.eq.0)) then
                    
                        adh=(dhlr-dhcritxy)/2.0_8
                        if (adh.gt.h_tmp(i,j)) adh=h_tmp(i,j)
                        
                        
                        !if (adh.gt.dhdt_max) adh=dhdt_max
                        
                        h_tmp(i,j)=h_tmp(i,j)-adh
                        ht(i,j)=ht(i,j)-adh
                        
                        h_tmp(i+1,j-1)=h_tmp(i+1,j-1)+adh
                        ht(i+1,j-1)=ht(i+1,j-1)+adh 
                        
                        !print*,'avalanching lr'
                        oneway=1
!                        avalanche_counter = avalanche_counter + 1
                    endif


                    
                    if ((thll.eq.tdhmax).and.(dhll.ge.dhcritxy).and.(oneway.eq.0)) then
                    
                        adh=(dhll-dhcritxy)/2.0_8
                        if (adh.gt.h_tmp(i,j)) adh=h_tmp(i,j)
                        
                        
                        !if (adh.gt.dhdt_max) adh=dhdt_max
                        
                        h_tmp(i,j)=h_tmp(i,j)-adh
                        ht(i,j)=ht(i,j)-adh
                        
                        h_tmp(i-1,j-1)=h_tmp(i-1,j-1)+adh
                        ht(i-1,j-1)=ht(i-1,j-1)+adh 
                        
                        !print*,'avalanching ll'
                        oneway=1
!                        avalanche_counter = avalanche_counter + 1
                    endif
                endif
            enddo
        enddo
        return
            end subroutine avalanche
        end module m_avalanche

