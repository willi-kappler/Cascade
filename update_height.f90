        module m_update_height
            contains
            subroutine update_height (nx,ny,dh,ht,mass_balance,h_tmp, &
                dx,dy,dtice,dhdt_allowed,tempb,tempm, &
                n,xns,c,cs,sliding,gamma,constriction,t, &
                tsmooth)

      integer(4) :: nx,ny,i,j,dir1,dir2,dir3,dir4,k
      real(8) :: dh(nx,ny),ht(nx,ny),mass_balance(nx,ny),h_tmp(nx,ny)
      real(8) :: tempb(nx,ny),tempm(nx,ny),sliding(nx,ny)
      real(8) :: d2tdx2(nx,ny),d2tdy2(nx,ny),conx(nx,ny),cony(nx,ny)
      real(8) :: constriction(nx,ny),t(nx,ny),tsmooth(nx,ny)
      real(8) :: d2tdd(nx,ny)
      real(8) :: dht1,dht2,dht3,dht4,dt1,dt2,dt3,dt4,cs
      real(8) :: normx,normy,dx,dy,h1,D1,c,n,xns,h2,D2,h3,D3,h4,D4,gamma
      real(8) :: D1s,D2s,D3s,D4s,D1sp,D2sp,D3sp,D4sp,slidingterms,dterms
      real(8) :: dterm1,dterm2,dterm3,dterm4,dtice,dhdt_allowed,dhmax,hh3
    
! updating height old way
!      dh=(((cshift(d,1,1)+d)/2.*((cshift(ht,1,1)-ht))/dx &
!          -(cshift(d,-1,1)+d)/2.*((ht-cshift(ht,-1,1)))/dx)/dx &
!         +((cshift(d,1,2)+d)/2.*((cshift(ht,1,2))-ht)/dy &
!          -(cshift(d,-1,2)+d)/2.*((ht-cshift(ht,-1,2)))/dy)/dy &
!         +a)*dtice
         
!       dh=(((d)*((cshift(ht,1,1)-ht))/dx &
!          -(d)*((ht-cshift(ht,-1,1)))/dx)/dx &
!         +((d)*((cshift(ht,1,2))-ht)/dy &
!          -(d)*((ht-cshift(ht,-1,2)))/dy)/dy &
!         +a)*dtice
!      dh=min(dhdt_allowed*dtice,dh)
!      where (a.ge.0) dh=max(-dhdt_allowed*dtice,dh)
!      h_tmp=h_tmp+dh
!      h_tmp=max(h_tmp,0.)
!       normx=h_tmp*cshift(h_tmp,1,1)*cshift(h_tmp,-1,1)
!      normy=h_tmp*cshift(h_tmp,1,2)*cshift(h_tmp,-1,2)
      !  My attempt
!       d2tdx2=(cshift(t,1,1)-cshift(t,-1,1))/dx
!       d2tdx2=(cshift(d2tdx2,1,1)-cshift(d2tdx2,-1,1))/dx
!       where (d2tdx2.lt.0) d2tdx2=0
!        d2tdd=(cshift(cshift(t,1,1),1,2)-cshift(cshift(t,-1,1),1,2) &
!              -cshift(cshift(t,1,1),-1,2)+cshift(cshift(t,-1,1),-1,2)) &
!             /4./dx/dy
!        d2tdy2=(cshift(t,1,2)-cshift(t,-1,2))/dy
!        d2tdy2=(cshift(d2tdy2,1,2)-cshift(d2tdy2,-1,2))/dy
        
        
!  Fred's old way...
      d2tdx2=(cshift(tsmooth,1,1)-2.0_8*tsmooth+cshift(tsmooth,-1,1))/dx/dx
      d2tdd=(cshift(cshift(tsmooth,1,1),1,2)-cshift(cshift(tsmooth,-1,1),1,2) &
              -cshift(cshift(tsmooth,1,1),-1,2)+cshift(cshift(tsmooth,-1,1),-1,2)) &
             /(dx**2 * dy**2)
      d2tdy2=(cshift(tsmooth,1,2)-2.0_8*tsmooth+cshift(tsmooth,-1,2))/dy/dy       
        
!           where (d2tdy2.lt.0) d2tdy2=0
!      conx=1./(1.+(d2tdx2*gamma))
       conx=1.0_8
       cony=1.0_8
!      cony=1./(1.+(d2tdy2*gamma))
   
      
!      constriction=sqrt(conx**2 +cony**2)

!  new way to calc height, test speed with old way
    

!    print *, "update_height: c (e-7): ", c
!    print *, "update_height: cs (e-8): ", cs
!    print *, "update_height: gamma (e3): ", gamma
!    print *, "update_height: n: ", n
!    print *, "update_height: xns: ", xns

       do j=3,ny-1
    do i=2,nx-1
 
    
        D1s=0.0_8
        D2s=0.0_8
        D3s=0.0_8
        D4s=0.0_8
         dir1=1
         dir2=1
         dir3=1
         dir4=1
         h1=h_tmp(i+1,j)
!        if (normx(i,j).gt.0)
         if (ht(i,j).gt.ht(i+1,j)) then
            h1=h_tmp(i,j)
!            dir1=-1
            endif
            
         D1=dble(dir1)*c*((h_tmp(i+1,j)+h_tmp(i,j))/2.0_8)**(n+1.0_8)*(abs(ht(i+1,j)-ht(i,j))/dx)**(n-1.0_8)
         if (tempb(i,j) > tempm(i,j)) &
         D1s=cony(i,j)*cs*((h_tmp(i+1,j)+h_tmp(i,j))/2.0_8)**(xns)*(abs(ht(i+1,j)-ht(i,j))/dx)**(xns-1.0_8)
         dht1=ht(i+1,j)-ht(i,j)
         dt1=t(i+1,j)-t(i,j)
         
         if (D1s.gt.2000.0_8) D1s=2000.0_8
            D1s=D1s*dble(dir1)
                  h2=h_tmp(i,j)
         if (ht(i,j).gt.ht(i-1,j)) then
            h2=h_tmp(i-1,j)
!           dir2=-1
            end if
         D2=dble(dir2)*c*((h_tmp(i,j)+h_tmp(i-1,j))/2.0_8)**(n+1.0_8)*(abs(ht(i,j)-ht(i-1,j))/dx)**(n-1.0_8)
         if (tempb(i,j) > tempm(i,j)) &
         D2s=cony(i,j)*cs*((h_tmp(i,j)+h_tmp(i-1,j))/2.0_8)**(xns)*(abs(ht(i,j)-ht(i-1,j))/dx)**(xns-1.0_8)
         dht2=ht(i,j)-ht(i-1,j)
         dt2=t(i,j)-t(i-1,j)
         
         if (D2s.gt.2000.0_8) D2s=2000.0_8
            D2s=D2s*dble(dir2)
!        endif
        
        
        
          
           h3=h_tmp(i,j+1)
         if (ht(i,j).gt.ht(i,j+1)) then
           h3=h_tmp(i,j)
!           dir3=-1
           end if
           
         D3=dble(dir3)*c*((h_tmp(i,j+1)+h_tmp(i,j))/2.0_8)**(n+1.0_8)*(abs(ht(i,j+1)-ht(i,j))/dy)**(n-1.0_8)
         if (tempb(i,j) > tempm(i,j)) &
         D3s=conx(i,j)*cs*((h_tmp(i,j+1)+h_tmp(i,j))/2.0_8)**(xns)*(abs(ht(i,j+1)-ht(i,j))/dy)**(xns-1.0_8)
         dht3=ht(i,j+1)-ht(i,j)
         dt3=t(i,j+1)-t(i,j)
         if (D3s.gt.2000.0_8) D3s=2000.0_8
            D3s=dble(dir3)*D3s
            
          h4=h_tmp(i,j)
         if (ht(i,j).gt.ht(i,j-1)) then
            h4=h_tmp(i,j-1)
!            dir4=-1
            end if
            
         D4=dble(dir4)*c*((h_tmp(i,j)+h_tmp(i,j-1))/2.0_8)**(n+1.0_8)*(abs(ht(i,j)-ht(i,j-1))/dy)**(n-1.0_8)
         if (tempb(i,j) > tempm(i,j)) &
         D4s=conx(i,j)*cs*((h_tmp(i,j)+h_tmp(i,j-1))/2.0_8)**(xns)*(abs(ht(i,j)-ht(i,j-1))/dy)**(xns-1.0_8)
         if (D4s.gt.2000.0_8) D4s=2000.0_8
            D4s=dble(dir4)*D4s
         dht4=ht(i,j)-ht(i,j-1)
         dt4=t(i,j)-t(i,j-1)
        
        

        
        
!        ads=(D1s+D2s)**2 + (D3s+D4s)**2
        D1sp=D1s*(ht(i+1,j)-ht(i,j))/dx
        D2sp=D2s*(ht(i,j)-ht(i-1,j))/dx
        D3sp=D3s*(ht(i,j+1)-ht(i,j))/dy
        D4sp=D4s*(ht(i,j)-ht(i,j-1))/dy
        sliding(i,j)=sqrt((0.5_8*(D1sp+D2sp))**2 + (0.5_8*(D3sp+D4sp))**2)
        normx=abs(0.5_8*(D1sp+D2sp)/sliding(i,j))
        normy=abs(0.5_8*(D3sp+D4sp)/sliding(i,j))
        
!       constriction(i,j)=(conx(i,j)**2 + cony(i,j)**2)/2
        constriction(i,j)=normx**2*d2tdx2(i,j)+normy**2*d2tdy2(i,j)+ & 
         2.0_8*normx*normy*d2tdd(i,j)
!        if (d2tdx2(i,j).gt.0) then
!         print*,constriction(i,j),normx,normy,sliding(i,j),D1s,D2s,D3s,D4s,d2tdx2(i,j),d2tdy2(i,j),d2tdd(i,j)
!        endif
!       if (constriction(i,j).lt.0) constriction(i,j)=0
        constriction(i,j)=max(constriction(i,j),0.0_8)
        constriction(i,j)=1.0_8/(1.0_8+constriction(i,j)*gamma)
        sliding(i,j)=sliding(i,j)*constriction(i,j)
!        print*,constriction(i,j)
!       D1s=D1s/(1+(gamma*normx**2*d2tdx2(i,j)))
!       D2s=D2s/(1+(gamma*normx**2*d2tdx2(i,j)))
!       D3s=D3s/(1+(gamma*normy**2*d2tdy2(i,j)))
!       D4s=D4s/(1+(gamma*normy**2*d2tdy2(i,j)))
        
!       D1s=D1sp*constriction(i,j)
!       D2s=D2sp*constriction(i,j)
!       D3s=D3sp*constriction(i,j)
!       D4s=D4sp*constriction(i,j)
        
!       D1t=D1s+ (D1*(ht(i+1,j)-ht(i,j))/dx)
!       D2t=D2s+ (D2*(ht(i,j)-ht(i-1,j))/dx)
!       D3t=D3s+ (D3*(ht(i,j+1)-ht(i,j))/dy)
!       D4t=D4s+ (D4*(ht(i,j)-ht(i,j-1))/dy)
!         dh(i,j)=(((((D1t*h1)) &
!                  -(((D2t*h2))))/dx) &
!                  +((((D3t*h3))) &
!                  -(((D4t*h4)))/dy) &
!                  +a(i,j))*dtice
       
            slidingterms=constriction(i,j)*(((h1*D1sp-h2*D2sp)/dx)+((h3*D3sp-h4*D4sp)/dy))
            dterm1=(h1*D1*(ht(i+1,j)-ht(i,j))/dx)
            dterm2=(h2*D2*(ht(i,j)-ht(i-1,j))/dx)
            dterm3=(h3*D3*(ht(i,j+1)-ht(i,j))/dy)
            dterm4=(h4*D4*(ht(i,j)-ht(i,j-1))/dy)
            dterms=((dterm1-dterm2)/dx)+((dterm3-dterm4)/dy)
!               dterms=(((h1*D1*(ht(i+1,j)-ht(i,j))/dx)-(h2*(D2*(ht(i,j)- &
!               ht(i-1,j))/dx)))/dx)+ &
!                (((h3*D3*(ht(i,j+1)-ht(i,j))/dy)-(h4*D4*(ht(i,j)-ht(i,j-1))/dy)))/dy)
       
         dh(i,j)=(slidingterms+dterms+mass_balance(i,j))*dtice

!       print*,'min',dhdt_allowed*dtice,dh(i,j)
          dh(i,j)=min(dhdt_allowed*dtice,dh(i,j))
!          print*,'max',-dhdt_allowed*dtice,dh(i,j)
          dh(i,j)=max(-dhdt_allowed*dtice,dh(i,j))
!          print*,'dh1',dh(i,j)
          if (abs(dh(i,j)).gt.dhmax) dhmax=abs(dh(i,j))
!          if (dh(i,j).gt.0.) then
!               print*,'dh2',dh(i,j)
!          
!          endif



           hh3 = h_tmp(i,j) + dh(i,j)
           h_tmp(i,j)=max(hh3,0.0_8)
                      
!          if (dh(i,j).gt.0.) then
!               print*,'h_tmp dh2',h_tmp(i,j),dh(i,j)
!          
!          endif
 !          print*,'max',h_tmp(i,j),dh(i,j)
 !    hp(i,j)=h_tmp(i,j)+dh(i,j)
!       if (h_tmp(i,j).gt.1000) then
!        print*,ht(i,j),ht(i-1,j),ht(i+1,j),ht(i,j-1),ht(i,j+1),a(i,j)          
!       print*,D1s,D2s,D3s,D4s,D1t,D2t,D3t,D4t,dh(i,j)
!     
!        print*,dh(i,j),h_tmp(i,j)
!       endif
!  calcualuting if   
!         dhts(1)=dht1
!         dhts(2)=dht2
!         dhts(3)=dht3
!         dhts(4)=dht4
!         adhts=abs(dhts)
!         mdht=maxval(dhts)
!          dts(1)=dt1
!         dts(2)=dt2
!         dts(3)=dt3
!         dts(4)=dt4
!         
!         adts=abs(dts)
!         
!        if (dht1.eq.mdht) then
!         mslope=dht1*dt1
!         difas=(adhts(1)-adts(1))/adhts(1)
!         if ((mslope.lt.0).and.(difas.le.0.5)) then
!           anthislope(i,j)=0
!         endif
!        elseif (dht2.eq.mdht) then
!          mslope=dht2*dt2
!          difas=(adhts(2)-adts(2))/adhts(2)
!          if ((mslope.lt.0).and.(difas.le.0.5)) then
!           anthislope(i,j)=0
!          endif
!        elseif (dht3.eq.mdht) then
!          mslope=dht3*dt3
!          difas=(adhts(3)-adts(3))/adhts(3)
!          if ((mslope.lt.0).and.(difas.le.0.5)) then
!           anthislope(i,j)=0
!          endif
!        elseif (dht3.eq.mdht) then
!          mslope=dht4*dt4
!          difas=(adhts(4)-adts(4))/adhts(4)
!          if ((mslope.lt.0).and.(difas.le.0.5)) then
!           anthislope(i,j)=0
!          endif
!        endif
      
      
      
! new way to find if antithetical slope is withing 50%
! Maybe add this idea: Also,maybe stop incision at middle and one upstream and one downstream?

!        anthislope(i,j)=0
!       dtopox= t(i-1,j)-t(i+1,j)     
!       dhtx= ht(i-1,j)-t(i+1,j)
!       dtopoy=t(i,j-1)-t(i,j+1)
!       dhty=ht(i,j-1)-ht(i,j+1)
!       
!       xdiv=dhtx/dtopox
!       ydiv=dhty/dtopoy
!       
!       if(xdiv.le.-.5) then
!         anthislope(i,j)=1
!         anthislope(i+1,j)=1
!         anthislope(i-1,j)=1
!       endif
!       
!       if(ydiv.le.-.5) then
!          anthislope(i,j)=1
!          anthislope(i,j+1)=1
!          anthislope(i,j-1)=1
!       endif
      
!  find if topographic slope is within 50%, antithetical to ice surface slope     
       enddo
     enddo

        do k=1,nx
            h_tmp(k,1)=0.0_8
            h_tmp(k,2)=0.0_8
            h_tmp(k,ny)=0.0_8
        enddo
        do k=1,ny
            h_tmp(k,1)=0.0_8
            h_tmp(k,nx)=0.0_8
        enddo

!print*,minval(dh)/dtice,maxval(dh)/dtice,dhdt_allowed/dtice,time

!      print*,maxval(a)

!        print *, "dhmax: ", dhmax

      return
          end subroutine update_height
        end module m_update_height

