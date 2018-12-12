        module m_landslide
            contains


! * * * * * * * * * * * * * * * * * * * * * 

       subroutine caire2(i,j,k,x,y,nnode,aire)
       
       integer(4) :: nnode,i,j,k
       real(8) :: x(nnode),y(nnode)
       real(8) :: aide,aire
               
       aide=(y(j)-y(i))*(x(k)-x(i))+(x(i)-x(j))*(y(k)-y(i))

! divide by 2 to get area
       aide = aide/2.0_8

       aire=aire+abs(aide)*1.e6_8
      
       return 

       end subroutine caire2

!************************************************************************

!       subroutine caire(cell,ptbaiss,kbaiss,x,y,nnode,aire,nbmax,nb)
       subroutine caire(configData, aire, ptbaiss, kbaiss)
            use rt_param
            use cascade_globals
            implicit none

            type(config) :: configData
            real(8) :: aire
            integer(4) :: ptbaiss(configData%nnode)
            integer(4) :: tri(configData%nnode,3)
            integer(4) :: ktri, j, k, knb, k0, kfin, kbaiss
       
       ktri=0

! get triangles connected to this node
       do k=1,kbaiss
        j=ptbaiss(k)

        do knb=1,nb(j)-1

         ktri = ktri + 1

!  sort vertices by id in descending order
         tri(ktri,1)=max0(j,cell(j,knb,1),cell(j,knb,2))
         if(tri(ktri,1).eq.j) then
          tri(ktri,3)=min0(cell(j,knb,1),cell(j,knb,2))
          tri(ktri,2)=max0(cell(j,knb,1),cell(j,knb,2))
         endif 
         if(tri(ktri,1).eq.cell(j,knb,1)) then
          tri(ktri,3)=min0(j,cell(j,knb,2))
          tri(ktri,2)=max0(j,cell(j,knb,2))
         endif
         if(tri(ktri,1).eq.cell(j,knb,2)) then
          tri(ktri,3)=min0(j,cell(j,knb,1))
          tri(ktri,2)=max0(j,cell(j,knb,1))
         endif

        enddo
       enddo


! take out double points
       k=1
       do while(k.le.ktri-1)
        k0=k+1
        do while(k0.le.ktri)
         if((tri(k,1).eq.tri(k0,1)).and.(tri(k,2).eq.tri(k0,2))) then
          if(tri(k,3).eq.tri(k0,3)) then 
           do kfin=k0,ktri-1
            tri(kfin,1)=tri(kfin+1,1)
            tri(kfin,2)=tri(kfin+1,2)
            tri(kfin,3)=tri(kfin+1,3)
           enddo
           ktri=ktri-1
          else
           k0=k0+1
          endif
         else
          k0=k0+1
         endif 
        enddo
        k=k+1
       enddo
       
! calculate area
       aire=0.0_8
       do k=1,ktri
        call caire2(tri(k,1),tri(k,2),tri(k,3),x,y,configData%nnode,aire)
       enddo              
                
      return
      end subroutine caire

!************************************************************************

! subroutine etalt(i0,x,y,h,vol,vol2,nnode,nb2,nn2,nbmax,cont,nvpt,dhls,cell,nb)
       subroutine etalt(vol, vol2, cont, i0, nvpt)
            use rt_param
            use cascade_globals
            implicit none

            real(8) :: hapr, havt, vol, vol2, distj, distj2, p
            real(8) :: pmax2
            integer(4) :: j, cont, i0, jmax2, jsup, k, nvpt
!       real x(nnode),y(nnode),h(nnode),dhls(nnode)
!       integer nn2(nbmax,nnode),nb2(nnode),nb(nnode)
!       integer cell(nnode,nbmax,2),cont
      
!       pi=3.141592 
        distj2 = 0.0_8
       pmax2=0.0_8
       jmax2=i0
       do k=1,nb2(i0)
        j=nn2(k,i0)
        if (j.ne.i0) then
          distj=(x(i0)-x(j))**2+(y(j)-y(i0))**2

          distj=1000.0_8*sqrt(distj)

          p=(h(i0)-h(j))/distj
        if(p.gt.pmax2)then
         pmax2=p
         jmax2=j
         distj2=distj
        endif
        endif
       enddo
       if(pmax2.gt.2.0_8*GLOBAL_PI/180.0_8) then
        havt=h(jmax2)
        hapr=h(i0)-distj2*tan(2.0_8*GLOBAL_PI/180.0_8)

!        call cvol(jmax2,nb,vol2,havt,hapr,x,y,h,nbmax,nnode,cell)
        call cvol(havt, hapr, vol2, jmax2)

        if(vol2.gt.vol)then
         h(jmax2)=(hapr-havt)*vol/vol2+havt
         memory(jmax2,8)=h(jmax2)-havt
         cont=0
        else
         h(jmax2)=hapr
         memory(jmax2,8)=hapr-havt
         nvpt=jmax2
         vol=vol-vol2
        endif 
       else
        pmax2=0.0_8
        jsup=i0
        do k=1,nb2(i0)
         j=nn2(k,i0)
         if (j.ne.i0) then
          distj=(x(i0)-x(j))**2+(y(j)-y(i0))**2

          distj=1000.0_8*sqrt(distj)

          p=(h(j)-h(i0))/distj
          if(p.gt.pmax2)then
           pmax2=p
           jsup=j
           distj2=distj
          endif
         endif
        enddo
        nvpt=jsup
        cont=1
       endif 
        
       return
       end subroutine etalt
       
!******************************************************************************

! subroutine cvol(j,nb,vol,havt,hapr,x,y,h,nbmax,nnode,cell)
       subroutine cvol(havt, hapr, vol, j)
            use rt_param
            use cascade_globals
            implicit none

            real(8) :: hapr, havt, aid, vol
            integer(4) :: k, j
!       integer nb(nnode)

!       real x(nnode),y(nnode),h(nnode)
!       integer cell(nnode,nbmax,2)
       
! this routine calculates the volume removed or added when a nodes
! elevation changes
       do k=1,nb(j)-1

        aid=(x(cell(j,k,2))-x(cell(j,k,1)))*(y(cell(j,k,1))-y(j))
        aid=aid+(y(cell(j,k,2))-y(cell(j,k,1)))*(x(j)-x(cell(j,k,1)))

! convert km2 to m2
        aid=abs(aid)*1.e6_8

! divide cross product by 2. to get area DS 8/13/1
        aid = aid/2.0_8

! quadrature formula??, is the 2 already factored into this equation??
        vol=vol+abs(hapr-havt)*(aid)/6.0_8

       enddo 

        
       return
       end subroutine cvol

!******************************************************************************
       subroutine alea0(alea,proba)

       integer(4) :: alea
       real(8) :: seed,proba

       alea=0

!      seed=rand(seed)
!       call random(seed,1)
!       if(seed.lt.proba) then   
!        alea=1
!       endif

! WK: needs to be tested!
        call random_number(seed)
        if (seed < proba) then
            alea = 1
        end if


       return
       end subroutine alea0

!******************************************************************************

! subroutine generating stochastic landslides 
! added by B.Champel-Duffait, january 2000

! DS 11/01
! This should be a working and correct version of the code developed by
! Peter van der Beek.  Many corrections have been made to the code as well
! as simple conversions from m to km.  Check the pvdb/ directory for slightly
! more explanation and archived codes.  Do not base future codes off of the
! archived codes as they are unstable and incorrect.
!

! added nb to end of parameter list DS 8/13/1
! added dtc to parameter list DS 8/15/1
!
! added lsmeth to parameter list DS 11/14/1
!  lsmeth = 1, probabilistic sliding
!         = 2, threshold slope of pmax
!         = 3, threshold slope of pmax but no deposition
!

      subroutine landslide(configData)
            use rt_param
            use cascade_globals
            implicit none
!      subroutine landslide(x,y,h,nn2,nb2,fix,surface,time, &
!                           nnode,nbmax,pmax,smax,dhls, &
!                           cohes,rho,grav,xk1,distmax, &
!                           dt,seed,xk0,dtc,tt,cell,nb,lsmeth,glacier)
                           

!       real x(nnode),y(nnode),h(nnode)
!       real fix(nnode),dhls(nnode)
! dhls = memory(1, 8)
!       integer nn2(nbmax,nnode),nb2(nnode),nb(nnode)
!       integer cell(nnode,nbmax,2)
       
!       real surface(nnode)
!       real smax(nnode)
       
!    glacier     = integer array of the location of glaciers (1=glacier, 0=no glacier)
!       integer   glacier(nnode)
! time since last landslide at this node :
!       real tt(nnode)
       
!       real pi

! smax = max slope at a node
! pot  = array of potential initiation sites
! npot = no of potential initiation sites
! cop  = array of i's lowest neighbour
! enl  = array of nodes that will be removed
! ptbaiss = array of lowered nodes
! ptaire = array of points for area calculation

            type(config) :: configData
            integer(4), dimension(:), allocatable :: pot,cop,enl
            integer(4), dimension(:), allocatable :: pot2,enl2
            integer(4) ::  npot,ienl
            integer(4) :: alea, jess
            integer(4) :: cont, nombre, nvpt
            integer(4), dimension(:), allocatable :: ptbaiss
            integer(4) :: test
            integer(4) kbaiss, i, i0, iess, j, ja, jb, jj, k, kk, kkk
            real(8) :: dij,dist,distj, s
            real(8) :: havt,hapr,vol,vol2
            real(8) :: dhls_ave, htemp
            real(8) :: proba, aire, cou

! record number of successful depositions
            integer(4) :: nsuccess

! hc = maximum stable height at a node
            real(8) :: hc

!       pi=3.14159

        cou = 0.0_8
        hc = 0.0_8
        htemp = 0.0_8

        allocate(pot(configData%nnode))
        allocate(cop(configData%nnode))
        allocate(enl(configData%nnode))
        allocate(pot2(configData%nnode))
        allocate(enl2(configData%nnode))
        allocate(ptbaiss(configData%nnode))



! fill array of potential initiation sites
       npot=1
        memory(1:,8)=0.0_8
       do i=1,configData%nnode
        smax(i)=0.0_8
        cop(i)=i
        
        do k=1,nb2(i)-1

         j=nn2(k,i)
         if (j.ne.i) then
           dij=(x(i)-x(j))**2+(y(i)-y(j))**2
           dij=1000.0_8*sqrt(dij)
           s=atan((h(j)-h(i))/dij)
         if (s.ge.smax(i)) then
            smax(i)=s
            cop(i)=j
         endif
         endif
        enddo
! bjy 101109 added .and.glacier.eq.0 to only allow landslides where there isn't ice
        if(smax(i).gt.configData%pmax.and.glacier(i).eq.0) then 
         pot(npot)=i
         npot=npot+1
        endif
       enddo 

! fill array of nodes to be removed

       ienl=1
       do k=1,npot-1
        i=pot(k)

        do kk=1,nb2(i)

         j=nn2(kk,i)

         do kkk=1,npot-1
          if(j.eq.pot(kkk).and.h(j).lt.h(i)) then
           enl(ienl)=i
           ienl=ienl+1
          endif
         enddo
        enddo
       enddo    
               
       
! order enl from highest to lowest

       do iess=1,ienl-2
        ja=enl(iess)
        jb=iess
        do jess=iess,ienl-1
         if(ja.lt.enl(jess)) then
          ja=enl(jess)
          jb=jess
         endif
        enddo
        enl(jb)=enl(iess)
        enl(iess)=ja
       enddo
              
! eliminate double points in enl

       i0=1
       enl2(1)=enl(1)
       do iess=2,ienl-1
        if(enl(iess).ne.enl2(i0)) then
         enl2(i0+1)=enl(iess)
         i0=i0+1
        endif
       enddo
       do i=1,i0-1
        enl(i)=enl2(i)
       enddo
       ienl=i0   
       
! eliminate points that are too high

       k=1
       do i=1,npot-1
        test=0
        do j=1,ienl-1
         if(pot(i).eq.enl(j)) test=1
        enddo
        if(test.eq.0) then
         pot2(k)=pot(i)
         k=k+1
        endif
       enddo    
       npot=k
       do i=1,npot-1
        pot(i)=pot2(i)
       enddo
! now pot only has correct points

       dist=0.0_8
       
! loop thru all potential slide sites
       nsuccess = 0
       do i=1,npot-1
       
! assign id of site
        j=pot(i)

        alea = 0

! probabilistic failure 
        if (configData%lsmeth.eq.1) then

         if (cos(smax(j)-configData%pmax).lt.1.0_8) then
          hc=(4.0_8*configData%cohes/(configData%rho*configData%grav))
          hc=hc*sin(smax(j))*cos(configData%pmax)
          hc=hc/(1.0_8-cos(smax(j)-configData%pmax))
          k=cop(j)
          do while(smax(cop(k)).gt.configData%pmax)
           k=cop(k)
          enddo
          htemp=h(k)-h(j)

          proba=htemp/hc+configData%xk0*tt(j)/configData%dtc
          proba=min(0.999_8,proba)

          proba=exp((configData%xk1*global_cascade_dt)*log(1.0_8-proba))
!          proba=exp((xk1*global_cascade_dt/dtc)*log(1.-proba))
          proba=1.0_8-proba
          proba=min(1.0_8,proba)

!       check if landsliding takes place
          call alea0(alea,proba)
         endif

! slope threshold failure
        elseif (configData%lsmeth.eq.2) then
         alea = 1

        endif
         
        if(alea.ne.0) then
        
         vol=0.0_8
        
! dist is the maximum length of the slide [m]
         dist = configData%distmax
        
! lower points and calculate volume

! kbaiss: # of nodes in slide
         kbaiss=0
         dhls_ave = 0.0_8

! find nodes in slide
         do jj=1,configData%nnode
          if (jj.ne.j) then

           distj=(x(jj)-x(j))**2.0_8+(y(jj)-y(j))**2.0_8
           distj=1000.0_8*sqrt(distj)

! use different failure planes for different sliding methods
           if (configData%lsmeth.eq.1) then
            cou=distj*tan((configData%pmax+smax(j))/2.0_8)
           elseif (configData%lsmeth.eq.2) then
            cou = distj*tan(configData%pmax)
           endif

           if((distj.le.dist).and.(h(jj).gt.(h(j)+cou))) then

            kbaiss=kbaiss+1
!            print *,kbaiss,j,jj,distj,distmax,h(jj),h(jj)+cou

            ptbaiss(kbaiss)=jj

            havt=h(jj)

! use different failure planes for different sliding methods
            if (configData%lsmeth.eq.1) then
             h(jj)=h(j)+distj*tan((configData%pmax+smax(j))/2.0_8)
            elseif (configData%lsmeth.eq.2) then
             h(jj) = h(j) + distj*tan(configData%pmax)
            endif

            hapr=h(jj)
            memory(jj,8)=hapr-havt
            dhls_ave = dhls_ave + memory(jj,8)

            call cvol(havt, hapr, vol, jj)
!            call cvol(jj,nb,vol,havt,hapr,x,y,h,nbmax,nnode,cell)

           endif
          endif
         enddo
         vol=abs(vol)

! get average change in elevation
         dhls_ave = dhls_ave / dble(kbaiss)
        
! calculate area
!  get surface area from map area
!  where (pmax+smax)/2 is the angle of the failure plane

         call caire(configData, aire, ptbaiss, kbaiss)
!         call caire(cell,ptbaiss,kbaiss,x,y,nnode,aire,nbmax,nb)
         if (configData%lsmeth.eq.1) then
          aire=aire/cos((configData%pmax+smax(j))/2.0_8)
         elseif (configData%lsmeth.eq.2) then
          aire = aire/cos(configData%pmax)
         endif


! continue programme
         if(vol.gt.0.0_8) then
        
! write out landslide info to file for probabilistic sliding
          if (configData%lsmeth.eq.1) then
           write (16,999) time,x(j),y(j),aire,vol,dhls_ave,htemp/hc,configData%xk0*tt(j)/configData%dtc,proba
          endif

! distribute volume below point i
          tt(j)=0.0_8
          cont=1
          nvpt=j
          nombre=0
          do while(cont.eq.1.and.nombre.lt.20)
           nombre=nombre+1
           i0=nvpt
        
           call etalt(vol, vol2, cont, i0, nvpt)
!           call etalt(i0,x,y,h,vol,vol2,nnode,nb2,nn2,nbmax,cont,nvpt,dhls,cell,nb)

          enddo
          if(cont.eq.1.and.nombre.ge.20) then
!           write(16,*) 'impossible to distribute landslide volume'
!           write(16,*) ': last point ',nvpt
          else
!          print *,'successful in distributing landslide volume'
           nsuccess = nsuccess + 1
          endif
        
! end of vol.gt.0 block
         endif
        
       
! end of alea.ne.0 block
        endif 

! end of i=1,npot-1 block
       enddo           

! write out to check for slides
       if (nsuccess.ne.(npot-1)) then
        write (16,998) time,npot-1,nsuccess
       endif
       
        deallocate(pot)
        deallocate(cop)
        deallocate(enl)
        deallocate(pot2)
        deallocate(enl2)
        deallocate(ptbaiss)

       return             

998    format (E13.5,I10,I10)
999    format (E13.5,E13.5,E13.5,E13.5,E13.5,E14.5,E14.5,E14.5,E14.5)
       end subroutine landslide
       
        end module m_landslide

