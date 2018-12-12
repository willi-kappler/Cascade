! DS 11/17/1
! this is a simplified version of the landslide routine originally by 
!  van der Beek.  it uses a simple threshold slope criterion to determine
!  sliding.  it only fails one node above the initiation site so there
!  is only ever one node in the failure.  
!  this version should reduce the cost of the sliding routine and should 
!  produce the slope-area relationship shown in tucker and bras, 1999.
!
        module m_landslide_simple
            contains

!******************************************************************************

       subroutine ccvol(j,nb,vol,havt,hapr,x,y,nbmax,nnode,cell)

       integer(4) :: nnode,nbmax,k,j
       integer(4) :: nb(nnode)

       real(8) :: x(nnode),y(nnode),aid,vol,hapr,havt
       integer(4) :: cell(nnode,nbmax,2)
       
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
       end subroutine ccvol

!******************************************************************************

       subroutine eetalt(configData, i0, nvpt, vol, vol2)
            use rt_param
            use cascade_globals
            implicit none
!       subroutine eetalt(i0,x,y,h,vol,vol2,nnode,nb2,nn2,nbmax,cont,nvpt,dhls,cell,nb)
       
!       real x(nnode),y(nnode),h(nnode),dhls(nnode)
!       integer nn2(nbmax,nnode),nb2(nnode),nb(nnode)
!       integer cell(nnode,nbmax,2),cont
      
            type(config) :: configData
            integer(4) :: i0, j, jmax2, jsup, k, cont, nvpt
            real(8) :: distj, distj2, hapr, havt, p, pmax2, vol, vol2


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

        call ccvol(jmax2,nb,vol2,havt,hapr,x,y,nbmax,configData%nnode,cell)

        if(vol2.gt.vol)then
         h(jmax2)=(hapr-havt)*vol/vol2+havt
         memory(jmax2,8) = memory(jmax2,8) + h(jmax2)-havt
         cont=0
        else
         h(jmax2)=hapr
         memory(jmax2,8) = memory(jmax2,8) + hapr-havt
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
       end subroutine eetalt

!******************************************************************************

       subroutine find_unstable(x,y,h,nnode,nbmax,nb,nn,pts,npot,dh, &
                                pmax,ifirst,sidex,sidey,glacier)

       integer(4) :: nnode,nbmax,ifirst
       integer(4) :: nb(nnode),nn(nbmax,nnode),pts(nnode*4,2),npot
       real(8) :: x(nnode),y(nnode),h(nnode),dh,pmax,sidex,sidey

       integer(4) :: bdry
       integer(4) :: i,j,k
       real(8) :: s,diff,dij
       integer(4) :: glacier(nnode)
       npot = 0

       do i=1,nnode

        do k=1,nb(i)

         bdry = 0

         j=nn(k,i)

! ignore points on bdry after first pass
         if (ifirst.eq.0) then
          if (x(j).eq.0.0_8.or.x(j).eq.sidex.or.y(j).eq.0.0_8.or.y(j).eq.sidey) bdry=1 
         endif

         if (j.ne.i.and.bdry.eq.0) then
          dij=(x(i)-x(j))**2+(y(i)-y(j))**2
          dij=1000.0_8*sqrt(dij)
          s=atan((h(j)-h(i))/dij)
          diff = h(j) - (h(i)+dij*tan(pmax))
          if (s.ge.pmax.and.diff.gt.dh.and.glacier(i).eq.0) then
!          if (s.ge.pmax.and.diff.gt.dh) then

           npot = npot + 1
           pts(npot,1) = i
           pts(npot,2) = j
          endif
         endif
        enddo
       enddo

       return
       end subroutine find_unstable

!******************************************************************************

              subroutine landslide_simple(configData)
                    use rt_param
                    use cascade_globals
                    implicit none
!      subroutine landslide_simple(x,y,h,nn,nb,nn2,nb2,fix,surface,time,&
!                           nnode,nbmax,pmax,dhls,cell,sidex,sidey,glacier)

! fix = memory(1, 5)
! surface = memory(1, 7)
! dhls = memory(1, 8)

!       real x(nnode),y(nnode),h(nnode)
!       real fix(nnode),dhls(nnode)

!       integer nn(nbmax,nnode),nb(nnode),nn2(nbmax,nnode),nb2(nnode)
!       integer cell(nnode,nbmax,2)
       
!       real surface(nnode)
!       real pmax
!       integer glacier(nnode)
!       real pi

            type(config) :: configData
            real(8) :: s
            integer(4) :: i0



! pot  = array of potential initiation sites
! npot = no of potential initiation sites
       integer(4) :: npot
       real(8) :: dij
       real(8) :: havt,hapr,vol,vol2

! used for runout
       integer(4) :: cont,nvpt,nombre

! check the dimensions of pts if you get seg faults
       integer(4), dimension(:,:), allocatable :: pts
!       real hp(nnode)

! record number of successful depositions
       integer(4) :: nsuccess

       integer(4) :: niter
       integer(4) :: ideposit

! dh, minimum height change [m] required for slide to occur
       real(8) :: diff,dh

! iterative variables
       integer(4) :: i,j,n

! number of calls to find_unstable
       integer(4) :: ctr

!       pi = 3.1415926

        allocate(pts(configData%nnode*4, 2))

       dh = 0.1_8
       ideposit = 0
! **********************************
! done with variable declaration
! **********************************

! fill array of potential initiation sites
       npot=0

!       print *,nnode,pmax
! initialize dhls and save previous height
       do i=1,configData%nnode
        memory(i, 8) = 0.0_8
        hp(i) = h(i)
       enddo

! find unstable sites
!       print *,npot
       call find_unstable(x,y,h,configData%nnode,nbmax,nb,nn,pts,npot,dh, &
                          configData%pmax,1,configData%sidex, &
                          configData%sidey,glacier)
!       print *,npot
       ctr = 1
       
! loop thru all potential slide sites

       niter = 1
 1     nsuccess = 0
       do n=1,npot
       
! assign id of site
        i = pts(n,1)
        j = pts(n,2)

! check if slope has changed
        dij=(x(i)-x(j))**2.0_8+(y(i)-y(j))**2.0_8
        dij=1000.0_8*sqrt(dij)
        s=atan((h(j)-h(i))/dij)
        diff = h(j) - (h(i)+dij*tan(configData%pmax))

        if (s.gt.configData%pmax) then

         vol=0.0_8
        
         havt=h(j)
         h(j) = h(i) + dij*tan(configData%pmax)
         hapr=h(j)
         memory(j, 8) = memory(j, 8) + havt-hapr

! deposit slide
         if (ideposit.eq.1) then

         call ccvol(j,nb,vol,havt,hapr,x,y,nbmax,configData%nnode,cell)

         vol=abs(vol)

         if(vol.gt.0.0_8) then
       
          cont=1

! initiation site
          nvpt=i

! number of deposition sites
          nombre=0

          do while(cont.eq.1.and.nombre.lt.20)
           nombre=nombre+1
           i0=nvpt
        
           call eetalt(configData, i0, nvpt, vol, vol2)
!           call eetalt(i0,x,y,h,vol,vol2,nnode,nb2,nn2,nbmax,cont,nvpt,dhls,cell,nb)

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

! end of ideposit block
         endif
        
        endif 

       enddo           

! write out to check for slides
       if (npot.gt.0) then
!        write (16,998) time,npot,nsuccess
       endif

! check for unstable sites and go back to fail them

!       call find_unstable(x,y,h,nnode,nbmax,nb,nn,pts,npot,dh,
!     & pmax,0,sidex,sidey)

       npot = 0

! increment number of calls to find_unstable
       ctr = ctr + 1

       if (npot.gt.0) then
        niter = niter + 1
!        print *,time,': iteration ',niter,' in landslide_simple.f'

! stop if too many iterations
        if (niter.eq.100) then
         print *,'STOPPING in landslide_simple.f!',time,': '
         stop
        endif

        goto 1
       endif

! write out to file
!       if (ctr.gt.2) write (16,*) ctr,'calls to find_unstable at time',time
       if (ctr.gt.2) write (16,*),ctr,time

! reset boundary elevations to fix problems with diffusion
!       do i=1,nnode
!        if (fix(i).eq.0) then
!         h(i) = hp(i)
!        endif
!       enddo

        deallocate(pts)
       return

       end subroutine landslide_simple

        end module m_landslide_simple

