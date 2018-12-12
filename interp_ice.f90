        module m_interp_ice
            contains
            subroutine interp_ice(configData)
                use rt_param
                use cascade_globals
                implicit none

!subroutine interp_ice(nnode,x,y,delx,dely,htoc,httoc,slidetoc,iceth,&
!             tott,slide,nxice,nyice,constoc,strict,antitoc,anthi)
! 
! the subroutine uses a linear interpolation from the Ice output grids to the original nodes.  the equations were snagged from the 'linear' subroutine in rainmaker.f
! 
! input:  nnode number of cascade nodes
!   x   the cascade array of x node locations
!   y   the cascade array of x node locations
!   delx    ice grid dimensions in x direction (km)
!   dely    ice grid dimensions in y direction (km)
!   nxice   number of Ice grid elements in x direction
!   nyice   number of Ice grid elements in y direction
!   htoc    ice thickness matrix coming from ice
!   httoc   total topography as computed by ice [km]
!   slidetoc  ice sliding velocity as computed by ice
! 
! output
!   iceth   interpreted ice thickness at nodes
!   tott    interpreted total topograph (ice + elevation) to be used to check for quality of interps [km]
!   slide   interpreted sliding velocity at each node
! 

                type(config) :: configData
                real(8) :: fwk, gwk, vv1, vv2, vv3, vv4, t5, u, xdel, ydel
                integer(4) :: i, i0, j0

!  real t5, u, fwk, gwk, vv1, vv2,vv3,vv4
!  real iceth(nnode),tott(nnode),slide(nnode),constoc(nxice,nyice)
!  real x(nnode), y(nnode), delx,ydel,dely,xdel
!  real htoc(nxice,nyice),httoc(nxice,nyice),slidetoc(nxice,nyice)
!  real strict(nnode)

          ydel=dely
          xdel=delx
          do i=1,configData%nnode
              fwk=x(i)/delx +1.0_8
              gwk=y(i)/dely +1.0_8
              i0=int(x(i)/xdel)+1
              j0=int(y(i)/ydel)+1

              t5=(fwk-dble(i0))
              u=(gwk-dble(j0))

              vv1=(1.0_8-t5)*(1.0_8-u)*htoc(i0,j0)
              vv2=t5*(1.0_8-u)*htoc(i0+1,j0)
              vv3=t5*u*htoc(i0+1,j0+1)
              vv4=(1.0_8-t5)*u*htoc(i0,j0+1)
              iceth(i)=vv1+vv2+vv3+vv4

              vv1=(1.0_8-t5)*(1.0_8-u)*httoc(i0,j0)
              vv2=t5*(1.0_8-u)*httoc(i0+1,j0)
              vv3=t5*u*httoc(i0+1,j0+1)
              vv4=(1.0_8-t5)*u*httoc(i0,j0+1)
              tott(i)=vv1+vv2+vv3+vv4

              vv1=(1.0_8-t5)*(1.0_8-u)*slidetoc(i0,j0)
              vv2=t5*(1.0_8-u)*slidetoc(i0+1,j0)
              vv3=t5*u*slidetoc(i0+1,j0+1)
              vv4=(1.0_8-t5)*u*slidetoc(i0,j0+1)
              slide(i)=vv1+vv2+vv3+vv4

              vv1=(1.0_8-t5)*(1.0_8-u)*constoc(i0,j0)
              vv2=t5*(1.0_8-u)*constoc(i0+1,j0)
              vv3=t5*u*constoc(i0+1,j0+1)
              vv4=(1.0_8-t5)*u*constoc(i0,j0+1)
              strict(i)=vv1+vv2+vv3+vv4
          end do

! added for debugging
! 2012.07.12, Willi Kappler

          print *, "minSlide: ", minval(slide)
          print *, "maxSlide: ", maxval(slide)
          print *, "avgSlide: ", sum(slide) / dble(configData%nnode)

          return
          end subroutine interp_ice
        end module m_interp_ice


