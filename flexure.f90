! flexure


        module m_flexure
            contains
              subroutine flexure (configData)
                    use rt_param
                    use cascade_globals
                    use m_sinft
                    use m_debug
                    use m_check_var
                    use m_del_sub

                    implicit none
!      subroutine flexure (x,y,h,h0,hi,hiso,nnode,flex,work_flex,nflex, &
!                          surface,points,vertices,neighbour,eps, &
!                          hflex,rhocflex,thickflex,ym,pratio,rhoaflex, &
!                          hisomin,hisomax,ixflex,iyflex,fix,isodh,dt, &
!                          shelf,iceth,hicerem)

! subroutine to calculate the flexural response due to loading/unloading
! by deposition/erosion on the landscape
! This routine uses a spectral method to solve the thin elastic plate
! equation. It therefore assumes that the plate has uniform elastic
! properties.
! Note that the "load" applied to the thin elastic plate is calculated
! from the eroded/deposited material since time t=0. The initial topography
! does not constitue a load in itself.

! INPUT: x,y        = x- and y- coordinates of the nodes
!        h          = present topography
!        h0         = initial topography,   ACTUALLY, I THINK THIS IS BEDROCK FLUVIAL INTERFACE
!        hi         = bedrock-alluvials interface,  THIS IS INITIAL TOPOGRAPHY BJY 102609
!        hiso       = amount of isistatic uplift = memory(1,4)
!        nnode      = number of nodes
!        flex       = working array
!        work_flex       = working array
!        nflex      = size of FFT (must be a power of 2)
!        surface    = surface area attached to each node
!        points     = working array
!        vertices   = triangle list
!        neighbour  = neighbour list
!        eps        = precision
!        hflex      = length of the thin elastic plate (where it is pinned)
!        rhocflex   = density of crustal rocks (in kg/m3)
!        thickflex  = elastic thickness (in km)
!        ym         = young modulus (in Pa)
!        pratio     = poisson's ratio
!        rhoaflex   = asthenospheric density (in kg/m3)
!        hisomin    = minimum isostatic deflection
!        hisomax    = maximum isostatic deflection
!        ixflex     = flag to turn x-isostasy on
!        iyflex     = flag to turn y-isostasy on
!        fix        = boundary conditions = memory(1,5)
!        dt         =current time step size

! OUTPUT: in output several arrays are updated:
!        h,hi,hiso 
!        isodh  -isostatic deflection in m/yr (deflection calculated for this timestep divided by the timestep)

! subroutines called
! - debug
! - Triloc_del
! - sinft

!     real*4    x(nnode),y(nnode),h(nnode),isodh(nnode)
!     real*4    h0(nnode),hiso(nnode),hi(nnode),dt
!     real*4    flex(nflex,nflex),work_flex(nflex,nflex)
!     real*4    surface(nnode),hicerem(nnode)
!     real*8    xfl,yfl
!     real*4    iceth(nnode)
!     integer   vertices(3,*)
!     integer   neighbour(3,*)
!     real      fix(nnode),shelf(nnode)
!     real*8    points(2,*),eps
!     logical   outside

! variable used, but not declared, WK
!      integer loc

            type(config) :: configData
            real(8) :: a, a1, a2, a3, b1, b2, b3, c1, c2, c3, d
            real(8) :: ddxf, ddyf, dflex, dh, drho, dxf, dyf, elt
            real(8) :: fi, fj, fmean, h1, h2, h3, hh, hh0, hice
            real(8) :: hisomax, hisomin, hisotot, hx, rhoice, rx, ry
            real(8) :: tij, x1, x2, x3, xfl, xflex, xfmax, xk, xloc, xmax
            real(8) :: xmean, xmin, xp, y1, y2, y3, yflex, yfmax, yfmin, yloc
            real(8) :: xfmin, yfl, ymax, ymean, yp, ymin
            integer(4) :: i, i1, i2, i3, iface, iflex
            integer(4) :: iflexfirst, iflexlast, iflexmax, iflexmin
            integer(4) :: iref, j, jflex, jflexfirst, jflexlast, jflexmax
            integer(4) :: jflexmin, jref, kface, lice, loc, pihx
            integer(4) :: ixflex_num, iyflex_num
            logical :: outside



            if (configData%ixflex) then
                ixflex_num = 1
            else
                ixflex_num = 0
            endif

            if (configData%iyflex) then
                iyflex_num = 1
            else
                iyflex_num = 0
            endif

      hx=configData%hflex*1.e3_8
      drho=configData%rhocflex*9.81_8
      elt=configData%thickflex*1.e3_8
      dflex=configData%young_mod/12.0_8/(1.0_8-configData%pratio**2)
      d=dflex*elt**3
      xk=configData%rhoaflex*9.81_8
      rhoice=910.0_8*9.81_8
! iref=0 reference height is sea-level
! iref=1 reference height is h0

      iref=1
      jref=1-iref

      xmin=x(1)
      xmax=x(1)
      ymin=y(1)
      ymax=y(1)
      do i=1,configData%nnode
        xmin=min(x(i),xmin)
        xmax=max(x(i),xmax)
        ymin=min(y(i),ymin)
        ymax=max(y(i),ymax)
        points(1,i)=x(i)
        points(2,i)=y(i)
      enddo

      xmean=(xmin+xmax)/2.0_8
      xfmin=xmean*1.e3_8-hx/2.0_8
      xfmax=xmean*1.e3_8+hx/2.0_8
      dxf=xfmax-xfmin
      ddxf=dxf/(dble(nflex-1))
      ymean=(ymin+ymax)/2.0_8
      yfmin=ymean*1.e3_8-hx/2.0_8
      yfmax=ymean*1.e3_8+hx/2.0_8
      dyf=yfmax-yfmin
      ddyf=dyf/(dble(nflex-1))


        do j=1,nflex
            do i=1,nflex
                flex(i,j)=0.0_8
            enddo
        enddo

      iflexmin=1+int((xmin*1.e3_8-xfmin)/ddxf)
      iflexmax=1+int((xmax*1.e3_8-xfmin)/ddxf)
      jflexmin=1+int((ymin*1.e3_8-yfmin)/ddyf)
      jflexmax=1+int((ymax*1.e3_8-yfmin)/ddyf)

        if (iflexmin < 1) then
            iflexmin = 1
        else if (iflexmin > nflex) then
            iflexmin = nflex
        endif

        if (iflexmax < 1) then
            iflexmax = 1
        else if (iflexmax > nflex) then
            iflexmax = nflex
        endif

        if (jflexmin < 1) then
            jflexmin = 1
        else if (jflexmin > nflex) then
            jflexmin = nflex
        endif

        if (jflexmax < 1) then
            jflexmax = 1
        else if (jflexmax > nflex) then
            jflexmax = nflex
        endif

      loc=1
      if (ivocal) call debug ('Triloc_del$')
        do j=jflexmin,jflexmax
            yloc=(dble(j-1)*ddyf+yfmin)/1.e3_8
            yfl=yloc
            do i=iflexmin,iflexmax
                xloc=(dble(i-1)*ddxf+xfmin)/1.e3_8
                xfl=xloc
                call Triloc_del (xfl,yfl,points,vertices,neighbour,loc,eps,outside,kface,iface)
                if (outside) then
                    flex(i,j)=0.0_8
                else
                    i1=vertices(1,loc)
                    i2=vertices(2,loc)
                    i3=vertices(3,loc)
                    x1=x(i1)
                    x2=x(i2)
                    x3=x(i3)
                    y1=y(i1)
                    y2=y(i2)
                    y3=y(i3)
                    a1=x2*y3-x3*y2
                    a2=x3*y1-x1*y3
                    a3=x1*y2-x2*y1
                    b1=y2-y3
                    b2=y3-y1
                    b3=y1-y2
                    c1=x3-x2
                    c2=x1-x3
                    c3=x2-x1
                    a=x1*y2+x2*y3+x3*y1-y1*x2-y2*x3-y3*x1
                    h1=(a1+b1*xloc+c1*yloc)/a
                    h2=(a2+b2*xloc+c2*yloc)/a
                    h3=(a3+b3*xloc+c3*yloc)/a
                    hh=h(i1)*h1+h(i2)*h2+h(i3)*h3
                    hice=iceth(i1)*h1+iceth(i2)*h2+iceth(i3)*h3
                    lice=int(hicerem(i1)*h1+hicerem(i2)*h2+hicerem(i3)*h3)
                    hh0=(hi(i1)*dble(iref)-memory(i1,4)*dble(iref))*h1 &
                      +(hi(i2)*dble(iref)-memory(i2,4)*dble(iref))*h2 &
                      +(hi(i3)*dble(iref)-memory(i3,4)*dble(iref))*h3
                    flex(i,j)=(-(hh-hh0)*drho*ddxf*ddyf) &
                    -((hice-dble(lice))*rhoice*ddxf*ddyf)
                    call check_var(2, "flexure, hh0 2b$", hh0, j)
                    call check_var(2, "flexure, flex(i,j) 3$", flex(i,j), i)
                endif
            enddo
        enddo

      if (ivocal) call debug ('flexure$')

      if (.not.configData%ixflex) then
        do j=1,nflex
            fmean=0.0_8
              do i=iflexmin,iflexmax
                  fmean=fmean+flex(i,j)
              enddo
              fmean=fmean/dble(iflexmax-iflexmin+1)
              do i=1,nflex
                flex(i,j)=fmean
!                call check_var(2, "flexure, flex(i,j), fmean, iflexmin, iflexmax 4$", &
!                               flex(i,j), i, fmean, iflexmin, iflexmax)
              enddo
        enddo
      endif

      if (.not.configData%iyflex) then
        do i=1,nflex
        fmean=0.0_8
          do j=jflexmin,jflexmax
              fmean=fmean+flex(i,j)
          enddo
          fmean=fmean/dble(jflexmax-jflexmin+1)
          do j=1,nflex
              flex(i,j)=fmean
             call check_var(2, "flexure, flex(i,j) 4$", flex(i,j), i)
          enddo
        enddo
      endif

      if (ivocal) call debug ('sinft$')
        do j=1,nflex
            call sinft (flex(1,j),nflex)
            call check_var(2, "flexure, flex(1,j) 5$", flex(1,j), i)
        enddo
      if (ivocal) call debug ('flexure$')

        do j=1,nflex
            do i=1,nflex
                work_flex(j,i)=flex(i,j)
            enddo
        enddo

      if (ivocal) call debug ('sinft$')
        do i=1,nflex
            call sinft (work_flex(1,i),nflex)
        enddo
          if (ivocal) call debug ('flexure$')

            do j=1,nflex
                do i=1,nflex
                    work_flex(j,i)=work_flex(j,i)*4.0_8/hx/hx
                enddo
            enddo

          pihx=int(GLOBAL_PI/hx)
          do j=1,nflex
            fj=dble((j*pihx)**2*iyflex_num)
            do i=1,nflex
                fi=dble((i*pihx)**2*ixflex_num)
                tij=d/xk*(fi**2+2.0_8*fi*fj+fj**2)+1.0_8
                work_flex(j,i)=work_flex(j,i)/xk/tij
            enddo
          enddo

      if (ivocal) call debug ('sinft$')
        do i=1,nflex
            call sinft (work_flex(1,i),nflex)
        enddo
      if (ivocal) call debug ('flexure$')

        do j=1,nflex
            do i=1,nflex
                flex(i,j)=work_flex(j,i)
                call check_var(2, "flexure, flex(i,j) 6$", flex(i,j), i)
            enddo
        enddo

      if (ivocal) call debug ('sinft$')
        do j=1,nflex
            call sinft (flex(1,j),nflex)
            call check_var(2, "flexure, flex(1,j) 7$", flex(1,j), i)
        enddo
      if (ivocal) call debug ('flexure$')

      if (.not.configData%ixflex) then
        iflexfirst=1+(nflex-1)*int((xmin*1.e3_8-xfmin)/dxf)
        do j=1,nflex
            flex(iflexfirst,j)=flex(iflexfirst+1,j)
        enddo
        iflexlast=1+(nflex-1)*int((xmax*1.e3_8-xfmin)/dxf)
        do j=1,nflex
            flex(iflexlast,j)=flex(iflexlast-1,j)
        enddo
      endif

      if (.not.configData%iyflex) then
        jflexfirst=1+(nflex-1)*int((ymin*1.e3_8-yfmin)/dyf)
        do i=1,nflex
            flex(i,jflexfirst)=flex(i,jflexfirst+1)
        enddo
        jflexlast=1+(nflex-1)*int((ymax*1.e3_8-yfmin)/dyf)
        do i=1,nflex
            flex(i,jflexlast)=flex(i,jflexlast-1)
        enddo
      endif


      hisomax=0.0_8
      hisomin=0.0_8
        do i=1,configData%nnode
            xp=x(i)*1.e3_8
            yp=y(i)*1.e3_8
            iflex=1+(nflex-1)*int((xp-xfmin)/dxf)
            jflex=1+(nflex-1)*int((yp-yfmin)/dyf)
            xflex=dble((iflex-1))*(ddxf+xfmin)
            rx=(xp-xflex)/dxf*2.0_8-1.0_8
            yflex=dble((jflex-1))*ddyf+yfmin
            ry=(yp-yflex)/dyf*2.0_8-1.0_8
            hisotot=(flex(iflex,jflex)*(1.0_8-rx)*(1.0_8-ry)/4.0_8 &
                   +flex(iflex+1,jflex)*(1.0_8+rx)*(1.0_8-ry)/4.0_8 &
                   +flex(iflex+1,jflex+1)*(1.0_8+rx)*(1.0_8+ry)/4.0_8 &
                   +flex(iflex,jflex+1)*(1.0_8-rx)*(1.0_8+ry)/4.0_8)
            call check_var(2, "flexure, histot, flex(iflex,jflex), 2$", hisotot, i, flex(iflex,jflex))
!  I took this off previous line becuase it should hall be in meters anyway.  check this/1.e3. 
            dh=hisotot-memory(i,4)
            memory(i,4)=hisotot
            hisomin=min(hisomin,dh)
            hisomax=max(hisomax,dh)
            dh=dh*memory(i,5)
            dh=dh*shelf(i)
            h(i)=h(i)+dh
            call check_var(2, "flexure, h(i), dh, 1$", h(i), i, dh)
            h0(i)=h0(i)+dh
            hi(i)=hi(i)+dh
            isodh(i)=dh/global_cascade_dt
!        print*,'isodh dh',isodh(i),dh,hisomin,hisomax
        enddo
!        print*,'isomin isomax',hisomin,hisomax
        
          return
          end subroutine flexure
        end module m_flexure

