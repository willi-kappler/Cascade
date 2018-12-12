    module m_basal_temperature
        contains

      real(8) FUNCTION gammln(xx)
      REAL(8) xx
      INTEGER j
      DOUBLE PRECISION ser,stp,tmp,x,y,cof(6)
      SAVE cof,stp
      DATA cof,stp/76.18009172947146d0,-86.50532032941677d0, &
      24.01409824083091d0,-1.231739572450155d0,.1208650973866179d-2, &
      -.5395239384953d-5,2.5066282746310005d0/
      x=xx
      y=x
      tmp=x+5.5d0
      tmp=(x+0.5d0)*log(tmp)-tmp
      ser=1.000000000190015d0
      do 11 j=1,6
        y=y+1.d0
        ser=ser+cof(j)/y
11    continue
      gammln=tmp+log(stp*ser/x)
      return
      end function gammln

    SUBROUTINE gcf(gammcf,a,x,gln)
      INTEGER ITMAX
      REAL(8) a,gammcf,gln,x,EPS,FPMIN
      PARAMETER (ITMAX=100,EPS=3.e-7_8,FPMIN=1.e-30_8)
!CU    USES gammln
      INTEGER i
      REAL(8) an,b,c,d,del,h_gcf
      gln=gammln(a)
      b=x+1.0_8-a
      c=1.0_8/FPMIN
      d=1.0_8/b
      h_gcf=d
      do 11 i=1,ITMAX
        an=dble(-i)*(dble(i)-a)
        b=b+2.0_8
        d=an*d+b
        if(abs(d).lt.FPMIN)d=FPMIN
        c=b+an/c
        if(abs(c).lt.FPMIN)c=FPMIN
        d=1.0_8/d
        del=d*c
        h_gcf=h_gcf*del
        if(abs(del-1.0_8).lt.EPS)goto 1
11    continue
      print *, 'a too large, ITMAX too small in gcf'
1     gammcf=exp(-x+a*log(x)-gln)*h_gcf
      return
      END subroutine gcf

          SUBROUTINE gser(gamser,a,x,gln)
      INTEGER ITMAX
      REAL(8) a,gamser,gln,x,EPS
      PARAMETER (ITMAX=100,EPS=3.e-7_8)
!CU    USES gammln
      INTEGER n
      REAL(8) ap,del,sum
      gln=gammln(a)
      if(x.le.0.0_8)then
        if(x.lt.0.0_8) then
            print *, 'x < 0 in gser'
        endif
        gamser=0.0_8
        return
      endif
      ap=a
      sum=1.0_8/a
      del=sum
      do 11 n=1,ITMAX
        ap=ap+1.0_8
        del=del*x/ap
        sum=sum+del
        if(abs(del).lt.abs(sum)*EPS)goto 1
11    continue
      print *, 'a too large, ITMAX too small in gser'
1     gamser=sum*exp(-x+a*log(x)-gln)
      return
      END subroutine gser

    real(8) FUNCTION gammp(a,x)
      REAL(8) a,x
!CU    USES gcf,gser
      REAL(8) gammcf,gamser,gln
      if(x.lt.0.0_8.or.a.le.0.0_8) then
        print *,'bad arguments in gammp'
      endif
      if(x.lt.a+1.0_8)then
        call gser(gamser,a,x,gln)
        gammp=gamser
      else
        call gcf(gammcf,a,x,gln)
        gammp=1.0_8-gammcf
      endif
      return
      END function gammp


      real(8) FUNCTION own_erf(x)
      REAL(8) x
! USES gammp
      if(x.lt.0.0_8)then
      own_erf=-gammp(0.5_8,x**2)
      else
      own_erf=gammp(0.5_8,x**2)
      endif
      return
      END function own_erf


!*****************************************************
      real(8) function ablate(mh,loss,k)
      real(8) :: fnc,z0,z1,fz1,fz0,del,mh,k,loss
      fnc = 0.0_8
      del = 0.05_8
!     del=mh/20.
      z1= 0.0_8
      fz1 = 0.0_8
      z0 = 0.0_8
      fz0 = 0.0_8
10    z1 = z0 + del
      fz1 = exp(-mh*loss*(z1**dble(2))/k/2.0_8)
 !     fz1 = exp(-z1**2*loss/(2.*k*mh))
      fnc = fnc + 0.5_8*del*(fz0 + fz1)
      if (z1 .lt. 1.0_8) then
!      if (z1 .lt. mh) then
      z0 = z1
      fz0 = fz1
      goto 10
      endif
     
      ablate = fnc
       return
       end function ablate
!*******************************************************

        subroutine basal_temperature (nx,ny,tempb,temps,tempm,dtemp, &
                                        Pe,junk,ice_mass_balance, &
                                        diffusivity,secinyr,dtdzb,h_tmp)

          implicit none

          integer(4) :: nx, ny, i, j
          real(8) :: diffusivity, secinyr, dtdzb

          real(8) :: tempb(nx,ny),temps(nx,ny),tempm(nx,ny),dtemp(nx,ny)
          real(8) :: Pe(nx,ny),junk(nx,ny),ice_mass_balance(nx,ny),h_tmp(nx,ny)

          real(8), dimension(:,:), allocatable :: geoTscale

            allocate(geoTscale(nx, ny))

! calculating basal temperature
geoTscale=1.0_8
      tempb=temps
      where (h_tmp > 0.0_8) tempb=temps+dtdzb*h_tmp
      Pe=0.0_8
      where (h_tmp > 0.0_8) Pe=ice_mass_balance*h_tmp/diffusivity/secinyr
      junk=0.0_8
        do j=1,ny
          do i=1,nx
           if (ice_mass_balance(i,j) > 0.0_8 .and. h_tmp(i,j) > 0.0_8) then
             junk(i,j)=2.0_8*h_tmp(i,j)*diffusivity/ice_mass_balance(i,j)*secinyr
             if (junk(i,j) /= 0.0_8) junk(i,j)=sqrt(junk(i,j))
             if (h_tmp(i,j) > 0.0_8 .and. junk(i,j) > 0.0_8) &
               tempb(i,j)=temps(i,j)+dtdzb*geoTscale(i,j)*junk(i,j)*(sqrt(3.1415_8)/2.0_8)*own_erf(h_tmp(i,j)/junk(i,j)) ! FIX_PI
          elseif (ice_mass_balance(i,j).le.0.0_8 .and.h_tmp(i,j).gt.0.0_8) then
!             tempb(i,j)=temps(i,j)+dtdzb*h_tmp(i,j)* &
!                       ablate(h_tmp(i,j),a(i,j)*3.17e-8,diffusivity)
                if (tempb(i,j).gt.-8.7e-4_8*h_tmp(i,j)) then
                
!   latent heat of fusion for watr  333.55 J/g              
!                  h_tmp(i,j)=  (tempb(i,j)+8.7e-4*h_tmp(i,j))
                   h_tmp(i,j)=h_tmp(i,j)-0.1_8;
                endif
           endif
          enddo
        enddo
      dtemp=tempb-tempm
!      tempb=0.
        deallocate(geoTscale)
      return
      end subroutine basal_temperature

      end module m_basal_temperature


