        module m_readmesh
            contains
            subroutine readmesh(configData)
            use rt_param
            use cascade_globals
            implicit none

! subroutine created by BJY 101609 to read in a cascade topography from a tecplot file.  No real(8) inputs except configData, just updates x,y,h,h0 with file.  You need to update 'meshname' however.  Should be stored in main folder 


            type(config) :: configData

          integer(4) :: i, j
          integer(4) :: node(configData%nnode),ccolor(configData%nnode),canum(configData%nnode)
          real(8)  :: dhfluv(configData%nnode),dhdiff(configData%nnode),dhland(configData%nnode)
          real(8)  :: dhtot(configData%nnode),dhglac(configData%nnode)
          real(8)  :: htot(configData%nnode),htemp(configData%nnode),htemp0(configData%nnode)
    
    

!   //inputs//

          print *, "read mesh: ", configData%meshname

          open (54,file=configData%meshname)

         do j=1,4
        read(54,*)
        end do

          do i=1,(configData%nnode-nextra)
          read(54,*) x(i),y(i),htemp(i),node(i),prec(i),dhfluv(i),dhdiff(i),dhland(i),&
                  dhtot(i),ccolor(i),canum(i),dhglac(i),iceth(i),gbalance(i),&
                  htot(i), slide(i),sediment(i),htemp0(i)
           h(i)=htemp(i)*1000.0_8
          h0(i)=htemp0(i)*1000.0_8
    !     print*,x(i),y(i),node(i),h(i),h0(i)
            memory(i,5)=1.0_8
    !       y(i)=y(i)-70
          end do
          close(54)



            return
            end subroutine readmesh
        end module m_readmesh

