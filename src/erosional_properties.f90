! erosional_properties

!      subroutine erosional_properties(x,y,h,h0,hi,nnode, param,nparam,nnodemax, xkf,xlf_BR,xkdiff)
        module m_erosional_properties
          contains
          subroutine erosional_properties(configData)
                use rt_param
                use cascade_globals
                implicit none

! subroutine to determine erosional properties (or lithology)
! written by Peter
! (comments by Jean)

! the main ingredients are the present topography (h)
! the present position of the bedrock-alluvion interface (h0)
! and the initial topography (hi)
! for every node

! note that erosional properties are nodal values
! and include param(*,1)=xkf (fluvial constant)
!             param(*,2)=xlf_BR (fluvial length scale)
!             param(*,3)=xkdiff (diffusional constant)

! INPUT: x,y      = x-,y- coordinates of nodes
!        h        = present topography
!        h0       = bedrock-alluvion interface
!        hi       = initial topography (at time 0)
!        nnode    = number of nodes
!        param    = erosional parameters
!        nparam   = numper ob erosional parameters
!        nnodemax = maximum number of nodes

! OUTPUT: param is updated

! subroutines called:
! NONE

                type(config) :: configData
                integer(4) :: inode

!      real     x(nnode),y(nnode),h(nnode)
!      real     h0(nnode),hi(nnode)
!      real     param(nnodemax,nparam)
!      real     xkf,xlf_BR,xkdiff

                do inode=1,configData%nnode
! changed xkf from 1e-5 
!        param(inode,1)=5.e-4

                    param(inode,1)=configData%xkf

! changed xlf from 10 
!        param(inode,2)=1000.
                    param(inode,2)=configData%xlf_BR
! changed xkdiff from 1e-6
!        param(inode,3)=1.e-5
                param(inode,3)=configData%xkdiff
                enddo

              return
            end subroutine erosional_properties
        end module m_erosional_properties

