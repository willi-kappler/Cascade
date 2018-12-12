! find_neighbours
! from peter van der beek (summmer,2001)

        module m_find_neighbours
            contains
              subroutine find_neighbours (configData)
                    use rt_param
                    use cascade_globals
                    use m_debug
                    use m_find_surface
                    use m_delaun
                    use m_nn2
                    use m_del_sub

                    implicit none

!      subroutine find_neighbours (x,y,nn,nb,configData%nnode,nbmax,nn2,nb2,&
!                                  points,vertices,neighbour,nodes,&
!                                  vis_tlist,vis_elist,add_tlist,nt,&
!                                  surface,newsurface,eps,&
!                                  xx,pp,aa,bb,surfscale,cell)

! subroutine to find the list of natural neighbours attached to each
! nodes. It computes the nn and nb lists but also the surfaces
! attached to the nodes.

! INPUT: x,y       = x,y nodal coordinates
!        configData%nnode     = number of nodes
!        nbmax     = maximum number of neighbour per node
!        points    = working array
!        nodes     = working array
!        vis_tlist = working array
!        vis_elist = working array
!        add_tlist = working array
!        newsurface= here it is just used as a working array = memory(1,6)
!        eps       = precision
!        xx        = working array = xy
!        pp        = working array
!        aa        = working array
!        bb        = working array
!        surfscale = average nodal surface = configData%surfscale

! OUTPUT: nn        = neighbour array
!         nb        = number of neighbour for each node
!         vertices  = triangle list
!         neighbour = neighbour list
!         nt        = number of triangle
!         surface   = nodal surface (voronoi cell surface area) = memory(1,7)
!         nn2       = added by Dffait, unknown purpose (TAE 5/01)
!         nb2       = added by Duffait, unknown purpose (TAE 5/01)
!         cell      = added by Duffait, unknown purpose (TAE 5/01)

! subroutines called:
! NONE

            type(config) :: configData
            integer(4) :: itstart, bfirst, i, i0, i1, i2, i3, ie
            integer(4) :: iess, it, itemp, j, ja, jb, jess, jess2
            integer(4) :: mode, np, nv_max, nt_max, k
            integer, dimension (:,:), allocatable :: nn3
            integer, dimension(:), allocatable :: subset


!      real          x(*),y(*),surface(*)
!      integer       nn(nbmax,*),nn2(nbmax,*)
!      integer       nb(*),nb2(*)
!      integer       cell(configData%nnode,nbmax,2)

!      real*8       points(2,*),eps
!      integer      vertices(3,*)
!      integer      neighbour(3,*)
!      integer       nodes(*)
!      integer       vis_tlist(*)
!      integer       vis_elist(*)
!      integer       add_tlist(*)
!      integer      ccw,np

!      real          xx(2),pp(2,nbmax),aa(nbmax,2),bb(nbmax)
!      real          newsurface(*)

!      integer       nn3(nbmax,configData%nnode)


      print *, "start find neighbours"
      allocate(nn3(nbmax,configData%nnode))
      allocate(subset(configData%nnode))

      do i=1,configData%nnode
       points(1,i)=dble(x(i))
       points(2,i)=dble(y(i))
       nodes(i)=i
      enddo

      np=configData%nnode

      nv_max=configData%nnode

! sorting by x

      if (ivocal) call debug ('indexx$')
      call indexx(np,points,nodes)
      if (ivocal) call debug ('find_neighbours$')

! ensure initial triangle is in ccw order
 
      if(ccw(points(1,nodes(1)),points(1,nodes(2)),points(1,nodes(3)),k).eq.-1)then
             itemp = nodes(1)
             nodes(1) = nodes(2)
             nodes(2) = itemp
      end if

!                       Call the routine that 
!                       does the work

      mode=0
      eps=0.d0
      if (ivocal) call debug ('delaun$')
      call delaun (points,np,neighbour,vertices,nt,2*np,&
                   vis_tlist,vis_elist,add_tlist,eps,nv_max,&
                   mode,inactive,bfirst,itstart,subset)
      if (ivocal) call debug ('find_neighbours$')

      nt_max=configData%nnode*3
        if(nt.gt.nt_max)then
        write(6,*)' Error number of triangles calculated is larger'
        write(6,*)' than array sizes.'
        write(6,*)' Number of triangles    = ',nt
        write(6,*)' Maximum value (nt_max) = ',nt_max
        stop
        endif

        do i=1,configData%nnode
            nb(i)=0
            nb2(i)=0
        enddo

        print *, "nbmax: ", nbmax


        do it=1,nt
        
! vertices are ordered ccw
         i1=vertices(1,it)
         i2=vertices(2,it)
         i3=vertices(3,it)
         
          nb(i1)=nb(i1)+1
          nn(nb(i1),i1)=i2

          nb2(i1)=nb2(i1)+1
          nn2(nb2(i1),i1)=i2

          if (nb2(i1).gt.nbmax) then
                print *, "nbmax: ", nbmax, "nb2(i1): ", nb2(i1)
                stop 'nbmax too small...1'
          end if

          cell(i1,int((1.+real(nb2(i1)))/2.),1)=i2
          nb2(i1)=nb2(i1)+1
          nn2(nb2(i1),i1)=i3
          cell(i1,int((1.+real(nb2(i1)))/2.),2)=i3

          nb(i2)=nb(i2)+1
          nn(nb(i2),i2)=i3

          nb2(i2)=nb2(i2)+1
            if (nb2(i2).gt.nbmax) then
                print *, "nbmax: ", nbmax, "nb2(i2): ", nb2(i2)
                print *, "i2: ", i2
                stop 'nbmax too small...2'
            endif

! testing
!          nn2(nb2(i2),i2)=i3
!          cell(i2,int((1.+real(nb2(i2)))/2.),1)=i3
!          nb2(i2)=nb2(i2)+1
!          nn2(nb2(i2),i2)=i1
!          cell(i2,int((1.+real(nb2(i2)))/2.),2)=i1

! original
          nn2(nb2(i2),i2)=i3
          cell(i2,int((1.+real(nb2(i2)))/2.),1)=i1
          nb2(i2)=nb2(i2)+1
          nn2(nb2(i2),i2)=i1
          cell(i2,int((1.+real(nb2(i2)))/2.),2)=i3

          nb(i3)=nb(i3)+1
          nn(nb(i3),i3)=i1

          nb2(i3)=nb2(i3)+1
            if (nb2(i3).gt.nbmax) then
                print *, "nbmax: ", nbmax, "nb2(i3): ", nb2(i3)
                stop 'nbmax too small...3'
            endif
          nn2(nb2(i3),i3)=i1
          cell(i3,int((1.+real(nb2(i3)))/2.),1)=i1
          nb2(i3)=nb2(i3)+1
          nn2(nb2(i3),i3)=i2
          cell(i3,int((1.+real(nb2(i3)))/2.),2)=i2

        enddo
        
! sort neighbor id's
        do i=1,configData%nnode

! classement de nn2 de i le plus gd au plus pt
         do ie=1,nb2(i)-1
          ja=nn2(ie,i)
          jb=ie
          do jess=ie+1,nb2(i)
           jess2=nn2(jess,i)
           if(ja.lt.jess2) then
            ja=jess2
            jb=jess
           endif
          enddo
          nn2(jb,i)=nn2(ie,i)
          nn2(ie,i)=ja
         enddo

! elimination des pts doubles de enl
         i0=1
         nn3(1,i)=nn2(1,i)
         do iess=2,nb2(i)-1
          if(nn2(iess,i).ne.nn3(i0,i)) then
           nn3(i0+1,i)=nn2(iess,i)
           i0=i0+1
          endif
         enddo
         do j=1,i0
          nn2(j,i)=nn3(j,i)
         enddo
         nb2(i)=i0 
        enddo

        do i=1,configData%nnode
            memory(i,6)=1.0_8
        enddo

      if (ivocal) call debug ('find_surface$')
      call find_surface (configData)
!      call find_surface (nn,nb,surface,nbmax,configData%nnode,&
!                        x,y,xx,pp,aa,bb,newsurface,surfscale)
      if (ivocal) call debug ('find_neighbours$')

! add id of node to end of neighbors list
        do i=1,configData%nnode
        nb(i)=nb(i)+1
          if (nb(i).gt.nbmax) then
            print *, "nbmax: ", nbmax, ", nb: ", nb(i)
            stop 'nbmax too small...4'
          endif
        nn(nb(i),i)=i
        enddo

! deallocate memory:

      deallocate(nn3)
      deallocate(subset)

              return
              end subroutine find_neighbours
        end module m_find_neighbours

