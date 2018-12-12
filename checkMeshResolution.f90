! Cleaned up and updated version of check_mesh.f90 PRE Dec2017
! It now actually removes nodes. In the previous version only added nodes could be removed. The base set nnode0 never changed.
! Node removal is now dependent on the absolutue distance of neighbours in the xy-plane, and distance to boundary


module m_checkMeshResolution
    contains
        subroutine checkMeshResolution(configData)
			use rt_param
			use cascade_globals
			use m_debug
			use m_check_for_removal
			use m_find_surface
			use m_delaun
			use m_nn_remove

			implicit none

            type(config) :: configData

! variables used by boundary node addition
            integer(4) :: bcnt

! variables used for height interpolation
            real(8), dimension(3) :: s

! variable used but not declared, WK
!            integer np, itstart

            integer(4), dimension(:,:), allocatable :: nn3
            integer(4), dimension(:), allocatable :: subset
            real(8) :: dhcritmin, fluxmax_erosion, tsurf, tsurfm
            real(8) :: x1, x2, x3, y1, y2, y3, TriangleEdge1, TriangleEdge2, TriangleEdge3
            real(8) :: x1_remove, x2_remove, x3_remove, y1_remove, y2_remove, y3_remove, i_remove1, i_remove2, i_remove3
            integer(4) :: i, i0, i1, i2, i3, ic, ie, iess, ii, iloc, it
            integer(4) :: iremove, itstart, j, ja, jess, jess2, k, kmem, kpar
            integer(4) :: jb, mode, newn, np, nrem, numtri, nv_max
            integer(4) :: nadd
            real(8) :: Distance_x1, Distance_x2, Distance_x3, Distance_y1, Distance_y2, Distance_y3

			real(8) :: triangleSurface, minimumTriangleSurface, maximumTriangleSurface, maximumTriangleAspectRatio
			real(8) :: maximumNodeDistance, minimumNodeDistance, nodeDistance, nodeDistanceMax
			integer(4) :: l, m, minimumNodeNumber, boundaryNode
			integer(4) :: lowerLeft1, upperLeft1, upperRight1, lowerRight1, lowerLeft2, upperLeft2, upperRight2, lowerRight2
			real(8) :: triangleNodeDistance1, triangleNodeDistance2, triangleNodeDistance3, triangleAspectRatio, semiPerimeter
			character(255) :: timeString

			real(8), allocatable, dimension(:) :: boundaryNodesPosition
			integer(4), allocatable, dimension(:) :: boundaryNodes
			integer(4) :: counter1, sortingBuffer2
			real(8) :: sortingBuffer1

            allocate(nn3(nbmax,nnodemax))
            allocate(subset(nnodemax))

			! Defining minimum and maximum triangle surfaces for method 1, distances for method 2, respectively
			! delta = mean nodal spacing [km]
			! Method 1 (based on triangle surfaces projected onto xy-plane)
			maximumTriangleSurface = 0.5_8 * delta * delta * 5.0_8 !2.0_8
!			minimumTriangleSurface = 0.5_8 * delta * delta / 1e2_8 !4.0_8
			! Method 2 (based on absolut distances projected onto the xy-plane
			minimumNodeDistance = delta / 5.0_8
			maximumNodeDistance = 2 * delta
			! Method 3 (based on length aspect ratios of vertices in triangle
!			maximumTriangleAspectRatio = 10.0_8

			! Maximum number of nodes that can be added
			nv_max = nnodemax
			! Minimum number of nodes that can't be removed (should align with maximum node distance and/or triangle surface)
!			minimumNodeNumber = nint(nnode0 / 1.0_8)

			! No idea what this does
		    do i=1,nnodemax
				memory(i,6) = 0.0_8
				inactive(i) = .false.

				if (memory(i,1) .gt. 0.5_8) then
				    memory(i,1) = memory(i,1) + 1.0_8
				end if
			end do

			! Number of time steps that an added node needs to be active before considered for removal
			dhcritmin = 0.0_8

			! iloc = initial guess input for nn_remove, numtri = number of triangles, nrem = initate status for node removal
			iloc = 1
			numtri = nt
			nrem = 0

			! ***** NODE REMOVAL ***** !
			12 continue

!			do i = nnode0 + 1,configData%nnode ! Keeps the original set of points

			do i = 1,configData%nnode
				! Method 1 (after Willet in previous version of code)
				! Node removal condition is based on minimum surface area
				! Triangle node numbers
!				i1 = vertices(1,i)
!				i2 = vertices(2,i)
!				i3 = vertices(3,i)
!				! Triangle node coordinates
!				x1 = x(i1)
!				y1 = y(i1)
!				x2 = x(i2)
!				y2 = y(i2)
!				x3 = x(i3)
!				y3 = y(i3)
				! Triangle node distances
!				triangleNodeDistance1 = sqrt((x1 - x2)**2 + (y1 - y2)**2)
!				triangleNodeDistance2 = sqrt((x1 - x3)**2 + (y1 - y3)**2)
!				triangleNodeDistance3 = sqrt((x2 - x3)**2 + (y2 - y3)**2)
				! Triangle aspect ratio
!				triangleAspectRatio = (triangleNodeDistance1 * triangleNodeDistance2 * triangleNodeDistance3) / &
!					((triangleNodeDistance2 + triangleNodeDistance3 - triangleNodeDistance1) * &
!					(triangleNodeDistance3 + triangleNodeDistance1 - triangleNodeDistance2) * &
!					(triangleNodeDistance1 + triangleNodeDistance2 - triangleNodeDistance3))
				! Calculate triangle surface area (prjected onto x-y plane; Heron's formula)
!				semiPerimeter = (triangleNodeDistance1 + triangleNodeDistance2 + triangleNodeDistance3) / 2.0_8
!				triangleSurface = sqrt(semiPerimeter * &
!					(semiPerimeter - triangleNodeDistance1) * &
!					(semiPerimeter - triangleNodeDistance2) * &
!					(semiPerimeter - triangleNodeDistance3))
!				tsurf=(x1*y2+x2*y3+x3*y1-y1*x2-y2*x3-y3*x1)/2.0_8

				iremove = 0

				! Method 1
!				if ((triangleSurface .lt. minimumTriangleSurface) .and. (memory(i,5) .gt. 0.5_8)) then
!					iremove = 1
!				print*, "Triangle area too small!"
!				print*, "Measured triangle area: ", triangleSurface, "Minimum required triangle area: ", minimumTriangleSurface
!				end if

				! Method 2
				! Check neighbouring nodes and determine distance to each
				do l = 1,nb(i)
					! Distance in 3D space if needed
!					nodeDistance = sqrt((x(nn(l,m)) - x(m))**2 + (y(nn(l,m)) - y(m))**2 + ((h(nn(l,m)) - h(m)) * 1e3)**2)
					nodeDistance = sqrt((x(nn(l,i)) - x(i))**2 + (y(nn(l,i)) - y(i))**2)

					if ((nodeDistance .le. minimumNodeDistance) .and. (memory(i,5) .ge. 0.5_8)) then
						! Making sure distance is not measured between identical points (= 0)
						if (i .ne. nn(l,i)) then
							iremove = 1
							write(22,*), "Time: ", time
							write(22,*), "Node ",i, " removed. x(i): ", x(i), "y(i): ", y(i)
							write(22,*), "Distance to a neighbouring node too small."
							write(22,*), "Permitted minimum distance: ", minimumNodeDistance
							exit
						end if
					end if
				end do
!
!				! Method 3
!				if (triangleAspectRatio .gt. maximumTriangleAspectRatio) then
!					iremove = 1
!					print*, "Triangle aspect ratio too large!"
!				end if

				! Check inner nodes that are too close to the boundaries
				if ((x(i) .gt. 0) .and. (x(i) .lt. configData%sidex) .and. (y(i) .gt. 0) .and. (y(i) .lt. configData%sidey)) then
					if ((x(i) .le. minimumNodeDistance) .or. &
					(y(i) .le. minimumNodeDistance) .or. &
					(x(i) .ge. (configData%sidex - minimumNodeDistance)) .or. &
					(y(i) .ge. (configData%sidey - minimumNodeDistance))) then
						iremove = 1 ! Point is too close to boundaries
						write(22,*), "Time: ", time
						write(22,*), "Node ",i, " removed. x(i): ", x(i), "y(i): ", y(i)
						write(22,*), "Too close to a model boundary."
						write(22,*), "Permitted minimum distance: ", minimumNodeDistance
					end if
				end if

				! Check boundary nodes
				if (memory(i,5) .lt. 0.5) then
					if ((x(i) .eq. 0.0_8) .and. (y(i) .ne. 0.0_8) .and. (y(i) .ne. configData%sidey)) then
						do l = 1,configData%nnode
							if ((x(l) .eq. 0.0_8) .and. (y(i) .ne. y(l))) then
								if (abs(y(i) - y(l)) .le. minimumNodeDistance) then
									iremove = 1
									write(22,*), "Time: ", time
									write(22,*), "Boundary node ",i, " removed. x(i): ", x(i), "y(i): ", y(i)
									write(22,*), "Too close to a neighbouring boundary node."
									write(22,*), "Permitted minimum distance: ", minimumNodeDistance
									exit
								end if
							end if
						end do
					else if ((x(i) .eq. configData%sidex) .and. (y(i) .ne. 0.0_8) .and. (y(i) .ne. configData%sidey)) then
						do l = 1,configData%nnode
							if ((x(l) .eq. configData%sidex) .and. (y(i) .ne. y(l))) then
								if (abs(y(i) - y(l)) .le. minimumNodeDistance) then
									iremove = 1
									write(22,*), "Time: ", time
									write(22,*), "Boundary node ",i, " removed. x(i): ", x(i), "y(i): ", y(i)
									write(22,*), "Too close to a neighbouring boundary node."
									write(22,*), "Permitted minimum distance: ", minimumNodeDistance
									exit
								end if
							end if
						end do
					else if ((y(i) .eq. configData%sidey) .and. (x(i) .ne. 0.0_8) .and. (x(i) .ne. configData%sidex)) then
						do l = 1,configData%nnode
							if ((y(l) .eq. configData%sidey) .and. (x(i) .ne. x(l))) then
								if (abs(x(i) - x(l)) .le. minimumNodeDistance) then
									iremove = 1
									write(22,*), "Time: ", time
									write(22,*), "Boundary node ",i, " removed. x(i): ", x(i), "y(i): ", y(i)
									write(22,*), "Too close to a neighbouring boundary node."
									write(22,*), "Permitted minimum distance: ", minimumNodeDistance
									exit
								end if
							end if
						end do
					else if ((y(i) .eq. 0.0_8) .and. (x(i) .ne. 0.0_8) .and. (x(i) .ne. configData%sidex)) then
						do l = 1,configData%nnode
							if ((y(l) .eq. 0.0_8) .and. (x(i) .ne. x(l))) then
								if (abs(x(i) - x(l)) .le. minimumNodeDistance) then
									iremove = 1
									write(22,*), "Time: ", time
									write(22,*), "Boundary node ",i, " removed. x(i): ", x(i), "y(i): ", y(i)
									write(22,*), "Too close to a neighbouring boundary node."
									write(22,*), "Permitted minimum distance: ", minimumNodeDistance
									exit
								end if
							end if
						end do
					end if
				end if

				! Proceed with formal node removal
				if (iremove .eq. 1) then
					nrem = nrem + 1
					np = configData%nnode

					do k = 1,nb(i)
						memory(nn(k,i),6) = 1.0_8
					end do

					! numtri = revised number of triangles after node removal
					call nn_remove (i,np,numtri,points,vertices,neighbour,iloc,nbmax,nv_max, &
						vis_tlist,vis_elist,add_tlist,&
						v_local,n_local,c_list,nodelist,tlist,.false.)
					do j = i+1,configData%nnode
						points(1,j-1) = points(1,j)
						points(2,j-1) = points(2,j)
						x(j-1) = x(j)
						y(j-1) = y(j)
						h(j-1) = h(j)
						h0(j-1) = h0(j)
						hi(j-1) = hi(j)

						do kpar = 1,nparam
							param(j-1,kpar)=param(j,kpar)
						end do

						do kmem = 1,nmemory
							memory(j-1,nmemory)=memory(j,nmemory)
						end do
					end do

					do it = 1,numtri
						do k = 1,3
							if (vertices(k,it) .gt. i) then
								vertices(k,it) = vertices(k,it) - 1
							end if
						end do
					end do

					configData%nnode = configData%nnode - 1

					do j = 1,configData%nnode
						nb(j) = 0
					end do

					do it = 1,numtri
						i1 = vertices(1,it)
						i2 = vertices(2,it)
						i3 = vertices(3,it)
						nb(i1) = nb(i1)+1

						if (nb(i1) .gt. nbmax) then
							print *, "nbmax: ", nbmax, ", nb: ", nb(i1)
							stop 'nbmax too small...1'
						end if

						nn(nb(i1),i1) = i2
						nb(i2) = nb(i2) + 1

						if (nb(i2) .gt. nbmax) then
							print *, "nbmax: ", nbmax, ", nb: ", nb(i2)
							stop 'nbmax too small...2'
						end if

						nn(nb(i2),i2) = i3
						nb(i3) = nb(i3) + 1

						if (nb(i3) .gt. nbmax) then
							print *, "nbmax: ", nbmax, ", nb: ", nb(i3)
							stop 'nbmax too small...3'
						end if

						nn(nb(i3),i3)=i1
					end do

					! Re-assigning boundary nodes PRE Jan2018
					do l = 1,configData%nnode
						if ((x(l) .le. 0.0_8) .or. (y(l) .le. 0.0_8) .or. &
							(x(l) .ge. configData%sidex) .or. (y(l) .ge. configData%sidey)) then
							memory(l,5) = 0.0_8 ! Point is on the boundary
						else
							memory(l,5) = 1.0_8 ! Point is within the boundaries
						end if
					end do

					goto 12
				end if
			end do

			if (nrem.gt.0) then

				! find nn2,nb2,cell
			    do i = 1,configData%nnode
				    nb2(i) = 0
				end do

			    do it = 1,numtri
					i1 = vertices(1,it)
					i2 = vertices(2,it)
					i3 = vertices(3,it)

					! i1
					nb2(i1) = nb2(i1) + 1
					nn2(nb2(i1),i1) = i2
					cell(i1,int((1. + real(nb2(i1))) / 2.),1) = i2

					nb2(i1) = nb2(i1) + 1
					nn2(nb2(i1),i1) = i3
					cell(i1,int((1. + real(nb2(i1))) / 2.),2) = i3

					if (nb2(i1) .gt. nbmax) then
						print *, "nbmax: ", nbmax, ", nb2: ", nb2(i1)
						stop 'nbmax too small...4'
					end if

					! i2
					nb2(i2) = nb2(i2) + 1
					nn2(nb2(i2),i2) = i3
					cell(i2,int((1. + real(nb2(i2))) / 2.),1) = i1

					nb2(i2) = nb2(i2) + 1
					nn2(nb2(i2),i2) = i1
					cell(i2,int((1. + real(nb2(i2))) / 2.),2) = i3

					if (nb2(i2) .gt. nbmax) then
						print *, "nbmax: ", nbmax, ", nb2: ", nb2(i2)
						stop 'nbmax too small...5'
					end if

					! i3
				    nb2(i3) = nb2(i3) + 1
					nn2(nb2(i3),i3) = i1
					cell(i3,int((1. + real(nb2(i3))) / 2.),1) = i1

					nb2(i3) = nb2(i3) + 1
					nn2(nb2(i3),i3) = i2
					cell(i3,int((1. + real(nb2(i3)))/2.),2) = i2

					if (nb2(i3) .gt. nbmax) then
						print *, "nbmax: ", nbmax, ", nb2: ", nb2(i3)
						stop 'nbmax too small...6'
					end if
				end do

			    ! operate on nn2,nb2: added directly from find_neighbours.f
				do i = 1,configData%nnode

					! classement de nn2 de i le plus gd au plus pt
				    do ie = 1,nb2(i) - 1
						ja = nn2(ie,i)
						jb = ie

						do jess = ie + 1,nb2(i)
							jess2 = nn2(jess,i)

							if(ja .lt. jess2) then
								ja = jess2
								jb = jess
							end if
						end do

						nn2(jb,i) = nn2(ie,i)
						nn2(ie,i) = ja
					end do

					! elimination des pts doubles de enl
					i0 = 1
					nn3(1,i) = nn2(1,i)
					do iess = 2,nb2(i) - 1
						if(nn2(iess,i) .ne. nn3(i0,i)) then
							nn3(i0 + 1,i) = nn2(iess,i)
						    i0 = i0 + 1
						end if
					end do

					do j = 1,i0
						nn2(j,i) = nn3(j,i)
					end do
						nb2(i) = i0
				end do

			! back to regular code

				! recompute surfaces around removed nodes
				call find_surface (configData)
				do i = 1,configData%nnode
					if (memory(i,7) .eq. 0.0_8) then
						print*,'surface nil at ',i,memory(i,6)
						print*,nb(i),(nn(k,i),k = 1,nb(i))

						do k = 1,nb(i)
							print*,x(nn(k,i)),y(nn(k,i))
						end do
					end if
				end do

				! recompute connectivities around removed nodes
				do i = 1,configData%nnode
					nkcon(i) = 0
				enddo

				do it = 1,numtri

					if (vertices(1,it) .le. configData%nnode .and. vertices(2,it) .le. &
						configData%nnode .and. vertices(3,it) .le. configData%nnode) then

						do i = 1,3
							ic = vertices(i,it)
						    nkcon(ic) = nkcon(ic) + 1

							if (nkcon(ic) .gt. ntmax) then
								print *, "ic, nkcon(ic): " ,ic,nkcon(ic)
								print *,(kcon(ii,ic),ii=1,nkcon(ic)-1)
								stop 'checkMeshResolution.f90: too tight connectivity...'
							end if

						    kcon(nkcon(ic),ic)=it
						enddo
					end if
				end do

				! Re-assigning boundary nodes PRE Jan2018
				do i = 1,configData%nnode
					if ((x(i) .le. 0.0_8) .or. (y(i) .le. 0.0_8) .or. &
						(x(i) .ge. configData%sidex) .or. (y(i) .ge. configData%sidey)) then
						memory(i,5) = 0.0_8 ! Point is on the boundary
					else
						memory(i,5) = 1.0_8 ! Point is within the boundaries
					end if
				end do

				! nt = number of triangles after node removal
			    nt = numtri
			end if

			! ***** NODE ADDITION ***** !
			! compute in which triangles points are to be added and location of new points
            nadd = 0

			do it = 1,nt
				jtadd(it) = 0
			enddo

			! CHECK 1: Node addition within triangles
			do it = 1,nt

				! Calculate triangle surface area (Check 1)
				! Triangle vertex node number
				i1 = vertices(1,it)
				i2 = vertices(2,it)
				i3 = vertices(3,it)

				! Triangle vertex coordinates
				x1 = x(i1)
				y1 = y(i1)
				x2 = x(i2)
				y2 = y(i2)
				x3 = x(i3)
				y3 = y(i3)

				! Node distances
				triangleNodeDistance1 = sqrt((x1 - x2)**2 + (y1 - y2)**2)
				triangleNodeDistance2 = sqrt((x1 - x3)**2 + (y1 - y3)**2)
				triangleNodeDistance3 = sqrt((x2 - x3)**2 + (y2 - y3)**2)

				! Triangle area (projected onto xy plane)
				semiPerimeter = (triangleNodeDistance1 + triangleNodeDistance2 + triangleNodeDistance3) / 2.0_8
				triangleSurface = sqrt(semiPerimeter * &
					(semiPerimeter - triangleNodeDistance1) * &
					(semiPerimeter - triangleNodeDistance2) * &
					(semiPerimeter - triangleNodeDistance3))
!				triangleSurface = (x1 * y2 + x2 * y3 + x3 * y1 - y1 * x2 - y2 * x3 - y3 * x1) / 2.0_8

				if (triangleSurface .ge. maximumTriangleSurface) then

					jtadd(it) = 1
					nadd = nadd + 1

					if (configData%nnode + nadd .gt. nnodemax) then
						print*, configData%nnode,nadd,configData%nnode+nadd,nnodemax
						print*,'Too many nodes in routine checkMeshResolution'
						stop
					end if

					itadd(nadd) = it
					newn = configData%nnode + nadd

					! adds point randomly inside of the triangle uses isoparametric basis functions to interpolate
					! position, height, etc. to new point

					call random_number(s)

				    do ii = 1,3
						! Let the random number be a bit more "central" PRE Jan2018
						! The previous adjustment seems to cause an error in finding a solution for diffusion ("Stop too many iterations...)
						if (s(ii) .gt. 0.5) then
							s(ii) = s(ii) - 0.2_8
						else
							s(ii) = s(ii) + 0.2_8
						end if
!						s(ii) = 0.8_8 * s(ii) + 0.1_8

					enddo

					bcnt = bdry(i1) + bdry(i2) + bdry(i3)
				    if (bcnt .ne. 2) then
						s(2) = (1.0_8 - s(1)) * s(2)
						s(3) = 1.0_8 - s(1) - s(2)
					    bdry(newn) = 0
					else
						if (bdry(i1) + bdry(i2) .eq. 2) then
							s(2) = 1.0_8 - s(1)
							s(3) = 0.0_8
					    elseif (bdry(i2) + bdry(i3) .eq. 2) then
							s(3) = 1.0_8 - s(2)
							s(1) = 0.0_8
					    else
							s(1) = 1.0_8 - s(3)
							s(2) = 0.0_8
					   end if
					   bdry(newn) = 1
					end if

					! Interpolate values using linear interp:  h(x,y) = N1h1 + N2h2 + N3h3 for linear triangles
					! N1=s(1) N2=s(2) N3=s(3) the values of s(i) vary depending if the triangle is on the moving bdry

					x(newn) = s(1) * x(i1) + s(2) * x(i2) + s(3) * x(i3)
					y(newn) = s(1) * y(i1) + s(2) * y(i2) + s(3) * y(i3)
					h(newn) = s(1) * h(i1) + s(2) * h(i2) + s(3) * h(i3)
					h0(newn) = s(1) * h0(i1) + s(2) * h0(i2) + s(3) * h0(i3)
					hi(newn) = s(1) * hi(i1) + s(2) * hi(i2) + s(3) * hi(i3)


					memory(newn,1) = 1.0_8
					memory(newn,2) = s(1) * memory(i1,2) + s(2) * memory(i2,2) + s(3) * memory(i3,2)
					memory(newn,3) = s(1) * memory(i1,3) + s(2) * memory(i2,3) + s(3) * memory(i3,3)
					memory(newn,4) = s(1) * memory(i1,4) + s(2) * memory(i2,4) + s(3) * memory(i3,4)
					memory(newn,8) = s(1) * memory(i1,8) + s(2) * memory(i2,8) + s(3) * memory(i3,8)

					memory(newn,6) = 0.0_8
					memory(newn,7) = 0.0_8

					memory(newn,5) = 1.0_8

					if ((memory(i1,5) .lt. 0.5_8) .and. (memory(i2,5) .lt. 0.5_8) .and. (memory(i3,5) .lt. 0.5_8)) then
						memory(newn,5) = 0.0_8
					end if

					do kpar = 1,nparam
						param(newn,kpar) = s(1) * param(i1,kpar) + s(2) * param(i2,kpar) + s(3) * param(i3,kpar)
					end do

					points(1,newn) = dble(x(newn))
					points(2,newn) = dble(y(newn))
					memory(newn,1) = 1.0_8

					write(22,*), "Time: ", time
					write(22,*), "Node ", newn, " added. x(i): ", x(newn), "y(i): ", y(newn), "h(i): ", h(newn)
					write(22,*), "Triangle area too large."
					write(22,*), "Maximum permitted triangle area: ", maximumTriangleSurface
				end if
			end do

			! CHECK 2: Boundary node addition
			! Boundary along y-axis at x = 0
			counter1 = 0
			allocate(boundaryNodes(configData%nnode), boundaryNodesPosition(configData%nnode))
			do i=1,configData%nnode
				if ((x(i) .eq. 0.0_8) .and. (y(i) .ge. 0.0_8) .and. (y(i) .le. configdata%sidey)) then
					counter1 = counter1 + 1
					boundaryNodes(counter1) = i
					boundaryNodesPosition(counter1) = y(i)
				end if
			end do
			! Sort boundary nodes array
			if (counter1 .gt. 1) then
				do i=1,counter1
					j = minloc(boundaryNodesPosition(i:counter1), dim=1) + i - 1
					sortingBuffer1 = boundaryNodesPosition(i)
					sortingBuffer2 = boundaryNodes(i)
					boundaryNodesPosition(i) = boundaryNodesPosition(j)
					boundaryNodes(i) = boundaryNodes(j)
					boundaryNodesPosition(j) = sortingBuffer1
					boundaryNodes(j) = sortingBuffer2
				end do
				! Add nodes, if needed
				do i=1,(counter1-1)
					if (abs(boundaryNodesPosition(i+1) - boundaryNodesPosition(i)) .ge. maximumNodeDistance) then
						nadd = nadd + 1
						newn = configData%nnode + nadd

						x(newn) = 0.0_8
						y(newn) = (boundaryNodesPosition(i+1) + boundaryNodesPosition(i)) / 2
						h(newn) = (h(boundaryNodes(i+1)) + h(boundaryNodes(i))) / 2
						hi(newn) = (hi(boundaryNodes(i+1)) + hi(boundaryNodes(i))) / 2

						memory(newn,1) = 1.0_8
						memory(newn,2) = (memory(boundaryNodes(i+1),2) + memory(boundaryNodes(i),2)) / 2
						memory(newn,3) = (memory(boundaryNodes(i+1),3) + memory(boundaryNodes(i),3)) / 2
						memory(newn,4) = (memory(boundaryNodes(i+1),4) + memory(boundaryNodes(i),4)) / 2
						memory(newn,8) = (memory(boundaryNodes(i+1),8) + memory(boundaryNodes(i),8)) / 2

						memory(newn,6) = 0.0_8
						memory(newn,7) = 0.0_8

						memory(newn,5) = 0.0_8

						do kpar = 1,nparam
							param(newn,kpar) = (param(boundaryNodes(i),kpar) + param(boundaryNodes(i+1),kpar))
						end do

						points(1,newn) = dble(x(newn))
						points(2,newn) = dble(y(newn))
						memory(newn,1) = 1.0_8

						write(22,*), "Time: ", time
						write(22,*), "Boundary node ", newn, " added. x(i): ", x(newn), "y(i): ", y(newn), "h(i): ", h(newn)
						write(22,*), "Distance to a neighbouring boundary node too large."
						write(22,*), "Permitted maximum distance: ", maximumNodeDistance
					end if
				end do
			end if

			deallocate(boundaryNodes, boundaryNodesPosition)

			! Boundary along y-axis at x = x_max
			counter1 = 0
			allocate(boundaryNodes(configData%nnode), boundaryNodesPosition(configData%nnode))
			do i=1,configData%nnode
				if ((x(i) .eq. configData%sidex) .and. (y(i) .ge. 0.0_8) .and. (y(i) .le. configdata%sidey)) then
					counter1 = counter1 + 1
					boundaryNodes(counter1) = i
					boundaryNodesPosition(counter1) = y(i)
				end if
			end do
			! Sort boundary nodes array
			if (counter1 .gt. 1) then
				do i=1,counter1
					j = minloc(boundaryNodesPosition(i:counter1), dim=1) + i - 1
					sortingBuffer1 = boundaryNodesPosition(i)
					sortingBuffer2 = boundaryNodes(i)
					boundaryNodesPosition(i) = boundaryNodesPosition(j)
					boundaryNodes(i) = boundaryNodes(j)
					boundaryNodesPosition(j) = sortingBuffer1
					boundaryNodes(j) = sortingBuffer2
				end do
				! Add nodes, if needed
				do i=1,(counter1-1)
					if (abs(boundaryNodesPosition(i+1) - boundaryNodesPosition(i)) .ge. maximumNodeDistance) then
						nadd = nadd + 1
						newn = configData%nnode + nadd

						x(newn) = configData%sidex
						y(newn) = (boundaryNodesPosition(i+1) + boundaryNodesPosition(i)) / 2
						h(newn) = (h(boundaryNodes(i+1)) + h(boundaryNodes(i))) / 2
						hi(newn) = (hi(boundaryNodes(i+1)) + hi(boundaryNodes(i))) / 2

						memory(newn,1) = 1.0_8
						memory(newn,2) = (memory(boundaryNodes(i+1),2) + memory(boundaryNodes(i),2)) / 2
						memory(newn,3) = (memory(boundaryNodes(i+1),3) + memory(boundaryNodes(i),3)) / 2
						memory(newn,4) = (memory(boundaryNodes(i+1),4) + memory(boundaryNodes(i),4)) / 2
						memory(newn,8) = (memory(boundaryNodes(i+1),8) + memory(boundaryNodes(i),8)) / 2

						memory(newn,6) = 0.0_8
						memory(newn,7) = 0.0_8

						memory(newn,5) = 0.0_8

						do kpar = 1,nparam
							param(newn,kpar) = (param(boundaryNodes(i),kpar) + param(boundaryNodes(i+1),kpar))
						end do

						points(1,newn) = dble(x(newn))
						points(2,newn) = dble(y(newn))
						memory(newn,1) = 1.0_8

						write(22,*), "Time: ", time
						write(22,*), "Boundary node ", newn, " added. x(i): ", x(newn), "y(i): ", y(newn), "h(i): ", h(newn)
						write(22,*), "Distance to a neighbouring boundary node too large."
						write(22,*), "Permitted maximum distance: ", maximumNodeDistance
					end if
				end do
			end if

			deallocate(boundaryNodes, boundaryNodesPosition)

			! Boundary along x-axis at y = 0
			counter1 = 0
			allocate(boundaryNodes(configData%nnode), boundaryNodesPosition(configData%nnode))
			do i=1,configData%nnode
				if ((y(i) .eq. 0.0_8) .and. (x(i) .ge. 0.0_8) .and. (x(i) .le. configdata%sidex)) then
					counter1 = counter1 + 1
					boundaryNodes(counter1) = i
					boundaryNodesPosition(counter1) = x(i)
				end if
			end do
			! Sort boundary nodes array
			if (counter1 .gt. 1) then
				do i=1,counter1
					j = minloc(boundaryNodesPosition(i:counter1), dim=1) + i - 1
					sortingBuffer1 = boundaryNodesPosition(i)
					sortingBuffer2 = boundaryNodes(i)
					boundaryNodesPosition(i) = boundaryNodesPosition(j)
					boundaryNodes(i) = boundaryNodes(j)
					boundaryNodesPosition(j) = sortingBuffer1
					boundaryNodes(j) = sortingBuffer2
				end do

				! Add nodes, if needed
				do i=1,(counter1-1)
					if (abs(boundaryNodesPosition(i+1) - boundaryNodesPosition(i)) .ge. maximumNodeDistance) then
					write(22,*), "Node 1: ", boundaryNodesPosition(i), "Node 2: ", boundaryNodesPosition(i+1)
						nadd = nadd + 1
						newn = configData%nnode + nadd

						y(newn) = 0.0_8
						x(newn) = (boundaryNodesPosition(i+1) + boundaryNodesPosition(i)) / 2
						h(newn) = (h(boundaryNodes(i+1)) + h(boundaryNodes(i))) / 2
						hi(newn) = (hi(boundaryNodes(i+1)) + hi(boundaryNodes(i))) / 2

						memory(newn,1) = 1.0_8
						memory(newn,2) = (memory(boundaryNodes(i+1),2) + memory(boundaryNodes(i),2)) / 2
						memory(newn,3) = (memory(boundaryNodes(i+1),3) + memory(boundaryNodes(i),3)) / 2
						memory(newn,4) = (memory(boundaryNodes(i+1),4) + memory(boundaryNodes(i),4)) / 2
						memory(newn,8) = (memory(boundaryNodes(i+1),8) + memory(boundaryNodes(i),8)) / 2

						memory(newn,6) = 0.0_8
						memory(newn,7) = 0.0_8

						memory(newn,5) = 0.0_8

						do kpar = 1,nparam
							param(newn,kpar) = (param(boundaryNodes(i),kpar) + param(boundaryNodes(i+1),kpar))
						end do

						points(1,newn) = dble(x(newn))
						points(2,newn) = dble(y(newn))
						memory(newn,1) = 1.0_8

						write(22,*), "Time: ", time
						write(22,*), "Boundary node ", newn, " added. x(i): ", x(newn), "y(i): ", y(newn), "h(i): ", h(newn)
						write(22,*), "Distance to a neighbouring boundary node too large."
						write(22,*), "Permitted maximum distance: ", maximumNodeDistance
					end if
				end do
			end if

			deallocate(boundaryNodes, boundaryNodesPosition)

			! Boundary along x-axis at y = y_max
			counter1 = 0
			allocate(boundaryNodes(configData%nnode), boundaryNodesPosition(configData%nnode))
			do i=1,configData%nnode
				if ((y(i) .eq. configData%sidey) .and. (x(i) .ge. 0.0_8) .and. (x(i) .le. configdata%sidex)) then
					counter1 = counter1 + 1
					boundaryNodes(counter1) = i
					boundaryNodesPosition(counter1) = x(i)
				end if
			end do
			! Sort boundary nodes array
			if (counter1 .gt. 1) then
				do i=1,counter1
					j = minloc(boundaryNodesPosition(i:counter1), dim=1) + i - 1
					sortingBuffer1 = boundaryNodesPosition(i)
					sortingBuffer2 = boundaryNodes(i)
					boundaryNodesPosition(i) = boundaryNodesPosition(j)
					boundaryNodes(i) = boundaryNodes(j)
					boundaryNodesPosition(j) = sortingBuffer1
					boundaryNodes(j) = sortingBuffer2
				end do

				! Add nodes, if needed
				do i=1,(counter1-1)
					if (abs(boundaryNodesPosition(i+1) - boundaryNodesPosition(i)) .ge. maximumNodeDistance) then
						nadd = nadd + 1
						newn = configData%nnode + nadd

						y(newn) = configData%sidey
						x(newn) = (boundaryNodesPosition(i+1) + boundaryNodesPosition(i)) / 2
						h(newn) = (h(boundaryNodes(i+1)) + h(boundaryNodes(i))) / 2
						hi(newn) = (hi(boundaryNodes(i+1)) + hi(boundaryNodes(i))) / 2

						memory(newn,1) = 1.0_8
						memory(newn,2) = (memory(boundaryNodes(i+1),2) + memory(boundaryNodes(i),2)) / 2
						memory(newn,3) = (memory(boundaryNodes(i+1),3) + memory(boundaryNodes(i),3)) / 2
						memory(newn,4) = (memory(boundaryNodes(i+1),4) + memory(boundaryNodes(i),4)) / 2
						memory(newn,8) = (memory(boundaryNodes(i+1),8) + memory(boundaryNodes(i),8)) / 2

						memory(newn,6) = 0.0_8
						memory(newn,7) = 0.0_8

						memory(newn,5) = 0.0_8

						do kpar = 1,nparam
							param(newn,kpar) = (param(boundaryNodes(i),kpar) + param(boundaryNodes(i+1),kpar))
						end do

						points(1,newn) = dble(x(newn))
						points(2,newn) = dble(y(newn))
						memory(newn,1) = 1.0_8

						write(22,*), "Time: ", time
						write(22,*), "Boundary node ", newn, " added. x(i): ", x(newn), "y(i): ", y(newn), "h(i): ", h(newn)
						write(22,*), "Distance to a neighbouring boundary node too large."
						write(22,*), "Permitted maximum distance: ", maximumNodeDistance
					end if
				end do
			end if

			deallocate(boundaryNodes, boundaryNodesPosition)

!			! CHECK 2: Boundary node addition near model corners
!			! Initialise add node condition (1: add node)
!			lowerLeft1 = 1
!			lowerLeft2 = 1
!			upperLeft1 = 1
!			upperLeft2 = 1
!			upperRight1 = 1
!			upperRight2 = 1
!			lowerRight1 = 1
!			lowerRight2 = 1
!
!			! Check if node needs to be added at corner (0: do not add node)
!			do i=1,configData%nnode
!				if ((memory(i,5) .lt. 0.5) .and. (x(i) .eq. 0.0_8) .and. (y(i) .lt. (2 * delta)) .and. (y(i) .ne. 0.0_8)) then
!					lowerLeft1 = 0
!				else if ((memory(i,5) .lt. 0.5) .and. (y(i) .eq. 0.0_8) .and. (x(i) .lt. (2 * delta)) .and. (x(i) .ne. 0.0_8)) then
!					lowerLeft2 = 0
!				else if ((memory(i,5) .lt. 0.5) .and. (x(i) .eq. 0.0_8) .and. (y(i) .gt. (configdata%sidey - 2 * delta)) .and. &
!				(y(i) .ne. configdata%sidey)) then
!					upperLeft1 = 0
!				else if ((memory(i,5) .lt. 0.5) .and. (y(i) .eq. configdata%sidey) .and. (x(i) .lt. (2 * delta)) .and. &
!				(x(i) .ne. 0.0_8)) then
!					upperLeft2 = 0
!				else if ((memory(i,5) .lt. 0.5) .and. (y(i) .eq. configdata%sidey) .and. &
!				(x(i) .gt. (configData%sidex - 2 * delta)) .and. (x(i) .ne. configData%sidex)) then
!					upperRight1 = 0
!				else if	((memory(i,5) .lt. 0.5) .and. (x(i) .eq. configdata%sidex) .and. (y(i) .gt. &
!				(configData%sidey - 2 * delta)) .and. (y(i) .ne. configData%sidey)) then
!					upperRight2 = 0
!				else if ((memory(i,5) .lt. 0.5) .and. (x(i) .eq. configdata%sidex) .and. (y(i) .lt. (2 * delta)) .and. &
!				(y(i) .ne. 0.0_8)) then
!					lowerRight1 = 0
!				else if ((memory(i,5) .lt. 0.5) .and. (y(i) .eq. 0.0_8) .and. (x(i) .gt. (configData%sidex - 2 * delta)) .and. &
!				(x(i) .ne. configData%sidex)) then
!					lowerRight2 = 0
!				end if
!			end do
!
!			! Add node at corner
!			if (lowerLeft1 .eq. 1) then
!
!				nadd = nadd + 1
!				newn = configData%nnode + nadd
!
!				x(newn) = 0.0_8
!				y(newn) = delta
!				h(newn) = (h(i) + h(nn(l,i))) / 2
!				hi(newn) = (hi(i) + hi(nn(l,i))) / 2
!
!				memory(newn,1) = 1.0_8
!				memory(newn,2) = (memory(i,2) + memory(nn(l,i),2)) / 2
!				memory(newn,3) = (memory(i,3) + memory(nn(l,i),3)) / 2
!				memory(newn,4) = (memory(i,4) + memory(nn(l,i),4)) / 2
!				memory(newn,8) = (memory(i,8) + memory(nn(l,i),8)) / 2
!
!				memory(newn,6) = 0.0_8
!				memory(newn,7) = 0.0_8
!
!				memory(newn,5) = 0.0_8
!
!				do kpar = 1,nparam
!					param(newn,kpar) = (param(i,kpar) + param(nn(l,i),kpar))
!				end do
!
!				points(1,newn) = dble(x(newn))
!				points(2,newn) = dble(y(newn))
!				memory(newn,1) = 1.0_8
!
!				write(22,*), "Time: ", time
!				write(22,*), "Boundary node added."
!				write(22,*), "Added node i: ", newn, "x(i): ", x(newn), "y(i): ", y(newn), "h(i): ", h(newn)
!			end if
!
!			if (lowerLeft2 .eq. 1) then
!
!				nadd = nadd + 1
!				newn = configData%nnode + nadd
!
!				x(newn) = delta
!				y(newn) = 0.0_8
!				h(newn) = (h(i) + h(nn(l,i))) / 2
!				hi(newn) = (hi(i) + hi(nn(l,i))) / 2
!
!				memory(newn,1) = 1.0_8
!				memory(newn,2) = (memory(i,2) + memory(nn(l,i),2)) / 2
!				memory(newn,3) = (memory(i,3) + memory(nn(l,i),3)) / 2
!				memory(newn,4) = (memory(i,4) + memory(nn(l,i),4)) / 2
!				memory(newn,8) = (memory(i,8) + memory(nn(l,i),8)) / 2
!
!				memory(newn,6) = 0.0_8
!				memory(newn,7) = 0.0_8
!
!				memory(newn,5) = 0.0_8
!
!				do kpar = 1,nparam
!					param(newn,kpar) = (param(i,kpar) + param(nn(l,i),kpar))
!				end do
!
!				points(1,newn) = dble(x(newn))
!				points(2,newn) = dble(y(newn))
!				memory(newn,1) = 1.0_8
!
!				write(22,*), "Time: ", time
!				write(22,*), "Boundary node added."
!				write(22,*), "Added node i: ", newn, "x(i): ", x(newn), "y(i): ", y(newn), "h(i): ", h(newn)
!			end if
!
!			if (upperLeft1 .eq. 1) then
!
!				nadd = nadd + 1
!				newn = configData%nnode + nadd
!
!				x(newn) = 0.0_8
!				y(newn) = configdata%sidey - delta
!				h(newn) = (h(i) + h(nn(l,i))) / 2
!				hi(newn) = (hi(i) + hi(nn(l,i))) / 2
!
!				memory(newn,1) = 1.0_8
!				memory(newn,2) = (memory(i,2) + memory(nn(l,i),2)) / 2
!				memory(newn,3) = (memory(i,3) + memory(nn(l,i),3)) / 2
!				memory(newn,4) = (memory(i,4) + memory(nn(l,i),4)) / 2
!				memory(newn,8) = (memory(i,8) + memory(nn(l,i),8)) / 2
!
!				memory(newn,6) = 0.0_8
!				memory(newn,7) = 0.0_8
!
!				memory(newn,5) = 0.0_8
!
!				do kpar = 1,nparam
!					param(newn,kpar) = (param(i,kpar) + param(nn(l,i),kpar))
!				end do
!
!				points(1,newn) = dble(x(newn))
!				points(2,newn) = dble(y(newn))
!				memory(newn,1) = 1.0_8
!
!				write(22,*), "Time: ", time
!				write(22,*), "Boundary node added."
!				write(22,*), "Added node i: ", newn, "x(i): ", x(newn), "y(i): ", y(newn), "h(i): ", h(newn)
!			end if
!
!			if (upperLeft2 .eq. 1) then
!
!				nadd = nadd + 1
!				newn = configData%nnode + nadd
!
!				x(newn) = delta
!				y(newn) = configdata%sidey
!				h(newn) = (h(i) + h(nn(l,i))) / 2
!				hi(newn) = (hi(i) + hi(nn(l,i))) / 2
!
!				memory(newn,1) = 1.0_8
!				memory(newn,2) = (memory(i,2) + memory(nn(l,i),2)) / 2
!				memory(newn,3) = (memory(i,3) + memory(nn(l,i),3)) / 2
!				memory(newn,4) = (memory(i,4) + memory(nn(l,i),4)) / 2
!				memory(newn,8) = (memory(i,8) + memory(nn(l,i),8)) / 2
!
!				memory(newn,6) = 0.0_8
!				memory(newn,7) = 0.0_8
!
!				memory(newn,5) = 0.0_8
!
!				do kpar = 1,nparam
!					param(newn,kpar) = (param(i,kpar) + param(nn(l,i),kpar))
!				end do
!
!				points(1,newn) = dble(x(newn))
!				points(2,newn) = dble(y(newn))
!				memory(newn,1) = 1.0_8
!
!				write(22,*), "Time: ", time
!				write(22,*), "Boundary node added."
!				write(22,*), "Added node i: ", newn, "x(i): ", x(newn), "y(i): ", y(newn), "h(i): ", h(newn)
!			end if
!
!			if (upperRight1 .eq. 1) then
!
!				nadd = nadd + 1
!				newn = configData%nnode + nadd
!
!				x(newn) = configdata%sidex - delta
!				y(newn) = configdata%sidey
!				h(newn) = (h(i) + h(nn(l,i))) / 2
!				hi(newn) = (hi(i) + hi(nn(l,i))) / 2
!
!				memory(newn,1) = 1.0_8
!				memory(newn,2) = (memory(i,2) + memory(nn(l,i),2)) / 2
!				memory(newn,3) = (memory(i,3) + memory(nn(l,i),3)) / 2
!				memory(newn,4) = (memory(i,4) + memory(nn(l,i),4)) / 2
!				memory(newn,8) = (memory(i,8) + memory(nn(l,i),8)) / 2
!
!				memory(newn,6) = 0.0_8
!				memory(newn,7) = 0.0_8
!
!				memory(newn,5) = 0.0_8
!
!				do kpar = 1,nparam
!					param(newn,kpar) = (param(i,kpar) + param(nn(l,i),kpar))
!				end do
!
!				points(1,newn) = dble(x(newn))
!				points(2,newn) = dble(y(newn))
!				memory(newn,1) = 1.0_8
!
!				write(22,*), "Time: ", time
!				write(22,*), "Boundary node added."
!				write(22,*), "Added node i: ", newn, "x(i): ", x(newn), "y(i): ", y(newn), "h(i): ", h(newn)
!			end if
!
!			if (upperRight2 .eq. 1) then
!
!				nadd = nadd + 1
!				newn = configData%nnode + nadd
!
!				x(newn) = configData%sidex
!				y(newn) = configdata%sidey - delta
!				h(newn) = (h(i) + h(nn(l,i))) / 2
!				hi(newn) = (hi(i) + hi(nn(l,i))) / 2
!
!				memory(newn,1) = 1.0_8
!				memory(newn,2) = (memory(i,2) + memory(nn(l,i),2)) / 2
!				memory(newn,3) = (memory(i,3) + memory(nn(l,i),3)) / 2
!				memory(newn,4) = (memory(i,4) + memory(nn(l,i),4)) / 2
!				memory(newn,8) = (memory(i,8) + memory(nn(l,i),8)) / 2
!
!				memory(newn,6) = 0.0_8
!				memory(newn,7) = 0.0_8
!
!				memory(newn,5) = 0.0_8
!
!				do kpar = 1,nparam
!					param(newn,kpar) = (param(i,kpar) + param(nn(l,i),kpar))
!				end do
!
!				points(1,newn) = dble(x(newn))
!				points(2,newn) = dble(y(newn))
!				memory(newn,1) = 1.0_8
!
!				write(22,*), "Time: ", time
!				write(22,*), "Boundary node added."
!				write(22,*), "Added node i: ", newn, "x(i): ", x(newn), "y(i): ", y(newn), "h(i): ", h(newn)
!			end if
!
!			if (lowerRight2 .eq. 1) then
!
!				nadd = nadd + 1
!				newn = configData%nnode + nadd
!
!				x(newn) = configData%sidex
!				y(newn) = delta
!				h(newn) = (h(i) + h(nn(l,i))) / 2
!				hi(newn) = (hi(i) + hi(nn(l,i))) / 2
!
!				memory(newn,1) = 1.0_8
!				memory(newn,2) = (memory(i,2) + memory(nn(l,i),2)) / 2
!				memory(newn,3) = (memory(i,3) + memory(nn(l,i),3)) / 2
!				memory(newn,4) = (memory(i,4) + memory(nn(l,i),4)) / 2
!				memory(newn,8) = (memory(i,8) + memory(nn(l,i),8)) / 2
!
!				memory(newn,6) = 0.0_8
!				memory(newn,7) = 0.0_8
!
!				memory(newn,5) = 0.0_8
!
!				do kpar = 1,nparam
!					param(newn,kpar) = (param(i,kpar) + param(nn(l,i),kpar))
!				end do
!
!				points(1,newn) = dble(x(newn))
!				points(2,newn) = dble(y(newn))
!				memory(newn,1) = 1.0_8
!
!				write(22,*), "Time: ", time
!				write(22,*), "Boundary node added."
!				write(22,*), "Added node i: ", newn, "x(i): ", x(newn), "y(i): ", y(newn), "h(i): ", h(newn)
!			end if
!
!			if (lowerRight2 .eq. 1) then
!
!				nadd = nadd + 1
!				newn = configData%nnode + nadd
!
!				x(newn) = configdata%sidex - delta
!				y(newn) = 0.0_8
!				h(newn) = (h(i) + h(nn(l,i))) / 2
!				hi(newn) = (hi(i) + hi(nn(l,i))) / 2
!
!				memory(newn,1) = 1.0_8
!				memory(newn,2) = (memory(i,2) + memory(nn(l,i),2)) / 2
!				memory(newn,3) = (memory(i,3) + memory(nn(l,i),3)) / 2
!				memory(newn,4) = (memory(i,4) + memory(nn(l,i),4)) / 2
!				memory(newn,8) = (memory(i,8) + memory(nn(l,i),8)) / 2
!
!				memory(newn,6) = 0.0_8
!				memory(newn,7) = 0.0_8
!
!				memory(newn,5) = 0.0_8
!
!				do kpar = 1,nparam
!					param(newn,kpar) = (param(i,kpar) + param(nn(l,i),kpar))
!				end do
!
!				points(1,newn) = dble(x(newn))
!				points(2,newn) = dble(y(newn))
!				memory(newn,1) = 1.0_8
!
!				write(22,*), "Time: ", time
!				write(22,*), "Boundary node added."
!				write(22,*), "Added node i: ", newn, "x(i): ", x(newn), "y(i): ", y(newn), "h(i): ", h(newn)
!			end if

			if (nadd.eq.0) then
				return
			end if

			! compute new Delaunay triangulation
			mode = 3
			np = configData%nnode + nadd
			itstart = 1
			numtri = nt

			call delaun (points,np,neighbour,vertices,numtri,2*np, &
				vis_tlist,vis_elist,add_tlist,eps,nv_max, &
				mode,inactive,configData%nnode+1,itstart,subset)

			nt = numtri

            configData%nnode = configData%nnode + nadd

			! compute neighbour list
			do i=1,configData%nnode
				nb(i)  = 0
				nb2(i) = 0
			enddo

			! find nn,nb, nb2,nn2,cell
			do it = 1,nt
				i1 = vertices(1,it)
				i2 = vertices(2,it)
				i3 = vertices(3,it)

				! i1
				nb(i1) = nb(i1) + 1
				nn(nb(i1),i1) = i2

				if (nb(i1) .gt. nbmax) then
					print *, "nbmax: ", nbmax, ", nb: ", nb(i1)
					stop 'nbmax too small...7'
				end if

				nb2(i1) = nb2(i1) + 1
				nn2(nb2(i1),i1) = i2
				cell(i1,int((1. + real(nb2(i1))) / 2.),1) = i2

				nb2(i1) = nb2(i1) + 1
				nn2(nb2(i1),i1) = i3
				cell(i1,int((1. + real(nb2(i1))) / 2.),2) = i3

				if (nb2(i1) .gt. nbmax) then
					print *, "nbmax: ", nbmax, ", nb2: ", nb2(i1)
					stop 'nbmax too small...8'
				end if

				! i2
				nb(i2) = nb(i2) + 1
				nn(nb(i2),i2) = i3

				if (nb(i2) .gt. nbmax) then
					print *, "nbmax: ", nbmax, ", nb: ", nb(i2)
					stop 'nbmax too small...9'
				end if

				nb2(i2) = nb2(i2) + 1
				nn2(nb2(i2),i2) = i3
				cell(i2,int((1. + real(nb2(i2)))/2.),1) = i1

				nb2(i2) = nb2(i2) + 1
				nn2(nb2(i2),i2) = i1
				cell(i2,int((1. + real(nb2(i2))) / 2.),2) = i3

				if (nb2(i2) .gt. nbmax) then
					print *, "nbmax: ", nbmax, ", nb2: ", nb2(i2)
					stop 'nbmax too small...10'
				endif

				! i3
				nb(i3) = nb(i3) + 1
				nn(nb(i3),i3) = i1

				if (nb(i3) .gt. nbmax) then
					print *, "nbmax: ", nbmax, ", nb: ", nb(i3)
					stop 'nbmax too small...11'
				endif

				nb2(i3) = nb2(i3) + 1
				nn2(nb2(i3),i3) = i1
				cell(i3,int((1. + real(nb2(i3))) / 2.),1) = i1

				nb2(i3) = nb2(i3) + 1
				nn2(nb2(i3),i3) = i2
				cell(i3,int((1. + real(nb2(i3))) / 2.),2) = i2

				if (nb2(i3).gt.nbmax) then
					print *, "nbmax: ", nbmax, ", nb2: ", nb2(i3)
					stop 'nbmax too small...12'
				end if

			end do

			! Operate on nn2,nb2 (added directly from find_neighbours.f)
			do i = 1,configData%nnode

				! classement de nn2 de i le plus gd au plus pt
				do ie = 1,nb2(i) - 1
					ja = nn2(ie,i)
					jb = ie

					do jess=ie+1,nb2(i)
						jess2=nn2(jess,i)

						if(ja .lt. jess2) then
							ja = jess2
							jb = jess
						end if

					end do

					nn2(jb,i)=nn2(ie,i)
					nn2(ie,i)=ja
				end do

				! elimination des pts doubles de enl
				i0 = 1
				nn3(1,i) = nn2(1,i)

				do iess=2,nb2(i)-1

					if (nn2(iess,i) .ne. nn3(i0,i)) then
						nn3(i0 + 1,i) = nn2(iess,i)
						i0 = i0 + 1
					end if

				end do

				do j = 1,i0
				  nn2(j,i)=nn3(j,i)
				end do

				nb2(i)=i0
			end do

			! back to regular code
			do i = (configData%nnode - nadd + 1),configData%nnode
				memory(i,6) = 1.0_8

				do j = 1,nb(i)
					memory(nn(j,i),6)=1.0_8
				end do

			end do

			! finds new surfaces where needed
			call find_surface (configData)

			do i = 1,configData%nnode
				nb(i) = nb(i) + 1

				if (nb(i) .gt. nbmax) then
					print *, "nbmax: ", nbmax, ", nb: ", nb(i)
					stop 'nbmax too small...12'
				end if

				nn(nb(i),i) = i
			end do

			deallocate(nn3)
			deallocate(subset)

			! Re-assigning boundary nodes PRE Jan2018
			do i = 1,configData%nnode
				if ((x(i) .le. 0.0_8) .or. (y(i) .le. 0.0_8) .or. &
					(x(i) .ge. configData%sidex) .or. (y(i) .ge. configData%sidey)) then
					memory(i,5) = 0.0_8 ! Point is on the boundary
				else
					memory(i,5) = 1.0_8 ! Point is within the boundaries
				end if
			end do

			return

        end subroutine checkMeshResolution

!##################################################################################################################################!
!                                                   SUPPLEMENTAL SUBROUTINES
!##################################################################################################################################!
end module m_checkMeshResolution
