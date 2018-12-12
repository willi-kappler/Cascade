!------------------------------------------------------------------------
!
!   nn2d_setup - Performs all setup procedures for natural neighbour 
!            interpolation routine nn2D.
!
!   Input:
!           np          number of nodes
!           nt_max          maximum number of triangles
!           nh_max          maximum number of triangles on convex 
!                   hull (max size of array hulltriangles)
!       np_max          maximum number of nodes 
!       nnpn_max        maximum number of neighbours per node
!                   (depends on the point distribution,
!                                        set to ~20 in calling program)
!       nmax            maximum sum of the number of neighbours 
!                   per node (should be set to 
!                   3*nt_max + np_max in calling program)
!       points(2,np)        array of node co-ordinates
!           dmode           Delaunay calculation mode (integer)
!           nmode           NN setup mode
!           clockwise       logical for the vertices input order  
!       data(np)        data values at each node
!       nnn         Integer work array used by build_nv
!       nnlist          Integer work array used by build_nv
!       ntrilist        Integer work array used by build_nv
!       nohalt_hull     determines error response in routine
!                   calculate_hulltriangles
!       eps             tolerance parameter used by delaun
!                   (see delaun for details)
!       vis_tlist       Integer work array used by delaun
!       vis_elist       Integer work array used by delaun
!       add_tlist       Integer work array used by delaun
!       nv_max          size of delaun work arrays vis_tlist
!                   vis_elist, & add_tlist (passed to 
!                   delaun for error checking)
!
!   Output:
!           nt          number of triangles
!           vertices(3,nt)      array of triangle vertices 
!               centres(3,nt)           centres(j,i) (j=1,2) contains the
!                                       co-ordinates of the centre of 
!                                       circumcircle about Delaunay 
!                                       triangle i, (i=1,...,nt),
!                                       j=3 contains squared radius of circle.
!           neighbour(3,nt)     array of neighbouring triangles.    
!                   Neighbour(i,j) is the triangle
!                   opposite node i in triangle j,
!                   stored counterclockwise about j.
!           nh          number of triangles with an edge
!                   on the convex hull
!       hulltriangles(nh)   array of triangles with an edge 
!                   on the convex hull
!       loc         an initial guess triangle for point 
!                   location routine `Triloc' used by nn2D
!                   (set somewhere near the centre)
!
!   Operation modes:
!
!        The setup routine will perform different tasks depending
!        on the input parameters dmode and nmode (see table below).
!        Depending on the modes used some work arrays may be set 
!        to size 1 to save memory. The "Memory Savings" column in the
!        table below shows the dimension statement that may
!        be used in the calling program if the routine is ONLY EVER 
!        CALLED IN THE CORRESPONDING MODE. 
!
!        PARAMETERS ACTION          MEMORY SAVINGS
!
!        nmode = 1  Delaunay only       real(8) centres(3,1)
!                           integer hulltriangles(1)
!        nmode = 0  Delaunay + nn setup 
!        nmode = -1 nn setup only       
!
!        dmode > 0  Delaunay read in from   integer vis_tlist(1)
!               logical unit dmode. integer vis_elist(1) 
!                           integer add_tlist(1) 
!        dmode = 0      Qhull used      Same as dmode > 0.
!        dmode = -1 Delaun + X-sort         integer nnn(1)
!                           integer nnlist(1)
!                           integer ntrilist(1)
!        dmode = -2 Delaun + no sort        Same as dmode=-1
!
!        dmode = 0 & nmode=1            integer neighbour(3,1)
!        
!        A call with nmode = -1 can only be made after a call 
!        with nmode = 1.
!
!   Comments:
!
!        If the arrays are used then they should be dimensioned
!        in the calling program in the following way:
!
!        real(8)     points(2,np_max)
!        real(8)     centres(3,nt_max)
!        integer    vertices(3,nt_max)
!        integer    neighbour(3,nt_max)
!        integer    hulltriangles(nh_max)
!        
!        Note: nh_max can usually be dimensioned much less than nt_max
!        because hulltriangles, stores in a compact form, all 
!        triangles with an edge on the convex hull. Except for
!        very irregular point distributions nh << nt. If nh is 
!        determined to be > nh_max then an error is reported and the
!        program is halted (unless nohalt parameter is set to 1). 
!        The array hulltriangles is only used by nn2Do see routine 
!        calculate_hulltriangles. If nh_max = 1 then hulltriangles 
!        is not calculated.
!
!        The initial guess triangle 'loc' is set in nn_setup but
!        at each call it will be set to the triangle found during
!        the previous call to nn2D. The user may modify its value
!        if the input point (x,y) is known to be in, or near, a
!        particular triangle.
!
!        If dmode > 0 the the deluanay tessellation is read in from
!        logical unit `dmode' instead of being calculated internally 
!        This can be useful if qhullf fails because
!        of precision errors. The Deluanay may be determined
!        externally to this program using a double precision version
!        or another algorithm, e.g. Fortune's sweepline method.
!
!        If 50 > dmode > 0 then:
!        It is assumed that the read in format has one triangle per
!        line represented as a triplet of nodes numbered from ZERO, 
!        which is the standard output format of codes qhull 
!        (quickhull method) and voronoi (sweepline method).
!        If clockwise = .true. (.false.) then the vertices are assumed 
!        to be in clockwise (anti-clockwise) order. Note program
!        qhull outputs vertices in anti-clockwise order while 
!        voronoi in clockwise order. The internal format is 
!        anti-clockwise and nodes numbered from ONE.
!
!        If dmode => 50 then:
!        It is assumed that the read in format has one triangle per
!        line represented as a triplet of nodes numbered from ONE,
!        which is the output format of program del (using delaun). 
!
!        Three other work arrays are produced as a `by product'
!        of the routine build_nv which calculates the neighbour
!        array. These must be dimensioned in the calling program in 
!        the following way (unless delaun is used for calculating the
!        Delaunay because it already determines the neighbour array)
!
!        integer nnn(np_max+1)  : number of neighbours per node
!        integer nnlist(nmax)   : natural neighbours per node
!        integer ntrilist(nmax) : triangles attached to each node 
!
!        The value of nmax should be set to (3*nt_max + np_max)
!        in the calling program.
!
!        Each of these are useful lists that describe features of
!        the Voronoi diagram. Both nnlist and ntrilist are stored in
!        a compact format to avoid zeros. They are only used 
!        in the setup routine and the memory may be freed once
!        initialization is completed.
!        
!
!        Calls are made to: qhullf, ccentres, build_nv and 
!                   calculate_hulltriangles, delaun.
!        
!                   M. Sambridge, RSES, April 1994.
!                           (Last modified 10/4/96)
!
!------------------------------------------------------------------------
!

    module m_nn1
        contains

        Subroutine nn2d_setup &
                      (np,nt_max,nh_max,nmax, &
                       points,dmode,nmode,clockwise,nt,vertices, &
                       centres,neighbour,nh,hulltriangles,nohalt_hull, &
                       loc,nnn,nnlist,ntrilist, &
                       eps,nv_max,vis_tlist,vis_elist,add_tlist, &
                    lt_work,ln_work)

            use m_delaun
            use m_nn2

        real(8) ::      points(2,*)
        real(8) ::     centres(3,*)
        real(8) ::     eps
        integer     vertices(3,*)
        integer     neighbour(3,*)
        integer     hulltriangles(*)
        integer     nnn(*)
        integer     nnlist(*)
        integer     ntrilist(*)
        integer         vis_tlist(*)
        integer         vis_elist(*)
        integer         add_tlist(*)
        integer         dmode,nmode
        logical*1   lt_work(*)
        logical*1   ln_work(*)
        logical     nnwrite
        logical     clockwise
        integer(4) :: np,nt,nt_max,nv_max,i1,i2,loc,nmax,nh_max,nh,nohalt_hull,lud
        integer(4) :: i
        logical     ldummy(np)
        integer(4) :: idummy(np)


        common/nnswitches/nnwrite,lud

            if(nmode.eq.1.or.nmode.eq.0)then

               if(dmode.eq.0)then
    !                                       calculate Delaunay using qhull 
     
                  call qhullf()

               else if(dmode.eq.-1.or.dmode.eq.-2)then

    !                   sort the points in ascending x order
              if(dmode.eq.-1)then

    !         write(*,*)' X sort in progress'
                  call hpsort_d(np,1,points)
    !         write(*,*)' X sort done'

              end if

    !                                       calculate Delaunay using delaun 
     
                  call delaun (points,np,neighbour,vertices,nt,nt_max, &
                              vis_tlist,vis_elist,add_tlist,eps,nv_max, &
                              0,ldummy,0,0,idummy)

           else
    !                                       read in Delaunay vertices

                  nt = 0
                  i1 = 1
                  i2 = 2
                  if(clockwise)then
                     i1 = 2
                     i2 = 1
                  end if
                  read(dmode,*)
      1           read(dmode,*,end=3,err=2) &
                 vertices(i1,nt+1),vertices(i2,nt+1),vertices(3,nt+1)
                  nt = nt + 1
                  if(nt.ge.nt_max)then
                     write(*,*) 'Error in nn_setup: too many triangles'
                     write(*,*) 'Remedy: increase size of parameter nt_max'
                     write(*,*) '        in calling program.'
                     stop 
                  end if
                  go to 1
      2           write(*,*) &
                 'Error in nn_setup: read error in Delaunay input file'
                  stop
      3           continue
         
               end if
     
    !                   adjust array vertices to
    !                   range from nodes 1 to np
    !
           if(dmode.ge.0.and.dmode.lt.50)then
              do 5 i = 1,nt
                 vertices(1,i) = vertices(1,i) + 1
                 vertices(2,i) = vertices(2,i) + 1
                 vertices(3,i) = vertices(3,i) + 1
     5            continue
           end if

        end if
    !
    !                   Perform set up for nn interpolation
    !
            if(nmode.eq.0.or.nmode.eq.-1)then

    !                   set initial guess for 
    !                   triangle location procedure
               loc = nt/2

    !                                       Calculate Circumcentres

               call ccentres(points,vertices,nt,centres)

    !                                       Build neighbour matrix
    !                   (if not already built)

               if(dmode.ge.0)then
                  call build_nv &
                 (np,vertices,nt,nmax, &
                  neighbour,nnn,nnlist,ntrilist)
               end if

    !                   calculate hulltriangles

               if(nh_max.gt.1) call calculate_hulltriangles &
              (neighbour,nt,nh_max,hulltriangles,nh,nohalt_hull)

    !                   initialize logical work arrays 
               do i=1,nt
                  lt_work(i) = logical(.false., 1)
               end do
               do i=1,np
                  ln_work(i) = logical(.false., 1)
               end do

        end if

        return
        end subroutine
    !
    !------------------------------------------------------------------------
    !
    !   nn2D - calculates natural neighbour interpolation at point x,y
    !
    !   Input:
    !       x,y         co-ordinates of input point
    !       points(2,np)        array of node points    
    !           vertices(3,nt)      array of triangle vertices  
    !           neighbour(3,nt)     array of neighbouring triangles.    
    !                   Neighbour(i,j) is the triangle
    !                   opposite node i in triangle j,
    !                   stored counterclockwise about j.
    !               centres(3,nt)           centres(j,i) (j=1,2) contains the
    !                                       co-ordinates of the centre of 
    !                                       circumcircle about Delaunay 
    !                                       triangle i, (i=1,...,nt),
    !                                       j=3 contains squared radius of circle.
    !       hulltriangles(nh)   array of triangles with an edge 
    !                   on the convex hull
    !           nh          number of triangles with an edge
    !                   on the convex hull
    !       loc         an initial guess triangle for point 
    !                   location routine `Triloc'
    !       data            data values at each node
    !       nnext           logical if true then extend nn outside
    !                   of convex hull
    !           int_method      determines the type of interpolation
    !                   method. 0= Watson's method
    !                       1= recursive method
    !                       2= recursive method + df(2)
    !                       3= Linear interpolation + df(2)
    !                       4= closed formula method 
    !                       5= same as 4 + df(2)
    !                       6= same as 4 + df(2) + ddf(4)
    !       nnpn_max        maximum number of neighbours per node
    !                   (depends on the point distribution,
    !                                        set to ~50 in calling program)
    !       work...                 work arrays of the size of 
    !                   nnpn_max.
    !       lt_work         logical work array of size nt_max 
    !       ln_work         logical work array of size np_max 
    !
    !   Output:
    !       f           interpolated data value
    !       df(1)           interpolated derivative df/dx
    !       df(2)           interpolated derivative df/dy
    !       v           area of voronoi cell about (x,y)
    !       out         logical determining if (x,y) is i
    !                   inside the convex hull of points
    !
    !   Comments:
    !        The area of the voronoi cell about (x,y) can be used 
    !        as an inverse measure of the quality of the interpolation.
    !        If (x,y) is outside of the hull the voronoi cell is
    !        unbounded and v is the sum of the natural neighbour
    !        co-ordinates calculated with Watson's method.
    !
    !        Note: plot_tc and plot_c are user supplied routines
    !        that can be dummy routines, or used with a plot
    !        program for plotting circum-triangles and circum-circles 
    !        found during the calculation. The file nnplot.f contains
    !        a set of dummy routines to be compiled with nn.f. 
    !        Alternatively all references to plot_tc and plot_c may
    !        commented out.
    !
    !        A recursive method is available, as an alternative to
    !        Watson's method, for calculation of natural neighbour
    !        co-ordinates. The action of nn_setup is the same for 
    !        either method although the parameter nnpn_max is also 
    !        used to size arrays used in the recursive mode.
    !
    !        Calls are made to: Triloc, nn2Dr, nn2Drd, nn2Do or nn2Di
    !                   nn2Df, nn2Dfd, nn2Dfdd.
    !
    !                   M. Sambridge, RSES, April 1994.
    !
    !------------------------------------------------------------------------
    !
        Subroutine nn2D &
           (x,y,points,vertices,neighbour,nnext,int_method,centres, &
            hulltriangles,nh,loc,data,nnpn_max,work_r1,work_r2, &
            work_d1,work_i1,work_i2,work_i3,work_d2,work_d3,work_d4, &
            work_d5,work_d6,work_d7,lt_work,ln_work,out,f,df,ddf,v)

        use m_nn2

        real(8) ::      points(2,*)
        real(8) ::      centres(3,*)
        real(8) ::     xd(2),x,y
        real(8) ::     data(*)
        real(8) ::     f,df(2),ddf(3),v
        integer     vertices(3,*),nnpn_max
        integer     neighbour(3,*)
        integer     hulltriangles(*)
        integer     int_method
        logical     out
        logical     nnext
        logical     nnwrite

        real(8) ::          work_d1(2,nnpn_max)
        real(8) ::         work_d2(2,nnpn_max)
        real(8) ::         work_d3(2,nnpn_max)
        real(8) ::         work_d4(2,nnpn_max)
        real(8) ::         work_d5(2,nnpn_max)
        real(8) ::         work_d6(2,nnpn_max)
        real(8) ::         work_d7(2,nnpn_max)
        real(8) ::           work_r1(nnpn_max,2)
        real(8) ::           work_r2(nnpn_max)
        integer     work_i1(nnpn_max)
        integer         work_i2(2,nnpn_max)
        integer         work_i3(nnpn_max)
        logical*1   lt_work(*)
        logical*1   ln_work(*)
        integer(4) :: loc,lud,nh


        common/nnswitches/nnwrite,lud
     
            xd(1) = x
            xd(2) = y
    !                   Use triangle walking routine
    !                   to locate triangle of input
    !                   point

            call Triloc(x,y,points,vertices,neighbour,loc,out)
    !
    !                   If point is outside convex hull
    !                   call routine nn2Do

        if(out)then


           if(nnext.and.int_method.eq.0)then
                if(nnwrite)write(*,*)' calling nn2Do'
                    call nn2Do &
                        (x,y,points,vertices,neighbour, &
                         centres,hulltriangles,nh,data,f,df,v)
               else
                   f = 0.d0
                   df(1) = 0.d0
                   df(2) = 0.d0
                   v = 0.d0
           end if


        else

    !                   use modified Watson's method 
           if(int_method.eq.0)then

              if(nnwrite)write(*,100)

                  call nn2Di &
                      (x,y,points,vertices,neighbour, &
                       centres,loc,data,f,df,v)

               else if(int_method.eq.1)then

              if(nnwrite)write(*,200)
    !                   use recursive method
                  call nn2Dr &
                      (x,y,points,vertices,neighbour,centres, &
                       loc,data,nnpn_max,work_r1,work_r2,work_d1, &
                       work_i1,work_i2,work_i3,f,df,v)


               else if(int_method.eq.2)then

              if(nnwrite)write(*,300)
    !                   use recursive method and 
    !                   calculate derivatives
                  call nn2Drd &
                      (x,y,points,vertices,neighbour,centres, &
                       loc,data,nnpn_max,work_r1,work_r2,work_d1, &
                       work_i1,work_i2,work_i3,f,df,v)

               else if(int_method.eq.3)then

              if(nnwrite)write(*,400)
    !                   use linear interpolation in triangles
              call nn2DL &
                      (x,y,points,vertices,loc,data,f,df,v)

               else if(int_method.eq.4)then

              if(nnwrite)write(*,500)
    !                   use closed formula method
    !                   no derivatives
    !
                  call nn2Df &
                      (xd,points,vertices,neighbour,centres, &
                       loc,data,nnpn_max,work_d1,work_d2,work_i1, &
                       work_i2,work_r2,work_i3,lt_work,ln_work,f,v)

                  df(1) = 0.d0
                  df(2) = 0.d0

               else if(int_method.eq.5)then

              if(nnwrite)write(*,600)
    !                   use closed formula method
    !                   with 1st derivatives
    !
                  call nn2Dfd &
                      (xd,points,vertices,neighbour,centres, &
                       loc,data,nnpn_max,work_d1,work_d2,work_i1, &
                       work_i2,work_r2,work_i3,work_d3,work_d4, &
                       lt_work,ln_work,f,df,v)

               else if(int_method.eq.6)then

              if(nnwrite)write(*,600)
    !                   use closed formula method
    !                   with 1st and 2nd derivatives
    !
                  call nn2Dfdd &
                      (xd,points,vertices,neighbour,centres, &
                       loc,data,nnpn_max,work_d1,work_d2,work_i1, &
                       work_i2,work_r2,work_i3,work_d3,work_d4, &
                        work_d5,work_d6,work_d7, &
                       lt_work,ln_work,f,df,ddf,v)

               else

                  write(*,*)' '
                  write(*,*) &
                 ' Error in nn2D: interpolation method not defined'
                  write(*,*)'                int_method set to ',int_method
                  write(*,*)' '
                  write(*,*)' Valid options are:'
                  write(*,*)'   0=NN-Watson'
                  write(*,*)'   1=NN-recursive'
                  write(*,*)'   2=NN-recursive+derivatives'
                  write(*,*)'   3=Linear interpolation'
                  write(*,*)'   4=NN-closed formula'
                  write(*,*)'   5=NN-closed formula + df(2)'
                  write(*,*)'   6=NN-closed formula + df(2) + ddf(3)'
                  stop


               end if

        end if

    !   if(nnwrite)then
    !      write(*,*)' Overlap voronoi area = ',v
    !      write(*,*)' Normalized value of f   =',f
    !      write(*,*)' Normalized value of dfx =',df(1)
    !      write(*,*)' Normalized value of dfy =',df(2)
    !   end if

     100    format(' calling nn2Di:   Watson method')
     200    format(' calling nn2Dr:   recursive method')
     300    format(' calling nn2Drd:  recursive method with derivatives')
     400    format(' calling nn2DL:   Linear interpolation')
     500    format(' calling nn2Df:   Closed formulae method')
     600    format(' calling nn2Dfd:   Closed formulae method', &
                  'with first derivatives')

        return
        end subroutine
    !
    !------------------------------------------------------------------------
    !
    !       nn2DL - calculates linear triangle interpolation at point x,y
    !               (This routine is an alternative to nn2D)
    !
    !       Input:
    !               x,y                     co-ordinates of input points
    !               points(2,np)            array of node points
    !               vertices(3,nt)          array of triangle vertices
    !               loc                     an initial guess triangle for point
    !                                       location routine `Triloc'
    !               data                    data values at each node
    !
    !       Output:
    !               f                       interpolated data value
    !               df(1)                   interpolated derivative df/dx
    !               df(2)                   interpolated derivative df/dy
    !               v                       area of voronoi cell about (x,y)
    !
    !       Comments:
    !
    !                Performs simple linear interpolation in triangles
    !
    !                                       M. Sambridge, RSES, April 1994.
    !
    !------------------------------------------------------------------------
    !
            Subroutine nn2DL &
           (x,y,points,vertices,loc,data,f,df,v)

            real(8) ::          points(2,*)
            real(8) ::         x,y
            real(8) ::         x1,y1,x2,y2,x3,y3,A,a1,a2,a3
            real(8) ::         dx12,dx13,dy12,dy13,df12,df13
            real(8) ::         data(*)
            real(8) ::         f,df(2),v
            integer(4) :: vertices(3,*),i,j,k,loc


    !                                       calculate area of triangle loc
            i = vertices(1,loc)
            j = vertices(2,loc)
            k = vertices(3,loc)
            x1 = points(1,i)
            y1 = points(2,i)
            x2 = points(1,j)
            y2 = points(2,j)
            x3 = points(1,k)
            y3 = points(2,k)
            A = ((x2-x1)*(y3-y1) - (x3-x1)*(y2-y1))
            if(A.lt.0.0_8) &
           write(6,*)' Error in nn2DL: negative area for T=',loc

    !                                       calculate f and derivatives
            df12 = data(i) - data(j)
            df13 = data(i) - data(k)
            dx12 = x1 - x2
            dx13 = x1 - x3
            dy12 = y1 - y2
            dy13 = y1 - y3
            a1 = (df12*dy13 - df13*dy12)/A
            a2 = (df13*dx12 - df12*dx13)/A
            a3 = data(i) - a1*x1 - a2*y1
            f = a1*x + a2*y + a3
            df(1) = a1
            df(2) = a2
            v = 0.d0
    !       write(*,*)' New value of f =',f
    !       write(*,*)' New value of dfx =',df(1)
    !       write(*,*)' New value of dfy =',df(2)

            return
            end subroutine
    !
    !
    !------------------------------------------------------------------------
    !
    !   build_nv - Builds neighbour array for Delaunay triangulation in 2-D.
    !
    !   Input:  
    !           vertices(3,nt)      array of triangle vertices  
    !           nt          number of triangles
    !       np_max          maximum number of nodes
    !       nmax            maximum total number of neighbours 
    !                   per node (should be set to 
    !                   3*nt_max + np_max in calling program)
    !
    !   Output:
    !       neighbour(3,nt)     array of neighbouring triangles
    !
    !   Comments:
    !        Assumes input list of vertices in anticlockwise sequence
    !        and produces an anticlockwise list of neighbour triangles.
    !        The value of neighbour(i,j) is the index of the neighbouring
    !        triangle opposite node i in triangle j.
    !
    !        Three temporary work arrays are used and must be dimensioned
    !        in the calling program in the following way:
    !
    !        integer nnn(np_max+1)  : number of neighbours per node
    !        integer nnlist(nmax)   : natural neighbours per node
    !        integer ntrilist(nmax) : triangles attached to node 
    !
    !        The value of nmax should be set to (3*nt_max + np_max)
    !        in the calling program.
    !
    !        No calls to other routines.
    !
    !                   M. Sambridge, RSES, April 1994.
    !                   (using ideas by J.Braun)
    !
    !------------------------------------------------------------------------
    !
        Subroutine build_nv &
                      (np,vertices,nt,nmax, &
                       neighbour,nnn,nnlist,ntrilist)
    !
        integer     vertices(3,*)
        integer     neighbour(3,*)
        integer     nnn(*)
        integer     nnlist(*)
        integer(4) ::    ntrilist(*),i,j,nt,nmax,np,it,i1,i2,i3,itemp,itemp2
        integer(4) :: j1,j2,jt,k1,k2,k3,lud
        logical     nnwrite

        common/nnswitches/nnwrite,lud

        if(nnwrite)write(*,*)' Building neighbour v ...'
    !
    !                   initialize neighbour list
        do 5 i = 1,3
           do 5 j = 1,nt
              neighbour(i,j) = 0
     5      continue
    !                   initialize work arrays
            do 6 i = 1,nmax
           nnlist(i) = 0
           ntrilist(i) = 0
     6      continue

            do 7 i = 1,np
           nnn(i) = 0
     7      continue

        do 10 it = 1,nt
           i1 = vertices(1,it)
           i2 = vertices(2,it)
           i3 = vertices(3,it)
           nnn(i1) = nnn(i1) + 1
           nnn(i2) = nnn(i2) + 1
           nnn(i3) = nnn(i3) + 1
     10     continue

    !                   turn nnn into a running sum
        itemp = nnn(1)+1
        nnn(1) = 1
        do 20 j = 2,np+1
           itemp2  = itemp 
           itemp   = itemp + nnn(j)+1
           nnn(j) = itemp2 + 1
     20     continue
    !       write(*,*)' size of array =',nnn(np+1)-1
    !       write(*,*)' 3nt+np        =',3*nt+np

        if(nnn(np+1).ge.nmax)then
               write(*,*)'Error: array sizes too small in subroutine ' &
                        ,'build_nv'
               write(*,*)'       maximum number of neighbours for all nodes'
               write(*,*)'       is too small: current value =',nmax
               write(*,*)'       Increase size of parameter nmax'
               write(*,*)'       to at least',nnn(np+1)
               write(*,*)'       This will be satisfied if nmax is set'
               write(*,*)'       to 3*nt_max+np_max in calling program' 
           stop
        end if

        do 25 it = 1,nt
           i1 = vertices(1,it) 
           i2 = vertices(2,it) 
           i3 = vertices(3,it) 
    !                       compare neighbours i1 i2
    !                       (remove go to ?)
           j1 = nnn(i1)
           j2 = nnn(i1+1) - 1 
           jt = 0
           do 30 j = j1,j2
              if(nnlist(j).eq.0)then
                 nnlist(j) = i2
                 ntrilist(j) = it
    !                       if we have recorded connection
    !                       then jump out of loop
                 go to 31
              else if(nnlist(j).eq.i2.and.ntrilist(j).ne.it)then
                     jt = ntrilist(j)
                 go to 31
              end if
      30       continue
      31       continue
    !                       if neighbours are found then
    !                       skip second loop 
           if(jt.eq.0)then
              j1 = nnn(i2)
              j2 = nnn(i2+1) - 1 
              do 32 j = j1,j2
                 if(nnlist(j).eq.0)then
                    nnlist(j) = i1
                    ntrilist(j) = it
    !                       if we have inserted connection
    !                       then jump out of loop
                    go to 33
                 end if
      32          continue
           end if
      33       continue

           if(jt.ne.0)then
    !                       found neighbours it,jt with
    !                       common nodes i1 and i2
              neighbour(3,it) = jt
              k1 = vertices(1,jt)
              k2 = vertices(2,jt)
              k3 = vertices(3,jt)
              if(k1.ne.i1.and.k1.ne.i2)then 
                 neighbour(1,jt) = it
              else if(k2.ne.i1.and.k2.ne.i2)then 
                 neighbour(2,jt) = it
              else
                 neighbour(3,jt) = it
              end if
               end if
    !                       compare neighbours i1 i3
           jt = 0
           j1 = nnn(i1)
           j2 = nnn(i1+1) - 1 
           do 130 j = j1,j2
              if(nnlist(j).eq.0)then
                 nnlist(j) = i3
                 ntrilist(j) = it
    !                       if we have recorded connection
    !                       then jump out of loop
                 go to 131
              else if(nnlist(j).eq.i3.and.ntrilist(j).ne.it)then
                     jt = ntrilist(j)
                 go to 131
              end if
      130      continue
      131      continue
    !                       if neighbours are found then
    !                       skip second loop 
           if(jt.eq.0)then
              j1 = nnn(i3)
              j2 = nnn(i3+1) - 1 
              do 132 j = j1,j2
                 if(nnlist(j).eq.0)then
                    nnlist(j) = i1
                    ntrilist(j) = it
    !                       if we have inserted connection
    !                       then jump out of loop
                    go to 133
                 end if
      132         continue
              end if
      133      continue
           if(jt.ne.0)then
    !                       found neighbours it,jt with
    !                       common nodes i1 and i3
             neighbour(2,it) = jt
             k1 = vertices(1,jt) 
             k2 = vertices(2,jt)
             k3 = vertices(3,jt)
             if(k1.ne.i1.and.k1.ne.i3)then 
                neighbour(1,jt) = it
             else if(k2.ne.i1.and.k2.ne.i3)then 
                neighbour(2,jt) = it
             else
                neighbour(3,jt) = it
             end if
               end if
    !                       compare neighbours i2 i3
           jt = 0
           j1 = nnn(i2)
           j2 = nnn(i2+1) - 1 
           do 230 j = j1,j2
              if(nnlist(j).eq.0)then
                 nnlist(j) = i3
                 ntrilist(j) = it
    !                       if we have recorded connection
    !                       then jump out of loop
                 go to 231
              else if(nnlist(j).eq.i3.and.ntrilist(j).ne.it)then
                     jt = ntrilist(j)
                 go to 231
              end if
      230      continue
      231      continue
    !                       if neighbours are found then
    !                       skip second loop 
           if(jt.eq.0)then
              j1 = nnn(i3)
              j2 = nnn(i3+1) - 1 
              do 232 j = j1,j2
                 if(nnlist(j).eq.0)then
                    nnlist(j) = i2
                    ntrilist(j) = it
    !                       if we have inserted connection
    !                       then jump out of loop
                    go to 233
                 end if
      232         continue
              end if
      233      continue
           if(jt.ne.0)then
    !                       found neighbours it,jt with
    !                       common nodes i2 and i3
             neighbour(1,it) = jt
             k1 = vertices(1,jt) 
             k2 = vertices(2,jt)
             k3 = vertices(3,jt)
             if(k1.ne.i2.and.k1.ne.i3)then 
                neighbour(1,jt) = it
             else if(k2.ne.i2.and.k2.ne.i3)then 
                neighbour(2,jt) = it
             else
                neighbour(3,jt) = it
             end if
               end if

     25     continue

        if(nnwrite)write(*,*)' built neighbour v'

        return
        end subroutine
    !
    !------------------------------------------------------------------------
    !
    !       Calculate_hulltriangles - finds all triangles with a face on
    !                                 the convex hull by searching through
    !                                 the entries in the array neighbour.
    !
    !       Input:
    !               neighbour(3,nt)         array of neighbouring tetrahedra
    !               nt                      number of tetrahedra
    !           nh_max          maximum number of triangles on convex 
    !                   hull (max size of array hulltriangles)
    !       nohalt          determines error response
    !       Output:
    !       hulltriangles(nh)   array of triangles with an edge
    !                   on the convex hull
    !               nh                      number of tetrahedra with an edge
    !                                       on the convex hull
    !       Comments:
    !
    !                This routine fills up the array hulltriangles which
    !                is only used by routine nn2Do, i.e the `pseudo-extension' 
    !        Watson's nn-interpolation method to points outside of the 
    !        convex hull. If nnext is set to false then hulltriangles
    !        is never used and the array can be set to size 1.
    !
    !        If nohalt = 0 then the routine will stop with an error
    !        message if nh > nh_max. If nohalt .ne. 0 and nh > nh_max
    !        then it will return nh = -1. 
    !
    !                No calls to other routines.
    !
    !                                       M. Sambridge, RSES, May 1995.
    !
    !------------------------------------------------------------------------
    !
        Subroutine calculate_hulltriangles &
                      (neighbour,nt,nh_max,hulltriangles,nh,nohalt)
    !
        integer     neighbour(3,*)
        integer     hulltriangles(*)
        integer(4) :: nh,j,nt,nh_max,nohalt

    !                                               store list of triangles
    !                                               which have an edge on the
    !                                               convex hull.
    !                                               (used by routine nn2D)
            nh = 1
            do 100 j = 1,nt
               if(neighbour(1,j).eq.0.or. &
                 neighbour(2,j).eq.0.or. &
                 neighbour(3,j).eq.0)then
                  hulltriangles(nh) = j
                  nh = nh + 1
                  if(nh.gt.nh_max.and.nohalt.eq.0)then
                      write(*,*)' Error array storing outward facing '
                      write(*,*)' triangles on convex hull is too small.'
                      write(*,*)' Increase size of parameter nh_max'
                      stop
                  else if(nh.gt.nh_max.and.nohalt.ne.0)then
                      nh = -1
                      return
                  end if
               end if
     100    continue
            nh = nh -1
     
        return
        end subroutine
    !
    !------------------------------------------------------------------------
    !
    !   Triloc - locates the triangle containing point x,y
    !
    !   Input:
    !       x,y         co-ordinates of input points    
    !       points(2,np)        array of node points    
    !           vertices(3,nt)      array of triangle vertices  
    !           neighbour(3,nt)     array of neighbouring triangles.    
    !                   Neighbour(i,j) is the triangle
    !                   opposite node i in triangle j,
    !                   stored counterclockwise about j.
    !           loc         first guess of triangle containing
    !                   (x, y).
    !
    !   Output:
    !           loc         index of triangle containing 
    !                   input point.
    !           out         =true if (x,y) is outside of
    !                   the convex hull, otherwise = false. 
    !
    !   Comments:
    !        If (x,y) is outside convex hull loc is a `nearby' triangle
    !        on the hull and out is set to true.
    !
    !        No calls to other routines.
    !
    !                   M. Sambridge, RSES, April 1994.
    !
    !------------------------------------------------------------------------
    !
        Subroutine Triloc(x,y,points,vertices,neighbour,loc,out)
    !
        real(8) ::      points(2,*)
        integer     vertices(3,*)
        integer     neighbour(3,*)
        integer     p1,p2
        logical     out
        real(8) ::      x,y,del1,del2
        integer     c1(3)
        integer(4) :: i,j,k,loc
        data        c1/2,3,1/
    !
        out = .false.

     10     continue
    !                   point is outside convex hull
            if(out)return

            do 20 i=1,3
           j = c1(i)
           k = c1(j)
               p1 = vertices(i,loc)
               p2 = vertices(j,loc)
           del1 = (points(2,p1)-y)*(points(1,p2)-x)
           del2 = (points(1,p1)-x)*(points(2,p2)-y)
           if(del1.gt.del2)then
              if(neighbour(k,loc).eq.0)then
                     out = .true.
              else
                 loc = neighbour(k,loc)
              end if
              go to 10
           end if
     20     continue
        
        return
        end subroutine
    !
    !------------------------------------------------------------------------
    !
    !   Ccentres - calculates centres of all Delaunay circumcircles
    !
    !
    !   Input:
    !       points(2,np)        array of node points    
    !           vertices(3,nt)      array of triangle vertices  
    !           nt          number of triangles
    !
    !   Output:
    !               centres(3,nt)           centres(j,i) (j=1,2) contains the
    !                                       co-ordinates of the centre of 
    !                                       circumcircle about Delaunay 
    !                                       triangle i, (i=1,...,nt),
    !                                       j=3 contains squared radius of circle.
    !
    !   Comments:
    !
    !        No calls to other routines.
    !
    !                   M. Sambridge, RSES, April 1994.
    !
    !------------------------------------------------------------------------
    !
        Subroutine ccentres(points,vertices,nt,centres)
    !
        real(8) ::      points(2,*)
        real(8) ::     centres(3,*)
        real(8) ::     x1,x2,x3,y1,y2,y3,x,y
        real(8) ::     dx2m1,dx2p1,dy2m1,dy2p1
        real(8) ::     dx3m1,dx3p1,dy3m1,dy3p1
        real(8) ::     denom
        integer     vertices(3,*)
        integer(4) :: i,nt
    !                       Find centres of all
    !                       Delaunay Circumcircles
        do 5 i= 1,nt

           x1 = points(1,vertices(1,i))
           x2 = points(1,vertices(2,i))
           x3 = points(1,vertices(3,i))
           y1 = points(2,vertices(1,i))
           y2 = points(2,vertices(2,i))
           y3 = points(2,vertices(3,i))

               dx2m1 = x2-x1
               dx2p1 = x2+x1
               dy2m1 = y2-y1
               dy2p1 = y2+y1
               dx3m1 = x3-x1
               dx3p1 = x3+x1
               dy3m1 = y3-y1
               dy3p1 = y3+y1
               denom = dx2m1*dy3m1-dx3m1*dy2m1
           x = ((dx2m1*dx2p1 + dy2m1*dy2p1)*dy3m1*0.5d0 &
                  -(dx3m1*dx3p1 + dy3m1*dy3p1)*0.5d0*dy2m1)/ &
                  (denom)

           y = (dx2m1*(dx3m1*dx3p1 + dy3m1*dy3p1)*0.5d0  &
                  -dx3m1*(dx2m1*dx2p1 + dy2m1*dy2p1)*0.5d0)/  &
                  (denom)

           centres(1,i) = x
           centres(2,i) = y
               x1 = x - x1
               y1 = y - y1
           centres(3,i) = x1*x1 + y1*y1

     5  continue

        return
        end subroutine
    !
    !------------------------------------------------------------------------
    !
    !   nn2Di - calculates natural neighbour interpolation at point x,y
    !       when point is inside convex hull 
    !
    !   Input:
    !       x,y         co-ordinates of input point
    !       points(2,np)        array of node points    
    !           vertices(3,nt)      array of triangle vertices  
    !           neighbour(3,nt)     array of neighbouring triangles.    
    !                   Neighbour(i,j) is the triangle
    !                   opposite node i in triangle j,
    !                   stored counterclockwise about j.
    !               centres(3,nt)           centres(j,i) (j=1,2) contains the
    !                                       co-ordinates of the centre of 
    !                                       circumcircle about Delaunay 
    !                                       triangle i, (i=1,...,nt),
    !                                       j=3 contains squared radius of circle.
    !           loc         index of triangle containing 
    !                   input point.
    !       data            data values at each node
    !
    !   Output:
    !       f           interpolated data value
    !       df(1)           interpolated derivative df/dx
    !       df(2)           interpolated derivative df/dy
    !       v           area of voronoi cell about (x,y)
    !
    !   Comments:
    !        On input point (x,y) must be in triangle loc. 
    !        Note: The input triangle contains (x,y) and so it's 
    !        circumcircle must also contain (x,y).
    !
    !        See comments on plot routine plot_tc above.
    !
    !        This routine uses Sloan's LIFO stack technique to 
    !        avoid an expensive global search over all triangles.
    !        (see Sambridge, Braun and McQueen, 1995; 
    !         Geophys. J. Int., vol 122, 837-857).
    !
    !        Calls are made to: nn_tri. plot_tc, stackpairinit,
    !                   poppair,pushpair & stackpairempty. 
    !
    !                   M. Sambridge, RSES, April 1994.
    !
    !------------------------------------------------------------------------
    !
        Subroutine nn2Di &
                      (x,y,points,vertices,neighbour, &
                       centres,loc,data,f,df,v)

        real(8)      points(2,*)
        real(8)      centres(3,*)
        real(8)      data(*)
        real(8)      x,y,dist,dx,dy
        real(8)      f,df(2),v,dvx,dvy
        integer     vertices(3,*)
        integer     neighbour(3,*)
        integer     tri,pos
        logical     nnwrite
        integer(4) :: lud,loc,i,j,k,new1,new2

        common/nnswitches/nnwrite,lud

        if(nnwrite)write(lud,*)' Result of nn2Di search:'

    !                   Initialize variables
        f = 0.d0
        df(1) = 0.d0
        df(2) = 0.d0
        v = 0.d0
        dvx = 0.d0
        dvy = 0.d0
    !
    !                   Calculate natural neighbour
    !                   contribution from input triangle.
    !
        call nn_tri(x,y,points,vertices,centres,loc, &
                       data,f,df,v,dvx,dvy)

        if(nnwrite)write(lud,*)loc
    !
    !                   Find all other circumcircles 
    !                   containing input point and update
    !                   natural neighbour contribution
    !                   from these triangles
    !
    !                   plot circum-triangle and circum-circle

        call plot_tc()

           
    !                   initialize stack
        call stackpairinit

    !                   put input triangle's neighbouring 
    !                   triangles on LIFO stack
    !                   together with position of input
    !                   triangle in their neighbour list

        i = neighbour(1,loc)
        j = neighbour(2,loc)
        k = neighbour(3,loc)
        if(i.ne.0)then
           if(neighbour(1,i).eq.loc)call pushpair(i,1)
           if(neighbour(2,i).eq.loc)call pushpair(i,2)
           if(neighbour(3,i).eq.loc)call pushpair(i,3)
        end if
        if(j.ne.0)then
           if(neighbour(1,j).eq.loc)call pushpair(j,1)
           if(neighbour(2,j).eq.loc)call pushpair(j,2)
           if(neighbour(3,j).eq.loc)call pushpair(j,3)
        end if
        if(k.ne.0)then
           if(neighbour(1,k).eq.loc)call pushpair(k,1)
           if(neighbour(2,k).eq.loc)call pushpair(k,2)
           if(neighbour(3,k).eq.loc)call pushpair(k,3)
        end if

     10 call stackpairempty(k)
    !                   if stack empty then finish
        if(k.eq.1)go to 100
    !                   take triangle from stack
        call poppair(tri,pos)

    !                   test if (x,y) is in circumcircle

             
            dx = x-centres(1,tri)
            dy = y-centres(2,tri)
        dist = dx*dx + dy*dy 

        if(dist.lt.centres(3,tri))then
               if(nnwrite)write(lud,*)tri
    !                   Calculate natural neighbour
    !                   contribution from current triangle.
    !
           call nn_tri(x,y,points,vertices,centres,tri &
                          ,data,f,df,v,dvx,dvy)

    !                   plot triangle and circumcircle

           call plot_tc()
           if(pos.eq.1)then
              new1 = neighbour(2,tri)
              new2 = neighbour(3,tri)
           end if
           if(pos.eq.2)then
              new1 = neighbour(1,tri)
              new2 = neighbour(3,tri)
           end if
           if(pos.eq.3)then
              new1 = neighbour(1,tri)
              new2 = neighbour(2,tri)
           end if
           if(new1.ne.0)then
              if(neighbour(1,new1).eq.tri)call pushpair(new1,1)
              if(neighbour(2,new1).eq.tri)call pushpair(new1,2)
              if(neighbour(3,new1).eq.tri)call pushpair(new1,3)
           end if
           if(new2.ne.0)then
              if(neighbour(1,new2).eq.tri)call pushpair(new2,1)
              if(neighbour(2,new2).eq.tri)call pushpair(new2,2)
              if(neighbour(3,new2).eq.tri)call pushpair(new2,3)
           end if
        end if
        go to 10
     
     100    continue
            call stackpairflush()
        if(v.ne.0.0_8)then
           f = f/v
           df(1) = (df(1) - f*dvx)/v
           df(2) = (df(2) - f*dvy)/v
    !      if(nnwrite)then
    !      write(*,*)' Overlap voronoi area = ',v
    !      write(*,*)' Normalized value of f   =',f
    !      write(*,*)' Normalized value of dfx =',df(1)
    !      write(*,*)' Normalized value of dfy =',df(2)
    !      end if
    !   else
    !      if(nnwrite)then
    !      write(*,*)' Subroutine nn2Di: overlap voronoi area = 0'
    !      write(*,*)' Unnormalized value of f   =',f
    !      write(*,*)' Current value of dfx      =',df(1)
    !      write(*,*)' Current value of dfy      =',df(2)
    !      end if
        end  if

        return
        end subroutine
    !
    !------------------------------------------------------------------------
    !
    !   nn2Do - calculates natural neighbour interpolation at point x,y
    !       when point is outside convex hull 
    !
    !   Input:
    !       x,y         co-ordinates of input point
    !       points(2,np)        array of node points    
    !           vertices(3,nt)      array of triangle vertices  
    !           neighbour(3,nt)     array of neighbouring triangles.    
    !                   Neighbour(i,j) is the triangle
    !                   opposite node i in triangle j,
    !                   stored counterclockwise about j.
    !               centres(3,nt)           centres(j,i) (j=1,2) contains the
    !                                       co-ordinates of the centre of 
    !                                       circumcircle about Delaunay 
    !                                       triangle i, (i=1,...,nt),
    !                                       j=3 contains squared radius of circle.
    !       hulltriangles(nh)   array of triangles with an edge
    !                   on the convex hull
    !           nh          number of triangles with an edge
    !                   on the convex hull
    !       data            data values at each node
    !
    !   Output:
    !       f           interpolated data value
    !       df(1)           interpolated derivative df/dx
    !       df(2)           interpolated derivative df/dy
    !       v           area of voronoi cell about (x,y)
    !
    !   Comments:
    !       The following procedure works on the principle that if (x,y) is
    !       inside the union of circumcircles then it must be in one 
    !       of the circumcircles whose Delaunay triangle has an edge 
    !       on the convex hull. It then proceeds to find all circumcircles
    !       containing the point by repeating the triangle walking
    !       algorithm described by Sloan (1987, and attributed to 
    !       D. Watson) starting from each triangle on the hull whose
    !       circumcircles contain the point. Note the repeated use of
    !       the procedure is necessary because when (x,y) is outside of 
    !       the hull the required triangles may not form a contiguous set.
    !
    !       See comments on plot routine plot_tc above.
    !
    !       Calls are made to: nn_tri. plot_tc, stackpairinit,
    !                   poppair,pushpair & stackpairempty. 
    !
    !                   M. Sambridge, RSES, April 1994.
    !
    !------------------------------------------------------------------------
    !
        Subroutine nn2Do &
           (x,y,points,vertices,neighbour,centres, &
            hulltriangles,nh,data,f,df,v)

        real(8)      points(2,*)
        real(8)      centres(3,*)
        real(8)      x,y,xc,yc,dist
        real(8)      data(*)
        real(8)      f,df(2),v,dvx,dvy
        integer     vertices(3,*)
        integer     neighbour(3,*)
        integer     tri,pos
        integer     hulltriangles(*)
        logical     inside_union
        logical     nnwrite
        integer(4) :: lud,list,nh,loc,i,j,k,new1,new2

        common/nnswitches/nnwrite,lud

        if(nnwrite)write(lud,*)' Result of nn2Do search:'
            inside_union = .false.

    !                   Initialize variables
        f = 0.d0
        df(1) = 0.d0
        df(2) = 0.d0
        v = 0.d0
        dvx = 0.d0
        dvy = 0.d0
    !                   Find all circumcircles containing
    !                   input point

    !                   test all circumcircles whose
    !                   Delaunay triangles have an edge
    !                   on the convex hull
        do 5 list = 1,nh
               tri = hulltriangles(list)
           xc = centres(1,tri)
           yc = centres(2,tri)
           dist = (xc-x)*(xc-x) + (yc-y)*(yc-y)
           if(dist.lt.centres(3,tri))then
              if(nnwrite)write(lud,*)' circle test found ',tri
              call plot_tc()
              inside_union = .True.
              loc = tri


              if(nnwrite)write(lud,*)loc
    !                   Calculate natural neighbour
    !                   contribution from current triangle.

              call nn_tri(x,y,points,vertices,centres,loc, &
                             data,f,df,v,dvx,dvy)


    !                   plot triangle and circumcircle

              call plot_tc()
           
    !                   initialize stack
              call stackpairinit
    !                   put input triangle's neighbouring 
    !                   triangles on LIFO stack
    !                   together with position of input
    !                   triangle in their neighbour list
              i = neighbour(1,loc)
              j = neighbour(2,loc)
              k = neighbour(3,loc)
              if(i.ne.0)then
                 if(neighbour(1,i).eq.loc)call pushpair(i,1)
                 if(neighbour(2,i).eq.loc)call pushpair(i,2)
                 if(neighbour(3,i).eq.loc)call pushpair(i,3)
              end if
              if(j.ne.0)then
                 if(neighbour(1,j).eq.loc)call pushpair(j,1)
                 if(neighbour(2,j).eq.loc)call pushpair(j,2)
                 if(neighbour(3,j).eq.loc)call pushpair(j,3)
              end if
              if(k.ne.0)then
                 if(neighbour(1,k).eq.loc)call pushpair(k,1)
                 if(neighbour(2,k).eq.loc)call pushpair(k,2)
                 if(neighbour(3,k).eq.loc)call pushpair(k,3)
              end if
             
     10       call stackpairempty(k)
    !                   if stack empty then finish
              if(k.eq.1)go to 100
    !                   take triangle from stack
              call poppair(tri,pos)

    !                   test if (x,y) is in circumcircle

              dist = (x-centres(1,tri))*(x-centres(1,tri)) +  &
                        (y-centres(2,tri))*(y-centres(2,tri))
              if(dist.lt.centres(3,tri))then
                     if(nnwrite)write(lud,*)tri
    !                   Calculate natural neighbour
    !                   contribution from current triangle.

                 call nn_tri(x,y,points,vertices,centres,tri, &
                                data,f,df,v,dvx,dvy)

    !                   plot triangle and circumcircle
             
                 call plot_tc()
                 if(pos.eq.1)then
                    new1 = neighbour(2,tri)
                    new2 = neighbour(3,tri)
                 end if
                 if(pos.eq.2)then
                    new1 = neighbour(1,tri)
                    new2 = neighbour(3,tri)
                 end if
                 if(pos.eq.3)then
                    new1 = neighbour(1,tri)
                    new2 = neighbour(2,tri)
                 end if
                 if(new1.ne.0)then
                    if(neighbour(1,new1).eq.tri)call pushpair(new1,1)
                    if(neighbour(2,new1).eq.tri)call pushpair(new1,2)
                    if(neighbour(3,new1).eq.tri)call pushpair(new1,3)
                 end if
                 if(new2.ne.0)then
                    if(neighbour(1,new2).eq.tri)call pushpair(new2,1)
                    if(neighbour(2,new2).eq.tri)call pushpair(new2,2)
                    if(neighbour(3,new2).eq.tri)call pushpair(new2,3)
                 end if
              end if
              go to 10
     100          continue
                  call stackpairflush()
           end if
     5      continue
        if(.not.inside_union.and.nnwrite) &
              write(lud,*)' point is outside all circumcircles'
        if(.not.inside_union.and.nnwrite) &
              write(*,*)' point is outside all circumcircles'

        if(v.ne.0.0_8)then
           f = f/v
           df(1) = (df(1) - f*dvx)/v
           df(2) = (df(2) - f*dvy)/v
        end  if

        return
        end subroutine
    !
    !------------------------------------------------------------------------
    !
    !   nn_tri - calculates natural neighbour contribution from 
    !        triangle tri at point x,y
    !
    !   Input:
    !       x,y         co-ordinates of input point
    !       points(2,np)        array of node points    
    !           vertices(3,nt)      array of triangle vertices  
    !               centres(3,nt)           centres(j,i) (j=1,2) contains the
    !                                       co-ordinates of the centre of 
    !                                       circumcircle about Delaunay 
    !                                       triangle i, (i=1,...,nt),
    !                                       j=3 contains squared radius of circle.
    !       tri         index of current triangle
    !   Output:
    !       f           partial f interpolation
    !       df(2)           partial derivative interpolation
    !       dvx         partial sum of derivatives
    !       dvy         partial sum of derivatives
    !       v           partial area of voronoi cell about (x,y)
    !
    !   Comments:
    !        Note this routine only adds on the contribution to
    !        f and derivatives to sum from last call. All summation
    !        variables are initialized in the calling routine. 
    !        Nodes are assumed to be in counterclockwise direction.
    !
    !        The terms dvx, dvy are needed in the calling routine to 
    !        complete the calculation of f and df. 
    !
    !        See comments on plot routine plot_c above.
    !
    !        Calls are made to: plot_c.
    !
    !                   M. Sambridge, RSES, April 1994.
    !
    !------------------------------------------------------------------------
    !
        Subroutine nn_tri &
                      (x1,x2,points,vertices,centres,tri, &
                       data,f,df,v,dvx,dvy)

        real(8)      points(2,*)
        real(8)      centres(3,*)
        real(8)      x1,x2,f,df(2),v
        real(8)      dvx,dvy,a,b,dxw,dyw,det
        real(8)      p(2,3),c(2,3),f1(2,3),f2(2,3),cx,cy
        real(8)      dcx(2,3),dcy(2,3)
        real(8)      data(*)
        integer     vertices(3,*)
        integer     tri
        logical     nnwrite
        integer     c1(3)
        integer(4) :: i,j,k,node,lud
        real(8) :: xs,ys
        data        c1/2,3,1/

        common/nnswitches/nnwrite,lud



    !                   set position vectors of nodes
        do 5 i=1,2
           do 6 j=1,3
              p(i,j) = points(i,vertices(j,tri))
     6         continue
     5      continue
    !                   set circumcentre nodes
        cx = centres(1,tri)
        cy = centres(2,tri)
        do 10 i = 1,3
               j = c1(i)
               k = c1(j)

    !                   calculate centres of three 
    !                   new triangles

               c(1,i) = (((p(1,k)-p(1,j))*(p(1,k)+p(1,j))/2.0_8  &
                        +(p(2,k)-p(2,j))*(p(2,k)+p(2,j))/2.0_8) &
                        *(x2-p(2,j)) &
                      -(((x1-p(1,j))*(x1+p(1,j))/2.0_8  &
                        +(x2-p(2,j))*(x2+p(2,j))/2.0_8) &
                        *(p(2,k)-p(2,j))))/ &
                        ((p(1,k)-p(1,j))*(x2-p(2,j)) &
                        -(x1-p(1,j))*(p(2,k)-p(2,j)))
     
               c(2,i) = ((p(1,k)-p(1,j))*((x1-p(1,j)) &
                       *(x1+p(1,j))/2.0_8  &
                       +(x2-p(2,j))*(x2+p(2,j))/2.0_8) &
                      -((x1-p(1,j))*((p(1,k)-p(1,j)) &
                       *(p(1,k)+p(1,j))/2.0_8  &
                       +(p(2,k)-p(2,j))*(p(2,k)+p(2,j))/2.0_8)))/ &
                       ((p(1,k)-p(1,j))*(x2-p(2,j)) &
                       -(x1-p(1,j))*(p(2,k)-p(2,j)))
     
               xs = c(1,i)
               ys = c(2,i)
               call plot_c()

           f1(1,i) = c(1,i) - x1
           f1(2,i) = f1(1,i) 
           f2(1,i) = c(2,i) - x2
           f2(2,i) = f2(1,i) 

           dcx(1,i) = ((p(2,k)-x2)*f1(1,i)-(p(2,j)-x2)*f1(2,i))/ &
                        ((p(1,j)-x1)*(p(2,k)-x2)-(p(2,j)-x2)*(p(1,k)-x1))
           dcx(2,i) = ((p(1,j)-x1)*f1(2,i)-(p(1,k)-x1)*f1(1,i))/ &
                        ((p(1,j)-x1)*(p(2,k)-x2)-(p(2,j)-x2)*(p(1,k)-x1))
           dcy(1,i) = ((p(2,k)-x2)*f2(1,i)-(p(2,j)-x2)*f2(2,i))/ &
                        ((p(1,j)-x1)*(p(2,k)-x2)-(p(2,j)-x2)*(p(1,k)-x1))
           dcy(2,i) = ((p(1,j)-x1)*f2(2,i)-(p(1,k)-x1)*f2(1,i))/ &
                        ((p(1,j)-x1)*(p(2,k)-x2)-(p(2,j)-x2)*(p(1,k)-x1))
     10 continue

        do 20 i = 1,3
               j = c1(i)
               k = c1(j)
           node = vertices(i,tri)
           det  = ((c(1,j)-cx)*(c(2,k)-cy) &
                     -(c(1,k)-cx)*(c(2,j)-cy))/2.d0
           v = v + det
           f = f + det*data(node)
           a = (dcx(1,j)*(c(2,k)-cy)-(c(1,k)-cx)*dcx(2,j))
           b = ((c(1,j)-cx)*dcx(2,k)-dcx(1,k)*(c(2,j)-cy))
           dxw = (a + b)/2.d0
           a = (dcy(1,j)*(c(2,k)-cy)-(c(1,k)-cx)*dcy(2,j))
           b = ((c(1,j)-cx)*dcy(2,k)-dcy(1,k)*(c(2,j)-cy))
           dyw = (a + b)/2.d0
           df(1) = df(1) + dxw*data(node)
           df(2) = df(2) + dyw*data(node)
           dvx =  dvx  + dxw
           dvy =  dvy  + dyw
     20 continue

    !   if(nnwrite)write(*,*)'x',x1,' y',x2,' f ',f,' t ',tri
    !   if(nnwrite)write(*,*)' df1',df(1),' df2 ',df(2),' v',v

        return
        end subroutine
    !
    !------------------------------------------------------------------------
    !
    !   nn2Dr - calculates natural neighbour interpolation at point x,y
    !       when point is inside convex hull 
    !       (using method based on recursive formula)
    !
    !   Input:
    !       x,y         co-ordinates of input point
    !       points(2,np)        array of node points    
    !           vertices(3,nt)      array of triangle vertices  
    !           neighbour(3,nt)     array of neighbouring triangles.    
    !                   Neighbour(i,j) is the triangle
    !                   opposite node i in triangle j,
    !                   stored counterclockwise about j.
    !               centres(3,nt)           centres(j,i) (j=1,2) contains the
    !                                       co-ordinates of the centre of 
    !                                       circumcircle about Delaunay 
    !                                       triangle i, (i=1,...,nt),
    !                                       j=3 contains squared radius of circle.
    !           loc         index of triangle containing 
    !                   input point.
    !       data            data values at each node
    !       nnpn_max        maximum number of neighbours per node
    !                   (depends on the point distribution,
    !                                        set to ~50 in calling program)
    !
    !   Output:
    !       f           interpolated data value
    !       df(1)           interpolated derivative df/dx
    !       df(2)           interpolated derivative df/dy
    !       v           area of voronoi cell about (x,y)
    !
    !   Comments:
    !        On input point (x,y) must be in triangle loc. 
    !        Note: The input triangle contains (x,y) and so it's 
    !        circumcircle must also contain (x,y).
    !
    !        In order to use the recursive Lasserre formula to
    !        calculate the nn co-ordinate of the input point with
    !        respect to its ith neighbour we must find the set of
    !        nodes that are both neighbours of i and (x,y), where 
    !        the neighbours of i are determined before addition of 
    !        (x,y). This is done using an approach similar to Watson's
    !        algorithm for updating a Delaunay triangulation, although
    !        we avoid the complication of having to remove `double
    !        entires in the list' and also use Sloan's LIFO stack
    !        approach to avoid an expensive global search over
    !        all triangles.
    !
    !        This version does not calculate derivatives.
    !
    !        Calls are made to: plot_tc, stackpairinit,
    !                   poppair,pushpair & stackpairempty. 
    !
    !                   M. Sambridge, RSES, April 1995.
    !
    !------------------------------------------------------------------------
    !
        Subroutine nn2Dr &
                      (x,y,points,vertices,neighbour,centres,loc, &
                       data,nnpn_max,a,b,p,nodes,pairs,tstore,f,df,v)

        use m_nn2

        integer(4) :: nnpn_max,lud,loc,i,j,k,new1,new2,n1,n2,n3,n4,ii,node
        integer(4) :: kk,jj
        real(8) :: vol

        real(8)      points(2,*)
        real(8)      centres(3,*)
        real(8)      data(*)
        real(8)      x,y,dist,dx,dy
        real(8)      f,df(2),v
        real(8)      xo(2)
        real(8)      xs(2)
        real(8)          p(2,nnpn_max)
        integer     vertices(3,*)
        integer     neighbour(3,*)
        integer     tri,pos,pcount
        logical     nnwrite
        integer     pairs(2,nnpn_max)
        integer     nodes(nnpn_max)
        integer     tstore(nnpn_max)
        real(8)            a(nnpn_max,2)
        real(8)            b(nnpn_max)

            

        common/nnswitches/nnwrite,lud

        if(nnwrite)write(lud,*)' Result of nn2Dr search:'

    !                   Initialize variables
        f = 0.d0
        df(1) = 0.d0
        df(2) = 0.d0
        v = 0.d0

    !                   Determine list of 
    !                   neighbours to input point.
        
    !
    !                   Initialize pair list 
    !

            pairs(1,1) = vertices(1,loc)
            pairs(2,1) = vertices(2,loc)
            pairs(1,2) = vertices(2,loc)
            pairs(2,2) = vertices(3,loc)
            pairs(1,3) = vertices(3,loc)
            pairs(2,3) = vertices(1,loc)
            pcount = 3

        if(nnwrite)write(lud,*)loc

    !                   plot input circum-triangle 
    !                   and circum-circle

        call plot_tc()

    !                   record circum-triangle 
            tstore(1) = loc
            tstore(2) = loc
            tstore(3) = loc
    !
    !                   Find all other circumcircles 
    !                   containing input point and update
    !                   pair list from these triangles
    !
    !                   initialize stack
        call stackpairinit


    !                   put input triangle's neighbouring 
    !                   triangles on LIFO stack
    !                   together with position of input
    !                   triangle in their neighbour list

        i = neighbour(1,loc)
        j = neighbour(2,loc)
        k = neighbour(3,loc)
        if(i.ne.0)then
           if(neighbour(1,i).eq.loc)call pushpair(i,1)
           if(neighbour(2,i).eq.loc)call pushpair(i,2)
           if(neighbour(3,i).eq.loc)call pushpair(i,3)
        end if
        if(j.ne.0)then
           if(neighbour(1,j).eq.loc)call pushpair(j,1)
           if(neighbour(2,j).eq.loc)call pushpair(j,2)
           if(neighbour(3,j).eq.loc)call pushpair(j,3)
        end if
        if(k.ne.0)then
           if(neighbour(1,k).eq.loc)call pushpair(k,1)
           if(neighbour(2,k).eq.loc)call pushpair(k,2)
           if(neighbour(3,k).eq.loc)call pushpair(k,3)
        end if

     10 call stackpairempty(k)
    !                   if stack empty then finish
        if(k.eq.1)go to 100
    !                   take triangle from stack
        call poppair(tri,pos)

    !                   test if (x,y) is in circumcircle

            dx = x-centres(1,tri)
            dy = y-centres(2,tri)
        dist = dx*dx + dy*dy 

        if(dist.lt.centres(3,tri))then
               if(nnwrite)write(lud,*)tri

    !                   plot circum-triangle and circum-circle

           call plot_tc()

    !                   record each edge of current
    !                   triangles and triangle
    !
           if(pos.eq.1)then
              new1 = neighbour(2,tri)
              new2 = neighbour(3,tri)
                  n1 = 3
                  n2 = 1
                  n3 = 1
                  n4 = 2
           else if(pos.eq.2)then
              new1 = neighbour(1,tri)
              new2 = neighbour(3,tri)
                  n1 = 2
                  n2 = 3
                  n3 = 1
                  n4 = 2
           else 
              new1 = neighbour(1,tri)
              new2 = neighbour(2,tri)
                  n1 = 2
                  n2 = 3
                  n3 = 3
                  n4 = 1
           end if
               pcount = pcount + 1
               pairs(1,pcount) = vertices(n1,tri)
               pairs(2,pcount) = vertices(n2,tri)
               tstore(pcount) = tri
               pcount = pcount + 1
               pairs(1,pcount) = vertices(n3,tri)
               pairs(2,pcount) = vertices(n4,tri)
               tstore(pcount) = tri

           if(new1.ne.0)then
              if(neighbour(1,new1).eq.tri)call pushpair(new1,1)
              if(neighbour(2,new1).eq.tri)call pushpair(new1,2)
              if(neighbour(3,new1).eq.tri)call pushpair(new1,3)
           end if
           if(new2.ne.0)then
              if(neighbour(1,new2).eq.tri)call pushpair(new2,1)
              if(neighbour(2,new2).eq.tri)call pushpair(new2,2)
              if(neighbour(3,new2).eq.tri)call pushpair(new2,3)
           end if
        end if
        go to 10
     
     100    continue
            call stackpairflush()

    !                       check size of pcount
            if(pcount.gt.nnpn_max)then
               write(*,*)' '
               write(*,*)' Error: work arrays in subroutine nn2Dr', &
                        ' not big enough.'
               write(*,*)' Remedy: Increase size of parameter'
               write(*,*)'         nnpn_max in calling program'
               write(*,*)'         current value = ',nnpn_max
               write(*,*)'         required value >= ',pcount
           stop
            end if
    !                       write out neighbouring
    !                       nodes of input points
    !
    !       do 110 i=1,pcount
    !          write(*,*)' neighbouring pair of nodes',pairs(1,i),pairs(2,i)
    !110    continue
    !
    !                       find set of common neighbours
    !                       between input point, and each
    !                       of its natural neighbours
    !                       and use this information to
    !                       set up p matrix of co-ordinates.
    !
            j=0
            xs(1) = x
            xs(2) = y
            do 120 i=1,pcount
           tri = tstore(i)
               do 121 ii=1,2
              node = pairs(ii,i)
                  do 122 kk=1,j
                     if(node.eq.nodes(kk))go to 121
     122          continue
                  j = j + 1
              nodes(j) = node
                  p(1,1) = points(1,node)
                  p(2,1) = points(2,node)
                  jj = 1
                  do 130 k=1,pcount
                     if(pairs(1,k).eq.node)then
    !                   write(*,*)' Node:',node,' neighbour',pairs(2,k)
                        jj = jj + 1
                        p(1,jj) = points(1,pairs(2,k))
                        p(2,jj) = points(2,pairs(2,k))
                     end if
                     if(pairs(2,k).eq.node)then
    !                   write(*,*)' Node:',node,' neighbour',pairs(1,k)
                        jj = jj + 1
                        p(1,jj) = points(1,pairs(1,k))
                        p(2,jj) = points(2,pairs(1,k))
                     end if
     130          continue
                  
    !                       calculate volume of 
    !                       second-order voronoi cell 
    !                       for current node using
    !                       recursive formula and reset
    !                       origin to circumcentre
    !
                  xo(1) = centres(1,tri)
                  xo(2) = centres(2,tri)

                  call second_voronoi (xs,p,2,jj,nnpn_max,2,xo,a,b,vol)
    !
                  v = v + vol
                  f = f + vol*data(node)
     121       continue
     120    continue

            do 140 k=1,j
               nodes(k) = 0
     140    continue

        if(v.ne.0.0_8) f = f/v

        return
        end subroutine
    !
    !------------------------------------------------------------------------
    !
    !   nn2Drd - calculates natural neighbour interpolation at point x,y
    !        when point is inside convex hull 
    !        (using method based on recursive formulae)
    !
    !   Input:
    !       x,y         co-ordinates of input point
    !       points(2,np)        array of node points    
    !           vertices(3,nt)      array of triangle vertices  
    !           neighbour(3,nt)     array of neighbouring triangles.    
    !                   Neighbour(i,j) is the triangle
    !                   opposite node i in triangle j,
    !                   stored counterclockwise about j.
    !               centres(3,nt)           centres(j,i) (j=1,2) contains the
    !                                       co-ordinates of the centre of 
    !                                       circumcircle about Delaunay 
    !                                       triangle i, (i=1,...,nt),
    !                                       j=3 contains squared radius of circle.
    !           loc         index of triangle containing 
    !                   input point.
    !       data            data values at each node
    !       nnpn_max        maximum number of neighbours per node
    !                   (depends on the point distribution,
    !                                        set to ~50 in calling program)
    !
    !   Output:
    !       f           interpolated data value
    !       df(1)           interpolated derivative df/dx
    !       df(2)           interpolated derivative df/dy
    !       v           area of voronoi cell about (x,y)
    !
    !   Comments:
    !        On input point (x,y) must be in triangle loc. 
    !        Note: The input triangle contains (x,y) and so it's 
    !        circumcircle must also contain (x,y).
    !
    !        In order to use the recursive Lasserre formula to
    !        calculate the nn co-ordinate of the input point with
    !        respect to its ith neighbour we must find the set of
    !        nodes that are both neighbours of i and (x,y), where 
    !        the neighbours of i are determined before addition of 
    !        (x,y). This is done using an approach similar to Watson's
    !        algorithm for updating a Delaunay triangulation, although
    !        we avoid the complication of having to remove `double
    !        entires in the list' and also use Sloan's LIFO stack
    !        approach to avoid an expensive global search over
    !        all triangles.
    !
    !        This version also calculates derivatives.
    !
    !        Calls are made to: plot_tc, stackpairinit,
    !                   poppair,pushpair & stackpairempty. 
    !
    !                   M. Sambridge, RSES, April 1995.
    !
    !------------------------------------------------------------------------
    !
        Subroutine nn2Drd &
                      (x,y,points,vertices,neighbour,centres,loc, &
                       data,nnpn_max,a,b,p,nodes,pairs,tstore,f,df,v)

        use m_nn2

        integer(4) :: nnpn_max,lud,loc,i,j,k,new1,new2,n1,n2,n3,n4,ii,node,kk,jj
        real(8) :: vol

        real(8)      points(2,*)
        real(8)      centres(3,*)
        real(8)      data(*)
        real(8)      x,y,dist,dx,dy
        real(8)      f,df(2),v,dvx,dvy
        real(8)      xo(2)
        real(8)      xs(2)
        real(8)          p(2,nnpn_max)
        integer     vertices(3,*)
        integer     neighbour(3,*)
        integer     tri,pos,pcount
        logical     nnwrite
        integer     pairs(2,nnpn_max)
        integer     nodes(nnpn_max)
        integer     tstore(nnpn_max)
        real(8) ::       deriv(2)
        real(8) ::           a(nnpn_max,2)
        real(8) ::           b(nnpn_max)

            

        common/nnswitches/nnwrite,lud

        if(nnwrite)write(lud,*)' Result of nn2Dr search:'

    !                   Initialize variables
        f = 0.d0
        df(1) = 0.d0
        df(2) = 0.d0
        v = 0.d0
        dvx = 0.d0
        dvy = 0.d0
    !                   Initialize pair list 
    !
            pairs(1,1) = vertices(1,loc)
            pairs(2,1) = vertices(2,loc)
            pairs(1,2) = vertices(2,loc)
            pairs(2,2) = vertices(3,loc)
            pairs(1,3) = vertices(3,loc)
            pairs(2,3) = vertices(1,loc)
            pcount = 3

        if(nnwrite)write(lud,*)loc

    !                   plot input circum-triangle 
    !                   and circum-circle

        call plot_tc()

    !                   record circum-triangle 
            tstore(1) = loc
            tstore(2) = loc
            tstore(3) = loc
    !
    !                   Find all other circumcircles 
    !                   containing input point and update
    !                   pair list from these triangles
    !
    !                   initialize stack
        call stackpairinit


    !                   put input triangle's neighbouring 
    !                   triangles on LIFO stack
    !                   together with position of input
    !                   triangle in their neighbour list

        i = neighbour(1,loc)
        j = neighbour(2,loc)
        k = neighbour(3,loc)
        if(i.ne.0)then
           if(neighbour(1,i).eq.loc)call pushpair(i,1)
           if(neighbour(2,i).eq.loc)call pushpair(i,2)
           if(neighbour(3,i).eq.loc)call pushpair(i,3)
        end if
        if(j.ne.0)then
           if(neighbour(1,j).eq.loc)call pushpair(j,1)
           if(neighbour(2,j).eq.loc)call pushpair(j,2)
           if(neighbour(3,j).eq.loc)call pushpair(j,3)
        end if
        if(k.ne.0)then
           if(neighbour(1,k).eq.loc)call pushpair(k,1)
           if(neighbour(2,k).eq.loc)call pushpair(k,2)
           if(neighbour(3,k).eq.loc)call pushpair(k,3)
        end if

     10 call stackpairempty(k)
    !                   if stack empty then finish
        if(k.eq.1)go to 100
    !                   take triangle from stack
        call poppair(tri,pos)

    !                   test if (x,y) is in circumcircle

            dx = x-centres(1,tri)
            dy = y-centres(2,tri)
        dist = dx*dx + dy*dy 

        if(dist.lt.centres(3,tri))then
               if(nnwrite)write(lud,*)tri

    !                   plot input circum-triangle 
    !                   and circum-circle

           call plot_tc()

    !                   record each edge 
    !                   of current triangle.
    !
           if(pos.eq.1)then
              new1 = neighbour(2,tri)
              new2 = neighbour(3,tri)
                  n1 = 3
                  n2 = 1
                  n3 = 1
                  n4 = 2
           else if(pos.eq.2)then
              new1 = neighbour(1,tri)
              new2 = neighbour(3,tri)
                  n1 = 2
                  n2 = 3
                  n3 = 1
                  n4 = 2
           else 
              new1 = neighbour(1,tri)
              new2 = neighbour(2,tri)
                  n1 = 2
                  n2 = 3
                  n3 = 3
                  n4 = 1
           end if
               pcount = pcount + 1
               pairs(1,pcount) = vertices(n1,tri)
               pairs(2,pcount) = vertices(n2,tri)
               tstore(pcount) = tri
               pcount = pcount + 1
               pairs(1,pcount) = vertices(n3,tri)
               pairs(2,pcount) = vertices(n4,tri)
               tstore(pcount) = tri

           if(new1.ne.0)then
              if(neighbour(1,new1).eq.tri)call pushpair(new1,1)
              if(neighbour(2,new1).eq.tri)call pushpair(new1,2)
              if(neighbour(3,new1).eq.tri)call pushpair(new1,3)
           end if
           if(new2.ne.0)then
              if(neighbour(1,new2).eq.tri)call pushpair(new2,1)
              if(neighbour(2,new2).eq.tri)call pushpair(new2,2)
              if(neighbour(3,new2).eq.tri)call pushpair(new2,3)
           end if
        end if
        go to 10
     
     100    continue
            call stackpairflush()
    !                       check size of pcount
            if(pcount.gt.nnpn_max)then
               write(*,*)' '
               write(*,*)' Error: work arrays in subroutine nn2Drd', &
                        ' not big enough.'
               write(*,*)' Remedy: Increase size of parameter'
               write(*,*)'         nnpn_max in calling program'
               write(*,*)'         current value = ',nnpn_max
               write(*,*)'         required value >= ',pcount
           stop
            end if

    !                       write out neighbouring
    !                       nodes of input points
    !
    !       do 110 i=1,pcount
    !          write(*,*)' neighbouring pair of nodes',pairs(1,i),pairs(2,i)
    !110    continue
    !
    !                       find set of common neighbours
    !                       between input point, and each
    !                       of its natural neighbours.
    !                       and use this information to
    !                       set up p matrix of co-ordinates.
    !
            j=0
            xs(1) = x
            xs(2) = y
            do 120 i=1,pcount
               tri = tstore(i)
               do 121 ii=1,2
              node = pairs(ii,i)
                  do 122 kk=1,j
                     if(node.eq.nodes(kk))go to 121
     122          continue
                  j = j + 1
              nodes(j) = node
                  p(1,1) = points(1,node)
                  p(2,1) = points(2,node)
                  jj = 1
                  do 130 k=1,pcount
                     if(pairs(1,k).eq.node)then
    !                   write(*,*)' Node:',node,' neighbour',pairs(2,k)
                        jj = jj + 1
                        p(1,jj) = points(1,pairs(2,k))
                        p(2,jj) = points(2,pairs(2,k))
                     end if
                     if(pairs(2,k).eq.node)then
    !                   write(*,*)' Node:',node,' neighbour',pairs(1,k)
                        jj = jj + 1
                        p(1,jj) = points(1,pairs(1,k))
                        p(2,jj) = points(2,pairs(1,k))
                     end if
     130          continue
                  
    !                       calculate volume and derivatives
    !                       of second-order voronoi cell 
    !                       for current node using
    !                       recursive formula and reset
    !                                               origin to circumcentre
    !
                  xo(1) = centres(1,tri)
                  xo(2) = centres(2,tri)

                  call second_voronoi_d  &
                      (xs,p,2,jj,nnpn_max,2,xo,a,b,vol,deriv)
    !
                  v = v + vol
                  f = f + vol*data(node)
                  df(1) = df(1) + data(node)*deriv(1)
                  df(2) = df(2) + data(node)*deriv(2)
                  dvx = dvx + deriv(1)
                  dvy = dvy + deriv(2)
     121       continue
     120    continue

            do 140 k=1,j
               nodes(k) = 0
     140    continue

        if(v.ne.0.0_8)then
           f = f/v
           df(1) = (df(1) - f*dvx)/v
           df(2) = (df(2) - f*dvy)/v
        end  if

        return
        end subroutine
    !
    !------------------------------------------------------------------------
    !
    !   nn2Df - calculates natural neighbour interpolation at point x
    !       when point is inside convex hull 
    !       (using method based on a closed 2-D formula)
    !
    !   Input:
    !       x(2)            co-ordinates of input point
    !       points(2,np)        array of node points    
    !           vertices(3,nt)      array of triangle vertices  
    !           neighbour(3,nt)     array of neighbouring triangles.    
    !                   Neighbour(i,j) is the triangle
    !                   opposite node i in triangle j,
    !                   stored counterclockwise about j.
    !               centres(3,nt)           centres(j,i) (j=1,2) contains the
    !                                       co-ordinates of the centre of 
    !                                       circumcircle about Delaunay 
    !                                       triangle i, (i=1,...,nt),
    !                                       j=3 contains squared radius of circle.
    !           loc         index of triangle containing 
    !                   input point.
    !       data            data values at each node
    !       nnpn_max        maximum number of neighbours per node
    !                   (depends on the point distribution,
    !                                        set to ~50 in calling program)
    !       tstore          integer work array of size nnpn_max
    !       index           integer work array of size nnpn_max
    !       vpairs          integer work array of size 2*nnpn_max
    !       angles          real work array of size nnpn_max
    !       pverts          real(8) work array of size 2*nnpn_max
    !       vverts          real(8) work array of size 2*nnpn_max
    !       ltri            logical work array size of nt_max
    !       lnode           logical work array size of nt_max
    !
    !   Output:
    !       f           interpolated data value
    !       v           area of voronoi cell about x
    !
    !   Comments:
    !        On input point x must be in triangle loc. 
    !        Note: The input triangle contains x and so it's 
    !        circumcircle must also contain x.
    !
    !        In order to use the closed formula to
    !        calculate the nn co-ordinate of the input point with
    !        respect to its ith neighbour we must find the set of
    !        nodes that are both neighbours of i and x, where 
    !        the neighbours of i are determined before addition of 
    !        x. This is done using a LIFO stack to avoid an 
    !        expensive global search over all triangles.
    !
    !        It is assumed that the logical arrays ltri(nt_max) 
    !        and lnode are initialized to false on input.
    !
    !        This version does not calculate derivatives.
    !
    !        Calls are made to: plot_tc, stackpairinit,
    !                   poppair,pushpair & stackpairempty
    !                   indexx & second_v_area.
    !
    !                   M. Sambridge, RSES, April 1996.
    !
    !------------------------------------------------------------------------
    !
        Subroutine nn2Df &
           (x,points,vertices,neighbour,centres,loc,data,nnpn_max, &
            pverts,vverts,tstore,vpairs,angles,index,ltri,lnode,f,v)

        use m_nn2

        integer(4) :: nnpn_max,lud,loc,i,j,k,new1,new2,nv,it,node,j1,j2,na
        integer(4) :: nb,iopp,ncount,in,nverts,ip,im,kp,km,jt,jtri,jn,jnode
        real(8) :: area

        real(8)      points(2,*)
        real(8)      centres(3,*)
        real(8)      data(*)
        real(8)      x(2),dist,dx,dy
        real(8)      f,v
        real(8)          pverts(2,nnpn_max)
        real(8)          vverts(2,nnpn_max)
        integer     vertices(3,*)
        integer     neighbour(3,*)
        integer     tri,pos,tcount
        integer     tstore(nnpn_max)
        integer     index(nnpn_max)
        integer     vpairs(2,nnpn_max)
        logical     nnwrite
        logical*1   ltri(*)
        logical*1   lnode(*)
        real(8)            angles(nnpn_max)
    !
        integer     cyc(3)
        data        cyc/2,3,1/

            

        common/nnswitches/nnwrite,lud

    !                   Initialize variables
        f = 0.d0
        v = 0.d0
            tcount = 1

        if(nnwrite)write(lud,*)loc
    !                   plot input circum-triangle 
    !                   and circum-circle

        call plot_tc()

    !                   record circum-triangle 
            tstore(tcount) = loc
            ltri(loc) = logical(.true., 1)
    !
    !                   Find all other circumcircles 
    !                   containing input point using a 
    !                   directed walk
    !
    !                   initialize stack
        call stackpairinit
    !                   put input triangle's neighbouring 
    !                   triangles on LIFO stack
    !                   together with position of input
    !                   triangle in their neighbour list

        i = neighbour(1,loc)
        j = neighbour(2,loc)
        k = neighbour(3,loc)
        if(i.ne.0)then
           if(neighbour(1,i).eq.loc)then
                  call pushpair(i,1)
           else if(neighbour(2,i).eq.loc)then
                  call pushpair(i,2)
           else if(neighbour(3,i).eq.loc)then
                  call pushpair(i,3)
               end if
        end if
        if(j.ne.0)then
           if(neighbour(1,j).eq.loc)then
                  call pushpair(j,1)
           else if(neighbour(2,j).eq.loc)then
                  call pushpair(j,2)
           else if(neighbour(3,j).eq.loc)then
                  call pushpair(j,3)
               end if
        end if
        if(k.ne.0)then
           if(neighbour(1,k).eq.loc)then
                  call pushpair(k,1)
           else if(neighbour(2,k).eq.loc)then
                  call pushpair(k,2)
           else if(neighbour(3,k).eq.loc)then
                  call pushpair(k,3)
               end if
        end if

     10 call stackpairempty(k)
    !                   if stack empty then finish
        if(k.eq.1)go to 100
    !                   take triangle from stack
        call poppair(tri,pos)

    !                   test if x is in circumcircle
            dx = x(1)-centres(1,tri)
            dy = x(2)-centres(2,tri)
        dist = dx*dx + dy*dy 

        if(dist.lt.centres(3,tri))then
               if(nnwrite)write(lud,*)tri

    !                   plot circum-triangle and circum-circle

           call plot_tc()

           if(pos.eq.1)then
              new1 = neighbour(2,tri)
              new2 = neighbour(3,tri)
           else if(pos.eq.2)then
              new1 = neighbour(1,tri)
              new2 = neighbour(3,tri)
           else 
              new1 = neighbour(1,tri)
              new2 = neighbour(2,tri)
           end if
    !                   record current circum-triangle
    !                   and pseudo angle to x
               tcount = tcount + 1
               tstore(tcount) = tri
               ltri(tri) = logical(.true., 1)

           if(new1.ne.0)then
              if(neighbour(1,new1).eq.tri)then
                     call pushpair(new1,1)
              else if(neighbour(2,new1).eq.tri)then
                     call pushpair(new1,2)
              else if(neighbour(3,new1).eq.tri)then
                     call pushpair(new1,3)
                  end if
           end if
           if(new2.ne.0)then
              if(neighbour(1,new2).eq.tri)then
                     call pushpair(new2,1)
              else if(neighbour(2,new2).eq.tri)then
                     call pushpair(new2,2)
              else if(neighbour(3,new2).eq.tri)then
                     call pushpair(new2,3)
                  end if
           end if
        end if
        go to 10
     
     100    continue
            call stackpairflush()

    !                       check size of tcount
            if(tcount+2.gt.nnpn_max)then
               write(*,*)' '
               write(*,*)' Error: work arrays in subroutine nn2Df', &
                        ' not big enough.'
               write(*,*)' Too many circum-triangles for this node'
               write(*,*)' Remedy: Increase size of parameter'
               write(*,*)'         nnpn_max in calling program'
               write(*,*)'         current value = ',nnpn_max
               write(*,*)'         required value >= ',tcount+2
           stop
            end if
    !
    !       call xsave
    !
    !
    !                       find list of vertices
    !                       of new voronoi cell about
    !                       x and store the pair of
    !                       nodes associated with each
    !                       vertex
    !       write(*,*)' Circum-triangles of x:'
    !
            nv = 0
            do 110 i=1,tcount
               it = tstore(i)
    !          write(*,*)it
               do j=1,3
                  node = vertices(j,it)
                  j1 = cyc(j)
                  j2 = cyc(j1)
                  na = vertices(j1,it)
                  nb = vertices(j2,it)
                  iopp = neighbour(j,it)
                  if(iopp.eq.0.or..not.logical(ltri(iopp), 4))then
                     nv = nv + 1
    !
    !                       find circum-centre of
    !                       x, node, and nodeb 
    !
                     call circum (x,points(1,na), &
                         points(1,nb),vverts(1,nv))

                     vpairs(1,nv) = na
                     vpairs(2,nv) = nb
                     angles(nv) = pangle(x,vverts(1,nv))
                      
                  end if
               end do
               
     110    continue
            
    !                       sort vertices 
    !
            call indexx(nv,angles,index)
    !
    !                       debug I/O
    !                       
    !
    !       write(*,*)' Number of vertices = ',nv
    !   do i=1,nv
    !          write(*,*)' i',i,' index',index(i),' a',angles(i)
    !       end do
    !
    !                       find vertices of each 
    !                       second-order Voronoi cell
    !
        ncount = 0
    !                       loop over neighbours of x
            do 120 it=1,tcount
           tri = tstore(it)
               do 130 in = 1,3
                  node = vertices(in,tri)
                  if(lnode(node))go to 130
                  lnode(node) = logical(.true., 1)
                  nverts=0
    !             write(*,*)' Neighbour node =',node
    !
    !                       find pair of `external' vertices
    !                       of second-order voronoi cell  
    !
                  do i=1,nv
                     k = index(i)
                     if(vpairs(1,k).eq.node.or. &
                       vpairs(2,k).eq.node)then
                        nverts = nverts + 1
                        pverts(1,nverts) = vverts(1,k) - x(1)
                        pverts(2,nverts) = vverts(2,k) - x(2)
                        ip = i+1
                        im = i-1
                        if(i.eq.nv)ip=1
                        if(i.eq.1)im=nv
                        kp = index(ip)
                        km = index(im)
                        if(vpairs(1,kp).eq.node.or. &
                          vpairs(2,kp).eq.node)then
                           nverts = nverts + 1
                           pverts(1,nverts) = vverts(1,kp) - x(1)
                           pverts(2,nverts) = vverts(2,kp) - x(2)
                        else if(vpairs(1,km).eq.node.or. &
                               vpairs(2,km).eq.node)then
                           nverts = nverts + 1
                           pverts(1,1) = vverts(1,km) - x(1)
                           pverts(2,1) = vverts(2,km) - x(2)
                           pverts(1,2) = vverts(1,k) - x(1)
                           pverts(2,2) = vverts(2,k) - x(2)
                        else
                           write(*,*)' Error in nn2Df'
                           write(*,*) &
                          ' Can not find neighbouring vertices' 
                           write(*,*)' node=',node,' i =',i
                        end if
                        go to 150
                     end if
                  end do
     150          continue
    !
    !                       find `internal' vertices of 
    !                       second-order voronoi cell  
              do 140 jt = 1,tcount
                     jtri = tstore(jt)
                     do jn = 1,3
                        jnode = vertices(jn,jtri)
                        if(jnode.eq.node)then
                           nverts = nverts + 1
    !                       record circum-centre
    !
                           pverts(1,nverts) = centres(1,jtri) - x(1)
                           pverts(2,nverts) = centres(2,jtri) - x(2)
                           go to 140
                        end if
                     end do
     140          continue
    !                       Calculate volume of 
    !                       second-order voronoi cell 
    !                       for current node using
    !                       direct 2-D formula 
    !
                  call second_v_area &
                      (nverts,pverts,angles,area)
    !
               v = v + area
               f = f + area*data(node)

     130       continue
     120    continue
        if(v.ne.0.0_8) f = f/v
            v = v*0.5_8
    !
    !                       reset logical arrays
            do i=1,tcount
               tri = tstore(i)
               ltri(tri) = logical(.false., 1)
               do j = 1,3
                  lnode(vertices(j,tri)) = logical(.false., 1)
               end do
            end do 

        return
        end subroutine

    end module m_nn1

