        module m_del_flip
            contains

!
!------------------------------------------------------------------------
!
!   del_flip - flips a triangulation into a Delaunay triangulation in 2-D
!
!   Input:
!       points(2,np)        list of node co-ordinates   
!           v(3,*)          list of triangle vertices   
!           e(3,*)          list of neighbouring triangles. 
!                   E(i,j) is the triangle which
!                   shares the face containing 
!                   node mod((i+1),3) and mod(i+1),3)+1
!                   in triangle j, stored counterclockwise 
!                   about j.
!                   (This is the `opposite' definition) 
!       nt          number of triangles in current
!                   triangulation.
!       mask            a logical work array which stores
!                   all triangles that have been flipped
!                   at sometime during the calculation.
!       mask_e          a logical work array which stores
!                   the list of edges on the stack.
!                   This is used to prevent the same
!                   edge being put on the stack twice.
!
!   Output:
!
!               v           updated
!               e           updated
!       nswaps          the number of swaps required to
!                   flip into a Delaunay. If nswaps = 0
!                   then original triangulation was 
!                   Delaunay.
!
!   Comments:
!
!   The method uses a combination of a standard flipping algorithm
!   using a LIFO stack and a mask to record all triangles touched
!   during the stack.
!
!   This uses an algorithm that guarantees a Delaunay after one pass.
!
!   Converted to use an adjacency array defined using the `opposite'
!   definition 31/1/96.
!
!                   M. Sambridge, RSES, Aug. 1995.
!
!------------------------------------------------------------------------
!
    Subroutine del_flip (points,e,v,nt,mask,mask_e,nswaps) 
        use m_del_sub

    real*8      points(2,*)
    integer     v(3,*)
    integer     e(3,*)
    integer     erl,era,erb,erc,v1,v2,v3,a,b,c,d,l,r
    integer     p,t,pos
    logical     mask(*)
    logical     mask_e(3,*)
    integer     c1(3),c2(3)
    data        c1/2,3,1/
    data        c2/3,1,2/
    integer(4) :: nswaps,nt,k

!                   initialize masks
    nswaps = 0
    do 10 t=1,nt
           mask(t) = .false.
           mask_e(1,t) = .false.
           mask_e(2,t) = .false.
           mask_e(3,t) = .false.
 10     continue
!                   main loop over initial triangles
    do 20 t=1,nt

       if(mask(t))go to 20

!d         write(*,*)' '
!d         write(*,*)' Starting stack from triangle',t
!d         write(*,*)' '

           v1 = v(1,t)
           v2 = v(2,t)
           v3 = v(3,t)
       a = e(c2(1),t)
       b = e(c2(2),t)
       c = e(c2(3),t)
!                   loop over edges of base triangle
           if(a.ne.0)then
              era = edg(a,t,e)
              era = c1(era)
           end if

           if(b.ne.0)then
              erb = edg(b,t,e)
              erb = c1(erb)
           end if

           if(c.ne.0)then
              erc = edg(c,t,e)
              erc = c1(erc)
           end if

!d         write(*,*)' i = 1',' t = ',t
!d         write(*,*)' v ',v(1,t),v(2,t),v(3,t)
!d         write(*,*)' e ',e(c2(1),t),e(c2(2),t),e(c2(3),t)
!d         write(*,*)' a b c ',a,b,c

!                   initialize stackpair
       call stackpairinit

!                   add edges of base triangle to stack
!
       if(a.ne.0)then
              call pushpair(a,era)
              mask_e(era,a) = .true.
!d        write(*,*)' pushed pair ',a,era,
!d   &                  ' nodes: ',v(era,a),v(c1(era),a)
           end if

       if(b.ne.0)then
              call pushpair(b,erb)
              mask_e(erb,b) = .true.
!d        write(*,*)' pushed pair ',b,erb,
!d   &                  ' nodes: ',v(erb,b),v(c1(erb),b)
           end if

       if(c.ne.0)then
              call pushpair(c,erc)
              mask_e(erc,c) = .true.
!d        write(*,*)' pushed pair ',c,erc,
!d   &                  ' nodes: ',v(erc,c),v(c1(erc),c)
           end if
!                   check stack is not empty

           if(a.eq.0.and.b.eq.0.and.c.eq.0)go to 100

!                   loop while stack is not empty
 50        continue
      
       call poppair(r,pos)

!d     write(*,*)' popped pair ',r,pos,
!d   &               ' nodes: ',v(pos,r),v(c1(pos),r)

       l = e(c2(pos),r)
!                   reset edge mask for edge l-r
           mask_e(pos,r) = .false.
!                   check if new point is in circumcircle
!
           era = c1(pos)
           erb = c2(pos)
       v1 = v(pos,r)
       v2 = v(era,r)
       v3 = v(erb,r)
       erl=edg(l,r,e)
           erl = c1(erl)
           p = v(c2(erl),l)
      
     if(swap(points(1,v1),points(1,v2),points(1,v3),points(1,p)))then

!                   update triangle mask
          mask(r) = .true.
          mask(l) = .true.

!d            write(*,*)' triangles flipped =',r,l

          nswaps = nswaps + 1
!                   new point is inside circumcircle
!                   for triangle r so swap diagonal
              a=e(c2(era),r)
              b=e(c2(erb),r)
              erc=c1(erl)
              c=e(c2(erc),l)
              d=e(c2(c2(erl)),l)
!                   update adjacency list for triangle l
          v(erc,l) = v3
          e(c2(erc),l) = r
          e(c2(erl),l)  = a

!                   update adjacency list for triangle r
          v(1,r)=p
          v(2,r)=v3
          v(3,r)=v1
          e(c2(1),r)=l
          e(c2(2),r)=b
          e(c2(3),r)=c
!                   put edges l-a and r-b on stack
!                   update adjacency list for 
!                   triangle a 
          if(a.ne.0)then
             pos = edg(a,r,e)
             pos = c1(pos)
             e(c2(pos),a)=l
                 if(.not.mask_e(pos,a))then
                    call pushpair(a,pos)
!d              write(*,*)' pushed pair ',a,pos,
!d   &                        ' nodes: ',v(pos,a),v(c1(pos),a)
         end if
          end if

          if(b.ne.0)then
             pos = edg(b,r,e)
             pos = c1(pos)
                 if(.not.mask_e(pos,b))then
                    call pushpair(b,pos)
!d              write(*,*)' pushed pair ',b,pos,
!d   &                        ' nodes: ',v(pos,b),v(c1(pos),b)
         end if
              end if
!                   put edges l-d and r-c on stack
!                   update adjacency list for 
!                   triangle c
          if(c.ne.0) then
                 pos = edg(c,l,e)
             pos = c1(pos)
                 e(c2(pos),c)=r
                 if(.not.mask_e(pos,c))then
                    call pushpair(c,pos)
!d              write(*,*)' pushed pair ',c,pos,
!d   &                        ' nodes: ',v(pos,c),v(c1(pos),c)
         end if
          end if

          if(d.ne.0) then
                 pos = edg(d,l,e)
             pos = c1(pos)
                 if(.not.mask_e(pos,d))then
                    call pushpair(d,pos)
!d              write(*,*)' pushed pair ',d,pos,
!d   &              ' nodes: ',v(pos,d),v(c1(pos),d)
         end if
          end if
       end if

       call stackpairempty(k)
       if(k.ne.1)go to 50
 100       continue
           call stackpairflush()
           
 20     continue
   
            return
            end subroutine del_flip
        end module m_del_flip

