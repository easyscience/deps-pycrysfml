!!----
!!---- SUBMODULE CFML_Math_3D
!!----
!!----
!!
Submodule (CFML_Maths) Maths_Resolv_System
 implicit none
 Contains

    !!----
    !!---- RESOLV_SIST_1X2
    !!--<<
    !!----     w11 x1 + w12 x2  = t1
    !!----     x_sol(i)= ts(i) + x(i) ix(i)
    !!-->>
    !!---- 04/04/2019
    !!
    Module Subroutine Resolv_Sist_1x2(w,t,ts,x,ix)
       !---- Arguments ----!
       integer, dimension(2),         intent( in) :: w      ! Input vector
       real(kind=cp),                 intent( in) :: t      ! Input value
       real(kind=cp), dimension(2),   intent(out) :: ts     ! Fixed value solution
       real(kind=cp), dimension(2),   intent(out) :: x      ! Fixed value for x,y
       integer, dimension(2),         intent(out) :: ix     ! 1: x, 2: y, 3: z

       !> Init
       ts = 0.0_cp
       x  = 1.0_cp
       ix = 0

       !> Both are zeros
       if ( all(w == 0)) then
          if (abs(t) < epss) then
             ix(1)=1
             ix(2)=2
          else
             err_cfml%ierr=1
             err_cfml%msg="MATHS@RESOLV_SIST_1x2: Inconsistent solution (1x2)"
          end if
          return
       end if

       !> Any is zero
       if (any(w == 0)) then
          if ( w(1) == 0 ) then
             ix(1)=1
             ts(2)=t/real(w(2))
              x(2)=0.0
          else
             ts(1)=t/real(w(1))
              x(1)=0.0
             ix(2)=2
          end if
       else
          ix(1)=1
          ts(2)=t/real(w(2))
           x(2)=-real(w(1))/real(w(2))
          ix(2)=1
       end if

    End Subroutine Resolv_Sist_1x2

    !!----
    !!----  RESOLV_SIST_1X3
    !!--<<
    !!----    w11 x1 + w12 x2 + w13 x3 = t1
    !!----    x_sol(i)= ts(i) + x(i) ix(i)
    !!-->>
    !!---- 04/04/2019
    !!
    Module Subroutine Resolv_Sist_1x3(w,t,ts,x,ix)
       !---- Arguments ----!
       integer, dimension(3),         intent( in) :: w      ! Input vector
       real(kind=cp),                 intent( in) :: t      ! Input value
       real(kind=cp), dimension(3),   intent(out) :: ts     ! Fixed value solution
       real(kind=cp), dimension(3),   intent(out) :: x      ! Fixed value for x,y,z
       integer, dimension(3),         intent(out) :: ix     ! 1: x, 2: y, 3: z

       !---- Local Variables ----!
       integer               :: i, zeros
       integer, dimension(2) :: w1
       integer, dimension(2) :: ix1
       real(kind=cp), dimension(2)    :: ts1
       real(kind=cp), dimension(2)    :: x1

       !> Init
       ts = 0.0_cp
       x  = 1.0_cp
       ix = 0

       zeros=0
       do i=1,3
          if (w(i) == 0) zeros=zeros+1
       end do
       select case (zeros)
          case (3)
             if (abs(t) < epss) then
                do i=1,3
                   ix(i)=i
                end do
             else
                err_cfml%ierr=1
                err_cfml%msg="MATHS@RESOLV_SIST_1x3: Inconsistent solution (1 x 3)"
             end if

          case (2)
             do i=1,3
                if (w(i) /= 0) then
                   ts(i)=t/real(w(i))
                   x(i) =0.0
                else
                   ix(i)=i
                end if
             end do

          case (1)
             do i=1,3
                if (w(i) == 0) exit
             end do
             select case (i)
                case (1)
                   w1=w(2:3)

                case (2)
                   w1(1)=w(1)
                   w1(2)=w(3)

                case (3)
                   w1=w(1:2)
             end select
             call resolv_sist_1x2(w1,t,ts1,x1,ix1)
             select case (i)
                case (1)
                   ix(1)  = 1
                   ts(2:3)= ts1
                   x(2:3) = x1
                   if (ix1(1)==1) ix(2)=2
                   if (ix1(1)==2) ix(2)=3
                   if (ix1(2)==1) ix(3)=2
                   if (ix1(2)==2) ix(3)=3

                  case (2)
                     ix(2)= 2
                     ts(1)= ts1(1)
                     ts(3)= ts1(2)
                     x(1) = x1(1)
                     x(3) = x1(2)
                     if (ix1(1)==1) ix(1)=1
                     if (ix1(1)==2) ix(1)=3
                     if (ix1(2)==1) ix(3)=1
                     if (ix1(2)==2) ix(3)=3

                  case (3)
                     ix(3)  = 3
                     ts(1:2)= ts1
                     x(1:2) = x1
                     ix(1:2)= ix1
               end select

          case (0)
             err_cfml%ierr=1
             err_cfml%msg="MATHS@RESOLV_SIST_1x3: Inconsistent case ax+by+cz=t (1x3)"
       end select

    End Subroutine Resolv_Sist_1x3

    !!----
    !!----  RESOLV_SIST_2X2
    !!--<<
    !!----     w11 x1 + w12 x2  = t1
    !!----     w21 x1 + w22 x2  = t2
    !!----     x_sol(i)= ts(i) + x(i) ix(i)
    !!-->>
    !!---- 04/04/2019
    !!
    Module Subroutine Resolv_Sist_2x2(w,t,ts,x,ix)
       !---- Arguments ----!
       integer, dimension(2,2),       intent( in) :: w       ! Input vector
       real(kind=cp),dimension(2),    intent( in) :: t       ! Input value
       real(kind=cp),dimension(2),    intent(out) :: ts      ! Fixed value solution
       real(kind=cp),dimension(2),    intent(out) :: x       ! Fixed value for x,y
       integer, dimension(2),         intent(out) :: ix      ! 1: x, 2: y, 3: z

       !---- Local Variables ----!
       integer                 :: i,deter
       integer, dimension(2)   :: zeros,colum
       real(kind=cp)           :: rden, rnum

       !> Init
       ts    = 0.0_cp
       x     = 1.0_cp
       ix    = 0

       deter = w(1,1)*w(2,2) - w(1,2)*w(2,1)
       rden=real(deter)
       if (deter /= 0) then
          !---- X(1) ----!
          rnum=t(1)*w(2,2) - w(1,2)*t(2)
          ts(1)=rnum/rden

          !---- X(2) ----!
          rnum=w(1,1)*t(2) - t(1)*w(2,1)
          ts(2)=rnum/rden

          x =0.0

       else                        ! Singular Matrix
          !---- Are there zero rows? ----!
          zeros=0
          do i=1,2
             if (w(i,1) == 0 .and. w(i,2) == 0 )  zeros(i)=1
          end do
          select case (sum(zeros))
             case (2)
                if (abs(t(1)) <= epss .and. abs(t(2)) <= epss) then
                   ix(1)=1
                   ix(2)=2
                else
                   err_cfml%ierr=1
                   err_cfml%msg="MATHS@RESOLV_SIST_2x2: Inconsistent solution (2x2)"
                end if

             case (1)
                do i=1,2
                   if (zeros(i) == 0) exit
                end do
                call resolv_sist_1x2(w(i,:),t(i),ts,x,ix)

             case (0)
                !---- Are there zero columns? ----!
                colum=0
                do i=1,2
                   if (w(1,i) == 0 .and. w(2,i) == 0 ) colum(i)=1
                end do
                select case (sum(colum))
                   case (1)
                      do i=1,2
                         if (colum(i) == 0) exit
                      end do
                      if (w(1,i) /= 0) then
                         ts(i)=t(1)/real(w(1,i))
                      else
                         ts(i)=t(2)/real(w(2,i))
                      end if
                      x(i)=0.0
                      if (i == 1) then
                         ix(2)=2
                      else
                         ix(1)=1
                      end if

                   case (0)
                      call resolv_sist_1x2(w(1,:),t(1),ts,x,ix)

                end select
          end select
       end if

    End Subroutine Resolv_Sist_2x2


    !!----
    !!---- RESOLV_SIST_2X3
    !!----     w11 x1 + w12 x2 + w13 x3 = t1
    !!----     w21 x1 + w22 x2 + w23 x3 = t2
    !!----     x_sol(i)= ts(i) + x(i) ix(i)
    !!----
    !!---- 04/04/2019
    !!
    Module Subroutine Resolv_Sist_2x3(w,t,ts,x,ix)
       !---- Arguments ----!
       integer, dimension(2,3),          intent( in) :: w     ! Input vector
       real(kind=cp), dimension(2),      intent( in) :: t     ! Input value
       real(kind=cp), dimension(3),      intent(out) :: ts    ! Fixed value solution
       real(kind=cp), dimension(3),      intent(out) :: x     ! Fixed value for x,y
       integer, dimension(3),            intent(out) :: ix    ! 1: x, 2: y, 3: z

       !---- Local Variables ----!
       integer                 :: i, j
       integer, dimension(2)   :: fila
       integer, dimension(2)   :: ix1
       integer, dimension(3)   :: colum
       integer, dimension(2,2) :: w1
       integer, dimension(2,3) :: wm
       integer, dimension(2)   :: wc
       real(kind=cp)                    :: tc
       real(kind=cp), dimension(2)      :: tm
       real(kind=cp), dimension(2)      :: ts1, x1

       !> Init
       ts    = 0.0_cp
       x     = 1.0_cp
       ix    = 0

       !---- Are there zero columns? ----!
       colum=0
       do i=1,3
            if (all(w(:,i) == 0)) colum(i)=1
       end do
       select case (sum(colum))
          case (3)
             if (abs(t(1)) <= epss .and. abs(t(2)) <= epss) then
                do i=1,3
                   ix(i)=i
                end do
             else
                err_cfml%ierr=1
                err_cfml%msg="MATHS@RESOLV_SIST_2x3: Inconsistent solution in (2x3)"
             end if

          case (2)
             do i=1,3
                if (colum(i) == 0) exit
             end do
             if (w(1,i) /= 0) then
                ts(i)=t(1)/real(w(1,i))
             else
                ts(i)=t(2)/real(w(2,i))
             end if
             x(i)=0.0
             select case (i)
                case (1)
                   ix(2)=2
                   ix(3)=3

                case (2)
                   ix(1)=1
                   ix(3)=3

                case (3)
                   ix(1)=1
                   ix(2)=2
             end select

          case (1)
             do i=1,3
                if (colum(i) == 1) exit
             end do
             select case (i)
                case (1)
                   w1=w(:,2:3)

                case (2)
                   w1(1,1)=w(1,1)
                   w1(1,2)=w(1,3)
                   w1(2,1)=w(2,1)
                   w1(2,2)=w(2,3)

                case (3)
                   w1=w(:,1:2)
             end select
             call resolv_sist_2x2(w1,t,ts1,x1,ix1)
             select case (i)
                case (1)
                   ix(1)  = 1
                   ts(2:3)= ts1
                   x (2:3)= x1
                   if (ix1(1) == 1) ix(2)=2
                   if (ix1(1) == 2) ix(2)=3
                   if (ix1(2) == 1) ix(3)=2
                   if (ix1(2) == 2) ix(3)=3

                case (2)
                   ix(2)=2
                   ts(1)=ts1(1)
                   ts(3)=ts1(2)
                   x(1) = x1(1)
                   x(3) = x1(2)
                   if (ix1(1) == 1) ix(1)=1
                   if (ix1(1) == 2) ix(1)=3
                   if (ix1(2) == 1) ix(3)=1
                   if (ix1(2) == 2) ix(3)=3

                case (3)
                   ix(3)  = 3
                   ts(1:2)= ts1
                   x (1:2)= x1
                   ix(1:2)= ix1
             end select

          case (0)
             !---- Are there zeros in any element of rows? ----!
             fila = 0
             do i=1,2
                if (all(w(i,:)==0)) fila(i)=1
             end do
             select case (sum(fila))
                case (1)
                   if (w(1,1) /= 0) then
                      call resolv_sist_1x3(w(1,:),t(1),ts,x,ix)
                   else
                      call resolv_sist_1x3(w(2,:),t(2),ts,x,ix)
                   end if

                case (0)
                   fila = 0
                   wm   = w
                   tm   = t
                   !---- Are there zeros in any element of rows? ----!
                   do i=1,2
                      do j=1,3
                         if (w(i,j)==0) fila(i)=fila(i)+1
                      end do
                   end do
                   if ( fila(2) > fila(1) ) then
                      wm(1,:)=w(2,:)
                      wm(2,:)=w(1,:)
                      tm(1)  =t(2)
                      tm(2)  =t(1)
                          j  =fila(1)
                      fila(1)=fila(2)
                      fila(2)=j
                   end if
                   select case (fila(1))
                      case (2)
                         do i=1,3
                            if (wm(1,i) /= 0) exit
                         end do
                         ts(i)=tm(1)/real(wm(1,i))
                         x(i)=0.0
                         select case (i)
                            case (1)
                               wc(1)=wm(2,2)
                               wc(2)=wm(2,3)
                               tc=tm(2)-(wm(2,1)*ts(i))

                            case (2)
                               wc(1)=wm(2,1)
                               wc(2)=wm(2,3)
                               tc=tm(2)-(wm(2,2)*ts(i))

                            case (3)
                               wc(1)=wm(2,1)
                               wc(2)=wm(2,2)
                               tc=tm(2)-(wm(2,3)*ts(i))
                         end select
                         call resolv_sist_1x2(wc,tc,ts1,x1,ix1)
                         select case(i)
                            case (1)
                               ts(2:3)=ts1
                                x(2:3)=x1
                                if (ix1(1)==1) ix(2)=2
                                if (ix1(1)==2) ix(2)=3
                                if (ix1(2)==1) ix(3)=2
                                if (ix1(2)==2) ix(3)=3

                            case (2)
                               ts(1)=ts1(1)
                               ts(3)=ts1(2)
                                x(1)=x1(1)
                                x(3)=x1(2)
                                if (ix1(1)==1) ix(1)=1
                                if (ix1(1)==2) ix(1)=3
                                if (ix1(2)==1) ix(3)=1
                                if (ix1(2)==2) ix(3)=3

                            case (3)
                               ts(1:2)=ts1
                                x(1:2)=x1
                               ix(1:2)=ix1
                         end select

                      case (1)
                         do i=1,3
                            if (wm(1,i) == 0) exit
                         end do
                         select case (fila(2))
                            case (1)
                               do j=1,3
                                  if (wm(2,j) == 0) exit
                               end do
                               select case (i)
                                  case (1)             ! 0 en w(1,1)
                                     select case (j)
                                        case (2)
                                           wc(1)=-wm(2,1)/wm(2,3)
                                           wc(2)= wm(1,2)/wm(1,3)
                                           tc=tm(1)/real(wm(1,3)) - tm(2)/real(wm(2,3))
                                           call resolv_sist_1x2(wc,tc,ts1,x1,ix1)
                                           ts(1:2)=ts1
                                           x(1:2) =x1
                                           ix(1:2)=ix1
                                           if (ix(1) == 0) then
                                              ts(3)=tm(2)/real(wm(2,3)) - ts(1)*wm(2,1)/real(wm(2,3))
                                              x(3)=0.0
                                           else
                                              if (ix(2) == 0) then
                                                 ts(3)=tm(1)/real(wm(1,3)) - ts(2)*wm(1,2)/real(wm(1,3))
                                                 x(3)=0.0
                                              else
                                                 ts(3)=tm(2)/real(wm(2,3))
                                                 x(3)=-real(wm(2,1))/real(wm(2,3))
                                                 ix(3)=1

                                                 ts(2)=tc/real(wc(2))
                                                 x(2) =-real(wc(1))/real(wc(2))
                                                 ix(2)=1
                                              end if
                                           end if

                                        case (3)
                                           wc(1)=-wm(2,1)/wm(2,2)
                                           wc(2)= wm(1,3)/wm(1,2)
                                           tc=tm(1)/real(wm(1,2)) - tm(2)/real(wm(2,2))
                                           call resolv_sist_1x2(wc,tc,ts1,x1,ix1)
                                           ts(1)=ts1(1)
                                           ts(3)=ts1(2)
                                           x(1) =x1(1)
                                           x(3) =x1(2)
                                           if (ix1(1) == 1) ix(1)=1
                                           if (ix1(1) == 2) ix(1)=3
                                           if (ix1(2) == 1) ix(3)=1
                                           if (ix1(2) == 2) ix(3)=3
                                           if (ix(1) == 0) then
                                              ts(2)=tm(2)/real(wm(2,2)) - ts(1)*wm(2,1)/real(wm(2,2))
                                              x(2)=0.0
                                           else
                                              if (ix(3) == 0) then
                                                 ts(2)=tm(1)/real(wm(1,2)) - ts(3)*wm(1,3)/real(wm(1,2))
                                                 x(2)=0.0
                                              else
                                                 ts(2)=tm(2)/real(wm(2,2))
                                                 x(3)=-real(wm(2,1))/real(wm(2,2))
                                                 ix(2)=1

                                                 ts(3)=tc/real(wc(2))
                                                 x(3) =-real(wc(1))/real(wc(2))
                                                 ix(3)=1
                                              end if
                                           end if
                                     end select

                                  case (2)             ! 0 en w(1,2)
                                     select case (j)
                                        case (1)
                                           wc(1)= wm(1,1)/wm(1,3)
                                           wc(2)=-wm(2,2)/wm(2,3)
                                           tc=tm(1)/real(wm(1,3)) - tm(2)/real(wm(2,3))
                                           call resolv_sist_1x2(wc,tc,ts1,x1,ix1)
                                           ts(1:2)=ts1
                                           x(1:2) =x1
                                           ix(1:2)=ix1
                                           if (ix(1) == 0) then
                                              ts(3)=tm(1)/real(wm(1,3)) - ts(1)*wm(1,1)/real(wm(1,3))
                                              x(3)=0.0
                                           else
                                              if (ix(2) == 0) then
                                                 ts(3)=tm(2)/real(wm(2,3)) - ts(2)*wm(2,2)/real(wm(2,3))
                                                 x(3)=0.0
                                              else
                                                 ts(3)=tm(1)/real(wm(1,3))
                                                 x(3)=-real(wm(1,1))/real(wm(1,3))
                                                 ix(3)=1

                                                 ts(2)=tc/real(wc(2))
                                                 x(2) = -real(wc(1))/real(wc(2))
                                                 ix(2)= 1
                                              end if
                                           end if

                                        case (3)
                                           wc(1)=-wm(2,2)/wm(2,1)
                                           wc(2)= wm(1,3)/wm(1,1)
                                           tc=tm(1)/real(wm(1,1)) - tm(2)/real(wm(2,1))
                                           call resolv_sist_1x2(wc,tc,ts1,x1,ix1)
                                           ts(2:3)=ts1
                                           x(2:3) =x1
                                           if (ix1(1) == 1) ix(2)=2
                                           if (ix1(1) == 2) ix(2)=3
                                           if (ix1(2) == 1) ix(3)=2
                                           if (ix1(2) == 2) ix(3)=3
                                           if (ix(2) == 0) then
                                              ts(1)=tm(2)/real(wm(2,1)) - ts(2)*wm(2,2)/real(wm(2,1))
                                              x(1)=0.0
                                           else
                                              if (ix(3) == 0) then
                                                 ts(1)=tm(1)/real(wm(1,1)) - ts(3)*wm(1,3)/real(wm(1,1))
                                                 x(1)=0.0
                                              else
                                                 ix(1)=1

                                                 ts(2)=tm(2)/real(wm(2,2))
                                                 x(2) =-real(wm(2,1))/real(wm(2,2))
                                                 ix(2)=1

                                                 ts(3)=tm(1)/real(wm(1,3))
                                                 x(3) =-real(wm(1,1))/real(wm(1,3))
                                                 ix(3)=1
                                              end if
                                           end if
                                     end select

                                  case (3)             ! 0 en w(1,3)
                                     select case (j)
                                        case (1)
                                           wc(1)= wm(1,1)/wm(1,2)
                                           wc(2)=-wm(2,3)/wm(2,2)
                                           tc=tm(1)/real(wm(1,2)) - tm(2)/real(wm(2,2))
                                           call resolv_sist_1x2(wc,tc,ts1,x1,ix1)
                                           ts(1)=ts1(1)
                                           ts(3)=ts1(2)
                                           x(1) =x1(1)
                                           x(3) =x1(2)
                                           if (ix1(1) == 1) ix(1)=1
                                           if (ix1(1) == 2) ix(1)=3
                                           if (ix1(2) == 1) ix(3)=1
                                           if (ix1(2) == 2) ix(3)=3
                                           if (ix(1) == 0) then
                                              ts(2)=tm(1)/real(wm(1,2)) - ts(1)*wm(1,1)/real(wm(1,2))
                                              x(2)=0.0
                                           else
                                              if (ix(3) == 0) then
                                                 ts(2)=tm(2)/real(wm(2,2)) - ts(3)*wm(2,3)/real(wm(2,2))
                                                 x(2)=0.0
                                              else
                                                 ts(2)=tm(1)/real(wm(1,2))
                                                 x(2) =-real(wm(1,1))/real(wm(1,2))
                                                 ix(2)=1

                                                 ts(3)=tc/real(wc(2))
                                                 x(3) =-real(wc(1))/real(wc(2))
                                                 ix(3)=1
                                              end if
                                           end if

                                        case (2)
                                           wc(1)= wm(1,2)/wm(1,1)
                                           wc(2)=-wm(2,3)/wm(2,1)
                                           tc=tm(1)/real(wm(1,1)) - tm(2)/real(wm(2,1))
                                           call resolv_sist_1x2(wc,tc,ts1,x1,ix1)
                                           ts(2:3)=ts1
                                           x(2:3) =x1
                                           if (ix1(1) == 1) ix(2)=2
                                           if (ix1(1) == 2) ix(2)=3
                                           if (ix1(2) == 1) ix(3)=2
                                           if (ix1(2) == 2) ix(3)=3
                                           if (ix(2) == 0) then
                                              ts(1)=tm(1)/real(wm(1,1)) - ts(2)*wm(1,2)/real(wm(1,1))
                                              x(1)=0.0
                                           else
                                              if (ix(3) == 0) then
                                                 ts(1)=tm(2)/real(wm(2,1)) - ts(3)*wm(2,3)/real(wm(2,1))
                                                 x(1)=0.0
                                              else
                                                 ix(1)=1

                                                 ts(2)=tm(1)/real(wm(1,2))
                                                 x(2) =-real(wm(1,1))/real(wm(1,2))
                                                 ix(2)=1

                                                 ts(3)=tm(2)/real(wm(2,3))
                                                 x(3) =-real(wm(2,1))/real(wm(2,3))
                                                 ix(3)=1
                                              end if
                                           end if
                                     end select
                               end select

                            case (0)
                               select case (i)
                                  case (1)
                                     wc(1)=wm(2,1)
                                     wc(2)=wm(2,2)- wm(2,3)*wm(1,2)/wm(1,3)
                                     tc=tm(2)-tm(1)*wm(2,3)/real(wm(1,3))
                                     call resolv_sist_1x2(wc,tc,ts1,x1,ix1)
                                     ts(1:2)=ts1
                                     x(1:2)=x1
                                     ix(1:2)=ix1
                                     if (ix(2) == 0) then
                                        ts(3)=tm(1)/real(wm(1,3)) - ts(2)*real(wm(1,2))/real(wm(1,3))
                                        x(3)=0.0
                                     else
                                        ix(1)=1

                                        ts(2)=(tm(2) - tm(1)*wm(2,3)/real(wm(1,3))) / &
                                              (real(wm(2,2)) - real(wm(2,3)*wm(1,2))/real(wm(1,3)) )
                                        x(2) =-real(wm(2,1)) / &
                                              (real(wm(2,2)) - real(wm(2,3)*wm(1,2))/real(wm(1,3)) )
                                        ix(2)=1

                                        ts(3)= tm(1)/real(wm(1,3)) - (real(wm(1,2))/real(wm(1,3)))*ts(2)
                                        x(3) =- (real(wm(1,2))/real(wm(1,3)))*x(2)
                                        ix(3)=1
                                     end if

                                  case (2)
                                     wc(1)=wm(2,1)-wm(2,3)*wm(1,1)/wm(1,3)
                                     wc(2)=wm(2,2)
                                     tc=tm(2)-tm(1)*wm(2,3)/real(wm(1,3))
                                     call resolv_sist_1x2(wc,tc,ts1,x1,ix1)
                                    ts(1:2)=ts1
                                    x(1:2)=x1
                                    ix(1:2)=ix1
                                    if (ix(1) == 0) then
                                       ts(3)=tm(1)/real(wm(1,3)) - ts(1)*real(wm(1,1))/real(wm(1,3))
                                       x(3)=0.0
                                    else
                                       ix(1)=1

                                       ts(2)=(tm(2) - tm(1)*wm(2,3)/real(wm(1,3)))/real(wm(2,2))
                                       x(2) =(real(wm(1,1)*wm(2,3))/real(wm(1,3)) - real(wm(2,1)))/real(wm(2,2))
                                       ix(2)=1

                                       ts(3)=tm(1)/real(wm(1,3))
                                       x(3) =-real(wm(1,1))/real(wm(1,3))
                                       ix(3)=1
                                    end if

                                 case (3)
                                    wc(1)=wm(2,1)-wm(1,1)*wm(2,2)/wm(1,2)
                                    wc(2)=wm(2,3)
                                    tc=tm(2)-tm(1)*wm(2,2)/real(wm(1,2))
                                    call resolv_sist_1x2(wc,tc,ts1,x1,ix1)
                                    ts(1)=ts1(1)
                                    ts(3)=ts1(2)
                                    x(1)=x1(1)
                                    x(3)=x1(2)
                                    if (ix1(1) == 1) ix(1)=1
                                    if (ix1(1) == 2) ix(1)=3
                                    if (ix1(2) == 1) ix(3)=1
                                    if (ix1(2) == 2) ix(3)=3
                                    if (ix(1) == 0) then
                                       ts(2)=tm(1)/real(wm(1,2)) - ts(1)*real(wm(1,1))/real(wm(1,2))
                                       x(2)=0.0
                                    else
                                       ix(1) =1

                                       ts(2)=tm(1)/real(wm(1,2))
                                       x(2) =-real(wm(1,1))/real(wm(1,2))
                                       ix(2)=1

                                       ts(3)=(tm(2) - tm(1)*wm(2,2)/real(wm(1,2)))/real(wm(2,3))
                                       x(3) =(real(wm(1,1)*wm(2,2))/real(wm(1,2)) - real(wm(2,1)))/real(wm(2,3))
                                       ix(3)=1
                                    end if
                               end select
                         end select

                      case (0)
                         call resolv_sist_1x3(wm(1,:),tm(1),ts,x,ix)
                   end select

             end select
       end select

    End Subroutine Resolv_Sist_2x3

    !!----
    !!---- RESOLV_SIST_3X3
    !!--<<
    !!----   w11 x1 + w12 x2 + w13 x3 = t1
    !!----   w21 x1 + w22 x2 + w23 x3 = t2
    !!----   w31 x1 + w32 x2 + w33 x3 = t3
    !!----   x_sol(i)= ts(i) + x(i) ix(i)
    !!-->>
    !!---- 04/04/2019
    !!
    Module Subroutine Resolv_Sist_3x3(w,t,ts,x,ix)
       !---- Arguments ----!
       integer, dimension(3,3),          intent(in) :: w       ! Input vector
       real(kind=cp), dimension(3),      intent(in) :: t       ! Input value
       real(kind=cp), dimension(3),      intent(out):: ts      ! Fixed value solution
       real(kind=cp), dimension(3),      intent(out):: x       ! Fixed value for x,y
       integer, dimension(3),            intent(out):: ix      ! 1: x, 2: y, 3: z

       !---- Local variables ----!
       integer                 :: i,j,deter
       integer, dimension(3)   :: fila
       integer, dimension(3,3) :: w1
       integer, dimension(2,3) :: wm
       real(kind=cp)                    :: rnum, rden
       real(kind=cp), dimension(3)      :: t1
       real(kind=cp), dimension(2)      :: tm
       real(kind=cp),dimension(3,3)     :: rw

       !>init
       ts  = 0.0_cp
       x   = 1.0_cp
       ix  = 0

       deter=Determ(w,3)
       rden=real(deter)

       if (deter /= 0) then
          !---- X(1) ----!
          rw=real(w)
          rw(:,1)=t
          rnum=Determ(rw,3)
          ts(1)=rnum/rden

          !---- X(2) ----!
          rw=real(w)
          rw(:,2)=t
          rnum=Determ(rw,3)
          ts(2)=rnum/rden

          !---- X(3) ----!
          rw=real(w)
          rw(:,3)=t
          rnum=Determ(rw,3)
          ts(3)=rnum/rden

          x=0.0

       else                     !  Singular Matrix
          !---- Are there zero rows? ----!
          fila=0
          do i=1,3
             if (all(w(i,:) == 0)) fila(i)=1
          end do
          select case (sum(fila))
             !---- All values are zeros ----!
             case (3)
                if (all(abs(t) < epss)) then
                   do i=1,3
                      ix(i)=i
                   end do
                else
                   err_cfml%ierr=1
                   err_cfml%msg="MATHS@RESOLV_SIST_3x3: Inconsistent system (3 x 3)"
                end if

             !---- Two rows with zeroes ----!
             case (2)
                do i=1,3
                   if (fila(i) == 0) exit
                end do
                call resolv_sist_1x3(w(i,:),t(i),ts,x,ix)

             !---- One row with zeroes ----!
             case (1)
                do i=1,3
                   if (fila(i) == 1) exit
                end do
                select case(i)
                   case (1)
                      wm(1,:)=w(2,:)
                      wm(2,:)=w(3,:)
                      tm=t(2:3)

                   case (2)
                      wm(1,:)=w(1,:)
                      wm(2,:)=w(3,:)
                      tm(1)=t(1)
                      tm(2)=t(3)

                   case (3)
                      wm(1,:)=w(1,:)
                      wm(2,:)=w(2,:)
                      tm=t(1:2)

                end select
                call resolv_sist_2x3(wm,tm,ts,x,ix)

             !---- Non zero rows ----!
             case (0)
                w1=w
                t1=t

                !---- Are there 2 rows proportional? ----!
                do i=1,3
                   if ( abs(w1(1,i)) > abs(w1(2,i)) ) then
                      if (w1(2,i) /= 0) then
                         j=w1(1,i)/w1(2,i)
                      else
                         j=0
                      end if
                      if (j /= 0) then
                         if (j*w1(2,1) == w1(1,1) .and. j*w1(2,2) == w1(1,2) .and. &
                             j*w1(2,3) == w1(1,3) ) then
                            w1(1,:)=w1(2,:)
                            t1(1)  =t1(2)
                            exit
                         end if
                      end if
                   else
                      if (w1(1,i) /= 0) then
                         j=w1(2,i)/w1(1,i)
                      else
                         j=0
                      end if
                      if (j /= 0) then
                         if (j*w1(1,1) == w1(2,1) .and. j*w1(1,2) == w1(2,2) .and. &
                             j*w1(1,3) == w1(2,3) ) then
                            w1(2,:)=w1(1,:)
                            t1(2)  =t1(1)
                            exit
                         end if
                      end if
                   end if
                end do

                do i=1,3
                   if ( abs(w1(1,i)) > abs(w1(3,i)) ) then
                      if (w1(3,i) /= 0) then
                         j=w1(1,i)/w1(3,i)
                      else
                         j=0
                      end if
                      if (j /= 0) then
                         if (j*w1(3,1) == w1(1,1) .and. j*w1(3,2) == w1(1,2) .and. &
                             j*w1(3,3) == w1(1,3) ) then
                            w1(1,:)=w1(3,:)
                            t1(1)  =t1(3)
                            exit
                         end if
                      end if
                   else
                      if (w1(1,i) /= 0) then
                         j=w1(3,i)/w1(1,i)
                      else
                         j=0
                      end if
                      if (j /= 0) then
                         if (j*w1(1,1) == w1(3,1) .and. j*w1(1,2) == w1(3,2) .and. &
                             j*w1(1,3) == w1(3,3) ) then
                            w1(3,:)=w1(1,:)
                            t1(3)  =t1(1)
                            exit
                         end if
                      end if
                   end if
                end do

                do i=1,3
                   if ( abs(w1(2,i)) > abs(w1(3,i)) ) then
                      if (w1(3,i) /= 0) then
                         j=w1(2,i)/w1(3,i)
                      else
                         j=0
                      end if
                      if (j /= 0) then
                         if (j*w1(3,1) == w1(2,1) .and. j*w1(3,2) == w1(2,2) .and. &
                             j*w1(3,3) == w1(2,3) ) then
                            w1(2,:)=w1(3,:)
                            t1(2)  =t1(3)
                            exit
                         end if
                      end if
                   else
                      if (w1(2,i) /= 0) then
                         j=w1(3,i)/w1(2,i)
                      else
                         j=0
                      end if
                      if (j /= 0) then
                         if (j*w1(2,1) == w1(3,1) .and. j*w1(2,2) == w1(3,2) .and. &
                             j*w1(2,3) == w1(3,3) ) then
                            w1(3,:)=w1(2,:)
                            t1(3)  =t1(2)
                            exit
                         end if
                      end if
                   end if
                end do

                !---- Are there 3 rows equal? ----!
                if ( (w1(1,1) == w1(2,1)) .and. (w1(1,1) == w1(3,1)) .and. &
                     (w1(1,2) == w1(2,2)) .and. (w1(1,2) == w1(3,2)) .and. &
                     (w1(1,3) == w1(2,3)) .and. (w1(1,3) == w1(3,3)) ) then

                   call resolv_sist_1x3(w1(1,:),t1(1),ts,x,ix)

                !---- Are there 2 rows equal? ----!
                elseif( (w1(1,1) == w1(2,1)) .and. (w1(1,2) == w1(2,2)) .and. &
                        (w1(1,3) == w1(2,3)) ) then

                   call resolv_sist_2x3(w1(2:3,:),t1(2:3),ts,x,ix)

                elseif( (w1(1,1) == w1(3,1)) .and. (w1(1,2) == w1(3,2)) .and. &
                        (w1(1,3) == w1(3,3)) ) then

                   call resolv_sist_2x3(w1(1:2,:),t1(1:2),ts,x,ix)

                elseif( (w1(2,1) == w1(3,1)) .and. (w1(2,2) == w1(3,2)) .and. &
                        (w1(2,3) == w1(3,3)) ) then

                   call resolv_sist_2x3(w1(1:2,:),t1(1:2),ts,x,ix)

                !---- Are linear combinations? ----!
                else
                   call resolv_sist_2x3(w1(1:2,:),t1(1:2),ts,x,ix)

                end if

          end select
       end if

    End Subroutine Resolv_Sist_3x3

End Submodule Maths_Resolv_System
