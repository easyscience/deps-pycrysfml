!!----
!!---- SUBMODULE CFML_Math_General
!!----
!!----
!!
Submodule (CFML_Maths) Math_Zbelong
 implicit none
 Contains

    !!----
    !!---- ZBELONG_M
    !!----    Determines if a real array is an Integer matrix
    !!----
    !!---- 28/03/2019
    !!
    Pure Module Function Zbelong_M(A) Result(belong)
       !---- Argument ----!
       real(kind=cp),   dimension(:,:), intent( in) :: a        ! Input array
       logical                                      :: belong

       !---- Local variables ----!
       real(kind=cp),   dimension(size(a,1),size(a,2)) :: vec

       vec= abs(real(nint (a))-a)
       belong=.not. ANY(vec > epss)

    End Function Zbelong_M

    !!----
    !!---- ZBELONG_R
    !!----    Determines if a real number is an Integer
    !!----
    !!---- 28/03/2019
    !!
    Pure Module Function Zbelong_R(x) Result(belong)
       !---- Argument ----!
       real(kind=cp), intent( in) :: x              ! Input number
       logical                    :: belong

       belong=.false.
       if (abs(real(nint (x))-x) > epss) return

       belong=.true.

    End Function Zbelong_R

    !!----
    !!---- ZBELONG_V
    !!----    Determines if a real vector is an Integer vector
    !!----
    !!---- 28/03/2019
    !!
    Pure Module Function Zbelong_V(v) Result(belong)
       !---- Argument ----!
       real(kind=cp),   dimension(:), intent( in) :: v      ! Input vector
       logical                                    :: belong

       !---- Local variables ----!
       integer                             :: i
       real(kind=cp),   dimension(size(v)) :: vec

       belong=.false.

       vec= abs(real(nint (v))-v)
       do i=1,size(v)
          if (vec(i) > epss) return
       end do
       belong=.true.

    End Function Zbelong_V

End Submodule Math_Zbelong
