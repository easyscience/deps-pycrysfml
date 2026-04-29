!!----
!!---- SUBMODULE CFML_Math_General
!!----
!!----
!!
Submodule (CFML_Maths) Maths_Swap
 implicit none
 Contains

    !!----
    !!---- SWAP_C
    !!----    Swap the contents of a and b
    !!----
    !!---- 29/03/2019
    !!
    Elemental Module Subroutine Swap_C(a,b)
       !---- Arguments ----!
       complex(kind=cp), intent(in out) :: a
       complex(kind=cp), intent(in out) :: b

       !---- Local variables ----!
       complex(kind=cp) :: dum

       dum=a
       a=b
       b=dum

    End Subroutine Swap_C

    !!----
    !!---- SWAP_I
    !!----    Swap the contents of a and b
    !!----
    !!---- 29/03/2019
    !!
    Elemental Module Subroutine Swap_I(A,B)
       !---- Arguments ----!
       integer , intent(in out) :: a
       integer , intent(in out) :: b

       !---- Local variables ----!
       integer  :: dum

       dum=a
       a=b
       b=dum

    End Subroutine Swap_I

    !!----
    !!---- SWAP_R
    !!----    Swap the contents of a and b
    !!----
    !!---- 29/03/2019
    !!
    Elemental Module Subroutine Swap_R(A,B)
       !---- Arguments ----!
       real(kind=cp), intent(in out) :: a
       real(kind=cp), intent(in out) :: b

       !---- Local variables ----!
       real(kind=cp) :: dum

       dum=a
       a=b
       b=dum

    End Subroutine Swap_R

    !!----
    !!---- SWAP_MASKED_C
    !!----    Swap the contents of a and b if mask=.true.
    !!----
    !!---- 29/03/2019
    !!
    Elemental Module Subroutine Swap_Masked_C(A,B,Mask)
       !---- Arguments ----!
       complex(kind=cp), intent(in out) :: a
       complex(kind=cp), intent(in out) :: b
       logical,       intent(in)     :: mask

       !---- Local Variables ----!
       complex(kind=cp) :: swp

       if (mask) then
          swp=a
          a=b
          b=swp
       end if

    End Subroutine Swap_Masked_C

    !!----
    !!---- SWAP_MASKED_I
    !!----    Swap the contents of a and b if mask=.true.
    !!----
    !!---- 29/03/2019
    !!
    Elemental Module Subroutine Swap_Masked_I(A,B,Mask)
       !---- Arguments ----!
       integer, intent(in out) :: a
       integer, intent(in out) :: b
       logical, intent(in)     :: mask

       !---- Local Variables ----!
       integer :: swp

       if (mask) then
          swp=a
          a=b
          b=swp
       end if

    End Subroutine Swap_Masked_I

    !!----
    !!---- SWAP_MASKED_R
    !!----    Swap the contents of a and b if mask=.true.
    !!----
    !!---- 29/03/2019
    !!
    Elemental Module Subroutine Swap_Masked_R(A,B,Mask)
       !---- Arguments ----!
       real(kind=cp), intent(in out) :: a
       real(kind=cp), intent(in out) :: b
       logical,       intent(in)     :: mask

       !---- Local Variables ----!
       real(kind=cp) :: swp

       if (mask) then
          swp=a
          a=b
          b=swp
       end if

    End Subroutine Swap_Masked_R

End Submodule Maths_Swap
