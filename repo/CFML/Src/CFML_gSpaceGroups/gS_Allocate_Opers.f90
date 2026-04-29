SubModule (CFML_gSpaceGroups) gS_Allocate_Opers
   implicit none

   Contains

   !!----
   !!---- ALLOCATE_OP
   !!----
   !!---- 19/04/19
   !!
   Module Subroutine Allocate_OP(d, Op)
      !---- Arguments ----!
      integer,              intent(in)     :: d
      type(Symm_Oper_Type), intent(in out) :: Op


      if (allocated(Op%Mat)) deallocate(Op%Mat)
      allocate(Op%Mat(d,d))

      !>Inititalize to identity matrix
      call Set_Identity_Matrix(d)

      Op%Mat=Identity_Matrix
      Op%time_inv=1
      Op%dt=1
   End Subroutine Allocate_OP

   !!----
   !!---- ALLOCATE_OPERATORS
   !!----
   !!----  Multip is the expected maximum number of operators
   !!----
   !!---- 25/04/2019
   !!
   Module Subroutine Allocate_Operators(D, Nmax, Op)
      !---- Arguments ----!
      integer,                                         intent(in)     :: d     ! Dimension
      integer,                                         intent(in)     :: Nmax  ! is the expected maximum number of operators
      type(Symm_Oper_Type), dimension(:), allocatable, intent(in out) :: Op

      !---- Local Variables ----!
      integer :: i

      !> Init
      if(allocated(Op)) deallocate(Op)
      allocate(Op(NMax))

      do i=1, NMax
         call Allocate_Op(d,Op(i))
      end do
   End Subroutine Allocate_Operators

End SubModule gS_Allocate_Opers