!!----
!!----
!!----
!!
SubModule (CFML_gSpaceGroups) gS_Sort_Operator
   implicit none
   Contains

   !!----
   !!---- SORT_OP
   !!----
   !!---- Order operators in such a way as putting those having positive determinant in
   !!---- top of the list when cod /= "tim". If cod="tim", the list of primed elements
   !!---- are put at the bottom of the list. This is useful for Type 4 groups having
   !!---- centring anti-translations because in such a case the primed elements are obtained
   !!---- from the non-primed by multiplication by the anti-translation operators
   !!----
   !!---- 20/04/19
   !!
   Module Subroutine Sort_Oper(N, Op, Cod, perm)
      !---- Arguments ----!
      integer,                           intent(in)     :: n
      type(Symm_Oper_Type),dimension(n), intent(in out) :: Op
      character(len=*),                  intent(in)     :: cod
      integer, optional,   dimension(n), intent(out)    :: perm

      !---- Local Variables ----!
      type(Symm_Oper_Type)  :: Ops
      integer               :: i, j, k, opso
      integer, dimension(n) :: option, per

      !> Init
      per=[(i,i=1,n)]
      if (cod == "tim") then
         option=Op(:)%time_inv
      else
         option=Op(:)%dt
      end if

      do i=2, n
         Ops = Op(i)
         opso= Ops%dt
         k=per(i)
         if (cod == "tim") opso= Ops%time_inv
         j = i - 1
         do while (j >= 1)
            if (option(j) >= opso) exit
            Op(j + 1) = Op(j)
            option(j + 1) =  option(j)
            per(j+1)=per(j)
            j = j - 1
         end do
         Op(j + 1) = Ops
         option(j+1) = opso
         per(j+1)=k
      end do
      if(present(perm)) perm=per
   End Subroutine Sort_Oper

End SubModule gS_Sort_Operator

