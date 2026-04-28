!!----
!!----
!!----
!!
SubModule (CFML_gSpaceGroups) gS_Get_Cosets
   implicit none
   Contains
   !!----
   !!---- GET_COSETS
   !!----
   !!---- 20/04/19
   !!
   Module Subroutine Get_Cosets(G, H, cosets)
      !---- Arguments ----!
      class(Spg_Type),                     intent(in)  :: G  ! Group G > H
      class(Spg_Type),                     intent(in)  :: H  ! Subgroup of G
      integer, dimension(:), allocatable,  intent(out) :: cosets

      !---- Local Variables ----!
      integer                      :: i,j,k,n,m
      character(len=80)            :: OpSymb
      type(Symm_Oper_Type)         :: Op
      integer, dimension(G%multip) :: ind
      logical, dimension(G%multip) :: done
      logical                      :: ncent_assigned

      !> Init
      n=0; done=.false.
      call Allocate_Op(G%d,Op)

      do_G: do i=2, G%multip
         if (done(i)) cycle
         OpSymb=G%Symb_Op(i)
         do j=2,H%multip
            if (trim(OpSymb) == trim(H%Symb_Op(j)) ) cycle do_G
         end do
         n=n+1
         ind(n)=i
         ncent_assigned=.false.

         !> Remove the new operators in aH from the list to be considered
         do k=2,H%multip
            Op=G%Op(i)*H%Op(k)
            do m=2,G%multip
               if (Op == G%Op(m)) then
                  done(m)=.true.

                  if (.not. ncent_assigned) then
                     if (Is_OP_Inversion_Centre(G%Op(m))) then
                        ind(n)=m
                        ncent_assigned=.true.
                        exit
                     end if
                  end if
                  exit
               end if
            end do
         end do
      end do do_G

      if(allocated(cosets)) deallocate(cosets)
      allocate(cosets(n))
      cosets=ind(1:n)
   End Subroutine Get_Cosets

End SubModule gS_Get_Cosets

