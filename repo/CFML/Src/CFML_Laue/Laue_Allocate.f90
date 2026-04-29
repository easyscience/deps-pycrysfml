!!----
!!----
!!----
SubModule (CFML_Laue) Laue_Allocate

    implicit none

    Contains

    Module Subroutine Allocate_Laue_Ref_list(L,n,d)

        ! Arguments
        class(Laue_Ref_List_Type), intent (in out) :: L
        integer,                   intent (in)     :: n
        integer, optional,         intent (in)     :: d !number of propagation vectors

        ! Local variables
        integer :: i

        if (allocated(L%LR)) deallocate (L%LR)
        allocate(L%LR(n))
        L%nref=n
        do i=1,n
            L%LR(i)%stats   = 0
            L%LR(i)%mult    = 0
            L%LR(i)%nodal   = 0
            L%LR(i)%ds      = 0.0
            L%LR(i)%gamma   = 0.0
            L%LR(i)%nu      = 0.0
            L%LR(i)%Ttheta  = 0.0
            L%LR(i)%Lambda  = 0.0
            L%LR(i)%h       = 0.0
            L%LR(i)%zv      = 0.0
            L%LR(i)%Obs_int = 0.0
            L%LR(i)%Cal_int = 0.0
            L%LR(i)%Sigma   = 0.0
            L%LR(i)%x       = 0.0
            L%LR(i)%z       = 0.0
            L%LR(i)%stheta  = 0.0
            L%LR(i)%sphi    = 0.0
            L%LR(i)%kindex  = 0
            if(present(d)) then
                if(allocated (L%LR(i)%hs)) deallocate(L%LR(i)%hs)
                allocate(L%LR(i)%hs(d))
                L%LR(i)%hs(:) = 0
            end if
        end do

    End Subroutine Allocate_Laue_Ref_list

End Submodule Laue_Allocate