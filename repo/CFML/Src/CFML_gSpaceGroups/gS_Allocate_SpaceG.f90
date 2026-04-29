!!----
!!----
!!----
!!
SubModule (CFML_gSpaceGroups) gS_Allocate_SpaceG
   implicit none
   Contains

   !!----
   !!---- Allocate_SpaceGroup
   !!----
   !!---- 19/04/19
   !!
   Module Subroutine Allocate_SpaceGroup(D, Multip, Grp)
      !---- Arguments ----!
      integer,         intent(in)     :: d,multip
      class(Spg_Type), intent(in out) :: Grp

      !---- Local Variables ----!
      integer :: i

      !> Init
      if (allocated(Grp%Op)) deallocate(Grp%Op)
      allocate(Grp%Op(multip))

      Grp%d=d
      Grp%multip=multip
      do i=1,multip
         call Allocate_Op(d,Grp%Op(i))
      end do

      if(allocated(Grp%Symb_Op)) deallocate(Grp%Symb_Op)
      allocate(Grp%Symb_Op(multip))

      if(allocated(Grp%inv)) deallocate(Grp%inv)
      allocate(Grp%inv(multip))
      Grp%inv = 0

      if(allocated(Grp%centre_coord)) deallocate(Grp%centre_coord)
      allocate(Grp%centre_coord(d-1))
      Grp%centre_coord=0_LI

      if(allocated(Grp%anticentre_coord)) deallocate(Grp%anticentre_coord)
      allocate(Grp%anticentre_coord(d-1))
      Grp%anticentre_coord=0_LI

   End Subroutine Allocate_SpaceGroup

   Module Subroutine Allocate_KVector(nk, nq, Kvec)
      !---- Arguments ----!
      integer,               intent(in)    :: nk  ! Number of independent k-vectors
      integer,               intent(in)    :: nq  !number of effective set of Q_coeff > nk
      type(Kvect_Info_Type), intent(in out):: Kvec

      !> Init
      if (nk == 0 .and. nq ==0) then
         Kvec%nk=0; Kvec%nq=0
         if (allocated(Kvec%kv))      deallocate(kvec%kv)
         if (allocated(Kvec%kv_std))  deallocate(kvec%kv_std)
         if (allocated(Kvec%sintlim)) deallocate(kvec%sintlim)
         if (allocated(Kvec%nharm))   deallocate(kvec%nharm)
         if (allocated(Kvec%Q_Coeff)) deallocate(kvec%Q_Coeff)
         return
      end if

      if (nk > 0) then
         !if (nk /= Kvec%nk) then
            if (allocated(Kvec%kv))      deallocate(kvec%kv)
            if (allocated(Kvec%kv_std))  deallocate(kvec%kv_std)
            if (allocated(Kvec%sintlim)) deallocate(kvec%sintlim)
            if (allocated(Kvec%nharm))   deallocate(kvec%nharm)
            allocate(Kvec%kv(3,nk))
            allocate(Kvec%kv_std(3,nk))
            allocate(Kvec%sintlim(nk))
            allocate(Kvec%nharm(nk))
         !end if
         Kvec%nk=nk
         Kvec%kv=0.0_cp
         Kvec%kv_std=0.0_cp
         Kvec%sintlim=1.0_cp
         Kvec%nharm=0
      end if

      if (nq > 0 .and. Kvec%nk > 0) then
         if (nq /= Kvec%nq) then
            if (allocated(Kvec%Q_Coeff)) deallocate(kvec%Q_Coeff)
            allocate(Kvec%q_coeff(Kvec%nk,nq))
         end if
         Kvec%nq=nq
         Kvec%q_coeff=0
      end if

   End Subroutine Allocate_KVector

End SubModule gS_Allocate_SpaceG

