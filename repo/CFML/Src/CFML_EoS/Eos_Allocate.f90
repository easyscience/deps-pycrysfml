!!----
!!----
!!----
SubModule (CFML_EoS) EoS_Allocate
   implicit none
   Contains

   !!----
   !!---- ALLOCATE_EOS_DATA_LIST
   !!----    Allocation of objet E of eos_list_data_type.
   !!----    This subroutine should be called before using an object of type eos_data_list.
   !!----
   !!---- 17/07/2015
   !!---- Revision: OK
   !!
   Module Subroutine Allocate_EoS_Data_List(N, E)
      !---- Arguments ----!
      integer,                    intent(in)       :: N  ! Number of elements of E
      type (eos_data_list_type),  intent(in out)   :: E  ! Objet to be allocated

      !---- Local Variables ----!
      integer :: i,ier

      !> Set dimension
      e%n = n
      if (allocated(e%eosd)) deallocate(e%eosd)

      allocate (e%eosd(n),stat=ier)
      if (ier /= 0) then
         e%n = 0
         call set_error(1,"Problems allocating memory for Eos_Data_List_Type variable")
         return
      end if

      !> Initializing
      do i=1,n
         call init_eos_data_type(e%eosd(i))
      end do

   End Subroutine Allocate_EoS_Data_List

   !!----
   !!---- ALLOCATE_EOS_LIST
   !!----    Allocation of objet E of eos_list_type.
   !!----    This subroutine should be called before using an object of type eos_list.
   !!----
   !!---- 17/07/2015
   !!---- Revision: OK
   !!
   Module Subroutine Allocate_EoS_List(N, E)
      !---- Arguments ----!
      integer,               intent(in)       :: N  ! Number of elements of E
      type (eos_list_type),  intent(in out)   :: E  ! Objet to be allocated

      !---- Local Variables ----!
      integer :: i,ier

      !> Set dimension
      e%n = n
      if (allocated(e%eos)) deallocate(e%eos)

      allocate (e%eos(n),stat=ier)
      if (ier /= 0) then
         e%n = 0
         call set_error(1,"Problems allocating memory for Eos_List_Type variable")
         return
      end if

      !> Initializing
      do i=1,n
         call init_eos_type(e%eos(i))
      end do

   End Subroutine Allocate_EoS_List

   !!----
   !!---- DEALLOCATE_EOS_DATA_LIST
   !!----    De-allocation of objet E of type eos_data_list.
   !!----    This subroutine should be after using an object of type eos_data_list that is no
   !!----    more needed.
   !!----
   !!---- 17/07/2015
   !!---- Revision: OK
   !!
   Module Subroutine Deallocate_EoS_Data_List(E)
      !---- Arguments ----!
      type (eos_data_list_type), intent(in out)   :: E  ! Objet to be deallocated

      if (allocated(E%eosd)) deallocate (E%eosd)
      E%n=0

   End Subroutine Deallocate_EoS_Data_List

   !!----
   !!---- DEALLOCATE_EOS_LIST
   !!----    De-allocation of objet E of type eos_list.
   !!----    This subroutine should be after using an object of type eos_list that is no
   !!----    more needed.
   !!----
   !!---- 17/07/2015
   !!---- Revision: OK
   !!
   Module Subroutine Deallocate_EoS_List(E)
      !---- Arguments ----!
      type (eos_list_type), intent(in out)   :: E  ! Objet to be deallocated

      if (allocated(E%eos)) deallocate (E%eos)
      E%n=0

   End Subroutine Deallocate_EoS_List

End SubModule EoS_Allocate