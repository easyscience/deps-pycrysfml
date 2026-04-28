!!----
!!----
!!----
SubModule (CFML_EoS) EoS_CopyEDat
   implicit none
   Contains

   !!----
   !!---- COPY_EOS_DATA_LIST
   !!----
   !!----
   !!---- Date: 03/02/2021
   !!---- Revision: OK
   !!
   Module Subroutine Copy_Eos_Data_List(Dat1, Dat2)
      !---- Arguments ----!
      type (eos_data_list_type), intent(in)      :: Dat1  ! Object to be copied
      type (eos_data_list_type), intent(in out)  :: Dat2  ! Output copy

      if (allocated(dat2%eosd))then
         call Deallocate_EoS_Data_List(dat2)
      end if

      call Allocate_EoS_Data_List(dat1%N, dat2)
      dat2=dat1

   End Subroutine Copy_Eos_Data_List

End SubModule EoS_CopyEDat