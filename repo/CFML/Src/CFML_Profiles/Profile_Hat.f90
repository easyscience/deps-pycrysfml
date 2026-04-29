!!----
!!----
!!----
!!
SubModule (CFML_Profiles) Profile_Hat
  implicit none
   Contains
   !!----
   !!---- FUNCTION HAT
   !!----
   !!----
   !!---- 21/04/2019
   !!
   Pure Module Function Hat(X,Par) Result (H_Val)
      !---- Arguments ----!
      real(kind=cp),              intent(in) :: x
      real(kind=cp), dimension(:),intent(in) :: par
      real(kind=cp)                          :: h_val

      !--- Local variables ---!
      real :: hw

      hw=par(1)*0.5

      if (x < -hw .or. x > hw) then
         h_val=0.0
      else
         h_val =1.0/par(1)
      end if

   End Function Hat

   !!----
   !!----  HAT_DER
   !!----
   !!---- 21/04/2019
   !!
   Pure Module Subroutine Hat_Der(X,Par,H_Val,Dpar)
      !---- Arguments ----!
      real(kind=cp),                        intent(in)  :: x
      real(kind=cp),           dimension(:),intent(in)  :: par
      real(kind=cp),                        intent(out) :: h_val
      real(kind=cp), optional, dimension(:),intent(out) :: dpar

      !--- Local variables ---!
      real(kind=cp) :: hw

      hw=par(1)*0.5

      if (x < -hw .or. x > hw) then
         h_val=0.0_cp
         if (present(dpar)) then
            dpar(1:2) = [0.0_cp,0.0_cp]
         end if
      else
         h_val =1.0/par(1)
         if (present(dpar)) then
            dpar(1:2) = [0.0_cp,-1.0_cp/(par(1)*par(1))]
         end if
      end if

   End Subroutine Hat_Der


End SubModule Profile_Hat