!!----
!!----
!!----
!!
SubModule (CFML_Profiles) Profile_Exponential
  implicit none
   Contains
   !!----
   !!---- EXPONENTIAL
   !!----
   !!----
   !!---- 21/04/2019
   !!
   Pure Module Function Exponential(X,Par) Result (Ex_Val)
      !---- Arguments ----!
      real(kind=cp),              intent(in) :: x
      real(kind=cp), dimension(:),intent(in) :: par
      real(kind=cp)                          :: ex_val

      !--- Local variables ---!
      real(kind=cp) :: alfa

      if (x < 0.0) then
         ex_val=0.0
      else
         alfa=par(1)
         ex_val = alfa*exp(-alfa*x)
      end if

   End Function Exponential

   !!----
   !!---- EXPONENTIAL_DER
   !!----
   !!---- 21/04/2019
   !!
   Pure Module Subroutine Exponential_Der(X,Par,Ex_Val,Dpar)
      !---- Arguments ----!
      real(kind=cp),                        intent(in) :: x
      real(kind=cp),           dimension(:),intent(in) :: par
      real(kind=cp),                        intent(out):: ex_val
      real(kind=cp), optional, dimension(:),intent(out):: dpar

      !--- Local variables ---!
      real(kind=cp) :: alfa

      if (x < 0.0) then
         ex_val=0.0
         if (present(dpar)) then
            dpar(1:2) = (/0.0,0.0/)
         end if
      else
         alfa=par(1)
         ex_val = alfa*exp(-alfa*x)
         if (present(dpar)) then
            dpar(1:2) = ex_val*(/-alfa, 1.0/alfa - x/)   !derX, derAlfa
         end if
      end if

   End Subroutine Exponential_Der

End SubModule Profile_Exponential