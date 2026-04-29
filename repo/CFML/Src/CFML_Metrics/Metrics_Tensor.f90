Submodule (CFML_Metrics) Metrics_Tensor
 Implicit none
 Contains
   !!----
   !!---- Subroutine Init_Strain_Tensor
   !!----
   !!---- Update: 19/01/2021
   !!
   Module Subroutine Init_Strain_Tensor(T)
      !---- Argument ----!
      type(Strain_Tensor_Type), intent(in out) :: T

      t%iref=0
      t%icell=0
      t%istype=0
      t%cartype='  '
      t%system=' '

      t%e  =0._cp
      t%esd=0._cp
      t%ep =0._cp
      t%esdp=0._cp
      t%property=''

   End Subroutine Init_Strain_Tensor

   !!----
   !!---- Subroutine Fix_Tensor
   !!----
   !!----    Makes the second rank tensor 'a' conform to crystal system symmetry
   !!----    Assuming that in higher symmetries the Cart axes are parallel to crystal axes
   !!----
   !!---- 19/01/2021
   !!
   Module Subroutine Fix_Tensor(A,Sys_In)
      !---- Arguments ----!
      real(kind=cp), dimension(3,3), intent(in out) :: A
      character(len=*),              intent(in)    :: Sys_in

      !---- Local Variables ----!
      character(len=len(sys_in)) :: sys

      !> process system
      sys=U_case(adjustl(sys_in))

      select case(sys(1:4))
         case ('TRIC','MONO')         ! cannot set mono off-diagonal terms zero, because we do not know Cart setting
            a(1,2)=(a(1,2)+a(2,1))/2.0
            a(1,3)=(a(1,3)+a(3,1))/2.0
            a(2,3)=(a(2,3)+a(3,2))/2.0

         case ('ORTH')
            a(1,2)=0._cp
            a(1,3)=0._cp
            a(2,3)=0._cp

         case ('TETR','TRIG','HEXA')
            a(1,1)=(a(1,1)+a(2,2))/2.0
            a(2,2)=a(1,1)
            a(1,2)=0._cp
            a(1,3)=0._cp
            a(2,3)=0._cp

         case ('CUBI')
            a(1,1)=(a(1,1)+a(2,2)+a(3,3))/3.0
            a(2,2)=a(1,1)
            a(3,3)=a(1,1)
            a(1,2)=0._cp
            a(1,3)=0._cp
            a(2,3)=0._cp

      end select

      !> make symmetric: for all systems
      a(2,1)=a(1,2)
      a(3,1)=a(1,3)
      a(3,2)=a(2,3)

   End Subroutine Fix_Tensor

End Submodule Metrics_Tensor