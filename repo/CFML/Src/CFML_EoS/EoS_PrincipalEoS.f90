!!----
!!----
!!----
SubModule (CFML_EoS) EoS_PrinEoS
   implicit none
   Contains

   !!----
   !!---- FUNCTION PRINCIPAL_EOS
   !!----
   !!----  For use with eos_cell_type: Converts request for recip axis to real axis, if allowed
   !!----
   !!---- Date: 09/09/2020
   !!
   Module Function Principal_EoS(Cell_Eos, I) Result(Ieos)
      !---- Arguments ----!
      type(eos_cell_type),intent(in) :: cell_eos !eos for cells
      integer,            intent(in) :: i        !proposed axis number
      integer                        :: ieos

      !---- Local Variables ----!

      select case(i)
         case(:-3,-1,7:)
            ieos=-1                   ! error

         case(-2,0:3)                 ! general dir, V a b c
            ieos=i

         case(4:6)
            select case(U_case(cell_eos%system(1:4)))
               case('ISOT','CUBI','TETR','ORTH')
                  ieos=i-3

               case('HEXA','TRIG')       !onl d(001) is equivalent to an axis in length
                  if (i == 6)then
                     ieos=3
                  else
                     ieos=-2
                  end if

               case('MONO')
                  if (i-3 == cell_eos%unique)then
                     ieos=cell_eos%unique
                  else
                     ieos=-2
                  end if

               case('TRIC')
                  ieos=i
            end select
      end select
   End Function Principal_EoS

End SubModule EoS_PrinEoS