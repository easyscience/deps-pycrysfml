Submodule (CFML_Molecules) Mol_IndexList

   implicit none

 Contains

   !!----
   !!----
   !!----
   !!----
   !!----
   !!----  June 2023
   !!
   Module Function Index_AtLab_on_Molecule(Lab, Mol) Result(Ind)
      !---- Arguments ----!
      character(len=*),    intent(in) :: Lab
      type(molecule_type), intent(in) :: Mol
      integer                         :: Ind

      !---- Local Variables ----!
      integer :: i

      !> Init
      Ind=0

      do i=1,Mol%natoms
         if (trim(lab) == trim(Mol%AtName(i))) then
            ind=i
            exit
         end if
      end do

   End Function Index_AtLab_on_Molecule

End Submodule Mol_IndexList