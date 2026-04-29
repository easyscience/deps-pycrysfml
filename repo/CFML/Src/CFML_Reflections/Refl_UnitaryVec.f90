!!----
!!----
!!----
SubModule (CFML_Reflections) Refl_UnitaryVec
   implicit none
   Contains
   !!----
   !!---- UNITARY_VECTOR_H
   !!----    Calculate a unitary vector in the cartesian crystal
   !!----    frame along a reciprocal vector hkl (reciprocal lattice)
   !!----
   !!---- 21/06/2019
   !!
   Module Function Unitary_Vector_H(H, Cell) Result (U)
      !---- Arguments ----!
      integer, dimension(3), intent(in) :: H
      class(Cell_G_Type),    intent(in) :: Cell
      real(kind=cp), dimension(3)       :: U

      !---- Local Variables ----!
      real(kind=cp), dimension(3)       :: v

      v=matmul(Cell%GR,real(h))     ![L-2]
      u=matmul(Cell%Cr_Orth_cel,v)  ![L-1]
      u=u/sqrt(u(1)*u(1)+u(2)*u(2)+u(3)*u(3))
   End Function Unitary_Vector_H

End SubModule Refl_UnitaryVec
