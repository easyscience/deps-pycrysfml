!!----
!!----
!!----
SubModule (CFML_EoS) EoS_Get_Tensor
   implicit none
   Contains

   !!----
   !!---- GET_TENSOR_EOS
   !!----
   !!---- Returns the value of alpha or beta tensor calculated from eos of cell and
   !!---- its eigen vectors etc
   !!----
   !!---- Date: 22/10/2020
   !!---- Revision: OK
   !!
   Module Subroutine Get_Tensor_Eos(P, T, Cell_Eos, Dx, X)
      !---- Arguments ----!
      real(kind=cp),           intent(in)     :: p,T
      type(eos_cell_type),     intent(in)     :: cell_eos
      character(len=1),        intent(in)     :: dx  ! ='T' for alpha or 'P' for beta
      type(Strain_Tensor_Type),intent(in out) :: x   ! only 'in' is cartype, this routine loads system and property and paxis angles

      !---- Local Variables ----!
      integer                     :: i
      real(kind=cp)               :: dr,cotbs,cotgs
      real(kind=cp),dimension(3)  :: d,da ! for derivatives 1/a. da/dP and dangle/dP
      type(axis_type)             :: axis
      type(cell_g_type)           :: c  !cell params, metric tensor at this P,T
      character(len=2)            :: cartype  !local copy
      character(len=2)            :: dtype  !local copy of P or T

      !> Init
      cartype=x%cartype
      !call init_strain_tensor(x) !This routine doesn't exist
      x%cartype=U_case(cartype)
      x%system=cell_eos%system
      dtype='P'
      if (U_case(dx) == 'T') dtype='T'

      !> Calculate cell edge compressibilities
      do i = 1,3
         axis%ieos=i
         if (dtype == 'P')then
            d(i)=-1.0_cp/Get_Mod_Cell(P,T,cell_eos,axis)        !d(1) is 1/a . da/dP: the negative of the compressibility
         else
            d(i)=Get_alpha_cell(P,T,cell_eos,axis)
         end if
      end do

      !> now do triclinic or monoclinic
      if (index(U_case(x%system),'TRIC') > 0 .or. index(U_case(x%system),'MONO') > 0)then
         !> First calculate the cell params and recip cell at this point
         c=  get_params_cell(P,T,cell_eos)
         do i=1,3
            da(i)=get_angle_deriv(P,T,cell_eos,i,.true.,dtype)
         end do

         select case(x%cartype)
            case('BC')   !cartype=2: Redfern & Carpenter Y // b and Z //c*
               dr=get_angle_deriv(P,T,cell_eos,2,.false.,dtype)   !d(beta*)/dP;  needed if beta changing but is 90.0
               !tensor coeffs if beta*=90
               x%ep(1,1)=d(1) +da(3)/tand(c%ang(3))
               x%ep(2,2)=d(2)
               x%ep(3,3)=d(3) +da(1)/tand(c%ang(1))
               x%ep(1,3)=0.5_cp*dr
               x%ep(2,3)=0.5_cp*((d(3)-d(2))/tand(c%ang(1))/sind(c%rang(2)) - da(1)/sind(c%rang(2)))
               x%ep(1,2)=0.5_cp*((d(1)-d(2))/tand(c%ang(3)) - da(3))

               if (abs(c%rang(2)-90.0) > 0.01)then
                  !add in terms when beta* /=90
                  cotbs=1.0_cp/tand(c%rang(2))        !cot(beta*)
                  x%ep(3,3)=x%ep(3,3) + dr*cotbs
                  x%ep(2,3)=x%ep(2,3) + 0.5_cp*cotbs*((d(1)-d(2))/tand(c%ang(3))     -da(3))
                  x%ep(1,3)=x%ep(1,3) + 0.5_cp*cotbs*(d(1)-d(3) -da(1)*cosd(c%ang(1)) + da(3)/tand(c%ang(3)))
               end if

            case('BA')  !cartype=3: Brown and Angel, Equations from Tribaudino et al (2011)
               dr=get_angle_deriv(P,T,cell_eos,2,.false.,dtype)   !d(be*)/dP;  needed if beta changing but is 90.0
               !tensor coeffs if beta*=90
               x%ep(1,1)=d(1) +da(3)/tand(c%ang(3))
               x%ep(2,2)=d(2)
               x%ep(3,3)=d(3) + da(1)/tand(c%ang(1))
               x%ep(1,2)=0.5_cp*((d(1)-d(2))/tand(c%ang(3))/sind(c%rang(2)) - da(3)/sind(c%rang(2)))
               x%ep(1,3)=0.5_cp*dr
               x%ep(2,3)=0.5_cp*((d(3)-d(2))/tand(c%ang(1)) - da(1))
               if (abs(c%rang(2)-90.0) > 0.01)then
                  !add in terms when beta* /=90
                  cotbs=1.0_cp/tand(c%rang(2))        !cot(beta*)
                  x%ep(1,1)=x%ep(1,1) + dr*cotbs
                  x%ep(1,2)=x%ep(1,2) + 0.5_cp*cotbs*((d(3)-d(2))/tand(c%ang(1))     -da(1))
                  x%ep(1,3)=x%ep(1,3) + 0.5_cp*cotbs*(d(3)-d(1) -da(3)*cosd(c%ang(3)) + da(1)/tand(c%ang(1)))
               end if

            case('CB')      ! cartype=4: Neumann (1861) Equations from Pauffler and Weber (1999)
               dr=get_angle_deriv(P,T,cell_eos,3,.false.,dtype)   !d(ga*)/dP;  needed if gamma changing but is 90.0
               !tensor coeffs if gamma*=90
               x%ep(1,1)=d(1) +da(2)/tand(c%ang(2))
               x%ep(2,2)=d(2) +da(1)/tand(c%ang(1))
               x%ep(3,3)=d(3)
               x%ep(1,2)=0.5_cp*dr
               x%ep(1,3)=0.5_cp*(d(1)-d(3))/tand(c%ang(2)) - 0.5_cp*da(2)
               x%ep(2,3)=0.5_cp*((d(2)-d(3))/tand(c%ang(1))/sind(c%rang(3)) - da(1)/sind(c%rang(3)))
               if (abs(c%rang(3)-90.0) > 0.01)then
                  !add in terms when gamma* /=90
                  cotgs=1.0_cp/tand(c%rang(3))        !cot(gamma*)
                  x%ep(2,2)=x%ep(2,2) + dr*cotgs
                  x%ep(1,2)=x%ep(1,2) + 0.5_cp*cotgs*(d(1)-d(2) -da(1)*cosd(c%ang(1)) + da(2)/tand(c%ang(2)))
                  x%ep(2,3)=x%ep(2,3) + 0.5_cp*cotgs*((d(1)-d(3))/tand(c%ang(2))     -da(2))
               end if

            case default ! CA cartype=1:  This is Z //C X//A*: IRE convention
               ! Invalid orientation code defaults to this one
               ! These equations derived by RJA, October 2020
               dr=get_angle_deriv(P,T,cell_eos,3,.false.,dtype)  !d(ga*)/dP;  needed if gamma changing but is 90.0
               !tensor coeffs if gamma*=90
               x%ep(2,2)=d(2) +da(1)/tand(c%ang(1))
               x%ep(1,1)=d(1) +da(2)/tand(c%ang(2))
               x%ep(3,3)=d(3)
               x%ep(1,2)=0.5_cp*dr
               x%ep(2,3)=0.5_cp*((d(2)-d(3))/tand(c%ang(1)) - da(1))
               x%ep(1,3)=0.5_cp*((d(1)-d(3))/tand(c%ang(2))/sind(c%rang(3)) - da(2)/sind(c%rang(3)))
               if (abs(c%rang(3)-90.0) > 0.01)then
                  !add in terms when gamma* /=90
                  cotgs=1.0_cp/tand(c%rang(3))        !cot(gamma*)
                  x%ep(1,1)=x%ep(1,1) + dr*cotgs
                  x%ep(1,2)=x%ep(1,2) + 0.5_cp*cotgs*(d(2)-d(1) -da(2)*cosd(c%ang(2)) + da(1)/tand(c%ang(1)))
                  x%ep(1,3)=x%ep(1,3) + 0.5_cp*cotgs*((d(2)-d(3))/tand(c%ang(1))     -da(1))
               end if

         end select

         !finish
         x%ep(2,1)=x%ep(1,2)
         x%ep(3,1)=x%ep(1,3)
         x%ep(3,2)=x%ep(2,3)
         if (dtype == 'P')then
            x%ep=-1000.0_cp*x%ep       ! because compressibilities are negative of 1/a da/dP etc
            if (len_trim(cell_eos%eosc%Pscale_name) > 0)then
               x%property='Compressibility in units of inverse '//trim(cell_eos%eosc%Pscale_name)//' x 10^3'
            else
               x%property='Compressibility in units of inverse pressure units x 10^3'
            end if

         else
            x%property='Thermal expansion x 10^5'
            x%ep=100000.0_cp*x%ep
         end if

         call fix_tensor(x%ep,x%system)   ! make strain conform to crystal system, and thus eliminate round-off error

         !> for monoclinic or triclinic calculate Eigenvalues and vectors from tensor of properties x%ep
         call Diagonalize_SH (X%Ep, 3, X%evalp, X%Evec)
         call orient_eigenvectors(X%evalp,X%evec)       !sort the eigen vectors so that #1 is close to +X etc
         call calc_Paxes_angles(x,c,3)

      else        !higher symmetries
         do i=1,3
            x%ep(i,i)=d(i)
         end do
         if (dtype == 'P')then
            x%ep=-1000.0_cp*x%ep       ! because compressibilities are negative of 1/a da/dP etc
            if (len_trim(cell_eos%eosc%Pscale_name) > 0)then
               x%property='Compressibility in units of inverse '//trim(cell_eos%eosc%Pscale_name)//' x 10^3'
            else
               x%property='Compressibility in units of inverse pressure units x 10^3'
            end if

         else
            x%property='Thermal expansion x 10^5'
            x%ep=100000.0_cp*x%ep
         end if
         call fix_tensor(x%ep,x%system)   ! make strain conform to crystal system, and thus eliminate round-off error
      end if

   End Subroutine Get_Tensor_Eos

End SubModule EoS_Get_Tensor