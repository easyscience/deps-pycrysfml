!!----
!!----
!!----
SubModule (CFML_Reflections) Refl_H_S
   implicit none
   Contains
   !!----
   !!---- H_S
   !!----   Calculates: sin_theta/lamda = 1/(2d)
   !!----
   !!---- 20/06/2019
   !!
   Module Function H_S_real(H, Cell) Result(S)
      !---- Arguments ----!
      real(kind=cp), dimension(3), intent(in) :: h
      class(Cell_G_Type),          intent(in) :: Cell
      real(kind=cp)                           :: S

      S= 0.5*sqrt( h(1)*h(1)*Cell%GR(1,1) +     h(2)*h(2)*Cell%GR(2,2) + &
                   h(3)*h(3)*Cell%GR(3,3) + 2.0*h(1)*h(2)*Cell%GR(1,2) + &
               2.0*h(1)*h(3)*Cell%GR(1,3) + 2.0*h(2)*h(3)*Cell%GR(2,3) )
   End Function H_S_real

   Module Function H_S_int(H, Cell, Nk, Kv) Result(S)
      !---- Arguments ----!
      integer, dimension(:),                  intent(in) :: h
      class(Cell_G_Type),                     intent(in) :: Cell
      integer, optional,                      intent(in) :: Nk
      real(kind=cp),dimension(:,:), optional, intent(in) :: Kv
      real(kind=cp)                                      :: S

      !--- Local variables ---!
      integer                     :: i
      real(kind=cp), dimension(3) :: hkl, hint

      hkl=h(1:3); hint=0.0
      if (present(nk) .and. present(kv)) then
         do i=1,nk
            hint=hint+h(3+i)*kv(:,i)
         end do
         hkl=hkl+hint
      end if
      S= 0.5*sqrt( hkl(1)*hkl(1)*Cell%GR(1,1) +     hkl(2)*hkl(2)*Cell%GR(2,2) + &
                   hkl(3)*hkl(3)*Cell%GR(3,3) + 2.0*hkl(1)*hkl(2)*Cell%GR(1,2) + &
               2.0*hkl(1)*hkl(3)*Cell%GR(1,3) + 2.0*hkl(2)*hkl(3)*Cell%GR(2,3) )
   End Function H_S_int

End SubModule Refl_H_S