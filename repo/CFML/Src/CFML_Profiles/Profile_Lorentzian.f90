!!----
!!----
!!----
!!
SubModule (CFML_Profiles) Profile_Lorentzian
  implicit none
   Contains
   !!----
   !!---- LORENTZIAN
   !!----
   !!----
   !!---- 21/04/2019
   !!
   Pure Module Function Lorentzian(X,Par) Result (Lor_Val)
      !---- Arguments ----!
      real(kind=cp),              intent(in) :: x
      real(kind=cp), dimension(:),intent(in) :: par
      real(kind=cp)                          :: lor_val

      !--- Local variables ---!
      real(kind=cp) :: H,al,bl

      H=par(1)
      al= 0.63661977236758134307553505349006/H
      bl= 4.0/(H*H)
      lor_val = al/(1.0+bl*x*x)

   End Function Lorentzian

   !!----
   !!---- LORENTZIAN_DER
   !!----
   !!---- 21/04/2019
   !!
   Pure Module Subroutine Lorentzian_Der(X,Par,Lor_Val,Dpar)
      !---- Arguments ----!
      real(kind=cp),                        intent(in) :: x
      real(kind=cp),           dimension(:),intent(in) :: par
      real(kind=cp),                        intent(out):: lor_val
      real(kind=cp), optional, dimension(:),intent(out):: dpar

      !--- Local variables ---!
      real(kind=cp) :: H,al,bl,lorp,dlorh,x2

      H=par(1)
      al= 0.63661977236758134307553505349006_cp/H
      bl= 4.0_cp/(H*H)
      x2=x*x
      lor_val = al/(1.0_cp+bl*x2)
      if (present(dpar)) then
         lorp  = -2.0_cp *lor_val*lor_val*bl*x/al
         dlorH = (2.0_cp*bl*lor_val*x2/al -1.0_cp)*lor_val/H
         dpar(1:2)=(/lorp,dlorH/)
      end if

   End Subroutine Lorentzian_Der

   !!--++
   !!--++ LORENTZIAN
   !!--++
   !!--++    Return value of Lorentzian at 'Pos' for peak at 'Pos0' and 'gamm'
   !!--++
   !!--++ 21/04/2019
   !!
   Module Subroutine Prof_Lorentzian(Pos , Pos0 , gamm , Dldt , Dldg, Lorentz )
      !---- Arguments ----!
      real(kind=cp), intent(in) :: pos
      real(kind=cp), intent(in) :: pos0
      real(kind=cp), intent(in) :: gamm
      real(kind=cp), intent(out):: dldt        !is derivative of L wrt Pos0
      real(kind=cp), intent(out):: dldg        !is derivative of L wrt gamm
      real(kind=cp), intent(out):: lorentz

      !---- Local Variables ----!
      real(kind=cp), parameter  :: cl=2.0_cp/pi
      real(kind=cp)             :: delp , denom, denom2, delp2, gamm2

      delp = pos - pos0
      delp2=delp*delp
      gamm2=gamm*gamm
      denom = 4.0 * delp2 + gamm2
      denom2=denom*denom
      lorentz = cl * gamm / denom
      dldt = 8.0 * cl * gamm * delp / denom2
      dldg = cl * (4.0 * delp2 - gamm2) / denom2

   End Subroutine Prof_Lorentzian

End SubModule Profile_Lorentzian