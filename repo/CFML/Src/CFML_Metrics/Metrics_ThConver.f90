Submodule (CFML_Metrics) Metrics_ThConver
 Implicit none
 Contains
    !!----
    !!---- FUNCTION U_EQUIV
    !!----
    !!----    Subroutine to obtain the U equiv from U11 U22 U33 U12 U13 U23
    !!----
    !!---- Update: February - 2005
    !!
    Pure Module Function U_Equiv(Cell, Th_U) Result(Uequi)
       !---- Arguments ----!
       class(Cell_G_Type),          intent(in)  :: Cell    ! Cell object
       real(kind=cp), dimension(6), intent(in)  :: Th_U    ! U thermal parameters
       real(kind=cp)                            :: Uequi   ! Uequiv

       !---- Local variables ----!
       real(kind=cp)    :: a, b, c, as, bs, cs, cosa, cosb, cosg
       real(kind=cp)    :: u11, u22, u33, u23, u13, u12

       a  =cell%cell(1)
       b  =cell%cell(2)
       c  =cell%cell(3)
       as =cell%rcell(1)
       bs =cell%rcell(2)
       cs =cell%rcell(3)
       cosa=cosd(cell%ang(1))
       cosb=cosd(cell%ang(2))
       cosg=cosd(cell%ang(3))

       u11=Th_u(1)
       u22=Th_u(2)
       u33=Th_u(3)
       u12=Th_u(4)
       u13=Th_u(5)
       u23=Th_u(6)
       uequi= (1.0/3.0) * (u11 * a * a * as * as + &
                           u22 * b * b * bs * bs + &
                           u33 * c * c * cs * cs + &
                           2.0*u12 * a * b * as * bs * cosg + &
                           2.0*u13 * a * c * as * cs * cosb + &
                           2.0*u23 * b * c * bs * cs * cosa )
    End Function U_Equiv

    !!----
    !!---- FUNCTION GET_BETAS_FROM_B
    !!----
    !!----    Convert Thermal factors from B to Betas
    !!----
    !!---- Update: February - 2003
    !!
    Pure Module Function Get_Betas_from_B(B,Cell) Result(Beta)
       !---- Arguments ----!
       real(kind=cp),dimension(6), intent(in)  :: B
       class(Cell_G_Type),         intent(in)  :: Cell
       real(kind=cp),dimension(6)              :: Beta
       beta(1)=0.25*b(1)*cell%gr(1,1)                ! beta11
       beta(2)=0.25*b(2)*cell%gr(2,2)                ! beta22
       beta(3)=0.25*b(3)*cell%gr(3,3)                ! beta33
       beta(4)=0.25*b(4)*cell%rcell(1)*cell%rcell(2) ! beta12
       beta(5)=0.25*b(5)*cell%rcell(1)*cell%rcell(3) ! beta13
       beta(6)=0.25*b(6)*cell%rcell(2)*cell%rcell(3) ! beta23
    End Function Get_Betas_from_B

    !!----
    !!---- FUNCTION GET_U_FROM_B
    !!----
    !!----    Convert Thermal factors from B to U
    !!----
    !!---- Update: February - 2003
    !!
    Pure Module Function Get_U_from_B(B) Result(U)
       !---- Arguments ----!
       real(kind=cp),dimension(6),  intent(in)  :: B
       real(kind=cp),dimension(6)               :: U

       u=b/(4.0_cp*TPI2)

    End Function Get_U_from_B

    !!----
    !!---- Function Get_B_from_Betas
    !!----
    !!----    Convert Thermal factors from Betas to B
    !!----
    !!---- Update: February - 2003
    !!
    Pure Module Function Get_B_from_Betas(Beta,Cell) Result(B)
       !---- Arguments ----!
       real(kind=cp),dimension(6), intent(in)  :: Beta
       class(Cell_G_Type),         intent(in)  :: Cell
       real(kind=cp),dimension(6)              :: B

       b(1)=4.0*beta(1)/cell%gr(1,1)                  ! B11
       b(2)=4.0*beta(2)/cell%gr(2,2)                  ! B22
       b(3)=4.0*beta(3)/cell%gr(3,3)                  ! B33
       b(4)=4.0*beta(4)/(cell%rcell(1)*cell%rcell(2)) ! B12
       b(5)=4.0*beta(5)/(cell%rcell(1)*cell%rcell(3)) ! B13
       b(6)=4.0*beta(6)/(cell%rcell(2)*cell%rcell(3)) ! B23
    End Function Get_B_from_Betas

    !!----
    !!---- FUNCTION GET_BETAS_FROM_U
    !!----
    !!----    Convert Thermal factors from U to Betas
    !!----
    !!---- Update: February - 2003
    !!
    Pure Module Function Get_Betas_from_U(U,Cell) Result(Beta)
       !---- Arguments ----!
       real(kind=cp),dimension(6),intent(in)  :: U
       class(Cell_G_Type),        intent(in)  :: Cell
       real(kind=cp),dimension(6)             :: Beta
       beta(1)=tpi2*u(1)*cell%gr(1,1)                ! beta11
       beta(2)=tpi2*u(2)*cell%gr(2,2)                ! beta22
       beta(3)=tpi2*u(3)*cell%gr(3,3)                ! beta33
       beta(4)=tpi2*u(4)*cell%rcell(1)*cell%rcell(2) ! beta12
       beta(5)=tpi2*u(5)*cell%rcell(1)*cell%rcell(3) ! beta13
       beta(6)=tpi2*u(6)*cell%rcell(2)*cell%rcell(3) ! beta23
    End Function Get_Betas_from_U

    !!----
    !!---- Function Get_Betas_from_Biso
    !!----
    !!----    Convert Biso to Betas
    !!----
    !!---- Update: April - 2013
    !!
    Pure Module Function Get_Betas_from_Biso(Biso,Cell) Result(Betas)
       !--- Argument ----!
       real(kind=cp),           intent(in)  :: Biso
       class(Cell_G_Type),      intent(in)  :: Cell
       real(kind=cp), dimension(6)          :: Betas

       !---- Local variables ----!
       real(kind=cp), dimension (3,3) :: L,LT,U,bet
       integer                        :: i

       !> Init
       betas=0.0

       l=Cell%Orth_Cr_cel
       lt=Transpose(l)
       u = 0.0
       do i=1,3
          u(i,i) = 0.25*biso
       end do
       bet=matmul(l,lt)
       bet=matmul(bet,u)
       do i=1,3
          betas(i) = bet(i,i)
       end do

       betas(4) = bet(1,2)
       betas(5) = bet(1,3)
       betas(6) = bet(2,3)
    End Function Get_Betas_from_Biso

    !!----
    !!---- FUNCTION GET_U_FROM_BETAS
    !!----
    !!----    Convert Thermal factors from Betas to U
    !!----
    !!---- Update: February - 2003
    !!
    Pure Module Function Get_U_from_Betas(Beta,Cell) Result(U)
       !---- Arguments ----!
       real(kind=cp),dimension(6),intent(in)  :: Beta
       class(Cell_G_Type),        intent(in)  :: Cell
       real(kind=cp),dimension(6)             :: U

       u(1)=beta(1)/(tpi2*cell%gr(1,1))                ! U11
       u(2)=beta(2)/(tpi2*cell%gr(2,2))                ! U22
       u(3)=beta(3)/(tpi2*cell%gr(3,3))                ! U33
       u(4)=beta(4)/(tpi2*cell%rcell(1)*cell%rcell(2)) ! U12
       u(5)=beta(5)/(tpi2*cell%rcell(1)*cell%rcell(3)) ! U13
       u(6)=beta(6)/(tpi2*cell%rcell(2)*cell%rcell(3)) ! U23

    End Function Get_U_from_Betas

    !!----
    !!---- FUNCTION GET_B_FROM_U
    !!----
    !!----    Convert Thermal factors from U to B
    !!----
    !!---- Update: February - 2003
    !!
    Pure Module Function Get_B_from_U(U) Result(B)
       !---- Arguments ----!
       real(kind=cp),dimension(6), intent(in)  :: U
       real(kind=cp),dimension(6)              :: B

       b=4.0*tpi2*u

    End Function Get_B_from_U

End Submodule Metrics_ThConver