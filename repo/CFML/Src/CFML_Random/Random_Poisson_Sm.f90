!!----
!!----
!!----
SubModule (CFML_Random) Random_Poisson_Sm
  implicit none
   Contains
   !!--++
   !!--++ GG_F
   !!--++
   !!--++ 14/04/2019
   !!
   Module Function Gg_F(Methodeg) Result(Gg)
      !---- Arguments ----!
      integer,       intent(in) :: methodeg
      real(kind=cp)             :: gg

      !---- Local variables ----!
      real(kind=cp), parameter :: dpi  =6.283185308_cp       ! 2*pi
      real(kind=cp), parameter :: sqdse=0.857763885_cp       ! sqrt(2/e)
      real(kind=cp)            :: u,v,ran1,ran2
      logical                  :: sifin

      if (methodeg == 2) then
         call random_number(ran1)
         call random_number(ran2)
         gg=sqrt(-2*log(ran1))*cos(dpi*ran2)

      else if (methodeg == 3) then
         sifin=.false.
         do
            if (sifin) exit
            call random_number(ran1)
            call random_number(ran2)
            u=ran1
            v=(2*ran2-1)*sqdse
            sifin = ((0.25_cp*v*v/(u*u)) <= -log(u))
         end do
         gg=v/u
      end if

   End Function Gg_F

   !!--++
   !!--++ GPG_F
   !!--++
   !!--++    Poisson distribution by approx.gauss. (N(0,1) -> P(MT))
   !!--++
   !!--++ 14/04/2019
   !!
   Module Function Gpg_F(Mt,Methodeg,Gpstab) Result(Gpg)
      !---- Arguments ----!
      real(kind=cp), intent(in) :: mt
      integer,       intent(in) :: methodeg,gpstab
      integer                   :: gpg

      !---- Local variables ----!
      integer       :: x
      real(kind=cp) :: gg

      if (gpstab <= 0) then
         gg=gg_f(methodeg)
         x=nint(mt+sqrt(mt)*gg)

      else if (gpstab == 1) then
         gg=gg_f(methodeg)
         x=nint((0.5_cp*gg + sqrt(mt))**2 - 0.33_cp)
      end if

      if (x < 0) x=0
      gpg=x

   End Function Gpg_F

   !!--++
   !!--++ GPP_F
   !!--++
   !!--++    Poisson distribution by the product method
   !!--++
   !!--++ 14/04/2019
   !!
   Module Function Gpp_F(Mt) Result(Gpp)
      !---- Arguments ----!
      real(kind=cp), intent(in) :: mt
      integer                   :: gpp

      !---- Local variables ----!
      real(kind=cp) :: p,r,ran
      integer       :: k

      call random_number(ran)
      p=ran
      k=0
      r=exp(-mt)
      do
         if (p < r) exit
         call random_number(ran)
         p=p*ran
         k=k+1
      end do

      gpp=k

   End Function Gpp_F

   !!----
   !!---- RANDOM_POISSON
   !!----
   !!----    Generates a single random deviate from a Poisson distribution
   !!----    with mean mu.
   !!--..    Based on: J.Berruyer, A.Antoniadis, A.Filhol  (1985)  Rapport ILL 85AN19T
   !!----
   !!----    Method for generation of a random number according to Poisson distribution
   !!----        METHODE  = 1/2/3 if (centred limit )/(box-muller)/(reject)
   !!----        METHODEG = 1/2/3 if (FR-1)/(product)/(gauss)
   !!----        GPSTAB   = 0/1/2 stabilisation of the variance
   !!----                   (non)/(0.33)/(0.375) cf. EFRON)
   !!----
   !!---- 14/04/2019
   !!
   Module Function Random_Poisson(mt) Result(Ival)
      !---- Arguments ----!
      real(kind=cp), intent(in)    :: mt
      integer                      :: Ival

      !---- Local Variables ----!
      integer ::  methode,methodeg,gpstab

      if (mt < 0.0_cp) then
         Ival=mt
         return
      end if

      if (mt < 30.0_cp) then
         methode=2
      else if ((mt >= 30.0_cp) .and. (mt < 87.0_cp)) then
         methode=3
         methodeg=2
         gpstab=1
      else if ((mt >= 87.0_cp) .and. (mt < 500.0_cp)) then
         methode=3
         methodeg=3
         gpstab=1
      else if (mt >= 500.0_cp) then
         methode=3
         methodeg=2
         gpstab=0
      end if

      if (methode == 2) then
         Ival=gpp_f(mt)
      else if (methode == 3) then
         Ival=gpg_f(mt,methodeg,gpstab)
      else
         Ival=mt
      end if

   End Function Random_Poisson

End SubModule Random_Poisson_Sm