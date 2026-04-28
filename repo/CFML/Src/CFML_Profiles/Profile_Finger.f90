!!----
!!----
!!----
!!
SubModule (CFML_Profiles) Profile_Finger
  implicit none
   Contains
   !!----
   !!---- FUNCTION DFUNC_INT
   !!----
   !!----     Function to give the analytical value of the normalisation constant
   !!----
   !!
   Pure Module Function dfunc_int(twopsi, twoth0) result(dfunc)
      !---- Arguments ----!
      Real(kind=cp), Intent(In)  :: twopsi
      Real(kind=cp), Intent(In)  :: twoth0
      Real(kind=cp)              :: dfunc

      !--- Local variables
      Real(kind=cp) :: sintp        !Sin twopsi
      Real(kind=cp) :: sin2t,sin2t2,csp,csm,ssp,ssm,a,b ! sin2Theta, (sin2Theta)^2

      If (Abs(twopsi-twoth0) < epsilon(1.0_cp)) Then
         dfunc=pi_over_two
      Else
         sin2t=Sin(twoth0)
         sin2t2=sin2t*sin2t
         sintp = Sin(twopsi)
         csp=sintp+sin2t2
         csm=sintp-sin2t2
         ssp=Abs((sintp+1.0_cp)*sin2t)
         ssm=Abs((sintp-1.0_cp)*sin2t)
         a=csm/ssm; b=-csp/ssp
         If (a > 1.0_cp) a=1.0_cp
         If (b <-1.0_cp) b=-1.0_cp
         dfunc=0.5_cp*(Asin(a)-Asin(b))
      End If

   End Function dfunc_int

   !!----
   !!---- FUNCTION EXTRA_INT
   !!----
   !!----    Function to calculate 1/4(log(|sin(x)+1|)-log(|sin(x)-1|))
   !!----
   !!
   Pure Module Function extra_int(x) result(extra)
      !---- Arguments ----!
      Real(kind=cp), Intent(In) :: x
      Real(kind=cp)             :: extra

      !--- Local variables
      Real(kind=cp)             :: sinx

      sinx = Sin(x)
      extra = 0.25_cp*(Log(Abs(sinx+1.0_cp))-Log(Abs(sinx-1.0_cp)))

   End Function extra_int

   !!----
   !!---- PROF_VAL
   !!----
   !!----    Return the value of Profile function at twoth of a peak of centre twoth0.
   !!----    Asymmetry due to axial divergence using the method of Finger, Cox and Jephcoat,
   !!----    J. Appl. Cryst. 27, 892, 1992.
   !!----
   !!----    This version is due to suggestions of James Hester based on new method of
   !!----    calculating derivatives.
   !!----
   !!---- 21/04/2019
   !!
   Module Subroutine Prof_Val( eta, gamm, asym1, asym2, twoth, twoth0, dprdt, dprdg,  &
                               dprde , dprds , dprdd , profval, use_asym, use_hps)
      !---- Arguments ----!
      real(kind=cp),   Intent(In)    :: eta            ! mixing coefficient between Gaussian and Lo
      real(kind=cp),   Intent(In)    :: gamm           ! FWHM
      real(kind=cp),   Intent(In)    :: asym1          ! s_l source width/detector distance or D_L+
      real(kind=cp),   Intent(In)    :: asym2          ! d_l detector width/detector distance or D_
      real(kind=cp),   Intent(In)    :: twoth          ! point at which to evaluate the profile
      real(kind=cp),   Intent(In)    :: twoth0         ! two_theta value for peak
      real(kind=cp),   Intent(Out)   :: dprdt          ! derivative of profile wrt TwoTH0
      real(kind=cp),   Intent(Out)   :: dprdg          ! derivative of profile wrt gamm
      real(kind=cp),   Intent(Out)   :: dprde          ! derivative of profile wrt Eta
      real(kind=cp),   Intent(Out)   :: dprds          ! derivative of profile wrt asym1
      real(kind=cp),   Intent(Out)   :: dprdd          ! derivative of profile wrt asym2
      real(kind=cp),   Intent(Out)   :: profval        ! Value of the profile at point twoth
      Logical,         Intent(In)    :: use_asym       ! true if asymmetry to be used
      Logical,         Intent(In)    :: use_hps        ! true if asym1=D_L+S_L and asym2=D_L-S_L
                                                       ! false if asym1=D_L ans asym2=S_L
      !---- Local Variables ----!

      !The variables below have the "save" attribute in order to save calculation
      !time when the subroutine is invoked for different points of the same peak
      real(kind=cp),save :: s_l , d_l, half_over_dl
      real(kind=cp),save :: df_dh_factor, df_ds_factor
      real(kind=cp),save :: dfi_emin, dfi_einfl
      real(kind=cp),save :: normv_analytic
      real(kind=cp),save :: einfl              ! 2phi value for inflection point
      real(kind=cp),save :: emin               ! 2phi value for minimum
      real(kind=cp),save :: cstwoth            ! cos(twoth)
      real(kind=cp),save :: coseinfl           ! cos(Einfl)
      real(kind=cp),save :: apb                ! (S + H)/L
      real(kind=cp),save :: amb                ! (S - H)/L
      real(kind=cp),save :: apb2               ! (ApB) **2
      Integer,save       :: arraynum, ngt, ngt2, it
      Logical,save       :: s_eq_d

      ! Variables not conserving their value between calls
      Integer       :: side, k
      real(kind=cp) :: tmp , tmp1 , tmp2  ! intermediate values
      real(kind=cp) :: delta              ! Angle of integration for comvolution
      real(kind=cp) :: sindelta           ! sine of DELTA
      real(kind=cp) :: cosdelta           ! cosine of DELTA
      real(kind=cp) :: rcosdelta          ! 1/cos(DELTA)
      real(kind=cp) :: f,g, einflr,eminr,twoth0r
      real(kind=cp) :: sumwg, sumwrg, sumwrdgda ,sumwdgdb , sumwrdgdb
      real(kind=cp) :: sumwgdrdg, sumwgdrde, sumwgdrd2t
      real(kind=cp) :: sumwx
      real(kind=cp),parameter :: eps_close=0.00001, eps_asym2=0.0002
      logical       :: re_calculate

      ! First simple calculation of Pseudo-Voigt if asymmetry is not used
      if(.not. use_asym .or. abs(twoth0-90.0) < 0.4) then
        call Psvoigtian(twoth,twoth0,eta,gamm,dprdt,dprdg,dprde,tmp)
        profval = tmp
        dprds = 0.0_cp
        dprdd = 0.0_cp
        return
      end if

      !From here to the end of the procedure asymmetry is used.

      !Make the calculations of some variables only if twoth0,asym1,asym2
      !are different from previous values. This save calculation time if the
      !different points of a peak are calculated sequentially for the same values
      !of twotheta and asymmetry parameters.

      re_calculate= abs(twoth0_prev-twoth0) > eps .or.  &
                    abs(asym1_prev-asym1)   > eps .or.  &
                    abs(asym2_prev-asym2)   > eps

      if(re_calculate) then

        twoth0_prev=twoth0
         asym1_prev=asym1
         asym2_prev=asym2

        twoth0r=twoth0*to_rad
        cstwoth = Cos(twoth0r)
        If (use_hps .or. asym2 < eps_asym2) Then
          s_l = 0.5*(asym1 - asym2)  ! 1/2(s_l+d_l - (d_l-s_l))
          d_l = 0.5*(asym1 + asym2)  ! 1/2(s_l+d_l + (d_l-s_l))
        Else
          s_l = asym1
          d_l = asym2
        End If
        apb = s_l + d_l
        amb = s_l - d_l
        ! Catch special case of S_L = D_L
        If (Abs(amb) <= eps_asym2) Then
          s_eq_d = .TRUE.
        Else
          s_eq_d = .FALSE.
        End If
        apb2 = apb*apb

        tmp = Sqrt(1.0 + amb*amb)*cstwoth
        If ((Abs(tmp) > 1.0) .or. (Abs(tmp) <= Abs(cstwoth))) Then
          einfl = twoth0
          einflr=einfl*to_rad
          dfi_einfl = pi_over_two
        Else
          einflr = Acos(tmp)
          einfl=einflr*to_deg
          dfi_einfl = dfunc_int(einflr,twoth0r)
        End If
        coseinfl = Cos(einflr)
        tmp2 = 1.0 + apb2
        tmp = Sqrt(tmp2) * cstwoth

        ! If S_L or D_L are zero, set Einfl = 2theta
        ! If S_L equals D_L, set Einfl = 2theta

        If (abs(s_l) <= eps  .OR.  abs(d_l) <= eps  .OR. s_eq_d) then
          einfl = twoth0
          einflr=einfl*to_rad
        End if

        If (Abs(tmp) <= 1.0) Then
          eminr = Acos(tmp)
          emin = eminr * to_deg
          tmp1 = tmp2 * (1.0 - tmp2 * cstwoth*cstwoth)
        Else
          tmp1 = 0.0
          If (tmp > 0.0) Then
            emin = 0.0
            eminr= 0.0
          Else
            emin = 180.0
            eminr= pi
          End If
        End If

        dfi_emin = dfunc_int(eminr,twoth0r)
        !
        ! Simplifications if S_L equals D_L
        !
        half_over_dl=0.5_cp/d_l
        If (s_eq_d) Then
          dfi_einfl = pi_over_two
          normv_analytic = (dfi_einfl - dfi_emin) -  &
              2.0_cp*half_over_dl*(extra_int(einflr)-extra_int(eminr))
          df_dh_factor =  half_over_dl * (pi_over_two - dfi_emin)
          df_ds_factor =  half_over_dl * (pi_over_two - dfi_emin)
          df_dh_factor = df_dh_factor - 2.0_cp*half_over_dl * normv_analytic
        Else
          dfi_einfl = dfunc_int(einflr,twoth0r)
          normv_analytic = Min(s_l,d_l)/d_l*(pi_over_two - dfi_einfl)
          normv_analytic = normv_analytic + apb*half_over_dl*(dfi_einfl-dfi_emin)   &
                          -2.0_cp*half_over_dl*(extra_int(einflr)-extra_int(eminr))
          tmp= half_over_dl*(pi - dfi_einfl - dfi_emin)
          tmp1=half_over_dl*(dfi_einfl - dfi_emin)
          If(d_l < s_l) Then
            df_dh_factor = tmp
            df_ds_factor = tmp1
          Else
            df_dh_factor = tmp1
            df_ds_factor = tmp
          End If
          df_dh_factor = df_dh_factor - 2.0_cp*half_over_dl * normv_analytic
        End If

        arraynum = 1
        k = ctrl_nsteps * (twoth0 - emin)   ! Calculate the number of terms needed
        Do
           if ( .not. ( arraynum < 14  .And.  k > nterms(arraynum) ) ) exit
           arraynum = arraynum + 1
        End Do
        ngt = nterms(arraynum)              ! Save the number of terms
        ngt2 = ngt / 2
        it = fstterm(arraynum)-ngt2
      End if   !re_calculate

        ! Clear terms needed for summations
      sumwg     = 0.0_cp
      sumwrg    = 0.0_cp
      sumwrdgda = 0.0_cp
      sumwdgdb  = 0.0_cp
      sumwrdgdb = 0.0_cp
      sumwgdrd2t= 0.0_cp
      sumwgdrdg = 0.0_cp
      sumwgdrde = 0.0_cp
      sumwx     = 0.0_cp
      ! Compute the convolution integral
      Do k = ngt2+1 , ngt
        Do side = 1,2
          If (side == 1) Then
            delta = (twoth0 + emin)/2 + (twoth0 - emin) * xp(k + it)/2
            sumwx = sumwx + wp(k+it)
          Else
            delta = (twoth0 + emin)/2 - (twoth0 - emin) * xp(k + it)/2
            sumwx = sumwx + wp(k+it)
          End If
          sindelta = Sin(delta*to_rad)
          cosdelta = Cos(delta*to_rad)
          If (Abs(cosdelta) < 1.0E-15) cosdelta = 1.0E-15
          rcosdelta = 1.0 / cosdelta   !  abs(1.0 / cosdelta) eliminate abs
          tmp = cosdelta*cosdelta - cstwoth*cstwoth
          If (tmp > eps_close) Then
            tmp1 = Sqrt(tmp)
            f = Abs(cstwoth) / tmp1
          Else
            f = 0.0
          End If
          !  calculate G(Delta,2theta) , FCJ eq. 7a and 7b
          If ( Abs(delta - emin) > Abs(einfl - emin)) Then
            If (s_l > d_l) Then
              g = 2.0 * d_l * f * rcosdelta
            Else
              g = 2.0 * s_l * f * rcosdelta
            End If
          Else
            g = abs((-1.0 + apb * f) * rcosdelta) !Avoid negative g in some circumstances
          End If
          call Psvoigtian(twoth-delta+twoth0,twoth0,eta,gamm,dprdt,dprdg,dprde,tmp)
          sumwg = sumwg + wp(k+it) * g
          sumwrg = sumwrg + wp(k+it) * g * tmp
          If ( Abs(cosdelta) > Abs(coseinfl)) Then
            sumwrdgda = sumwrdgda + wp(k+it) * f * rcosdelta * tmp
            sumwrdgdb = sumwrdgdb + wp(k+it) * f * rcosdelta * tmp
          Else
            If (s_l < d_l) Then
              sumwrdgdb = sumwrdgdb + 2.0*wp(k+it)*f* rcosdelta*tmp
            Else
              sumwrdgda = sumwrdgda + 2.0*wp(k+it)*f* rcosdelta*tmp
            End If
          End If
          sumwgdrd2t = sumwgdrd2t + wp(k+it) * g * dprdt
          sumwgdrdg = sumwgdrdg + wp(k+it) * g * dprdg
          sumwgdrde = sumwgdrde + wp(k+it) * g * dprde
        End Do  ! loop over left, right side of quadrature
      End Do

      If (abs(sumwg) <= eps) then
         !sumwg = 1.0_cp
         profval = 0.0
         dprdt = 0.0
         dprdg = 0.0
         dprde = 0.0
      else
         profval = sumwrg / sumwg
         dprdt = sumwgdrd2t/ sumwg
         dprdg = sumwgdrdg / sumwg
         dprde = sumwgdrde / sumwg
      end if
      !
      If(normv_analytic <= eps) then
        !normv_analytic=1.0_cp
        dprdd = 0.0
        dprds = 0.0
      else
        dprdd = sumwrdgda / sumwg - df_dh_factor*profval/normv_analytic - profval/d_l
        dprds = sumwrdgdb / sumwg - df_ds_factor*profval/normv_analytic
      end if

      If (use_hps .or. asym2 < eps) Then
        dprds = 0.5_cp*(dprdd + dprds)  !S is really D+S
        dprdd = 0.5_cp*(dprdd - dprds)  !D is really D-S
      End If
      Return
    End Subroutine Prof_Val

End SubModule Profile_Finger