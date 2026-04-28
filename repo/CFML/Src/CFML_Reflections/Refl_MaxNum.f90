!!----
!!----
!!----
SubModule (CFML_Reflections) Refl_MaxNum
   implicit none
   Contains

   !!----
   !!---- GET_MAXNUMREF
   !!----    Provides un upper limit of the expected maximum number of
   !!----    reflections up to SinTLMax for a volume VolCell of the
   !!----    primitive cell.
   !!----    If SinTLMin is given, the result is the number of reflections
   !!----    in the interval (SinTLMin,SinTLMax).
   !!----    If Mult is provided, the number of reflections is multiplied as NumRef=nint(Mult*NumRef)
   !!----    If Multip is provided the result is divided by half this multiplicity,
   !!----    so we obtain an estimation of the expected mumber of unique reflections.
   !!----
   !!---- 21/06/2019
   !!
   Module Function Get_MaxNumRef(SinTLMax, VolCell, SinTLMin, Mult, Multip) Result(numref)
      !---- Arguments ----!
      real(kind=cp),           intent(in) :: SinTLMax    !> Maximum sinTheta/Lambda
      real(kind=cp),           intent(in) :: VolCell     !> Direct Cell Volume
      real(kind=cp), optional, intent(in) :: SinTLMin    !> Minimum sinTheta/Lambda
      real(kind=cp), optional, intent(in) :: Mult        !> Factor for controlling Numref= nint(Mult*numref)
      integer,       optional, intent(in) :: Multip      !> Multiplicity: Numref=2*Numref/(max(1,Multip))
      integer                             :: numref

      !---- Local Variables ----!
      real(kind=cp) :: r3, delta,sint_eff

      delta=1.0_cp/(VolCell**(1.0_cp/3.0_cp)) !Average reciprocal cell parameter
      sint_eff=2.0_cp*SinTLMax+delta          !The effective radius 2sinTheta/Lambda is increased by delta
      r3= sint_eff * sint_eff * sint_eff * 1.05_cp

      if (present(SinTLMin)) r3= r3 - 8.0_cp*SinTLMin * SinTLMin * SinTLMin

      numref=nint(4.0_cp*PI*r3*VolCell / 3.0_cp)

      !> The factor Mult is given because, for high symmetry, sometimes the obtained number
      !> is not enough for allocating the real number of reflections
      if (present(Mult)) numref= nint(Mult * numref)
      if (present(Multip)) numref= 2*numref/max(1,Multip)
   End Function Get_MaxNumRef
End SubModule Refl_MaxNum