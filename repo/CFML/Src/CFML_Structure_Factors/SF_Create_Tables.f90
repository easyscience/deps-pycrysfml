Submodule (CFML_Structure_Factors) SF_Create_Tables
   !---- Variables ----!
   implicit none

 Contains

   !!--++
   !!--++ SUBROUTINE CREATE_TABLE_HR_HT
   !!--++
   !!--++    Calculate a Table with HR and HT values
   !!--..       HR(Numops,Nref)
   !!--..       HT(Numops,Nref)
   !!--++
   !!--++ Update: April 2022
   !!
   Module Subroutine Create_Table_HR_HT(Reflex, Grp)
      !---- Arguments ----!
      class(RefList_Type),intent(in) :: Reflex
      type(SpG_type),     intent(in) :: Grp

      !---- Local Variables ----!
      integer                 :: i,j
      integer, dimension(3,3) :: Mat
      real, dimension(3)      :: t

      do j=1,reflex%nref
         do i=1,grp%NumOps
            Mat=opMat(:,:,i)    !grp%op(i)%Mat(1:3,1:3)
            t=opTr(:,i)         !grp%op(i)%Mat(1:3,4)
            hr(i,j)%h=real(matmul(reflex%ref(j)%h, Mat))
            ht(i,j)=dot_product(real(reflex%ref(j)%h),t)
         end do
      end do

   End Subroutine Create_Table_HR_HT

   !!--++
   !!--++ SUBROUTINE Calc_Table_AB
   !!--++
   !!--++    Calculate Table with Aj(h) and Bj(h) values
   !!--++
   !!--++ Update: April 2022
   !!
   Module Subroutine Calc_Table_AB(Nref, Atm, Grp)
      !---- Arguments ----!
      integer,            intent(in) :: Nref
      type(AtList_type),  intent(in) :: Atm
      type(SpG_type),     intent(in) :: Grp

      !---- Local Variables ----!
      integer                       :: i,j,k
      real(kind=cp)                 :: arg,anis
      real(kind=cp),dimension(3)    :: h
      real(kind=cp),dimension(6)    :: beta

      !> Init
      Ajh=0.0_cp
      Bjh=0.0_cp

      if (Grp%Centred == 2) then
         do j=1,Nref
            do i=1,Atm%natoms
               arg=0.0_cp

               do k=1,grp%NumOps
                  h=hr(k,j)%h
                  arg=tpi*(dot_product(h,Atm%atom(i)%x)+ht(k,j))
                  anis=1.0_cp
                  if (Atm%atom(i)%thtype == "ani") then
                     beta=Atm%atom(i)%u(1:6)
                     anis=     h(1)*h(1)*beta(1)+     h(2)*h(2)*beta(2)+    h(3)*h(3)*beta(3) &
                          +2.0*h(1)*h(2)*beta(4)+ 2.0*h(1)*h(3)*beta(5)+2.0*h(2)*h(3)*beta(6)
                     anis=exp(-anis)
                  end if
                  Ajh(i,j)=Ajh(i,j)+cos(arg)*anis
               end do ! symmetry
            end do ! Atoms
         end do ! Reflections

      else
         do j=1,Nref
            do i=1,Atm%natoms
               arg=0.0_cp

               do k=1,grp%NumOps
                  h=hr(k,j)%h
                  arg=tpi*(dot_product(h,Atm%atom(i)%x)+ht(k,j))
                  anis=1.0_cp
                  if (Atm%atom(i)%thtype == "ani") then
                     beta=Atm%atom(i)%u(1:6)
                     anis=     h(1)*h(1)*beta(1)+     h(2)*h(2)*beta(2)+    h(3)*h(3)*beta(3) &
                           +2.0*h(1)*h(2)*beta(4)+ 2.0*h(1)*h(3)*beta(5)+2.0*h(2)*h(3)*beta(6)
                     anis=exp(-anis)
                  end if
                  Ajh(i,j)=Ajh(i,j)+cos(arg)*anis
                  Bjh(i,j)=Bjh(i,j)+sin(arg)*anis
               end do ! symmetry
            end do ! Atoms
         end do ! Reflections
      end if

   End Subroutine Calc_Table_AB

   !!--++
   !!--++ SUBROUTINE CALC_TABLE_TH
   !!--++    Calculate a Table of Isotropinc Thermal contribution and occupation
   !!--..         TH(Natoms,Nref)
   !!--++
   !!--++ Update: April - 2022
   !!
   Module Subroutine Calc_Table_TH(Reflex, Atm)
      !---- Argument ----!
      class(RefList_Type),intent(in) :: Reflex
      type(AtList_type),  intent(in) :: Atm

      !---- Local variables ----!
      integer          :: i,j
      real(kind=cp)    :: b,s

      !---- Isotropic model ----!
      do j=1,reflex%nref
         s=reflex%ref(j)%s
         do i=1,atm%natoms
            b=atm%atom(i)%u_iso
            th(i,j)= atm%atom(i)%occ * exp(-b*s*s)
         end do
      end do

   End Subroutine Calc_Table_TH

   !!--++
   !!--++ SUBROUTINE CREATE_TABLE_AF0_ELECTRONS
   !!--++    Calculate a Table of Atomic Factors for Electrons applying the Mott-Bethe formula:
   !!--++
   !!--++          fe=me^2/(8pi Eps0 h^2) (Z-fx(s))/s^2
   !!--++
   !!--++ Update: April - 2022
   !!
   Module Subroutine Create_Table_AF0_Electrons(Reflex, Atm, lun)
      !---- Arguments ----!
      class(RefList_Type),intent(in) :: Reflex
      type(AtList_type),  intent(in) :: Atm
      integer, optional,  intent(in) :: lun

      !---- Local Variables ----!
      character(len=4)               :: symbcar
      integer                        :: i,j, k,n,L
      integer, dimension(atm%natoms) :: ix,jx,ia
      real(kind=cp)                  :: fx

      !>Init
      call clear_error()


      !> Load form factor values for XRay
      call Set_Xray_Form()

      !> Found Species on Xray_Form
      ix=0
      jx=0
      n=0
      do i=1,atm%natoms
         if(atm%atom(i)%magnetic) then
            symbcar=l_case(atm%atom(i)%ChemSymb)
         else
            symbcar=l_case(atm%atom(i)%SfacSymb)
         end if
         do_repeat: do k=1,2
            do j=1, NUM_XRAY_FORM
               if (symbcar /= Xray_form(j)%Symb) cycle
               ix(i)=j
               if(any(jx == j) ) exit
               n=n+1
               jx(n)=j
               ia(n)=i
               exit do_repeat
            end do
            if (ix(i) == 0) then
               symbcar=l_case(atm%atom(i)%ChemSymb)
               cycle do_repeat
            else
               exit
            end if
         end do do_repeat
      end do

      if (present(lun)) then
         write(unit=lun,fmt="(/,a)") "  INFORMATION FROM TABULATED X-RAY SCATTERING FACTORS (For Electron Diffraction)"
         write(unit=lun,fmt="(a,/)") "  =============================================================================="
      end if

      if (any(ix == 0)) then
         err_CFML%IErr=1
         Err_CFML%flag=.true.
         Err_CFML%Msg="The Species "//trim(symbcar)//" was not found!"

      else
         !> Fill AF Table
         do j=1,reflex%nref
            do i=1,atm%natoms
               fx=fj(reflex%ref(j)%s,xray_form(ix(i))%a,xray_form(ix(i))%b,xray_form(ix(i))%c)+afp(i)

               !>Mott-Bethe formula fe=me^2/(8pi Eps0 h^2) (Z-fx(s))/s^2
               af0(i,j)=0.023934*(xray_form(ix(i))%Z-fx)/(reflex%ref(j)%s*reflex%ref(j)%s)
            end do
         end do
      end if

      !> Printing Information
      if (present(lun)) then
         write(unit=lun,fmt="(/,a,/)")    "   ATOMIC SCATTERING FACTOR COEFFICIENTS: {A(i),B(i),I=1,4},C  and Atomic Number "
         write(unit=lun,fmt="(a,i3)")     "   Number of chemically different species: ",n
         write(unit=lun,fmt="(/,a)") &
              "   Atom     a1       b1       a2       b2       a3       b3       a4       b4        c       Z"
         do k=1,n
            j = jx(k)
            i = ia(k)
            write(unit=lun,fmt="(a,9F9.5,i7)")    &
                          "     "//atm%atom(i)%chemsymb, &
                          (xray_form(j)%a(L),xray_form(j)%b(L), L=1,4), xray_form(j)%c, &
                           xray_form(j)%Z
         end do
         write(unit=lun,fmt="(/,/)")
      end if

      call Remove_Xray_Form()

   End Subroutine Create_Table_AF0_Electrons

   !!--++
   !!--++ SUBROUTINE CREATE_TABLE_AF0_XRAY
   !!--++    Calculate a Table of Atomic Factors for X-Ray
   !!--++
   !!--..      AF0(Natoms,Nref), AFP(Natoms), AFPP(Natoms)
   !!--++
   !!--++ Update: April - 2022
   !!
   Module Subroutine Create_Table_AF0_Xray(Reflex, Atm, lambda, lun)
      !---- Arguments ----!
      class(RefList_Type),     intent(in) :: Reflex
      type(AtList_type),       intent(in) :: Atm
      real(kind=cp), optional, intent(in) :: lambda
      integer,       optional, intent(in) :: lun

      !---- Local Variables ----!
      character(len=4)               :: symbcar,symban
      integer                        :: i,j, k,n,L
      integer, dimension(atm%natoms) :: ix,jx,ia
      real(kind=cp)                  :: dmin,d

      !> Init
      call clear_error()

      !> Load form factor values for XRay
      call Set_Xray_Form()

      !> Found Species on Xray_Form
      ix=0
      jx=0
      n=0
      do i=1,atm%natoms
         if(atm%atom(i)%magnetic) then
            symbcar=l_case(atm%atom(i)%ChemSymb)
         else
            symbcar=l_case(atm%atom(i)%SfacSymb)
         end if
         do_repeat: do k=1,2
            do j=1, NUM_XRAY_FORM
               if (symbcar /= Xray_form(j)%Symb) cycle
               ix(i)=j
               if(any(jx == j) ) exit
               n=n+1
               jx(n)=j
               ia(n)=i
               exit do_repeat
            end do
            if (ix(i) == 0) then
               symbcar=l_case(atm%atom(i)%ChemSymb)
               cycle do_repeat
            else
               exit
            end if
         end do do_repeat
      end do

      if (present(lun)) then
         write(unit=lun,fmt="(/,a)") "  INFORMATION FROM TABULATED X-RAY SCATTERING FACTORS"
         write(unit=lun,fmt="(a,/)") "  ==================================================="
      End if

      if (present(lambda)) then
         !> Load anomalous scattering form factor values for XRays
         call Set_Delta_Fp_Fpp()

         !> Select wavelength (by default is CuKalpha1: k=5 in the list)
         dmin=1000.0
         do i=1,5
            d=abs(lambda-Xray_Wavelengths(i)%Kalfa(1))
            if (d < dmin) then
               dmin=d
               k=i        !Selection of the index for fp and fpp lists
            end if
         end do

         !> Found Species on Anomalous_ScFac
         do i=1,atm%natoms
            symban=l_case(atm%atom(i)%chemsymb)
            do j=1,Num_Delta_Fp
               if (symban /= Anomalous_ScFac(j)%Symb) cycle
               afp(i)=Anomalous_ScFac(j)%fp(k)
               afpp(i)=Anomalous_ScFac(j)%fpp(k)
               exit
            end do
         end do
         call Remove_Delta_Fp_Fpp()

      else
         if (present(lun)) then
            write(unit=lun,fmt="(a)")    "  Missed lambda, anomalous dipersion corrections not applied   "
            write(unit=lun,fmt="(a)")    "  The default wavelength is that of Cu-Kalpha1 spectral line  "
         end if
      end if

      if (any(ix == 0)) then
         err_CFML%Ierr=1
         Err_CFML%flag=.true.
         Err_CFML%Msg="The Species "//trim(symbcar)//" was not found!"

      else
         !> Fill AF Table
         do j=1,reflex%nref
            do i=1,atm%natoms
               af0(i,j)=fj(reflex%ref(j)%s,xray_form(ix(i))%a,xray_form(ix(i))%b,xray_form(ix(i))%c)+afp(i)
            end do
         end do
      end if

      !> Printing Information
      if (present(lun)) then
         write(unit=lun,fmt="(/,a,/)")    "   ATOMIC SCATTERING FACTOR COEFFICIENTS: {A(i),B(i),I=1,4},C  Dfp  Dfpp "
         write(unit=lun,fmt="(a,i3)")     "   Number of chemically different species: ",n
         write(unit=lun,fmt="(/,a)") &
              "   Atom     a1       b1       a2       b2       a3       b3       a4       b4        c      Dfp     Dfpp"
         do k=1,n
            j = jx(k)
            i = ia(k)
            write(unit=lun,fmt="(a,11F9.5)")    &
                          "     "//atm%atom(i)%chemsymb, &
                          (xray_form(j)%a(L),xray_form(j)%b(L), L=1,4), xray_form(j)%c, &
                          afp(i), afpp(i)
         end do
         write(unit=lun,fmt="(/,/)")
      end if

      call Remove_Xray_Form()

   End Subroutine Create_Table_AF0_Xray

   !!--++
   !!--++ SUBROUTINE CREATE_TABLE_AFP_NEUTNUC
   !!--++
   !!--++    Setting a Table of Fermi Lengths for Neutron Nuclear Scattering
   !!--..      AFP(Natoms)
   !!--++
   !!--++ Update: April - 2022
   !!
   Module Subroutine Create_Table_AFP_NeutNuc(Atm, lun)
      !---- Arguments ----!
      type(AtList_type), intent(in) :: Atm
      integer, optional, intent(in) :: lun

      !---- Local Variables ----!
      character(len=4)                        :: symbcar
      integer                                 :: i,k,n
      character(len=4), dimension(atm%natoms) :: symb
      real(kind=cp),    dimension(atm%natoms) :: bs
      real(kind=cp)                           :: b

      !> Init
      call clear_error()

      !> Load chemical information
      call set_chem_info()

      !> Getting Fermi Lengths of atoms
      symb="    "
      bs=0.0
      n=0
      do i=1,atm%natoms
         if(atm%atom(i)%magnetic) then
            symbcar=u_case(atm%atom(i)%ChemSymb)
         else
            symbcar=u_case(atm%atom(i)%SfacSymb)
         end if
         b=Get_Fermi_Length(symbcar)
         if (abs(b) < 0.0001) then
            err_CFML%Ierr=1
            Err_CFML%flag=.true.
            err_CFML%Msg="The Fermi Length of Species "//trim(symbcar)//" was not found!"
            return
         else
            afp(i) = b
            if(any(symb == symbcar)) cycle
            n=n+1
            symb(n)=symbcar
            bs(n) = b
         end if
      end do

      !> Printing Information
      if (present(lun)) then
         write(unit=lun,fmt="(/,a)")  "  INFORMATION FROM TABULATED NEUTRON SCATTERING FACTORS"
         write(unit=lun,fmt="(a,/)")  "  ==================================================="
         write(unit=lun,fmt="(a)")    "  FERMI LENGTHS "
         write(unit=lun,fmt="(a,i3)") "   Number of chemically different species: ",n
         write(unit=lun,fmt="(/,a)")  "   Atom     Fermi Length [10^(-12) cm]"
         do k=1,n
            write(unit=lun,fmt="(a,F15.6)")  "     "//symb(k), bs(k)
         end do
         write(unit=lun,fmt="(/,/)")
      end if

      call Remove_chem_info()

   End Subroutine Create_Table_AFP_NeutNuc

   !!--++
   !!--++ SUBROUTINE CREATE_TABLE_FABC_XRAY
   !!--++
   !!--++    Calculate a Table of Coefficients for Atomic Form Factors for X-Ray
   !!--++    ff_A(4,species),ff_B(4,Nspecies),ff_C(Nspecies), AFP(Nspecies), AFPP(Nspecies)
   !!--++    ff_z(Nspecies) contains the atomic number of the chemical species (useful for Electron diffraction)
   !!--++    p_a(Natoms) => pointer to the species of each atom
   !!--++
   !!--++ Update: April - 2022
   !!
   Module Subroutine Create_Table_fabc_Xray(Atm, Lambda, Elect, lun)
      !---- Arguments ----!
      type(AtList_type),           intent(in) :: Atm
      real(kind=cp),     optional, intent(in) :: lambda
      integer,           optional, intent(in) :: elect
      integer,           optional, intent(in) :: lun

      !---- Local Variables ----!
      character(len=4)               :: symbcar
      integer                        :: i,j, k,n,L
      integer, dimension(atm%natoms) :: ix,jx,ia
      real(kind=cp)                  :: dmin,d

      !>Init
      call clear_error()

      !> Load form factor values for XRay
      call Set_Xray_Form()

      !>Found Species on Xray_Form
      ix=0; jx=0
      n=0
      if(allocated(P_a)) deallocate(P_a)
      allocate(P_a(atm%natoms))

      do i=1,atm%natoms
         if(atm%atom(i)%magnetic) then
           symbcar=l_case(atm%atom(i)%ChemSymb)
         else
           symbcar=l_case(atm%atom(i)%SfacSymb)
         end if
         do_repeat: do k=1,2
            do j=1, NUM_XRAY_FORM
               if (symbcar /= Xray_form(j)%Symb) cycle
               ix(i)=j
               if(any(jx == j) ) exit
               n=n+1
               jx(n)=j
               ia(n)=i
               exit do_repeat
            end do
            if (ix(i) == 0) then
               symbcar=l_case(atm%atom(i)%ChemSymb)
               cycle do_repeat
            else
               exit
            end if
         end do do_repeat
      end do

      if (any(ix==0)) then
         err_CFML%Ierr=1
         Err_CFML%flag=.true.
         Err_CFML%Msg="The Species "//trim(symbcar)//" was not found in X-ray form-factor tables!"
         return
      end if

      do i=1,atm%natoms
         j=ix(i)
         do k=1,n
            if (jx(k) == j) then
               P_a(i)=k              !The atom i is of species k
               exit
            end if
         end do
      end do

      Nspecies=n ! Global private variable (Total number of chemical species)
      if (allocated(FF_a)) deallocate (FF_a)
      if (allocated(FF_b)) deallocate (FF_b)
      if (allocated(FF_c)) deallocate (FF_c)
      if (allocated(FF_z)) deallocate (FF_z)
      allocate(FF_a(4,n),FF_b(4,n),FF_c(n),FF_z(n))

      do k=1,n
         j = jx(k)
         i = ia(k)
         FF_a(:,k)= xray_form(j)%a(:)
         FF_b(:,k)= xray_form(j)%b(:)
         FF_c(  k)= xray_form(j)%c
         FF_z(  k)= xray_form(j)%Z
      end do

      if (present(lun)) then
         if (present(elect)) then
            write(unit=lun,fmt="(/,a)") "  INFORMATION FROM TABULATED X-RAY SCATTERING FACTORS (For Electron Diffraction)"
            write(unit=lun,fmt="(a,/)") "  =============================================================================="
         else
            write(unit=lun,fmt="(/,a)") "  INFORMATION FROM TABULATED X-RAY SCATTERING FACTORS"
            write(unit=lun,fmt="(a,/)") "  ==================================================="
         end if
      end if

      if (.not. present(elect)) then
         if (present(lambda)) then
            !> Load anomalous scattering form factor values for XRays
            call Set_Delta_Fp_Fpp()

            !> Select wavelength (by default is CuKalpha1: k=5 in the list)
            dmin=1000.0
            do i=1,5
               d=abs(lambda-Xray_Wavelengths(i)%Kalfa(1))
               if (d < dmin) then
                  dmin=d
                  k=i        ! Selection of the index for fp and fpp lists
               end if
            end do

            !> Found Species on Anomalous_ScFac
            do i=1,atm%natoms
               symbcar=l_case(atm%atom(i)%chemsymb)
               do j=1,Num_Delta_Fp
                  if (symbcar /= Anomalous_ScFac(j)%Symb) cycle
                  afp(i)=Anomalous_ScFac(j)%fp(k)
                  afpp(i)=Anomalous_ScFac(j)%fpp(k)
                  exit
               end do
            end do
            call Remove_Delta_Fp_Fpp()

         else
            if (present(lun)) then
               write(unit=lun,fmt="(a)")    "  Missed lambda, anomalous dipersion corrections not applied   "
               write(unit=lun,fmt="(a)")    "  The default wavelength is that of Cu-Kalpha1 spectral line  "
            end if
         end if
      end if !present(elect)

      !> Printing Information
      if (present(lun)) then
         if (present(elect)) then
            write(unit=lun,fmt="(/,a,/)")    "   ATOMIC SCATTERING FACTOR COEFFICIENTS: {A(i),B(i),I=1,4},C  and Atomic Number "
            write(unit=lun,fmt="(a,i3)")     "   Number of chemically different species: ",n
            write(unit=lun,fmt="(/,a)") &
              "   Atom     a1       b1       a2       b2       a3       b3       a4       b4        c       Z"

            do k=1,n
               j = jx(k)
               i = ia(k)
               write(unit=lun,fmt="(a,9F9.5,i7)")    &
                          "     "//atm%atom(i)%chemsymb, &
                          (xray_form(j)%a(L),xray_form(j)%b(L), L=1,4), xray_form(j)%c, &
                          xray_form(j)%z
            end do
         else
            write(unit=lun,fmt="(/,a,/)")    "   ATOMIC SCATTERING FACTOR COEFFICIENTS: {A(i),B(i),I=1,4},C  Dfp  Dfpp "
            write(unit=lun,fmt="(a,i3)")     "   Number of chemically different species: ",n
            write(unit=lun,fmt="(/,a)") &
                 "   Atom     a1       b1       a2       b2       a3       b3       a4       b4        c      Dfp     Dfpp"
            do k=1,n
               j = jx(k)
               i = ia(k)
               write(unit=lun,fmt="(a,11F9.5)")    &
                          "     "//atm%atom(i)%chemsymb, &
                          (xray_form(j)%a(L),xray_form(j)%b(L), L=1,4), xray_form(j)%c, &
                          afp(i), afpp(i)
            end do
         end if
         write(unit=lun,fmt="(/,/)")
      end if

      call Remove_Xray_Form()

   End Subroutine Create_Table_fabc_Xray

End Submodule SF_Create_Tables