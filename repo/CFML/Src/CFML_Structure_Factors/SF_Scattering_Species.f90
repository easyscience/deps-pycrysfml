Submodule (CFML_Structure_Factors) SF_Scattering_Species
   !---- Variables ----!
   implicit none

 Contains
   !!----
   !!---- SUBROUTINE ALLOCATE_SCATTERING_SPECIES
   !!----
   !!----     Allocates the components of Scf
   !!----
   !!----  Updated: April-2022
   !!----
   Module Subroutine Allocate_Scattering_Species(N, Scf)
      !---- Arguments ----!
      integer,                       intent(in)  :: n
      type(Scattering_Species_Type), intent(out) :: Scf

      !---- Local variables ----!
      integer :: i

      Scf%Num_Species=n
      Scf%Num_magspc=0

      allocate(Scf%br(n),Scf%bi(n),Scf%delta_fp(n),Scf%delta_fpp(n),Scf%symb(n))
      Scf%br=0.0
      Scf%bi=0.0
      Scf%delta_fp=0.0
      Scf%delta_fpp=0.0
      Scf%symb= " "

      allocate(Scf%Xcoef(n))
      do i=1,Scf%Num_Species
         Scf%Xcoef(i)%Z=0
         Scf%Xcoef(i)%a=0.0
         Scf%Xcoef(i)%b=0.0
         Scf%Xcoef(i)%c=0.0
      end do

   End Subroutine Allocate_Scattering_Species

   !!----
   !!----  SUBROUTINE ADDITIONAL_SCATTERING_FACTORS (overloaded)
   !!----
   !!----  Subroutine constructing add_Scatt by reading fil of File_List_Type
   !!----  These values replace those read from database CFML_Scattering_Chemical_Tables
   !!----
   !!----  Created (JRC): October-2015
   !!----
   Module Subroutine Additional_Scattering_Factors_FLT(Fil, Add_Scatt)
      !---- Arguments ----!
      type(File_list_Type),          intent(in)  :: fil
      Type(Scattering_Species_Type), intent(out) :: add_Scatt
      !--- Local variables ---!
      type(File_Type)  :: FT
      integer :: i
      FT%nlines=fil%nlines
      allocate(FT%line(fil%nlines))
      do i=1,fil%nlines
        FT%line(i)%Str=trim(fil%line(i))
      end do
      call Additional_Scattering_Factors(FT, Add_Scatt)
   End Subroutine Additional_Scattering_Factors_FLT

   Module Subroutine Additional_Scattering_Factors_FT(Fil, Add_Scatt)
      !---- Arguments ----!
      type(File_Type),               intent(in)  :: fil
      Type(Scattering_Species_Type), intent(out) :: add_Scatt

      !---- Local variables ----!
      integer, parameter :: N_ADD = 20

      integer            :: i,nsp,j,ier
      character(len=132)                   :: line
      character(len=4), dimension(N_add)   :: names
      real(kind=cp),    dimension(N_add)   :: b_real,b_imag
      real(kind=cp),    dimension(N_add)   :: d_fp,d_fpp,cc
      real(kind=cp),    dimension(4,N_add) :: ac,bc

      !> Init
      call clear_error()

      b_real=0.0; b_imag=0.0; d_fp=0.0; d_fpp=0.0; cc=0.0
      ac=0.0; bc=0.0
      nsp=0

      do i=1,fil%nlines
         line=adjustl(fil%line(i)%str)
         if (U_case(line(1:2)) == "B_") then
            nsp=nsp+1
            j=index(line," ")
            names(nsp)=line(3:j-1)
            read(unit=line(j:), fmt=*, iostat=ier) b_real(nsp),b_imag(nsp)
            if (ier /= 0) then
               err_CFML%IErr=1
               Err_CFML%flag=.true.
               err_CFML%Msg="Error reading scattering length on line containing: "//trim(line)
               return
            end if
         end if

         if (U_case(line(1:5)) == "DELT_") then
            nsp=nsp+1
            j=index(line," ")
            names(nsp)=line(6:j-1)
            read(unit=line(j:), fmt=*, iostat=ier) d_fp(nsp),d_fpp(nsp)
            if (ier /= 0) then
               err_CFML%IErr=1
               err_CFML%Msg="Error reading anomalous scattering terms on line containing: "//trim(line)
               return
            end if
         end if

         if (U_case(line(1:7)) == "XCOEFF_") then
            nsp=nsp+1
            j=index(line," ")
            names(nsp)=line(8:j-1)
            read(unit=line(j:), fmt=*, iostat=ier) ac(:,nsp),bc(:,nsp),cc(nsp)
            if (ier /= 0) then
               err_CFML%IErr=1
               Err_CFML%flag=.true.
               err_CFML%Msg="Error reading X-ray scattering coefficients on line containing: "//trim(line)
               return
            end if
         end if
      end do

      if (nsp > N_ADD) then
         err_CFML%IErr=1
         Err_CFML%flag=.true.
         write(unit=err_CFML%Msg,fmt="(a,i3,a)") "The number of additional scattering factors is limited to ",N_add," !!!"
         return
      end if

      if (nsp > 0) then
         call Allocate_Scattering_Species(nsp,add_Scatt)
         do i=1,nsp
            add_Scatt%Symb(i)      = names(i)
            add_Scatt%br(i)        = b_real(i)
            add_Scatt%bi(i)        = b_imag(i)
            add_Scatt%delta_fp(i)  = d_fp(i)
            add_Scatt%delta_fpp(i) = d_fpp(i)
            add_Scatt%Xcoef(i)%Symb= names(i)
            add_Scatt%Xcoef(i)%a   = ac(:,i)
            add_Scatt%Xcoef(i)%b   = bc(:,i)
            add_Scatt%Xcoef(i)%c   = cc(i)
         end do

      else
         add_Scatt%Num_species=0
      end if

   End Subroutine Additional_Scattering_Factors_FT

   !!----
   !!---- SUBROUTINE SET_FORM_FACTORS
   !!----
   !!----  Constructor subroutine of object Scf of Scattering_Species_Type,
   !!----  by reading the database contained in module CFML_Scattering_Chemical_Tables
   !!----  and, in the appropriate case, the object Add_Scatt.
   !!----
   !!----  Updated: April - 2022
   !!----
   Module Subroutine Set_Form_Factors(Atm, Scf, Lambda, Add_Scatt, Mag, Lun)
     type(AtList_type),                      intent(in out):: Atm
     Type(Scattering_Species_Type),          intent(out)   :: Scf
     real(kind=cp),                optional, intent(in)    :: lambda
     Type(Scattering_Species_Type),optional, intent(in)    :: Add_Scatt
     logical,                      optional, intent(in)    :: mag
     integer,                      optional, intent(in)    :: lun

     !---- Local variables ----!
     character(len=4)                        :: symbcar
     character(len=4), dimension(atm%natoms) :: symb, notfound
     character(len=4), dimension(atm%natoms) :: elem
     logical,          dimension(atm%natoms) :: magAtm
     integer                                 :: i,j,k,n,m,L,mf
     integer,          dimension(atm%natoms) :: ix,jx
     real(kind=cp)                           :: b,dmin,d
     real(kind=cp),    dimension(atm%natoms) :: bs
     logical                                 :: found, scf_changed

     call clear_error()
     call set_chem_info()

     !> Getting Fermi Lengths of atoms
     symb="    "
     Elem="    "
     notfound="    "
     magAtm=.false.
     scf_changed=.false.
     bs=0.0
     n=0
     do i=1,atm%natoms
        symbcar=u_case(atm%atom(i)%chemsymb)
        b=Get_Fermi_Length(symbcar)      ! equal to the charge of the nuclei

        if (abs(b) < 0.0001) then
           err_CFML%Ierr=1
           Err_CFML%flag=.true.
           err_CFML%Msg="The Fermi Length of Species "//trim(symbcar)//" was not found"
           return

        else
           if(any(Elem == symbcar)) cycle
           n=n+1
           bs(n) = b
           Elem(n)=u_case(atm%atom(i)%chemsymb)
           symb(n)=atm%atom(i)%SfacSymb
           magAtm(n)=atm%atom(i)%Magnetic
        end if
     end do
     call Remove_chem_info()
     Call Allocate_Scattering_Species(n,Scf)

     Scf%br=bs(1:n)
     if (present(lambda)) then

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
        do i=1,Scf%Num_species
           symbcar=l_case(Elem(i))
           do j=1,NUM_DELTA_FP
              if (symbcar /= Anomalous_ScFac(j)%Symb) cycle
              Scf%delta_fp(i)=Anomalous_ScFac(j)%fp(k)
              Scf%delta_fpp(i)=Anomalous_ScFac(j)%fpp(k)
              exit
           end do
        end do
        call Remove_Delta_Fp_Fpp()
     end if

     !> Look for X-ray scattering coefficients
     call Set_Xray_Form()
     ix=0; mf=0
     do i=1,Scf%Num_species
        symbcar=l_case(symb(i)) !Scattering factors
        if(magAtm(i)) then
           symbcar=l_case(Elem(i))
           ix(i) = i  !> Magnetic atom
        end if
        found=.false.

        do j=1,NUM_XRAY_FORM
           if (symbcar /= Xray_form(j)%Symb) cycle
           Scf%xcoef(i)=Xray_form(j)
           found=.true.
           exit
        end do

        if(.not. found) then
           mf=mf+1
           notfound(mf)=symb(i)
        end if

        if (.not. found) then
            !Try with the chemical element
            scf_changed=.true.
            symbcar=l_case(Elem(i))
            do j=1,NUM_XRAY_FORM
               if (trim(symbcar) /= trim(Xray_form(j)%Symb)) cycle
               Scf%xcoef(i)=Xray_form(j)
               found=.true.
               exit
            end do
            if (.not. found) then
              err_CFML%IErr=1
              Err_CFML%flag=.true.
              err_CFML%Msg="Error: X-ray scattering form factor coefficients not found for "//trim(Elem(i))//" or "//trim(symb(i))
              return
            end if
        end if
     end do

     call Remove_Xray_Form()

     m=0
     do i=1,atm%natoms
        symbcar=u_case(atm%atom(i)%chemsymb)
        if (u_case(trim(atm%atom(i)%SfacSymb)) == "MPOL") m=m+1
        do j=1,Scf%Num_species
           if (symbcar == Elem(j)) then
              atm%atom(i)%ind_ff(1)=j
              Scf%Xcoef(j)%Z=atm%atom(i)%Z
              Scf%Symb(j)=atm%atom(i)%SfacSymb
              exit
           end if
        end do
     end do

     if (present(Add_Scatt)) then
        if (Add_Scatt%Num_Species > 0) then
           do i=1,Add_Scatt%Num_Species
              do j=1,Scf%Num_species
                 if (Scf%Symb(j) == Add_Scatt%Symb(i)) then
                    if (abs(Add_Scatt%br(i))> 0.00001)  Scf%br(j)=Add_Scatt%br(i)
                    if (abs(Add_Scatt%bi(i))> 0.00001)  Scf%bi(j)=Add_Scatt%bi(i)
                    if (abs(Add_Scatt%delta_fp(i))> 0.00001)  Scf%delta_fp(j) =Add_Scatt%delta_fp(i)
                    if (abs(Add_Scatt%delta_fpp(i))> 0.00001) Scf%delta_fpp(j)=Add_Scatt%delta_fpp(i)
                    if (abs(sum(Add_Scatt%Xcoef(i)%a))> 0.00001) then
                       Scf%Xcoef(j)%a=Add_Scatt%Xcoef(i)%a
                       Scf%Xcoef(j)%b=Add_Scatt%Xcoef(i)%b
                       Scf%Xcoef(j)%c=Add_Scatt%Xcoef(i)%c
                    end if
                 end if
              end do
           end do
        end if
     end if

     if (present(mag) .or. any(magAtm .eqv. .true.)) then
        !> Load form factor values for Magnetic Scattering
        call Set_Magnetic_Form()

        !> Find Species in Magnetic_Form
        ix=0
        jx=0
        n=0
        do i=1,atm%natoms
           if(.not. atm%atom(i)%magnetic) cycle
           symbcar=atm%atom(i)%SfacSymb
           do j=1,NUM_MAG_FORM
              if(CFML_debug) write(*,"(i4,tr2,a)") j,trim(symbcar)//" <-> "//trim(Magnetic_Form(j)%Symb)
              if (trim(symbcar) /= trim(Magnetic_Form(j)%Symb)) cycle
              if (any(jx == j) ) exit
              n=n+1
              jx(n)=j
              ix(n)=i
              exit
           end do
           if(ix(n) == 0) then
              err_CFML%IErr=1
              Err_CFML%flag=.true.
              err_CFML%Msg="Error: Magnetic form factor coefficients not found for "//symbcar
              call Remove_Magnetic_Form()
              return
           end if
        end do
        Scf%Num_magspc=n
        allocate(Scf%Mcoef(n),Scf%Symb_mag(n))
        do k=1,Scf%Num_magspc
           j = jx(k)
           i = ix(k)
           Scf%Mcoef(k)=Magnetic_Form(j)
           Scf%Symb_mag(k)= atm%atom(i)%SfacSymb
        end do
        do i=1,atm%natoms
           if(.not. atm%atom(i)%magnetic) cycle
           symbcar=u_case(atm%atom(i)%SfacSymb)
           do j=1,Scf%Num_magspc
              if (symbcar == Scf%Symb_mag(j)) then
                 atm%atom(i)%ind_ff(2)=j
                 exit
              end if
           end do
        end do
        call Remove_Magnetic_Form()
     end if

     !> Printing Information
     if (present(lun)) then
        write(unit=lun,fmt="(/,a)")  "  INFORMATION FROM TABULATED NEUTRON SCATTERING LENGTHS"
        write(unit=lun,fmt="(a,/)")  "  ====================================================="
        write(unit=lun,fmt="(a)")    "  FERMI LENGTHS "
        write(unit=lun,fmt="(a,i3)") "   Number of chemically different species: ",Scf%Num_Species
        write(unit=lun,fmt="(/,a)")  "   Atom     Fermi Length (Br,Bi)[10^(-12) cm]      Atomic Number"
        do k=1,Scf%Num_species
           write(unit=lun,fmt="(a,2F10.6,tr20,i8)")  "     "//Scf%Symb(k), Scf%br(k), Scf%bi(k), Scf%Xcoef(k)%Z
        end do


        if(.not. present(mag) .and. .not. any(magAtm .eqv. .true.)) then
           write(unit=lun,fmt="(/,/)")
           write(unit=lun,fmt="(/,a)")  "  INFORMATION FROM TABULATED X-RAY SCATTERING FACTORS"
           write(unit=lun,fmt="(a,/)")  "  ==================================================="
           write(unit=lun,fmt="(/,a,/)")    "   ATOMIC SCATTERING FACTOR COEFFICIENTS: {A(i),B(i),I=1,4},C  Dfp  Dfpp "
           write(unit=lun,fmt="(a,i3)")     "   Number of chemically different species: ",Scf%Num_Species
           if(present(lambda)) then
              write(unit=lun,fmt="(a,f10.6,a)")  "  Wavelength: ",lambda," Angstroms"
           else
              write(unit=lun,fmt="(/,a)")  "  WAVELENGTH not provided: anomalous scattering not applied "
           end if
           if(scf_changed) then
              write(unit=lun,fmt="(2(/,a))") " => Warning! Some atomic scattering factor was not found!", &
                                             "    The coefficients corresponding to the chemical symbol are used as a replacement!"
             write(unit=lun,fmt="(21a)")     "    The following species are missing: ",(trim(notfound(i))//"  ",i=1,mf)
           end if
           write(unit=lun,fmt="(/,a)") &
                 " Atom(ScF)  Atom(ScFX)           a1       b1       a2       b2       a3       b3       a4       b4        c      Dfp     Dfpp"


           do k=1,Scf%Num_species
              write(unit=lun,fmt="(a,11F9.5)")    &
                              "  "//Scf%Symb(k)//"        "//Scf%Xcoef(k)%Symb//"        ", &
                             (Scf%xcoef(k)%a(L),Scf%xcoef(k)%b(L), L=1,4), Scf%xcoef(k)%c, &
                             Scf%delta_fp(k), Scf%delta_fpp(k)
           end do
           write(unit=lun,fmt="(/,/)")

        else

           write(unit=lun,fmt="(/,a)")  "  INFORMATION FROM TABULATED MAGNETIC FORM FACTORS"
           write(unit=lun,fmt="(a,/)")  "  ================================================"
           write(unit=lun,fmt="(/,a,/)")    "   MAGNETIC FORM FACTOR COEFFICIENTS: {A(i),B(i),I=1,3},C  "
           write(unit=lun,fmt="(a,i3)")     "   Number of chemically different species: ",Scf%Num_magspc
           write(unit=lun,fmt="(/,a)") &
                "     Atom     a1       b1       a2       b2       a3       b3       c      "
           do k=1,Scf%Num_magspc
              write(unit=lun,fmt="(a,7F9.5)")    &
                            "     "//Scf%Symb_mag(k), (Scf%Mcoef(k)%SctM(L), L=1,7)
           end do
           write(unit=lun,fmt="(/,/)")

        end if
     end if

   End Subroutine Set_Form_Factors

End Submodule SF_Scattering_Species