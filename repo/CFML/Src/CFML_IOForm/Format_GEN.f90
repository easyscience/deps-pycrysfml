!!----
!!----
!!----
SubModule (CFML_IOForm) Format_GEN
   !---- Variables ----!
   implicit none

 Contains

    !!----
    !!---- READ_ATOM
    !!----
    !!----    Subroutine to read the atom parameters from a given "line"
    !!----
    !!---- 04/05/2020
    !!
    Module Subroutine Read_Atom(Str, Atm)
       !---- Arguments ----!
       character(len=*), intent(in)    :: Str   ! Input string with ATOM directive
       Class(Atm_Type),  intent(out)   :: Atm   ! Parameters on variable At

       !---- Local variables -----!
       integer                           :: iv,i, nlong1,n !,ier,q

       real(kind=cp), dimension (10)     :: vet1
       real(kind=cp), dimension (10)     :: vet2

       character(len=132), dimension(1)  :: filevar
       character(len=40)                 :: magmom
       character(len=4)                  :: direc
       character(len=6)                  :: label

       !> Init
       call clear_error()
       call init_atom_type(Atm,0)
       magmom=" "

       !> Copy str to line
       line=adjustl(trim(str))

       ! Look for the item "Utyp" if we want to use Uiso instead of Biso
       iv= index(line,"Utyp")
       if ( iv /= 0) then
          atm%Utype="u_ij"
          line(iv:iv+3)=" "
       else
          atm%Utype="b_ij"
       end if


       !> Cut ATOM Directive
       call cut_string(line,nlong1,direc)
       if (u_case(direc) /= "ATOM") then
          Err_CFML%IErr=1  ! Error
          Err_CFML%Msg=" Error reading the ATOM keyword"
          return
       end if


       !> Extra info
       iv=index(line,"#")
       if (iv /= 0) then
          atm%AtmInfo=trim(line(iv+1:))
          line=line(:iv-1)
       end if

       !> Magnetic moment components
       iv=index(line,"Moment:")
       magmom=" "
       if (iv /= 0) then
          magmom=trim(line(iv+7:))  ! magmon should contain magnetic moment
          line=line(:iv-1)          ! Line after removing "Moment:" and infor
       end if

       !> Atom Label
       call cut_string(line,nlong1,label)
       atm%lab=label

       !> Atom Type (Chemical symbol & Scattering Factor)
       call cut_string(line,nlong1,label)

       !> Magnetic?
       i=index(DIGCAR(1:10),label(4:4))
       if(i == 0) i=index(DIGCAR(1:10),label(3:3)) !This is the case of MV3
       if ((u_case(label(1:1)) == "M" .or. u_case(label(1:1)) == "J" ) &
          .and. i /= 0 .and. index(label,"+") == 0) then
          atm%ChemSymb=u_case(label(2:2))//l_case(label(3:3))
          atm%magnetic=.true.
       else
          n=index(DIGCAR,label(2:2))
          if (n /=0) then
            atm%ChemSymb=u_case(label(1:1))
          else
            atm%ChemSymb=u_case(label(1:1))//l_case(label(2:2))
          end if
       end if

       atm%Z=Get_Z_Symb(atm%ChemSymb)
       atm%SfacSymb=label(1:4)

       !> Parameters
       filevar(1)="atm "//trim(line)

       n=1
       call Read_Key_ValueSTD(filevar,n,n,"atm",vet1,vet2,iv)
       if (iv <= 0) then
          Err_CFML%IErr=1  ! Error
          Err_CFML%Msg="Error reading parameters of atom: "//trim(atm%lab)
          return
       end if

       !> Coordinates
       if (iv < 3) then
          Err_CFML%IErr=1  ! Error
          Err_CFML%Msg="Error reading coordinates of atom: "//trim(atm%lab)
          return
       end if

       atm%x(:)=vet1(1:3)
       if (iv > 3) atm%u_iso=vet1(4)
       if (iv > 4) atm%occ=vet1(5)
       if (iv > 5) then
          atm%mom=vet1(6)
       end if
       if (iv > 6) atm%charge=vet1(7)

       select type (atm)
          class is (Atm_Std_Type)
              atm%x_std(:)=vet2(1:3)
              if (iv > 3) atm%u_iso_std=vet2(4)
              if (iv > 4) atm%occ_std=vet2(5)
       end select

       !> Attempt to get the oxidation state from "Label"
       if (abs(atm%charge) < EPS) then
          atm%charge=charge(label)
       end if

       !> Now read the components of the magnetic moment
       if (len_trim(magmom) /= 0) then
          filevar(1)="mom "//trim(magmom)
          n=1
          call Read_Key_ValueSTD(filevar,n,n,"mom",vet1,vet2,iv)

          !> Moment components
          if (iv < 3) then
             Err_CFML%IErr=1  ! Error
             Err_CFML%Msg="Error reading magnetic moment components of atom: "//trim(atm%lab)
             return
          end if
          atm%moment=vet1(1:3)
          select type(atm)
             class is (Atm_Std_Type)
                 atm%moment_std=vet2(1:3)
          end select
          if (atm%mom < 0.001) atm%mom=maxval(abs(atm%moment))
       end if

    End Subroutine Read_Atom

    !!----
    !!---- READ_CELL
    !!----
    !!----    Subroutine to read the cell parameters from a given "line"
    !!----
    !!---- 04/05/2020
    !!
    Module Subroutine Read_Cell(Str,Celda,Std,Cell,CFrame)
       !---- Arguments ----!
       character(len=*),                      intent(in)  :: Str
       real(kind=cp), dimension(6),           intent(out) :: Celda
       real(kind=cp), dimension(6), optional, intent(out) :: Std
       class(Cell_G_Type),          optional, intent(out) :: Cell
       character(len=*),            optional, intent(in)  :: CFrame

       !---- Local variables -----!
       character(len=4)                     :: direc
       character(len=132), dimension(1)     :: filevar
       integer                              :: nlong1,n,iv
       real(kind=cp), dimension (6)         :: vet1,vet2

       !> Init
       celda=0.0_cp
       if (present(std)) std=0.0_cp

       call clear_error()

       !> Copy
       line=str

       call cut_string(line,nlong1,direc)
       if (u_case(direc) /= "CELL") then
          Err_CFML%IErr=1  ! Error
          Err_CFML%Msg=" Error reading the CELL keyword"
          return
       end if

       filevar(1)="cell "//trim(line)
       n=1
       call Read_Key_ValueSTD(filevar,n,n,"cell",vet1,vet2,iv)
       if (iv /= 6 ) then
          Err_CFML%IErr=1  ! Error
          Err_CFML%Msg=" Error reading the CELL Parameters"
          return
       end if

       celda=vet1
       if (present(std)) std=vet2

       if (present(cell)) then
          if (present(CFrame)) then
             call Set_Crystal_Cell(vet1(1:3),vet1(4:6), Cell, CarType=CFrame, Vscell=vet2(1:3), Vsang=vet2(4:6))
          else
             call Set_Crystal_Cell(vet1(1:3),vet1(4:6), Cell, Vscell=vet2(1:3), Vsang=vet2(4:6))
          end if
       end if

    End Subroutine Read_Cell

    !!----
    !!---- READ_UTHERMS
    !!----    Subroutine to read the anisotropic thermal parameters from a given Line
    !!----    it complets the object Atm.
    !!----    Assumes the string Line has been read from a file and  starts with one of the words
    !!----    u_ij, b_ij or beta
    !!----
    !!---- 04/05/2020
    !!
    Module Subroutine Read_UTherms(Str, Atm)
       !---- Arguments ----!
       character(len=*), intent(in )     :: Str        ! String containing info
       Class (Atm_Type), intent(in out)  :: Atm

       !---- Local variables -----!
       integer                              :: iv,nlong1,n
       character(len=20)                    :: direc
       character(len=len(str)), dimension(1):: linec
       real(kind=cp), dimension (6)         :: vet1,vet2

       !> Init
       call clear_error()

       !> Copy str to line
       line=adjustl(trim(str))

       !> Cut UTherm Directive
       call cut_string(line,nlong1,direc)
       select case (trim(U_case(direc)))
          case ('U_IJ')
             Atm%UType='u_ij'

          case ('B_IJ')
             Atm%UType='b_ij'

          case ('BETA')
             Atm%UType='beta'

          case default
             Err_CFML%IErr=1  ! Error
             Err_CFML%Msg=" Error reading the defining the Thermal type parameter"
             return
       end select

       linec(1)="Uval "//trim(line)
       n=1
       call Read_Key_ValueSTD(linec,n,n,"Uval",vet1,vet2,iv)

       if (iv /= 6) then
          Err_CFML%IErr=1  ! Error
          Err_CFML%Msg=" Error reading the anisotropic thermal parameters of atom:"//atm%lab
          return
       end if

       atm%ThType="aniso"
       atm%U=vet1
       select type(atm)
          class is (Atm_Std_Type)
              atm%U_std=vet2
       end select

    End Subroutine Read_UTherms

    !!----
    !!---- READ_MOMENT
    !!----
    !!---- 04/05/2020
    !!
    Module Subroutine Read_Moment(Str, Atm)
       !---- Arguments ----!
       character(len=*), intent(in )     :: Str        ! String containing info
       Class (Atm_Type), intent(in out)  :: Atm

       !---- Local variables -----!
       integer                              :: iv,nlong1,n
       character(len=20)                    :: direc
       character(len=len(str)), dimension(1):: linec
       real(kind=cp), dimension (6)         :: vet1,vet2

       !> Init
       call clear_error()

       !> Copy str to line
       line=adjustl(trim(str))

       !> Cut Moment Directive
       call cut_string(line,nlong1,direc)
       if (trim(u_case(direc)) /= "MOMENT") then
          Err_CFML%IErr=1  ! Error
          Err_CFML%Msg=" Error reading the Moment parameters"
          return
       end if

       !> Now read the components of the magnetic moment
       linec(1)="moment "//trim(line)
       n=1
       call Read_Key_ValueSTD(linec,n,n,"moment",vet1,vet2,iv)

       !> Moment components
       if (iv < 3) then
          Err_CFML%IErr=1  ! Error
          Err_CFML%Msg="Error reading magnetic moment components of atom: "//trim(atm%lab)
          return
       end if
       atm%moment=vet1(1:3)
       atm%magnetic=.true.
       select type(atm)
          class is (Atm_Std_Type)
              atm%moment_std=vet2(1:3)
       end select
       if (atm%mom < 0.001) atm%mom=maxval(abs(atm%moment))

    End Subroutine Read_Moment

    !!----
    !!---- READ_MODULATION_AMPLITUDES
    !!----
    !!---- 07/05/2020
    !!
    Module Subroutine Read_Modulation_Amplitudes(Str, Atm, Ulabel, Nt)
       !---- Arguments ----!
       character(len=*),      intent(in )     :: str
       class(ModAtm_Std_Type),intent(in out)  :: Atm
       character(len=*),      intent(in)      :: ulabel
       integer,               intent(in)      :: nt  !number of the amplitude

       !---- Local variables -----!
       character(len=len(str)), dimension(1):: linec
       real(kind=cp), dimension (12)        :: vet1,vet2
       integer                              :: iv,n,nq,ier

       !> Init
       call clear_error()

       !> Copy
       linec(1)=str

       !> Cut first word
       call Cut_String(linec(1))

       !> Read Nq
       read(unit=linec(1),fmt=*,iostat=ier) nq
       if (ier /= 0) then
          Err_CFML%Ierr=1
          Err_CFML%Msg="Error reading the Q_coeff number in modulation amplitude line: "//trim(Ulabel)
          return
       end if

       !> Cut Nq
       call Cut_String(linec(1))

       linec(1)=trim(Ulabel)//" "//trim(linec(1))
       n=1
       call Read_Key_ValueSTD(linec, n, n,trim(Ulabel),vet1,vet2,iv)

       Select Case(trim(u_case(Ulabel)))
          case("O_CS")
             if (iv /= 2) then
                ERR_CFML%Ierr=1
                ERR_CFML%Msg="  Error reading the Modulation Amplitudes: "//trim(Ulabel)//" of atom:"//atm%lab
                return
             end if
             Atm%poc_q(nt) = nq
             Atm%Ocs(1:2,nt) = vet1(1:2)

          case("M_CS")
             if (iv /= 6) then
                ERR_CFML%Ierr=1
                ERR_CFML%Msg="  Error reading the Modulation Amplitudes: "//trim(Ulabel)//" of atom:"//atm%lab
                return
             end if
             Atm%pmc_q(nt) = nq
             Atm%Mcs(1:6,nt) = vet1(1:6)

          case("D_CS")
             if (iv /= 6) then
                ERR_CFML%Ierr=1
                ERR_CFML%Msg="  Error reading the Modulation Amplitudes: "//trim(Ulabel)//" of atom:"//atm%lab
                return
             end if
             Atm%pdc_q(nt) = nq
             Atm%Dcs(1:6,nt) = vet1(1:6)

          case("U_CS")
             if (iv /= 12) then
                ERR_CFML%Ierr=1
                ERR_CFML%Msg="  Error reading the Modulation Amplitudes: "//trim(Ulabel)//" of atom:"//atm%lab
                return
             end if
             Atm%puc_q(nt) = nq
             Atm%Ucs(1:12,nt) = vet1(1:12)

       End Select

    End Subroutine Read_Modulation_Amplitudes

    !!----
    !!---- READ_WAVELENGTH
    !!----
    !!----    Read wavelengths and ratio.
    !!----    If no value is read, Lambda1=Lambda2=1.54056 Angstroms, ratio=0.0
    !!----    If only one value is read Lambda1=Lambda2=v1, ratio=0
    !!----    If only two values iare read Lambda1=v1, Lambda2=v2, ratio=0.5
    !!----    In other cases Lambda1=v1, Lambda2=v2, ratio=v3
    !!----
    !!---- 08/05/2020
    !!
    Module Subroutine Read_Wavelength(Str,v1,v2,ratio)
       !---- Arguments ----!
       character(len=*), intent(in)     :: Str
       real(kind=cp),    intent(out)    :: v1,v2
       real(kind=cp),    intent(out)    :: ratio

       !---- Local Variables ----!
       integer                              :: iv, n
       character(len=len(str)), dimension(1):: linec

       !> Init
       call clear_error()
       if (len_trim(str) <=0) then
          ERR_CFML%Ierr=1
          ERR_CFML%Msg=" Empty line in Read_Wavelength procedure !"
          return
       end if

       !> Copy
       v1=1.54056; v2=0.0; ratio=0.0
       linec(1)=str

       !> Reading values
       n=1
       call read_key_value(linec,n,n,"wave",vet,ivet,iv)
       select case (iv)
          case (0)
             v2=1.54056

          case (1)
             v1=vet(1)
             v2=vet(1)

          case (2)
             v1=vet(1)
             v2=vet(2)
             if (abs(v1-v2) > EPS) ratio=0.5

          case (3)
             v1=vet(1)
             v2=vet(2)
             ratio=vet(3)
       end select

    End Subroutine Read_Wavelength

    !!----
    !!---- READ_SPACEGROUP
    !!----
    !!----    Read spacegroup
    !!----
    !!---- 08/05/2020
    !!
    Module Subroutine Read_SpaceGroup(Str,Spg)
       !---- Arguments ----!
       character(len=*),             intent(in)     :: Str
       class(SpG_Type), allocatable, intent(out)    :: SpG

       !---- Local Variables ----!
       integer, parameter       :: NDIR=2
       integer, dimension(NDIR) :: ind

       !> Init
       call clear_error()
       if (len_trim(str) <=0) then
          ERR_CFML%Ierr=1
          ERR_CFML%Msg=" Empty line in Read_SpaceGroup procedure !"
          return
       end if

       ind(1)=index(u_case(str),'SPACEG')
       ind(2)=index(u_case(str),'SPGR')

       if (all(ind ==0)) then
          ERR_CFML%Ierr=1
          ERR_CFML%Msg=" Not found directive for give the SpaceGroup!"
          return
       end if

       !> copy
       line=str

       !> Delet fiirst word
       call cut_string(line)
       if (len_trim(line) <=0) then
          ERR_CFML%Ierr=1
          ERR_CFML%Msg=" Empty line in Read_SpaceGroup procedure !"
          return
       end if

       !> Set SpaceGroup
       call Set_SpaceGroup(line, SpG)

    End Subroutine Read_SpaceGroup

    !!----
    !!---- READ_RNGSINT
    !!----
    !!----    Read range for sintheta/lambda.
    !!----
    !!---- 08/05/2020
    !!
    Module Subroutine Read_RngSintL(Str, v1,v2)
       !---- Arguments ----!
       character(len=*), intent(in)  :: str
       real(kind=cp),    intent(out) :: v1,v2

       !---- Local Variables ----!
       integer                     :: iv, n
       character(len=len(str)), dimension(1):: linec

       !> Init
       call clear_error()
       if (len_trim(str) <=0) then
          ERR_CFML%Ierr=1
          ERR_CFML%Msg=" Empty line in Read_RngSinTHL!"
          return
       end if

       !> Copy
       v1=0.0_cp; v2=1.0_cp
       linec(1)=str

       !> Reading values
       n=1
       call read_key_value(linec,n,n,"rngsl",vet,ivet,iv)
       select case (iv)
          case (1)
             v1=0.0_cp
             v2=vet(1)

          case (2)
             v1=vet(1)
             v2=vet(2)
       end select

    End Subroutine Read_RngSintL

    !!----
    !!---- READ_TRANSF
    !!----
    !!----    Read transformation matrix for changing the space group or cell setting.
    !!----    First the matrix M is read row by row and then the origin in the old setting
    !!----    is finally read. A single line with 12 real numbers should be given.
    !!--<<
    !!----    e.g.: TRANS  m11 m12 m13  m21 m22 m33  m31 m32 m33   o1 o2 o3
    !!----
    !!----    That means       a'=m11 a + m12 b + m13 c
    !!----                     b'=m21 a + m22 b + m23 c
    !!----                     c'=m31 a + m32 b + m33 c
    !!----
    !!----                     X' = inv(Mt) (X-O)
    !!-->>
    !!----
    !!---- 08/05/2020
    !!
    Module Subroutine Read_Transf(str, trans, orig)
       !---- Arguments ----!
       character(len=*),                intent(in)     :: str
       real(kind=cp),dimension(3,3),    intent(out)    :: trans
       real(kind=cp),dimension(3  ),    intent(out)    :: orig

       !---- Local Variables ----!
       integer                      :: iv,n
       character(len=len(str)), dimension(1) :: linec
       character(len=80)                     :: transf_key

       !> Init
       call clear_error()
       if (len_trim(str) <=0) then
          ERR_CFML%Ierr=1
          ERR_CFML%Msg=" Empty line in Read_SpaceGroup procedure !"
          return
       end if

       !> copy
       linec(1)=str

       !> transformation matrix
       n=1
       call read_key_value(linec,n,n,"trans",vet,ivet,iv,"#",transf_key)
       if (iv /= 12 ) then
          !> Try to read the transformation from transf_key
          if (len_trim(transf_key) /= 0) then
             call Get_Transf(transf_key,trans,orig)
             if (ERR_CFML%Ierr == 1) then
                ERR_CFML%Msg=" Bad matrix/origin setting in string: "//trim(transf_key)
                return
             end if
          else
             ERR_CFML%Ierr=1
             ERR_CFML%Msg=" Bad matrix/origin setting..."
             return
          end if

       else
          trans(1,1:3)=vet(1:3)
          trans(2,1:3)=vet(4:6)
          trans(3,1:3)=vet(7:9)
          orig(1:3) = vet(10:12)
       end if

    End Subroutine Read_Transf

    Module Function Charge(label) result(q)
       !---- Arguments ----!
       character(len=*), intent(in):: label   ! Input string containing oxidation state
       integer                     :: q

       !---- local variables ----!
       integer :: iv,n,ier

       q=0
       iv=index(label,"+")
       select case(iv)
          case(0) ! No + sign
             n=index(label,"-")
             select case(n)
                case(2) ! Element with a single character symbol F-1
                   read(unit=label(3:),fmt="(i1)",iostat=ier) q
                   if (ier /= 0) q=0

                case(3) ! Element in the form: F1- or Br-1
                   read(unit=label(2:2),fmt="(i1)",iostat=ier)  q
                   if (ier /= 0) then
                      read(unit=label(4:4),fmt="(i1)",iostat=ier)  q
                      if (ier /= 0) q=0.0
                   end if

                case(4) ! Element in the form: Br1-
                   read(unit=label(3:3),fmt="(i1)",iostat=ier)  q
                   if (ier /= 0) q=0
             end select
             q=-q   ! anions

          case(2) ! Element with a single character symbol C+4
             read(unit=label(3:),fmt="(i1)",iostat=ier)  q
             if (ier /= 0) q=0

          case(3) ! Element in the form: C4+ or Fe+3
             read(unit=label(2:2),fmt="(i1)",iostat=ier)  q
             if (ier /= 0) then
                read(unit=label(4:4),fmt="(i1)",iostat=ier)  q
                if (ier /= 0) q=0
             end if

          case(4) ! Element in the form: Fe3+
             read(unit=label(3:3),fmt="(i1)",iostat=ier)  q
             if (ier /= 0) q=0
       end select

    End Function Charge

End SubModule Format_GEN