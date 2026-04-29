Submodule (CFML_Keywords_Code_Parser) KWC_ReadCodes
   !---- Variables ----!
   implicit none

 Contains

    !!----
    !!---- Module Subroutine Read_RefCodes_File(file_dat,n_ini,n_end,FAtom/MolCrys/Molec/MagStr,Spg)
    !!----    Type(file_list_type),          intent( in)    :: file_dat
    !!----    integer,                       intent( in)    :: n_ini
    !!----    integer,                       intent( in)    :: n_end
    !!----    type(AtList_Type),             intent(in out) :: FAtom
    !!----    or
    !!----    type(mAtom_List_Type),         intent(in out) :: FmAtom
    !!----    or
    !!----    type(MolCrystal_Type),         intent(in out) :: molcrys
    !!----    or
    !!----    type(molecule_type),           intent(in out) :: molec
    !!----    type(AtList_Type)   ,          intent(in out) :: FAtom
    !!----    or
    !!----    type(mAtom_List_Type),         intent(in out) :: FmAtom
    !!----    and type(Magnetic_Domain_type),intent(in out) :: Mag_dom
    !!----
    !!----    type(SPG_Type),                intent(in)     :: Spg
    !!----
    !!----    Subroutine for treatment of Codes controls taken from FAtom/Molcrys/Molec
    !!----
    !!---- Update: March - 2005
    !!---- Update: February - 2012
    !!

    !!--++
    !!--++ Subroutine Read_RefCodes_File_FAtom(file_dat,n_ini,n_end,FAtom,Spg)
    !!--++    Type(file_list_type),    intent( in)    :: file_dat
    !!--++    integer,                 intent( in)    :: n_ini
    !!--++    integer,                 intent( in)    :: n_end
    !!--++    type(AtList_Type),       intent(in out) :: FAtom
    !!--++    type(SPG_Type),          intent(in)     :: Spg
    !!--++
    !!--++    (Overloaded)
    !!--++    Subroutine for treatment of Codes controls
    !!--++
    !!--++ Update: March - 2005
    !!
    Module Subroutine Read_RefCodes_File_FAtom(file_dat,n_ini,n_end,FAtom,Spg)
       !---- Arguments ----!
       Type(file_list_type),     intent( in)    :: file_dat
       integer,                  intent( in)    :: n_ini
       integer,                  intent( in)    :: n_end
       type(AtList_Type),        intent(in out) :: fatom
       type(SPG_Type),           intent(in)     :: Spg

       !---- Local variables ----!
       character(len=132)              :: line
       character(len=132),dimension(5) :: dire
       integer                         :: i,k,nlong
       integer                         :: nop_in_line,Keyv

       call clear_error()

       do i=n_ini,n_end
          line=adjustl(file_dat%line(i))
          if (line(1:1) ==" ") cycle
          if (line(1:1) =="!") cycle
          k=index(line,"!")
          if( k /= 0) line=trim(line(1:k-1))

          select case (l_case(line(1:4)))

             !---- Main Directive: FIX ----!
             case ("fix ", "fixe")
                call Cut_string(line,nlong)
                call split_operations(line,nop_in_line,dire)
                do k=1,nop_in_line
                   select case (l_case(dire(k)(1:4)))
                      case ("xyz ")
                         Keyv=1
                      case ("occ ")
                         Keyv=2
                      case ("biso")
                         Keyv=3
                      case ("bani")
                         Keyv=4
                      case ("all ")
                         Keyv=5
                      case ("cent")
                         Keyv=6
                         return       !!!!
                      case ("orie")
                         Keyv=7
                         return       !!!!
                      case ("ther")
                         Keyv=8
                         return       !!!!
                      case default
                         Keyv=0
                   end select
                   if (Keyv /=0) call Cut_string(dire(k),nlong)
                   call get_refcodes_line(Keyv,"fix",dire(k),fatom,spg)
                   if (Err_CFML%Flag) return
                end do

             !---- Main Directive: VARY ----!
             case ("vary")
                call Cut_string(line,nlong)
                call split_operations(line,nop_in_line,dire)
                do k=1,nop_in_line
                   select case (l_case(dire(k)(1:4)))
                      case ("xyz ")
                         Keyv=1
                      case ("occ ")
                         Keyv=2
                      case ("biso")
                         Keyv=3
                      case ("bani")
                         Keyv=4
                      case ("all ")
                         Keyv=5
                      case ("cent")
                         Keyv=6
                         return     !!!!
                      case ("orie")
                         Keyv=7
                         return     !!!!
                      case ("ther")
                         Keyv=8
                         return     !!!!
                      case default
                         Keyv=0
                   end select
                   if (Keyv /= 0) call Cut_string(dire(k),nlong)
                   call get_refcodes_line(Keyv,"var",dire(k),fatom,spg)
                   if (Err_CFML%Flag) return
                end do

             !---- Main Directive: EQUAL ----!
             case ("equa")
                call Cut_string(line,nlong)
                call get_concodes_line(line,fatom)

             !---- Restraints Cases ----!
             case ("aequ") ! AEQU sigma        (Equal Angles restraints)

             case ("afix") ! AFIX ang sigma    (Angles restraints)
                call Cut_string(line,nlong)
                call get_restang_line(line,fatom)

             case ("dequ") ! DEQU sigma        (Equal Distance restraints)

             case ("dfix") ! DFIX d sigma      (Distance restraints)
                call Cut_string(line,nlong)
                call get_restdis_line(line,fatom)

             case ("flat") ! FLAT

             case ("tequ") ! TEQU sigma        (Equal Torsion angle restraints)

             case ("tfix") ! TFIX ang sigma    (Torsion angle restraints)
                call Cut_string(line,nlong)
                call get_resttor_line(line,fatom)

          end select
       end do
    End Subroutine Read_RefCodes_File_FAtom

    !!--++
    !!--++ Module Subroutine Read_RefCodes_File_MagStr(file_dat,n_ini,n_end,FmAtom,Mag_dom)
    !!--++    Type(file_list_type),    intent( in)    :: file_dat
    !!--++    integer,                 intent( in)    :: n_ini
    !!--++    integer,                 intent( in)    :: n_end
    !!--++    type(mAtom_List_Type),   intent(in out) :: FmAtom
    !!--++    type(Magnetic_Domain_type),intent(in out) :: Mag_dom
    !!--++
    !!--++    Subroutine for treatment of magnetic Codes controls
    !!--++    magnetic clone of Read_RefCodes_File_FmAtom + Magdom
    !!--++    Modified to get the umber of the propagation vector to which the directive refers
    !!--++ Created: February - 2012, updated March 2020
    !!
    Module Subroutine Read_RefCodes_File_MagStr(file_dat,n_ini,n_end,FmAtom,Mag_dom)
       !---- Arguments ----!
       Type(file_list_type),              intent( in)    :: file_dat
       integer,                           intent( in)    :: n_ini
       integer,                           intent( in)    :: n_end
       type(mAtom_List_Type),             intent(in out) :: FmAtom
       type(Magnetic_Domain_type),optional,intent(in out):: Mag_dom

       !---- Local variables ----!
       character(len=132)              :: line
       character(len=132),dimension(5) :: dire
       integer                         :: i,k,nlong,ik,ier
       integer                         :: nop_in_line,Keyv

       call clear_error()

       do i=n_ini,n_end
          line=adjustl(file_dat%line(i))
          if (line(1:1) ==" ") cycle
          if (line(1:1) =="!") cycle
          k=index(line,"!")
          if( k /= 0) line=trim(line(1:k-1))

          !Determine to which propagation vector refers the instruction that must be at the end of the line
          k=index(u_case(line),"KV_")

          if(k == 0) then
            ik=1 !Refers to the first propagation vector
          else
            read(unit=line(k+3:),fmt=*,iostat=ier) ik
            if(ier /= 0) ik=1
            line=line(1:k-1)
          end if

          select case (l_case(line(1:4)))

             !---- Main Directive: FIX ----!
             case ("fix ", "fixe")
                call Cut_string(line,nlong)
                call split_moperations(line,nop_in_line,dire)
                do k=1,nop_in_line
                   select case (l_case(dire(k)(1:4)))
                      case ("rxyz")
                         Keyv=1
                      case ("ixyz")
                         Keyv=2
                      case ("mxyz")
                         Keyv=3
                      case ("magd")
                         Keyv=4
                      case default
                         Keyv=0
                   end select
                   if (Keyv /=0 .and. Keyv /=4) call Cut_string(dire(k),nlong)
                   if (Keyv == 4) then
                     call get_refcodes_line(Keyv,"fix",dire(k),Mag_dom)
                   else
                     call get_refcodes_line(Keyv,"fix",dire(k),ik,FmAtom)
                   end if
                   if (Err_CFML%Flag) return
                end do

             !---- Main Directive: VARY ----!
             case ("vary")
                call Cut_string(line,nlong)
                call split_moperations(line,nop_in_line,dire)
                do k=1,nop_in_line
                   select case (l_case(dire(k)(1:4)))
                      case ("rxyz")
                         Keyv=1
                      case ("ixyz")
                         Keyv=2
                      case ("mxyz")
                         Keyv=3
                      case ("magd")
                         Keyv=4
                      case default
                         Keyv=0
                   end select

                   if (Keyv /= 0 .and. Keyv /= 4) call Cut_string(dire(k),nlong)
                   if (Keyv == 4) then
                    call get_refcodes_line(Keyv,"var",dire(k),Mag_dom)
                   else
                    call get_refcodes_line(Keyv,"var",dire(k),ik,FmAtom)
                   end if
                   if (Err_CFML%Flag) return
                end do

             !---- Main Directive: EQUAL ----! under construction
             case ("equa")
                call Cut_string(line,nlong)
                   if (Keyv == 4) then
                    call get_concodes_line(line,Mag_dom)
                   else
                    call get_concodes_line(line,ik,FmAtom)
                   end if

          end select
       end do
    End Subroutine Read_RefCodes_File_MagStr

    !!--++
    !!--++ Module Subroutine Read_RefCodes_File_Molcrys(file_dat,n_ini,n_end,molcrys)
    !!--++    Type(file_list_type),         intent( in)    :: file_dat
    !!--++    integer,                      intent( in)    :: n_ini
    !!--++    integer,                      intent( in)    :: n_end
    !!--++    type(MolCrystal_Type),        intent(in out) :: molcrys
    !!--++
    !!--++    (Overloaded)
    !!--++    Subroutine for treatment of Codes controls
    !!--++
    !!--++ Update: March - 2005
    !!
    Module Subroutine Read_RefCodes_File_Molcrys(file_dat,n_ini,n_end,molcrys)
       !---- Arguments ----!
       Type(file_list_type),  intent( in)    :: file_dat
       integer,               intent( in)    :: n_ini
       integer,               intent( in)    :: n_end
       type(MolCrystal_Type), intent(in out) :: molcrys

       !---- Local variables ----!
       character(len=132)              :: line
       character(len=132),dimension(5) :: dire
       integer                         :: i,k,npos,nlong,iv
       integer                         :: nop_in_line,Keyv,nmol
       integer, dimension(1)           :: ivet
       real(kind=cp), dimension(1)     :: vet

       call clear_error()

       do i=n_ini,n_end
          line=adjustl(file_dat%line(i))
          if (line(1:1) ==" ") cycle
          if (line(1:1) =="!") cycle
          k=index(line,"!")
          if( k /= 0) line=trim(line(1:k-1))

          nmol=-1
          select case (l_case(line(1:4)))

             !---- Main Directive: FIX ----!
             case ("fix ", "fixe") ! FIX
                call Cut_string(line,nlong)

                !---- Molecule Information ----!
                if (u_case(line(1:3)) =="MOL") then
                   npos=index(line," ")
                   call Get_Num(line(4:npos),vet,ivet,iv)
                   if (iv /= 1) then
                      nmol=0
                   else
                      nmol=ivet(1)
                   end if
                end if
                call Cut_string(line,nlong)

                !---- Splitting Line ----!
                call split_operations(line,nop_in_line,dire)
                do k=1,nop_in_line
                   select case (l_case(dire(k)(1:4)))
                      case ("xyz ")
                         Keyv=1
                      case ("occ ")
                         Keyv=2
                      case ("biso")
                         Keyv=3
                      case ("bani")
                         Keyv=4
                      case ("all ")
                         Keyv=5
                      case ("cent")
                         Keyv=6
                      case ("orie")
                         Keyv=7
                      case ("ther")
                         Keyv=8
                      case default
                         Keyv=0
                   end select
                   if (Keyv /=0) call Cut_string(dire(k),nlong)
                   call get_refcodes_line(Keyv,"fix",dire(k),molcrys,nmol)
                   if (Err_CFML%Flag) return
                end do

             !---- Main Directive: VARY ----!
             case ("vary") ! VARY
                call Cut_string(line,nlong)
                !---- Molecule Information ----!
                if (u_case(line(1:3)) =="MOL") then
                   npos=index(line," ")
                   call Get_Num(line(4:npos),vet,ivet,iv)
                   if (iv /= 1) then
                      nmol=0
                   else
                      nmol=ivet(1)
                   end if
                end if
                call Cut_string(line,nlong)

                !---- Splitting Line ----!
                call split_operations(line,nop_in_line,dire)
                do k=1,nop_in_line
                   select case (l_case(dire(k)(1:4)))
                      case ("xyz ")
                         Keyv=1
                      case ("occ ")
                         Keyv=2
                      case ("biso")
                         Keyv=3
                      case ("bani")
                         Keyv=4
                      case ("all ")
                         Keyv=5
                      case ("cent")
                         Keyv=6
                      case ("orie")
                         Keyv=7
                      case ("ther")
                         Keyv=8
                      case default
                         Keyv=0
                   end select
                   if (Keyv /=0) call Cut_string(dire(k),nlong)
                   call get_refcodes_line(Keyv,"var",dire(k),molcrys,nmol)
                   if (Err_CFML%Flag) return
                end do

             !---- Main Directive: EQUAL ----!
             case ("equa")
                call Cut_string(line,nlong)
                call get_concodes_line(line,molcrys)

             !---- Restraints Cases ----!
             case ("aequ") ! AEQU sigma        (Angles restraints)
             case ("afix") ! AFIX ang sigma    (Angles restraints)
             case ("dequ") ! DEQU sigma        (Distance restraints)
             case ("dfix") ! DFIX d sigma      (Distance restraints)
             case ("flat") ! FLAT
             case ("tequ") ! TEQU sigma        (Torsion angle restraints)
             case ("tfix") ! TFIX ang sigma    (Torsion angle restraints)

          end select
       end do
    End Subroutine Read_RefCodes_File_Molcrys

    !!--++
    !!--++ Module Subroutine Read_RefCodes_File_Molec(file_dat,n_ini,n_end,molec,spg)
    !!--++    Type(file_list_type),   intent( in)    :: file_dat
    !!--++    integer,                intent( in)    :: n_ini
    !!--++    integer,                intent( in)    :: n_end
    !!--++    type(molecule_type),    intent(in out) :: molec
    !!--++    type(SPG_Type),         intent(in)     :: Spg
    !!--++
    !!--++    (Overloaded)
    !!--++    Subroutine for treatment of Codes controls
    !!--++
    !!--++ Update: March - 2005
    !!
    Module Subroutine Read_RefCodes_File_Molec(file_dat,n_ini,n_end,molec,spg)
       !---- Arguments ----!
       Type(file_list_type),   intent( in)    :: file_dat
       integer,                intent( in)    :: n_ini
       integer,                intent( in)    :: n_end
       type(molecule_type),    intent(in out) :: molec
       type(SPG_Type),         intent(in)     :: Spg

       !---- Local variables ----!
       character(len=132)              :: line
       character(len=132),dimension(5) :: dire
       integer                         :: i,k,nlong
       integer                         :: nop_in_line,Keyv

       call clear_error()

       do i=n_ini,n_end
          line=adjustl(file_dat%line(i))
          if (line(1:1) ==" ") cycle
          if (line(1:1) =="!") cycle
          k=index(line,"!")
          if( k /= 0) line=trim(line(1:k-1))

          select case (l_case(line(1:4)))

             !---- Main Directive: FIX ----!
             case ("fix ", "fixe")
                call Cut_string(line,nlong)

                !---- Splitting Line ----!
                call split_operations(line,nop_in_line,dire)
                do k=1,nop_in_line
                   select case (l_case(dire(k)(1:4)))
                      case ("xyz ")
                         Keyv=1
                      case ("occ ")
                         Keyv=2
                      case ("biso")
                         Keyv=3
                      case ("bani")
                         Keyv=4
                         return     !!!!
                      case ("all ")
                         Keyv=5
                      case ("cent")
                         Keyv=6
                      case ("orie")
                         Keyv=7
                      case ("ther")
                         Keyv=8
                      case default
                         Keyv=0
                   end select
                   if (Keyv /=0) call Cut_string(dire(k),nlong)
                   call get_refcodes_line(Keyv,"fix",dire(k),molec,spg)
                   if (Err_CFML%Flag) return
                end do

             !---- Main Directive: VARY ----!
             case ("vary")
                call Cut_string(line,nlong)

                !---- Splitting Line ----!
                call split_operations(line,nop_in_line,dire)
                do k=1,nop_in_line
                   select case (l_case(dire(k)(1:4)))
                      case ("xyz ")
                         Keyv=1
                      case ("occ ")
                         Keyv=2
                      case ("biso")
                         Keyv=3
                      case ("bani")
                         Keyv=4
                         return     !!!!
                      case ("all ")
                         Keyv=5
                      case ("cent")
                         Keyv=6
                      case ("orie")
                         Keyv=7
                      case ("ther")
                         Keyv=8
                      case default
                         Keyv=0
                   end select
                   if (Keyv /=0) call Cut_string(dire(k),nlong)
                   call get_refcodes_line(Keyv,"var",dire(k),molec,spg)
                   if (Err_CFML%Flag) return
                end do

             !---- Main Directive: EQUAL ----!
             case ("equa")
                call Cut_string(line,nlong)
                call get_concodes_line(line,molec)

             !---- Restraints Cases ----!
             case ("aequ") ! AEQU sigma        (Angles restraints)
             case ("afix") ! AFIX ang sigma    (Angles restraints)
             case ("dequ") ! DEQU sigma        (Distance restraints)
             case ("dfix") ! DFIX d sigma      (Distance restraints)
             case ("flat") ! FLAT
             case ("tequ") ! TEQU sigma        (Torsion angle restraints)
             case ("tfix") ! TFIX ang sigma    (Torsion angle restraints)

          end select
       end do
    End Subroutine Read_RefCodes_File_Molec

    !!----
    !!---- Module Subroutine Read_RefGCodes_File(file_dat,n_ini,n_end,model,sys,Iphas)
    !!----    Type(file_list_type),                intent( in)    :: file_dat
    !!----    integer,                             intent( in)    :: n_ini
    !!----    integer,                             intent( in)    :: n_end
    !!----    type(Nonatomic_Parameter_List_Type), intent(in out) :: model
    !!----    character(len=*), optional,          intent( in)    :: sys
    !!----    integer, optional,                   intent( in)    :: Iphas
    !!----
    !!----    Subroutine for treatment of Codes for non-atomic parameters.
    !!----    The optional argument Sys containts the crystal system and the
    !!----    setting in case of the monoclinic, e.g. Sys="Monoclinic b".
    !!----
    !!---- Update: November - 2013
    !!
    Module Subroutine Read_RefGCodes_File(file_dat,n_ini,n_end,model,sys,Iphas)
       Type(file_list_type),                intent( in)    :: file_dat
       integer,                             intent( in)    :: n_ini
       integer,                             intent( in)    :: n_end
       type(Nonatomic_Parameter_List_Type), intent(in out) :: model
       character(len=*), optional,          intent( in)    :: sys
       integer, optional,                   intent( in)    :: Iphas
       !---- Local variables ----!
       character(len=132)              :: line
       character(len=20)               :: namp
       character(len=132),dimension(5) :: dire
       integer                         :: i,k,nlong
       integer                         :: nop_in_line,Keyv

       call clear_error()

       do i=n_ini,n_end
          line=adjustl(file_dat%line(i))
          if (line(1:1) =="!" .or. len_trim(line) == 0) cycle
          namp=" "
          k=index(line,"!")
          if( k /= 0) line=trim(line(1:k-1))

          select case (l_case(line(1:5)))

             !---- Main Directive: GFIX ----!
             case ("gfix ")
                call Cut_string(line,nlong)
                call split_goperations(line,nop_in_line,dire)
                do k=1,nop_in_line
                   namp=trim(l_case(dire(k)))  !keep the name of the parameter in the directive
                   select case (trim(namp))    !Simple general directives Keyv /= 0
                      case ("bkg")
                         Keyv=1
                      case ("cell")
                         Keyv=2
                      case ("uvw")
                         Keyv=3
                      case ("asize")
                         Keyv=4
                      case ("astrain")
                         Keyv=5
                      case ("extinct")
                         Keyv=6
                      case ("scalefs")
                         Keyv=7
                      case ("all")
                         Keyv=8
                      case default  !other parameter names or the directive contains numbers or other options
                         Keyv=0
                   end select
                   if (Keyv /=0) call Cut_string(dire(k),nlong)  !in case Keyv=0  dire(k)=namp, otherwise dire(k) does not contain the generic name of the parameter
                   if(present(iphas)) then
                     call get_refGcodes_line(Keyv,"gfix",dire(k),namp,model,Iphas=iphas)
                   else
                     call get_refGcodes_line(Keyv,"gfix",dire(k),namp,model)
                   end if
                   if (Err_CFML%Flag) return
                end do

             !---- Main Directive: GVARY ----!
             case ("gvary")
                call Cut_string(line,nlong)
                call split_goperations(line,nop_in_line,dire)
                do k=1,nop_in_line
                   namp=trim(l_case(dire(k)))  !keep the name of the parameter in the directive
                   select case (trim(namp))    !Simple general directives Keyv /= 0
                      case ("bkg")
                         Keyv=1
                      case ("cell")
                         Keyv=2
                      case ("uvw")
                         Keyv=3
                      case ("asize")
                         Keyv=4
                      case ("astrain")
                         Keyv=5
                      case ("extinct")
                         Keyv=6
                      case ("scalefs")
                         Keyv=7
                      case ("all")
                         Keyv=8
                      case default  !other parameter names or the directive contains numbers or other options
                         Keyv=0
                   end select
                   if (Keyv /=0) call Cut_string(dire(k),nlong) !in case Keyv=0  dire(k)=namp,otherwise dire(k) does not contain the generic name of the parameter
                   if(present(Sys)) then
                      if(present(iphas)) then
                        call get_refGcodes_line(Keyv,"gvar",dire(k),namp,model,sys,Iphas=iphas)
                      else
                        call get_refGcodes_line(Keyv,"gvar",dire(k),namp,model,sys)
                      end if
                   else
                      if(present(iphas)) then
                        call get_refGcodes_line(Keyv,"gvar",dire(k),namp,model,Iphas=iphas)
                      else
                        call get_refGcodes_line(Keyv,"gvar",dire(k),namp,model)
                      end if
                   end if
                   if (Err_CFML%Flag) return
                end do

             !---- Main Directive: EQUAL ----!
              !case ("equa")
              !   call Cut_string(line,nlong)
              !   call get_congcodes_line(line,model)

          end select
       end do
    End Subroutine Read_RefGCodes_File

End Submodule KWC_ReadCodes
