Submodule (CFML_Keywords_Code_Parser) KWC_RefCodes
   !---- Variables ----!
   implicit none

 Contains
    !!----
    !!---- Module Subroutine Init_RefCodes(FAtom,FmAtom,Mag_dom,MolCrys,Molec,Model)
    !!----    type(AtList_Type),                  optional,intent(in out) :: FAtom   ! Free Atom Object
    !!----    type(mAtom_List_Type),              optional,intent(in out) :: FmAtom  ! Magnetic Atom Object
    !!----    type(Magnetic_Domain_type),         optional,intent(in out) :: Mag_dom ! Magnetic domain object
    !!----    type(MolCrystal_Type),              optional,intent(in out) :: MolCrys ! Molecular Crystal Object
    !!----    type(Molecule_Type),                optional,intent(in out) :: Molec   ! Molecule Object
    !!----    type(Nonatomic_Parameter_List_Type),optional,intent(in out) :: Model   !Non atomic parameter object
    !!----
    !!----
    !!---- Initialize all refinement codes. This subroutine has been merged with individual ones
    !!---- into a single subroutine with optional arguments. It seems more simple because the
    !!---- global set of parameters is nullified and initialized.
    !!----
    !!---- Update: November 2 - 2012
    !!
    Module Subroutine Init_RefCodes(FAtom,FmAtom,Mag_dom,MolCrys,Molec,Model)
       !---- Arguments ----!
       type(AtList_Type),                  optional,intent(in out) :: FAtom   ! Free Atom Object
       type(mAtom_List_Type),              optional,intent(in out) :: FmAtom  ! Magnetic Atom Object
       type(Magnetic_Domain_type),         optional,intent(in out) :: Mag_dom ! Magnetic domain object
       type(MolCrystal_Type),              optional,intent(in out) :: MolCrys ! Molecular Crystal Object
       type(Molecule_Type),                optional,intent(in out) :: Molec   ! Molecule Object
       type(Nonatomic_Parameter_List_Type),optional,intent(in out) :: Model   !Non atomic parameter object

       !---- Local variables ----!
       integer :: i

       !NP_Refi=0  !This has been removed because the initialization of V-arrays
       !NP_Cons=0  !is done in Allocate_VParam
       !
       !V_Vec   =0.0
       !V_Name=" "
       !V_Bounds=0.0
       !V_BCon  =0

       if(present(FAtom)) then
         Select Type(atom => FAtom%atom)
          type is (atm_ref_type)
              do i=1,FAtom%Natoms
                 atom(i)%m_x=0.0
                 atom(i)%l_x=0

                 atom(i)%m_u_iso=0.0
                 atom(i)%l_u_iso=0

                 atom(i)%m_occ=0.0
                 atom(i)%l_occ=0

                 atom(i)%m_u=0.0
                 atom(i)%l_u=0

                 atom(i)%m_moment=0.0
                 atom(i)%l_moment=0
              end do
         End Select !type
       end if

       if(present(FmAtom)) then
          do i=1,FmAtom%Natoms

             FmAtom%atom(i)%mSkR=0.0
             FmAtom%atom(i)%lskr=0

             FmAtom%atom(i)%mSkI=0.0
             FmAtom%atom(i)%lski=0

             FmAtom%atom(i)%mmphas=0.0
             FmAtom%atom(i)%lmphas=0

             FmAtom%atom(i)%mbas=0.0
             FmAtom%atom(i)%lbas=0

          end do
       end if !present FmAtom

       if(present(Mag_dom)) then
          do i=1,Mag_Dom%nd

             Mag_Dom%Mpop(1:2,i)=0.0
             Mag_Dom%Lpop(1:2,i)=0

          end do
       end if  !present Mag_Dom

       if (present(MolCrys)) then
          if (MolCrys%N_Free > 0 .and. allocated(MolCrys%Atm)) then
            Select Type(Atm => MolCrys%Atm)
              type is(atm_ref_type)
                 do i=1,MolCrys%N_Free
                    Atm(i)%m_x=0.0
                    Atm(i)%l_u=0

                    Atm(i)%m_u_iso=0.0
                    Atm(i)%l_u_iso=0

                    Atm(i)%m_occ=0.0
                    Atm(i)%l_occ=0

                    Atm(i)%m_u=0.0
                    Atm(i)%l_u=0
                 end do
            end select !type
          end if
          if (MolCrys%N_Mol > 0 .and. allocated(MolCrys%Mol)) then
             do i=1,MolCrys%N_Mol
                MolCrys%mol(i)%mxcentre=0.0
                MolCrys%mol(i)%lxcentre=0

                MolCrys%mol(i)%morient=0.0
                MolCrys%mol(i)%lorient=0

                MolCrys%mol(i)%mT_TLS=0.0
                MolCrys%mol(i)%lT_TLS=0

                MolCrys%mol(i)%mL_TLS=0.0
                MolCrys%mol(i)%lL_TLS=0

                MolCrys%mol(i)%mS_TLS=0.0
                MolCrys%mol(i)%lS_TLS=0

                if (MolCrys%Mol(i)%natoms > 0) then
                   MolCrys%mol(i)%mI_coor=0.0
                   MolCrys%mol(i)%lI_coor=0

                   molcrys%mol(i)%mu_iso =0.0
                   molcrys%mol(i)%lu_iso =0

                   molcrys%mol(i)%mocc =0.0
                   molcrys%mol(i)%locc =0
                end if
             end do
          end if
       end if !present MolCrys

       if(present(Molec)) then
          Molec%mxcentre=0.0
          Molec%lxcentre=0

          Molec%morient=0.0
          Molec%lorient=0

          Molec%mT_TLS=0.0
          Molec%lT_TLS=0

          Molec%mL_TLS=0.0
          Molec%lL_TLS=0

          Molec%mS_TLS=0.0
          Molec%lS_TLS=0

          do i=1,molec%natoms
             Molec%mI_coor(:,i)=0.0
             Molec%lI_coor(:,i)=0

             Molec%mu_iso(i)=0.0
             Molec%lu_iso(i)=0

             molec%mocc(i)=0.0
             molec%locc(i)=0
          end do
       end if !if present Molec

       if(present(Model)) then
         do i=1,Model%npar
            Model%par(i)%multip=0.0
            Model%par(i)%Lcode=0
         end do
       end if
    End Subroutine Init_RefCodes

    !!--++
    !!--++ Module Subroutine Get_RefCodes_Line(Keyv,Dire,Line,FAtom/FmAtom/MolCrys/Molec/MagDom,Spg)
    !!--++    integer,                      intent(in)     :: Keyv
    !!--++    character(len=*),             intent(in)     :: Dire
    !!--++    character(len=*),             intent(in)     :: Line
    !!--++    type(AtList_Type),            intent(in out) :: FAtom
    !!--++    or
    !!--++    type(mAtom_List_Type),        intent(in out) :: FmAtom
    !!--++    or
    !!--++    type(MolCrystal_Type), intent(in out) :: MolCrys
    !!--++    or
    !!--++    type(molecule_type),          intent(in out) :: Molec
    !!--++    or
    !!--++    type(Magnetic_Domain_type),   intent(in out) :: Magdom
    !!--++
    !!--++    type(SPG_Type),       intent(in)     :: Spg
    !!--++
    !!--++    (Private)
    !!--++    Get Refinement Codes for Free atoms type
    !!--++
    !!--++ Update: March - 2005
    !!

    !!--++
    !!--++ Module Subroutine Get_RefCodes_Line_FAtom(Keyv,Dire,Line,FAtom,Spg)
    !!--++    integer,                 intent(in)     :: Keyv
    !!--++    character(len=*),        intent(in)     :: Dire
    !!--++    character(len=*),        intent(in)     :: Line
    !!--++    type(AtList_Type),       intent(in out) :: FAtom
    !!--++    type(SPG_Type),  intent(in)     :: Spg
    !!--++
    !!--++ Overloaded
    !!--++ Get Refinement Codes for Free atoms type
    !!--++
    !!--++ Update: March - 2005
    !!
    Module Subroutine Get_RefCodes_Line_FAtom(Keyv,Dire,Line,FAtom,Spg)
       !---- Arguments ----!
       integer,                 intent(in)     :: Keyv
       character(len=*),        intent(in)     :: Dire
       character(len=*),        intent(in)     :: Line
       type(AtList_Type),       intent(in out) :: FAtom
       type(SPG_Type),  intent(in)     :: Spg

       !---- Local Variables ----!
       character(len=20), dimension(30) :: label
       integer                          :: i,j,n,na,nb,ndir,npos,nlong,ic !,k,nc
       integer                          :: icond,iv,n_ini,n_end
       integer, dimension(5)            :: ivet
       integer, dimension(30)           :: ilabel
       real(kind=cp)                    :: x_low,x_up,x_step
       real(kind=cp),dimension(5)       :: vet

       call clear_error()
       nlong=len_trim(line)

       if (nlong == 0) then
          !---- Default Values ----!
          do i=1,FAtom%natoms
             call Fill_RefCodes(Keyv,Dire,i,0,0.0_cp,0.0_cp,0.0_cp,0,Fatom,Spg)
          end do

       else
          !---- VARY/FIX Line: [LABEL, [INF,[SUP,[STEP,[COND]]]]] ----!
          ilabel=0
          call Get_Words(line,label,ic)
          do i=1,ic
             call Get_Num(label(i),vet,ivet,iv)
             if (iv == 1) ilabel(i)=1
          end do
          ndir=count(ilabel(1:ic) < 1)

          if (ndir <=0) then
             !--- [INF,[SUP,[STEP,[COND]]]] ----!
             call Get_Num(line,vet,ivet,iv)
             select case (iv)
                case (1)
                   x_low=vet(1)
                    x_up=vet(1)
                  x_step=0.0
                   icond=0

                case (2)
                   x_low=vet(1)
                    x_up=vet(2)
                  x_step=0.0
                   icond=0

                case (3)
                   x_low=vet(1)
                    x_up=vet(2)
                  x_step=vet(3)
                   icond=0

                case (4)
                   x_low=vet(1)
                    x_up=vet(2)
                  x_step=vet(3)
                   icond=ivet(4)

                case default
                   Err_CFML%Flag=.true.
                   Err_CFML%Msg="Only numbers in "//trim(line)
                   return
             end select

             nb=0
             do na=1,FAtom%natoms
                call fill_refcodes(Keyv,dire,na,nb,x_low,x_up,x_step,icond,fatom,spg)
             end do
             if (Err_CFML%Flag) return

          else
             !---- [LABEL, [INF,[SUP,[STEP,[COND]]]]] ----!
             ! If Ilabel(i) == 0 then is a label otherwise is a number
             n_ini=minloc(ilabel,dim=1)
             ilabel(n_ini)=2
             n_end=minloc(ilabel,dim=1)-1

             do n=1,ndir
                na=0
                nb=0

                !---- Default values ----!
                x_low =0.0
                x_up  =0.0
                x_step=0.0
                icond =0

                !---- Label ----!
                npos=index(label(n_ini),"_")
                if (npos >0) then
                   do j=1,ncode
                      if (u_case(label(n_ini)(1:npos))==u_case(trim(code_nam(j)))) then
                         nb=j
                         exit
                      end if
                   end do
                   if (nb == 0) then
                      Err_CFML%Flag=.true.
                      Err_CFML%Msg="Code-name not found for "//trim(label(n_ini))
                      return
                   end if
                end if

                do j=1,FAtom%natoms
                   if (u_case(fatom%atom(j)%lab) == u_case(label(n_ini)(npos+1:npos+6))) then
                      na=j
                      exit
                   end if
                end do
                if (na == 0) then
                   Err_CFML%Flag=.true.
                   Err_CFML%Msg=" Atom label not found for "//trim(line)
                   return
                end if

                !---- Valu List: Inf,Sup,Step,Cond ----!
                i=0
                do j=n_ini+1,n_end
                   i=i+1
                   call Get_Num(label(j),vet,ivet,iv)
                   select case (i)
                      case (1)
                         x_low=vet(1)
                         x_up =vet(1)

                      case (2)
                         x_up =vet(1)

                      case (3)
                         x_step =vet(1)

                      case (4)
                         icond = ivet(1)
                   end select
                end do

                call fill_refcodes(Keyv,dire,na,nb,x_low,x_up,x_step,icond,fatom,spg)
                if (Err_CFML%Flag) return

                n_ini=minloc(ilabel,dim=1)
                ilabel(n_ini)=2
                n_end=minloc(ilabel,dim=1)-1

             end do
          end if

       end if
    End Subroutine Get_RefCodes_Line_FAtom

    !!--++
    !!--++ Module Subroutine Get_RefCodes_Line_FmAtom(Keyv,Dire,Line,ik,FmAtom)
    !!--++    integer,                 intent(in)     :: Keyv
    !!--++    character(len=*),        intent(in)     :: Dire
    !!--++    character(len=*),        intent(in)     :: Line
    !!--++    integer,                 intent(in)     :: ik
    !!--++    type(mAtom_List_Type),   intent(in out) :: FmAtom
    !!--++
    !!--++ Get Refinement Codes for Free Magnetic Atom type
    !!--++ magnetic clone of Get_RefCodes_Line_FAtom
    !!--++ Created: December - 2011
    !!--++ Updated: February - 2012
    !!
    Module Subroutine Get_RefCodes_Line_FmAtom(Keyv,Dire,Line,ik,FmAtom)
       !---- Arguments ----!
       integer,                 intent(in)     :: Keyv
       character(len=*),        intent(in)     :: Dire
       character(len=*),        intent(in)     :: Line
       integer,                 intent(in)     :: ik
       type(mAtom_List_Type),   intent(in out) :: FmAtom

       !---- Local Variables ----!
       character(len=20), dimension(30) :: label
       integer                          :: i,j,n,na,nb,ndir,npos,nlong,ic
       integer                          :: icond,iv,n_ini,n_end
       integer, dimension(5)            :: ivet
       integer, dimension(30)           :: ilabel
       real(kind=cp)                    :: x_low,x_up,x_step
       real(kind=cp),dimension(5)       :: vet


       call clear_error()


       nlong=len_trim(line)
       if (nlong == 0) then
          !---- Default Values ----!
          do i=1,FmAtom%natoms
             call Fill_Refcodes(Keyv,Dire,i,0,0.0_cp,0.0_cp,0.0_cp,0,ik,FmAtom)
          end do

       else
          !---- VARY/FIX Line: [LABEL, [INF,[SUP,[STEP,[COND]]]]] ----!
          ilabel=0
          call Get_Words(line,label,ic)
          do i=1,ic
             call Get_Num(label(i),vet,ivet,iv)
             if (iv == 1) ilabel(i)=1
          end do
          ndir=count(ilabel(1:ic) < 1)

          if (ndir <= 0) then
             !--- [INF,[SUP,[STEP,[COND]]]] ----!
             call Get_Num(line,vet,ivet,iv)
             select case (iv)
                case (1)
                   x_low=vet(1)
                    x_up=vet(1)
                  x_step=0.0
                   icond=0

                case (2)
                   x_low=vet(1)
                    x_up=vet(2)
                  x_step=0.0
                   icond=0

                case (3)
                   x_low=vet(1)
                    x_up=vet(2)
                  x_step=vet(3)
                   icond=0

                case (4)
                   x_low=vet(1)
                    x_up=vet(2)
                  x_step=vet(3)
                   icond=ivet(4)

                case default
                   Err_CFML%Flag=.true.
                   Err_CFML%Msg="Only numbers in "//trim(line)
                   return
             end select

             nb=0
             do na=1,FmAtom%natoms
                call Fill_Refcodes(Keyv,dire,na,nb,x_low,x_up,x_step,icond,ik,FmAtom)
             end do
             if (Err_CFML%Flag) return

          else

             !---- [LABEL, [INF,[SUP,[STEP,[COND]]]]] ----!
             ! If Ilabel(i) == 0 then is a label otherwise is a number
             n_ini=minloc(ilabel,dim=1)
             ilabel(n_ini)=2
             n_end=minloc(ilabel,dim=1)-1

             do n=1,ndir
                na=0
                nb=0

                !---- Default values ----!
                x_low =0.0
                x_up  =0.0
                x_step=0.0
                icond =0

                !---- Label ----!
                npos=index(label(n_ini),"_")
                if (npos > 0) then
                   do j=1,mncode
                      if (u_case(label(n_ini)(1:npos))==u_case(trim(mcode_nam(j)))) then
                         nb=j
                         exit
                      end if
                   end do
                   if (nb == 0) then
                      Err_CFML%Flag=.true.
                      Err_CFML%Msg="Code-name not found for "//trim(label(n_ini))
                      return
                   end if
                end if

                do j=1,FmAtom%natoms
                   if (u_case(FmAtom%atom(j)%lab) == u_case(label(n_ini)(npos+1:npos+6))) then
                      na=j
                      exit
                   end if
                end do
                if (na == 0) then
                   Err_CFML%Flag=.true.
                   Err_CFML%Msg=" Atom label not found for "//trim(line)
                   return
                end if

                !---- Valu List: Inf,Sup,Step,Cond ----!
                i=0
                do j=n_ini+1,n_end
                   i=i+1
                   call Get_Num(label(j),vet,ivet,iv)
                   select case (i)
                      case (1)
                         x_low=vet(1)
                         x_up =vet(1)

                      case (2)
                         x_up =vet(1)

                      case (3)
                         x_step =vet(1)

                      case (4)
                         icond = ivet(1)
                   end select
                end do

                call Fill_Refcodes(Keyv,dire,na,nb,x_low,x_up,x_step,icond,ik,FmAtom)
                if (Err_CFML%Flag) return

                n_ini=minloc(ilabel,dim=1)
                ilabel(n_ini)=2
                n_end=minloc(ilabel,dim=1)-1

             end do
          end if

       end if
    End Subroutine Get_RefCodes_Line_FmAtom

    !!--++
    !!--++ Module Subroutine Get_RefCodes_Line_Molcrys(Keyv,Dire,Line,Molcrys,NMol)
    !!--++   integer,                      intent(in)     :: Keyv
    !!--++   character(len=*),             intent(in)     :: Dire
    !!--++   character(len=*),             intent(in)     :: Line
    !!--++   type(MolCrystal_Type), intent(in out) :: MolCrys
    !!--++   integer,                      intent(in)     :: NMol
    !!--++
    !!--++ Overloaded
    !!--++ Get Refinement Codes for Free atoms type
    !!--++
    !!--++ Update: March - 2005
    !!
    Module Subroutine Get_RefCodes_Line_Molcrys(Keyv,Dire,Line,Molcrys,NMol)
       !---- Arguments ----!
       integer,                      intent(in)     :: Keyv
       character(len=*),             intent(in)     :: Dire
       character(len=*),             intent(in)     :: Line
       type(MolCrystal_Type),        intent(in out) :: MolCrys
       integer,                      intent(in)     :: NMol

       !---- Local Variables ----!
       character(len=20), dimension(30) :: label
       integer                          :: i,j,k,n,na,nb,ndir,npos,nlong,ic !,nc
       integer                          :: icond,iv,n_ini,n_end
       integer, dimension(5)            :: ivet
       integer, dimension(30)           :: ilabel
       real(kind=cp)                    :: x_low,x_up,x_step
       real(kind=cp),dimension(5)       :: vet


       call clear_error()

       nlong=len_trim(line)
       if (nlong ==0) then
          !---- Default values ----!
          select case (NMol)
             case (-1)
                !---- No Molecule Information ----!
                do i=1,molcrys%n_free
                   call Fill_RefCodes(Keyv,Dire,i,0,0.0_cp,0.0_cp,0.0_cp,0,Molcrys,NMol)
                end do

             case (0)
                do k=1,molcrys%n_mol
                   do i=1,molcrys%mol(k)%natoms
                      if (k > 1) then
                         na=molcrys%n_free+sum(molcrys%mol(1:k-1)%natoms)+i
                      else
                         na=molcrys%n_free+i
                      end if
                      call Fill_RefCodes(Keyv,Dire,na,0,0.0_cp,0.0_cp,0.0_cp,0,Molcrys,NMol)
                   end do
                end do

             case (1:)
                do i=1,molcrys%mol(nmol)%natoms
                   if (nmol > 1) then
                      na=molcrys%n_free+sum(molcrys%mol(1:nmol-1)%natoms)+i
                   else
                      na=molcrys%n_free+i
                   end if
                   call Fill_RefCodes(Keyv,Dire,na,0,0.0_cp,0.0_cp,0.0_cp,0,Molcrys,NMol)
                end do
          end select

       else
          !---- VARY/FIX Line: [LABEL,[INF,[SUP,[STEP,[COND]]]]] ----!
          ilabel=0
          call Get_Words(line,label,ic)
          do i=1,ic
             call Get_Num(label(i),vet,ivet,iv)
             if (iv == 1) ilabel(i)=1
          end do
          ndir=count(ilabel(1:ic) < 1)

          if (ndir <=0) then
             !---- [INF,[SUP,[STEP,[COND]]]] ----!
             call Get_Num(line,vet,ivet,iv)
             select case (iv)
                case (1)
                   x_low=vet(1)
                    x_up=vet(1)
                  x_step=0.0
                   icond=0

                case (2)
                   x_low=vet(1)
                    x_up=vet(2)
                  x_step=0.0
                   icond=0

                case (3)
                   x_low=vet(1)
                    x_up=vet(2)
                  x_step=vet(3)
                   icond=0

                case (4)
                   x_low=vet(1)
                    x_up=vet(2)
                  x_step=vet(3)
                   icond=ivet(4)

                case default
                   Err_CFML%Flag=.true.
                   Err_CFML%Msg="Only numbers in "//trim(line)
                   return
             end select

             nb=0
             select case (nmol)
                case (-1)
                   !---- No Molecule Information ----!
                   do na=1,molcrys%n_free
                      call fill_refcodes(Keyv,dire,na,nb,x_low,x_up,x_step,icond,Molcrys,NMol)
                   end do

                case ( 0)
                   !---- For all Molecules Defined ----!
                   do n=1,molcrys%n_mol
                      do i=1,molcrys%mol(n)%natoms
                         if (n > 1) then
                            na=molcrys%n_free+sum(molcrys%mol(1:n-1)%natoms)+i
                         else
                            na=molcrys%n_free+i
                         end if
                      end do
                      call fill_refcodes(Keyv,dire,na,nb,x_low,x_up,x_step,icond,Molcrys,NMol)
                   end do

                case (1:)
                   !---- Particular molecule ----!
                   do i=1,molcrys%mol(nmol)%natoms
                      if (nmol > 1) then
                         na=molcrys%n_free+sum(molcrys%mol(1:nmol-1)%natoms)+i
                      else
                         na=molcrys%n_free+i
                      end if
                      call fill_refcodes(Keyv,dire,na,nb,x_low,x_up,x_step,icond,Molcrys,NMol)
                   end do
             end select
             if (Err_CFML%Flag) return

          else
             !---- [LABEL,[INF,[SUP,[STEP,[COND]]]]] ----!
             ! If Ilabel(i) == 0 then is a label otherwise is a number
             n_ini=minloc(ilabel,dim=1)
             ilabel(n_ini)=2
             n_end=minloc(ilabel,dim=1)-1

             do n=1,ndir
                na=0
                nb=0

                !---- Deafult values ----!
                x_low =0.0
                x_up  =0.0
                x_step=0.0
                icond =0

                !---- Label ----!
                npos=index(label(n_ini),"_")
                if (npos >0) then
                   do j=1,ncode
                      if (u_case(label(n_ini)(1:npos))==u_case(trim(code_nam(j)))) then
                         nb=j
                         exit
                      end if
                   end do
                   if (nb == 0) then
                      Err_CFML%Flag=.true.
                      Err_CFML%Msg="Code-name not found for "//trim(label(n_ini))
                      return
                   end if
                end if

                select case (nmol)
                   case (-1)
                      !---- No molecule Information ----!
                      do j=1,molcrys%n_free
                         if (u_case(molcrys%atm(j)%lab) == u_case(label(n_ini)(npos+1:npos+6))) then
                            na=j
                            exit
                         end if
                      end do

                   case ( 0)
                      !---- For all molecules defined ----!
                      do i=1,molcrys%n_mol
                         do j=1,molcrys%mol(i)%natoms
                            if (u_case(molcrys%mol(i)%Atname(j)) == u_case(label(n_ini)(npos+1:npos+6))) then
                               if (j > 1) then
                                  na=molcrys%n_free+sum(molcrys%mol(1:j-1)%natoms)+j
                               else
                                  na=molcrys%n_free+j
                               end if
                               exit
                            end if
                         end do
                      end do

                   case (1:)
                      !---- Particular Molecule ----!
                      do j=1,molcrys%mol(nmol)%natoms
                         if (u_case(molcrys%mol(nmol)%Atname(j)) == u_case(label(n_ini)(npos+1:npos+6))) then
                            if (j > 1) then
                               na=molcrys%n_free+sum(molcrys%mol(1:j-1)%natoms)+j
                            else
                               na=molcrys%n_free+j
                            end if
                            exit
                         end if
                      end do
                end select
                if (na == 0) then
                   Err_CFML%Flag=.true.
                   Err_CFML%Msg=" Atom label not found for "//trim(line)
                   return
                end if

                !---- Valu List: Inf,Sup,Step,Cond ----!
                i=0
                do j=n_ini+1,n_end
                   i=i+1
                   call Get_Num(label(j),vet,ivet,iv)
                   select case (i)
                      case (1)
                         x_low=vet(1)
                         x_up =vet(1)

                      case (2)
                         x_up =vet(1)

                      case (3)
                         x_step =vet(1)

                      case (4)
                         icond = ivet(1)
                   end select
                end do

                call fill_refcodes(Keyv,dire,na,nb,x_low,x_up,x_step,icond,molcrys,NMol)
                if (Err_CFML%Flag) return

                n_ini=minloc(ilabel,dim=1)
                ilabel(n_ini)=2
                n_end=minloc(ilabel,dim=1)-1

             end do
          end if
       end if
    End Subroutine Get_RefCodes_Line_Molcrys

    !!--++
    !!--++ Module Subroutine Get_RefCodes_Line_Molec(Keyv,Dire,Line,Molec,Spg)
    !!--++    integer,                      intent(in)     :: Keyv
    !!--++    character(len=*),             intent(in)     :: Dire
    !!--++    character(len=*),             intent(in)     :: Line
    !!--++    type(molecule_type),          intent(in out) :: Molec
    !!--++    type(SPG_Type),       intent(in)     :: Spg
    !!--++
    !!--++ Overloaded
    !!--++ Get Refinement Codes for Free atoms type
    !!--++
    !!--++ Update: March - 2005
    !!
    Module Subroutine Get_RefCodes_Line_Molec(Keyv,Dire,Line,Molec,Spg)
       !---- Arguments ----!
       integer,                      intent(in)     :: Keyv
       character(len=*),             intent(in)     :: Dire
       character(len=*),             intent(in)     :: Line
       type(molecule_type),          intent(in out) :: Molec
       type(SPG_Type),               intent(in)     :: Spg

       !---- Local Variables ----!
       character(len=20), dimension(30) :: label
       integer                          :: i,j,n,na,nb,ndir,npos,nlong,ic !,k,nc
       integer                          :: icond,iv,n_ini,n_end
       integer, dimension(5)            :: ivet
       integer, dimension(30)           :: ilabel
       real(kind=cp)                    :: x_low,x_up,x_step
       real(kind=cp),dimension(5)       :: vet

       call clear_error()

       nlong=len_trim(line)

       if (nlong ==0) then
          !---- Default values ----!
          do i=1,molec%natoms
             call Fill_RefCodes(Keyv,dire,i,0,0.0_cp,0.0_cp,0.0_cp,0,molec,spg)
          end do

       else
          !---- VARY/FIX Line: [LABEL,[INF,[SUP,[STEP,[COND]]]]] ----!
          ilabel=0
          call Get_Words(line,label,ic)
          do i=1,ic
             call Get_Num(label(i),vet,ivet,iv)
             if (iv == 1) ilabel(i)=1
          end do
          ndir=count(ilabel(1:ic) < 1)

          if (ndir <=0) then
             !---- [INF,[SUP,[STEP,[COND]]]] ----!
             call Get_Num(line,vet,ivet,iv)
             select case (iv)
                case (1)
                   x_low=vet(1)
                    x_up=vet(1)
                  x_step=0.0
                   icond=0

                case (2)
                   x_low=vet(1)
                    x_up=vet(2)
                  x_step=0.0
                   icond=0

                case (3)
                   x_low=vet(1)
                    x_up=vet(2)
                  x_step=vet(3)
                   icond=0

                case (4)
                   x_low=vet(1)
                    x_up=vet(2)
                  x_step=vet(3)
                   icond=ivet(4)

                case default
                   Err_CFML%Flag=.true.
                   Err_CFML%Msg="Only numbers in "//trim(line)
                   return
             end select

             nb=0
             do na=1,molec%natoms
                call fill_refcodes(Keyv,dire,na,nb,x_low,x_up,x_step,icond,molec,spg)
             end do
             if (Err_CFML%Flag) return

          else
             !---- [LABEL,[INF,[SUP,[STEP,[COND]]]]] ----!
             ! If Ilabel(i) == 0 then is a label otherwise is a number
             n_ini=minloc(ilabel,dim=1)
             ilabel(n_ini)=2
             n_end=minloc(ilabel,dim=1)-1

             do n=1,ndir
                na=0
                nb=0

                !---- Default values ----!
                x_low =0.0
                x_up  =0.0
                x_step=0.0
                icond =0

                !---- Label ----!
                npos=index(label(n_ini),"_")
                if (npos >0) then
                   do j=1,ncode
                      if (u_case(label(n_ini)(1:npos))==u_case(trim(code_nam(j)))) then
                         nb=j
                         exit
                      end if
                   end do
                   if (nb == 0) then
                      Err_CFML%Flag=.true.
                      Err_CFML%Msg="Code-name not found for "//trim(label(n_ini))
                      return
                   end if
                end if

                do j=1,molec%natoms
                   if (u_case(molec%AtName(j)) == u_case(label(n_ini)(npos+1:npos+6))) then
                      na=j
                      exit
                   end if
                end do
                if (na == 0) then
                   Err_CFML%Flag=.true.
                   Err_CFML%Msg=" Atom label not found for "//trim(line)
                   return
                end if

                !---- Value List: Inf,Sup,Step,Cond ----!
                i=0
                do j=n_ini+1,n_end
                   i=i+1
                   call Get_Num(label(j),vet,ivet,iv)
                   select case (i)
                      case (1)
                         x_low=vet(1)
                         x_up =vet(1)

                      case (2)
                         x_up =vet(1)

                      case (3)
                         x_step =vet(1)

                      case (4)
                         icond = ivet(1)
                   end select
                end do

                call fill_refcodes(Keyv,dire,na,nb,x_low,x_up,x_step,icond,molec,spg)
                if (Err_CFML%Flag) return

                n_ini=minloc(ilabel,dim=1)
                ilabel(n_ini)=2
                n_end=minloc(ilabel,dim=1)-1

             end do
          end if

       end if
    End Subroutine Get_RefCodes_Line_Molec

    !!--++
    !!--++ Module Subroutine Get_RefCodes_Line_Magdom(Keyv,Dire,Line,Mag_dom)
    !!--++    integer,                   intent(in)     :: Keyv
    !!--++    character(len=*),          intent(in)     :: Dire
    !!--++    character(len=*),          intent(in)     :: Line
    !!--++    type(Magnetic_Domain_type),intent(in out) :: Mag_dom
    !!--++
    !!--++ Get Refinement Codes for Magnetic domain
    !!--++ Created: February - 2012
    !!
    !!
    Module Subroutine Get_RefCodes_Line_Magdom(Keyv,Dire,Line,Mag_dom)
       !---- Arguments ----!
       integer,                   intent(in)     :: Keyv
       character(len=*),          intent(in)     :: Dire
       character(len=*),          intent(in)     :: Line
       type(Magnetic_Domain_type),intent(in out) :: Mag_dom

       !---- Local Variables ----!
       character(len=20), dimension(30) :: label
       integer                          :: i,na,nb,ndir,nlong,ic,ich
       integer                          :: icond,iv
       integer, dimension(5)            :: ivet
       integer, dimension(30)           :: ilabel
       real(kind=cp)                    :: x_low,x_up,x_step
       real(kind=cp),dimension(5)       :: vet

       !---- Check is chirality is present ----!
        if (Mag_Dom%chir) then
         ich=2
        else
         ich=1
        end if

       call clear_error()

       nlong=len_trim(line)

       if (nlong ==0) then

          !---- Default Values ----!
        do i=1,Mag_Dom%nd*ich
             call Fill_Refcodes(Keyv,Dire,i,0,0.0_cp,0.0_cp,0.0_cp,0,Mag_dom)
        end do

       else
          !---- VARY/FIX Line: [LABEL, [INF,[SUP,[STEP,[COND]]]]] ----!
          ilabel=0
          call Get_Words(line,label,ic)
          do i=1,ic
             call Get_Num(label(i),vet,ivet,iv)
             if (iv == 1) ilabel(i)=1
          end do
          ndir=count(ilabel(1:ic) < 1) !number of refined domains

                !---- Default values ----!
                x_low =0.0
                x_up  =1.0
                x_step=0.01
                icond =1

          do na=1,Mag_Dom%nd
           do nb=1,ich
            do i=1,ic
             if ( u_case(Mag_dom%lab(nb,na)) == u_case(label(i)) ) &
              call Fill_Refcodes(Keyv,dire,na,nb,x_low,x_up,x_step,icond,Mag_dom)
            end do
           end do
          end do

             return
       end if
    End Subroutine Get_RefCodes_Line_Magdom
    !!----
    !!---- Module Subroutine Get_RefGCodes_Line(Keyv,Dire,Line,namp,model,Sys,Iphas)
    !!----    integer,                             intent(in)     :: Keyv
    !!----    character(len=*),                    intent(in)     :: Dire ! "gvary" or "gfix"
    !!----    character(len=*),                    intent(in)     :: Line ! directive after gvary or gfix
    !!----    character(len=*),                    intent(in)     :: namp ! name of the parameter to be fixed or refined
    !!----    type(Nonatomic_Parameter_List_Type), intent(in out) :: model
    !!----    character(len=*), optional,          intent( in)    :: sys
    !!----    integer,          optional,          intent( in)    :: Iphas
    !!----
    !!---- Get Refinement Codes for non-atomic parameters in the current line
    !!----
    !!---- Update: November 1 - 2013
    !!
    Module Subroutine Get_RefGCodes_Line(Keyv,Dire,Line,namp,model,sys,Iphas)
       integer,                             intent(in)     :: Keyv
       character(len=*),                    intent(in)     :: Dire
       character(len=*),                    intent(in)     :: Line
       character(len=*),                    intent(in)     :: namp
       type(Nonatomic_Parameter_List_Type), intent(in out) :: model
       character(len=*), optional,          intent(in)     :: sys
       integer,          optional,          intent( in)    :: Iphas

       !---- Local Variables ----!
       character(len=20), dimension(30) :: label
       character(len=20)                :: new_label,aux_string
       character(len=2)                 :: phase
       integer                          :: i,j,n,na,nb,ndir,nlong,ic !,k,nc
       integer                          :: icond,iv,n_ini,n_end
       integer, dimension(5)            :: ivet
       integer, dimension(30)           :: ilabel
       real(kind=cp)                    :: x_low,x_up,x_step
       real(kind=cp),dimension(5)       :: vet

       call clear_error()
       nlong=len_trim(line)  !length of the directive, eg. for "cell_a  5 6 0.1 0", nlong=17

       if (nlong ==0) then  !In this case "Keyv" cannot be zero and "namp" does not contain numbers
          !---- Default Values ----!
          if(present(sys)) then
            if(present(Iphas)) then
               call Fill_RefGCodes(Keyv,Dire,namp,0.0_cp,0.0_cp,0.0_cp,0,model,sys,Iphas)
            else
               call Fill_RefGCodes(Keyv,Dire,namp,0.0_cp,0.0_cp,0.0_cp,0,model,sys)
            end if
          else
            if(present(Iphas)) then
               call Fill_RefGCodes(Keyv,Dire,namp,0.0_cp,0.0_cp,0.0_cp,0,model,Iphas=Iphas)
            else
               call Fill_RefGCodes(Keyv,Dire,namp,0.0_cp,0.0_cp,0.0_cp,0,model)
            end if
          end if

       else
          !---- GVARY/GFIX Line: [LABEL, [INF,[SUP,[STEP,[COND]]]]] ----!
          ilabel=0
          call Get_Words(line,label,ic)
          do i=1,ic
             call Get_Num(label(i),vet,ivet,iv)
             if (iv == 1) ilabel(i)=1
          end do
          ndir=count(ilabel(1:ic) < 1)  !Counts the number of zeroes  (label(1) contains always the name of the parameter)
                                        !This means the number of keywords

          if (ndir <= 0) then  !label(i) contain only numbers (no more keywords!)
             !--- [INF,[SUP,[STEP,[COND]]]] ----! It corresponds to a generic Keyv /= 0 keyword plus numbers
             call Get_Num(line,vet,ivet,iv)

             select case (iv)
                case (1)
                   x_low=vet(1)
                    x_up=vet(1)
                  x_step=0.0
                   icond=0

                case (2)
                   x_low=vet(1)
                    x_up=vet(2)
                  x_step=0.0
                   icond=0

                case (3)
                   x_low=vet(1)
                    x_up=vet(2)
                  x_step=vet(3)
                   icond=0

                case (4)
                   x_low=vet(1)
                    x_up=vet(2)
                  x_step=vet(3)
                   icond=ivet(4)

                case default
                   Err_CFML%Flag=.true.
                   Err_CFML%Msg="Maximum of 4 numbers in "//trim(namp)
                   return
             end select

             if(present(sys)) then
               if(present(Iphas)) then
                  call Fill_refGcodes(Keyv,dire,namp,x_low,x_up,x_step,icond,model,sys,Iphas)
               else
                   call Fill_refGcodes(Keyv,dire,namp,x_low,x_up,x_step,icond,model,sys) !dire="gvary" or "gfix", namp may contain numbers
               end if
             else
               if(present(Iphas)) then
                  call Fill_refGcodes(Keyv,dire,namp,x_low,x_up,x_step,icond,model,Iphas=Iphas)
               else
                  call Fill_refGcodes(Keyv,dire,namp,x_low,x_up,x_step,icond,model)
               end if
             end if
             if (Err_CFML%Flag) return

          else  !Now there are more keywords and eventually numbers
             !---- [LABEL, [INF,[SUP,[STEP,[COND]]]]] ----!
             ! If Ilabel(i) == 0 then is a label otherwise is a number
             n_ini=minloc(ilabel,dim=1)
             ilabel(n_ini)=2
             n_end=minloc(ilabel,dim=1)-1

             do n=1,ndir  !This runs over the number of directives (keywords, names of parameters)
                nb=0
                !---- Default values ----!
                x_low =0.0
                x_up  =0.0
                x_step=0.0
                icond =0
                aux_string=u_case(label(n_ini))

                if(present(Iphas)) then
                  write(unit=phase,fmt="(i2.2)") iphas
                  if(index(aux_string,phase) == 0) then
                    aux_string=trim(aux_string)//"_"//phase
                  end if
                end if

                do j=1,model%npar
                   if (index(u_case(model%Par(j)%nam),trim(aux_string)) /= 0 ) then
                      na=j
                      exit
                   end if
                end do

                if (na == 0) then
                   Err_CFML%Flag=.true.
                   Err_CFML%Msg=" NonAtomic label not found for "//trim(line)
                   return
                else
                   new_label=model%Par(na)%nam  !this is the name of the parameter to be fixed or refined
                end if

                !---- Valu List: Inf,Sup,Step,Cond ----!
                i=0
                do j=n_ini+1,n_end
                   i=i+1
                   call Get_Num(label(j),vet,ivet,iv)
                   select case (i)
                      case (1)
                         x_low=vet(1)
                         x_up =vet(1)

                      case (2)
                         x_up =vet(1)

                      case (3)
                         x_step =vet(1)

                      case (4)
                         icond = ivet(1)
                   end select
                end do

                if(present(sys)) then
                  if(present(Iphas)) then
                     call Fill_refGcodes(Keyv,dire,new_label,x_low,x_up,x_step,icond,model,sys,Iphas=Iphas)
                  else
                     call Fill_refGcodes(Keyv,dire,new_label,x_low,x_up,x_step,icond,model,sys)
                  end if
                else
                  if(present(Iphas)) then
                     call Fill_refGcodes(Keyv,dire,new_label,x_low,x_up,x_step,icond,model,Iphas=Iphas)
                  else
                     call Fill_refGcodes(Keyv,dire,new_label,x_low,x_up,x_step,icond,model)
                  end if
                end if
                if (Err_CFML%Flag) return

                n_ini=minloc(ilabel,dim=1)
                ilabel(n_ini)=2
                n_end=minloc(ilabel,dim=1)-1

             end do
          end if

       end if
    End Subroutine Get_RefGCodes_Line

End Submodule KWC_RefCodes
