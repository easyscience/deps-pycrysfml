!!----
!!----
!!----
SubModule (CFML_IOForm) Format_fst
   !---- Variables ----!
   implicit none

 Contains

   Module Subroutine Read_XTal_FST(FST, Cell, SpG, Atm, MGp, mAtm, Mag_dom, database_path)
      Type(File_Type),                     intent(in)     :: FST
      class(Cell_G_Type),allocatable,      intent(out)    :: Cell    ! Cell object
      Class(SpG_Type), allocatable,        intent(out)    :: SpG
      Type(AtList_Type),                   intent(out)    :: Atm
      Type(MagSymm_k_Type),      optional, intent(out)    :: MGp
      Type(mAtom_List_Type),     optional, intent(out)    :: mAtm
      Type(Magnetic_Domain_type),optional, intent(out)    :: Mag_dom
      character(len=*),          optional, intent(in)     :: database_path

      !---- Local variables ----!
      integer  :: i, n_ini, n_end, npos
      Type(File_Type) :: struct
      Type(File_list_Type) :: magstr

      npos=fst%nlines
      do i=1, fst%nlines
        line=adjustl(fst%line(i)%str)
        if(line(1:1) == "{") then
           npos=i-1
           exit
        end if
      end do

      !Splitting the file between structural part and magnetic part
      struct%Fname="Structural_Part_of_FST_file"
      struct%nlines=npos
      allocate(struct%line(npos))
      struct%line(1:npos)=fst%line(1:npos)
      do i=1,npos
        struct%line(i)%str=adjustl(struct%line(i)%str)
        line=l_case(struct%line(i)%str)
        n_ini=index(line,"scale")
        if( n_ini /= 0) struct%line(i)%str(n_ini:) = " "
        n_ini=index(line,"multiple")
        if( n_ini /= 0) struct%line(i)%str(n_ini:) = " "
        n_ini=index(line,"nodisplay")
        if( n_ini /= 0) struct%line(i)%str(n_ini:) = " "
        n_ini=index(line,"display")
        if( n_ini /= 0) struct%line(i)%str(n_ini:) = " "
        n_ini=index(line,"color")
        if( n_ini /= 0) struct%line(i)%str(n_ini:) = " "
      end do

      if (present(database_path)) then
        call Read_XTal_CFL(struct, Cell, SpG, Atm, database_path=database_path)
      else
        call Read_XTal_CFL(struct, Cell, SpG, Atm)
      end if
      if(err_CFML%Ierr /= 0) return
      !Correct occupation factor that is not given in FST files
      do i=1,Atm%natoms
        Atm%atom(i)%Occ = real(Atm%atom(i)%mult,kind=cp)/real(SpG%Multip,kind=cp)
      end do

      if(npos < fst%nlines) then
         !magstr%Fname="Magnetic_Part_of_FST_file"
         magstr%nlines=fst%nlines-npos
         allocate(magstr%line(magstr%nlines))
         do i=1,magstr%nlines
           magstr%line(i)=fst%line(npos+i)%Str
         end do
         do i=1,magstr%nlines
           magstr%line(i)=adjustl(magstr%line(i))
           line=l_case(magstr%line(i))
           n_ini=index(line,"scale")
           if( n_ini /= 0) magstr%line(i)(n_ini:) = " "
           n_ini=index(line,"nodisplay")
           if( n_ini /= 0) magstr%line(i)(n_ini:n_ini+8) = " "
           n_ini=index(line,"color")
           if( n_ini /= 0) magstr%line(i)(n_ini:) = " "
         end do

         !Attempt to read the magnetic part to construct the magnetic atoms and the MagSymm_k_Type object MGp
         if(present(MGp) .and. present(mAtm)) then
           n_ini=1; n_end=magstr%nlines
           if(present(Mag_Dom)) then
              call Readn_Set_Magnetic_Kv_Structure(magstr,n_ini,n_end,MGp,mAtm,Mag_dom)
           else
              call Readn_Set_Magnetic_Kv_Structure(magstr,n_ini,n_end,MGp,mAtm)
           end if
         end if
      end if
   End Subroutine Read_XTal_FST

End SubModule Format_fst