SubModule (CFML_gSpaceGroups) gS_Identify_Groups
   implicit none
   Contains

   !!----
   !!---- Identify_Group
   !!----
   !!---- Initialize the identification of the group by calling
   !!---- the appropiate subroutine according to the nature  of
   !!---- the group -crystallographic, magnetic, superspace-.
   !!----
   !!---- 22/04/2019
   !!
   Module Subroutine Identify_Group(G,database_path)
      !---- Arguments ----!
      type(spg_type),             intent(in out) :: G
      character(len=*), optional, intent(in)     :: database_path

      !---- Local Variables ----!
      character(len=5) :: car
      integer          :: n
      logical          :: pout

      !>===== DEBUG =====
      pout=.false.
      pout=(pout .or. CFML_DEBUG)
      !>=================

      !> Init
      call Clear_Error()

      if (G%d < 4) then
         Err_CFML%Ierr = 1
         Err_CFML%Msg = "Identify_Group@SPACEG: Group dimension < 4."
         return
      else if (G%d == 4) then ! Shubnikov groups
         if (present(database_path)) then
            call Identify_Shubnikov_Group(G,database_path)
         else
            call Identify_Shubnikov_Group(G)
         end if
         if (G%Numspg == 0) then
            if (G%Numshu > 0) then
               car=adjustl(nlabel_bns(G%numshu))
               n=index(car,'.')
               car=car(:n-1)
               read(unit=car,fmt='(i3)') G%numspg
            end if
         end if
      else ! Superspace groups
         Err_CFML%Ierr = 1
         Err_CFML%Msg = "Identify_Group@SPACEG: Superspace groups not implemented yet"
         return
      end if

   End Subroutine Identify_Group

   !!----
   !!---- IDENTIFY_MAGNETIC_SPACE_GROUP
   !!----
   !!---- Identifies the Shubnikov group of group G. Before
   !!---- using this subroutine, the subroutine Identify_
   !!---- _Crystallographic_Space_Group must be called
   !!----
   !!---- 22/04/2019
   !!----
   Module Subroutine Identify_Shubnikov_Group(G,database_path)
      !---- Arguments ----!
      type(spg_type),             intent(in out) :: G
      character(len=*), optional, intent(in)     :: database_path

      !---- Local variables ---!
      type(rational), dimension(3,3)   :: P,Mp,Mc,M
      logical :: pout

      !>===== DEBUG =====
      pout=.false.
      pout=(pout .or. CFML_DEBUG)
      !>=================

      if(.not. Magnetic_DBase_allocated) then
        if (present(database_path)) then 
            call Read_Magnetic_Data(database_path)
        else
            call Read_Magnetic_Data()
        end if
        if (Err_CFML%IErr /=0) return
      end if

      call Identify_PointGroup(G)
      if (Err_CFML%Ierr /= 0) return

      call Identify_LaueClass(G)
      if (Err_CFML%Ierr /= 0) return

      call Identify_Crystal_System(G)
      if (Err_CFML%Ierr /= 0) return

      P=Get_P_Matrix(G)
      if (Err_CFML%Ierr /= 0) return

      Mp=Get_Mp_Matrix(G,P)
      if (Err_CFML%Ierr /= 0) return

      Mc=Get_Mc_Matrix(G%laue,Mp)
      if (Err_CFML%Ierr /= 0) return

      M = matmul(Mp,Mc)
      G%spg_lat=Get_Lattice_Type(M)
      if (Err_CFML%Ierr /= 0) return

      if (present(database_path)) then
         call Match_Shubnikov_Group(G,P,M,database_path)
      else
         call Match_Shubnikov_Group(G,P,M)
      end if
      if (Err_CFML%Ierr /= 0) then
         write(unit=*,fmt="(a)") " => "//trim(err_CFML%msg)
      end if

   End Subroutine Identify_Shubnikov_Group

End Submodule gS_Identify_Groups