!!----
!!----
!!----
!!
SubModule (CFML_Symmetry_Tables) TAB_Get_SpgT
   Implicit none
   Contains
   !!----
   !!---- GET_IT_GENERATORS
   !!----
   !!----    Provides the string "gener" containing the list of the generators
   !!----    (as given in the IT Crystallography) corresponding to the space group
   !!----    of symbol "spg". In "spg" the Hermann-Mauguin symbol, Hall, comapct
   !!----    H-M or the number of the space group should be given.
   !!----    The calling program is responsible of decoding the string "gener".
   !!----    Generator are given in the Jone's Faithful notation and
   !!----    the separator is the symbol ";".
   !!----
   !!----    An example: R-3 c
   !!----    gener = " x+1/3,y+2/3,z+2/3; -y,x-y,z; -y,-x,z+1/2"
   !!----    The variable is the string contained between the quotes.
   !!----
   !!---- 20/04/2019
   !!
   Module Function Get_IT_Generators(Spg) Result(StrGen)
      !---- Arguments ----!
      character(len=*), intent(in)  :: spg      ! H-M, Hall, IT number and compact HM
      character(len=:), allocatable :: StrGen

      !----  Local variables ----!
      integer                 :: ier, numg

      !> Init
      Strgen=" "
      if (len_trim(it_spg_gen(1)) <=0) call set_IT_gen()


      !> IT Number?
      read(unit=spg,fmt=*,iostat=ier) numg
      if (ier == 0) then
         if (numg > 0 .and. numg <= 230) then
            Strgen=it_spg_gen(numg)(12:)
         end if
         return
      end if

      !> Other symbol?
      call Get_SpaceGroup_Symbols(Spg, IT=numg)
      if (numg > 0 .and. numg <= 230) then
            Strgen=it_spg_gen(numg)(12:)
      end if
   End Function Get_IT_Generators

   !!----
   !!---- GET_SHUBNIKOV_INFO
   !!----    Function which returns the row r from the shubnikov_info table
   !!----
   !!---- 13/09/2024
   !!
   Module Function Get_Shubnikov_Info(r) Result(info)
      !---- Argument ----!
      integer,             intent(in) :: r      ! Row
      type(Shub_Spgr_Info_Type)       :: info   ! Tabulated data of a Shubnikov group

      !> Init
      if (r > 0 .and. r <= NUM_SHUBNIKOV) then
         if (.not. allocated(shubnikov_info)) call set_shubnikov_info()
         info = shubnikov_info(r)
      end if

   End Function Get_Shubnikov_Info

   !!----
   !!---- GET_SPGR_INFO
   !!----    Function which returns the row r from the spgr_info table
   !!----
   !!---- 13/09/2024
   !!
   Module Function Get_Spgr_Info(r) Result(info)
      !---- Argument ----!
      integer,             intent(in) :: r      ! Row
      type(Spgr_Info_Type)            :: info   ! Tabulated data of a crystallographic space group

      !> Init
      if (r > 0 .and. r <= NUM_SPGR_INFO) then
         if (.not. allocated(spgr_info)) call set_spgr_info()
         info = spgr_info(r)
      end if

   End Function Get_Spgr_Info

End SubModule TAB_Get_SpgT