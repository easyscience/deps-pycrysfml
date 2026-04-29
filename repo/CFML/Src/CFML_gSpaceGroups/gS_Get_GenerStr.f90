!!----
!!----
!!----
!!
SubModule (CFML_gSpaceGroups) gS_Get_GenerStr
   implicit none
   Contains

   !!----
   !!---- Get_Generators_from_Str
   !!----
   !!---- 20/04/19
   !!
   Module Subroutine Get_Generators_from_Str(StrGen, d, gen, ngen,time_given)
      !---- Arguments ----!
      character(len=*),                            intent(in)  :: StrGen
      integer,                                     intent(out) :: d
      character(len=*), dimension(:), allocatable, intent(out) :: gen
      integer,                                     intent(out) :: ngen
      logical, optional,                           intent(out) :: time_given

      !--- Local variables ---!
      character(len=:),                allocatable :: symbol, ListGen
      character(len=80), dimension(:), allocatable :: gen_aux
      integer, dimension(40)                       :: Pos
      integer                                      :: i,j,k,np,ngg
      logical                                      :: timerev_provided

      !> Init
      ngg=0

      !> Determine the dimension from the generatorList (first operator)
      ListGen=trim(StrGen)//"  "

      !Check if the generator string is given in ISOTROPY format
      if(index(ListGen,"(") /= 0 .and. index(ListGen,")") /= 0) call ISO_to_jones_notation(ListGen)

      i=index(ListGen,";")
      if (i == 0) then
         !> there is only one generator and ";" is not given
         Symbol=ListGen
         ngen=1
      else
         Symbol=ListGen(1:i-1)
      end if
      d=Get_Dimension_SymmOp(Symbol)

      !> List of Generators
      call Get_Separator_Pos(ListGen,";",pos,np)

      !> Verify if there is a final ";" not followed by a generator
      if (np == 0) then !there is only one generator and ";" is not given
         np=1
         Pos(np)=len_trim(ListGen)+2 !add artificially a position for ";"
      end if
      if (len_trim(ListGen(Pos(np)+1:)) == 0) then
         ngg=np  !final ;
      else
         ngg=np+1
      end if
      allocate(gen_aux(ngg))

      !> Obtaining generators
      j=1
      do i=1, np
         k=pos(i)
         gen_aux(i)=ListGen(j:k-1)
         j = k + 1
      end do
      if (ngg > np) then
         gen_aux(ngg)=ListGen(j:)
         i=index(gen_aux(ngg),";")
         if (i /= 0) gen_aux(ngg)(i:i) = " "
      end if

      !> Time reversal
      timerev_provided=.false.
      do i=1,ngg
         call Get_Separator_Pos(gen_aux(i),",",pos,np)
         if (np < d-1) cycle
         timerev_provided=.true.
      end do

      !> Add time inversion in those operators that have not been Read
      if (timerev_provided) then
         do i=1,ngg
            call Get_Separator_Pos(gen_aux(i),",",pos,np)
            if (np < d-1) gen_aux(i)=trim(gen_aux(i))//",+1"
         end do
      end if
      if(present(time_given)) time_given=timerev_provided

      call Check_Gener(Gen_aux, Gen, ngen)
      if (allocated(Gen)) then
         ngen=size(Gen)
      else
         ngen = 0
      end if

   End Subroutine Get_Generators_from_Str

   Module Subroutine ISO_to_jones_notation(gen_string)
      !---- Arguments ----!
      character(len=*), intent(in out) :: gen_string
      !--- Local variables ---!
      character(len=len(gen_string)+2) :: loc_str
      integer                          :: i,j,n_it
      character(len=35), dimension(25) :: items

      !Transform the list of generators to the standard form in CrysFML
      call Get_Words(gen_string,items,n_it,";")
      loc_str=" "
      do i=1,n_it
        j=index(items(i),")")
        if(index(items(i),"'") /= 0) then
          items(i) = trim(adjustl(items(i)(2:j-1)))//",-1;"
        else
          items(i) = trim(adjustl(items(i)(2:j-1)))//",+1;"
        end if
        loc_str=trim(loc_str)//items(i)
      end do
      j=len_trim(loc_str)
      if(loc_str(j:j) == ";") loc_str(j:j)=" "
      gen_string=loc_str
   End Subroutine ISO_to_jones_notation

End SubModule gS_Get_GenerStr

