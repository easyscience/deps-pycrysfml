!!
Submodule (CFML_IOForm) Format_Blocks
   implicit none

   Contains

   !!----
   !!---- SUBROUTINE GET_BLOCK_KEY
   !!----
   !!---- BLOCK definition:
   !!----     KEY_StringName Number
   !!----     ....
   !!----     ....
   !!----     END_KEY_StringName
   !!----
   !!---- Update: 17/05/2023
   !!
   Module Subroutine Get_Block_KEY(Key, ffile, N_Ini, N_End, Ind, StrName, N_Id)
      !---- Arguments ----!
      character(len=*),              intent(in)  :: Key         ! 'Pattern','Phase',....
      Type(file_type),               intent(in)  :: ffile
      integer,                       intent(in)  :: n_ini
      integer,                       intent(in)  :: n_end
      integer, dimension(2),         intent(out) :: Ind         ! Start; End
      character(len=*),              intent(out) :: StrName     ! String identification
      integer,                       intent(out) :: N_Id        ! Number of ID

     !---- Local Arguments ----!
     integer           :: i, j, n, nc, iv
     character(len=80) :: car

     !> Init
     Ind=0
     StrName=' '
     N_Id=0

     car=u_case(trim(key))

     i=N_ini
     do while(i <= N_end)
        line=adjustl(ffile%line(i)%str)

        !> No blank lines
        if (len_trim(line) == 0 ) then
           i=i+1
           cycle
        end if

        !> No comments
        if (line(1:1) =='!' .or. line(1:1) ==' ') then
           i=i+1
           cycle
        end if

        !> Purge comments
        j=index(line,'!')
        if (j > 0) line=line(:j-1)
        j=index(line,'#')
        if (j > 0) line=line(:j-1)

        !> Block type
        j=index(u_case(line), trim(car)//'_')
        if (j <= 0) then
           i=i+1
           cycle
        end if

        !> Identification String
        j=index(u_case(line), '_')
        call get_words(line(j+1:), dire, nc)
        select case (nc)
           case (1)
              strname=adjustl(dire(1))

           case (2)
              strname=adjustl(dire(1))
              call get_num(dire(2), vet, ivet, iv)
              if (iv == 1) N_Id=ivet(1)

           case default
              call set_error(-1, " Error in the format of the Block definition")
              return
        end select

        do n=i+1,n_end
           line=adjustl(ffile%line(n)%str)
           if (len_trim(line) <= 0) cycle
           if (line(1:1) =='!') cycle
           if (line(1:1) ==' ') cycle

           j=index(u_case(line),'END_'//trim(car)//'_'//trim(u_case(strname)))
           if (j <= 0) cycle

           !> Max dimension for Ind and ID_Str. Pay attention
           Ind(1)=i
           Ind(2)=n
           exit
        end do
        exit
     end do

     !> Debug information
     !if (CFML_DEBUG .and. any(ind /=0)) then
     !   write(unit=*,fmt='(a)') ' '
     !   write(unit=*,fmt='(a)') ' '//trim(car)//': '//trim(strname)
     !   write(unit=*,fmt='(a, i5)') '    Num. ID: ',N_Id
     !   write(unit=*,fmt='(a, i5)') ' Start line: ',ind(1)
     !   write(unit=*,fmt='(a, i5)') '   End line: ',ind(2)
     !   write(unit=*,fmt='(a)') ' '
     !end if

  End Subroutine Get_Block_Key

   !!----
   !!---- SUBROUTINE GET_ZONECOMMANDS
   !!----
   !!---- Date: 11/05/2022
   !!
   Module Subroutine Get_Block_Commands(ffile, N_Ini, N_End)
      !---- Arguments ----!
      Type(file_type),    intent(in)  :: ffile
      integer,            intent(out) :: n_ini
      integer,            intent(out) :: n_end

      !---- Local Variables ----!
      integer            :: i, j

      !> Init
      n_Ini=0; n_End=0

      !> Determine the zone of commands in the file
      do i=1,ffile%nlines
         line=adjustl(ffile%line(i)%str)
         if (len_trim(line) <= 0) cycle
         if (line(1:1) =='!') cycle
         if (line(1:1) ==' ') cycle

         j=index(line,'!')
         if (j > 0) line=line(:j-1)
         j=index(line,'#')
         if (j > 0) line=line(:j-1)

         !> N_ini point the next line into Command zone
         if (n_Ini == 0) then
            if (u_case(line(1:8)) == 'COMMANDS') n_ini=i
            cycle
         end if

         !> N_end point the previous line from end Command zone
         if (n_ini > 0 .and. i >= n_ini) then
            if (u_case(line(1:12)) == 'END_COMMANDS') then
               n_End=i
               exit
            end if
         end if
      end do

      !> Check error
      if (n_ini > 0 .and. n_end == 0) then
         call set_error(1, "Error in COMMANDS/END_COMMANDS Block definition!")
      end if

   End Subroutine Get_Block_Commands

   !!----
   !!---- SUBROUTINE GET_SUBBLOCK
   !!----
   !!---- SUBBLOCK definition:
   !!---- It is a block into a Block
   !!----     KEY_StringName
   !!----     ....
   !!----     ....
   !!----     END_KEY_StringName
   !!----
   !!---- Date: 15/05/2023
   !!
   Module Subroutine Get_SubBlock_KEY(Key, ffile, n_ini, n_end, Ind, StrName)
      !---- Arguments ----!
      character(len=*),           intent(in)  :: key
      Type(file_type),            intent(in)  :: ffile
      integer,                    intent(in)  :: n_ini
      integer,                    intent(in)  :: n_end
      integer, dimension(2),      intent(out) :: Ind
      character(len=*), optional, intent(out) :: StrName

      !---- Local Variables ----!
      character(len=:), allocatable    :: car
      integer                          :: i,j

      !> Init
      Ind=0
      StrName=' '
      car=u_case(trim(key))

      !> Determine the zone of Background in the file
      do i=n_ini, n_end
         line=adjustl(ffile%line(i)%str)
         if(len_trim(line) == 0) cycle
         if (line(1:1) =='!') cycle
         if (line(1:1) ==' ') cycle

         j=index(line,'!')
         if (j > 0) line=line(:j-1)
         j=index(line,'#')
         if (j > 0) line=line(:j-1)

         !> N_ini point the next line into Command zone
         if (Ind(1) == 0) then
            j=index(u_case(line),trim(car))
            if (j > 0) then
               Ind(1)=i
               cycle
            end if
         end if

         !> N_end point the previous line from end Command zone
         if (Ind(1) > 0 .and. i >= Ind(1)) then
            j=index(u_case(line),'END_'//trim(car))
            if (j > 0) then
               ind(2)=i
               exit
            end if
         end if
      end do

      if (present(StrName) .and. ind(1) > 0) then
         line=adjustl(ffile%line(ind(1))%str)
         j=index(line,'_')
         StrName=' '
         if (j > 0) StrName=line(j+1:)
      end if

   End Subroutine Get_SubBlock_KEY

   !!----
   !!---- SUBROUTINE READBLOCK_EXCLUDEREG
   !!----
   !!----     Exclude regions into a Pattern zone
   !!----
   !!---- Update: 12/05/2023
   !!
   Module Subroutine Read_Block_ExcludeReg(ffile, n_ini, n_end, Exclr)
      !---- Arguments ----!
      !---- Arguments ----!
      Type(file_type),         intent(in)    :: ffile
      integer,                 intent(in)    :: n_ini
      integer,                 intent(in)    :: n_end
      Type(Excl_reg_type),     intent(out)   :: Exclr

      !---- Local Variables ----!
      integer               :: i, j, iv,n
      integer, dimension(2) :: Ind
      character(len=:), allocatable :: line
      real(kind=cp),     dimension(15) :: vet
      integer,           dimension(15) :: ivet

      !> Init
      call clear_error()

      call Get_SubBlock_KEY('EXCLUDED_REGIONS', ffile, n_ini, n_end, Ind)
      if (all(Ind ==0)) return

      !Count the number of excluded regions for this pattern
      n=0
      do i=Ind(1)+1, Ind(2)-1
         line=adjustl(ffile%line(i)%str)
         if (len_trim(line) == 0) cycle
         if (line(1:1) =="!" .or. line(1:1) =="#") cycle
         n=n+1  !Number of excluded regions
      end do
      Exclr%num_excl=n
      if(allocated(Exclr%Exc)) deallocate(Exclr%Exc)
      allocate(Exclr%Exc(n))

      n=0
      do i=Ind(1)+1, Ind(2)-1
         line=adjustl(ffile%line(i)%str)
         if (len_trim(line) == 0) cycle
         if (line(1:1) =="!") cycle
         j=index(line,'!')
         if (j > 0) line=line(:j-1)
         j=index(line,'#')
         if (j > 0) line=line(:j-1)
         call Get_Num(line, vet, ivet, iv)
         if (iv /=2) then
            call set_error(1,'Wrong format for Excluded Regions interval: '//trim(line))
            return
         end if
         n=n+1
         Exclr%Exc(n)%mina=vet(1)
         Exclr%Exc(n)%maxb=vet(2)
      end do

   End Subroutine Read_Block_ExcludeReg

   !!----
   !!---- SUBROUTINE READBLOCK_BACKGD
   !!----
   !!----
   !!---- Update: 12/05/2023
   !!
   Module Subroutine Read_Block_Backgd(ffile, n_ini, n_end, Bck, ipatt)
      !---- Arguments ----!
      Type(file_type),     intent(in)    :: ffile
      integer,             intent(in)    :: n_ini
      integer,             intent(in)    :: n_end
      Type(Bck_type),      intent(out)   :: Bck
      integer, optional,   intent(in)    :: ipatt

      !---- Local Variables ----!
      integer, dimension(2) :: Ind
      character(len=:), allocatable    :: keyw, line
      character(len=30), dimension(15) :: dire
      real(kind=cp),     dimension(15) :: vet
      integer,           dimension(15) :: ivet
      integer                          :: i,j, k, iv, ic, np, ier, ip

      !> Init
      call clear_error()

      call Get_SubBlock_KEY('BACKGD', ffile, n_ini, n_end, Ind)
      if (all(Ind == 0)) return

      j=ind(1)
      ip=1
      if(present(ipatt)) ip=ipatt
      do while(j <= ind(2)-2)
         j=j+1
         line = adjustl(ffile%line(j)%str)
         if (len_trim(line) == 0) cycle
         if (line(1:1) =='!') cycle

         k=index(line,'!')
         if (k > 0) line=line(:k-1)
         k=index(line,'#')
         if (k > 0) line=line(:k-1)

         !> Read the model of Background
         call Get_words(line, dire, iv)
         if (iv < 1 .or. iv > 3) then
            write(dire(15),"(a,i4)") " @line: ",j
            call set_error(1,'@Read_Block_Backgd:Wrong format for Background model into BACKGD zone '//trim(dire(15)))
            return
         end if

         !> Models for Background considerations
         keyw="                "
         if(iv == 3) keyw=trim(u_case(dire(3)))
         select case (trim(u_case(dire(1))))
            case ('LINEAR_INTERPOLATION','SPLINE_INTERPOLATION')
              read(dire(2),*,iostat=ier) np
              if(ier /= 0) then
                 write(dire(15),"(a,i4)") " @line: ",j
                 call set_error(1,"After the 'LINEAR(SPLINE)_INTERPOLATION' keyword, the number of points should be provided"//trim(dire(15)))
                 return
              else
                 if(allocated(Bck%Fpar)) deallocate(Bck%Fpar)
                 if(allocated(Bck%xFpar)) deallocate(Bck%xFpar)
                 allocate(Bck%xFpar(np),Bck%Fpar(np))
                 Bck%Funct_typ=trim(dire(1))
                 Bck%ipatt=ipatt
              end if
              i=0
              do
                 j=j+1
                 line = adjustl(ffile%line(j)%str)
                 if (len_trim(line) == 0) cycle
                 if (line(1:1) =='!') cycle
                 k=index(line,'!')
                 if (k > 0) line=line(:k-1)
                 k=index(line,'#')
                 if (k > 0) line=line(:k-1)
                 call get_num(line, vet, ivet, ic)
                 i=i+1
                 if (ic /= 2) then
                   write(dire(15),"(a,i4)") " @line: ",j
                   call set_error(1,"Wrong format of the interpolation points (it should be 2 real numbers!)"//trim(dire(15)))
                   return
                 end if
                 Bck%xFpar(i)=vet(1)
                 Bck%Fpar(i)=vet(2)
                 if(i == np) exit
              end do
              ! Now the background points have been read
              Bck%Num_Fpar=np

            case ('POLYNOMIAL','CHEBYCHEV')
              read(dire(2),*,iostat=ier) np  !This is the order of the polynomial, number of parameters np+1
              if(ier /= 0) then
                 write(dire(15),"(a,i4)") " @line: ",j
                 call set_error(1,"After the 'POLYNOMIAL(CHEBYCHEV)' keyword, the order 'n' of the polynomial should be provided (n+1 parameters!)")
                 return
              else
                 if(allocated(Bck%Fpar)) deallocate(Bck%Fpar)
                 allocate(Bck%Fpar(np+1))
                 Bck%Funct_typ=trim(dire(1))
                 Bck%ipatt=ip
              end if
              i=0
              do_ext: do
                 j=j+1
                 line = adjustl(ffile%line(j)%str)
                 if (len_trim(line) == 0) cycle
                 if (line(1:1) =='!') cycle
                 k=index(line,'!')
                 if (k > 0) line=line(:k-1)
                 k=index(line,'#')
                 if (k > 0) line=line(:k-1)
                 call Get_words(line, dire, iv)
                 do k=1,iv
                   read(dire(k),*,iostat=ier) Bck%Fpar(i+k)
                   if(ier /= 0) then
                      write(dire(15),"(i4,a,i4)") i+k," @line: ",j
                      call set_error(1,"Error reading POLYNOMIAL coefficient number# "//trim(dire(15)))
                      return
                   end if
                   if(i+k == np+1) exit do_ext
                 end do
                 i=i+iv
              end do do_ext
              ! Now the polynomial/Chebychev background parameters have been read
              Bck%Num_Fpar=np+1

            case ('PEAKS')   !keyw='PSEUDO-VOIGT' or 'SPLIT-PSEUDO-VOIGT'
              read(dire(2),*,iostat=ier) np
              if(ier /= 0) then
                  write(dire(15),"(a,i4)") " @line: ",j
                 call set_error(1,"After the 'PEAKS' keyword, the number of peaks should be provided")
                 return
              else
                 if(allocated(Bck%Ppar)) deallocate(Bck%Ppar)
                 if(index(keyw,"SPLIT") /= 0) then
                    allocate(Bck%Ppar(6,np))
                    Bck%Peak_typ="split-pseudo-Voigt"
                    Bck%ipatt=ip
                 else
                    allocate(Bck%Ppar(4,np))
                    Bck%Peak_typ="pseudo-Voigt"
                    Bck%ipatt=ip
                 end if
              end if
              i=0
              do
                 j=j+1
                 line = adjustl(ffile%line(j)%str)
                 if (len_trim(line) == 0) cycle
                 if (line(1:1) =='!') cycle
                 k=index(line,'!')
                 if (k > 0) line=line(:k-1)
                 k=index(line,'#')
                 if (k > 0) line=line(:k-1)
                 call Get_words(line, dire, iv)
                 i=i+1
                 if(i > np) exit
                 do k=1,iv
                   read(dire(k),*) Bck%Ppar(k,i)
                 end do
              end do
              ! Now the background points have been read
              Bck%Num_Peaks=np

            case default
               call set_error(1,'The required method for background determination: '//trim(u_case(dire(1)))//', is not implemented!')
               return
         end select
      end do

   End Subroutine Read_Block_Backgd

   !!----
   !!---- SUBROUTINE READBLOCK_INSTRUCTIONS
   !!----
   !!----
   !!---- Update: 12/05/2023
   !!
   Module Subroutine Read_Block_Instructions(ffile, N_ini, N_end, LSymm)
      !---- Arguments ----!
      type(File_type),   intent(in) :: Ffile
      integer,           intent(in) :: N_ini
      integer,           intent(in) :: N_end
      logical, optional, intent(in) :: LSymm

      !---- Local Variables ----!
      logical               :: exc_reg, exc_bck, exc_symm
      integer               :: i, j, k, kk, ic, iv
      integer, dimension(2) :: Ind1, Ind2
      character(len=10)     :: str
      character(len=132)    :: linec


      !> Init
      Ind1=0; Ind2=0
      exc_symm=.false.
      if (present(lsymm)) exc_symm=lsymm

      call clear_error()

      !> Excluded regions
      exc_reg=.false.
      call Get_SubBlock_KEY('EXCLUDED_REGIONS', ffile, n_ini, n_end, Ind1)
      if (all(ind1 >0)) exc_reg=.true.

      !> BackGD
      exc_bck=.false.
      call Get_SubBlock_KEY('BACKGD', ffile, n_ini, n_end, Ind2)
      if (all(ind2 >0)) exc_bck=.true.

      !> Main loop
      i=n_ini+1
      do while (i <= n_end-1)
         !> Exclusion zones
         if (exc_reg) then
            if (i >= ind1(1) .and. i <= ind1(2)) then
               i=ind1(2)+1
               cycle
            end if
         end if

         if (exc_bck) then
            if (i >= ind2(1) .and. i <= ind2(2)) then
               i=ind2(2)+1
               cycle
            end if
         end if

         line=adjustl(ffile%line(i)%str)

         if (len_trim(line) == 0) then
            i=i+1
            cycle
         end if

         if (line(1:1) =="!" .or. line(1:1)==' ') then
            i=i+1
            cycle
         end if

         j=index(line,'!')
         if (j > 0) line=line(:j-1)
         j=index(line,'#')
         if (j > 0) line=line(:j-1)

         if (exc_symm) then
            !> Special cases on symmetry / space groups
            linec = line
            call cut_string(linec, ic, str)
            str=adjustl(u_case(str))
            j=string_count(linec,';')

            select case (trim(str))
               case ('SPG','SSPG','MSG','MSPG','SPGR','SHUB','MSSG','MSSPG')
                  select case (j)
                     case(0)
                        NP_Instr=NP_Instr+1
                        Vec_Instr(NP_Instr)%Str=trim(str)
                        Vec_Instr(NP_Instr)%NPar=1
                        Vec_Instr(NP_Instr)%CV(1)=trim(linec)

                     case(1)
                        NP_Instr=NP_Instr+1
                        Vec_Instr(NP_Instr)%Str=trim(str)
                        Vec_Instr(NP_Instr)%NPar=1
                        Vec_Instr(NP_Instr)%CV(1)=trim(linec)

                     case default
                        call set_error(1,'Format not defined! ->'//trim(str)//' '//trim(linec))
                        return
                  end select

                  i=i+1
                  cycle

               case ('GEN','SYMM','GENERATORS')
                  select case (j)
                     case(0)
                        NP_Instr=NP_Instr+1
                        Vec_Instr(NP_Instr)%Str=trim(str)
                        Vec_Instr(NP_Instr)%NPar=1
                        Vec_Instr(NP_Instr)%CV(1)=trim(linec)

                     case(1:)
                        NP_Instr=NP_Instr+1
                        Vec_Instr(NP_Instr)%Str=trim(str)
                        kk=0
                        do while(len_trim(linec) > 0)
                           k=index(linec,';')
                           if (k > 0) then
                              kk=kk+1
                              Vec_Instr(NP_Instr)%CV(kk)=linec(:k-1)
                              linec=linec(k+1:)

                           else
                              kk=kk+1
                              Vec_Instr(NP_Instr)%CV(kk)=linec
                              linec=''
                           end if
                        end do

                     case default
                        call set_error(1,'Format not defined! ->'//trim(str)//' '//trim(line))
                        return
                  end select

                  i=i+1
                  cycle
            end select
         end if

         do while(len_trim(line) > 0)
            j=index(line,';')
            if (j > 0) then
               linec=line(:j-1)
               line=line(j+1:)
            else
               linec=trim(line)
               line=''
            end if

            call get_words(linec, dire, ic)
            if (ic < 1 .or. ic > 15) then
               call set_error(1,'The directive has a wrong number of parameters! '//trim(linec))
               return
            end if

            !if(CFML_DEBUG) then
            !   write(*,"(a,i3)") " Number of words: ",ic
            !   write(*,"(a)") " Linec => "//trim(linec)
            !   do j=1,ic
            !      write(*,"(i3,a)") j," dire => "//trim(dire(j))
            !   end do
            !end if

            NP_Instr=NP_Instr+1
            Vec_Instr(NP_Instr)%Str=trim(u_case(dire(1)))
            Vec_Instr(NP_Instr)%NPar=ic-1
            do j=2,ic
               call get_num(dire(j),vet,ivet,iv)
               if (iv <= 0) then
                  Vec_Instr(NP_Instr)%CV(j-1)=trim(dire(j))
               else
                  Vec_Instr(NP_Instr)%IV(j-1)=ivet(1)
                  Vec_Instr(NP_Instr)%RV(j-1)=vet(1)
               end if
            end do

         end do

         i=i+1
      end do

   End Subroutine Read_Block_Instructions

   !!----
   !!---- SUBROUTINE GET_ZONEPATTERNS
   !!----
   !!---- Date: 11/05/2022
   !!
   Module Subroutine Get_Block_Patterns(ffile, N_Ini, N_End, NPatt, Patt, Ex_Ind)
      !---- Arguments ----!
      Type(file_type),                    intent(in)     :: ffile
      integer,                            intent(in)     :: n_ini
      integer,                            intent(in)     :: n_end
      integer,                            intent(out)    :: NPatt
      type(BlockInfo_Type), dimension(:), intent(in out) :: Patt
      integer, dimension(2), optional,    intent(in)     :: Ex_ind


      !---- Local Variables ----!
      character(len=60)     :: StrName
      integer               :: i, n_fin, N_id
      integer, dimension(2) :: Ind, Indx
      logical               :: exclusion=.false.

      !> Init
      NPatt=0
      Indx=0
      if (present(Ex_Ind)) then
         exclusion=.true.
         Indx=Ex_ind
      end if

      i=N_ini
      n_fin=N_end

      do while (i < N_end)
         !> Exclude zone
         if (exclusion) then
            if (i < Indx(1)-1) n_fin=Indx(1)-1
            if (i >=Indx(1) .and. i <=Indx(2)) then
               n_fin=ffile%nlines
               i=Indx(2)+1
               cycle
            end if
         end if

         call Get_Block_KEY('PATTERN', ffile, i, n_fin, Ind, StrName, N_Id)
         if (Err_CFML%IErr /= 0) return

         if (all(ind == 0)) then
            i=i+1
            cycle
         end if

         select case (N_Id)
            case (0)
               if (NPatt ==0) then
                  NPatt=1

                  Patt(1)%StrName=trim(StrName)
                  Patt(1)%BlName='PATTERN'   !Block containing a pattern
                  Patt(1)%IBl=2              !Numerical type of block
                  Patt(1)%Nl=Ind
               else
                  call set_error(1,'There is a previous Pattern block defined as 1!')
                  return
               end if

            case (1:MAX_PATTERNS)
               if (Patt(N_id)%IBl == 1) then
                  call set_error(1,'There is a previous Pattern Block defined with the same identificator!')
                  return
               end if

               Patt(N_id)%StrName=trim(StrName)
               Patt(N_id)%BlName='PATTERN'  !Block containing patterns
               Patt(N_id)%IBl=2             !Numerical type of block
               Patt(N_id)%Nl=Ind

               NPatt=NPatt+1
         end select

         i=ind(2)+1
      end do

   End Subroutine Get_Block_Patterns

   !!----
   !!---- SUBROUTINE GET_ZONEPHASES
   !!----
   !!---- Date: 11/05/2022
   !!
   Module Subroutine Get_Block_Phases(ffile, N_Ini, N_End, NPhas, Phas, Ex_Ind)
      !---- Arguments ----!
      Type(file_type),                    intent(in)     :: ffile
      integer,                            intent(in)     :: n_ini
      integer,                            intent(in)     :: n_end
      integer,                            intent(out)    :: NPhas
      type(BlockInfo_Type), dimension(:), intent(in out) :: Phas
      integer, dimension(2), optional,    intent(in)     :: Ex_ind


      !---- Local Variables ----!
      character(len=60)     :: StrName
      integer               :: i, n_fin, N_Id
      integer, dimension(2) :: Ind, Indx
      logical               :: exclusion=.false.


      !> Init
      NPhas=0
      Indx=0
      if (present(Ex_Ind)) then
         exclusion=.true.
         Indx=Ex_ind
      end if

      i=N_ini
      n_fin=N_end

      do while (i < N_end)
         !> Exclude zone
         if (exclusion) then
            if (i < Indx(1)-1) n_fin=Indx(1)-1
            if (i >=Indx(1) .and. i <=Indx(2)) then
               n_fin=ffile%nlines
               i=Indx(2)+1
               cycle
            end if
         end if

         call Get_Block_KEY('PHASE', ffile, i, n_fin, Ind, StrName, N_Id)
         if (Err_CFML%IErr /= 0) return
         if (all(ind == 0)) then
            i=i+1
            cycle
         end if
         select case (N_Id)
            case (0)
               if (NPhas == 0) then
                  NPhas=1
                  Phas(1)%StrName=trim(StrName)
                  Phas(1)%BlName='PHASE'  !Block containing a phase
                  Phas(1)%IBl=1           !Numerical type of block
                  Phas(1)%Nl=Ind
               else
                  call set_error(1,'There is a previous Phase block defined as 1!')
                  return
               end if

            case (1:MAX_PHASES)
               if (Phas(N_id)%IBl == 1) then
                  call set_error(1,'There is a previous Phase Block defined with the same identificator!')
                  return
               end if

               Phas(N_id)%StrName=trim(StrName)
               Phas(N_id)%BlName='PHASE'
               Phas(N_id)%IBl=1             !Numerical type of block
               Phas(N_id)%Nl=Ind

               NPhas=NPhas+1
         end select

         i=ind(2)+1
      end do

   End Subroutine Get_Block_Phases

   !!----
   !!---- SUBROUTINE Get_SubBlock_MolPhases
   !!----
   !!---- Date: June- 2023
   !!
   Module Subroutine Get_SubBlock_MolPhases(ffile, N_Ini, N_End, NMol, BMol)
      !---- Arguments ----!
      Type(file_type),                    intent(in)     :: ffile
      integer,                            intent(in)     :: n_ini
      integer,                            intent(in)     :: n_end
      integer,                            intent(out)    :: NMol
      type(BlockInfo_Type), dimension(:), intent(in out) :: BMol

      !---- Local Variables ----!
      character(len=60)     :: StrName
      integer               :: i, nc, n_fin, iv, n
      integer, dimension(2) :: Ind

      !> Init
      NMol=0

      i=N_ini
      n_fin=N_end

      do while (i < N_end)
         call Get_SubBlock_KEY('MOLEX', ffile, i, n_fin, Ind, StrName)
         !write(*,"(a,2i5,a)") " Ind ->",Ind,"  "//trim(StrName)
         if (Err_CFML%IErr /= 0) return
         if (all(ind == 0)) then
            i=i+1
            cycle
         end if

         call get_words(strname,dire,nc)
         n=0
         if (nc == 3) then
            call get_num(dire(3),vet, ivet, iv)
            if (iv == 1) n=ivet(1)
         end if

         NMol=NMol+1

         BMol(NMol)%StrName=trim(dire(1))
         BMol(NMol)%BlName='MOLEX'   !Block containing a Molecular phase
         BMol(NMol)%IBl=3            !Numerical type of block
         BMol(NMol)%Nl=Ind
         BMol(NMol)%Iex=n

         i=ind(2)+1
      end do

   End Subroutine Get_SubBlock_MolPhases

   !!----
   !!---- Subroutine Write_InfoBlock_Backgd
   !!----
   !!----    Write the information about Background Blocks in file associated with
   !!----    logical unit "iunit".
   !!----    If no argument is passed the standard output (iunit=6) is used
   !!----
   !!---- Update: May - 2023
   !!
   Module Subroutine Write_InfoBlock_Backgd(Bck,Iunit,IPatt)
      !---- Arguments ----!
      type(Bck_type),      intent(in) :: Bck
      integer, optional,   intent(in) :: Iunit
      integer, optional,   intent(in) :: IPatt

      !---- Local variables ----!
      integer :: lun, ip
      integer :: i,j, np

      lun=6
      if (present(iunit)) lun=iunit
      ip=1
      if(present(IPatt)) ip=IPatt

      if (Bck%ipatt /= IP) return
      if (Bck%Num_Fpar == 0) return
      np=Bck%Num_Fpar
      write(unit=lun, fmt="(a)") " "
      if(present(IPatt)) then
        write(unit=lun, fmt="(a,i4)") " Background Attributes for Pattern: ",IPatt
      else
        write(unit=lun, fmt="(a)") " Background Attributes: "
      end if
      write(unit=lun, fmt="(a)") " "

      Select Case (trim(u_case(Bck%Funct_typ)))

        Case('LINEAR_INTERPOLATION')
           write(unit=lun, fmt="(a)") " "
           write(unit=lun, fmt="(a)") " Background Points for Using Linear Interpolation"
           write(unit=lun, fmt="(a)") " --------------------------------------------"
           write(unit=lun, fmt="(a)") " "
           write(unit=lun, fmt="(a)")     "      Position       Intensity"
           do i=1,Bck%Num_Fpar
             write(unit=lun, fmt="(2f16.5)") Bck%xFpar(i), Bck%Fpar(i)
           end do
        Case('SPLINE_INTERPOLATION')
           write(unit=lun, fmt="(a)") " "
           write(unit=lun, fmt="(a)") " Background Points for Using Cubic Splines Interpolation"
           write(unit=lun, fmt="(a)") " -------------------------------------------------------"
           write(unit=lun, fmt="(a)") " "
           write(unit=lun, fmt="(a)")     "      Position       Intensity"
           do i=1,Bck%Num_Fpar
             write(unit=lun, fmt="(2f16.5)") Bck%xFpar(i), Bck%Fpar(i)
           end do
        Case ('POLYNOMIAL')
           write(unit=lun, fmt="(a)") " "
           write(unit=lun, fmt="(a)") " Values of polynomial coefficients for Background Calculation"
           write(unit=lun, fmt="(a)") " ------------------------------------------------------------"
           write(unit=lun, fmt="(a)") " "
           i=0
           do
             if(i > np-1) exit
             write(unit=lun, fmt="(a,6i14)") " Order:",(j,j=i,min(np,i+5))
             write(unit=lun, fmt="(tr7,6f14.5)") Bck%Fpar(i+1:min(np,i+6))
             i=i+6
           end do

        Case('CHEBYCHEV')
           write(unit=lun, fmt="(a)") " "
           write(unit=lun, fmt="(a)") " Values of Chebychev polynomial Coefficients for Background Calculation"
           write(unit=lun, fmt="(a)") " ----------------------------------------------------------------------"
           write(unit=lun, fmt="(a)") " "
           i=0
           do
             if(i > np-1) exit
             write(unit=lun, fmt="(a,6i14)") " Order:", (j,j=i,min(np,i+5))
             write(unit=lun, fmt="(tr7,6f14.5)") Bck%Fpar(i+1:min(np,i+6))
             i=i+6
           end do

      End Select

      if(Bck%Num_peaks > 0) then
        Select Case (trim(u_case(Bck%Peak_typ)))

          Case('PSEUDO-VOIGT')
            write(unit=lun, fmt="(a)") " "
            write(unit=lun, fmt="(a)") " Values of PSEUDO-VOIGT Peak Parameters for Background Calculation"
            write(unit=lun, fmt="(a)") " -----------------------------------------------------------------"
            write(unit=lun, fmt="(a)") "   Peak#      Position     Intensity        FWHM           Eta"
            do i=1,Bck%Num_peaks
              write(unit=lun, fmt="(i8,4f14.5)") i,Bck%Ppar(1:4,i)
            end do

          Case('SPLIT-PSEUDO-VOIGT')
            write(unit=lun, fmt="(a)") " "
            write(unit=lun, fmt="(a)") " Values of SPLIT-PSEUDO-VOIGT Peak Parameters for Background Calculation"
            write(unit=lun, fmt="(a)") " -----------------------------------------------------------------------"
            write(unit=lun, fmt="(a)") "   Peak#      Position     Intensity     FWHM-left    FWHM-right      Eta-left     Eta-right"
            do i=1,Bck%Num_peaks
              write(unit=lun, fmt="(i8,6f14.5)") i,Bck%Ppar(1:6,i)
            end do
        End Select
      end if
      write(unit=lun, fmt="(a)") " "

   End Subroutine Write_InfoBlock_Backgd

   !!----
   !!---- Subroutine Write_InfoBlock_ExcludedRegions
   !!----
   !!----    Write the information about Excluded Regions in file associated with
   !!----    logical unit "iunit".
   !!----    If no argument is passed the standard output (iunit=6) is used
   !!----
   !!---- Update: May - 2023
   !!
   Module Subroutine Write_InfoBlock_ExcludedRegions(Excl,Iunit,IPatt)
      !---- Arguments ----!
      Type(Excl_reg_type), intent(in) :: Excl
      integer, optional,   intent(in) :: Iunit
      integer, optional,   intent(in) :: IPatt

      !---- Local variables ----!
      integer :: i, lun

      lun=6
      if (present(iunit)) lun=iunit

      write(unit=lun, fmt="(a)") " "
      if(present(IPatt)) then
        write(unit=lun, fmt="(a,i4)") " Excluded Regions for Pattern: ",IPatt
      else
        write(unit=lun, fmt="(a)") " Excluded Regions: "
      end if
      write(unit=lun, fmt="(a)") " "
      write(unit=lun, fmt="(a,a)")"  Region             Start          End"

      do i=1,Excl%num_excl
         write(unit=lun,fmt="(i6,5x,2f15.3)") i, Excl%Exc(i)%mina,Excl%Exc(i)%maxb
      end do
   End Subroutine Write_InfoBlock_ExcludedRegions

   !!----
   !!---- SUBROUTINE Get_SubBlock_CommPatterns
   !!----
   !!----    Determine the zone of Pattern Blocks into a Command Zone
   !!----
   !!---- Update: June - 2023
   !!
   Module Subroutine Get_SubBlock_CommPatterns(ffile, N_ini, N_end, Bl_Patt, NPatt, C_Patt)
      !---- Arguments ----!
      type(File_type),                    intent(in)     :: ffile
      integer,                            intent(in)     :: N_ini
      integer,                            intent(in)     :: N_end
      type(BlockInfo_Type), dimension(:), intent(in)     :: Bl_Patt
      integer,                            intent(out)    :: Npatt
      type(BlockInfo_Type), dimension(:), intent(in out) :: C_Patt

      !---- Local Variables ----!
      integer               :: i, iv
      integer, dimension(2) :: ind
      character(len=60)     :: StrName

      !> Init
      NPatt=0

      i=N_ini

      do while(i < n_end)
         call Get_SubBlock_KEY('PATTERN', ffile, i, n_end, Ind, StrName)
         if (all(ind > 0)) then
            NPatt=NPatt+1

            call get_num(strName, vet, ivet, iv)
            if (iv > 0) then
               C_Patt(NPatt)%StrName=Bl_Patt(ivet(1))%StrName
            else
               C_Patt(NPatt)%StrName=trim(StrName)
            end if
            call clear_error()

            C_Patt(NPatt)%BlName='COM_PATTERN' !Command for patterns
            C_Patt(NPatt)%IBl=2                !Numerical type of block
            C_Patt(NPatt)%Nl=Ind

            i=Ind(2)+1

         else
            exit
         end if
      end do

   End Subroutine Get_SubBlock_CommPatterns

   !!----
   !!---- SUBROUTINE Get_SubBlock_CommPhases
   !!----
   !!----    Determine the zone of Pattern Blocks into a Command Zone
   !!----
   !!---- Update: June - 2023
   !!
   Module Subroutine Get_SubBlock_CommPhases(ffile, N_ini, N_end, Bl_Phas, NPhas, C_Phas)
      !---- Arguments ----!
      type(File_type),                    intent(in)     :: ffile
      integer,                            intent(in)     :: N_ini
      integer,                            intent(in)     :: N_end
      type(BlockInfo_Type), dimension(:), intent(in)     :: Bl_Phas
      integer,                            intent(out)    :: Nphas
      type(BlockInfo_Type), dimension(:), intent(in out) :: C_Phas

      !---- Local Variables ----!
      integer               :: i, iv
      integer, dimension(2) :: ind
      character(len=60)     :: StrName

      !> Init
      NPhas=0

      i=N_ini

      do while(i < n_end)
         call Get_SubBlock_KEY('PHASE', ffile, i, n_end, Ind, StrName)
         if (all(ind > 0)) then
            NPhas=NPhas+1

            call get_num(strName, vet, ivet, iv)
            if (iv > 0) then
               C_Phas(NPhas)%StrName=Bl_Phas(ivet(1))%StrName
            else
               C_Phas(NPhas)%StrName=trim(StrName)
            end if
            call clear_error()

            C_Phas(NPhas)%BlName='COM_PHASE' !Command for phases
            C_Phas(NPhas)%IBl=1              !Numerical type of block
            C_Phas(NPhas)%Nl=Ind

            i=Ind(2)+1

         else
            exit
         end if
      end do
   End Subroutine Get_SubBlock_CommPhases

   !!----
   !!---- SUBROUTINE Get_Blocks_Filetype
   !!----
   !!----
   !!---- June - 2023
   !!----
   Module Subroutine Get_Blocks_Filetype(ffile, NComm, Bl_Comm, NPatt, Bl_Patt, &
                                         NPhas, Bl_Phas, NCPatt, BlC_Patt, NCPhas, BlC_Phas)
      !---- Arguments ----!
      type(File_type),                               intent(in)  :: ffile
      integer,                            optional,  intent(out) :: NComm    ! Number of Commands zone
      type(BlockInfo_Type),               optional,  intent(out) :: Bl_Comm  ! Command block info
      integer,                            optional,  intent(out) :: NPatt    ! Number of Patterns zone
      type(BlockInfo_Type), dimension(:), optional,  intent(out) :: Bl_Patt  ! Patterns block info
      integer,                            optional,  intent(out) :: NPhas    ! Number of Phases zone
      type(BlockInfo_Type), dimension(:), optional,  intent(out) :: Bl_Phas  ! Phases block info
      integer,                            optional,  intent(out) :: NCPatt   ! Number of Patterns into Commands zone
      type(BlockInfo_Type), dimension(:), optional,  intent(out) :: BlC_Patt ! Patterns block info
      integer,                            optional,  intent(out) :: NCPhas   ! Number of Phases into Commands zone
      type(BlockInfo_Type), dimension(:), optional,  intent(out) :: BlC_Phas ! Phases block info

      !---- Local Variables ----!
      integer              :: n_ini, n_end, nbc
      type(BlockInfo_Type) :: BC

      !> Init
      call clear_error()

      nbc=0

      call Get_Block_Commands(ffile, N_Ini, N_End)
      if (Err_CFML%IErr /= 0) return

      if (n_ini > 0 .and. n_end >= n_ini) then
         BC%Nl(1)=n_ini
         BC%Nl(2)=n_end
         BC%IBl=0              !Numerical type of block
         BC%BlName='COMMAND'   !Block containing commands
         Nbc=1
      end if

      !> Command Zone
      if (present(NComm) .and. present(Bl_Comm)) then
         NComm=0
         if (nbc > 0) then
            Bl_Comm= BC
            NComm=1
         end if
      end if

      !> Patterns Zone
      if (present(NPatt) .and. present(Bl_Patt)) then
         NPatt=0

         n_ini=1
         n_end=ffile%nlines
         if (Nbc > 0) then
            call Get_Block_Patterns(ffile, n_ini, n_end, NPatt, Bl_Patt, BC%Nl)
         else
            call Get_Block_Patterns(ffile, n_ini, n_end, NPatt, Bl_Patt)
         end if
      end if

      !> Phases Zone
      if (present(NPhas) .and. present(Bl_Phas)) then
         NPhas=0

         n_ini=1
         n_end=ffile%nlines
         if (Nbc > 0) then
            call Get_Block_Phases(ffile, n_ini, n_end, NPhas, Bl_Phas, BC%Nl)
         else
            call Get_Block_Phases(ffile, n_ini, n_end, NPhas, Bl_Phas)
         end if
      end if

      !> Patterns sublocks into Command zone
      if (present(NCPatt) .and. present(BlC_Patt) .and. present(Bl_Patt)) then
         NCPatt=0

         if (nbc > 0) then
            n_ini=BC%Nl(1)
            n_end=BC%Nl(2)
            call Get_SubBlock_CommPatterns(ffile, N_ini, N_end, Bl_Patt, NCPatt, BlC_Patt)
         end if
      end if

      !> Phases sublocks into Command zone
      if (present(NCPhas) .and. present(BlC_Phas) .and. present(Bl_Phas)) then
         NCPhas=0

         if (nbc > 0) then
            n_ini=BC%Nl(1)
            n_end=BC%Nl(2)
            call Get_SubBlock_CommPhases(ffile, N_ini, N_end, Bl_Phas, NCPhas, BlC_Phas)
         end if
      end if

   End Subroutine Get_Blocks_FileType

End SubModule Format_Blocks