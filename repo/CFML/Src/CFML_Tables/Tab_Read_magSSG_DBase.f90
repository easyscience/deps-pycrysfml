!!----
!!----
!!----
!!----
SubModule (CFML_magSuperSpace_Database) TAB_Read_magSSG_DBase
   Implicit none
   Contains
   !!----
   !!---- READ magSUPERSPACE DATA BASE
   !!----
   !!---- Read data about magnetic superspace groups
   !!---- input data from magnetic_table.dat
   !!----
   !!---- 16/06/2022
   !!
   Module Subroutine Read_magSSG_DBase(DB_Path, EnvDB)
      !---- Arguments ----!
      character(len=*), optional, intent(in)  :: DB_Path
      character(len=*), optional, intent(in)  :: EnvDB

      !---- Local Variables ----!
      integer :: i,j,k,m,n,nmgroups,n4, n1, n2, n3, num1, num2, num3
      integer :: i_db, ier,L, i_m1, i_m2, i_m3 !,i_lab
      integer, dimension(Mag_NGS) :: pos_group
      character(len=512) :: database
      character(len=40)  :: Env
      character(len=4)   :: line

      !> Init
      call clear_error()

      if (.not. magSSG_DBase_allocated) call Allocate_magSSG_DBase()

      !> Path
      Database=" "
      if (present(DB_Path)) then
         database = trim(DB_Path)
         n=len_trim(database)
         if (database(n:n) /= OPS_SEP) database=trim(database)//OPS_SEP
      else
         Env="CRYSFML_DB"
         if (present(EnvDB)) Env=trim(EnvDB)
         call GET_ENVIRONMENT_VARIABLE(trim(Env),database)
         n=len_trim(database)
         if (n == 0) then
            !Try FullProf
            Env="FULLPROF"
            call GET_ENVIRONMENT_VARIABLE(trim(Env),database)
            n=len_trim(database)
            if (n == 0) then
              err_CFML%IErr=1
              write(unit=err_cfml%msg,fmt="(a)") " => The "//trim(Env)//" environment variable is not defined! "//newline// &
                                               "    This is needed for localizing the data base: magnetic_data.txt"//newline// &
                                               "    that should be within the %"//trim(Env)//"% directory"
              return
            else
              database=trim(database)//OPS_SEP//"Databases"
            end if
         end if
         if (database(n:n) /= OPS_SEP) database=trim(database)//OPS_SEP
      end if

      !> Open
      open(newunit=i_db,file=trim(database)//'ssgmag_datafile.txt',status='old',action='read',position='rewind',iostat=ier)
      if (ier /= 0) then
         err_CFML%IErr=1
         err_cfml%msg= 'Error opening the database file: '//trim(database)//'ssgmag_datafile.txt'
         return
      end if

      ! Uncomment for creating the file SSG_Labels.txt
      !open(newunit=i_lab,file=trim(database)//"magSSG_Labels.txt",status='replace',action='write',iostat=ier)
      !if(ier /= 0) then
      !  err_CFML%IErr=1
      !  err_cfml%msg= 'Error opening the labels file: '//trim(database)//"magSSG_Labels.txt"
      !  return
      !else                 !12345678901*12345678901*12345678901*12345678901*12345678901*12345678901*12345678901*12345678901*
      !  write(i_lab,"(a)") "       Num-Order  Pos-Gr  NonMag-SSG   BasMagSpG    Num-mods Num-BMagSpG NumMagSSG-Label           magSSG-Label"
      !end if

      !L=0; i=0
      !
      !do
      !   i=i+1
      !   read(unit=i_db,fmt="(a)",iostat=ier) line
      !   if (ier /= 0) exit
      !   if (line(1:1) == '"') then
      !      L=L+1
      !      pos_group(L)= i-1
      !   end if
      !end do
      !rewind(unit=i_db)

      ! skip heading
      read(unit=i_db,fmt=*)
      read(unit=i_db,fmt=*)
      read(unit=i_db,fmt=*)

      ! read number of superspace groups
      read(i_db,*) nmgroups

      ! read each superspace group
      n1=0; n2=0; n3=0

      do m=1,nmgroups
        read(unit=i_db,fmt=*) n,mgroup_ssg(m),mgroup_spacegroup(m),mgroup_nmod(m),mgroup_mag(m)
        if(n /= m)then
            err_CFML%IErr=1
            write(unit=err_cfml%msg,fmt="(a,i3)") 'Error in ssgmat_datafile.txt @reading group#: ',m
            close(unit=i_db)
            return
        end if
        if(mgroup_nmod(m) == 1) n1=n1+1
        if(mgroup_nmod(m) == 2) n2=n2+1
        if(mgroup_nmod(m) == 3) n3=n3+1
        read(unit=i_db,fmt=*) mgroup_nlabel(m),mgroup_label(m)
        read(unit=i_db,fmt=*) mgroup_nops(m)
        n4=mgroup_nmod(m)+4
        read(unit=i_db,fmt=*) (mgroup_ops_r(k,m), &
                   ((mgroup_ops(i,j,k,m),i=1,n4),j=1,n4),k=1,mgroup_nops(m))
        read(unit=i_db,fmt=*) ((mgroup_transmag(i,j,m),i=1,4),j=1,4)
       !write(i_lab,"(6i12,2a)") n,pos_group(m),mgroup_ssg(m),mgroup_spacegroup(m),mgroup_nmod(m),mgroup_mag(m), &
       !                         "   "//mgroup_nlabel(m),"   "//trim(mgroup_label(m))
      end do
      close(unit=i_db)
      !close(unit=i_lab)

      !Create the binary data base for each number of modulation
      open(newunit=i_m1,file=trim(database)//"magSSG_mod1.bin", Status ="Replace",  &
           Form="Unformatted", Access="stream",Action="Write", iostat=ier)
      write(unit=i_m1) n1
      num1=n1
      do m=1,num1
        write(unit=i_m1) m,mgroup_ssg(m),mgroup_spacegroup(m),mgroup_nmod(m),mgroup_mag(m)
        write(unit=i_m1) mgroup_nlabel(m),mgroup_label(m)
        write(unit=i_m1) mgroup_nops(m)
        n4=mgroup_nmod(m)+4
        write(unit=i_m1) (mgroup_ops_r(k,m), &
                   ((mgroup_ops(i,j,k,m),i=1,n4),j=1,n4),k=1,mgroup_nops(m))
        write(unit=i_m1) ((mgroup_transmag(i,j,m),i=1,4),j=1,4)
      end do
      close(unit=i_m1)

      open(newunit=i_m2,file=trim(database)//"magSSG_mod2.bin", Status ="Replace",  &
           Form="Unformatted", Access="stream",Action="Write", iostat=ier)
      write(unit=i_m2) n2
      Num2=n1+n2
      do m=Num1+1,Num2
        write(unit=i_m2) m,mgroup_ssg(m),mgroup_spacegroup(m),mgroup_nmod(m),mgroup_mag(m)
        write(unit=i_m2) mgroup_nlabel(m),mgroup_label(m)
        write(unit=i_m2) mgroup_nops(m)
        n4=mgroup_nmod(m)+4
        write(unit=i_m2) (mgroup_ops_r(k,m), &
                   ((mgroup_ops(i,j,k,m),i=1,n4),j=1,n4),k=1,mgroup_nops(m))
        write(unit=i_m2) ((mgroup_transmag(i,j,m),i=1,4),j=1,4)
      end do
      close(unit=i_m2)

      open(newunit=i_m3,file=trim(database)//"magSSG_mod3.bin", Status ="Replace",  &
           Form="Unformatted", Access="stream",Action="Write", iostat=ier)
      write(unit=i_m3) n3
      num3=n1+n2+n3
      do m=Num2+1,Num3
        write(unit=i_m3) m,mgroup_ssg(m),mgroup_spacegroup(m),mgroup_nmod(m),mgroup_mag(m)
        write(unit=i_m3) mgroup_nlabel(m),mgroup_label(m)
        write(unit=i_m3) mgroup_nops(m)
        n4=mgroup_nmod(m)+4
        write(unit=i_m3) (mgroup_ops_r(k,m), &
                   ((mgroup_ops(i,j,k,m),i=1,n4),j=1,n4),k=1,mgroup_nops(m))
        write(unit=i_m3) ((mgroup_transmag(i,j,m),i=1,4),j=1,4)
      end do
      close(unit=i_m3)
      magSSG_DBase_allocated=.true.
   End Subroutine Read_magSSG_DBase

   !!----
   !!---- READ_SINGLE_SSG
   !!----
   !!----
   !!---- 16/06/2022
   !!
   !Module Subroutine Read_single_magSSG(str,num,DB_Path, EnvDB)
   !   character(len=*),           intent(in)  :: str
   !   integer,                    intent(out) :: num
   !   character(len=*), optional, intent(in)  :: DB_Path
   !   character(len=*), optional, intent(in)  :: EnvDB
   !   !
   !   integer :: i,j,k,n,m,i_pos,n_skip,nmod,i_db,ier,iclass,i_lab
   !   character(len=512) :: ssg_file,pos_file,database,lab_file
   !   character(len=256) :: line
   !   character(len=40)  :: Env
   !   logical            :: found
   !
   !   database=" "
   !   if (present(DB_Path)) then
   !      database=trim(db_path)
   !      n=len_trim(database)
   !      if( n /= 0) then
   !        if (database(n:n) /= OPS_SEP) database=trim(database)//OPS_SEP
   !      end if
   !   else
   !      Env="CRYSFML_DB"
   !      if (present(EnvDB)) Env=trim(EnvDB)
   !
   !      call GET_ENVIRONMENT_VARIABLE(trim(Env),database)
   !      n=len_trim(database)
   !      if (n == 0) then
   !         err_CFML%IErr=1
   !         write(unit=err_cfml%msg,fmt="(a)") " => The "//trim(Env)//"  environment variable is not defined! "//newline// &
   !                                            "    This is needed for localizing the data base: ssg_datafile.txt"//newline// &
   !                                            "    that should be within the %"//trim(Env)//"%  directory"
   !         return
   !      end if
   !      if (database(n:n) /= OPS_SEP) database=trim(database)//OPS_SEP
   !   end if
   !
   !   ssg_file=trim(database)//'ssgmag_datafile.txt'
   !
   !   if(.not. magSSG_DBase_allocated) call Allocate_magSSG_DBase()
   !
   !   call clear_error()
   !   found=.false.
   !   !First determine the number of the space groups (it may be provided in the string "str")
   !   read(unit=str,fmt=*,iostat=ier) num
   !   if (ier /= 0) then !The provided string does not contain the number
   !      open(newunit=i_lab,file=trim(lab_file),status='old',action='read',position='rewind',iostat=ier)
   !      read(i_lab,*)
   !      do i=1,m_ngs
   !         read(i_lab,"(a)") line
   !         !write(*,"(a)") trim(line)//"     <====>   "//trim(str)
   !         j=index(line,trim(str))
   !         if (j /= 0) then
   !            found=.true.
   !            !backspace(i_lab)
   !            read(line,*) num
   !            found=.true.
   !            exit
   !         end if
   !      end do
   !      if (.not. found) then
   !         err_CFML%IErr=1
   !         err_CFML%Msg= 'The space group label: '//trim(str)//" has not been found in the database!"
   !         return
   !      end if
   !   end if
   !
   !   ! open data file
   !   open(newunit=i_db,file=trim(ssg_file),status='old',action='read',position='rewind',iostat=ier)
   !   if (ier /= 0) then
   !      err_CFML%IErr=1
   !      err_CFML%Msg= 'Error opening the database file: '//trim(ssg_file)
   !      return
   !   end if
   !
   !   open(newunit=i_pos,file=trim(pos_file),status='old',action='read',position='rewind',iostat=ier)
   !   if (ier /= 0) then
   !      err_CFML%IErr=1
   !      err_CFML%Msg= 'Error opening the database file: '//trim(pos_file)
   !      return
   !   end if
   !   read(unit=i_pos,fmt=*) !skip class line
   !   read(unit=i_pos,fmt=*) pos_class
   !   read(unit=i_pos,fmt=*) !skip group line
   !   read(unit=i_pos,fmt=*) pos_group
   !   close(unit=i_pos)
   !   read(unit=i_db,fmt=*)
   !   read(unit=i_db,fmt=*)
   !   read(unit=i_db,fmt=*)
   !
   !   ! read number of Bravais classes
   !   read(unit=i_db,fmt=*) nclasses
   !
   !   ! read each Bravais class
   !   do m=1,nclasses
   !      read(unit=i_db,fmt=*) n,iclass_nmod(m),iclass_number(m), iclass_spacegroup(m),iclass_nstars(m), &
   !                           (iclass_nmodstar(i,m),i=1,iclass_nstars(m))
   !      nmod=iclass_nmod(m)
   !      read(unit=i_db,fmt=*) class_nlabel(m),class_label(m)
   !      read(unit=i_db,fmt=*) (((iclass_qvec(i,j,k,m),i=1,3),j=1,3),k=1,nmod)
   !      read(unit=i_db,fmt=*) iclass_ncentering(m)
   !      read(unit=i_db,fmt=*) ((iclass_centering(i,j,m),i=1,nmod+4),j=1,iclass_ncentering(m))
   !   end do
   !   rewind(unit=i_db)
   !   !write(*,"(10i8)") pos_group
   !
   !   n_skip=pos_group(num)-1
   !   !write(*,"(a,i12)") "Skipping ",n_skip
   !   do i=1,n_skip
   !      read(unit=i_db,fmt=*)
   !   end do
   !   m=num
   !   read(unit=i_db,fmt=*) n,igroup_number(m),igroup_class(m),igroup_spacegroup(m)
   !   if (n /= m)then
   !      err_CFML%IErr=1
   !      write(unit=err_CFML%Msg,fmt="(a,2i5)") 'Error in ssg_datafile @reading group#: ',m,n
   !      close(unit=i_db)
   !      return
   !   end if
   !   iclass=igroup_class(m)
   !   nmod=iclass_nmod(iclass)
   !   read(unit=i_db,fmt=*) group_nlabel(m),group_label(m)
   !   read(unit=i_db,fmt=*) igroup_nops(m)
   !   read(unit=i_db,fmt=*) (((igroup_ops(i,j,k,m),i=1,nmod+4),j=1,nmod+4), k=1,igroup_nops(m))
   !
   !   close(unit=i_db) !Some variables are needed
   !
   !End Subroutine Read_single_magSSG

End SubModule TAB_Read_magSSG_DBase