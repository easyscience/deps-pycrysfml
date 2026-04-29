!!----
!!----
!!----
!!----
SubModule (CFML_SuperSpace_Database) TAB_Read_SSG_DBase
   Implicit none
   Contains
   !!----
   !!---- READ SUPERSPACE DATA BASE
   !!----
   !!---- Read data about magnetic space groups
   !!---- input data from magnetic_table.dat
   !!----
   !!---- 24/04/2019
   !!
   Module Subroutine Read_SSG_DBase(DB_Path, EnvDB)
      !---- Arguments ----!
      character(len=*), optional, intent(in)  :: DB_Path
      character(len=*), optional, intent(in)  :: EnvDB

      !---- Local Variables ----!
      integer :: i,j,k,m,n,nmod,iclass
      integer :: i_db, ier,L !,i_lab
      character(len=512) :: database
      character(len=40)  :: Env
      character(len=4)   :: line

      !> Init
      call clear_error()

      if (.not. SSG_DBase_allocated) call Allocate_SSG_DBase()

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
      open(newunit=i_db,file=trim(database)//'ssg_datafile.txt',status='old',action='read',position='rewind',iostat=ier)
      if (ier /= 0) then
         err_CFML%IErr=1
         err_cfml%msg= 'Error opening the database file: '//trim(database)//'ssg_datafile.txt'
         return
      end if

      ! Uncomment for creating the file SSG_Labels.txt
      !open(newunit=i_lab,file=trim(fullprof_suite)//OPS_SEP//"Databases"//OPS_SEP//"SSG_Labels.txt",status='replace',action='write',iostat=ier)
      !if(ier /= 0) then
      !  err_CFML%IErr=1
      !  err_cfml%msg= 'Error opening the labels file: '//trim(trim(fullprof_suite)//"Databases"//OPS_SEP//"Labels.txt")
      !  return
      !else                 !12345678123456  123456  123456  123456  123456  12345678901      123456
      !  write(i_lab,"(a)") "       #   Class  Pos-Cl  Pos-Gr  Gr-Num  Parent  Num-Label        SSG-Label"
      !end if

      L=0
      do i=1,2526
         read(unit=i_db,fmt="(a)") line
         if (line(1:1) == '"') then
            L=L+1
            pos_class(L)= i-1
         end if
      end do
      L=0

      do i=2527,300000
         read(unit=i_db,fmt="(a)",iostat=ier) line
         if (ier /= 0) exit
         if (line(1:1) == '"') then
            L=L+1
            pos_group(L)= i-1
         end if
      end do
      rewind(unit=i_db)

      ! skip heading
      read(unit=i_db,fmt=*)
      read(unit=i_db,fmt=*)
      read(unit=i_db,fmt=*)

      ! read number of Bravais classes
      read(unit=i_db,fmt=*) nclasses

      ! read each Bravais class
      do m=1,nclasses
         read(unit=i_db,fmt=*)n,iclass_nmod(m),iclass_number(m), iclass_spacegroup(m),iclass_nstars(m), &
                  (iclass_nmodstar(i,m),i=1,iclass_nstars(m))
         nmod=iclass_nmod(m)
         if (n /= m) then
            err_CFML%IErr=1
            write(unit=err_cfml%msg,fmt="(a,i3)") 'Error in ssg_datafile @reading Bravais class #: ',m
            close(unit=i_db)
            return
         end if

         read(unit=i_db,fmt=*)class_nlabel(m),class_label(m)
         read(unit=i_db,fmt=*)(((iclass_qvec(i,j,k,m),i=1,3),j=1,3),k=1,nmod)
         read(unit=i_db,fmt=*)iclass_ncentering(m)
         read(unit=i_db,fmt=*)((iclass_centering(i,j,m),i=1,nmod+4),j=1,iclass_ncentering(m))
      end do

      ! read number of superspace groups
      read(i_db,*)ngroups

      ! read each superspace group
      do m=1,ngroups
         !write(6,'(i5)')m
         read(i_db,*)n,igroup_number(m),igroup_class(m),igroup_spacegroup(m)
         if (n /= m)then
            err_CFML%IErr=1
            write(unit=err_cfml%msg,fmt="(a,i3)") 'Error in ssg_datafile @reading group#: ',m
            close(unit=i_db)
            return
         end if
         iclass=igroup_class(m)
         nmod=iclass_nmod(iclass)
         read(i_db,*)group_nlabel(m),group_label(m)
         read(i_db,*)igroup_nops(m)
         read(i_db,*)(((igroup_ops(i,j,k,m),i=1,nmod+4),j=1,nmod+4), k=1,igroup_nops(m))
         read(i_db,*)igroup_nconditions(m)
         if (igroup_nconditions(m) > 0)then
            read(unit=i_db,fmt=*)(((igroup_condition1(i,j,k,m),i=1,nmod+3),j=1,nmod+3),(igroup_condition2(j,k,m),j=1,nmod+4), &
                                    k=1,igroup_nconditions(m))
         end if
         !write(i_lab,"(6i8,2a)") m,iclass,pos_class(iclass),pos_group(m),igroup_number(m),igroup_spacegroup(m),"   "//group_nlabel(m),"   "//trim(group_label(m))
      end do
      close(unit=i_db)

      !close(unit=i_lab)
      SSG_DBase_allocated=.true.
   End Subroutine Read_SSG_DBase

   !!----
   !!---- READ_SINGLE_SSG
   !!----
   !!----
   !!---- 14/05/2020
   !!
   Module Subroutine Read_single_SSG(str,num,DB_Path, EnvDB)
      character(len=*),           intent(in)  :: str
      integer,                    intent(out) :: num
      character(len=*), optional, intent(in)  :: DB_Path
      character(len=*), optional, intent(in)  :: EnvDB
      !
      integer :: i,j,k,n,m,i_pos,n_skip,nmod,i_db,ier,iclass,i_lab
      character(len=512) :: ssg_file,pos_file,database,lab_file
      character(len=256) :: line
      character(len=40)  :: Env
      logical            :: found

      database=" "
      if (present(DB_Path)) then
         database=trim(db_path)
         n=len_trim(database)
         if( n /= 0) then
           if (database(n:n) /= OPS_SEP) database=trim(database)//OPS_SEP
         end if
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

      ssg_file=trim(database)//'ssg_datafile.txt'
      pos_file=trim(database)//'class+group_pos.txt'
      lab_file=trim(database)//'SSG_Labels.txt'

      if(.not. ssg_DBase_allocated) call Allocate_SSG_DBase()

      call clear_error()
      found=.false.
      !First determine the number of the space groups (it may be provided in the string "str")
      read(unit=str,fmt=*,iostat=ier) num
      if (ier /= 0) then !The provided string does not contain the number
         open(newunit=i_lab,file=trim(lab_file),status='old',action='read',position='rewind',iostat=ier)
         if(ier /= 0) then
            err_CFML%IErr=1; err_CFML%flag=.true.
            err_CFML%Msg= 'The database: '//trim(lab_file)//" has not been found in "//trim(database)
            return
         end if
         read(i_lab,*)
         do i=1,m_ngs
            read(i_lab,"(a)") line
            !write(*,"(a)") trim(line)//"     <====>   "//trim(str)
            j=index(line,trim(str))
            if (j /= 0) then
               found=.true.
               !backspace(i_lab)
               read(line,*) num
               found=.true.
               exit
            end if
         end do
         if (.not. found) then
            err_CFML%IErr=1
            err_CFML%Msg= 'The space group label: '//trim(str)//" has not been found in the database!"
            return
         end if
      end if

      ! open data file
      open(newunit=i_db,file=trim(ssg_file),status='old',action='read',position='rewind',iostat=ier)
      if (ier /= 0) then
         err_CFML%IErr=1
         err_CFML%Msg= 'Error opening the database file: '//trim(ssg_file)
         return
      end if

      open(newunit=i_pos,file=trim(pos_file),status='old',action='read',position='rewind',iostat=ier)
      if (ier /= 0) then
         err_CFML%IErr=1
         err_CFML%Msg= 'Error opening the database file: '//trim(pos_file)
         return
      end if
      read(unit=i_pos,fmt=*) !skip class line
      read(unit=i_pos,fmt=*) pos_class
      read(unit=i_pos,fmt=*) !skip group line
      read(unit=i_pos,fmt=*) pos_group
      close(unit=i_pos)
      read(unit=i_db,fmt=*)
      read(unit=i_db,fmt=*)
      read(unit=i_db,fmt=*)

      ! read number of Bravais classes
      read(unit=i_db,fmt=*) nclasses

      ! read each Bravais class
      do m=1,nclasses
         read(unit=i_db,fmt=*) n,iclass_nmod(m),iclass_number(m), iclass_spacegroup(m),iclass_nstars(m), &
                              (iclass_nmodstar(i,m),i=1,iclass_nstars(m))
         nmod=iclass_nmod(m)
         read(unit=i_db,fmt=*) class_nlabel(m),class_label(m)
         read(unit=i_db,fmt=*) (((iclass_qvec(i,j,k,m),i=1,3),j=1,3),k=1,nmod)
         read(unit=i_db,fmt=*) iclass_ncentering(m)
         read(unit=i_db,fmt=*) ((iclass_centering(i,j,m),i=1,nmod+4),j=1,iclass_ncentering(m))
      end do
      rewind(unit=i_db)
      !write(*,"(10i8)") pos_group

      n_skip=pos_group(num)-1
      !write(*,"(a,i12)") "Skipping ",n_skip
      do i=1,n_skip
         read(unit=i_db,fmt=*)
      end do
      m=num
      read(unit=i_db,fmt=*) n,igroup_number(m),igroup_class(m),igroup_spacegroup(m)
      if (n /= m)then
         err_CFML%IErr=1
         write(unit=err_CFML%Msg,fmt="(a,2i5)") 'Error in ssg_datafile @reading group#: ',m,n
         close(unit=i_db)
         return
      end if
      iclass=igroup_class(m)
      nmod=iclass_nmod(iclass)
      read(unit=i_db,fmt=*) group_nlabel(m),group_label(m)
      read(unit=i_db,fmt=*) igroup_nops(m)
      read(unit=i_db,fmt=*) (((igroup_ops(i,j,k,m),i=1,nmod+4),j=1,nmod+4), k=1,igroup_nops(m))

      close(unit=i_db) !Some variables are needed

   End Subroutine Read_single_SSG

End SubModule TAB_Read_SSG_DBase