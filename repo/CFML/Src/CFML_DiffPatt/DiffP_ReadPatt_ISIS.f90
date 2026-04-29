SubModule (CFML_DiffPatt) DiffP_ReadPatt_ISIS

   !---- Global Variables for this SubModule ----!
   implicit none

   logical :: ralf_type  =.false.
   logical :: title_given=.false.

 Contains
    !!--++
    !!--++ READ_PATTERN_ISIS_M
    !!--++
    !!--++    Read a powder pattern from ISIS
    !!--++
    !!--++ 01/05/2019
    !!
    Module Subroutine Read_Pattern_Isis_m(Filename, VPat, NPat)
       !---- Arguments ----!
       Character(len=*),                   intent(in)  :: Filename
       class(DiffPat_Type), dimension(:),  intent(out) :: VPat
       integer,                            intent(out) :: Npat          ! Number of Patterns readed

       !---- Local Variables ----!
       integer,       parameter                        :: NPAT_MAX= 100
       real(kind=cp), parameter                        :: EPS1    =1.0e-1

       integer                                         :: ntt, i, j, ier, i_dat, npts
       integer, dimension(NPAT_MAX)                    :: npp        !number of points per pattern
       integer                                         :: n_pat      !index of current pattern

       real(kind=cp)                                   :: fac_y
       real(kind=cp)                                   :: cnorm
       real(kind=cp)                                   :: sumavar
       real(kind=cp)                                   :: divi

       character(len=120)                              :: txt1
       character(len=132)                              :: aline

       logical                                         :: bankfound, info

       !> Init
       call clear_error()

       !> File exists?
       inquire(file=trim(filename),exist=info)
       if ( .not. info) then
          Err_CFML%IErr=1
          Err_CFML%Msg="Read_Pattern_Isis_M@DIFFPATT: The file "//trim(filename)//" doesn't exist"
          return
       end if

       !> Open File
       open(newunit=i_dat,file=trim(filename),status="old",action="read",position="rewind",iostat=ier)
       if (ier /= 0 ) then
          Err_CFML%IErr=1
          Err_CFML%Msg="Read_Pattern_Isis_M@DIFFPATT: Problems opening the file: "//trim(filename)
          return
       end if

       !> Init
       fac_y=1000.0_cp
       n_pat=0
       npp=0
       bankfound=.false.
       title_given=.false.

       do
          read(unit=i_dat,fmt="(a)", iostat = ier) txt1
          if (ier /= 0) exit

          txt1=adjustl(txt1)
          if (txt1(1:4) == "BANK") then
             n_pat=n_pat+1
             if (n_pat > NPAT_MAX) then
                Err_CFML%IErr=1
                Err_CFML%Msg="Read_Pattern_Isis_M@DIFFPATT: Wrong number of patterns!"
                close(unit=i_dat)
                return
             end if
             bankfound=.true.
             cycle
          end if
          if (bankfound) npp(n_pat)=npp(n_pat)+1
       end do

       !> Define parameters
       npat=n_pat                  ! Update the number of patterns

       do n_pat=1,npat
          call Allocate_Pattern(Vpat(n_pat),npp(n_pat))
       end do

       rewind(unit=i_dat)
       do
          read(unit=i_dat,fmt="(a)") txt1
          if (.not. title_given) then
             VPat(1)%title=txt1
             title_given=.true.
          end if
          if (txt1(1:4) == "BANK") then
             IF (index(txt1,"RALF") /= 0) ralf_type =.true.
             exit
          end if
          i=index(txt1,"fac_y")
          if (i /= 0) then
             read(unit=txt1(i+5:),fmt=*) fac_y
          end if
       end do

       do n_pat=1,npat
          i=0
          ntt=0
          sumavar=0.0_cp
          cnorm=0.0_cp
          VPat(n_pat)%title=VPat(1)%title

          if (ralf_type) then
             do j=1,npp(n_pat)+1
                read(unit=i_dat,fmt="(a)",iostat=ier) aline
                if (ier /= 0)  exit

                if (aline(1:1) == "!" .or. aline(1:1) == "#") cycle
                if (aline(1:4) == "BANK") exit
                if (len_trim(aline)==0)exit

                i=i+1
                read(unit=aline,fmt=*,iostat=ier) Vpat(n_pat)%x(i),Vpat(n_pat)%y(i),Vpat(n_pat)%sigma(i)
                if (ier /= 0) then
                   Err_CFML%IErr=1
                   Err_CFML%Msg="Read_Pattern_Isis_M@DIFFPATT: Problems reading ISIS profile DATA file!"
                   close(unit=i_dat)
                   return
                end if

                if (abs(Vpat(n_pat)%x(i)) < EPS1 .and. Vpat(n_pat)%y(i) < EPS1 .and. Vpat(n_pat)%sigma(i) < EPS1) exit

                Vpat(n_pat)%y(i)    =Vpat(n_pat)%y(i)*fac_y
                Vpat(n_pat)%sigma(i)=(Vpat(n_pat)%sigma(i)*fac_y)**2.0
                sumavar=sumavar+Vpat(n_pat)%sigma(i)

                if (Vpat(n_pat)%sigma(i) < EPS1) Vpat(n_pat)%sigma(i) =fac_y
                cnorm=cnorm+Vpat(n_pat)%sigma(i)/max(abs(Vpat(n_pat)%y(i)),0.001_cp)
                if (i > 1) ntt=ntt+1
             end do

             do i=1,ntt
                divi=Vpat(n_pat)%x(i+1)-Vpat(n_pat)%x(i)
                Vpat(n_pat)%y(i)=Vpat(n_pat)%y(i)/divi
                Vpat(n_pat)%sigma(i)=Vpat(n_pat)%sigma(i)/divi/divi
             end do
             ntt=ntt-1

          else
             do j=1,npp(n_pat)
                read(unit=i_dat,fmt="(a)",iostat=ier) aline
                if (ier /= 0) exit
                if (aline(1:1) == "!" .or. aline(1:1) == "#") cycle
                if (aline(1:4) == "BANK") exit

                i=i+1
                read(unit=aline,fmt=*,iostat=ier) Vpat(n_pat)%x(i),Vpat(n_pat)%y(i),Vpat(n_pat)%sigma(i)
                if (ier /= 0) then
                   Err_CFML%IErr=1
                   Err_CFML%Msg="Read_Pattern_Isis_M@DIFFPATT: Problems reading ISIS profile DATA file"
                   close(unit=i_dat)
                   return
                end if
                if(abs(Vpat(n_pat)%x(i)) < EPS1 .and. Vpat(n_pat)%y(i) < EPS1 .and. Vpat(n_pat)%sigma(i) < EPS1) exit
                Vpat(n_pat)%y(i)=Vpat(n_pat)%y(i)*fac_y
                Vpat(n_pat)%sigma(i)=(Vpat(n_pat)%sigma(i)*fac_y)**2.0
                sumavar=sumavar+Vpat(n_pat)%sigma(i)

                if (Vpat(n_pat)%sigma(i) < EPS1) Vpat(n_pat)%sigma(i) =fac_y
                cnorm=cnorm+Vpat(n_pat)%sigma(i)/max(abs(Vpat(n_pat)%y(i)),0.001_cp)
                if (i > 1) ntt=ntt+1
             end do
          end if  !RALF question

          npts=ntt+1

          Vpat(n_pat)%npts=npts
          Vpat(n_pat)%xmin=Vpat(n_pat)%x(1)
          Vpat(n_pat)%xmax=Vpat(n_pat)%x(npts)

          cnorm=cnorm/real(npts)
          if (sumavar < EPS1) then
             do i=1,npts
                Vpat(n_pat)%sigma(i)=abs(Vpat(n_pat)%y(i))
             end do
             cnorm=1.0
          end if

          Vpat(n_pat)%ymin=minval(Vpat(n_pat)%y(1:npts))
          Vpat(n_pat)%ymax=maxval(Vpat(n_pat)%y(1:npts))
       end do !n_pat

       !> close
       close(unit=i_dat)
    End Subroutine Read_Pattern_Isis_M

End SubModule DiffP_ReadPatt_ISIS