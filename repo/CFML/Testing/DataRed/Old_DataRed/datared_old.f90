!!-------------------------------------------------------------
!!---- FullProf software
!!
!! @license   Copyright 2019, Juan Rodriguez Carvajal, Institut Laue-Langevin All rights reserved (see LICENSE)
!! @authors   Juan Rodriguez Carvajal (see AUTHORS)
!!
!!-------------------------------------------------------------
 Program DataRed

   ! Program for single crystal data reduction
   ! Perform the averaging of a data collection according to the user given symmetry.
   ! Read data from file COLL5.hkl
    !use f2kcli
    use CFML_Crystallographic_Symmetry
    use CFML_String_Utilities
    use CFML_Symmetry_Tables
    use CFML_IO_Messages
    use CFML_Reflections_Utilities
    use CFML_Math_General,              only: sort, asind, zbelong, set_epsg
    use CFML_Crystal_Metrics
    use CFML_Propagation_vectors
    use Twin_mod  !contains z1frnb

    implicit none

    integer, parameter :: nref=400000, inp=1, iou=2, ihkl=3, irej=4, iin=10
    real,    dimension(3,nref)  :: h
    integer, dimension(3,nref)  :: hkls
    integer, dimension(nref)    :: idomain
    real,    dimension(nref)    :: intens, sigma, twtheta, omega, chi, phi, intav, sigmav,  &
                                   sigstat,lambda_laue,tbar,twotet,absorpt
    integer, dimension(nref)    :: itreat, iord, nequv, ini, fin, numor, ivk, icod, warn
    real,    dimension(3)       :: h1,h2,h3
    real,    dimension(3)       :: kv
    real,    dimension(3,48)    :: kvec
    real,    dimension(4)       :: angles
    real,    dimension(256)     :: weight  ! A maximum of 256 reflections equivalent to one of them
                                           ! can be treated
    real,    dimension(3,3)     :: transhkl
    real,    dimension(3,nmax)  :: hkln
    integer, dimension(  nmax)  :: contr
    character(len=512)          :: filein, fileout, filecon
    character(len=256)          :: line, cmdline, title
    character(len=50)           :: forma
    character(len=20)           :: line1,wmess
    character(len=1)            :: ans
    character(len=6)            :: key
    integer, dimension(3)       :: hkl  !,ha
    real, dimension(3)          :: cel,ang
    type (Space_Group_Type)     :: grp_espacial, twSpG
    type (Crystal_Cell_Type)    :: celda
    Type (Group_k_Type)         :: Gk

    character(len=*),parameter,dimension(0:1) :: warn_mess=(/"                      ",  &
                                                              " <- Dubious reflection"/)
    Logical :: Friedel=.true., prop=.false., filgiv=.false. , cell_given=.false.,                &
               wave_given=.false., powder=.false., domain=.false., twinned=.false.,              &
               transf_ind=.false.,twin_acc, hkl_real=.false., absent=.false., eps_given=.false., &
               scale_given=.false., statistics=.false.   !lauetof=.false.,
    real    :: total, sig, suma, suman, sumaw, sumanw, Rint, Rwint, aver_sig, &
               wavel,sigg, int_rej, epsg, delt, warning, scal_fact, q2, aver_int
    integer :: i,j,k, ier, nr=0, iv, ns, rej, ival, len_cmdline, nkv, nn, &
               lenf, nin, hkl_type, ivp, nk, nequiv, L, drej, Lmin,i_refout, a,b
    integer :: narg

    narg=COMMAND_ARGUMENT_COUNT()
    len_cmdline=0
    if(narg > 0) then
            call GET_COMMAND_ARGUMENT(1,cmdline)
            len_cmdline=len_trim(cmdline)
    end if

    if(len_cmdline /= 0) then
        lenf=index(cmdline," ")-1
        filecon=cmdline(1:lenf)//".red"
        open(unit=iin,file=filecon,status="old",iostat=ier, action="read")
        if(ier/=0) then
          write(unit=*,fmt="(3a,i5,a)") " => (1)File: ", trim(filecon)," not found! (Err:",ier,")"
          stop
        end if
        read(unit=iin,fmt="(a)") title
        filgiv=.true.
    else
        title=" Data reduction on the file: "
    end if


    write(unit=*,fmt="(a)") "       ==============================="
    write(unit=*,fmt="(a)") "       DATA REDUCTION PROGRAM: DataRed"
    write(unit=*,fmt="(a)") "       ==============================="
    write(unit=*,fmt="(a)") "          JRC-ILL version:13-4-2018"
    write(unit=*,fmt="(a)") "       "
    twtheta(:) =0.0
    omega(:)=0.0
    chi(:)=0.0
    phi(:)=0.0

    if(filgiv) then

       read(unit=iin,fmt="(a,a)") key, filein
       filein=adjustl(filein)
       write(unit=*,fmt="(a,a)") " => Name of the input file: ", trim(filein)
       read(unit=iin,fmt="(a,a)") key, fileout
       fileout=adjustl(fileout)
       write(unit=*,fmt="(a,a)") " => Code of the output file: ", trim(fileout)
        ans="n"
        hkl_type=1
        warning=0.25  ! 25% error for warning equivalent reflections

       do
          read(unit=iin,fmt="(a)", iostat=ier) line
          if(ier /= 0) exit
          line=adjustl(line)
          if(line(1:1) == "!") cycle

          key=u_case(line(1:5))

          Select Case(key(1:5))
             Case("KVEC ")
               read(unit=line(6:),fmt=*,iostat=ier) kv
               if(ier /= 0) then
                 write(unit=*,fmt="(a)") " => Error reading the propagation vector"
                 stop
               end if
               prop=.true.

             Case("TRANS")
               ans="y"
               transf_ind=.true.
               read(unit=line(7:),fmt=*)  ((transhkl(i,j),j=1,3),i=1,3)

             Case("SCALE")
               scale_given=.true.
               read(unit=line(6:),fmt=*)  scal_fact

             Case("STATI")
               statistics=.true.

             Case("SPGR ")
               line1=adjustl(line(6:))

             Case("EPSIL")
               read(unit=line(6:),fmt=*)  epsg
                eps_given=.true.

             Case("NFRDL")
                Friedel=.false.

             Case("CELL ")
                read(unit=line(6:),fmt=*)  cel, ang
                call Set_Crystal_Cell(cel,ang,Celda)
                cell_given=.true.

             Case("WAVE ")
                read(unit=line(6:),fmt=*)  wavel
                wave_given=.true.

             Case("HKL_T")
                read(unit=line(6:),fmt=*)  hkl_type

             Case("WARN ")
                read(unit=line(6:),fmt=*)  warning

             Case("POWDE")
                powder=.true.

             Case("TWIN ")
               call read_twinlaw(iin)
               twinned=.true.
               if(iubm) lambda=wavel

             Case("DOMAI")
               domain=.true.

          End Select

       end do

    else

       write(unit=*,fmt="(a)") " => Give the name of the input file: "
       read(unit=*,fmt="(a)") filein
       write(unit=*,fmt="(a)") " => Give the code of the output file (xx for xx.out,xx.int): "
       read(unit=*,fmt="(a)") fileout
       write(unit=*,fmt="(a)")" => Is there a propagation vector?:"
       read(unit=*,fmt="(a)") ans
       if(ans == "y" .or. ans == "Y") then
          write(unit=*,fmt="(a)")" => Give the propagation vector: "
          read(unit=*,fmt=*) kv
          prop=.true.
       end if
       write(unit=*,fmt="(a)")" => Do you want to change the indexation?:"
       read(unit=*,fmt="(a)") ans
       if(ans == "y" .or. ans == "Y") then
         write(unit=*,fmt="(a)")" => Give the transformation matrix (by rows):"
         read(unit=*,fmt=*) ((transhkl(i,j),j=1,3),i=1,3)
         transf_ind=.true.
       end if
       write(unit=*,fmt="(a)") " => Raw data (hkl F2 sigma - 1) or angles also given ( hkl F2 sigma twotheta, .. -2): "
       read(unit=*,fmt=*) hkl_type

    end if

    if(eps_given) then
       call set_epsg(epsg) !make epsilon for real/integer comparisons
       write(unit=*,fmt="(a,f8.4)")" => Epsilon for integer/real comparisons set to: ",epsg
    else
       call set_epsg(0.01) !default value in datared
       write(unit=*,fmt="(a,f8.4)")" => Default value of Epsilon for integer/real comparisons : ",0.01
    end if

    open(unit=inp, file=filein, status="old", iostat=ier, action="read")
    if( ier /= 0 ) then
       write(unit=*,fmt="(3a,i5,a)") " => (2)File: ", trim(filein)," not found! (err:",ier,")"
       stop
    end if

    numor(:)=0

    if(.not. scale_given) scal_fact=1.0

    open(unit=iou,  file=trim(fileout)//".out", status="replace", iostat=ier,action="write")
    if( ier /= 0 ) then
       write(unit=*,fmt="(a,i5,2a)") " ==> Error:",ier," (3)File (replace): ", trim(fileout)//".out"
       stop
    end if

    open(unit=ihkl, file=trim(fileout)//".int", status="replace", iostat=ier,action="write")
    if( ier /= 0 ) then
       write(unit=*,fmt="(a,i5,2a)") " ==> Error:",ier," (4)File (replace): ", trim(fileout)//".int"
       stop
    end if

    open(unit=irej, file=trim(fileout)//".rej", status="replace", iostat=ier,action="write")
    if( ier /= 0 ) then
       write(unit=*,fmt="(a,i5,2a)") " ==> Error:",ier," (5)File (replace): ", trim(fileout)//".rej"
       stop
    end if


!  First read the input file
    nr=0

    Select Case(hkl_type)

        Case(0)       !Shelx-like input file (3i4,2f8.2)

            if(.not. cell_given) then
              write(unit=*,fmt=*)" => UNIT CELL not GIVEN! Modify your input file."
              stop
            end if
            if(.not. wave_given) then
              write(unit=*,fmt=*)" => WAVELENGTH not GIVEN! Modify your input file."
              stop
            end if

            do
             nr=nr+1
             read(unit=inp,fmt="(3i4,2f8.2)",iostat=ier) hkl(:), intens(nr), sigma(nr)
              if(ier /= 0 .or. (hkl(1)==0 .and. hkl(2)==0 .and. hkl(3) == 0) ) then
               nr=nr-1
               exit
              end if
              h1(:)=real(hkl(:))
              if(transf_ind) then
                h2=matmul(transhkl,h1)
              else
                h2=h1
              end if
              twtheta(nr)=2.0* ASIND( hkl_s(h2,celda)*WAVEL)
              if(twtheta(nr) < 0.0001) ier=1
              if(intens(nr) < 0.0001) intens(nr)=0.0001
              if(sigma(nr) < 0.00001) sigma(nr)=sqrt(abs(intens(nr)))
              !!!!! h(:,nr)=h1(:)  !error
              h(:,nr)=h2(:)
              if(intens(nr) <= 0.0 ) intens(nr)=0.001
              if(sigma (nr) <= 0.0 )  sigma(nr)=0.004
            end do

        Case(10)       !Shelx-like HKLF5 input file (3i4,2f8.2,i4)

            if(.not. cell_given) then
              write(unit=*,fmt=*)" => UNIT CELL not GIVEN! Modify your input file."
              stop
            end if
            if(.not. wave_given) then
              write(unit=*,fmt=*)" => WAVELENGTH not GIVEN! Modify your input file."
              stop
            end if

            do
             nr=nr+1
             read(unit=inp,fmt="(3i4,2f8.2,i4)",iostat=ier) hkl(:), intens(nr), sigma(nr),idomain(nr)
              if(ier /= 0 .or. (hkl(1)==0 .and. hkl(2)==0 .and. hkl(3) == 0) ) then
               nr=nr-1
               exit
              end if
              h1(:)=real(hkl(:))
              if(transf_ind) then
                h2=matmul(transhkl,h1)
              else
                h2=h1
              end if
              twtheta(nr)=2.0* ASIND( hkl_s(h2,celda)*WAVEL)
              if(twtheta(nr) < 0.0001) ier=1
              if(intens(nr) < 0.0001) intens(nr)=0.0001
              if(sigma(nr) < 0.00001) sigma(nr)=sqrt(abs(intens(nr)))
              !!!!! h(:,nr)=h1(:)  !error
              hkls(:,nr)=h2(:)
              if(intens(nr) <= 0.0 ) intens(nr)=0.001
              if(sigma (nr) <= 0.0 )  sigma(nr)=0.004
            end do

        Case(1)           !Free format  h,k,l,int,sigma (h,k,l real)

            if(.not. cell_given) then
              write(unit=*,fmt=*)" => UNIT CELL not GIVEN! Modify your input file."
              stop
            end if
            if(.not. wave_given) then
              write(unit=*,fmt=*)" => WAVELENGTH not GIVEN! Modify your input file."
              stop
            end if
            do
             nr=nr+1
             read(unit=inp,fmt=*,iostat=ier) h1(:), intens(nr), sigma(nr)
              if(ier /= 0) then
               nr=nr-1
               exit
              end if
             if(.not. zbelong(h1(:))) then
               if(prop) then
                 hkl_real=.true.
               else
                 if(transf_ind) then
                    !h(:,nr)=matmul(transhkl,h(:,nr))   !Error
                    h1=matmul(transhkl,h1)
                    if(.not. zbelong(h1(:))) then
                      write(unit=*,fmt="(a,3f8.3)")" => Removed reflection (non-integer): ",h1(:)
                      nr= nr-1
                      cycle
                    end if
                 else
                    write(unit=*,fmt="(a,3f8.3)")" => Removed reflection (non-integer): ",h1(:)
                    nr= nr-1
                    cycle
                 end if
               end if
             else
               if(transf_ind) then
                  h1=matmul(transhkl,h1)
               else
                  h1=real(nint(h1))
               end if
             end if
             twtheta(nr)=2.0* ASIND( hkl_s(h1,celda)*WAVEL)
             if(twtheta(nr) < 0.0001) ier=1
             if(intens(nr) < 0.0001) intens(nr)=0.0001
             if(sigma(nr) < 0.00001) sigma(nr)=sqrt(abs(intens(nr)))
             h(:,nr)=h1(:)
             if(intens(nr) <= 0.0 ) intens(nr)=0.001
             if(sigma (nr) <= 0.0 )  sigma(nr)=0.004
            end do

        Case(2)     ! Jane's format for .fsq:   Two lines per reflection

            if(.not. cell_given) then
              write(unit=*,fmt=*)" => UNIT CELL not GIVEN! Modify your input file."
              stop
            end if
            if(.not. wave_given) then
              write(unit=*,fmt=*)" => WAVELENGTH not GIVEN! Modify your input file."
              stop
            end if
            line="(i6,3f7.3,2f10.2,3f8.2)"
            do
             nr=nr+1
             read(unit=inp,fmt=line,iostat=ier) &
                 numor(nr),h(:,nr), intens(nr), sigma(nr), omega(nr), chi(nr), phi(nr)
             if(ier /= 0) then
               nr=nr-1
               exit
             end if
             if(.not. zbelong(h(:,nr))) then
               if(prop) then
                 hkl_real=.true.
               else
                 if(transf_ind) then
                    h(:,nr)=matmul(transhkl,h(:,nr))
                 else
                    write(unit=*,fmt="(a,3f8.3)")" => Removed reflection (non-integer): ",h(:,nr)
                    nr= nr-1
                    cycle
                 end if
               end if
             else
                 if(transf_ind) then
                    h(:,nr)=matmul(transhkl,h(:,nr))
                 else
                    h(:,nr)=real(nint(h(:,nr)))
                 end if
             end if
             if(cell_given .and. wave_given) then
               h2=h(:,nr)
               twtheta(nr)=2.0* ASIND( hkl_s(h2,celda)*WAVEL)
             end if
             if(intens(nr) <= 0.0 ) intens(nr)=0.001
             if(sigma (nr) <= 0.0 )  sigma(nr)=0.004
             read(unit=inp,fmt="(a)",iostat=ier) key
             if(ier /= 0) exit
            end do

        Case(3)                     !FullProf format

            read(unit=inp,fmt="(a)") line
            read(unit=inp,fmt="(a)") line
            read(unit=inp,fmt=*) wavel
            do
             nr=nr+1
             read(unit=inp,fmt=line,iostat=ier) &
                  hkl(:), intens(nr), sigma(nr),i,twtheta(nr), omega(nr), chi(nr), phi(nr)
             if(ier /= 0) then
               nr=nr-1
               exit
             end if

             if(cell_given .and. wave_given) then
               if(transf_ind) then
                  h2=matmul(transhkl,hkl)
               else
                  h2=hkl
               end if
               twtheta(nr)=2.0* ASIND( hkl_s(h2,celda)*WAVEL)
             end if
             h(:,nr)= h2
             if(intens(nr) <= 0.0 ) intens(nr)=0.001
             if(sigma (nr) <= 0.0 )  sigma(nr)=0.004
            end do

        Case(4)                  !COLL 5 hkl integer

            do
             nr=nr+1
             read(unit=inp,fmt="(i6,3i4,2f10.2,4f8.2)",iostat=ier) &
                  numor(nr),hkl(:), intens(nr), sigma(nr),twtheta(nr), omega(nr), chi(nr), phi(nr)
             if(ier /= 0 .or. numor(nr) == 0) then
               nr=nr-1
               exit
             end if

             if(cell_given .and. wave_given) then
               if(transf_ind) then
                  h2=matmul(transhkl,real(hkl))
               else
                  h2=real(hkl)
               end if
               twtheta(nr)=2.0* ASIND( hkl_s(h2,celda)*WAVEL)
             end if
             !write(*,"(i8,3i4,tr5,3i4,f14.3)") nr,hkl, nint(h2), intens(nr)
             h(:,nr)= h2
             if(intens(nr) <= 0.0 ) intens(nr)=0.001
             if(sigma (nr) <= 0.0 )  sigma(nr)=0.004
            end do

        Case(5) !COLL 5 hkl real numbers (old and new format with autodetection)

            read(unit=inp,fmt="(a)" ,iostat=ier) line  !Detect one of the two formats
            if(line(10:10) == ".") then  !If this position is . the old format is active
              if(line(16:16) == ".") then
                forma="(i6,3f6.2,f8.0,f4.0,4f8.2)"
              else
                forma="(i6,3f7.3,2f10.2,4f8.2)"
              end if
            else ! autodetection of the new format
              a = index(line, ".")
              b = index(line(a+1:), ".")
              if (b < 10) then
                write(forma,"(a,i1,a,i1,a)") '(i6,3f', b, '.', b+6-a, ',2f10.2,4f8.2)'
              else
                write(forma,"(a,i2,a,i1,a)") '(i6,3f', b, '.', b+6-a, ',2f10.2,4f8.2)'
              end if
              write(unit=*,fmt=*)"=> Will use format ",forma
            end if
            rewind(unit=inp)
            do
             nr=nr+1
             read(unit=inp,fmt=forma ,iostat=ier) &
                  numor(nr), h(:,nr), intens(nr), sigma(nr),twtheta(nr), omega(nr), chi(nr), phi(nr)
             if(ier /= 0) then
               nr=nr-1
               exit
             end if

             if(.not. zbelong(h(:,nr))) then
               if(prop) then
                 hkl_real=.true.
               else
                 if(transf_ind) then
                    h(:,nr)=matmul(transhkl,h(:,nr))
                 else
                    write(unit=*,fmt="(a,3f8.3)")" => Removed reflection (non-integer): ",h(:,nr)
                    nr= nr-1
                    cycle
                 end if
               end if
             else
               if(transf_ind) then
                  h(:,nr)=matmul(transhkl,h(:,nr))
               else
                  h(:,nr)=real(nint(h(:,nr)))
               end if
             end if

             if(cell_given .and. wave_given) then
                  h2=h(:,nr)
                  twtheta(nr)=2.0* ASIND( hkl_s(h2,celda)*WAVEL)
             end if
             if(intens(nr) <= 0.0 ) intens(nr)=0.001
             if(sigma (nr) <= 0.0 )  sigma(nr)=0.004
            end do

        Case(6)    !COLL 5 hkl real numbers - 6T2 ?  Only difference with 4: i4 instead of i6 for the numor
                   !and 3f6.2 instead of 3i4 for reading the hkl-indices

            do
             nr=nr+1
             read(unit=inp,fmt="(i4,3f6.2,2f10.2,4f8.2)" ,iostat=ier) &
                  numor(nr), h(:,nr), intens(nr), sigma(nr),twtheta(nr), omega(nr), chi(nr), phi(nr)
             if(ier /= 0) then
               nr=nr-1
               exit
             end if
             if(.not. zbelong(h(:,nr))) then
               if(prop) then
                 hkl_real=.true.
               else
                 if(transf_ind) then
                    h(:,nr)=matmul(transhkl,h(:,nr))
                 else
                    write(unit=*,fmt="(a,3f8.3)")" => Removed reflection (non-integer): ",h(:,nr)
                    nr= nr-1
                    cycle
                 end if
               end if
             else
               if(transf_ind) then
                  h(:,nr)=matmul(transhkl,h(:,nr))
               else
                  h(:,nr)=real(nint(h(:,nr)))
               end if
             end if
             if(cell_given .and. wave_given) then
               h2=h(:,nr)
               twtheta(nr)=2.0* ASIND( hkl_s(h2,celda)*WAVEL)
             end if
             if(intens(nr) <= 0.0 ) intens(nr)=0.001
             if(sigma (nr) <= 0.0 )  sigma(nr)=0.004
            end do

        Case(7)    ! SXD data for FullProf

            if(.not. cell_given) then
              write(unit=*,fmt=*)" => UNIT CELL not GIVEN! Modify your input file."
              stop
            end if

            if(prop) hkl_real=.true.

            do
             read(unit=inp,fmt="(a)",iostat=ier) line
             if( ier /= 0) exit
             write(unit=ihkl,fmt="(a)") line
             if(line(1:1) == "#" .or. line(1:1) == "!") cycle
             if(line(1:1) == "(") then
              forma=trim(line)
              read(unit=inp,fmt="(a)",iostat=ier) line
              write(unit=ihkl,fmt="(a)") line
              if(prop) then
                  read(unit=inp,fmt="(a)",iostat=ier) line
                  write(unit=ihkl,fmt="(a)") line
                  read(unit=line,fmt=*,iostat=ier) nkv
                  do i=1,nkv
                     read(unit=inp,fmt="(a)",iostat=ier) line
                     write(unit=ihkl,fmt="(a)") line
                     read(unit=line,fmt=*,iostat=ier) j,kvec(:,i)
                  end do
              end if
              exit
             end if
            end do

            do
             read(unit=inp,fmt="(a)",iostat=ier) line
             if( ier /= 0) exit
             if(line(1:1) == "#" .or. line(1:1) == "!") then
               write(unit=ihkl,fmt="(a)") line
               cycle
             end if
             nr=nr+1
             if(prop) then
               read(unit=line,fmt=forma,iostat=ier) hkl(:), ivk(nr),intens(nr), sigma(nr), icod(nr), Lambda_Laue(nr), &
                                                    twotet(nr),absorpt(nr),tbar(nr)
                if(ier /= 0 .or. (hkl(1)==0 .and. hkl(2)==0 .and. hkl(3) == 0 .and. ivk(nr) == 0) ) then
                 nr=nr-1
                 exit
                end if
             else
               read(unit=line,fmt=forma,iostat=ier) hkl(:), intens(nr), sigma(nr), icod(nr), Lambda_Laue(nr), &
                                                    twotet(nr),absorpt(nr),tbar(nr)
                if(ier /= 0 .or. (hkl(1)==0 .and. hkl(2)==0 .and. hkl(3) == 0) ) then
                 nr=nr-1
                 exit
                end if
             end if
             h1(:)=real(hkl(:))
              if(transf_ind) then
                h2=matmul(transhkl,h1)
                if(prop) then
                  h3=matmul(transhkl,kvec(:,ivk(nr)))
                  h2=h2+h3
                end if
              else
                h2=h1
                if(prop) h2=h2+kvec(:,ivk(nr))
              end if
              twtheta(nr)=hkl_s(h2,celda)
              if(twtheta(nr) < 0.0001) ier=1
              if(intens(nr) < 0.0001) intens(nr)=0.0001
              if(sigma(nr) < 0.00001) sigma(nr)=sqrt(abs(intens(nr)))
              if(prop) then
                h(:,nr)=h1(:)+kvec(:,ivk(nr))
              else
                h(:,nr)=h1(:)
              end if
              if(intens(nr) <= 0.0 ) intens(nr)=0.001
              if(sigma (nr) <= 0.0 )  sigma(nr)=0.004
            end do

        Case(8)       ! Flipping ratios from D3

            if(.not. cell_given) then
              write(unit=*,fmt=*)" => UNIT CELL not GIVEN! Modify your input file."
              stop
            end if
            if(.not. wave_given) then
              write(unit=*,fmt=*)" => WAVELENGTH not GIVEN! Modify your input file."
              stop
            end if
            line="(i8,6f8.3,2f10.2)"
            do
             nr=nr+1      ! gamma  omega  nu  => in fact it is: omega, gamma, nu!!!!
             read(unit=inp,fmt=line,iostat=ier) &
                 numor(nr),h(:,nr), omega(nr), phi(nr),  chi(nr),  intens(nr), sigma(nr)
                 !                   ^omega     ^gamma    ^nu
             if(ier /= 0) then
               nr=nr-1
               exit
             end if
             if(.not. zbelong(h(:,nr))) then
                 if(transf_ind) then
                    h(:,nr)=matmul(transhkl,h(:,nr))
                 else
                    write(unit=*,fmt="(a,3f8.3)")" => Removed reflection (non-integer): ",h(:,nr)
                    nr= nr-1
                    cycle
                 end if
             else
                 if(transf_ind) then
                    h(:,nr)=matmul(transhkl,h(:,nr))
                 else
                    h(:,nr)=real(nint(h(:,nr)))
                 end if
             end if
             if(cell_given .and. wave_given) then
               h2=h(:,nr)
               twtheta(nr)=2.0* ASIND( hkl_s(h2,celda)*WAVEL)
             end if
             if(intens(nr) <= 0.0 ) intens(nr)=0.001
             if(sigma (nr) <= 0.0 )  sigma(nr)=0.004
            end do

        Case (9)  !i6,3f10.4,2f10.2,4f8.2  Suggested by Juerg Schefer

            do
             nr=nr+1
             read(unit=inp,fmt="(i6,3f10.4,2f10.2,4f8.2)" ,iostat=ier) &
                  numor(nr), h(:,nr), intens(nr), sigma(nr),twtheta(nr), omega(nr), chi(nr), phi(nr)
             if(ier /= 0) then
               nr=nr-1
               exit
             end if

             if(.not. zbelong(h(:,nr))) then
               if(prop) then
                 hkl_real=.true.
               else
                 if(transf_ind) then
                    h(:,nr)=matmul(transhkl,h(:,nr))
                 else
                    write(unit=*,fmt="(a,3f8.3)")" => Removed reflection (non-integer): ",h(:,nr)
                    nr= nr-1
                    cycle
                 end if
               end if
             else
               if(transf_ind) then
                  h(:,nr)=matmul(transhkl,h(:,nr))
               else
                  h(:,nr)=real(nint(h(:,nr)))
               end if
             end if

             if(cell_given .and. wave_given) then
                  h2=h(:,nr)
                  twtheta(nr)=2.0* ASIND( hkl_s(h2,celda)*WAVEL)
             end if
             if(intens(nr) <= 0.0 ) intens(nr)=0.001
             if(sigma (nr) <= 0.0 )  sigma(nr)=0.004
            end do
    End Select


    write(unit=*,fmt="(a,i6)") " => Total number of reflections read: ", nr
    if(nr <= 2) then
      write(unit=*,fmt="(a)") " => The number of reflections is TOO LOW! ... there is a problem!"
      write(unit=*,fmt="(a,a)") " => Last read line: ",trim(line)
      stop
    end if
    !
    !  Order the reflections by ascending twtheta
    !
    if(hkl_type /= 10) then
      call sort(twtheta,nr,iord)

      open(unit=99,status="scratch",form="unformatted", iostat=ier, action="readwrite")
      if( ier /= 0 ) then
         write(unit=*,fmt="(a,i5,a)") " ==> Error:",ier," (6)Unformatted file"
         stop
      end if

      if(hkl_type == 1) then
        do i=1,nr
          k=iord(i)
          write(unit=99) h(:,k), intens(k), sigma(k), twtheta(k)
        end do
        rewind (unit=99)
        do k=1,nr
          read(unit=99) h(:,k), intens(k), sigma(k), twtheta(k)
        end do

      else if(hkl_type == 7) then

        if(prop) then
            do i=1,nr
              k=iord(i)
              write(unit=99)  h(:,k), ivk(k),intens(k), sigma(k), icod(k), Lambda_Laue(k), twotet(k),absorpt(k),tbar(k)
            end do
            rewind (unit=99)
            do k=1,nr
              read(unit=99)   h(:,k), ivk(k),intens(k), sigma(k), icod(k), Lambda_Laue(k), twotet(k),absorpt(k),tbar(k)
            end do
        else
            do i=1,nr
              k=iord(i)
              write(unit=99) h(:,k), intens(k), sigma(k), icod(k), Lambda_Laue(k), twotet(k),absorpt(k),tbar(k)
            end do
            rewind (unit=99)
            do k=1,nr
              read(unit=99)  h(:,k), intens(k), sigma(k), icod(k), Lambda_Laue(k), twotet(k),absorpt(k),tbar(k)
            end do
        end if

      else

        do i=1,nr
          k=iord(i)
          write(unit=99) numor(k), h(:,k), intens(k), sigma(k), twtheta(k), omega(k), chi(k), phi(k)
        end do
        rewind (unit=99)
        do k=1,nr
          read(unit=99)  numor(k), h(:,k), intens(k), sigma(k), twtheta(k), omega(k), chi(k), phi(k)
        end do
      end if
      close(unit=99)
      write(unit=*,fmt="(a)")" => Reflections ordered by ascending two-theta O.K.!"
    end if  !hkl_type /= 10

    write(unit=iou,fmt="(a)") "       ==============================="
    write(unit=iou,fmt="(a)") "       DATA REDUCTION PROGRAM: DataRed"
    write(unit=iou,fmt="(a)") "       ==============================="
    write(unit=iou,fmt="(a)") "          JRC-ILL version:30-5-2006"
    write(unit=iou,fmt="(a)") "       "

    if(filgiv) then
      write(unit=iou,fmt="(a,a)") " Control file: ", trim(filecon)
    else
      write(unit=iou,fmt="(a)")   " Control file: interactive"
    end if
    write(unit=iou,fmt="(a,a)")   " Input   file: ", trim(filein)
    write(unit=iou,fmt="(a,a)")   " Output  file: ", trim(fileout)//".out"
    write(unit=iou,fmt="(a,a)")   " Reflex  file: ", trim(fileout)//".int"
    write(unit=iou,fmt="(a,a//)") " Reject  file: ", trim(fileout)//".rej"

    Select Case(hkl_type)
      Case(0)       !Shelx-like input file (3i4,2f8.2)
         write(unit=iou,fmt="(a/)") " Format of the reflections file =>  ShelX-like format h k l intens sigma (3i4,2f8.2)"
      case(1)
         write(unit=iou,fmt="(a/)") " Format of the reflections file =>  FREE format:  h k l intens sigma "
      case(2)
         write(unit=iou,fmt="(a)")  " Data from COLL5 (two lines per reflection) *.fsq           "
         write(unit=iou,fmt="(a)")  " Format of the reflections file =>  (i6,3f7.3,2f10.2,3f8.2) "
         write(unit=iou,fmt="(a/)") " For reading the items: Numor, h k l Int Sigma gamma nu phi"
      case(3)
         write(unit=iou,fmt="(a)")  " Data in FullProf *.int format           "
         write(unit=iou,fmt="(a)")  " Format of the reflections read in the file "
         write(unit=iou,fmt="(a/)") " For reading the items:  h k l Int Sigma angles(1:4)"
      case(4)
         write(unit=iou,fmt="(a)")  " Data from COLL5 (1 line per reflection) *.col (hkl-integer)      "
         write(unit=iou,fmt="(a)")  " Format of the reflections file =>  (i6,3i4,2f10.2,4f8.2) "
         write(unit=iou,fmt="(a/)") " For reading the items: Numor, h k l Int Sigma theta omega chi phi"
      case(5)
         write(unit=iou,fmt="(a)")  " Data from COLL5 (1 line per reflection) *.col (hkl-real)"
         write(unit=iou,fmt="(a)")  " Format of the reflections file =>  "//trim(forma)
         write(unit=iou,fmt="(a/)") " For reading the items: Numor, h k l Int Sigma theta omega chi phi"
      case(6)
         write(unit=iou,fmt="(a)")  " Data from COLL5 (1 line per reflection) *.col (hkl-real) 6T2-LLB"
         write(unit=iou,fmt="(a)")  " Format of the reflections file =>  (i4,3f6.2,2f10.2,4f8.2) "
         write(unit=iou,fmt="(a/)") " For reading the items: Numor, h k l Int Sigma theta omega chi phi"
      case(7)
         write(unit=iou,fmt="(a)")  " Data from SXD in format adequate for FullProf"
         write(unit=iou,fmt="(a,a)")" Format of the reflections file => " , trim(forma)
         if(prop) then
             write(unit=iou,fmt="(a/)") " For reading:  h k l ivk Fsqr s(Fsqr) Cod  Lambda Twotheta Absorpt. Tbar "
         else
             write(unit=iou,fmt="(a/)") " For reading:  h k l Fsqr s(Fsqr) Cod  Lambda Twotheta Absorpt. Tbar "
         end if
      case(8)
         write(unit=iou,fmt="(a)")  " Data from D3 file (1 line per reflection) *.bpbres (hkl-real) D3-ILL"
         write(unit=iou,fmt="(a)")  " (Warning: if the orientation of the crytal is not in a  "
         write(unit=iou,fmt="(a)")  "  symmetry direction, use 'P 1' as space group)"
         write(unit=iou,fmt="(a)")  " Format of the Flipping Ratio reflections file =>  (i8,6f8.3,2f10.2) "
         write(unit=iou,fmt="(a/)") " For reading the items: Numor, h k l omega gamma nu R Sigma(R) "
      case(9)
         write(unit=iou,fmt="(a)")  " Data from Modified COLL5 (1 line per reflection) *.col (hkl-real)"
         write(unit=iou,fmt="(a)")  " Format of the reflections file =>  (i6,3f10.4,2f10.2,4f8.2) "
         write(unit=iou,fmt="(a/)") " For reading the items: Numor, h k l Int Sigma theta omega chi phi"
      case(10)
         write(unit=iou,fmt="(a)")  " Data from SHELX HKLF5-format (domains have been treated outside DataRed)"
         write(unit=iou,fmt="(a)")  " Format of the reflections file =>  (3i4,2f8.0,i4) "
         write(unit=iou,fmt="(a/)") " For reading the items: h k l Int Sigma domain_code"
    End Select


    write(unit=iou,fmt="(a,i6)") " => Number of reflections read: ",nr

    if(scale_given) then
      write(unit=iou,fmt="(a,f8.4,a)") " => A scale factor ",scal_fact," has been applied to intensities and sigmas "
      intens(1:nr)=intens(1:nr)*scal_fact
      sigma(1:nr)= sigma(1:nr)*scal_fact
    end if

    if(hkl_real) then
       write(unit=*,fmt="(a)")   " => hkl indices will be treated as real numbers "
       write(unit=iou,fmt="(a)") " => hkl indices will be treated as real numbers "
    end if

    if(prop) write(unit=iou,fmt="(a,3f8.4)") " => Input propagation vector: ", kv

    if(hkl_type /= 10) then
      if(statistics) then
         write(unit=*,fmt="(a)")   &
         " => Statistical errors are considered for sigmas of average intensisites (propagation error formula)"
         write(unit=iou,fmt="(a)") &
         " => Statistical errors are considered for sigmas of average intensisites (propagation error formula)"
      else
         write(unit=*,fmt="(a)")   &
         " => Statistics is NOT considered for sigmas of average intensities: exp. variance weighted with 1/sigmas^2"
         write(unit=iou,fmt="(a)") &
         " => Statistics is NOT considered for sigmas of average intensities: exp. variance weighted with 1/sigmas^2"
      end if
    end if

    if(transf_ind) then
       write(unit=iou,fmt="(a)") " => Input indices trasformed with matrix: "
       write(unit=iou,fmt="(a,3f7.2,a)") "         (Hnew)     (",transhkl(1,:)," )   (Hold)"
       write(unit=iou,fmt="(a,3f7.2,a)") "         (Knew)  =  (",transhkl(2,:)," )   (Kold)"
       write(unit=iou,fmt="(a,3f7.2,a)") "         (Lnew)     (",transhkl(3,:)," )   (Lold)"

       if(prop) then
         kv=matmul(transhkl,kv)
         write(unit=iou,fmt="(a,3f8.4)") " => Transformed propagation vector: ", kv
       end if
    end if
    !
    !  Set symmetry
    !
    warn(1:nr)=0
    if(.not. filgiv) then
     write(unit=*,fmt="(a)",advance="no") " => Give the symbol (H-M,Hall,number) of the space group: "
     read(unit=*,fmt="(a)") line1
     line1=adjustl(line1)
    end if
    write(unit=iou,fmt="(/,a,a)") " => Input Space Group Symbol: ", trim(line1)

    call Set_SpaceGroup(line1,grp_espacial)
    call write_spacegroup(grp_espacial,iou)
    if(cell_given) then
      call write_Crystal_cell(celda,iou)
      if(twinned) call write_twinlaw(iou,celda)
    else
      if(twinned) call write_twinlaw(iou)
    end if

    if(twinned .and. ISpG)  then
      call Set_SpaceGroup(twin_SpG,twSpG)
      write(unit=iou,fmt="(/,a)") "     SPACE GROUP FOR DOMAINS OF A TWINNED CRYSTAL: USED FOR SYSTEMATIC ABSENCES"
      write(unit=iou,fmt="( a)") "     =========================================================================="
      call write_spacegroup(twSpG,iou)
    end if

    if(grp_espacial%centred == 1 .and. .not. filgiv) then
      write(unit=*,fmt="(a)") " => Non-centrosymmetric group, is the Friedel law obeyed? (y/n): "
      read(unit=*,fmt="(a)") ans
      if(ans == "n" .or. ans == "N") Friedel=.false.
    end if
    !
    !  Determination of the propagation vector group
    !
    if(prop) then
      call K_Star(kv,grp_espacial,Gk)
      call Write_Group_k(Gk,iou)
    end if

   if(hkl_type == 10) then !The only data reduction applied concerns the symmetry ABSENCES
     write(unit=ihkl,fmt="(a)") title
     write(unit=ihkl,fmt="(a)") "(3i4,2F14.4,i5,4f8.2)"
     write(unit=ihkl,fmt="(f9.5,a)") wavel,"   0   1"
     rej=0
     do i=1,nr
       wmess=" "
       if(idomain(i) < 0) then
         intens(i) = -1.0
         sigma(i)  =  1.0
       end if
       if(hkl_absent(hkls(:,i),grp_espacial)) then
         rej=rej+1
         wmess="  <-  Forbidden reflection"
         write(unit=irej,fmt="(3i4,2F12.3,i5,f8.2,a)") &
         hkls(:,i),intens(i),sigma(i),twtheta(i),wmess
       end if
       write(unit=ihkl,fmt="(3i4,2F14.4,i5,4f8.2,a)") &
       hkls(:,i), intens(i),sigma(i),abs(idomain(i)), twtheta(i), 0.0, 0.0, 0.0,wmess
     end do
     write(unit=*,fmt="(/,a,i10)")  " => Number of reflections read               : ", nr
     write(unit=*,fmt="(a,i10)")    " => Number of rejected (absences) reflections: ", rej
     write(unit=*,fmt="(a)")        " => No averaging/merging reflections for HKL_TYPE=10 (HKLF5 Shelx) "
     write(unit=iou,fmt="(/,a,i10)")" => Number of reflections read               : ", nr
     write(unit=iou,fmt="(a,i10)")  " => Number of rejected (absences) reflections: ", rej
     write(unit=iou,fmt="(a)")      " => No averaging/merging reflections for HKL_TYPE=10 (HKLF5 Shelx) "

   else
     !
     !  First loop over reflections
     !
     nin=0
     itreat(:)=0
     ini(:)=0
     fin(:)=0
     total=0.0
     if(transf_ind) then
       write(unit=iou,fmt="(/,a)") "    DETAILED TREATMENT OF REFLECTIONS USING TRANSFORMED INDICES AS GIVEN ABOVE"
       write(unit=iou,fmt="(a,/)") "    =========================================================================="
     else
       write(unit=iou,fmt="(/,a)") "    DETAILED TREATMENT OF REFLECTIONS USING INDICES AS IN DATA COLLECTION"
       write(unit=iou,fmt="(a,/)") "    ====================================================================="
     end if
     if(prop) then
      write(unit=iou,fmt="(a)")  &
      "  No      hr      kr      lr     NEqv   Ini   Fin      <Inten>       Sigma      h   k   l   iv       Numor    2Theta"
     else if(hkl_real) then
      write(unit=iou,fmt="(a)")  &
      "  No      hr      kr      lr    NEqv   Ini   Fin     <Inten>      Sigma       Numor    2Theta"
     else
      write(unit=iou,fmt="(a)")  &
      "      No   h   k   l  NEqv   Ini   Fin     <Inten>       Sigma       Numor    2Theta"
     end if

     rej=0
     i_refout=max(nr/60,100)

     !----------------------------------------------------
     do i=1,nr        !Loop over all measured reflections
         if(mod(i,i_refout) == 0) write(unit=*,fmt="(a,i10)") "=> Reflection:",i
         if(itreat(i) == 0) then   !If not yet treated do the following
           absent=.false.
           h1(:)=h(:,i)

           if(prop) then   !Propagation vector is given
            ivp=0
            hkl(:) = nint( h1 )
            h3(:)=hkl(:)-h1(:)
             !Verify first if the reflection is fundamental or is a satellite
             if(Zbelong(h3) .and. grp_espacial%NumLat == 0) then
               absent=hkl_absent(h1,grp_espacial)    !Fundamental reflections
               ivp=0
             else
               do k=1, Gk%nk
                 h3(:)=  h1-Gk%stark(:,k)
                 if(Zbelong(h3)) then
                   ivp=k
                   if(Gk%k_equiv_minusk) then
                     hkl(:) = int( h3 )
                   else
                     hkl(:) = nint( h3(:))
                   end if
                   exit
                 end if
               end do
             end if
           else         !No Propagation vector is given
             h1=nint(h1)
             absent=hkl_absent(h1,grp_espacial)
           end if

           if(absent) then  !reject absent reflections
             rej=rej+1
             if(prop .or. hkl_real) then
              write(unit=irej,fmt="(3f8.4,2F12.3,4f8.2,i12)") &
              h1(:),intens(i),sigma(i),twtheta(i), omega(i), chi(i), phi(i), numor(i)
             else
              write(unit=irej,fmt="(3i4,2F12.3,4f8.2,i12)") &
              nint(h1(:)),intens(i),sigma(i),twtheta(i), omega(i), chi(i), phi(i), numor(i)
             end if
             cycle
           end if
           nin=nin+1    !update the numer of independent reflections
           itreat(i)=i  !Make this reflection treated
           !  suma=intens(i)
           sig =1.0/sigma(i)**2
           ini(nin)=i   !put pointers for initial and final equivalent reflections
           fin(nin)=i
           nequv(nin)=1 !One reflection for the moment equivalent to itself
           if(prop) then
             ivk(nin)=ivp
             hkls(:,nin)=hkl(:)
             write(unit=iou,fmt="(i8,3f8.4,a,2f12.3,a,3i4,i5,i12,f10.4)")&
                     i,h1,"                  ",intens(i),sigma(i),"   ",hkl,ivp,numor(i),twtheta(i)
           else if(hkl_real) then
             write(unit=iou,fmt="(i8,3f8.4,a,2f12.3,i12,f10.4,a)") &
                    i,h1,"                  ",intens(i),sigma(i),numor(i),twtheta(i),trim(warn_mess(warn(i)))
           else
             write(unit=iou,fmt="(i8,3i4,a,2f12.3,i12,f10.4)") &
                    i,nint(h1),"                  ",intens(i),sigma(i),numor(i),twtheta(i)
           end if
             !-----------------------------------------------------------
             do j=i+1,nr       !look for equivalent reflections to the current (i) in the list
               if(abs(twtheta(i)-twtheta(j)) > 0.2) exit
                if(hkl_type == 7 .and. abs(Lambda_laue(i)-Lambda_laue(j)) > 0.1) cycle
                h2=h(:,j)
                if(prop) then
                  iv=0
                  hkl(:) = nint( h2 )
                   do k=1, Gk%nk
                     h3(:)=  h2-Gk%stark(:,k)
                     if(Zbelong(h3)) then
                      iv=k
                      hkl(:) = nint( h3(:))
                      exit
                     end if
                   end do
                  if(iv /= ivp) cycle

                  if(iv == 0) then

                     if(hkl_equiv(h1,h2,grp_espacial,Friedel)) then
                      itreat(j) = i
                      nequv(nin)=nequv(nin)+1
                      sig=sig + 1.0/sigma(j)**2
                      fin(nin)=j
                      write(unit=iou,fmt="(i8,3f8.4,a,2f12.3,a,3i4,i5,i12,f10.4)") &
                            j,h2,"                  ",intens(j),sigma(j),"   ",hkl,iv,numor(j),twtheta(j)
                     end if

                  else

                     if(hk_equiv(h1,h2,Gk,Friedel)) then
                      itreat(j) = i
                      nequv(nin)=nequv(nin)+1
                      sig=sig + 1.0/sigma(j)**2
                      fin(nin)=j
                      write(unit=iou,fmt="(i8,3f8.4,a,2f12.3,a,3i4,i5,i12,f10.4)") &
                            j,h2,"                  ",intens(j),sigma(j),"   ",hkl,iv,numor(j),twtheta(j)
                     end if

                  end if

                else if (hkl_real)  then
                  if(hkl_equiv(h1,h2,grp_espacial,Friedel)) then
                   itreat(j) = i
                   nequv(nin)=nequv(nin)+1
                   sig=sig + 1.0/sigma(j)**2
                   fin(nin)=j
                   write(unit=iou,fmt="(i8,3f8.4,a,2f12.3,i12,f10.4)") &
                         j,h2,"                  ",intens(j),sigma(j),numor(j),twtheta(j)
                  end if
                else
                  if(hkl_equiv(h1,h2,grp_espacial,Friedel)) then ! if h1 eqv h2
                   itreat(j) = i                                 ! add h2 to the list equivalent to i
                   nequv(nin)=nequv(nin)+1                       ! update the number of equivalents
                   sig=sig + 1.0/sigma(j)**2
                   fin(nin)=j
                   write(unit=iou,fmt="(i8,3i4,a,2f12.3,i12,f10.4)")  &
                         j,nint(h2),"                  ",intens(j),sigma(j),numor(j),twtheta(j)
                  end if
                end if


             end do

             ns=0
             do j=ini(nin),fin(nin)
               if(itreat(j) == i) then
                 ns=ns+1
                 weight(ns)=(1.0/sigma(j)**2)/sig
               end if
             end do

             suma=0.0
             ns=0
             do j=ini(nin),fin(nin)
               if(itreat(j) == i) then
                 ns=ns+1
                 suma=suma+weight(ns)*intens(j)
               end if
             end do


            intav(nin)=suma

             suma=0.0
             ns=0
             do j=ini(nin),fin(nin)
               if(itreat(j) == i) then
                 ns=ns+1
                 delt= intav(nin)-intens(j)
                 if(abs(delt)/intav(nin) > warning) warn(nin)=1
                 suma=suma+weight(ns)*delt*delt
               end if
             end do

             sigmav(nin)=sqrt(suma)

             ! Average sigma using propagation error formula:
             !  I = Sum(v)/n => var(I)= 1/n^2 Sum(var(v)) => s(I)= sqrt(Sum(var(v))/n
             sigstat(nin)=0.0
             nn=0
             sigg=0.0
             do j=ini(nin),fin(nin)
               if(itreat(j) == i) then
                 nn=nn+1
                 sigg=sigg+sigma(j)
                 sigstat(nin)=sigstat(nin)+sigma(j)*sigma(j)
               end if
             end do
             sigstat(nin) = sqrt(sigstat(nin))/real(nn)
             !
             sigg=sigg/real(nn)
             if(sigmav(nin) < sigg) sigmav(nin) = sigg
            ! Use statistical errors instead of experimental variance
            if(statistics) sigmav(nin)=sigstat(nin)

            total=total+sigmav(nin)

           if(prop .or. hkl_real ) then
            write(unit=iou,fmt="(a,3f8.4,3i7,2f12.3,a/)")"   =>", h1,nequv(nin),ini(nin),fin(nin),intav(nin),sigmav(nin),&
                                                         trim(warn_mess(warn(nin)))
           else
           !hkl(:) = nint(h1(:))
           !ha=asu_hkl(hkl,grp_espacial)
            write(unit=iou,fmt="(a,3i4,i4,2i7,2f12.3,a/)")"   =>   ",nint(h1),nequv(nin),ini(nin),fin(nin),intav(nin),sigmav(nin),&
                                                          trim(warn_mess(warn(nin)))
           end if
         end if !itreat
     end do
     !
     ! Second loop over reflections to calculate R-int
     !
     ns=0
     suma =0.0
     suman=0.0
     sumaw =0.0
     sumanw=0.0
     do i=1,nin
       k=ini(i)
       if(nequv(i) < 2 ) cycle
       sig=0.0
       nn=0
       do j=ini(i),fin(i)
         if(itreat(j) == k) then
           nn=nn+1
           sig=sig+1.0/sigma(j)**2
         end if
       end do
       sig=1.0/sig
       do j=ini(i),fin(i)
         if(itreat(j) == k) then
           ns=ns+1
           suma=suma+abs(intav(i)-intens(j))
           suman=suman+intens(j)
           sumaw=sumaw+ sig*((intav(i)-intens(j))**2)/sigma(j)**2
           sumanw=sumanw+sig*(intens(j)/sigma(j))**2
         end if
       end do
     end do
     Rint = 100.0*suma/max(1.0,suman)
     Rwint= 100.0*sqrt(sumaw/max(1.0,sumanw))
     aver_sig= total/real(nin)
     aver_int=suman/real(nin)
     nequiv=ns
     !
     !  Writing the list of averaged reflections
     !

     i=len_trim(forma)
     forma=forma(1:i-1)//",a)"

     if(powder) then

       write(unit=ihkl,fmt="(a)") title
       write(unit=ihkl,fmt="(a)") "Intensities to be read from FullProf JBT=-3, IRF=2"
       do i=1,nin
        j=ini(i)
        hkl(:)=  Get_Hequiv_Asu(nint(h(:,j)),grp_espacial)
        k=HKL_MULT(hkl,grp_espacial,Friedel)
        sigg=sqrt(intav(i))
        write(unit=ihkl,fmt="(3i4,i6,2F14.4)") hkl(:),k,sigg,0.5*sigmav(i)/sigg
       end do

     else  !Single X-tals

      if(prop) then

       if(hkl_type /= 7) then
         write(unit=ihkl,fmt="(a)") title
         write(unit=ihkl,fmt="(a)") "(3i4,i5,2F14.4,i5,4f8.2)"
         if(hkl_type /= 8) then
           write(unit=ihkl,fmt="(f9.5,a)") wavel,"   0   0"
         else
           write(unit=ihkl,fmt="(f9.5,a)") wavel,"   2   0"
         end if
       end if

       if(domain) then
          if(.not. Gk%k_equiv_minusk) then
            nk= 2
            h1=Gk%stark(:,1)
            h2=-Gk%stark(:,1)
            if(hkl_type /= 7) then
              write(unit=ihkl,fmt="(i6,a)") nk, "  ! Number of Propagation vectors (Domain averaged: only k,-k)"
              write(unit=ihkl,fmt="(i4,3f10.4)") 1, h1
              write(unit=ihkl,fmt="(i4,3f10.4)") 2, h2
            end if
            nk=Gk%nk/2
          else
            h1=Gk%stark(:,1)
            write(unit=ihkl,fmt="(i6,a)") 1, "  ! Number of Propagation vectors (Domain averaged for all k in the star)"
            write(unit=ihkl,fmt="(i4,3f10.4)") 1, h1
            nk=Gk%nk
          end if


          do i=1,nin
            if(ivk(i) > nk) then  !belongs to -k (this not happens if k eqv -k)
              ivk(i)=2
            else if(ivk(i) > 0) then   !belongs to  k
              ivk(i)=1
            end if
          end do

          itreat(:) = 0
          ival=0
          do i=1,nin
           k=ini(i)
           if(itreat(i) == 0) then
            ns = 1
            suma = intav(i)
            sig  = sigmav(i)
            suman=0.0
             do j=i+1,nin
                 if(abs(twtheta(k)-twtheta(ini(j))) > 0.4) exit
                 if( any(hkls(:,i) /= hkls(:,j)) ) cycle
                 if( ivk(i) /= ivk(j) ) cycle
                 ns=ns+1
                 suman=suman + abs(intav(i)-intav(j))
                 suma=suma + intav(j)
                 sig = sig + sigmav(j)
                 itreat(j)=j
             end do
             itreat(i)=i
             suma=real(nk)*suma/real(ns)
             suman=real(nk)*suman/real(ns)
             suman=100.0*suman/suma
             sig=real(nk)*sig/real(ns)
             ival=ival+1
             if(hkl_type /= 7) then
               write(unit=ihkl,fmt="(3i4,i5,2f14.4,i5,4f8.2,a,f8.2,i5,a)")    &
               hkls(:,i),ivk(i), suma, sig,1, twtheta(k), omega(k), chi(k), phi(k),"     ", suman, ns,trim(warn_mess(warn(k)))
             else
               write(unit=ihkl,fmt=forma)    &
               hkls(:,i),ivk(i), suma, sig,icod(k), Lambda_Laue(k), twotet(k),absorpt(k),tbar(k),trim(warn_mess(warn(k)))
             end if
           end if !itreat
          end do
          write(unit=*,fmt="(a,i6)") " => Number of domain-averaged reflections    : ",ival

       else
          write(unit=ihkl,fmt="(i6,a)") Gk%nk, "  ! Number of Propagation vectors (star_k)"
          do i=1, Gk%nk
            write(unit=ihkl,fmt="(i4,3f10.4)") i, Gk%stark(:,i)
          end do
          do i=1,nin
            j=ini(i)
            if(hkl_type /= 7) then
              write(unit=ihkl,fmt="(3i4,i5,2F14.4,i5,4f8.2,a)") &
              hkls(:,i),ivk(i), intav(i),sigmav(i),1, twtheta(j), omega(j), chi(j), phi(j),trim(warn_mess(warn(i)))
            else
               write(unit=ihkl,fmt=forma)    &
               hkls(:,i),ivk(i), intav(i),sigmav(i),icod(j), Lambda_Laue(j), twotet(j),absorpt(j),tbar(j),trim(warn_mess(warn(i)))
            end if
          end do

       end if

      else  !no propagation vectors

       if(hkl_type /= 7) then
         write(unit=ihkl,fmt="(a)") title
         if(hkl_type == 8) then
           write(unit=ihkl,fmt="(a)") "(3i4,2f10.6,i5,f8.5,3f8.2,a)"
         else
           write(unit=ihkl,fmt="(a)") "(3i4,2F14.4,i5,4f8.2)"
         end if
       end if

       if(twinned) then

         if(hkl_type /= 7) write(unit=ihkl,fmt="(f9.5,a)") wavel,"   0   1"
         write(unit=*,fmt="(/,a)")   " => Indices have been transformed according to TWIN commands "
         write(unit=iou,fmt="(/,a)") " => Indices have been transformed according to TWIN commands "
         drej=0
         int_rej=0.0
         do i=1,nin
           j=ini(i)
           angles(1)= twtheta(j)
           angles(2)= omega(j)
           angles(3)= chi(j)
           angles(4)=  phi(j)
           call get_domain_contrib(h(:,j),hkln,contr,angles,twSpG)
           twin_acc=.false.
           Lmin=0
           do L=1,nmat
            if(contr(L) == 1) then  !Handles the possibility that the initial reflection has not
              Lmin=L                !integer indices
              twin_acc=.true.
              exit
            end if
           end do
           do L=nmat,Lmin+1,-1
             if(contr(L) == 1) then
                write(unit=ihkl,fmt="(3i4,2F14.4,i5)") nint(hkln(:,L)),-1.0,0.0,L
                twin_acc=.true.
             end if
           end do
           if(.not. twin_acc .or. Lmin == 0) then
              if(drej == 0) write(unit=irej,fmt="(/,a,/)") " => LIST OF REJECTED REFLECTIONS FOR DOMAIN CONTRIBUTION:"
              int_rej=int_rej+ intav(i)
              drej=drej+1
              write(unit=irej,fmt="(/,a,3f6.2,2f10.2,a)") &
              " => Reflection:", h(:,j), intav(i),sigmav(i), " has been rejected for the present TWIN LAW"
              do L=1,nmat
               if(contr(L) == 0) then
                write(unit=irej,fmt="(a,i3,a,3f6.2)")   &
                "                Domain:",L,"    -> ",hkln(:,L)
               else
                write(unit=irej,fmt="(a,i3,a,3f6.2,a)") &
                "                Domain:",L,"    -> ",hkln(:,L)," Forbidden in "//trim(twin_SpG)
               end if
              end do
           end if
           if(Lmin > 0) write(unit=ihkl,fmt="(3i4,2f14.4,i5,4f8.2,a)") &
                        nint(hkln(:,Lmin)),intav(i),sigmav(i),Lmin, angles(:),trim(warn_mess(warn(i)))
         end do
          if(drej > 0) int_rej=int_rej/real(drej)

       else

         if(hkl_type /= 7) then
           if(hkl_type /= 8) then
             write(unit=ihkl,fmt="(f9.5,a)") wavel,"   0   0"
           else                            !23456789    2       0      0.9400    0.9400    0      0       0
             write(unit=ihkl,fmt="(a)")   "! Lambda   itypd    ipow    Polarp    Polarm  UB_mat  Ext  read_strf"
             write(unit=ihkl,fmt="(f9.5,a)") wavel,"    2       0      0.9400    0.9400    0      0       0"
           end if
         end if

         do i=1,nin
           j=ini(i)
           hkl(:)=  nint(h(:,j))
           if(hkl_type /= 7) then
             if(hkl_type == 8) then
               !                 gamma    omega    nu
               call z1frnb(wavel,phi(j), omega(j),chi(j),h1)
               q2=1.0 - h1(3)*h1(3)/dot_product(h1,h1)
               write(unit=ihkl,fmt="(3i4,2f10.6,i5,f8.5,3f8.2,a)") hkl(:),intav(i),sigmav(i),1, q2, 0.0, &
                                                              0.0, 0.0,trim(warn_mess(warn(i)))
             else
               hkl=Get_Hequiv_Asu(hkl,grp_espacial)
               write(unit=ihkl,fmt="(3i4,2f14.4,i5,4f8.2,a)") hkl(:),intav(i),sigmav(i),1, twtheta(j), omega(j), &
                                                              chi(j), phi(j),trim(warn_mess(warn(i)))
             end if
           else
               write(unit=ihkl,fmt=forma)    &
               hkl(:), intav(i),sigmav(i),icod(j), Lambda_Laue(j), twotet(j),absorpt(j),tbar(j),trim(warn_mess(warn(i)))
           end if
         end do

       end if

      end if

     end if
     write(unit=iou,fmt="(/,a,i10)")" => Number of reflections read               : ", nr
     write(unit=iou,fmt="(a,i10)")  " => Number of valid independent   reflections: ", nin
     write(unit=iou,fmt="(a,i10)")  " => Number of obs. with equival.  reflections: ", ns
     write(unit=iou,fmt="(a,i10)")  " => Number of rejected (absences) reflections: ", rej
     write(unit=iou,fmt="(a,f10.2)")" => R-internal for equivalent reflections (%): ", Rint
     write(unit=iou,fmt="(a,f10.2)")" => R-weighted for equivalent reflections (%): ", Rwint
     write(unit=iou,fmt="(a,f10.2)")" => Average Intensity of reflections         : ", aver_int
     write(unit=iou,fmt="(a,f10.2)")" => Average sigma for equivalent reflections : ", aver_sig


     write(unit=*,fmt="(/,a,i10)")" => Number of reflections read               : ", nr
     write(unit=*,fmt="(a,i10)")  " => Number of valid independent   reflections: ", nin
     write(unit=*,fmt="(a,i10)")  " => Number of obs. with equival.  reflections: ", nequiv
     write(unit=*,fmt="(a,i10)")  " => Number of rejected (absences) reflections: ", rej
     write(unit=*,fmt="(a,f10.2)")" => R-internal for equivalent reflections (%): ", Rint
     write(unit=*,fmt="(a,f10.2)")" => R-weighted for equivalent reflections (%): ", Rwint
     write(unit=*,fmt="(a,f10.2)")" => Average Intensity of reflections         : ", aver_int
     write(unit=*,fmt="(a,f10.2)")" => Average sigma for equivalent reflections : ", aver_sig
     if(twinned) then
        write(unit=iou,fmt="(a,i10)")    " => Number of domain rejected reflections    : ", drej
        write(unit=*  ,fmt="(a,i10)")    " => Number of domain rejected reflections    : ", drej
        write(unit=iou,fmt="(a,f10.2)")  " => Average intensity of domain rejected ref.: ", int_rej
        write(unit=*,  fmt="(a,f10.2)")  " => Average intensity of domain rejected ref.: ", int_rej
     end if
   end if !hkl_type == 10

   write(unit=*,fmt="(a)")     " => Program finished O.K.!, look in output files!"
   write(unit=*,fmt="(a,a)")   "           Output  file: ", trim(fileout)//".out"
   write(unit=*,fmt="(a,a)")   "           Reflex  file: ", trim(fileout)//".int"
   write(unit=*,fmt="(a,a)")   "           Reject  file: ", trim(fileout)//".rej"
   write(unit=*,fmt="(a)")   " "
   write(unit=*,fmt="(a)")   "  => Press <cr> to finish ...."
   read(unit=*,fmt="(a)") ans
   stop
 End Program DataRed
