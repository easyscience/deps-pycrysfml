!!----
SubModule (CFML_DiffPatt) DiffP_ReadPatterns

  implicit none

   Contains

   !!--++
   !!--++ READ_PATTERN_MULT
   !!--++
   !!--++    Read patterns from a Filename
   !!--++
   !!--++ 30/04/2019
   !!
   Module Subroutine Read_Pattern_Mult(Filename, Patts, NPats, mode)
      !---- Arguments ----!
      character(len=*),                   intent (in)      :: Filename        ! Path + Name of File containing Patterns
      class(DiffPat_Type), dimension(:),  intent (out)     :: Patts           ! Pattern objects
      integer,                            intent (in out)  :: NPats           ! In: Number of Patterns to read, Out: Number of Patterns readed
      character(len=*), optional,         intent (in)      :: Mode            ! Mode: ISIS, GSAS, XYSIGMA


      !> Init
      call clear_error()

      !> Mode option
      if (present(mode)) then
         select case (u_case(mode))
             case ("XYSIGMA")
                !call  Read_Pattern_xysigma(trim(filename),Patts,npat)

             case ("ISIS")
                call Read_Pattern_isis_m(trim(filename),Patts,NPats)
                !dif_pat%diff_kind = "neutrons_tof"
                !dif_pat%scat_var =  "TOF"
                !dif_pat%xax_text =  "TOF (micro-seconds)"
                !dif_pat%yax_text =  "Intensity (arb. units)"
                !dif_pat%instr  = " 14  - "//mode

             case ("GSAS")
                !call Read_Pattern_gsas_m(dif_pat,npat)      ! GSAS file

             case default
                Err_CFML%IErr=1
                Err_CFML%Msg="Read_Pattern_Mult@DIFFPATT: Invalid Mode!"
         end select

      else

      end if

   End Subroutine Read_Pattern_Mult

   Module Function Load_Pattern(Filename, Mode, Sig, Header) Result(Pat)
      ! This function has been added to make it easy the access to read_pattern_one
      ! from Python
      character(len=*),                    intent (in)      :: filename ! Full path of the file
      character(len=*), target, optional,  intent (in)      :: mode     ! Mode: CIF | DMC | D1A | D1AOLD | D1B | D2B | D2BOLD | D20 | GSAS | GSASTOF | G41 | G42 | HRPT | PANALYTICAL | NLS |  SOCABIM | TIMEVARIABLE | XYSIGMA | 3T2
      logical,          target, optional,  intent (in)      :: sig      ! If sig is False, pat%sigma will be the standard deviation. Otherwise, it will be the variance.
      character(len=*), target, optional,  intent (out)     :: header   ! Header
      type(DiffPat_E_Type)                                  :: pat      ! Diffraction pattern

      ! Local variables
      character(len=:), pointer :: p_mode => null()
      logical, pointer :: p_sig => null()
      character(len=:), pointer :: p_header => null()

      if (present(mode)) p_mode => mode
      if (present(sig)) p_sig => sig
      if (present(header)) p_header => header

      call read_pattern(filename,pat,p_mode,p_sig,p_header)

   End Function Load_Pattern

   !!--++
   !!--++ READ_PATTERN_ONE
   !!--++
   !!--++    Read one pattern from a Filename. If sig is present the content of Dif_Pat%sigma
   !!--++    is the true sigma not the variance.
   !!--++
   !!--++ 01/05/2019
   !!
   Module Subroutine Read_Pattern_One(Filename, Pat, Mode, Sig, Header)
      !---- Arguments ----!
      character(len=*),            intent (in)      :: Filename       ! Path Name of File
      class(DiffPat_Type),         intent (in out)  :: Pat            ! Pattern object
      character(len=*), optional,  intent (in)      :: mode           ! Mode
      logical,          optional,  intent (in)      :: sig            ! Sigma or Variance
      character(len=*), optional,  intent (out)     :: header         ! Header

      !---- Local Variables ----!
      character(len=6)           :: extdat !extension of panalytical file
      character(len=4)           :: tofn
      character(len=12)          :: typef
      logical                    :: gr
      integer                    :: i

      !> Init
      call clear_error()


      Typef="DEFAULT"
      TofN=" "

      if (present(mode)) then
         Typef=trim(u_case(mode))
         if (Typef(1:7) == "GSASTOF") then
            if (len_trim(mode) > 7) then
               tofn="TOF"//mode(8:8)
               typef="GSASTOF"
            else
               tofn="TOF"
            end if
         end if
      end if

      select case (trim(typef))
         case ("CIF")
            call Read_Pattern_CIF(trim(filename),Pat)
            if (err_CFML%IErr /= 0) return

            select type(Pat)
               type is (DiffPat_E_Type)
                  pat%ct_step = .false.
                  pat%instr  = "  XY  - "//trim(typef)

               type is (DiffPat_G_Type)
                  pat%ct_step = .false.
                  pat%instr  =  "  XY  - "//trim(typef)
                  pat%legend_Y ="Intensity (arb. units)"
            end select

         case ("D1B" , "D20")
            pat%kindrad = "Neutrons_CW"
            pat%scatvar = "2Theta"
            select type(Pat)
               class is (DiffPat_E_Type)
                  call Read_Pattern_D1B_D20(trim(filename), Pat)
                  if (err_CFML%IErr /= 0) return

                  pat%ct_step = .true.
                  pat%instr=trim(Typef)

                  select type(Pat)
                     type is (DiffPat_G_Type)
                        pat%legend_X ="2Theta(degrees)"
                        pat%legend_Y ="Intensity (arb. units)"
                  end select
            end select

         case ("NLS")
            call Read_Pattern_NLS(trim(filename), Pat)
            if (err_CFML%IErr /= 0) return

            pat%kindrad = "XRay_CW"
            pat%scatvar = "2Theta"

            select type(Pat)
               type is (DiffPat_E_Type)
                  pat%ct_step = .true.
                  pat%instr=trim(Typef)

               type is (DiffPat_G_Type)
                  pat%ct_step = .true.
                  pat%instr=trim(Typef)

                  pat%legend_X ="2Theta(degrees)"
                  pat%legend_Y ="Intensity (arb. units)"
            end select

         case ("G41")
            pat%kindrad = "Neutrons_CW"
            pat%scatvar = "2Theta"
            select type (Pat)
               class is (DiffPat_E_Type)
                  call Read_Pattern_G41(trim(filename), Pat)
                  if (err_CFML%IErr /= 0) return

                  pat%ct_step = .true.
                  pat%instr=trim(Typef)

                  select type(pat)
                     type  is (DiffPat_G_Type)
                        pat%legend_X ="2Theta(degrees)"
                        pat%legend_Y ="Intensity (arb. units)"
                  end select
            end select

         case ("D1A","D2B","3T2","G42")
            pat%kindrad = "Neutrons_CW"
            pat%scatvar = "2Theta"
            select type(Pat)
               class is (DiffPat_E_Type)
                  call Read_Pattern_D1A_D2B(trim(filename), Pat)
                  if (err_CFML%IErr /= 0) return

                  pat%ct_step = .true.
                  pat%instr=trim(Typef)

                  select type(pat)
                     type is (DiffPat_G_Type)
                        pat%legend_X ="2Theta(degrees)"
                        pat%legend_Y ="Intensity (arb. units)"
                  end select
            end select

         case ("D1AOLD", "D2BOLD","OLDD1A", "OLDD2B")
            pat%kindrad = "Neutrons_CW"
            pat%scatvar = "2Theta"
            select type(Pat)
               class is (DiffPat_E_Type)
                  call Read_Pattern_D1A_D2B_OLD(trim(filename), Pat)
                  if (err_CFML%IErr /= 0) return

                  pat%ct_step = .true.
                  pat%instr=trim(Typef)

                  select type(pat)
                     type is (DiffPat_G_Type)
                        pat%legend_X ="2Theta(degrees)"
                        pat%legend_Y ="Intensity (arb. units)"
                  end select
            end select

         case ("DMC","HRPT")
            pat%kindrad = "Neutrons_CW"
            pat%scatvar = "2Theta"
            select type(Pat)
               class is (DiffPat_E_Type)
                  call Read_Pattern_DMC(trim(filename), Pat)
                  if (err_CFML%IErr /= 0) return

                  pat%ct_step = .true.
                  pat%instr=trim(Typef)

                  select type(pat)
                     type is (DiffPat_G_Type)
                        pat%legend_X ="2Theta(degrees)"
                        pat%legend_Y ="Intensity (arb. units)"
                  end select
            end select

         case ("SOCABIM")
             call Read_Pattern_SOCABIM(trim(filename), Pat)
            if (err_CFML%IErr /=0) return

            pat%kindrad = "XRay_CW"
            pat%scatvar = "2Theta"
            select type(Pat)
               type is (DiffPat_E_Type)
                  pat%ct_step = .true.
                  pat%instr=trim(Typef)

               type is (DiffPat_G_Type)
                  pat%ct_step = .true.
                  pat%instr=trim(Typef)

                  pat%legend_X ="2Theta(degrees)"
                  pat%legend_Y ="Intensity (arb. units)"
            end select

         case ("XYSIGMA")
            i=index(filename,".",back=.true.)
            if (i > 0) then
               extdat=u_case(filename(i:))
            else
               extdat="---"
            end if

            select case (trim(extdat))

               case (".GR")
                  if (present(header)) then
                     call  Read_Pattern_xysigma(filename, Pat, GR, Header)
                  else
                     call  Read_Pattern_xysigma(filename, Pat, GR)
                  end if

               case ("---")
                  if (present(header)) then
                     call  Read_Pattern_xysigma(filename, Pat, Header=Header)
                  else
                     call  Read_Pattern_xysigma(filename, Pat)
                  end if

               case default

                  if (present(header)) then
                     call  Read_Pattern_xysigma(filename, Pat, Header=Header)
                  else
                     call  Read_Pattern_xysigma(filename, Pat)
                  end if

            end select

            if (err_CFML%IErr /= 0) return

            pat%kindrad = "Unknown"
            if (pat%x(pat%npts) > 180) then
               pat%scatvar =  "TOF"
            else
               pat%scatvar =  "2Theta"
            end if
            select type(Pat)
               type is (DiffPat_E_Type)
                  pat%instr=trim(Typef)

               type is (DiffPat_G_Type)
                  pat%instr=trim(Typef)

                  pat%legend_Y ="Intensity (arb. units)"
                  if (index(pat%scatvar,"TOF") > 0) then
                     pat%legend_X ="TOF(micro-seconds)"
                  else
                     pat%legend_X ="2Theta(degrees)"
                  end if
            end select

         case ("GSAS")
            call Read_Pattern_GSAS(trim(filename), Pat)
            if (err_CFML%IErr /= 0) return

            pat%kindrad = "Constant_Wavelength"
            pat%scatvar = "2Theta"
            select type(Pat)
               type is (DiffPat_E_Type)
                  pat%instr=trim(Typef)

               type is (DiffPat_G_Type)
                  pat%instr=trim(Typef)

                  pat%legend_X ="2Theta(degrees)"
                  pat%legend_Y ="Intensity (arb. units)"
            end select

         case ("GSASTOF")
            call Read_Pattern_GSAS(trim(filename), Pat, TOFN)
            if (err_CFML%IErr /= 0) return

            pat%kindrad = "Neutrons_TOF"
            pat%scatvar = "TOF"
            select type(Pat)
               type is (DiffPat_E_Type)
                  pat%instr=trim(Typef)

               type is (DiffPat_G_Type)
                  pat%instr=trim(Typef)

                  pat%legend_X ="TOF(micro-seconds)"
                  pat%legend_Y ="Intensity (arb. units)"
            end select

         case ("PANALYTICAL")
            i=index(filename,".",back=.true.)
            extdat=u_case(filename(i:))
            select case (trim(extdat))
               case (".CSV")
                  call Read_Pattern_PANalytical_CSV(trim(filename), Pat)

               case (".UDF")
                  call Read_Pattern_PANalytical_UDF(trim(filename), Pat)

               case (".JCP")
                  call Read_Pattern_PANalytical_JCP(trim(filename), Pat)

               case(".XRDML")
                  call Read_Pattern_PANalytical_XRDML(trim(filename), Pat)
            end select
            if (err_CFML%IErr /= 0) return

            pat%kindrad = "XRays_CW"
            pat%scatvar = "2Theta"
            select type(Pat)
               type is (DiffPat_E_Type)
                  pat%instr=trim(Typef)

               type is (DiffPat_G_Type)
                  pat%instr=trim(Typef)

                  pat%legend_X ="2Theta(degrees)"
                  pat%legend_Y ="Intensity (arb. units)"
            end select


         case ("TIMEVARIABLE")
            call Read_Pattern_TimeVar(trim(filename), Pat)
            if (err_CFML%IErr /= 0) return

            pat%kindrad = "XRays_CW"
            pat%scatvar = "2Theta"
            select type(Pat)
               type is (DiffPat_E_Type)
                  pat%instr=trim(Typef)

               type is (DiffPat_G_Type)
                  pat%instr=trim(Typef)

                  pat%legend_X ="2Theta(degrees)"
                  pat%legend_Y ="Intensity (arb. units)"
            end select

         case default
            i=index(filename,".",back=.true.)
            extdat=u_case(filename(i:))
            call Read_Pattern_FREE(trim(filename), Pat, extdat)
            if (err_CFML%IErr /= 0) return

            pat%kindrad = "unknown"
            if (pat%x(pat%npts) > 180.0 ) then
               pat%scatvar =  "TOF"
            else
               pat%scatvar =  "2Theta"
            end if
            select type (Pat)
               type is (DiffPat_E_Type)
                  pat%instr   = "Free format"
                  pat%ct_step=.true.

               type is (DiffPat_G_Type)
                  pat%instr   = "Free format"
                  pat%ct_step=.true.
                  pat%Legend_Y="Intensity (arb. units)"

                  if (index(pat%scatvar,'TOF')> 0) then
                     pat%Legend_X="TOF(micro-seconds)"
                  else
                     pat%Legend_X="2Theta(degrees)"
                  end if
            end select

      end select

      !> CHECK PLEASE
      if (present(sig)) then
         if (.not. sig) pat%sigma=sqrt(pat%sigma)
      end if
   End Subroutine Read_Pattern_One

End SubModule DiffP_ReadPatterns