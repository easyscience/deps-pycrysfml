Submodule (CFML_Keywords_Code_Parser) KWC_FillCodes_Gen
   !---- Variables ----!
   implicit none

 Contains
    !!--++
    !!--++ Module Subroutine Fill_RefCodes_Magdom(Keyv,Dire,Na,Nb,Xl,Xu,Xs,Ic,Mag_dom)
    !!--++    integer,                       intent(in)     :: Keyv
    !!--++    character(len=*),              intent(in)     :: Dire
    !!--++    integer,                       intent(in)     :: Na
    !!--++    integer,                       intent(in)     :: Nb
    !!--++    real(kind=cp),                 intent(in)     :: Xl
    !!--++    real(kind=cp),                 intent(in)     :: Xu
    !!--++    real(kind=cp),                 intent(in)     :: Xs
    !!--++    integer,                       intent(in)     :: Ic
    !!--++    type(Magnetic_Domain_type),    intent(in out) :: Mag_dom
    !!--++
    !!--++ Write on Vectors the Information for Magnetic Domains
    !!--++ Created: February - 2012
    !!--++
    Module Subroutine Fill_RefCodes_Magdom(Keyv,Dire,Na,Nb,Xl,Xu,Xs,Ic,Mag_dom)
       !---- Arguments ----!
       integer,                       intent(in)     :: Keyv
       character(len=*),              intent(in)     :: Dire
       integer,                       intent(in)     :: Na
       integer,                       intent(in)     :: Nb
       real(kind=cp),                 intent(in)     :: Xl
       real(kind=cp),                 intent(in)     :: Xu
       real(kind=cp),                 intent(in)     :: Xs
       integer,                       intent(in)     :: Ic
       type(Magnetic_Domain_type),    intent(in out) :: Mag_dom

       !---- Local variables ----!
       integer           :: nc

       call clear_error()
       if (Na <= 0) then
          Err_CFML%Flag=.true.
          Err_CFML%Msg="The number of domains no defined"
          return
       end if

       select case (dire)
          !---- FIX Directive ----!
          case ("fix")

             select case (keyv)
                case (0)
                   Err_CFML%Flag=.true.
                   Err_CFML%Msg="Option not defined"
                   return

                case (4)
                   !---- Magdomain ----!
                      if (Mag_Dom%Lpop(nb,na) /=0) then
                         nc=Mag_Dom%Lpop(nb,na)
                         call Delete_RefCodes(nc,Mag_dom)
                      end if

                case (2:3)
                   Err_CFML%Flag=.true.
                   Err_CFML%Msg="Option not defined"
                   return

                case (5:)
                   Err_CFML%Flag=.true.
                   Err_CFML%Msg="Option not defined"
                   return
             end select

          !---- VARY Directive ----!
          case ("var")

              select case (keyv)
                case (0)
                   Err_CFML%Flag=.true.
                   Err_CFML%Msg="Option not defined"
                   return

                case (4)
                   !---- Magd ----!
                      if (Mag_Dom%Lpop(nb,na) ==0) then
                            np_refi=np_refi+1
                            Mag_Dom%Lpop(nb,na)=np_refi
                            Mag_Dom%Mpop(nb,na)=1.0

                         if (Mag_Dom%Lpop(nb,na) == np_refi) then
                            V_Vec(np_refi)=Mag_Dom%pop(nb,na)
                            V_Name(np_refi)=trim(Mag_dom%lab(nb,na))
                            V_Bounds(1,np_refi)=xl
                            V_Bounds(2,np_refi)=xu
                            V_Bounds(3,np_refi)=xs
                            V_BCon(np_refi)=ic
                            V_list(np_refi)=0
                         else
                            np_refi=np_refi-1
                         end if
                      end if

                case (2:3)
                   Err_CFML%Flag=.true.
                   Err_CFML%Msg="Option not defined"
                   return

                case (5:)
                   Err_CFML%Flag=.true.
                   Err_CFML%Msg="Option not defined"
                   return
              end select
       end select
    End Subroutine Fill_RefCodes_Magdom

    !!----
    !!---- Module Subroutine Fill_RefGCodes(Keyv,Dire,namp,Xl,Xu,Xs,Ic,model,sys,Iphas)
    !!----    integer,                             intent(in)     :: Keyv  !0=> nb as below,
    !!----    character(len=*),                    intent(in)     :: Dire !GVar of GFix
    !!----    character(len=*),                    intent(in)     :: namp !Name of the parameter to be refined or fixed
    !!----    real(kind=cp),                       intent(in)     :: Xl   !Lower bound of parameter
    !!----    real(kind=cp),                       intent(in)     :: Xu   !Upper bound of parameter
    !!----    real(kind=cp),                       intent(in)     :: Xs   !Step of parameter
    !!----    integer,                             intent(in)     :: Ic   !Boundary condition (0:fixed or 1:periodic)
    !!----    type(NonAtomic_Parameter_List_Type), intent(in out) :: model
    !!----    character(len=*), optional,          intent( in)    :: sys
    !!----    integer,          optional,          intent(in)     :: Iphas
    !!----
    !!---- Write on Vectors the Information for Non atomic parameters
    !!----  Keyv=0 -> Provide information on individual parameter for atom na (nb should be given)
    !!----  Keyv=1  BKG -> fix or vary all background parameters
    !!----  Keyv=2  CELL -> fix or vary cell parameters
    !!             For the constraints on cell parameters the optional argument sys should be
    !!----         provided. It contains the crystal system and the (in the case of Monoclinic)
    !!----         the setting "a", "b" or "c" for the twofold axis. For instance the content
    !!----         of Sys may be Sys="Monoclinic c". Between the crystal system and the information
    !!----         about the setting a space must exist.
    !!----         The proper application of the constraints supposes that the cell parameters
    !!----         are contiguous in the list of non-atomic parameters.
    !!----  Keyv=3  UVW -> fix or vary u,v,w parameters
    !!----  Keyv=4  ASIZE -> fix or vary size parameters
    !!----  Keyv=5  ASTRAIN -> fix or vary strain parameters
    !!----  Keyv=6  EXTINCT -> fix or vary extinction parameters
    !!----  Keyv=7  SCALEFS -> fix or vary scale factors
    !!----  Keyv=9  ALL -> fix or vary all parameters
    !!----
    !!----
    !!---- Updated: November 3 - 2013
    !!
    Module Subroutine Fill_RefGCodes(Keyv,Dire,Namp,Xl,Xu,Xs,Ic,model,sys,Iphas)
       integer,                             intent(in)     :: Keyv !0 => nb as below,
       character(len=*),                    intent(in)     :: Dire !GVar of GFix
       character(len=*),                    intent(in)     :: Namp !Name of the parameter to be refined or fixed
       real(kind=cp),                       intent(in)     :: Xl   !Lower bound of parameter
       real(kind=cp),                       intent(in)     :: Xu   !Upper bound of parameter
       real(kind=cp),                       intent(in)     :: Xs   !Step of parameter
       integer,                             intent(in)     :: Ic   !Boundary condition (0:fixed or 1:periodic)
       type(NonAtomic_Parameter_List_Type), intent(in out) :: model
       character(len=*), optional,          intent(in)     :: sys
       integer,          optional,          intent(in)     :: Iphas

       !---- Local variables ----!
       integer                  :: i,k,nc !j,,np_ini
       character(len=len(namp)) :: name_par
       character(len=15)        :: c_system
       character(len=5)         :: info
       character(len=2)         :: phase

       call clear_error()
       if (len_trim(namp) == 0) then
          Err_CFML%Flag=.true.
          Err_CFML%Msg="No name for a parameter to be refined"
          return
       end if
       if(present(Iphas)) then
         write(unit=phase,fmt="(i2.2)") Iphas
       else
         phase="  "
       end if

       select case (trim(dire))
          !---- GFIX Directive ----!
          case ("gfix")

             select case (keyv)
                case (0) !Keyv=0 -> Provide information on individual parameter for type of parameter na (nb should be given)

                   do i=1,model%npar
                      name_par=l_case(model%par(i)%nam)
                      if ( name_par /= l_case(namp)) cycle
                      if ( model%par(i)%lcode /= 0) then
                         nc=model%par(i)%lcode
                         call Delete_RefGCodes(nc,model)
                      end if
                      exit
                   end do

                case (1) ! Keyv=1  BKG -> Fix all background parameters
                   !---- BKG ----!
                   do i=1,model%npar
                      name_par=l_case(model%par(i)%nam)
                      if ( name_par(1:3) /= "bkg") cycle
                      if ( model%par(i)%lcode /= 0) then
                         nc=model%par(i)%lcode
                         call Delete_RefGCodes(nc,model)
                      end if
                      exit
                   end do

                case (2) !  Keyv=2  CELL -> Fix all cell parameters
                   !---- CELL ----!
                   do i=1,model%npar
                      name_par=l_case(model%par(i)%nam)
                      if(index(name_par,"cell") /= 0 ) then
                         if(present(Iphas)) then
                           if(index(name_par,phase) == 0) cycle
                         end if
                         if ( model%par(i)%lcode /= 0) then
                            nc=model%par(i)%lcode
                            call Delete_RefGCodes(nc,model)
                         end if
                      end if
                   end do

                case (3) !  Keyv=3  UVW -> Fix U,V,W parameters
                   !---- UVW ----!
                   do i=1,model%npar
                      name_par=l_case(model%par(i)%nam)
                      if ( trim(name_par) == "up" .or. trim(name_par) == "vp" .or. trim(name_par) == "wp" ) then
                         if ( model%par(i)%lcode /= 0) then
                            nc=model%par(i)%lcode
                            call Delete_RefGCodes(nc,model)
                         end if
                      end if
                   end do

                case (4) !  Keyv=4  ASIZE -> Fix all size parameters
                  !---- ASIZE ----!
                   do i=1,model%npar
                      name_par=l_case(model%par(i)%nam)
                      if ( name_par(1:3) == "siz" .or. name_par(1:4) == "gsiz") then
                         if ( model%par(i)%lcode /= 0) then
                            nc=model%par(i)%lcode
                            call Delete_RefGCodes(nc,model)
                         end if
                      end if
                   end do

                case (5) !  Keyv=5  ASTRAIN -> Fix all strain parameters
                   !---- ASTRAIN ----!
                   do i=1,model%npar
                      name_par=l_case(model%par(i)%nam)
                      if ( name_par(1:3) == "str" .or. name_par(1:4) == "lstr") then
                         if ( model%par(i)%lcode /= 0) then
                            nc=model%par(i)%lcode
                            call Delete_RefGCodes(nc,model)
                         end if
                      end if
                   end do

                case (6)  !  Keyv=6  EXTINCT -> Fix all extinction parameters
                   !---- EXTINCT ----!
                   do i=1,model%npar
                      name_par=l_case(model%par(i)%nam)
                      if ( name_par(1:3) == "ext" .or. name_par(1:5) == "bcext") then
                         if ( model%par(i)%lcode /= 0) then
                            nc=model%par(i)%lcode
                            call Delete_RefGCodes(nc,model)
                         end if
                      end if
                   end do

                case (7)  !  Keyv=7  SCALEFS -> Fix all scale factors

                    !---- SCALEFS ----!
                   do i=1,model%npar
                      name_par=l_case(model%par(i)%nam)
                      if ( name_par(1:2) == "sc" .or. name_par(1:5) == "scale") then
                         if ( model%par(i)%lcode /= 0) then
                            nc=model%par(i)%lcode
                            call Delete_RefGCodes(nc,model)
                         end if
                      end if
                   end do

                case (8)  !  Keyv=8  ALL -> Fix all non atomic parameters
                    !---- ALL ----!
                   do i=1,model%npar
                      if ( model%par(i)%lcode /= 0) then
                         nc=model%par(i)%lcode
                         call Delete_RefGCodes(nc,model)
                      end if
                   end do

                case (9:)
                   Err_CFML%Flag=.true.
                   Err_CFML%Msg="Incompatible information for this Non-atomic parameter "//trim(Namp)
                   return

             end select

          !---- GVARY Directive ----!
          case ("gvar")

             select case (Keyv)
                case (0) !Keyv=0 -> Provide information on individual model parameter
                         !Vary a single model parameter
                   do i=1,model%npar
                      name_par=l_case(model%par(i)%nam)
                      if ( name_par /= l_case(namp)) cycle
                      if ( model%par(i)%lcode == 0) then
                         call update_vect(i)
                         exit
                      end if
                   end do

                case (1) ! Keyv=1  BKG -> Vary all background parameters

                   do i=1,model%npar
                      name_par=l_case(model%par(i)%nam)
                      if ( name_par(1:3) /= "bkg") cycle
                      if ( model%par(i)%lcode == 0) call update_vect(i)
                   end do

                case (2) !  Keyv=2  CELL -> Vary all cell parameters
                   !---- CELL ----!
                   if(present(sys)) then

                      do i=1,model%npar
                         name_par=l_case(model%par(i)%nam)
                         if ( index(name_par,"cell") /= 0) then
                            if(present(Iphas)) then
                              if(index(name_par,phase) == 0) cycle
                            end if
                            k=i
                            exit
                         end if
                      end do
                      i=index(sys," ")
                      c_system=sys(1:i-1)
                      info=sys(i+1:)
                      Select Case(c_system)

                          case("Triclinic")
                             do i=k,k+5
                               call update_vect(i)
                             end do

                          case("Monoclinic")

                             do i=k,k+2
                               call update_vect(i)
                             end do

                            if(index(Info,"b") /= 0) then
                               model%par(k+3)%value=90.0;  model%par(k+5)%value=90.0
                               call update_vect(k+4)

                            else if(index(Info,"c") /= 0) then
                               model%par(k+3)%value=90.0;  model%par(k+4)%value=90.0
                               call update_vect(k+5)

                            else if(index(Info,"a") /= 0) then
                               model%par(k+4)%value=90.0;  model%par(k+5)%value=90.0
                               call update_vect(k+3)

                            end if

                          case("Orthorhombic")
                            model%par(k+3:k+5)%value=90.0
                            do i=k,k+2
                              call update_vect(i)
                            end do

                          case("Tetragonal")
                            model%par(k+1)%value=model%par(k)%value
                            model%par(k+3:k+5)%value=90.0
                            call update_vect(k)
                            call update_vect(k+1,.false.)
                            call update_vect(k+2)

                          case("Trigonal","Hexagonal")
                            model%par(k+1)%value=model%par(k)%value
                            model%par(k+3:k+4)%value=90.0
                            model%par(k+5)%value=120.0
                            call update_vect(k)
                            call update_vect(k+1,.false.)
                            call update_vect(k+2)

                          case("Cubic")
                            model%par(k+3:k+5)%value=90.0
                            model%par(k+1)%value=model%par(k)%value
                            model%par(k+2)%value=model%par(k)%value
                            call update_vect(k)
                            call update_vect(k+1,.false.)
                            call update_vect(k+2,.false.)
                     End Select

                   else

                     do i=1,model%npar
                        name_par=l_case(model%par(i)%nam)
                        if ( index(name_par,"cell") /= 0) then
                            if(present(Iphas)) then
                              if(index(name_par,phase) == 0) cycle
                            end if
                           if ( model%par(i)%lcode == 0) call update_vect(i)
                        end if
                     end do

                   end if

                case (3) !  Keyv=3  UVW -> Vary U,V,W parameters
                   !---- UVW ----!
                   do i=1,model%npar
                      name_par=l_case(model%par(i)%nam)
                      if ( trim(name_par) == "up" .or. trim(name_par) == "vp" .or. trim(name_par) == "wp" ) then
                         if ( model%par(i)%lcode == 0) call update_vect(i)
                      end if
                   end do

                case (4) !  Keyv=4  ASIZE -> Vary all size parameters
                  !---- ASIZE ----!
                   do i=1,model%npar
                      name_par=l_case(model%par(i)%nam)
                      if ( name_par(1:3) == "siz" .or. name_par(1:4) == "gsiz") then
                         if ( model%par(i)%lcode == 0) call update_vect(i)
                      end if
                   end do

                case (5) !  Keyv=5  ASTRAIN -> Vary all strain parameters
                   !---- ASTRAIN ----!
                   do i=1,model%npar
                      name_par=l_case(model%par(i)%nam)
                      if ( name_par(1:3) == "str" .or. name_par(1:4) == "lstr") then
                         if ( model%par(i)%lcode == 0) call update_vect(i)
                      end if
                   end do

                case (6)  !  Keyv=6  EXTINCT -> Vary all extinction parameters
                   !---- EXTINCT ----!
                   do i=1,model%npar
                      name_par=l_case(model%par(i)%nam)
                      if ( name_par(1:3) == "ext") then
                         if ( model%par(i)%lcode == 0) call update_vect(i)
                      end if
                   end do

                case (7)  !  Keyv=7  SCALEFS -> Vary all scale factors
                   !---- SCALEFS ----!
                   do i=1,model%npar
                      name_par=l_case(model%par(i)%nam)
                      if ( name_par(1:2) == "sc" .or. name_par(1:5) == "scale") then
                         if ( model%par(i)%lcode == 0) call update_vect(i)
                      end if
                   end do

                case (8)  !  Keyv=8  ALL -> Vary all non atomic parameters
                  !---- ALL ----!
                   do i=1,model%npar
                      if ( model%par(i)%lcode == 0) call update_vect(i)
                   end do


                case (9:)
                   Err_CFML%Flag=.true.
                   Err_CFML%Msg="Incompatible information for this Non-atomic parameter "//trim(Namp)
                   return
             end select

       end select

       contains

         Subroutine update_vect(n,up_np_refi)  !Internal subroutine to avoid copying the same text
           integer, intent(in):: n             !every time we need to do the same thing!
           logical, optional,intent(in):: up_np_refi
           logical :: local_up

           local_up=.true.
           if(present(up_np_refi)) local_up=up_np_refi
           if(local_up) np_refi=np_refi+1        !Here V_Vec_std should not be updated
           model%par(n)%lcode=np_refi
           model%par(n)%multip=1.0
           if(local_up) then
              V_Vec(np_refi)=model%par(n)%value
              V_Name(np_refi)=model%par(n)%nam
              V_Bounds(1,np_refi)=xl
              V_Bounds(2,np_refi)=xu
              V_Bounds(3,np_refi)=xs
              V_BCon(np_refi)=ic
              V_list(np_refi)=n
           end if
         End Subroutine update_vect

    End Subroutine Fill_RefGCodes

End Submodule KWC_FillCodes_Gen
