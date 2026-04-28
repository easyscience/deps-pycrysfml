Submodule (CFML_KeyCodes) KeyCod_Atm
   implicit none

   Contains

   !!--++
   !!--++ Subroutine Vary_XYZ_Atm
   !!--++
   !!--++    Vary Coordinates Codes
   !!--++
   !!--++ Update: April - 2022
   !!
   Module Subroutine Vary_XYZ_Atm(A, NCode, Ind)
      !---- Arguments ----!
      class(Atm_Type), intent(in out) :: A
      integer,         intent(in out) :: NCode
      integer,         intent(in)     :: Ind    ! 1:X, 2:Y, 3:Z, 0:XYZ


      !---- Local Variables ----!
      integer :: i

      select type (A)
         type is (Atm_Ref_Type)
            select case (Ind)
               case (1:3)
                  if (A%l_x(Ind) ==0) then
                     NCode=NCode+1
                     A%l_x(Ind)=NCode
                     A%m_x(Ind)=1.0_cp
                  end if

               case (0)
                  do i=1,3
                     if (A%l_x(i) ==0) then
                        A%m_x(i)=1.0_cp
                        NCode=NCode+1
                        A%l_x(i)=NCode
                     end if
                  end do

            end select  ! Ind

         type is (ModAtm_Ref_Type)
            select case (Ind)
               case (1:3)
                  if (A%l_x(Ind) ==0) then
                     A%m_x(Ind)=1.0_cp
                     NCode=NCode+1
                     A%l_x(Ind)=NCode
                  end if

               case (0)
                  do i=1,3
                     if (A%l_x(i) ==0) then
                        A%m_x(i)=1.0_cp
                        NCode=NCode+1
                        A%L_x(i)=NCode
                     end if
                  end do

            end select  ! Ind
      end select ! A

   End Subroutine Vary_XYZ_Atm

   !!--++
   !!--++ Subroutine Vary_OCC_Atm
   !!--++
   !!--++    Vary Occupancy factor Code
   !!--++
   !!--++ Update: April - 2022
   !!
   Module Subroutine Vary_OCC_Atm(A, NCode)
      !---- Arguments ----!
      class(Atm_Type),           intent(in out) :: A
      integer,                   intent(in out) :: NCode

      !---- Local Variables ----!

      select type (A)
         type is (Atm_Ref_Type)
            if (A%l_occ ==0) then
               A%m_occ=1.0_cp
               ncode=ncode+1
               A%l_occ=ncode
            end if

         type is (ModAtm_Ref_Type)
            if (A%l_occ ==0) then
               A%m_occ=1.0_cp
               ncode=ncode+1
               A%l_occ=ncode
            end if
      end select

   End Subroutine Vary_OCC_Atm

   !!--++
   !!--++ Subroutine Vary_U_Atm
   !!--++
   !!--++    Vary Thermal factors  Codes
   !!--++
   !!--++ Update: April - 2022
   !!
   Module Subroutine Vary_U_Atm(A, NCode, Ind)
      !---- Arguments ----!
      class(Atm_Type), intent(in out) :: A
      integer,         intent(in out) :: NCode
      integer,         intent(in)     :: Ind    ! 0:Uiso, 1-6:Uij, -1:All thermal


      !---- Local Variables ----!
      integer :: i

      select type (A)
         type is (Atm_Ref_Type)
            select case (Ind)
               case (0)
                  if (A%l_U_iso ==0) then
                     A%m_U_iso=1.0_cp
                     NCode=NCode+1
                     A%l_U_iso=NCode
                  end if

               case (1:6)
                  if (A%l_u(Ind) ==0) then
                     A%m_u(Ind)=1.0_cp
                     NCode=NCode+1
                     A%l_u(Ind)= NCode
                  end if

               case (-1)
                  do i=1,6
                     if (A%l_u(i) ==0) then
                        A%m_u(i)=1.0_cp
                        NCode=NCode+1
                        A%l_u(i)= NCode
                     end if
                  end do

            end select  ! Ind

         type is (ModAtm_Ref_Type)
            select case (Ind)
               case (0)
                  if (A%l_U_iso ==0) then
                     A%m_U_iso=1.0_cp
                     NCode=NCode+1
                     A%l_U_iso=NCode

                  end if

               case (1:6)
                  if (A%l_u(Ind) ==0) then
                     A%m_u(Ind)=1.0_cp
                     NCode=NCode+1
                     A%l_U(ind)=NCode

                  end if

               case (-1)
                  do i=1,6
                     if (A%l_u(i) ==0) then
                        A%m_u(i)=1.0_cp
                        NCode=NCode+1
                        A%l_U(i)=NCode
                     end if
                  end do

            end select  ! Ind
      end select ! A

   End Subroutine Vary_U_Atm

End SubModule KeyCod_Atm
