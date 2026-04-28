!!----
!!----
!!----
SubModule (CFML_Reflections) Refl_Write_List
   implicit none
   Contains

   !!----
   !!---- WRITE_INFO_REFLIST
   !!----    Write information about the Reflection List
   !!----
   !!---- 24/06/2019
   !!
   Module Subroutine Write_Info_RefList(Reflex, Iunit, Mode,kinfo)
      !---- Arguments ----!
      class(RefList_Type),             intent(in) :: Reflex
      integer,               optional, intent(in) :: Iunit
      character(len=*),      optional, intent(in) :: Mode
      type(kvect_info_type), optional, intent(in) :: kinfo

      !---- Local variables ----!
      integer                              :: i,j,n,lun,d,ir
      character(len=:), allocatable        :: forma
      character(len=*), dimension(0:2), parameter :: charRef = ["Nuclear ","Magnetic"," Nuc+Mag"]
      real(kind=cp), dimension(3,reflex%nref) :: hr
      real(kind=cp), dimension(3)             :: hext,hint

      !> Init
      lun=6
      if (present(iunit)) lun=iunit
      if (present(mode)) then
         select case (l_case(mode(1:3)))
            case("neu")
               ir = 2
            case("ele")
               ir = 3
            case default
               ir = 1
         end select
      else
         ir=1
      end if

      if (reflex%nref <=0) then
         write(unit=lun,fmt="(/,a)")   "There is no reflection in the List!"
         return
      else
         write(unit=lun,fmt="(/,a,i7)")   " => The total number of reflections in the List is: ", reflex%nref
      end if

      n=reflex%nref
      d=size(reflex%Ref(1)%h)
      if(present(kinfo)) then
        do i=1,n
          hext=reflex%Ref(i)%h(1:3)
          hint=0.0
          do j=1,kinfo%nk
            hint=hint + reflex%Ref(i)%h(j+3)*kinfo%kv(:,j)
          end do
          !write(*,"(6i4,i10)") reflex%Ref(i)%h, reflex%Ref(i)%pcoeff
          !write(*,"(6f8.4)") hext,hint
          hr(:,i)=hext+hint
        end do
      end if


      Select Type (r => Reflex%Ref)
         type is (Refl_Type)
            if (present(mode)) then
               select case (l_case(mode(1:3)))
                  case("neu")
                     write(unit=lun,fmt="(/,/,a)") "    LIST OF REFLECTIONS (NEUTRONS)"
                     write(unit=lun,fmt="(a)")     "    =============================="
                  case("ele")
                     write(unit=lun,fmt="(/,/,a)") "    LIST OF REFLECTIONS (ELECTRONS)"
                     write(unit=lun,fmt="(a)")     "    ==============================="
                  case default
                     write(unit=lun,fmt="(/,/,a)") "    LIST OF REFLECTIONS (X-RAYS)"
                     write(unit=lun,fmt="(a)")     "    ============================"
               end select
            else
               write(unit=lun,fmt="(/,/,a)") "    LIST OF REFLECTIONS"
               write(unit=lun,fmt="(a)")     "    ==================="
            end if


            if(d == 3) then
               forma="(i11,tr1, i4,i7,tr3,f15.5,tr3,a)"
            else
               if(present(kinfo)) then
                 forma="(i11,tr1, i4,i7,tr3,f15.5,tr3,a,i8,3f10.4)"
               else
                 forma="(i11,tr1, i4,i7,tr3,f15.5,tr3,a,i8)"
               end if
            end if
            write(unit=forma(10:10),fmt="(i1)") d

            Select Case(d)
              Case(3)
                write(lun,'(a)')  &
                '     NumRef    h   k   l   Mult   sinTheta/Lambda   Character'
              Case(4)
                if(present(kinfo)) then
                  write(lun,'(a)')  &
                  '     NumRef    h   k   l   m   Mult   sinTheta/Lambda   Character   Q_coeff   Hr        Kr        Lr'
                else
                  write(lun,'(a)')  &
                  '     NumRef    h   k   l   m   Mult   sinTheta/Lambda   Character   Q_coeff'
                end if
              Case(5)
                if(present(kinfo)) then
                   write(lun,'(a)')  &
                   '     NumRef    h   k   l   m   n   Mult   sinTheta/Lambda   Character   Q_coeff   Hr        Kr        Lr'
                else
                   write(lun,'(a)')  &
                   '     NumRef    h   k   l   m   n   Mult   sinTheta/Lambda   Character   Q_coeff'
                end if
              Case(6)
                if(present(kinfo)) then
                   write(lun,'(a)')  &
                   '     NumRef    h   k   l   m   n   p   Mult   sinTheta/Lambda   Character   Q_coeff   Hr        Kr        Lr'
                else
                   write(lun,'(a)')  &
                   '     NumRef    h   k   l   m   n   p   Mult   sinTheta/Lambda   Character   Q_coeff'
                end if
            End Select

            if(d == 3) then
               do i=1,n
                  write(unit=lun,fmt=forma) i, r(i)%h, r(i)%mult, r(i)%S, charRef(r(i)%imag)
               end do
            else
               if(present(kinfo)) then
                  do i=1,n
                     write(unit=lun,fmt=forma) i, r(i)%h, r(i)%mult, r(i)%S, charRef(r(i)%imag),r(i)%Pcoeff,hr(:,i)
                  end do
               else
                  do i=1,n
                     write(unit=lun,fmt=forma) i, r(i)%h, r(i)%mult, r(i)%S, charRef(r(i)%imag),r(i)%Pcoeff
                  end do
               end if
            end if

         type is (SRefl_Type)
            if (present(mode)) then
               select case (l_case(mode(1:3)))
                  case("neu")
                     write(unit=lun,fmt="(/,/,a)") "    LIST OF REFLECTIONS AND STRUCTURE FACTORS(NEUTRONS)"
                     write(unit=lun,fmt="(a)")     "    ==================================================="
                  case("ele")
                     write(unit=lun,fmt="(/,/,a)") "    LIST OF REFLECTIONS AND STRUCTURE FACTORS(ELECTRONS)"
                     write(unit=lun,fmt="(a)")     "    ===================================================="
                  case default
                     write(unit=lun,fmt="(/,/,a)") "    LIST OF REFLECTIONS AND STRUCTURE FACTORS(X-RAYS)"
                     write(unit=lun,fmt="(a)")     "    ================================================="
               end select

            else
               write(unit=lun,fmt="(a)")   "    LIST OF REFLECTIONS AND STRUCTURE FACTORS"
               write(unit=lun,fmt="(a)")   "    ========================================="
             end if

            if(d == 3) then
               forma="(i11,tr1, i4,i7,tr3,f15.5,tr3,a,4f12.4)"
            else
               if(present(kinfo)) then
                  forma="(i11,tr1, i4,i7,tr3,f15.5,tr3,a,4f12.4,i8,3f10.4)"
               else
                  forma="(i11,tr1, i4,i7,tr3,f15.5,tr3,a,4f12.4,i8)"
               end if
            end if
            write(unit=forma(10:10),fmt="(i1)") d

            Select Case(d)
              Case(3)
                write(lun,'(a)')  &
                '     NumRef    h   k   l   Mult   sinTheta/Lambda   Character     |Fobs|    sig(Fobs)       |Fc|      Phase'
                                                                             !123456789012123456789012123456789012123456789012
              Case(4)
                if(present(kinfo)) then
                   write(lun,'(a)')  &
                   '     NumRef    h   k   l   m   Mult   sinTheta/Lambda   Character     |Fobs|    sig(Fobs)       |Fc|      Phase   Q_coeff   Hr        Kr        Lr'
                else
                   write(lun,'(a)')  &
                   '     NumRef    h   k   l   m   Mult   sinTheta/Lambda   Character     |Fobs|    sig(Fobs)       |Fc|      Phase   Q_coeff'
                end if
              Case(5)
                if(present(kinfo)) then
                   write(lun,'(a)')  &
                   '     NumRef    h   k   l   m   n   Mult   sinTheta/Lambda   Character     |Fobs|    sig(Fobs)       |Fc|      Phase   Q_coeff   Hr        Kr        Lr'
                else
                   write(lun,'(a)')  &
                   '     NumRef    h   k   l   m   n   Mult   sinTheta/Lambda   Character     |Fobs|    sig(Fobs)       |Fc|      Phase   Q_coeff'
                end if
              Case(6)
               if(present(kinfo)) then
                   write(lun,'(a)')  &
                   '     NumRef    h   k   l   m   n   p   Mult   sinTheta/Lambda   Character     |Fobs|    sig(Fobs)       |Fc|      Phase   Q_coeff   Hr        Kr        Lr'
               else
                   write(lun,'(a)')  &
                   '     NumRef    h   k   l   m   n   p   Mult   sinTheta/Lambda   Character     |Fobs|    sig(Fobs)       |Fc|      Phase   Q_coeff'
               end if
            End Select

            if(d == 3) then
               do i=1,n
                  write(unit=lun,fmt=forma) i, r(i)%h, r(i)%mult, r(i)%S, charRef(r(i)%imag), &
                                            r(i)%Fo(ir),r(i)%SFo(ir), r(i)%Fc(ir), r(i)%Phase(ir)
               end do
            else
               if(present(kinfo)) then
                 do i=1,n
                    write(unit=lun,fmt=forma) i, r(i)%h, r(i)%mult, r(i)%S, charRef(r(i)%imag), &
                                              r(i)%Fo(ir),r(i)%SFo(ir), r(i)%Fc(ir), r(i)%Phase(ir),r(i)%Pcoeff,hr(:,i)
                 end do
               else
                 do i=1,n
                    write(unit=lun,fmt=forma) i, r(i)%h, r(i)%mult, r(i)%S, charRef(r(i)%imag), &
                                              r(i)%Fo(ir),r(i)%SFo(ir), r(i)%Fc(ir), r(i)%Phase(ir),r(i)%Pcoeff
                 end do
               end if
            end if

         type is (MRefl_Type)
            write(unit=lun,fmt="(/,/,a)") "    LIST OF REFLECTIONS AND STRUCTURE FACTORS(Nucler & Magnetic)"
            write(unit=lun,fmt="(a)")     "    ============================================================"

            if(d == 3) then
               forma="(i11,tr1, i4,i7,tr3,f15.5,tr3,a,4f12.4,tr3,14f12.4)"
            else
               if(present(kinfo)) then
                  forma="(i11,tr1, i4,i7,tr3,f15.5,tr3,a,4f12.4,tr3,14f12.4,i8,3f10.4)"
               else
                  forma="(i11,tr1, i4,i7,tr3,f15.5,tr3,a,4f12.4,tr3,14f12.4,i8)"
               end if
            end if
            write(unit=forma(10:10),fmt="(i1)") d

            Select Case(d)
              Case(3)
                  write(lun,'(a)')  &
                  '     NumRef    h   k   l   Mult   sinTheta/Lambda   Character     |Fobs|    sig(Fobs)       |Fc|      Phase'// &
                  '   |oMagIntVector|     sigma  Real(MsFx)  Real(MsFy)  Real(MsFz)  Imag(MsFx)  Imag(MsFy)  Imag(MsFz)  Real(MiVx)  Real(MiVy)  Real(MiVz)  Imag(MiVx)  Imag(MiVy)  Imag(MiVz)'
              Case(4)
                if(present(kinfo)) then
                  write(lun,'(a)')  &
                  '     NumRef    h   k   l   m   Mult   sinTheta/Lambda   Character     |Fobs|    sig(Fobs)       |Fc|      Phase'// &
                  '   |oMagIntVector|     sigma  Real(MsFx)  Real(MsFy)  Real(MsFz)  Imag(MsFx)  Imag(MsFy)  Imag(MsFz)  Real(MiVx)  Real(MiVy)  Real(MiVz)  Imag(MiVx)  Imag(MiVy)  Imag(MiVz)   Q_coeff   Hr        Kr        Lr'
                else
                  write(lun,'(a)')  &
                  '     NumRef    h   k   l   m   Mult   sinTheta/Lambda   Character     |Fobs|    sig(Fobs)       |Fc|      Phase'// &
                  '   |oMagIntVector|     sigma  Real(MsFx)  Real(MsFy)  Real(MsFz)  Imag(MsFx)  Imag(MsFy)  Imag(MsFz)  Real(MiVx)  Real(MiVy)  Real(MiVz)  Imag(MiVx)  Imag(MiVy)  Imag(MiVz)   Q_coeff'
                end if
              Case(5)
                if(present(kinfo)) then
                  write(lun,'(a)')  &
                  '     NumRef    h   k   l   m   n   Mult   sinTheta/Lambda   Character     |Fobs|    sig(Fobs)       |Fc|      Phase'// &
                  '   |oMagIntVector|     sigma  Real(MsFx)  Real(MsFy)  Real(MsFz)  Imag(MsFx)  Imag(MsFy)  Imag(MsFz)  Real(MiVx)  Real(MiVy)  Real(MiVz)  Imag(MiVx)  Imag(MiVy)  Imag(MiVz)   Q_coeff   Hr        Kr        Lr'
                else
                  write(lun,'(a)')  &
                  '     NumRef    h   k   l   m   n   Mult   sinTheta/Lambda   Character     |Fobs|    sig(Fobs)       |Fc|      Phase'// &
                  '   |oMagIntVector|     sigma  Real(MsFx)  Real(MsFy)  Real(MsFz)  Imag(MsFx)  Imag(MsFy)  Imag(MsFz)  Real(MiVx)  Real(MiVy)  Real(MiVz)  Imag(MiVx)  Imag(MiVy)  Imag(MiVz)   Q_coeff'
                end if
              Case(6)
                if(present(kinfo)) then
                  write(lun,'(a)')  &
                  '     NumRef    h   k   l   m   n   p   Mult   sinTheta/Lambda   Character     |Fobs|    sig(Fobs)       |Fc|      Phase'// &
                  '   |oMagIntVector|     sigma  Real(MsFx)  Real(MsFy)  Real(MsFz)  Imag(MsFx)  Imag(MsFy)  Imag(MsFz)  Real(MiVx)  Real(MiVy)  Real(MiVz)  Imag(MiVx)  Imag(MiVy)  Imag(MiVz)   Q_coeff   Hr        Kr        Lr'
                else
                  write(lun,'(a)')  &
                  '     NumRef    h   k   l   m   n   p   Mult   sinTheta/Lambda   Character     |Fobs|    sig(Fobs)       |Fc|      Phase'// &
                  '   |oMagIntVector|     sigma  Real(MsFx)  Real(MsFy)  Real(MsFz)  Imag(MsFx)  Imag(MsFy)  Imag(MsFz)  Real(MiVx)  Real(MiVy)  Real(MiVz)  Imag(MiVx)  Imag(MiVy)  Imag(MiVz)   Q_coeff'
                end if
            End Select

            if(d == 3) then
               do i=1,n
                  write(unit=lun,fmt=forma) i, r(i)%h, r(i)%mult, r(i)%S, charRef(r(i)%imag), &
                                               r(i)%Fo(ir),r(i)%SFo(ir), r(i)%Fc(ir), r(i)%Phase(ir), &
                                               r(i)%mIvo,r(i)%smIvo,real(r(i)%msF(:)),aimag(r(i)%msF(:)),&
                                               real(r(i)%mIv(:)),aimag(r(i)%mIv(:))
               end do
            else
               if(present(kinfo)) then
                  do i=1,n
                     write(unit=lun,fmt=forma) i, r(i)%h, r(i)%mult, r(i)%S, charRef(r(i)%imag), &
                                                  r(i)%Fo(ir),r(i)%SFo(ir), r(i)%Fc(ir), r(i)%Phase(ir), &
                                                  r(i)%mIvo,r(i)%smIvo,real(r(i)%msF(:)),aimag(r(i)%msF(:)),&
                                                  real(r(i)%mIv(:)),aimag(r(i)%mIv(:)), r(i)%Pcoeff,hr(:,i)
                  end do
               else
                  do i=1,n
                     write(unit=lun,fmt=forma) i, r(i)%h, r(i)%mult, r(i)%S, charRef(r(i)%imag), &
                                                  r(i)%Fo(ir),r(i)%SFo(ir), r(i)%Fc(ir), r(i)%Phase(ir), &
                                                  r(i)%mIvo,r(i)%smIvo,real(r(i)%msF(:)),aimag(r(i)%msF(:)),&
                                                  real(r(i)%mIv(:)),aimag(r(i)%mIv(:)), r(i)%Pcoeff
                  end do
               end if
            end if
      End Select

   End Subroutine Write_Info_RefList

End SubModule Refl_Write_List