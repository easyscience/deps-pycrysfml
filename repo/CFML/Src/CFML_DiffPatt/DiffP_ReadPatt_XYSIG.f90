!!----
SubModule (CFML_DiffPatt) DiffP_ReadPatt_XYSIG

  implicit none

   Contains

   !!--++
   !!--++ READ_PATTERN_XYSIGMA
   !!--++
   !!--++    Read a pattern for X,Y,Sigma.
   !!--++    Adding (2014) the possibility to read a calculated pattern
   !!--++    in a fouth column. If gr is present a PDFGUI pattern is read.
   !!--++    If header is present the full header of the file is stored in
   !!--++    the hopefully long string: header
   !!--++
   !!--++ 30/04/2019
   !!
   Module Subroutine Read_Pattern_XYSigma(Filename, Pat, PDF, Header)
      !---- Arguments ----!
      character(len=*),           intent(in)  :: Filename      ! Path+Filename
      class(DiffPat_Type),        intent(out) :: Pat
      logical,          optional, intent(in)  :: PDF
      character(len=*), optional, intent (out):: Header

      !---- Local Variables ----!
      real(kind=cp), parameter                     :: EPS1=epsilon(1.0_cp)

      character(len=180)                           :: txt1, aline, fmtfields !, fmtformat
      !character (len=5)                            :: date1
      integer                                      :: line_da, ntt, interpol, i, j,ier,npp,lenhead,&
                                                      i_dat
      real(kind=cp)                                :: fac_x, fac_y,  yp1, sumavar, cnorm, step
      real(kind=cp)                                :: ycor, xt, stepin, ypn, dum
      real(kind=cp), dimension(:), allocatable     :: yc, bk
      logical                                      :: info

      !> Init
      call clear_error()

      !> File exists?
      inquire(file=trim(filename),exist=info)
      if ( .not. info) then
         Err_CFML%IErr=1
         Err_CFML%Msg="Read_Pattern_XYSigma@DIFFPATT: The file "//trim(filename)//" doesn't exist"
         return
      end if

      !> Open File
      open(newunit=i_dat,file=trim(filename),status="old",action="read",position="rewind",iostat=ier)
      if (ier /= 0 ) then
         Err_CFML%IErr=1
         Err_CFML%Msg="Read_Pattern_XYSigma@DIFFPATT: Problems opening the file: "//trim(filename)
         return
      end if

      !---- Or X,Y sigma data ----!
      fac_x=1.0_cp
      fac_y=1.0_cp
      interpol=0
      line_da=1
      npp=0
      ntt=0
      if (present(header)) then
         lenhead=len(header)
         header=" "
      end if

      do
         read(unit=i_dat,fmt="(a)", iostat=ier) txt1
         if (ier /= 0)   exit
         npp=npp+1
         if (index(txt1,"#L r(A)") /= 0) then
            line_da=npp
            npp=0
         end if
      end do

      pat%npts=npp
      if (pat%npts <= 0) then
         Err_CFML%IErr=1
         Err_CFML%Msg="Read_Pattern_XYSigma@DIFFPATT: Number of points negative or zero!"
         close(unit=i_dat)
         return
      end if
      rewind(unit=i_dat)

      if (present(PDF)) then
         do i=1, line_da
            read(unit=i_dat,fmt="(a)") txt1
            if (present(header)) header=trim(header)//trim(txt1)//char(0)
            j= index(txt1,"title=")
            if ( j /= 0 ) then !Title given
               pat%title=adjustl(txt1(j+6:))
            end if
         end do
         Select Type(Pat)
            Type is(DiffPat_G_Type)
              pat%Legend_X="Distance R(Angstroms)"
              pat%Legend_Y="G(R)x1000"
         End Select
         pat%scatvar="Distance"

      else
         read(unit=i_dat,fmt="(a)") txt1
         IF (txt1(1:6) /= "XYDATA") THEN
            pat%title=trim(txt1)
            do
               read(unit=i_dat,fmt="(a)", iostat=ier) txt1
               if (ier /= 0) then
                  Err_CFML%IErr=1
                  Err_CFML%Msg="Read_Pattern_XYSigma@DIFFPATT: Problems with profile DATA file!"
                  close(unit=i_dat)
                  return
               end if
               txt1=adjustl(txt1)
               if(present(header)) header=trim(header)//trim(txt1)//char(0)
               j= index(txt1,"TITLE")
               if ( j /= 0 ) then !Title given
                  pat%title=adjustl(txt1(j+5:))
                  cycle
               end if

               i=index(txt1,"Scattering variable:")
               if (i /= 0) then
                  pat%scatvar=adjustl(txt1(i+20:))
                  cycle
               end if

               i=index(txt1,"Legend_X")
               if (i /= 0)  then
                 Select Type(p => Pat)
                   Type is(DiffPat_G_Type)
                    p%Legend_X = adjustl(txt1(i+9:))
                 End Select
                 cycle
               end if

               i=index(txt1,"Legend_Y")
               if (i /= 0) then
                 Select Type(p => Pat)
                   Type is(DiffPat_G_Type)
                    p%Legend_Y = adjustl(txt1(i+9:))
                 End Select
                 cycle
               end if

               if (txt1(1:1) == "!" .or. txt1(1:1) == "#") cycle
               read(unit=txt1, fmt=*, iostat=ier) yp1, ypn !This is to detect the beginning of numerical values
               if ( ier /= 0) cycle
               backspace (unit=i_dat)
               exit
            end do
            if (present(header)) then
               j=len_trim(header)-1
               i=index(header(1:j),char(0),back=.true.)
               header=header(1:i)
            end if
         else
            pat%title=trim(txt1)
            i=0
            do
               i=i+1
               if (i == 6) exit
               read(unit=i_dat,fmt="(a)", iostat=ier) txt1
               if (ier /= 0) then
                  Err_CFML%IErr=1
                  Err_CFML%Msg="Read_Pattern_XYSigma@DIFFPATT: Problems with profile DATA file!"
                  close(unit=i_dat)
                  return
               end if
               if (txt1(1:1) == "#") i=i-1
               if (present(header)) header=trim(header)//trim(txt1)//char(0)
               line_da=line_da+1
               j= index(txt1,"TITLE")
               if ( j /= 0 ) then !Title given
                  pat%title=adjustl(txt1(j+5:))
               end if

               j=index(txt1,"Scattering variable:")
               if (j /= 0) then
                  pat%scatvar=adjustl(txt1(j+20:))
               end if

               j=index(txt1,"Legend_X")
               if (j /= 0 ) then
                 Select Type(p => Pat)
                   Type is(DiffPat_G_Type)
                    p%Legend_X = adjustl(txt1(i+9:))
                 End Select
               end if

               j=index(txt1,"Legend_Y")
               if (j /= 0) then
                 Select Type(p =>Pat)
                   Type is(DiffPat_G_Type)
                    p%Legend_Y = adjustl(txt1(i+9:))
                 End Select
               end if

               if (txt1(1:5) == "INTER") then !Interpolation possible!
                  read(unit=txt1(6:),fmt=*,iostat=ier) fac_x,fac_y,interpol,stepin
                  if(ier /= 0) then
                    fac_x=1.0_cp; fac_y=1.0_cp; interpol=0; stepin=0.0_cp
                  end if
                  if (fac_x <= 0.0) fac_x=1.0_cp
                  if (fac_y <= 0.0) fac_y=1.0_cp
               end if

               if (txt1(1:4) == "TEMP") then
                 Select Type(p => Pat)

                   Type is(DiffPat_E_Type)

                     read(unit=txt1(5:80),fmt=*, iostat=ier) p%Tsample
                     if(ier == 0) then
                       p%tset=p%Tsample
                     else
                       p%Tsample=0.0
                       p%tset=0.0
                     end if

                   Type is(DiffPat_G_Type)
                     p%Legend_X = adjustl(txt1(i+9:))
                     read(unit=txt1(5:80),fmt=*, iostat=ier) p%Tsample
                     if(ier == 0) then
                       p%tset=p%Tsample
                     else
                       p%Tsample=0.0
                       p%tset=0.0
                     end if

                 End Select
               end if
            end do
         end if

         Select Type(p => Pat)
           class is(DiffPat_E_Type)
              if (interpol == 0) then
                 p%CT_Step = .false.
              else if(interpol == 1) then
                 p%CT_Step = .true.
              else if(interpol == 2) then
                 p%CT_Step = .true.
              end if
         End Select
      end if

      !> Allocating
      call Allocate_Pattern(pat)

      fmtfields = "ffff"  !Now four columns are read in order to incorporate the calculated pattern
      if(trim(pat%scatvar) == "r.l.u.") fmtfields = "fff"
      sumavar=0.0
      cnorm=0.0
      i=0
      do j=1,pat%npts
         read(unit=i_dat,fmt="(a)", iostat=ier) aline
         if(ier /= 0) exit
         aline=adjustl(aline)
         if(aline(1:1) == "!" .or. aline(1:1) == "#") cycle

         i=i+1

         Select Type(p => Pat)
           Type is(DiffPat_Type)
             if (present(PDF)) then
                read(unit=aline,fmt = *, iostat=ier ) pat%x(i),pat%y(i),dum,pat%sigma(i)
             else
                read(unit=aline,fmt = *, iostat=ier ) pat%x(i),pat%y(i),pat%sigma(i)
             end if

           class is(DiffPat_E_Type)
             if (present(PDF)) then
                read(unit=aline,fmt = *, iostat=ier ) p%x(i),p%y(i),dum,p%sigma(i)
             else if(trim(pat%scatvar) == "r.l.u.")  then
                read(unit=aline,fmt = *, iostat=ier ) p%x(i),p%y(i),p%sigma(i)
             else
                read(unit=aline,fmt = *, iostat=ier ) p%x(i),p%y(i),p%sigma(i),p%ycalc(i)
                if(ier /= 0) then
                   p%ycalc(i)=0.0
                   read(unit=aline,fmt = *, iostat=ier ) p%x(i),p%y(i),p%sigma(i)
                   if(ier /= 0) then
                      read(unit=aline,fmt = *, iostat=ier ) p%x(i),p%y(i)
                      p%sigma(i)=0.0
                   end if
                end if
             end if
         End Select

         if (ier /=0) then
            Err_CFML%IErr=1
            Err_CFML%Msg="Read_Pattern_XYSigma@DIFFPATT:  Error in Intensity file, check your instr parameter!"
            close(unit=i_dat)
            return
         end if
         if (i > 10 .and. ABS(pat%x(i)) < EPS1 .AND. pat%y(i) < EPS1 .AND.  pat%sigma(i) < EPS1) then
            i=i-1
            exit
         end if
         pat%x(i)=pat%x(i)*fac_x
         pat%y(i)=pat%y(i)*fac_y

         Select Type(p => Pat)
          class is(DiffPat_E_Type)
           p%ycalc(i)=p%ycalc(i)*fac_y
         End Select

         pat%sigma(i)=pat%sigma(i)*fac_y
         pat%sigma(i)=pat%sigma(i)*pat%sigma(i)
         sumavar=sumavar+pat%sigma(i)
         if (pat%sigma(i) < EPS1) pat%sigma(i) =1.0_cp
         cnorm=cnorm+pat%sigma(i)/MAX(abs(pat%y(i)),0.001_cp)
         if(i > 1) then
           pat%step=pat%step+pat%x(i)-pat%x(i-1)
         end if
      end do

      ntt=i
      pat%xmin=pat%x(1)
      pat%xmax=pat%x(ntt)
      cnorm=cnorm/real(ntt)
      if (sumavar < EPS1) then
         do i=1,ntt
            pat%sigma(i)=abs(pat%y(i))
         end do
         cnorm=1.0
      end if

      if (interpol == 0 .or. interpol == 2) then      !if interpol
         pat%step=pat%step/real(ntt-1)
         pat%npts=ntt

      else                        !else interpol
         pat%step=stepin
         j=(pat%x(ntt)-pat%x(1))/pat%step+1.05
         if( j > pat%npts) then
            pat%step=(pat%x(ntt)-pat%x(1))/(ntt-1)
            pat%npts=ntt
         else
            pat%npts=j
         end if
         if(allocated(bk) ) deallocate(bk)
         allocate(bk(pat%npts))
         if(allocated(yc) ) deallocate(yc)
         allocate(yc(pat%npts))

         !>First interpolate the raw intensities ----!
         yp1=huge(1.0)
         ypn=huge(1.0)
         bk=spline_d2y(pat%x(:),pat%y(:),ntt,yp1,ypn)
         do i=1,pat%npts
            xt=pat%x(1)+(i-1)*step
            ycor=spline_interpol(xt,pat%x(:),pat%y(:),bk(:),ntt)
            yc(i)=ycor !max(1.0_cp,ycor) now negative values are allowed
         end do
         do i=1,pat%npts
            pat%y(i)=yc(i)
            yc(i)=0.0_cp
            bk(i)=0.0_cp
         end do

         !> Second interpolate the sigmas
         bk=spline_d2y(pat%x(:),pat%sigma(:),ntt,yp1,ypn)
         do i=1,pat%npts
            xt=pat%x(1)+(i-1)*step
            ycor=spline_interpol(xt,pat%x(:),pat%sigma(:),bk(:),ntt)
            yc(i)=ycor !max(1.0_cp,ycor) now negative values are allowed
         end do
         do i=1,pat%npts
            pat%sigma(i)=abs(yc(i))
            yc(i)=0.0
            bk(i)=0.0
         end do
         pat%xmax=pat%xmin+step*(pat%npts-1)
      end if                       !End If interpol
      pat%ymin=minval(pat%y(1:pat%npts))
      pat%ymax=maxval(pat%y(1:pat%npts))

      close(unit=i_dat)
   End Subroutine Read_Pattern_XYSigma

End SubModule DiffP_ReadPatt_XYSIG