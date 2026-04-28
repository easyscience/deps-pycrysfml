 Submodule (CFML_Optimization_LSQ) OPT_LSQ_Output
  implicit none
  contains
    !!---- Module Function Correl_from_LSQ_LM_VS(c) Result(correl)
    !!----  type(LSQ_conditions_type),  intent(in)     :: c
    !!----
    !!---- Function for returning the correlation matrix after a
    !!---- a Levenberg-Marquard fit.
    !!----
    !!---- Introduced by Nebil March 6 - 2025
    !!
    Module Function Correl_from_LSQ_LM_VS(c) Result(corr_matrix)
      !---- Arguments ----!
      type(LSQ_conditions_type), intent(in) :: c
      real(kind=cp), dimension(c%npvar,c%npvar) :: corr_matrix

      !---- Local variables ----!
      integer :: i,j
      real(kind=cp) :: g2

      do i=1,c%npvar
         do j=i,c%npvar
            correl(i,j)=correl(i,j)/sqrt(curv_mat(i,i)*curv_mat(j,j))
            correl(j,i)=correl(i,j)
         end do
      end do
      do i=1,c%npvar
         g2=sqrt(correl(i,i))
         do j=i,c%npvar
            correl(i,j)=correl(i,j)/g2/sqrt(correl(j,j))*100.0
            correl(j,i)=correl(i,j)
         end do
      end do

      corr_matrix(:,:) = correl(1:c%npvar,1:c%npvar)

    End Function Correl_from_LSQ_LM_VS

    !!--..
    !!--..  Module Subroutine Info_LSQ_LM_V(Chi2,Lun,c,v,vstd,vnam)
    !!--..   real(kind=cp),                 intent(in) :: chi2         !Final Chi2
    !!--..   integer,                       intent(in) :: lun          !Logical unit for output
    !!--..   type(LSQ_conditions_type),     intent(in) :: c            !Conditions of the refinement
    !!--..   real(kind=cp),   dimension(:), intent(in) :: v,vstd       !State vector and standad deviations (parameters of the model)
    !!--..   character(len=*),dimension(:), intent(in) :: vnam         !Names of the refined parameters
    !!--..
    !!--..  Subroutine for output information at the end of refinement of a Levenberg-Marquard fit
    !!--..
    !!---- Update: November 1 - 2013
    !!
    Module Subroutine Info_LSQ_LM_V(Chi2,Lun,c,v,vstd,vnam)
       !---- Arguments ----!
       real(kind=cp),                 intent(in) :: chi2
       integer,                       intent(in) :: lun
       type(LSQ_conditions_type),     intent(in) :: c
       real(kind=cp),   dimension(:), intent(in) :: v,vstd
       character(len=*),dimension(:), intent(in) :: vnam
       !---- Local variables ----!
       integer       :: i,j,inum

       !---- Correlation matrix ----!
       !Here the correlation matrix is already well calculated

       write(unit=lun,fmt="(/,a,/)")   " => Correlation Matrix: "

       do i=1,c%npvar
          do j=i,c%npvar
             correl(i,j)=correl(i,j)*100.0
             correl(j,i)=correl(i,j)
          end do
       end do

       inum=0
       do i=1,c%npvar-1
          do j=i+1,c%npvar
             if (correl(i,j) > real(c%corrmax) ) then
                write(unit=lun,fmt="(a,i4,a,i2,4a)") "    Correlation:",nint(min(correl(i,j),100.0)),  &
                     " > ",c%corrmax,"% for parameters:   ", adjustr(vnam(i))," & ", vnam(j)
                inum=inum+1
             end if
          end do
       end do
       if (inum == 0) then
          write(unit=lun,fmt="(/,a,i2,a)") " => There is no correlations greater than ",c%corrmax,"% "
       else
          write(unit=lun,fmt="(/,a,i3,a,i2,a)") " => There are ",inum," pairs of parameters with Correlation > ",c%corrmax,"%"
       end if

       write(unit=lun,fmt="(/,/,a,/,a,/)") "      FINAL LIST OF REFINED PARAMETERS AND STANDARD DEVIATIONS",&
                                           "      --------------------------------------------------------"
       write(unit=lun,fmt="(/,a,/)") &
       "    Parameter name     No.(LSQ)         Final-Value   Standard Deviation"
       do i=1,c%npvar
        write(unit=lun,fmt="(a,i6,2f20.5)") "    "//vnam(i),i,v(i),vstd(i)
       end do
       write(unit=lun,fmt="(/,a,g13.5)")  " => Final value of Chi2: ",chi2
       write(unit=lun,fmt="(a)")  " => Well-behaved LSQ-problems give Tikhonov regularization equal to zero (convergence status above)"
       write(unit=lun,fmt="(a)")  &
       " => In ill-behaved LSQ-problems Tikhonov regularization is selected equal to 10^-6*Maximum(Singular Value) in SVDecomposition"
       if(chi2 > 1.0) then
          write(unit=lun,fmt="(a)") " => The LSQ-standard deviations have been mutiplied by SQRT(Chi2)"
       end if
    End Subroutine Info_LSQ_LM_V

    !!--..
    !!--..  Module Subroutine Info_LSQ_LM_VS(Chi2,Lun,c,vs)
    !!--..   real(kind=cp),              intent(in)     :: chi2       !Final Chi2
    !!--..   integer,                    intent(in)     :: lun        !Logical unit for output
    !!--..   type(LSQ_conditions_type),  intent(in)     :: c          !Conditions of the refinement
    !!--..   type(LSQ_State_Vector_type),intent(in)     :: vs         !State vector (parameters of the model)
    !!--..
    !!--..  Subroutine for output information at the end of refinement of a Levenberg-Marquard fit
    !!--..
    !!--.. Update: November 1 - 2013
    !!
    Module Subroutine Info_LSQ_LM_VS(Chi2,Lun,c,vs)
       !---- Arguments ----!
       real(kind=cp),              intent(in)     :: chi2
       integer,                    intent(in)     :: lun
       type(LSQ_conditions_type),  intent(in)     :: c
       type(LSQ_State_Vector_type),intent(in)     :: vs

       !---- Local variables ----!
       integer       :: i,j,inum
       real(kind=cp) :: g2


       !---- Correlation matrix ----!
       write(unit=lun,fmt="(/,a,/)")   " => Correlation Matrix: "

       do i=1,c%npvar
          do j=i,c%npvar
             correl(i,j)=correl(i,j)/sqrt(curv_mat(i,i)*curv_mat(j,j))
             correl(j,i)=correl(i,j)
          end do
       end do
       do i=1,c%npvar
          g2=sqrt(correl(i,i))
          do j=i,c%npvar
             correl(i,j)=correl(i,j)/g2/sqrt(correl(j,j))*100.0
             correl(j,i)=correl(i,j)
          end do
       end do

       inum=0
       do i=1,c%npvar-1
          do j=i+1,c%npvar
             if (correl(i,j) > real(c%corrmax) ) then
                write(unit=lun,fmt="(a,i4,a,i2,4a)") "    Correlation:",nint(min(correl(i,j),100.0)),  &
                     " > ",c%corrmax,"% for parameters:   ", adjustr(namfree(i))," & ", namfree(j)
                inum=inum+1
             end if
          end do
       end do
       if (inum == 0) then
          write(unit=lun,fmt="(/,a,i2,a)") " => There is no correlations greater than ",c%corrmax,"% "
       else
          write(unit=lun,fmt="(/,a,i3,a,i2,a)") " => There are ",inum," pairs of parameters with Correlation > ",c%corrmax,"%"
       end if

       write(unit=lun,fmt="(/,/,a,/,a,/)") "      FINAL LIST OF REFINED PARAMETERS AND STANDARD DEVIATIONS",&
                                           "      --------------------------------------------------------"
       write(unit=lun,fmt="(/,a,/)") &
       "    #   Parameter name                       No.(Model)         Final-Value   Standard Deviation"

       inum=0
       do i=1,vs%np
          if (vs%code(i)/=0) then
            inum=inum+1
            write(unit=lun,fmt="(i5,a,i6,2f20.5)") inum,"    "//vs%nampar(i),i,vs%pv(i),vs%spv(i)
          end if
       end do

       write(unit=lun,fmt="(/,a,g13.5)") " => Final value of Chi2: ",chi2

    End Subroutine Info_LSQ_LM_VS

    !!----
    !!----  Module Subroutine Info_LSQ_Output(Chi2,FL,Nobs,X,Y,Yc,W,Lun,c,vs,algor,out_obscal,text_info)
    !!----   real(kind=cp),              intent(in)     :: chi2       !Final Chi2
    !!----   real(kind=cp),              intent(in)     :: FL         !Final Marquardt lambda
    !!----   integer,                    intent(in)     :: nobs       !Number of data points
    !!----   real(kind=cp),dimension(:), intent(in)     :: x          !Array with abcisae of Data points
    !!----   real(kind=cp),dimension(:), intent(in)     :: y          !Array with data point values
    !!----   real(kind=cp),dimension(:), intent(in)     :: yc         !Array with calculated values
    !!----   real(kind=cp),dimension(:), intent(in)     :: w          !Array with weight factors
    !!----   integer,                    intent(in)     :: lun        !Logical unit for output
    !!----   type(LSQ_conditions_type),  intent(in)     :: c          !Conditions of the refinement
    !!----   type(LSQ_State_Vector_type),intent(in)     :: vs         !State vector (parameters of the model)
    !!----   character(len=*),           intent(in)     :: algor      !Used algorithm CURFIT or LEVMAR
    !!----   character(len=*), optional, intent(in)     :: out_obscal !If present the vectors X,Y,Yc,Sig(=sqrt(1/w))
    !!----                                                            !Are output in a file called LM_fit.xy
    !!----   character(len=*), optional, intent(in)     :: text_info  !If present, this text is written in the unit Lun
    !!----
    !!----  Subroutine for output information of the least squares program at the end of refinement
    !!----
    !!---- Update: August - 2009, January 2021
    !!
    Module Subroutine Info_LSQ_Output(Chi2,FL,Nobs,X,Y,Yc,W,Lun,c,vs,algor,out_obscal,text_info)
       !---- Arguments ----!
       real(kind=cp),              intent(in)     :: chi2
       real(kind=cp),              intent(in)     :: FL
       integer,                    intent(in)     :: nobs
       real(kind=cp),dimension(:), intent(in)     :: x
       real(kind=cp),dimension(:), intent(in)     :: y
       real(kind=cp),dimension(:), intent(in)     :: yc
       real(kind=cp),dimension(:), intent(in)     :: w
       integer,                    intent(in)     :: lun
       type(LSQ_conditions_type),  intent(in)     :: c
       type(LSQ_State_Vector_type),intent(in)     :: vs
       character(len=*),           intent(in)     :: algor
       character(len=*), optional, intent(in)     :: out_obscal
       character(len=*), optional, intent(in)     :: text_info
       !---- Local variables ----!
       integer       :: i,j,inum, lob=22
       real(kind=dp) :: rfact,rwfact,riobs,rex
       real(kind=cp) :: del,g2
       character(len=:), allocatable :: info_text

       !---- Final calculation and writings R-Factors calculations ----!
       if(present(text_info)) then
         info_text=" FOR "//trim(text_info)
       else
         info_text="  "
       end if
       rfact=0.0
       rwfact=0.0
       riobs=0.0
       do i=1,nobs
          riobs=riobs+y(i)
          del=y(i)-yc(i)
          rfact=rfact+abs(del)
          rwfact=rwfact+w(i)*del*del
       end do
       rfact=rfact/riobs*100.0
       rwfact=sqrt(rwfact/riobs)*100.0
       rex=sqrt(real(nobs-c%npvar)/riobs)*100.0
       write(unit=lun,fmt="(/,(3(a,f8.3)))") "  Rfact= ",rfact,"   Rwfact= ",rwfact,"   Rex= ",rex
       write(unit=lun,fmt="(/,a,F16.3)") "  Final value of Marquardt F-Lambda = ",FL
       write(unit=lun,fmt="(/,a)")       "  The used algorithm is "//algor

       !---- Correlation matrix ----!
       write(unit=lun,fmt="(/,a,/)")   " => Correlation Matrix: "

       do i=1,c%npvar
          do j=i,c%npvar
             correl(i,j)=correl(i,j)/sqrt(curv_mat(i,i)*curv_mat(j,j))
             correl(j,i)=correl(i,j)
          end do
       end do
       do i=1,c%npvar
          g2=sqrt(correl(i,i))
          do j=i,c%npvar
             correl(i,j)=correl(i,j)/g2/sqrt(correl(j,j))*100.0
             correl(j,i)=correl(i,j)
          end do
       end do

       inum=0
       do i=1,c%npvar-1
          do j=i+1,c%npvar
             if (correl(i,j) > real(c%corrmax) ) then
                write(unit=lun,fmt="(a,i4,a,i2,4a)") "    Correlation:",nint(min(correl(i,j),100.0)),  &
                     " > ",c%corrmax,"% for parameters:   ", adjustr(namfree(i))," & ", namfree(j)
                inum=inum+1
             end if
          end do
       end do
       if (inum == 0) then
          write(unit=lun,fmt="(/,a,i2,a)") " => There is no correlations greater than ",c%corrmax,"% "
       else
          write(unit=lun,fmt="(/,a,i3,a,i2,a)") " => There are ",inum," pairs of parameters with Correlation > ",c%corrmax,"%"
       end if

       write(unit=lun,fmt="(/,/,a,/,a,/)") "      FINAL LIST OF REFINED PARAMETERS AND STANDARD DEVIATIONS"//info_text,&
                                           "      --------------------------------------------------------"
       write(unit=lun,fmt="(/,a,/)") &
       "    #   Parameter name                       No.(Model)         Final-Value   Standard Deviation"
       inum=0
       do i=1,vs%np
          if (vs%code(i)/=0) then
            inum=inum+1
            write(unit=lun,fmt="(i5,a,i6,2f20.5)") inum,"    "//vs%nampar(i),i,vs%pv(i),vs%spv(i)
          end if
       end do
       write(unit=lun,fmt="(/,a,g13.5)") " => Final value of Chi2: ",chi2

       if (present(out_obscal)) then
          !---- Output of a file with the observed and calculated curves ----!
          open(unit=lob,file="LM_fit.xy",status="replace", action="write")
          write(unit=lob,fmt="(a)") "!        X             Y-obs          Y-calc           Sigma"
          do i=1,nobs
             write(unit=lob,fmt="(4f16.4)") x(i),y(i),yc(i),sqrt(1.0/w(i))
          end do
          close(unit=lob)
       end if

    End Subroutine Info_LSQ_Output

    !!--++
    !!--++  Module Subroutine Output_Cyc(Ic,Lun,Chi2,vs)
    !!--++   integer,                    intent(in) :: ic  !cycle number
    !!--++   integer,                    intent(in) :: lun !logical number of the output file
    !!--++   real(kind=cp),              intent(in) :: chi2
    !!--++   type(LSQ_State_Vector_type),intent(in) :: vs
    !!--++
    !!--++  (PRIVATE)
    !!--++  Subroutine for output information on each cycle
    !!--++
    !!--++ Update: February - 2005
    !!
    Module Subroutine Output_Cyc(Ic,Lun,Chi2,vs)
       !---- Arguments ----!
       integer,                    intent(in) :: ic  !cycle number
       integer,                    intent(in) :: lun !logical number of the output file
       real(kind=cp),              intent(in) :: chi2
       type(LSQ_State_Vector_type),intent(in) :: vs

       !---- Local variables ----!
       integer       :: i,j
       real(kind=cp) :: rat

       !---- Writing during cycles ----!
       write(unit=lun,fmt="(/,/,a,i5,a,f14.6)")" => Cycle No.:",ic,"  Chi2 =",chi2
       write(unit=lun,fmt="(/,/,a,/)") &
       "              Name-Par       No.      Old-Value          Change        New-Value         Sigma        Change/Sigma"
       j=0
       do i=1,vs%np
          if (vs%code(i)/=0) then
             j=j+1
             if (vs%spv(i) > 1.0e-36) then
                rat=ch(i)/vs%spv(i)
             else
                rat=0.0
             end if
             write(unit=lun,fmt="(a25,i6,a,5f16.6)") " "//trim(namfree(j)),i," ",vs%pv(i),ch(i),pn(i),vs%spv(i),rat
          end if
       end do

    End Subroutine Output_Cyc

 End Submodule OPT_LSQ_Output
