  Module ODR_wrapper
    use CFML_GlobalDeps,       only : cp, Err_CFML
    use CFML_Optimization_LSQ, only : LSQ_State_Vector_type,LSQ_Conditions_type,LSQ_Data_Type
    use ODRPACK95
    implicit none

    private
    Public :: ODR_LSQ, Info_ODR_VS
    !!--++
    !!--++ CORREL
    !!--++    real(kind=cp), dimension(:,:), allocatable, private  :: correl
    !!--++
    !!--++    Variance/covariance/correlation matrix
    !!--++
    !!--++ Update: October - 2021
    !!
    real(kind=cp), dimension(:,:), allocatable, private  :: correl     !Variance/covariance/correlation matrix

    contains

     Subroutine ODR_LSQ(fcn,d,vs,c,lower,upper,iprint,lun)
       Type(LSQ_Data_Type),                       intent(in out) :: d   !Data to be refined (set in the main program)
       Type(LSQ_State_Vector_type),               intent(in out) :: vs  !State vector containing pv, code, vs%nampar,etc..
       Type(LSQ_Conditions_type ),                intent(in)     :: c   !conditions of the algorithm
       real(kind=cp), dimension(vs%np), optional, intent(in)     :: lower,upper
       integer,                         optional, intent(in)     :: iprint,lun
       interface
         Subroutine fcn(n,m,np,nq,ldn,ldm,ldnp,beta,xplusd,ifixb,ifixx,ldifx,ideval,f,fjacb,fjacd,istop)
            !import :: cp
            Integer,                         Intent(In)    :: n       !Number of observations
            Integer,                         Intent(In)    :: m       !Number of colums of array X+D (n,m)
            Integer,                         Intent(In)    :: np      !Number of function parameters
            Integer,                         Intent(In)    :: nq      !Number of responses per observation
            Integer,                         Intent(In)    :: ldn     !Leading dimension >= n
            Integer,                         Intent(In)    :: ldm     !Leading dimension >= m
            Integer,                         Intent(In)    :: ldnp    !Leading dimension >= np
            Real,Dimension(np),              Intent(In)    :: beta    !Current values of parameters
            Real,Dimension(ldn,m),           Intent(In)    :: xplusd  !Current Values of the explanatory variables X+D
            Integer,      Dimension(np),     Intent(In)    :: ifixb   !0 fixed, 1 varied
            Integer,                         Intent(In)    :: ldifx   !leading dimension of array ifixx
            Integer,      Dimension(ldifx,m),Intent(In)    :: ifixx   !=0
            Integer,                         Intent(In)    :: ideval
            Real,Dimension(ldn,nq),          Intent(In Out):: f
            Real,Dimension(ldn,ldnp,nq),     Intent(Out)   :: fjacb
            Real,Dimension(ldn,ldm,nq),      Intent(Out)   :: fjacd
            Integer,                         Intent(In Out):: istop
         End Subroutine fcn
       end interface

       !Variables used by ODR procedures to index information in work-array
       logical:: isodr
       integer:: deltai,epsi,xplusi,fni,sdi,vcvi,rvari,wssi,wssdei,wssepi,rcondi,etai,        &
                 olmavi,taui,alphai,actrsi,pnormi,rnorsi,prersi,partli,sstoli,taufci,epsmai,  &
                 beta0i,betaci,betasi,betani,si,ssi,ssfi,qrauxi,ui, fsi,fjacbi,we1i,diffi,    &
                 deltsi,deltni,ti,tti,omegai,fjacdi,wrk1i,wrk2i,wrk3i,wrk4i,wrk5i,wrk6i,wrk7i,&
                 loweri,upperi,lwkmn
       integer:: i_odr, i_print, job, i,j
       real(kind=cp)                        :: gi,gj
       real(kind=cp), dimension(d%nobs)     :: ww
       real(kind=cp), dimension(:), pointer :: work

       !job=I5 I4 I3 I2 I1
       ! I1 >=2  ordinary least squares, =0 explicit ODR, =1 implicit ODR
       ! I2 >= 3  user-supplied derivative code (no-cheked), =2 Checked, =1 central finite differences, =0 forward fd
       ! I3 = 0  V_beta, sigma_beta using derivatives computed at the solution, =1, last iteration, >=2 not computed
       ! I4  only for pure ODR
       ! I5 = 0 fit is not a restart, >=1 fit is a restart
       !lwork=18 + 13*vs%np + vs%np*vs%np + 1 + 1*1 + 4*d%nobs*1 + 2*d%nobs*1 + 2*d%nobs*1*1 + 5*1 + &
       !      1*(vs%np+1) + (d%nobs*1)*1
       !allocate(work(lwork))  <= it is an output parameter from ODR
       job= 00032  !Analytical derivatives for ordinary LSQ, sigmas calculated at the solution
       ww=0.0
       where(d%sw > 0.0_cp) ww=1.0_cp/(d%sw*d%sw)
       i_odr=6; i_print=1001
       if(present(lun)) i_odr=lun
       if(present(iprint)) i_print=iprint
       if(present(lower) .and. present(upper) ) then
          call ODR(fcn, n=d%nobs, m=1, np=vs%np, nq=1, beta=vs%pv, Y=reshape(d%y,[d%nobs,1]), X=reshape(d%x,[d%nobs,1]), &
                   ifixb=vs%code, we = reshape(ww,[d%nobs,1,1]), Lower=lower, Upper=upper, iprint=i_print, lunerr=i_odr, lunrpt=i_odr, &
                   Maxit=c%icyc, work=work, job=job)
       else
          call ODR(fcn, n=d%nobs, m=1, np=vs%np, nq=1, beta=vs%pv, Y=reshape(d%y,[d%nobs,1]), X=reshape(d%x,[d%nobs,1]), &
                   ifixb=vs%code, we = reshape(ww,[d%nobs,1,1]), iprint=i_print, lunerr=i_odr, lunrpt=i_odr, &
                   Maxit=c%icyc,work=work, job=job)
       end if

       !Call to the following subroutine to obtain the indices of the different parts of the working array
       !that contains information about estimated standard deviations of the final parameters: index SDI
       !call dwinf(n,m=1,np,nq=1,ldwe=1,ld2we=1,.false.....
       isodr=.false.
       call dwinf(d%nobs,1,vs%np,1,1,1,isodr,deltai,epsi,xplusi,fni,sdi,vcvi,rvari,wssi,wssdei,wssepi,rcondi,etai,  &
                  olmavi,taui,alphai,actrsi,pnormi,rnorsi,prersi,partli,sstoli,taufci,epsmai,beta0i,betaci,betasi,  &
                  betani,si,ssi,ssfi,qrauxi,ui,fsi,fjacbi,we1i,diffi,deltsi,deltni,ti,tti,omegai,fjacdi,wrk1i,wrk2i,&
                  wrk3i,wrk4i,wrk5i,wrk6i,wrk7i,loweri,upperi,lwkmn)

       !WORK(SDI) is the first element of a p×1 array SD containing the standard deviations
       !SDBETA of the function parameters BETA, i.e., the square roots of the diagonal
       !entries of the covariance matrix, where
       !WORK(SDI-1+K) = SD(K) = sqrt(V(K,K))=sigma(beta) for K = 1, . . . , p.
       !
       !The standard deviations are only computed when the
       !third digit of JOB is less than or equal to 1. (See §2.A.ii, subroutine
       !argument JOB, and §4.B.) Rows of SD corresponding to fixed elements
       !of BETA, and to elements dropped because they induced rank deficiency,
       !are set to zero.
       !SDI = 2nm + 2nq + 1.
       do i=1,vs%np
          vs%spv(i)=work(sdi-1+i)
          !write(*,"(i5,a,2f15.5)") i,vs%nampar(i),vs%pv(i),vs%spv(i)
       end do
       !vs%spv(1:vs%np)=work(sdi:sdi+vs%np-1)  !Standard deviations

       if(allocated(correl)) deallocate(correl)
       allocate(correl(vs%np,vs%np))
       do i=1,vs%np
         do j=1,vs%np
            correl(i,j) = work(vcvi-1+i+(j-1)*vs%np)
         end do
       end do
       do i=1,vs%np
          gi=sqrt(correl(i,i))
          do j=i,vs%np
             if(gi < 1.0e-8) then
               correl(i,j)=0.0_cp
             else
               gj=sqrt(correl(j,j))
               if(gj < 1.0e-8)  then
                  correl(i,j)=0.0_cp
               else
                  correl(i,j)=100.0*correl(i,j)/gi/gj
               end if
             end if
             correl(j,i)=correl(i,j)
          end do
       end do


     End Subroutine ODR_LSQ
    !!----
    !!----  Subroutine Info_ODR_VS(Chi2,Lun,vs)
    !!----   real(kind=cp),              intent(in)     :: chi2       !Final Chi2
    !!----   integer,                    intent(in)     :: lun        !Logical unit for output
    !!----   type(LSQ_conditions_type),  intent(in)     :: c          !Conditions of the refinement
    !!----   type(LSQ_State_Vector_type),intent(in)     :: vs         !State vector (parameters of the model)
    !!----
    !!----  Subroutine for output information at the end of refinement of an ODR fit
    !!----
    !!---- Update: October 31 - 2021
    !!
    Subroutine Info_ODR_VS(Chi2,Lun,c,vs,d,text_info)
       !---- Arguments ----!
       real(kind=cp),              intent(in)     :: chi2
       integer,                    intent(in)     :: lun
       type(LSQ_conditions_type),  intent(in)     :: c
       type(LSQ_State_Vector_type),intent(in)     :: vs
       Type(LSQ_Data_Type),        intent(in)     :: d
       character(len=*),optional,  intent(in)     :: text_info

       !---- Local variables ----!
       integer       :: i,j,inum
       real(kind=cp) :: del,rwfact,rfact,riobs,rex
       character(len=:), allocatable :: info_text

       if(present(text_info)) then
         info_text=" FOR "//trim(text_info)
       else
         info_text="  "
       end if
       !---- Correlation matrix ----!
       write(unit=lun,fmt="(/,a,/)")   " => Correlation Matrix: "
       inum=0
       do i=1,vs%np-1
          do j=i+1,vs%np
             if (correl(i,j) > real(c%corrmax) ) then
                write(unit=lun,fmt="(a,i4,a,i2,4a)") "    Correlation:",nint(min(correl(i,j),100.0)),  &
                     " > ",c%corrmax,"% for parameters:   ", adjustr(vs%nampar(i))," & ", vs%nampar(j)
                inum=inum+1
             end if
          end do
       end do
       if (inum == 0) then
          write(unit=lun,fmt="(/,a,i2,a)") " => There is no correlations greater than ",c%corrmax,"% "
       else
          write(unit=lun,fmt="(/,a,i3,a,i2,a)") " => There are ",inum," values of Correlation > ",c%corrmax,"%"
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
       rfact=0.0
       rwfact=0.0
       riobs=0.0
       do i=1,d%nobs
          riobs=riobs+d%y(i)
          del=d%y(i)-d%yc(i)
          rfact=rfact+abs(del)
          rwfact=rwfact+del*del/(d%sw(i)*d%sw(i))
       end do
       rfact=rfact/riobs*100.0
       rwfact=sqrt(rwfact/riobs)*100.0
       rex=sqrt(real(d%nobs-c%npvar)/riobs)*100.0
       write(unit=lun,fmt="(/,(3(a,f8.3)))") "    Rfact= ",rfact,"   Rwfact= ",rwfact,"   Rex= ",rex
       write(unit=lun,fmt="(/,a,g13.5)")     " => Final value of Chi2: ",chi2

    End Subroutine Info_ODR_VS

  End Module ODR_wrapper