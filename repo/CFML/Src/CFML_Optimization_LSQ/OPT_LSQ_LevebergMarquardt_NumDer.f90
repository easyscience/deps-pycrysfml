 Submodule (CFML_Optimization_LSQ) OPT_LSQ_LevenbergMarquardt_NumDer
  implicit none
   contains

    !!--++
    !!--++   Module Subroutine LM_Dif(Model_Functn, m, c, Vs, chi2, infout,residuals,idebug)
    !!--++     Integer,                     Intent(In)      :: m        !Number of observations
    !!--++     type(LSQ_conditions_type),   Intent(In Out)  :: c        !Conditions of refinement
    !!--++     type(LSQ_State_Vector_type), Intent(In Out)  :: Vs       !State vector
    !!--++     Real (Kind=cp),              Intent(out)     :: chi2     !final Chi2
    !!--++     character(len=*),            Intent(out)     :: infout   !Information about the refinement (min length 256)
    !!--++     Real (Kind=cp), dimension(:),optional, intent(out) :: residuals
    !!--++     integer,                     optional, intent(in)  :: idebug !logical unit for writing results
    !!--++     !--- Local Variables ---!
    !!--++     Interface
    !!--++       Subroutine Model_Functn(m, n, x, fvec, iflag)             !Model Function subroutine
    !!--++         Use CFML_GlobalDeps, Only: cp
    !!--++         Integer,                       Intent(In)    :: m, n    !Number of observations and free parameters
    !!--++         Real (Kind=cp),Dimension(:),   Intent(In)    :: x       !Array with the values of free parameters: x(1:n)
    !!--++         Real (Kind=cp),Dimension(:),   Intent(In Out):: fvec    !Array of residuals fvec=(y-yc)/sig : fvec(1:m)
    !!--++         Integer,                       Intent(In Out):: iflag   !If iflag=1 calculate only fvec without changing fjac
    !!--++       End Subroutine Model_Functn                               !If iflag=2 calculate only fjac keeping fvec fixed
    !!--++     End Interface
    !!--++
    !!--++
    !!--++
    !!--++
    !!--++  This interface to lmdif has been modified with respect to the original lmdif1 (also available in
    !!--++  the overloaded procedure) in order to use the LSQ types of this module.
    !!--++  The residuals vector has been eliminated from the arguments in order to expose to the
    !!--++  user only the important information that are stored in Vs of type LSQ_Vector_State_Type.
    !!--++  The "iwa" argument has also been suppressed.
    !!--++  If one uses Vs%code_comp=.true.  be careful with the calculations inside
    !!--++  Model_Functn, for making properly the constraints one has to work with
    !!--++  shift of parameters instead of the parameters themselves.
    !!--++
    !!--++ Updated: January - 2014
    !!
    Module Subroutine LM_Dif(Model_Functn, m, c, Vs, chi2, infout,residuals,idebug)
       !---- Arguments ----!
       Integer,                     Intent(In)      :: m        !Number of observations
       type(LSQ_conditions_type),   Intent(In Out)  :: c        !Conditions of refinement
       type(LSQ_State_Vector_type), Intent(In Out)  :: Vs       !State vector
       Real (Kind=cp),              Intent(out)     :: chi2     !final Chi2
       character(len=*),            Intent(out)     :: infout   !Information about the refinement  (min length 256)
       Real (Kind=cp), dimension(:),optional, intent(out) :: residuals
       integer,                     optional, intent(in)  :: idebug !logical unit for writing results

       Interface
         Subroutine Model_Functn(m, n, x, fvec, iflag)             !Model Function subroutine
            Use CFML_GlobalDeps, Only: cp
            Integer,                       Intent(In)    :: m, n    !Number of observations and free parameters
            Real (Kind=cp),Dimension(:),   Intent(In)    :: x       !Array with the values of free parameters: x(1:n)
            Real (Kind=cp),Dimension(:),   Intent(In Out):: fvec    !Array of residuals fvec=(y-yc)/sig : fvec(1:m)
            Integer,                       Intent(In Out):: iflag   !If iflag=1 calculate only fvec without changing fjac
         End Subroutine Model_Functn                                !If iflag=2 calculate only fjac keeping fvec fixed
       End Interface

       !--- Local Variables ---!
       Integer                              :: i,j, maxfev, mode, nfev, nprint, n, &
                                               iflag,info
       Integer,        dimension(c%npvar)   :: ipvt
       Integer, dimension(c%npvar,c%npvar)  :: p,id
       Real (Kind=cp), dimension(c%npvar)   :: x, sx,xo
       Real (Kind=cp), dimension(vs%np)     :: vp
       Real (Kind=cp), dimension(m)         :: fvec  !Residuals
       Real (Kind=cp), dimension(m,c%npvar) :: fjac
       Real (Kind=cp)                       :: epsfcn,ftol, gtol, xtol,iChi2,deni,denj
       Real (Kind=cp), Parameter            :: factor = 100.0_CP, zero = 0.0_CP
       character(len=20)                    :: formt

       info = 0
       n=c%npvar
       c%reached=.false.
       id=0
       do i=1,n
          id(i,i) = 1
       end do
       Chi2=1.0e35
       infout=" "
       !     check the input parameters for errors.
       If ( n <= 0 .OR. m < n .OR. c%tol < zero ) then
         write(unit=infout,fmt="(2a,i5,a,i5,a,f10.7)") "Improper input parameters in LM-optimization (n,m,tol):", &
                        " nb of free (refined) parameters: ", n, &
                        ", number of observations: ", m, &
                        ", tolerance: ", c%tol
          Return
       End if


       j=0
       if(vs%code_comp) then
          !save the initial values of all parameters
          vp=vs%pv(1:vs%np)
          do j=1,n
            do i=1,vs%np
               if(vs%code(i) == j) then
                  x(j)=Vs%pv(i)
                  namfree(j)=vs%nampar(i)
                  exit
               end if
            end do
          end do
          xo=x !saving the initial free parameter
       else
          do i=1,vs%np
             if (vs%code(i) == 0) cycle
             j=j+1
             x(j)=Vs%pv(i)
             namfree(j)=vs%nampar(i)
          end do
       end if

       !Initial calculation of Chi2
       iflag=1
       Call Model_Functn(m, n, x, fvec, iflag)
       ichi2=norm2(fvec(1:m))
       if (ichi2 < 1.0e15) then
          ichi2=ichi2*ichi2/max(1.0_cp,real(m-n,kind=cp))
       end if
       If ( c%icyc < 5*n) then
          maxfev = 200*(n + 1)
          c%icyc= maxfev
       Else
          maxfev = c%icyc
       End if
       ftol = c%tol
       xtol = c%tol
       gtol = zero
       epsfcn = zero
       mode = 1
       nprint = c%nprint

       ! Call to the core procedure of the MINPACK implementation of the Levenberg-Marquardt algorithm
       Call lmdif(Model_Functn, m, n, x, fvec, ftol, xtol, gtol, maxfev, epsfcn,   &
                  mode, factor, nprint, info, nfev, fjac, ipvt)

       !Calculate final Chi2 or norm
       chi2=norm2(fvec(1:m))
       if (chi2 < 1.0e15) then
          chi2=chi2*chi2/max(1.0_cp,real(m-n,kind=cp))
       end if
       sx=zero
       !Extract the curvature matrix side of equation below from fjac
       !        t     t           t
       !       p *(jac *jac)*p = r *r
       !           t            t     t
       ! cvm = (jac *jac) = p*(r *r)*p     -> Permutation matrices are orthogonal
       ! See the documentation of lmder1 and lmdif1 for why we use only the (n,n) submatrix of fjac
       !curv_mat(1:n,1:n)=matmul( transpose( fjac(1:n,1:n) ) , fjac(1:n,1:n) )
       curv_mat(1:n,1:n)=matmul( transpose( Upper_Triangular(fjac,n)) , Upper_Triangular(fjac,n) )
       do j=1,n
          p(1:n,j) = id(1:n,ipvt(j))
       end do
       curv_mat(1:n,1:n) = matmul(  p, matmul( curv_mat(1:n,1:n),transpose(p) )  )

       do j=1,n
          denj=curv_mat(j,j)
          if( denj <= zero) denj=1.0
          do i=1,n
             deni=curv_mat(i,i)
             if( deni <= zero) deni=1.0
             correl(j,i)=curv_mat(j,i)/sqrt(denj*deni)
          end do
          correl(j,j)=1.0001
       end do

       if(present(idebug)) then
         formt="(100g10.3)"
         !write(unit=formt(2:5),fmt="(i4)") n/12
         write(unit=idebug,fmt="(a,i5,a,/)") "  Output pseudo-Jacobian for ",n," free parameters"
         do j=1,n
           write(unit=idebug,fmt=formt) fjac(j,1:n)
         end do
         write(unit=idebug,fmt="(/,a,i5,a,/)") "  Diagonal of the calculated Curvature matrix for ",n," free parameters"
         write(unit=idebug,fmt=formt) (curv_mat(j,j),j=1,n)
         write(unit=idebug,fmt="(a,g14.4)") "  Maxvalue of Correl before inversion: ",maxval(correl)
         write(unit=idebug,fmt="(a,g14.4)") "  Minvalue of Correl before inversion: ",minval(correl)
       end if

       correl(1:n,1:n) = Inverse_Matrix(correl(1:n,1:n))


       If (Err_CFML%Ierr == 0) then
          Do i=1,n
             deni = curv_mat(i,i)
             if( deni <= zero) deni=1.0
             sx(i) = sqrt(abs(correl(i,i)/deni))         !sqrt(abs(correl(i,i)*Chi2))
          End Do
       Else
          Do i=1,n
             deni = curv_mat(i,i)
             if( deni <= zero) then
               sx(i) = 99999.9
             else
               sx(i) = sqrt(abs(correl(i,i)/deni))         !sqrt(abs(correl(i,i)*Chi2))
             end if
          End Do
          Err_CFML%Ierr = 1
          Err_CFML%Msg=" => Singular Matrix at the end of LM_Der for calculating std deviations ... provided sigmas are dubious!"
          c%failed=.true.
       End if

       if(present(idebug)) then
         write(unit=idebug,fmt="(/,a,i5,a,/)") "  Diagonal of the Inverse Correlation matrix for ",n," free parameters"
         write(unit=idebug,fmt=formt) (correl(j,j),j=1,n)
         write(unit=idebug,fmt="(/,a,i5,a,/)") "  Variances for ",n," free parameters"
         write(unit=idebug,fmt=formt) (correl(j,j)/curv_mat(j,j),j=1,n)
       end if

       !Update the State vector Vs
       n=0
       if(vs%code_comp) then
          Do i=1,Vs%np
             if (Vs%code(i) == 0) then
                 Vs%spv(i)=0.0
             Else
                n=vs%code(i)
                Vs%pv(i) = vp(i)+(x(n)-xo(n))*Vs%mul(i)
                Vs%spv(i)=sx(n)*Vs%mul(i)
             End if
          End do
       else
          Do i=1,Vs%np
             if (Vs%code(i) == 0) then
                 Vs%spv(i)=0.0
             Else
                n=n+1
                Vs%pv(i) = x(n)
                Vs%spv(i)=sx(n)
             End if
          End do
       end if

       Select Case (info)
          Case(0)
             c%reached=.false.
          Case(1)
             c%reached=.true.
             write(unit=infout,fmt="(a,g12.5,a,e12.5)") "Initial Chi2:", ichi2, &
                   "     Convergence reached: The relative error in the sum of squares is at most ",c%tol

          Case(2)
             c%reached=.true.
             write(unit=infout,fmt="(a,g12.5,a,e12.5)") "Initial Chi2:", ichi2,&
                    "     Convergence reached: The relative error between x and the solution is at most ",c%tol

          Case(3)
             c%reached=.true.
             write(unit=infout,fmt="(a,g12.5,a,e12.5)") "Initial Chi2:", ichi2, &
             "    Convergence reached: The relative error"// &
             " in the sum of squares and the difference between x and the solution are both at most ",c%tol

          Case(4,8)
             c%reached=.true.
             write(unit=infout,fmt="(a,g12.5,a)") "Initial Chi2:", ichi2, &
              "    Convergence reached: Residuals vector is orthogonal to the columns of the Jacobian to machine precision"

          Case(5)
             c%reached=.false.
             write(unit=infout,fmt="(a,i6)") &
             "Convergence NOT reached: Number of calls to model function with iflag=1 has reached ",maxfev

          Case(6)
             c%reached=.false.
             write(unit=infout,fmt="(a,e12.5,a)") "Convergence NOT reached: Provided tolerance ",c%tol,&
                                            " is too small! No further reduction in the sum of squares is possible"
          Case(7)
             c%reached=.false.
             write(unit=infout,fmt="(a,e12.5,a)") "Convergence NOT reached: Provided tolerance ",c%tol,&
                                            " is too small! No further improvement in the approximate solution x is possible"
       End Select
       if (present(residuals)) residuals(1:m)=fvec(1:m)

    End Subroutine LM_Dif

    !!--++
    !!--++   Module Subroutine LM_DifV(Model_Functn, m, c, V, Vstd, chi2, infout,residuals)
    !!--++     Integer,                     Intent(In)      :: m        !Number of observations
    !!--++     type(LSQ_conditions_type),   Intent(In Out)  :: c        !Conditions of refinement
    !!--++     Real (Kind=cp),dimension(:), Intent(In Out)  :: V        !State vector
    !!--++     Real (Kind=cp),dimension(:), Intent(In Out)  :: Vstd     !Standard deviations vector
    !!--++     Real (Kind=cp),              Intent(out)     :: chi2     !final Chi2
    !!--++     character(len=*),            Intent(out)     :: infout   !Information about the refinement (min length 256)
    !!--++     Real (Kind=cp), dimension(:),optional, intent(out) :: residuals
    !!--++
    !!--++     !--- Local Variables ---!
    !!--++     Interface
    !!--++       Subroutine Model_Functn(m, n, x, fvec, fjac, iflag)       !Model Function subroutine
    !!--++         Use CFML_GlobalDeps, Only: cp
    !!--++         Integer,                       Intent(In)    :: m, n    !Number of obserations and free parameters
    !!--++         Real (Kind=cp),Dimension(:),   Intent(In)    :: x       !Array with the values of free parameters: x(1:n)
    !!--++         Real (Kind=cp),Dimension(:),   Intent(In Out):: fvec    !Array of residuals fvec=(y-yc)/sig : fvec(1:m)
    !!--++         Real (Kind=cp),Dimension(:,:), Intent(Out)   :: fjac    !Jacobian Dfvec/Dx(i,j) = [ dfvec(i)/dx(j) ] : fjac(1:m,1:n)
    !!--++         Integer,                       Intent(In Out):: iflag   !If iflag=1 calculate only fvec without changing fjac
    !!--++       End Subroutine Model_Functn                               !If iflag=2 calculate only fjac keeping fvec fixed
    !!--++     End Interface
    !!--++
    !!--++
    !!--++     This interface to lmdif has been modified with respect to the original
    !!--++     lmdif1 (also available in the overloaded procedure) in order to use the
    !!--++     LSQ types of this module. This is similar to LM_DERV
    !!--++     In this case we use only the LSQ_conditions_type 'c' derived type.
    !!--++     The state vector is similar to V_vec in CFML_Refcodes.
    !!--++
    !!--++
    !!--++ Update: November 1 - 2013
    !!
    Module Subroutine LM_DifV(Model_Functn, m, c, V, Vstd, chi2, infout,residuals)
       !---- Arguments ----!
       Integer,                               Intent(In)      :: m
       type(LSQ_conditions_type),             Intent(In Out)  :: c
       Real (Kind=cp), dimension(:),          Intent(In Out)  :: V,Vstd
       Real (Kind=cp),                        Intent(out)     :: chi2
       character(len=*),                      Intent(out)     :: infout
       real (Kind=cp), dimension(:),optional, Intent(out)     :: residuals

       Interface
         Subroutine Model_Functn(m, n, x, fvec, iflag)
            Use CFML_GlobalDeps, Only: cp
            Integer,                     Intent(In)      :: m, n
            Real (Kind=cp),Dimension(:), Intent(In)      :: x
            Real (Kind=cp),Dimension(:), Intent(In Out)  :: fvec
            Integer,                     Intent(In Out)  :: iflag
         End Subroutine Model_Functn
       End Interface

       !--- Local Variables ---!
       Integer                                    :: i,j, maxfev, mode, nfev, nprint, n, &
                                                     iflag,info
       Integer,        dimension(c%npvar)         :: ipvt
       Integer,        dimension(c%npvar,c%npvar) :: p,id
       Real (Kind=cp), dimension(c%npvar,c%npvar) :: U,VS,Sigm
       Real (Kind=cp), dimension(c%npvar)         :: Sig
       Real (Kind=cp), dimension(m)               :: fvec  !Residuals
       Real (Kind=cp), dimension(m,c%npvar)       :: fjac
       Real (Kind=cp)                             :: epsfcn, gtol,iChi2,deni,denj, &
                                                     Tikhonov
       Real (Kind=cp), Parameter                  :: factor = 100.0_CP, zero = 0.0_CP

       info = 0
       n=c%npvar
       c%reached=.false.
       id=0
       do i=1,n
          id(i,i) = 1
       end do
       Chi2=1.0e35
       infout=" "

       !     check the input parameters for errors.
       If ( n <= 0 .OR. m < n .OR. c%tol < zero ) then
         write(unit=infout,fmt="(2a,i5,a,i5,a,f10.7)") "Improper input parameters in LM-optimization (n,m,tol):", &
                        " nb of free (refined) parameters: ", n, &
                        ", number of observations: ", m, &
                        ", tolerance: ", c%tol
          return
       end if

       ! Initial calculation of Chi2
       iflag=1
       epsfcn = zero
       Call Model_Functn(m, n, v, fvec, iflag)
       ichi2=norm2(fvec(1:m))
       if (ichi2 < 1.0e15) then
          ichi2=ichi2*ichi2/max(1.0_cp,real(m-n,kind=cp))
       end if
       If ( c%icyc < 5*n) then
          maxfev = 200*(n + 1)
          c%icyc= maxfev
       Else
          maxfev = c%icyc
       End if
       gtol = zero
       mode = 1
       nprint = c%nprint

       ! Call to the core procedure of the MINPACK implementation of the Levenberg-Marquardt algorithm
       Call lmdif(Model_Functn, m, n, V, fvec, c%tol, c%tol, gtol, maxfev, epsfcn,   &
           mode, factor, nprint, info, nfev, fjac, ipvt)

       !Calculate final Chi2 or norm
       chi2=norm2(fvec(1:m))
       if (chi2 < 1.0e15) then
          chi2=chi2*chi2/max(1.0_cp,real(m-n,kind=cp))
       end if
       Vstd=zero

       !Extract the curvature matrix side of equation below from fjac
       !        t     t           t
       !       p *(jac *jac)*p = r *r
       !           t            t     t
       ! cvm = (jac *jac) = p*(r *r)*p     -> Permutation matrices are orthogonal
       ! See the documentation of lmder1 and lmdif1 for why we use only the (n,n) submatrix of fjac
       !curv_mat(1:n,1:n)=matmul( transpose( fjac(1:n,1:n) ) , fjac(1:n,1:n) )  !this is Rt.R

        curv_mat(1:n,1:n)=matmul( transpose( Upper_Triangular(fjac,n)) , Upper_Triangular(fjac,n) )
        do j=1,n
           p(1:n,j) = id(1:n,ipvt(j))
        end do
        curv_mat(1:n,1:n) = matmul(  p, matmul( curv_mat(1:n,1:n),transpose(p) )  )
        correl(1:n,1:n)=Inverse_Matrix(curv_mat(1:n,1:n))
        !If the final curvature matrix is singular perform a Tikhonov
        !regularization (This increases the error bars!)
        Tikhonov=0.0
        if(Err_CFML%Ierr /=  0) then
          c%failed=.true.
          Err_CFML%Ierr = 1
          Err_CFML%Msg="Regularization (SVD,Tikhonov) of the final Curvature Matrix unsuccessfull (no standard deviations) ..."
          j=0
          do
            !first do a SVD decomposition
            U=curv_mat(1:n,1:n)
            call SVDcmp(U,Sig,VS)
            Tikhonov=maxval(Sig)*1.0e-6
            Sig=Sig+Tikhonov
            Sigm=0.0
            do i=1,n
               Sigm(i,i)=Sig(i)
            end do
            curv_mat(1:n,1:n)=matmul(U,matmul(Sigm,transpose(VS)))
            correl(1:n,1:n)= Inverse_Matrix(curv_mat(1:n,1:n))
            if(Err_CFML%Ierr ==  0) exit
            j=j+1
            if(j > 3) exit
          end do
          if(j <= 3) then
            Err_CFML%Msg="Regularization (SVD,Tikhonov) of the final Curvature Matrix OK! ... bigger Standard Deviations"
          end if
        end if
       !
       !Alternatively, make an additional direct final call to the model function in order to calculate the Jacobian J,
       !curvature matrix A=Jt.J, invert it and take the diagonal elements for calculating the standard
       !deviations at the final point.
       !iflag=2
       !Call Model_Functn(m, n, v, fvec, fjac, iflag)
       !curv_mat(1:n,1:n) = matmul (transpose(fjac),fjac) !Least squares matrix

       If (Err_CFML%Ierr ==  0) then
          !Here correl is the inverse of the curvature matrix (Variance-co-variance matrix)
          deni=max(chi2,1.0)
          Do i=1,n
             Vstd(i) = sqrt(deni*abs(correl(i,i)))
          End Do
          !Now correl is the true correlation matrix
          forall (i=1:n,j=1:n)
              correl(i,j)=correl(i,j)/sqrt(correl(i,i)*correl(j,j))
          end forall
       Else
          !Here correl is the "pseudo"-inverse of the curvature-matrix
          Do i=1,n
             if( deni <= zero) then
               Vstd(i) = 99999.9
             else
               Vstd(i) = sqrt(chi2*abs(correl(i,i)/deni))         !sqrt(abs(correl(i,i)*Chi2))
             end if
          End Do
          !Pseudo-correlation matrix
          do j=1,n
             denj=correl(j,j)
             if( denj <= zero) denj=1.0
             do i=1,n
                deni=correl(i,i)
                if( deni <= zero) deni=1.0
                correl(j,i)=correl(j,i)/sqrt(denj*deni)
             end do
          end do
       End if


       Select Case (info)
          Case(0)
             c%reached=.false.
          Case(1)
             c%reached=.true.
             write(unit=infout,fmt="(a,g12.5,a,e12.5)") "Tikhonov regularization:", Tikhonov, &
               "     Convergence reached: The relative error in the sum of squares is at most ",c%tol

          Case(2)
             c%reached=.true.
             write(unit=infout,fmt="(a,g12.5,a,e12.5)") "Tikhonov regularization:", Tikhonov,&
                "     Convergence reached: The relative error between x and the solution is at most ",c%tol

          Case(3)
            c%reached=.true.
             write(unit=infout,fmt="(a,g12.5,a,e12.5)") "Tikhonov regularization:", Tikhonov,&
             "     Convergence reached: The relative error "// &
             "in the sum of squares and the difference between x and the solution are both at most ",c%tol

          Case(4,8)
             c%reached=.true.
             write(unit=infout,fmt="(a,g12.5,a)") "Tikhonov regularization:", Tikhonov, &
              "     Convergence reached: Residuals vector is orthogonal to the columns of the Jacobian to machine precision"

          Case(5)
             c%reached=.false.
             write(unit=infout,fmt="(a,i6)") &
             "Convergence NOT reached: Number of calls to model function with iflag=1 has reached ",maxfev

          Case(6)
             c%reached=.false.
             write(unit=infout,fmt="(a,e12.5,a)") "Convergence NOT reached: Provided tolerance ",c%tol,&
                                            " is too small! No further reduction in the sum of squares is possible"
          Case(7)
             c%reached=.false.
             write(unit=infout,fmt="(a,e12.5,a)") "Convergence NOT reached: Provided tolerance ",c%tol,&
                                            " is too small! No further improvement in the approximate solution x seems to be possible"
       End Select
       if (present(residuals)) residuals(1:m)=fvec(1:m)

    End Subroutine LM_DifV
    !!--++
    !!--++   Subroutine lmdif(fcn, m, n, x, fvec, ftol, xtol, gtol, maxfev, epsfcn,  &
    !!--++                    mode, factor, nprint, info, nfev, fjac, ipvt)
    !!--++     INTEGER,                       INTENT(IN)    :: m
    !!--++     INTEGER,                       INTENT(IN)    :: n
    !!--++     REAL (kind=cp),dimension(:),   INTENT(IN OUT):: x
    !!--++     REAL (kind=cp),dimension(:),   INTENT(OUT)   :: fvec
    !!--++     REAL (kind=cp),                INTENT(IN)    :: ftol
    !!--++     REAL (kind=cp),                INTENT(IN)    :: xtol
    !!--++     REAL (kind=cp),                INTENT(IN OUT):: gtol
    !!--++     INTEGER,                       INTENT(IN OUT):: maxfev
    !!--++     REAL (kind=cp),                INTENT(IN OUT):: epsfcn
    !!--++     INTEGER,                       INTENT(IN)    :: mode
    !!--++     REAL (kind=cp),                INTENT(IN)    :: factor
    !!--++     INTEGER,                       INTENT(IN)    :: nprint
    !!--++     INTEGER,                       INTENT(OUT)   :: info
    !!--++     INTEGER,                       INTENT(OUT)   :: nfev
    !!--++     REAL (kind=cp),dimension(:,:), INTENT(OUT)   :: fjac     ! fjac(ldfjac,n)
    !!--++     INTEGER,dimension(:),          INTENT(OUT)   :: ipvt(:)
    !!--++
    !!--++     INTERFACE
    !!--++       SUBROUTINE fcn(m, n, x, fvec, iflag)
    !!--++         Use CFML_GlobalDeps, only: cp
    !!--++         INTEGER,                     INTENT(IN)      :: m, n
    !!--++         REAL (kind=cp),dimension(:), INTENT(IN)      :: x
    !!--++         REAL (kind=cp),dimension(:), INTENT(IN OUT)  :: fvec
    !!--++         INTEGER,                     INTENT(IN OUT)  :: iflag
    !!--++       END SUBROUTINE fcn
    !!--++     END INTERFACE
    !!--++
    !!--++
    !!--..  Original documentation
    !!--..
    !!--..  The purpose of lmdif is to minimize the sum of the squares of m nonlinear
    !!--..  functions in n variables by a modification of the Levenberg-Marquardt
    !!--..  algorithm.  The user must provide a subroutine which calculates the
    !!--..  functions.  The jacobian is then calculated by a forward-difference
    !!--..  approximation.
    !!--..
    !!--..  The subroutine statement is
    !!--..
    !!--..    subroutine lmdif(fcn, m, n, x, fvec, ftol, xtol, gtol, maxfev, epsfcn,
    !!--..                     diag, mode, factor, nprint, info, nfev, fjac,
    !!--..                     ldfjac, ipvt, qtf, wa1, wa2, wa3, wa4)
    !!--..
    !!--..  where
    !!--..
    !!--..    fcn       is the name of the user-supplied subroutine which calculates the
    !!--..              functions.  fcn must be declared in an external statement in the
    !!--..              user calling program, and should be written as follows.
    !!--..
    !!--..              subroutine fcn(m, n, x, fvec, iflag)
    !!--..                 integer m, n, iflag
    !!--..                 real (kind=cp) x(:), fvec(m)
    !!--..                 ----------
    !!--..                 calculate the functions at x and return this vector in fvec.
    !!--..                 ----------
    !!--..                 return
    !!--..              end
    !!--..
    !!--..              The value of iflag should not be changed by fcn unless the user
    !!--..              wants to terminate execution of lmdif. In this case set iflag to
    !!--..              a negative integer.
    !!--..
    !!--..    m         is a positive integer input variable set to the number of functions.
    !!--..
    !!--..    n         is a positive integer input variable set to the number of variables.
    !!--..              n must not exceed m.
    !!--..
    !!--..    x         is an array of length n.  On input x must contain an initial
    !!--..              estimate of the solution vector. On output x contains the final
    !!--..              estimate of the solution vector.
    !!--..
    !!--..    fvec      is an output array of length m which contains the functions evaluated
    !!--..              at the output x.
    !!--..
    !!--..    ftol      is a nonnegative input variable. Termination occurs when both the
    !!--..              actual and predicted relative reductions in the sum of squares are
    !!--..              at most ftol.  Therefore, ftol measures the relative error desired
    !!--..              in the sum of squares.
    !!--..
    !!--..    xtol      is a nonnegative input variable.  Termination occurs when both the
    !!--..              actual and predicted relative reductions in the sum of squares are
    !!--..              at most ftol.  Therefore, ftol measures the relative error desired
    !!--..              in the sum of squares.
    !!--..
    !!--..    gtol      is a nonnegative input variable.  Termination occurs when both the
    !!--..              actual and predicted relative reductions in the sum of squares are
    !!--..              at most ftol.  Therefore, ftol measures the relative error desired
    !!--..              in the sum of squares.
    !!--..
    !!--..    maxfev    is a positive integer input variable.  Termination occurs when the
    !!--..              number of calls to fcn is at least maxfev by the end of an iteration.
    !!--..
    !!--..    epsfcn    is an input variable used in determining a suitable step length
    !!--..              for the forward-difference approximation.  This approximation
    !!--..              assumes that the relative errors in the functions are of the order
    !!--..              of epsfcn.
    !!--..              If epsfcn is less than the machine precision, it is assumed that
    !!--..              the relative errors in the functions are of the order of the
    !!--..              machine precision.
    !!--..
    !!--..    diag      is an array of length n.  If mode = 1 (see below), diag is
    !!--..              internally set.  If mode = 2, diag must contain positive entries
    !!--..              that serve as multiplicative scale factors for the variables.
    !!--..
    !!--..    mode      is an integer input variable.  If mode = 1, the variables will be
    !!--..              scaled internally.  If mode = 2, the scaling is specified by the
    !!--..              input diag. other values of mode are equivalent to mode = 1.
    !!--..
    !!--..    factor    is a positive input variable used in determining the initial step
    !!--..              bound.  This bound is set to the product of factor and the euclidean
    !!--..              norm of diag*x if nonzero, or else to factor itself.  In most cases
    !!--..              factor should lie in the interval (.1,100.). 100. is a generally
    !!--..              recommended value.
    !!--..
    !!--..    nprint    is an integer input variable that enables controlled printing of
    !!--..              iterates if it is positive.  In this case, fcn is called with
    !!--..              iflag = 0 at the beginning of the first iteration and every nprint
    !!--..              iterations thereafter and immediately prior to return, with x and
    !!--..              fvec available for printing.  If nprint is not positive, no special
    !!--..              calls of fcn with iflag = 0 are made.
    !!--..
    !!--..    info      is an integer output variable.  If the user has terminated execution,
    !!--..              info is set to the (negative) value of iflag. See description of fcn.
    !!--..              Otherwise, info is set as follows.
    !!--..
    !!--..              info = 0  improper input parameters.
    !!--..
    !!--..              info = 1  both actual and predicted relative reductions
    !!--..                        in the sum of squares are at most ftol.
    !!--..
    !!--..              info = 2  relative error between two consecutive iterates <= xtol.
    !!--..
    !!--..              info = 3  conditions for info = 1 and info = 2 both hold.
    !!--..
    !!--..              info = 4  the cosine of the angle between fvec and any column of
    !!--..                        the Jacobian is at most gtol in absolute value.
    !!--..
    !!--..              info = 5  number of calls to fcn has reached or exceeded maxfev.
    !!--..
    !!--..              info = 6  ftol is too small. no further reduction in
    !!--..                        the sum of squares is possible.
    !!--..
    !!--..              info = 7  xtol is too small. no further improvement in
    !!--..                        the approximate solution x is possible.
    !!--..
    !!--..              info = 8  gtol is too small. fvec is orthogonal to the
    !!--..                        columns of the jacobian to machine precision.
    !!--..
    !!--..    nfev      is an integer output variable set to the number of calls to fcn.
    !!--..
    !!--..    fjac      is an output m by n array. the upper n by n submatrix of fjac
    !!--..              contains an upper triangular matrix r with diagonal elements of
    !!--..              nonincreasing magnitude such that
    !!--..
    !!--..                      t     t           t
    !!--..                     p *(jac *jac)*p = r *r,
    !!--..
    !!--..              where p is a permutation matrix and jac is the final calculated
    !!--..              Jacobian.  Column j of p is column ipvt(j) (see below) of the
    !!--..              identity matrix. the lower trapezoidal part of fjac contains
    !!--..              information generated during the computation of r.
    !!--..
    !!--..    ldfjac    is a positive integer input variable not less than m which i
    !!--..              specifies the leading dimension of the array fjac.
    !!--..
    !!--..    ipvt      is an integer output array of length n.  ipvt defines a permutation
    !!--..              matrix p such that jac*p = q*r, where jac is the final calculated
    !!--..              jacobian, q is orthogonal (not stored), and r is upper triangular
    !!--..              with diagonal elements of nonincreasing magnitude.
    !!--..              Column j of p is column ipvt(j) of the identity matrix.
    !!--..
    !!--..    qtf       is an output array of length n which contains the first n elements i
    !!--..              of the vector (q transpose)*fvec.
    !!--..
    !!--..    wa1...3   are work arrays of length n.
    !!--..
    !!--..    wa4       is a work array of length m.
    !!--..
    !!--..  Argonne National Laboratory. minpack project. march 1980.
    !!--..  Burton S. Garbow, Kenneth E. Hillstrom, Jorge J. More
    !!--++
    !!--++ Updated: February - 2009
    !!
    Subroutine lmdif(fcn, m, n, x, fvec, ftol, xtol, gtol, maxfev, epsfcn,  &
                     mode, factor, nprint, info, nfev, fjac, ipvt)
       !---- Arguments ----!
       Integer,                        Intent(In)    :: m
       Integer,                        Intent(In)    :: n
       Real (Kind=cp), Dimension(:),   Intent(In Out):: x
       Real (Kind=cp), Dimension(:),   Intent(Out)   :: fvec
       Real (Kind=cp),                 Intent(In)    :: ftol
       Real (Kind=cp),                 Intent(In)    :: xtol
       Real (Kind=cp),                 Intent(In Out):: gtol
       Integer,                        Intent(In Out):: maxfev
       Real (Kind=cp),                 Intent(In Out):: epsfcn
       Integer,                        Intent(In)    :: mode
       Real (Kind=cp),                 Intent(In)    :: factor
       Integer,                        Intent(In)    :: nprint
       Integer,                        Intent(Out)   :: info
       Integer,                        Intent(Out)   :: nfev
       Real (Kind=cp), Dimension(:,:), Intent(Out)   :: fjac     ! fjac(ldfjac,n)
       Integer,        Dimension(:),   Intent(Out)   :: ipvt(:)

       Interface
         Subroutine fcn(m, n, x, fvec, iflag)
            Use CFML_GlobalDeps, Only: cp
            Integer,                     Intent(In)      :: m, n
            Real (Kind=cp),Dimension(:), Intent(In)      :: x
            Real (Kind=cp),Dimension(:), Intent(In Out)  :: fvec
            Integer,                     Intent(In Out)  :: iflag
         End Subroutine fcn
       End Interface

       !--- Local Variables ---!
       Integer                     :: i, iflag, iter, j, l
       Real (Kind=cp)              :: actred, delta, dirder, epsmch, fnorm, fnorm1, gnorm,  &
                                      par, pnorm, prered, ratio, suma, temp, temp1, temp2, xnorm
       Real (Kind=cp), Dimension(n):: diag, qtf, wa1, wa2, wa3
       Real (Kind=cp), Dimension(m):: wa4
       Real (Kind=cp), Parameter   :: one = 1.0_CP, p1 = 0.1_CP, p5 = 0.5_CP,  &
                                      p25 = 0.25_CP, p75 = 0.75_CP, p0001 = 0.0001_CP, &
                                      zero = 0.0_CP
       Logical                     :: lmerror

       !     epsmch is the machine precision.
       epsmch = Epsilon(zero)
       lmerror=.false.
       info = 0
       iflag = 0
       nfev = 0

       !     check the input parameters for errors.
       If (n <= 0 .OR. m < n .OR. ftol < zero .OR. xtol < zero .OR. gtol < zero  &
           .OR. maxfev <= 0 .OR. factor <= zero) lmerror=.true.

       If (mode == 2 .and. .not. lmerror) Then
          Do j = 1, n
             If (diag(j) <= zero) Then
                lmerror=.true.
                exit
             End If
          End Do
       End If

       !     evaluate the function at the starting point and calculate its norm.
       if (.not. lmerror) then
          iflag = 1
          Call fcn(m, n, x, fvec, iflag)
          nfev = 1
          If (iflag < 0) lmerror=.true.
       end if

       if (.not. lmerror) then
          fnorm = norm2(fvec(1:m))

          !     initialize levenberg-marquardt parameter and iteration counter.
          par = zero
          iter = 1

          !     beginning of the outer loop.
          Do_out: Do  !

             !        calculate the jacobian matrix.
             iflag = 2
             Call fdjac2(fcn, m, n, x, fvec, fjac, iflag, epsfcn)
             nfev = nfev + n
             If (iflag < 0) Exit Do_out

             !        If requested, call fcn to enable printing of iterates.
             If (nprint > 0) Then
                iflag = 0
                If (Mod(iter-1,nprint) == 0) Call fcn(m, n, x, fvec, iflag)
                If (iflag < 0) Exit Do_out
             End If

             !        Compute the qr factorization of the jacobian.
             Call qrfac(m, n, fjac, .TRUE., ipvt, wa1, wa2)

             !        On the first iteration and if mode is 1, scale according
             !        to the norms of the columns of the initial jacobian.
             If (iter == 1) Then
                If (mode /= 2) Then
                   Do j = 1, n
                      diag(j) = wa2(j)
                      If (wa2(j) == zero) diag(j) = one
                   End Do
                End If

                !        On the first iteration, calculate the norm of the scaled x
                !        and initialize the step bound delta.
                wa3(1:n) = diag(1:n)*x(1:n)
                xnorm = norm2(wa3(1:n))
                delta = factor*xnorm
                If (delta == zero) delta = factor
             End If

             !        Form (q transpose)*fvec and store the first n components in qtf.
             wa4(1:m) = fvec(1:m)
             Do j = 1, n
                If (fjac(j,j) /= zero) Then
                   suma = Dot_Product( fjac(j:m,j), wa4(j:m) )
                   temp = -suma/fjac(j,j)
                   Do i = j, m
                      wa4(i) = wa4(i) + fjac(i,j)*temp
                   End Do
                End If
                fjac(j,j) = wa1(j)
                qtf(j) = wa4(j)
             End Do

             !        compute the norm of the scaled gradient.
             gnorm = zero
             If (fnorm /= zero) Then
                Do j = 1, n
                   l = ipvt(j)
                   If (wa2(l) == zero) Cycle
                   suma = zero
                   Do i = 1, j
                      suma = suma + fjac(i,j)*(qtf(i)/fnorm)
                   End Do
                   gnorm = Max(gnorm, Abs(suma/wa2(l)))
                End Do
             End If

             !        test for convergence of the gradient norm.
             If (gnorm <= gtol) info = 4
             If (info /= 0) Exit Do_out

             !        rescale if necessary.
             If (mode /= 2) Then
                Do j = 1, n
                   diag(j) = Max(diag(j), wa2(j))
                End Do
             End If

             !        beginning of the inner loop.
             Do_In: Do

                !           determine the Levenberg-Marquardt parameter.
                Call lmpar(n, fjac, ipvt, diag, qtf, delta, par, wa1, wa2)

                !           store the direction p and x + p. calculate the norm of p.
                Do j = 1, n
                   wa1(j) = -wa1(j)
                   wa2(j) = x(j) + wa1(j)
                   wa3(j) = diag(j)*wa1(j)
                End Do
                pnorm = norm2(wa3(1:n))

                !           on the first iteration, adjust the initial step bound.
                If (iter == 1) delta = Min(delta, pnorm)

                !           evaluate the function at x + p and calculate its norm.
                iflag = 1
                Call fcn(m, n, wa2, wa4, iflag)
                nfev = nfev + 1
                If (iflag < 0) Exit Do_Out
                fnorm1 = norm2(wa4(1:m))

                !           compute the scaled actual reduction.
                actred = -one
                If (p1*fnorm1 < fnorm) actred = one - (fnorm1/fnorm)**2

                !           Compute the scaled predicted reduction and
                !           the scaled directional derivative.
                Do j = 1, n
                   wa3(j) = zero
                   l = ipvt(j)
                   temp = wa1(l)
                   Do i = 1, j
                      wa3(i) = wa3(i) + fjac(i,j)*temp
                   End Do
                End Do
                temp1 = norm2(wa3(1:n))/fnorm
                temp2 = (Sqrt(par)*pnorm)/fnorm
                prered = temp1**2 + temp2**2/p5
                dirder = -(temp1**2 + temp2**2)

                !           compute the ratio of the actual to the predicted reduction.
                ratio = zero
                If (prered /= zero) ratio = actred/prered

                !           update the step bound.
                If (ratio <= p25) Then
                   If (actred >= zero) temp = p5
                   If (actred < zero) temp = p5*dirder/(dirder + p5*actred)
                   If (p1*fnorm1 >= fnorm .OR. temp < p1) temp = p1
                   delta = temp*Min(delta,pnorm/p1)
                   par = par/temp
                Else
                   If (.not. (par /= zero .AND. ratio < p75) ) Then
                      delta = pnorm/p5
                      par = p5*par
                   End If
                End If

                !           test for successful iteration.
                If (ratio >= p0001) Then

                   !           successful iteration. update x, fvec, and their norms.
                   Do j = 1, n
                      x(j) = wa2(j)
                      wa2(j) = diag(j)*x(j)
                   End Do
                   fvec(1:m) = wa4(1:m)
                   xnorm = norm2(wa2(1:n))
                   fnorm = fnorm1
                   iter = iter + 1
                End If

                !           tests for convergence.
                If (Abs(actred) <= ftol .AND. prered <= ftol .AND. p5*ratio <= one) info = 1
                If (delta <= xtol*xnorm) info = 2
                If (Abs(actred) <= ftol .AND. prered <= ftol .AND. p5*ratio <= one .AND. info == 2) info = 3
                If (info /= 0) Exit Do_out

                !           tests for termination and stringent tolerances.
                If (nfev >= maxfev) info = 5
                If (Abs(actred) <= epsmch .AND. prered <= epsmch .AND. p5*ratio <= one) info = 6
                If (delta <= epsmch*xnorm) info = 7
                If (gnorm <= epsmch) info = 8
                If (info /= 0) Exit Do_out

                !           end of the inner loop. repeat if iteration unsuccessful.
                If (ratio >= p0001) Exit Do_In
             End Do Do_in
             !        end of the outer loop.
          End Do  Do_out
       End If !.not. lmerror

       !     termination, either normal or user imposed.
       If (iflag < 0) info = iflag
       iflag = 0
       If (nprint > 0) Call fcn(m, n, x, fvec, iflag)

    End Subroutine lmdif

    !!--++
    !!--++  Module Subroutine lmdif1(fcn, m, n, x, fvec, tol, nprint, info, iwa)
    !!--++     Integer,                     Intent(In)      :: m
    !!--++     Integer,                     Intent(In)      :: n
    !!--++     Real (Kind=cp),Dimension(:), Intent(In Out)  :: x
    !!--++     Real (Kind=cp),Dimension(:), Intent(Out)     :: fvec
    !!--++     Real (Kind=cp),              Intent(In)      :: tol
    !!--++     Integer,                     Intent(In)      :: nprint
    !!--++     Integer,                     Intent(Out)     :: info
    !!--++     Integer,Dimension(:),        Intent(Out)     :: iwa
    !!--++
    !!--++     Interface
    !!--++       Subroutine fcn(m, n, x, fvec, iflag)
    !!--++         Use CFML_GlobalDeps, Only: cp
    !!--++         Integer,                     Intent(In)      :: m, n
    !!--++         Real (Kind=cp),Dimension(:), Intent(In)      :: x
    !!--++         Real (Kind=cp),Dimension(:), Intent(In Out)  :: fvec
    !!--++         Integer,                     Intent(In Out)  :: iflag
    !!--++       End Subroutine fcn
    !!--++     End Interface
    !!--++
    !!--..  Original documentation
    !!--..
    !!--..  The purpose of lmdif1 is to minimize the sum of the squares of m nonlinear
    !!--..  functions in n variables by a modification of the Levenberg-Marquardt
    !!--..  algorithm.  This is done by using the more general least-squares solver lmdif.
    !!--..  The user must provide a subroutine which calculates the functions.  The
    !!--..  jacobian is then calculated by a forward-difference approximation.
    !!--..
    !!--..  The subroutine statement is
    !!--..
    !!--..      subroutine lmdif1(fcn, m, n, x, fvec, tol, nprint, info, iwa)
    !!--..
    !!--..  where
    !!--..
    !!--..    fcn      is the name of the user-supplied subroutine which calculates
    !!--..             the functions.  fcn must be declared in an external statement in
    !!--..             the user calling program, and should be written as follows.
    !!--..
    !!--..             subroutine fcn(m, n, x, fvec, iflag)
    !!--..                integer m, n, iflag
    !!--..                real (kind=cp) x(n), fvec(m)
    !!--..                ----------
    !!--..                calculate the functions at x and return this vector in fvec.
    !!--..                ----------
    !!--..                return
    !!--..             end
    !!--..
    !!--..             The value of iflag should not be changed by fcn unless the user
    !!--..             wants to terminate execution of LM_Dif. In this case set iflag
    !!--..             to a negative integer.
    !!--..
    !!--..    m        is a positive integer input variable set to the number of functions.
    !!--..
    !!--..    n        is a positive integer input variable set to the number of variables.
    !!--..             n must not exceed m.
    !!--..
    !!--..    x        is an array of length n.  On input x must contain an initial
    !!--..             estimate of the solution vector.  On output x contains the final
    !!--..             estimate of the solution vector.
    !!--..
    !!--..    fvec     is an output array of length m which contains the functions evaluated
    !!--..             at the output x.
    !!--..
    !!--..    tol      is a nonnegative input variable.  Termination occurs when the
    !!--..             algorithm estimates either that the relative error in the sum of
    !!--..             squares is at most tol or that the relative error between x and the
    !!--..             solution is at most tol.
    !!--..
    !!--..    info     is an integer output variable.  If the user has terminated execution,
    !!--..             info is set to the (negative) value of iflag.  See description of
    !!--..             fcn. Otherwise, info is set as follows.
    !!--..
    !!--..             info = 0  improper input parameters.
    !!--..
    !!--..             info = 1  algorithm estimates that the relative error
    !!--..                       in the sum of squares is at most tol.
    !!--..
    !!--..             info = 2  algorithm estimates that the relative error
    !!--..                       between x and the solution is at most tol.
    !!--..
    !!--..             info = 3  conditions for info = 1 and info = 2 both hold.
    !!--..
    !!--..             info = 4  fvec is orthogonal to the columns of the
    !!--..                       jacobian to machine precision.
    !!--..
    !!--..             info = 5  number of calls to fcn has reached or exceeded 200*(n+1).
    !!--..
    !!--..             info = 6  tol is too small. no further reduction in
    !!--..                       the sum of squares is possible.
    !!--..
    !!--..             info = 7  tol is too small.  No further improvement in
    !!--..                       the approximate solution x is possible.
    !!--..
    !!--..    iwa      is an integer work array of length n.
    !!--..
    !!--..    wa       is a work array of length lwa.
    !!--..
    !!--..    lwa      is a positive integer input variable not less than m*n+5*n+m.
    !!--..
    !!--..  Argonne National Laboratory. minpack project. march 1980.
    !!--..  Burton S. Garbow, Kenneth E. Hillstrom, Jorge J. More
    !!--++
    !!--++ Update: February - 2009
    !!
    Module Subroutine lmdif1(Model_Functn, m, n, x, fvec, tol, nprint, info, iwa)
       !---- Argument ----!
       Integer,                     Intent(In)      :: m
       Integer,                     Intent(In)      :: n
       Real (Kind=cp),Dimension(:), Intent(In Out)  :: x
       Real (Kind=cp),Dimension(:), Intent(Out)     :: fvec
       Real (Kind=cp),              Intent(In)      :: tol
       Integer,                     Intent(In)      :: nprint
       Integer,                     Intent(Out)     :: info
       Integer,Dimension(:),        Intent(Out)     :: iwa

       Interface
         Subroutine Model_Functn(m, n, x, fvec, iflag)
            Use CFML_GlobalDeps, Only: cp
            Integer,                     Intent(In)      :: m, n
            Real (Kind=cp),Dimension(:), Intent(In)      :: x
            Real (Kind=cp),Dimension(:), Intent(In Out)  :: fvec
            Integer,                     Intent(In Out)  :: iflag
         End Subroutine Model_Functn
       End Interface

       !--- Local Variables ---!
       Integer                       :: maxfev, mode, nfev
       Real (Kind=cp)                :: epsfcn, ftol, gtol, xtol
       Real (Kind=cp),Dimension(m,n) :: fjac
       Real (Kind=cp), Parameter     :: factor = 100.0_CP, zero = 0.0_CP

       info = 0
       !     check the input parameters for errors.
       If (n <= 0 .OR. m < n .OR. tol < zero) Return
       !     call lmdif.
       maxfev = 200*(n + 1)
       ftol = tol
       xtol = tol
       gtol = zero
       epsfcn = zero
       mode = 1

       Call lmdif(Model_Functn, m, n, x, fvec, ftol, xtol, gtol, maxfev, epsfcn,   &
                  mode, factor, nprint, info, nfev, fjac, iwa)
       If (info == 8) info = 4

    End Subroutine lmdif1

    !!--++
    !!--++ Subroutine Fdjac2(Fcn, M, N, X, Fvec, Fjac, Iflag, Epsfcn)
    !!--++   Integer,                       Intent(In)      :: m
    !!--++   Integer,                       Intent(In)      :: n
    !!--++   Real (Kind=cp),dimension(n),   Intent(In Out)  :: x
    !!--++   Real (Kind=cp),dimension(m),   Intent(In)      :: fvec
    !!--++   Real (Kind=cp),dimension(:,:), Intent(Out)     :: fjac     ! fjac(ldfjac,n)
    !!--++   Integer,                       Intent(In Out)  :: iflag
    !!--++   Real (Kind=cp),                Intent(In)      :: epsfcn
    !!--++
    !!--++   Interface
    !!--++     Subroutine fcn(m, n, x, fvec, iflag)
    !!--++       Use CFML_GlobalDeps, Only: cp
    !!--++       Integer,                     Intent(In)      :: m, n
    !!--++       Real (Kind=cp),Dimension(:), Intent(In)      :: x
    !!--++       Real (Kind=cp),Dimension(:), Intent(In Out)  :: fvec
    !!--++       Integer,                     Intent(In Out)  :: iflag
    !!--++     End Subroutine fcn
    !!--++   End Interface
    !!--++
    !!--..   Original documentation:
    !!--..
    !!--..  This subroutine computes a forward-difference approximation to the m by n
    !!--..  jacobian matrix associated with a specified problem of m functions in
    !!--..  n variables.
    !!--..  The subroutine statement is
    !!--..
    !!--..    subroutine fdjac2(fcn,m,n,x,fvec,fjac,ldfjac,iflag,epsfcn,wa)
    !!--..
    !!--..  where
    !!--..
    !!--..    fcn   is the name of the user-supplied subroutine which calculates the
    !!--..          functions.  fcn must be declared in an external statement in the
    !!!-..+         user calling program, and should be written as follows.
    !!--..
    !!--..          subroutine fcn(m,n,x,fvec,iflag)
    !!--..             integer m,n,iflag
    !!--..             real (kind=cp) x(n),fvec(m)
    !!--..             ----------
    !!--..             calculate the functions at x and
    !!--..             return this vector in fvec.
    !!--..             ----------
    !!--..             return
    !!--..          end
    !!--..
    !!--..          the value of iflag should not be changed by fcn unless the user
    !!--..          wants to terminate execution of fdjac2. In this case set iflag
    !!--..          to a negative integer.
    !!--..
    !!--..    m     is a positive integer input variable set to the number of functions.
    !!--..
    !!--..    n     is a positive integer input variable set to the number of variables.
    !!--..          n must not exceed m.
    !!--..
    !!--..    x     is an input array of length n.
    !!--..
    !!--..    fvec  is an input array of length m which must contain the functions
    !!--..          evaluated at x.
    !!--..
    !!--..    fjac  is an output m by n array which contains the approximation to the
    !!--..          jacobian matrix evaluated at x.
    !!--..
    !!--..   ldfjac is a positive integer input variable not less than m which specifies
    !!--..          the leading dimension of the array fjac.
    !!--..
    !!--..    iflag is an integer variable which can be used to terminate the execution
    !!--..          of fdjac2.  see description of fcn.
    !!--..
    !!--..   epsfcn is an input variable used in determining a suitable step length
    !!--..          for the forward-difference approximation.  This approximation assumes
    !!--..          that the relative errors in the functions are of the order of epsfcn.
    !!--..          If epsfcn is less than the machine precision, it is assumed that the
    !!--..          relative errors in the functions are of the order of the machine
    !!--..          precision.
    !!--..
    !!--..    wa    is a work array of length m.
    !!--..
    !!--..
    !!--..  Argonne National Laboratory. minpack project. march 1980.
    !!--..  Burton S. Garbow, Kenneth E. Hillstrom, Jorge J. More
    !!--++
    !!--++
    !!--++ Update: February - 2009
    !!
    Subroutine Fdjac2(Fcn, M, N, X, Fvec, Fjac, Iflag, Epsfcn)
       !---- Arguments ----!
       Integer,                       Intent(In)      :: m
       Integer,                       Intent(In)      :: n
       Real (Kind=cp),dimension(n),   Intent(In Out)  :: x
       Real (Kind=cp),dimension(m),   Intent(In)      :: fvec
       Real (Kind=cp),dimension(:,:), Intent(Out)     :: fjac     ! fjac(ldfjac,n)
       Integer,                       Intent(In Out)  :: iflag
       Real (Kind=cp),                Intent(In)      :: epsfcn

       Interface
          Subroutine fcn(m, n, x, fvec, iflag)
             Use CFML_GlobalDeps, Only: cp
             Integer,                     Intent(In)      :: m, n
             Real (Kind=cp),Dimension(:), Intent(In)      :: x
             Real (Kind=cp),Dimension(:), Intent(In Out)  :: fvec
             Integer,                     Intent(In Out)  :: iflag
          End Subroutine fcn
       End Interface

       !---- Local Variables ----!
       Integer                   :: j
       Real (Kind=cp)            :: eps, epsmch, h, temp, wa(m)
       Real (Kind=cp), Parameter :: zero = 0.0_CP

       ! epsmch is the machine precision.
       epsmch = Epsilon(zero)

       eps = Sqrt(Max(epsfcn, epsmch))
       Do j = 1, n
          temp = x(j)
          h = eps*Abs(temp)
          If (h == zero) h = eps
          x(j) = temp + h
          Call fcn(m, n, x, wa, iflag)
          If (iflag < 0) Exit
          x(j) = temp
          fjac(1:m,j) = (wa(1:m) - fvec(1:m))/h
       End Do

    End Subroutine Fdjac2

 End Submodule OPT_LSQ_LevenbergMarquardt_NumDer