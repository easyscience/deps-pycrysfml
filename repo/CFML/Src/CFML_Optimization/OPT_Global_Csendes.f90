Submodule (CFML_Optimization) OPT_Global_Csendes
  implicit none
  contains
    !!----
    !!---- Module Subroutine Csendes_Global(Model_Functn,Mini, Maxi, Nparm, Nsampl, Nsel, Nsig, X0, Nc, F0, Ipr,mode)
    !!----    Model_Functn : is the dummy name of the objective function to be optimized.
    !!----    real(kind=cp), dimension(:),   intent(in)    :: mini   ! Vector of Length Nparm Containing The Lower Bounds
    !!----    real(kind=cp), dimension(:),   intent(in)    :: maxi   ! Vector of Length Nparm Containing The Upper Bounds
    !!----    integer,                       intent(in)    :: nparm  ! Number of Parameters
    !!----    integer,                       intent(in out):: Nsampl ! Number of Sample Points To Be Drawn Uniformly In One Cycle < 10000
    !!----                                                           ! Suggested value is 100*Nparm
    !!----    integer,                       intent(in out):: nsel   ! Number of Best Points Selected From The Actual Sample
    !!----                                                           ! The Suggested Value Is Twice The Expected Number Of Local Minima
    !!----    integer,                       intent(in)    :: nsig   ! Convergence Criterion
    !!----    real(kind=cp), dimension(:,:), intent(in out):: x0     ! Output (up to Npar x Nc) Matrix Containing Nc Local Minimizers Found.
    !!----    integer,                       intent(out)   :: nc     ! Number of Different Local Minimizers Found
    !!----    real(kind=cp), dimension(:),   intent(in out):: f0     ! Output Vector Of Nc  Objective Function Values,
    !!----                                                           ! F0(I) Belongs To The Parameters X0(1,I), X0(2,I), ..., X0(Nparm,I)
    !!----    integer,                       intent(in)    :: ipr    ! Printing Information
    !!----    integer, optional,             intent(in)    :: mode   ! if present the routine Local_DFP is replaced by Local_Rand
    !!----
    !!--<<    Interface
    !!----       Subroutine Model_Functn(Nparm,X, F)
    !!----          real(kind=cp),dimension(:), intent(in)  :: x
    !!----          real(kind=cp),              intent(out) :: f
    !!----          integer,                    intent(in)  :: nparm
    !!----       End Subroutine Model_Functn
    !!----    End Interface
    !!-->>
    !!----
    !!----    Global optimization using the Boender-Timmer-Rinnoy Kan algorithm
    !!----
    !!--..    GLOBAL MINIMUM OF FUNCTION OF N VARIABLES USING A LOCAL SEARCH METHOD
    !!--..
    !!--..    Information
    !!--..
    !!--..    Convergence criterium: The accuracy required in the parameter estimates.
    !!--..    this convergence criterion is satisfied if on two successive iterations
    !!--..    the parameter estimates agree, component by component, to nsig digits.
    !!--..    the suggested value is 6.
    !!--..
    !!--..        SHORT DESCRIPTION OF THE GLOBAL OPTIMIZATION SUBROUTINE
    !!--..        -------------------------------------------------------
    !!--..        (Based in the original explanations by Tibor Csendes)
    !!--..
    !!--..   Global optimization is a part of nonlinear optimization, it deals with
    !!--..   problems with (possibly) several local minima. The presented method is
    !!--..   stochastic (i.e. not deterministic). The framework procedure, the GLOBAL
    !!--..   routine gives a computational evidence, that the best local minimum found is
    !!--..   with high probability the global minimum.  This routine calls a local search
    !!--..   routine, and a routine for generating random numbers.
    !!--..
    !!--..   Let F(X) be a real function of NPARM parameters and we are looking for
    !!--..   parameter values X(I) from the given intervals [MIN(I), MAX(I)] for each
    !!--..   I = 1, 2, ..., NPARM.  The problem is to determine such a point X*, that the
    !!--..   function value F(X) is greater than or equal to F(X*) for every X in the
    !!--..   NPARM-dimensional interval specified by MIN(I)'s and MAX(I)'s.
    !!--..
    !!--..   The given version allows 15 parameters to be optimized.  For modifying the
    !!--..   program to accept larger problems change the numbers 15 everywhere in the
    !!--..   source lines to the new value N, and change 120 in a declaration to N(N+1).
    !!--..
    !!--..   I. THE ALGORITHM
    !!--..   ----------------
    !!--..
    !!--..   The algorithm consists of the following steps:
    !!--..
    !!--..   0. step: initialization of the parameters of the algorithm.  X0 is the set of
    !!--..            the local minima found, F0 contains corresponding values of the
    !!--..            objective function.  The local minima are ordered increasingly
    !!--..            according to the function values.  X1 is the set of points starting
    !!--..            from which the local search procedure led to a local minimum.  These
    !!--..            points are called seed points, and as such they are used as seed
    !!--..            points in the clustering phase.  At the start the mentioned sets are
    !!--..            empty.  The subroutine checks the parameter bounds, and gives error
    !!--..            message, and stops if they are not correct, or if they do not meet
    !!--..            the requirements.
    !!--..
    !!--..   1. step: generate sample points with uniform distribution, and add them to the
    !!--..            sample.  The number of generated points is specified by NSAMPL.
    !!--..
    !!--..   2. step: generate the actual sample, as the best 100 * NSEL/NSAMPL percentage
    !!--..            of the sample points generated so far (according to the objective
    !!--..            function values).  This actual sample contains in general NSEL more
    !!--..            points than the previous one.
    !!--..
    !!--..   3. step: form clusters in the actual sample by Single Linkage method, by
    !!--..            growing the clusters first around the seed points (elements of the
    !!--..            sets X0 and X1).  A new point will join a cluster, if there is a point
    !!--..            in the cluster with which the distance is less than a critical
    !!--..            distance calculated automatically by the program for the given
    !!--..            problem, and if the point in the cluster has a smaller objective
    !!--..            function value than that of the considered one.  The critical distance
    !!--..            depends on the number of points in the whole sample, and on the
    !!--..            dimension of the problem NPARM.  If all points of the actual sample
    !!--..            are successfully ordered to some of the existing clusters, then comes
    !!--..            the 5th step.
    !!--..
    !!--..   4. step: start local search from the actual sample points not yet clustered in
    !!--..            ascending order by the values of the respective function values. If
    !!--..            the result of the local search is close to an element of the sets X0
    !!--..            and X1, then the starting point will be added to the set X1.  If the
    !!--..            result of the local search cannot be clustered to any of the existing
    !!--..            clusters then the point is regarded as a new local minimizer, and is
    !!--..            added to X0.  Choose now this result point of the local search to be a
    !!--..            seed point, and try to find (not clustered) points in the current
    !!--..            sample that can be clustered to this one.  If a new local minimum was
    !!--..            found in step 4, go to the step 1.
    !!--..
    !!--..   5. step: determine the element of the set X0 with the smallest function value.
    !!--..            This is the candidate of the program for the global minimizer.  Stop.
    !!--..
    !!--..
    !!--..   The presented program is a modification of the algorithm by Boender et al.
    !!--..   (see [1]).  The following changes were made.
    !!--..
    !!--..   1. The local search procedure (Local_DFP) is an algorithm of Quasi-Newton type
    !!--..      which uses the so called DFP (Davidon-Fletcher-Powell) update formula.
    !!--..      The comparison of this local search method with others can be found in [2].
    !!--..      For smooth objective functions this seems to be a good choice, for problems
    !!--..      with discontinuous objective function or derivatives the robust random search
    !!--..      method UNIRANDI (called here Local_RAND) (more details in [5]) can be recommended.
    !!--..
    !!--..   2. The subroutine GLOBAL will automatically scale the parameters to be
    !!--..
    !!--..      optimized (as in [3]) in order to keep all the starting points of the
    !!--..      parameters between -1 and 1 for the local minimizer procedure.  The user
    !!--..      does not have to care about the scaling, because the results are
    !!--..      transformed back to the original interval before each output, and before
    !!--..      giving the control back to the calling program.
    !!--..
    !!--..   3. We have also made use of later results [6] on this method.  For example, the
    !!--..      condition used at clustering has been changed to another one.
    !!--..
    !!--..   4. Instead of the Euclidean distance the greatest difference in absolute
    !!--..      values is used in our GLOBAL routine.  This change leads to a simpler and
    !!--..      quicker version of the algorithm.
    !!--..
    !!--..   II. HOW TO CALL THE SUBROUTINE GLOBAL
    !!--..   -------------------------------------
    !!--..
    !!--.. Call Csendes_Global(Model_Functn,Mini, Maxi, Nparm, Nsampl, Nsel, Nsig, X0, Nc, F0, Ipr,mode)
    !!--..
    !!--..   MINi and MAXi are real vectors (input) of NPARM elements.  They specify an
    !!--..   NPARM-dimensional interval that contains the starting points for the local
    !!--..   searches, i.e. the Ith coordinate of a random sample point, X(I) is always in
    !!--..   [MINi(I), MAXi(I)].  The values of this vector will not be changed during the run
    !!--..   of the GLOBAL subroutine.
    !!--..
    !!--..   NPARM is an integer constant (input) containing the number of the parameters
    !!--..   to be optimized.  The value will not be changed during the run of GLOBAL.
    !!--..
    !!--..   NSAMPL is an integer constant (input) containing the number of the sampling
    !!--..   points to be generated in the parameter space with uniform distribution in one
    !!--..   loop.  The value will not be changed during the run of the GLOBAL subroutine.
    !!--..
    !!--..   NSEL is an integer constant (input).  In one sampling the number of points
    !!--..   selected for starting points of local search routine is NSEL.  These points are
    !!--..   those with the smallest objective function value.  The value will not be
    !!--..   changed by the GLOBAL routine.
    !!--..
    !!--..   IPR is an integer constant (input), the FORTRAN logical file number for the
    !!--..   output file. The value will not be changed during the GLOBAL run.  The user
    !!--..   should take care on the corresponding OPEN statement in the main program.  In
    !!--..   the IBM-PC version of the GLOBAL routine each output of the routine will be
    !!--..   repeated for the default unit (the screen).
    !!--..
    !!--..   NSIG is an integer constant (input), a parameter for the stopping criterion of
    !!--..   the local search algorithm.  If the value of the objective function is the same
    !!--..   in the last two steps in the first NSIG significant digits, then the local
    !!--..   search will be cancelled.  The value of NSIG will not be changed during the run
    !!--..   of GLOBAL.
    !!--..
    !!--..   X0 is a two-dimensional array of 15 * 20 real elements (output).  After the run
    !!--..   of GLOBAL this matrix contains the parameters of the local minimizer points
    !!--..   found: the J-th local minimizer vector is stored in X0(I,J) I=1,2,...,NPARM.
    !!--..   These minimizers are ordered in ascending order according to their objective
    !!--..   function values.  X0 will be set by GLOBAL.
    !!--..
    !!--..   NC is an integer constant (output) containing the number of the local minima
    !!--..   found. This constant will be set by the GLOBAL routine.
    !!--..
    !!--..   F0 is a real array of 20 elements (output) that contains the objective
    !!--..   function values of the respective local minimizer points.  Thus F(X0(.,J)) =
    !!--..   F0(J).  F0 will be set by GLOBAL.
    !!--..
    !!--..   MODE is an optional integer argument, if present the subroutine Local_RAND is
    !!--..   selected instead of the Local_DFP procedure.
    !!--..
    !!--..
    !!--..   III. THE FOLLOWING SUBROUTINES ARE CALLED BY THE SUBROUTINE GLOBAL:
    !!--..   -------------------------------------------------------------------
    !!--..
    !!--..   FUN, LOCAL_DFP/LOCAL_RAND, UPDATE, Model_Functn, TIMER
    !!--..
    !!--..   The user has to provide only the routine Model_Functn (see later) out of the
    !!--..   mentioned.
    !!--..
    !!--..   The starting value for the random number generation is given by the TIMER
    !!--..   routine.  This starting value is based on the computer clock, hence it is
    !!--..   expected to be different for all runs.  The routine TIMER is written in the
    !!--..   ASSEMBLER language of the IBM-PC (MASM).  It can be changed to another routine
    !!--..   that asks the user what should be the starting number - if it is preferred.
    !!--..
    !!--..   FUN transforms the scaled variables back before calling the Model_Functn routine.  It
    !!--..   is provided together with the GLOBAL routine.  Thus, the user can write the
    !!--..   Model_Functn routine according to the usual parameter space.
    !!--..
    !!--..   The subroutines LOCAL and UPDATE contain the Quasi-Newton local search method.
    !!--..   If a random search algorithm is preferred, the routine UNIRANDI should be
    !!--..   linked, it has the same calling form as LOCAL.
    !!--..
    !!--..   IV. THE SUBROUTINE Model_Functn FOR COMPUTING THE OBJECTIVE FUNCTION
    !!--..   --------------------------------------------------------------------
    !!--..
    !!--..   The calling sequence is:
    !!--..
    !!--..                     CALL Model_Functn (NPARM,X, F)
    !!--..
    !!--..   where the parameters should be understood as follows:
    !!--..
    !!--..   X is a real vector of NPARM elements (input) containing the values of the
    !!--..   parameters, i.e. the co-ordinates of the point in the parameter space.  The
    !!--..   variables X(I) are now not scaled.  (They are transformed back to the interval
    !!--..   given by the user.)  The vector X must not be changed in the routine Model_Functn.
    !!--..
    !!--..   F is a real constant (output).  This variable has to contain the computed
    !!--..   objective function value after returning from the routine FUNCT.
    !!--..
    !!--..   If there are more necessary information besides the values of X, and NPARM
    !!--..   for the routine Model_Functn, then they can be passed through global module variables.
    !!--..
    !!--..
    !!--..   V. LIMITATIONS
    !!--..   --------------
    !!--..
    !!--..   parameter           limitation               otherwise
    !!--..
    !!--..   ------------------------------------------------------------------------------
    !!--..
    !!--..   MIN(I)              MIN(I) = MAX(I)          error message, interrupt
    !!--..
    !!--..   MAX(I)              MIN(I) = MAX(I)          error message, interrupt
    !!--..
    !!--..   NPARM               1 <= NPARM <= 15         error message, interrupt
    !!--..
    !!--..   NSAMPL              20 <= NSAMPL <= 10000    the value of NSAMPL is changed to
    !!--..                                                20 or 10000 whichever is closer
    !!--..                                                to the previous value
    !!--..
    !!--..   NSEL                1 <= NSEL <= 20          the value of NSEL is changed to 1
    !!--..                                                or 20 whichever is closer to the
    !!--..                                                previous value
    !!--..
    !!--..   VI. INPUT - OUTPUT
    !!--..   ------------------
    !!--..
    !!--..   Input is a reponsability of the user. The program writes to the
    !!--..   given logical device (specified by IPR).  This output gives a document of
    !!--..   the run, and provides a list of the local minima found.
    !!--..
    !!--..
    !!--..   VII. THE SUGGESTED VALUES FOR THE PARAMETERS OF THE ALGORITHM
    !!--..   -------------------------------------------------------------
    !!--..
    !!--..   The filling out of the arrays MIN and MAX does not mean generally any problem.
    !!--..   The program accepts the input parameters even if MAX(I) is less than MIN(I).
    !!--..   However, if MIN(I) = MAX(I) the program halts with an error message.  According
    !!--..   to this description of the algorithm, the sampling points are generated in the
    !!--..   NPARM-dimensional interval given by vectors MIN and MAX.  However, the local
    !!--..   search may leave this interval.  If the given limits are ment as strict limits,
    !!--..   the objective function should be modified to incorporate some form of penalty
    !!--..   for points outside the interval.
    !!--..
    !!--..   NSAMPL and NSEL can affect the reliability of the GLOBAL subroutine.  If NSAMPL
    !!--..   = NSEL, then the subroutine corresponds to the so-called Multiply Starts
    !!--..   method (usually it is not sufficiently efficient).  It is worth to choose
    !!--..   NSAMPL to be at least as large as the number of function evaluations used by a
    !!--..   single local search. The smaller the NSEL/NSAMPL ratio, the smaller can be the
    !!--..   region of attraction of the global minimum.  Thus, decreasing this ratio will
    !!--..   cause the GLOBAL routine to be more reliable.
    !!--..
    !!--..   It is not worth to give small value for NSIG.  Take into account that the local
    !!--..   search procedure is capable to determine the location of the local minimum
    !!--..   only to that extent what is allowed by the objective function.  As a minimum, 6
    !!--..   is suggested for a value of NSIG. The clustering phase of the GLOBAL routine
    !!--..   is more effective if the local minima are well approximated by the local
    !!--..   search procedure.
    !!--..
    !!--..
    !!--..   VIII. SAMPLE PROGRAM TO SHOW THE USAGE OF THE GLOBAL SUBROUTINE ON PC-S
    !!--..   -----------------------------------------------------------------------
    !!--..
    !!--..         REAL X0(15,20), F0(20), MIN(15), MAX(15)
    !!--..
    !!--..         M = 1
    !!--..         NPARM = 1
    !!--..         NSAMPL = 100
    !!--..         NSEL = 2
    !!--..         IPR = 6
    !!--..
    !!--..         OPEN(6, FILE='OUTPUT')
    !!--..
    !!--..         NSIG = 6
    !!--..         MIN(1) = -100.0
    !!--..         MAX(1) =  100.0
    !!--..
    !!--..         CALL GLOBAL(MIN, MAX, NPARM, NSAMPL, NSEL, IPR, NSIG, X0, NC, F0)
    !!--..
    !!--..         END
    !!--..
    !!--..
    !!--..
    !!--..         SUBROUTINE Model_Functn(NPARM, X, VALUE)
    !!--..
    !!--..         DIMENSION X(:)
    !!--..
    !!--..         VALUE = 1.0 - COS(X(1)) + (X(1)/100.0)**2
    !!--..
    !!--..         RETURN
    !!--..         SUBROUTINE Model_Functn
    !!--..
    !!--..   The given test example is a one-dimensional problem, it has several local
    !!--..   minima.  The global minimum is 0, and the objective function reaches this value
    !!--..   at the point x(1) = 0.0. The test program found 5 local minima in 48 sec. -
    !!--..   among them the global one - with 523 function evaluations.  Note, that if the
    !!--..   NSEL/NSAMPL ratio is too small, then the local minima with relatively large
    !!--..   objective function values cannot be found by the program.  This feature is
    !!--..   useful if we are interested mainly only in the global minimum.
    !!--..
    !!--..   Dr. Tibor Csendes,
    !!--..   Institute of Informatics, Jozsef Attila University
    !!--..   H-6701 Szeged, Pf. 652, Hungary
    !!--..   E-mail: csendes@inf.u-szeged.hu
    !!--..
    !!--..   URL: http://www.inf.u-szeged.hu/~csendes/
    !!--..
    !!--..   You can find additional important information in the comments of the source codes.
    !!--..
    !!--..
    !!--..
    !!--..   REFERENCES
    !!--..   ----------
    !!--..
    !!--..   1. Boender, C.G.E., A.H.G. Rinnooy Kan, G.T. Timmer, L. Stougie: A stochastic
    !!--..      method for global optimization, Mathematical Programming 22(1982) 125-140.
    !!--..
    !!--..   2. Gill, P.E., W. Murray, M.H. Wright: Practical Optimization (Academic Press,
    !!--..      London, 1981).
    !!--..
    !!--..   3. Csendes, T., B. Daroczy, Z. Hantos: Nonlinear parameter estimation by
    !!--..      global optimization: comparison of local search methods in respiratory
    !!--..      system modelling, in: System Modelling and Optimization (Springer-Verlag,
    !!--..      Berlin, 1986) 188-192.
    !!--..
    !!--..   4. Csendes, T.: Nonlinear parameter estimation by global optimization -
    !!--..      Efficiency and reliability, Acta Cybernetica 8(1988) 361-370.
    !!--..
    !!--..   5. Jarvi, T.: A random search optimizer with an application to a maxmin
    !!--..      problem, Publications of the Inst. of Appl. Math., Univ. of Turku, No. 3,
    !!--..      1973.
    !!--..
    !!--..   6. Timmer, G.T.: Global optimization: a stochastic approach, (Ph.D. Thesis,
    !!--..      Erasmus University, Rotterdam, 1984).
    !!--..
    !!---- Update: February - 2005
    !!
    Module Subroutine Csendes_Global(Model_Functn, Mini, Maxi, Nparm, Nsampl, Nsel, Nsig, X0, Nc, F0, Ipr,mode)
       !---- Arguments ----!
       real(kind=cp), dimension(:),  intent(in)      :: mini    !Nparm
       real(kind=cp), dimension(:),  intent(in)      :: maxi    !Nparm
       integer,                      intent(in)      :: nparm
       integer,                      intent(in out)  :: Nsampl  !100*nparm
       integer,                      intent(in out)  :: nsel    ! 2times number of expected local minima
       integer,                      intent(in)      :: nsig
       real(kind=cp), dimension(:,:),intent(in out)  :: x0      ! nparm x Nc
       integer,                      intent(out)     :: nc
       real(kind=cp), dimension(:),  intent(in out)  :: f0      !Nc
       integer,                      intent(in)      :: ipr
       integer, optional,            intent(in)      :: mode

       Interface
          Subroutine Model_Functn(n,x,f,g)
             use CFML_GlobalDeps,  only: cp
             integer,                             intent(in) :: n
             real(kind=cp),dimension(:),          intent(in) :: x
             real(kind=cp),                       intent(out):: f
             real(kind=cp),dimension(:),optional, intent(out):: g
          End Subroutine Model_Functn
       End Interface

       !---- Local Variables ----!
       logical                  :: new_locmin=.false.
       integer                  :: i, i1, icc, icj, ig, ii, iii, im, in1, inum, inum1, inum2, it, iv, &
                                   j, jj, l1, maxfn, n, n0, n1, ncp, nfe, nfe1, ng, ng10, nm, nn100, ns

       integer, dimension(100)       :: ic
       integer, dimension(size(f0))  :: ic1

       real(kind=cp)                             :: a,  b  , alfa, b1, bb, fc, ff, fm, relcon
       real(kind=cp), dimension(nparm,100)       :: x, xcl
       real(kind=cp), dimension(nparm,size(f0))  :: x1
       real(kind=cp), dimension(100,nparm)       :: r
       real(kind=cp), dimension(nparm)           :: w, y, mmin, mmax
       real(kind=cp), dimension(100)             :: f,  fcl
       real(kind=cp), dimension(size(f0))        :: f1
       real(kind=cp), parameter                  :: zero = 0.0_cp, one = 1.0_cp, two = 2.0_cp, ten = 10.0_cp

       if (nparm <= 0) then
          Err_CFML%Ierr=1
          Err_CFML%Msg="  ERROR: Negative number of parameters! "
          write(unit=ipr,fmt="(a)")   Err_CFML%Msg
          return
       end if

       do i = 1, nparm
          mmin(i) = mini(i)
          mmax(i) = maxi(i)
          if (mmin(i) == mmax(i)) then
             Err_CFML%Ierr=1
             write(unit=Err_CFML%Msg,fmt="(a,i3)")"  ERROR: Minimum=Maximum, for parameter #",i
             write(unit=ipr,fmt="(a)")   Err_CFML%Msg
             return
          end if
       end do

       b1 = one / real(nparm)
       if ( nsel <  1)     nsel = 1
       if ( nsel > 20)     nsel = 20
       if (Nsampl < 20)    Nsampl = 20
       if (Nsampl > 10000) Nsampl = 10000
       if (Nsampl < 100) then
          nn100 = Nsampl
          n = 1
       else
          nn100 = 100
          n = Nsampl / 100
          Nsampl = n * 100
       end if
       ng10 = 100
       do i = 1, ng10
          f(i)  = 9.9e10
          ic(i) = 0
          fcl(i)= 0.0
       end do
       do i = 1, nparm
          mmax(i) = (mmax(i)-mmin(i)) / two
          mmin(i) = mmin(i) + mmax(i)
       end do
       alfa = 0.01
       nfe = 0
       ng = 0
       ns = 0
       nc = 0
       ncp = 1
       n0 = 0
       n1 = 0
       im = 1
       ig = 0
       fm = 9.9E10
       maxfn = 500 * nparm
       relcon = ten ** (-nsig)

       !----  SAMPLING ----!
       do_main: DO      !Main infinite loop controlled by variable "it"
          n0 = n0 + Nsampl
          nm = n0 - 1
          ng = ng + nsel
          ns = ns + 1
          if (ns*nsel > 100) then
             write(unit=ipr,fmt="(a)") " ***   Too many sampling"
             exit do_main
          end if

          b = (one - alfa**(one/real(nm))) ** b1
          bb = 0.1 * b
          do i1 = 1, n
             call random_number(r)   !generates 100 random vectors r(100,nparm)
             do j = 1, nn100
                do i = 1, nparm
                   y(i) = two * r(j,i) - one
                end do
                call fun(y, fc, nparm, mmin, mmax,Model_Functn)
                if (fc < fm) then
                   f(im) = fc
                   do i = 1, nparm
                      x(i,im) = y(i)
                   end do
                   if (im <= ng .and. ic(im) > 0) ig = ig - 1
                   ic(im) = 0
                   im = 1
                   fm = f(1)
                   do i = 2, ng10
                      if (f(i) >= fm) then
                         im = i
                         fm = f(i)
                      end if
                   end do
                end if
             end do
          end do
          nfe = nfe + Nsampl
          write(unit=ipr,fmt="(/,a,i5,a)") " ", Nsampl," Function Evaluations Used For Sampling"

          !---- SORTING ----!
          inum = ng10 - 1
          do i = 1, inum
             im = i
             fm = f(i)
             inum1 = i + 1
             do j = inum1, ng10
                if (f(j) < fm) then
                   im = j
                   fm = f(j)
                end if
             end do
             if (im > i) then
                a = fm
                do j = 1, nparm
                   y(j) = x(j,im)
                end do
                if (i <= ng .and. im > ng) then
                   if (ic(ng) == 0 .and. ic(im)  > 0) ig = ig + 1
                   if (ic(ng)  > 0 .and. ic(im) == 0) ig = ig - 1
                end if
                icc = ic(im)
                inum1 = im - i
                do j = 1, inum1
                   inum2 = im - j
                   f(inum2+1) = f(inum2)
                   ic(inum2+1) = ic(inum2)
                   do jj = 1, nparm
                      x(jj,inum2+1) = x(jj,inum2)
                   end do
                end do
                f(i) = a
                do j = 1, nparm
                   x(j,i) = y(j)
                end do
                ic(i) = icc
             end if
          end do

          if (nc > 0) then
             !---- CLUSTERING TO    X*
             do iii = 1, nc
                i = 1
                in1 = i
                fcl(i) = f0(iii)
                do j = 1, nparm
                   xcl(j,i) = x0(j,iii)
                end do
                do j = 1, ng
                   if (ic(j) == iii) then
                      in1 = in1 + 1
                      xcl(1:nparm,in1) = x(1:nparm,j)
                   end if
                end do

                do    ! while i <= in1
                   do j = 1, ng
                      if (ic(j) == 0) then
                         if (fcl(i) < f(j)) then
                            do l1 = 1, nparm
                               w(l1) = abs(xcl(l1,i)-x(l1,j))
                            end do
                            a = zero
                            do l1 = 1, nparm
                               if (w(l1) > a) a = w(l1)
                            end do
                            if (a < b) then
                               write(unit=ipr,fmt="(a,i2)") " Sample point added to the cluster # ",iii
                               do ii = 1, nparm
                                  w(ii) = x(ii,j) * mmax(ii) + mmin(ii)
                               end do
                               write(unit=ipr,fmt="(tr1,f14.8,3(/,t5, 5(f14.8,tr1)))") f(j), (w(ii),ii = 1,nparm)
                               ig = ig + 1
                               if (ig >= ng) exit do_main
                               in1 = in1 + 1
                               fcl(in1) = f(j)
                               do ii = 1, nparm
                                  xcl(ii,in1) = x(ii,j)
                               end do
                               ic(j) = iii
                            end if
                         end if
                      end if
                   end do
                   i = i + 1
                   if (i > in1) exit
                end do
             end do

             if (n1 > 0) then
                !---- CLUSTERING TO    X1
                do iii = 1, n1
                   i = 1
                   in1 = i
                   fcl(i) = f1(iii)
                   do j = 1, nparm
                      xcl(j,i) = x1(j,iii)
                   end do
                   do   ! while i <= in1
                      do j = 1, ng
                         if (ic(j) == 0) then
                            if (fcl(i) < f(j)) then
                               do l1 = 1, nparm
                                  w(l1) = abs(xcl(l1,i)-x(l1,j))
                               end do
                               a = zero
                               do l1 = 1, nparm
                                  if (w(l1) > a) a = w(l1)
                               end do
                               if (a < b) then
                                  write(unit=ipr,fmt="(a,i2)") " Sample point added to the cluster # ",ic1(iii)
                                  do ii = 1, nparm
                                     w(ii) = x(ii,j) * mmax(ii) + mmin(ii)
                                  end do
                                  write(unit=ipr,fmt="(tr1,f14.8,3(/,t5, 5(f14.8,tr1)))") f(j), w(1:nparm)
                                  ig = ig + 1
                                  if (ig >= ng) exit do_main
                                  in1 = in1 + 1
                                  fcl(in1) = f(j)
                                  do ii = 1, nparm
                                     xcl(ii,in1) = x(ii,j)
                                  end do
                                  ic(j) = ic1(iii)
                               end if
                            end if
                         end if
                      end do
                      i = i + 1
                      if (i > in1) exit
                   end do
                end do
             end if
          end if

          !---- LOCAL SEARCH ----!
          it = 0
          do i1 = 1, ng
             if (ic(i1) == 0) then
                y(1:nparm) = x(1:nparm,i1)
                ff = f(i1)
                if(present(mode)) then
                  call local_rand(nparm, relcon, maxfn, y, ff, nfe1, mmin, mmax,Model_Functn)
                else
                  call local_dfp (nparm, relcon, maxfn, y, ff, nfe1, mmin, mmax,Model_Functn)
                end if
                new_locmin=.true.
                if (nc > 0) then
                   do iv = 1, nc
                      do l1 = 1, nparm
                         w(l1) = abs(x0(l1,iv) - y(l1))
                      end do
                      a = zero
                      do l1 = 1, nparm
                         if (w(l1) > a) a = w(l1)
                      end do
                      if (a < bb) then
                         !---- new seed-point
                         n1 = n1 + 1
                         write(unit=ipr,fmt="(a,i2,a,i5)") " New Seed Point Added To The Cluster No. ",iv,", NFEV=", nfe1
                         do ii = 1, nparm
                            w(ii) = x(ii,i1) * mmax(ii) + mmin(ii)
                         end do
                         write(unit=ipr,fmt="(tr1,f14.8,3(/,t5, 5(f14.8,tr1)))") ff, (w(ii),ii = 1,nparm)
                         if (ff < f0(iv)) then
                            write(unit=ipr,fmt="(a,i2,2(a,f16.8))")   &
                                 " *** Improvement On The Local Minimum No. ",iv,":", f0(iv), " For ", ff
                            do ii = 1, nparm
                               w(ii) = y(ii) * mmax(ii) + mmin(ii)
                            end do
                            write(unit=ipr,fmt="(tr1,f14.8,3(/,t5, 5(f14.8,tr1)))") ff, (w(ii),ii = 1,nparm)
                            f0(iv) = ff
                            do ii = 1, nparm
                               x0(ii,iv) = y(ii)
                            end do
                         end if
                         if (n1 > 20) then
                            write(unit=ipr,fmt="(a)") " ***   Too many new seed points"
                            exit do_main
                         end if
                         do ii = 1, nparm
                            x1(ii,n1) = x(ii,i1)
                            xcl(ii,1) = x(ii,i1)
                         end do
                         f1(n1) = f(i1)
                         fcl(1) = f(i1)
                         ic1(n1) = iv
                         icj = iv
                         new_locmin=.false.
                         exit
                      end if
                   end do
                end if

                if (new_locmin) then
                   !---- NEW LOCAL MINIMUM
                   nc = nc + 1
                   ncp = ncp + 1
                   write(unit=ipr,fmt="(a,i2,a,f14.8,a,i5)") " *** The Local Minimum No. ",nc,": ", ff,", NFEV=", nfe1
                   do ii = 1, nparm
                      w(ii) = y(ii) * mmax(ii) + mmin(ii)
                   end do
                   write(unit=ipr,fmt="(tr1,f14.8,3(/,t5, 5(f14.8,tr1)))") ff, (w(ii),ii = 1,nparm)
                   do ii = 1, nparm
                      x0(ii,nc) = y(ii)
                      xcl(ii,1) = y(ii)
                   end do
                   fcl(1) = ff
                   f0(nc) = ff
                   if (nc >= 20) then
                      write(unit=ipr,fmt="(a)")  " ***   Too many clusters"
                      exit do_main
                   end if
                   it = 1
                   icj = nc
                end if

                !----  CLUSTERING TO THE NEW POINT
                nfe = nfe + nfe1
                ic(i1) = icj
                ig = ig + 1
                if (ig >= ng) exit
                i = 1
                in1 = i

                do   ! while i < in1
                   do j = 1, ng
                      if (ic(j) == 0) then
                         if (fcl(i) < f(j)) then
                            do l1 = 1, nparm
                               w(l1) = abs(xcl(l1,i)-x(l1,j))
                            end do
                            a = zero
                            do l1 = 1, nparm
                               if (w(l1) > a) a = w(l1)
                            end do
                            if (a < b) then
                               in1 = in1 + 1
                               do ii = 1, nparm
                                  xcl(ii,in1) = x(ii,j)
                               end do
                               fcl(in1) = f(j)
                               ic(j) = icj
                               write(unit=ipr,fmt="(a,i2)") " Sample Point Added To The Cluster No. ",icj
                               do ii = 1, nparm
                                  w(ii) = x(ii,j) * mmax(ii) + mmin(ii)
                               end do
                               write(unit=ipr,fmt="(tr1,f14.8,3(/,t5, 5(f14.8,tr1)))") f(j), (w(ii),ii = 1,nparm)
                               ig = ig + 1
                               if (ig >= ng) exit
                            end if
                         end if
                      end if
                   end do
                   i = i + 1
                   if (i >= in1) exit
                end do
             end if
          end do

          if (it == 0) exit
       end do do_main      !End of main infinite loop controlled by variable "it"

       !----  PRINT RESULTS ----!
       write(unit=ipr,fmt="(/,/,/,a,/,/)") " Local Minima Found:"
       if (nc > 1) then
          inum = nc - 1
          do i = 1, inum
             im = i
             fm = f0(i)
             inum1 = i + 1
             do j = inum1, nc
                if (f0(j) < fm) then
                   im = j
                   fm = f0(j)
                end if
             end do
             if (im > i) then
                a = fm
                do j = 1, nparm
                   y(j) = x0(j,im)
                end do
                inum1 = im - i
                do j = 1, inum1
                   inum2 = im - j
                   f0(inum2+1) = f0(inum2)
                   do jj = 1, nparm
                      x0(jj,inum2+1) = x0(jj,inum2)
                   end do
                end do
                f0(i) = a
                do j = 1, nparm
                   x0(j,i) = y(j)
                end do
             end if
          end do
       end if

       if (nc > 0) then
          do i = 1, nc
             do ii = 1, nparm
                x0(ii,i) = x0(ii,i) * mmax(ii) + mmin(ii)
             end do
             write(unit=ipr,fmt="(tr1,f14.8,3(/,t5, 5(f14.8,tr1)))") f0(i), (x0(ii,i),ii = 1,nparm)
          end do
       end if
       write(unit=ipr,fmt="(a,i5,a)") " Normal Termination After ",Nfe," Function Evaluations"

    End Subroutine Csendes_Global

    !!----
    !!---- Subroutine Local_Rand(N,Relcon,Maxfn,X,F,Nfev,Mini,Maxi,Model_Functn)
    !!----    integer,            intent(in)   :: n       ! the number of unknown parameters
    !!----    real,               intent(in)   :: relcon  ! convergence criterion.
    !!----    integer,            intent(in)   :: maxfn   ! maximum number of function evaluations allowed
    !!----    real, dimension(:), intent(out)  :: x       ! Normalized vector of dimension n containing parameter values
    !!----    real,               intent(out)  :: f       ! value at the final parameter estimates
    !!----    integer,            intent(out)  :: nfev    ! number of function evaluations
    !!----    real, dimension(:), intent(in)   :: mini    ! scaling factors supplied by the global routine
    !!----    real, dimension(:), intent(in)   :: maxi    ! scaling factors supplied by the global routine
    !!----
    !!--<<    Interface
    !!----       Subroutine Model_Functn(n,x,f,g)
    !!----          use CFML_GlobalDeps, only:cp
    !!----          integer,                             intent(in) :: n
    !!----          real(kind=cp),dimension(:),          intent(in) :: x
    !!----          real(kind=cp),                       intent(out):: f
    !!----          real(kind=cp),dimension(:),optional, intent(out):: g
    !!----       End Subroutine Model_Functn
    !!-->>    End Interface
    !!----
    !!----   MINIMUM OF FUNCTION OF N VARIABLES USING A RANDOM WALK METHOD
    !!----   UNIRANDI WITH A GIVEN INITIAL STEP LENGTH.
    !!----
    !!----   Adapted by J. Rodriguez-Carvajal to F90
    !!----   Original comments (slightly modified) follow
    !!----
    !!--..   LATEST REVISION
    !!--..   August 2007  (from F77 version October 15, 1986)
    !!--..   (ALTERATIONS IN THE COMMENTS AND IN THE ROUTINE PARAMETERS TO MEET THE
    !!--..    REQUIREMENTS OF THE LATEST(JULY 31, 1986) VERSION OF THE GLOBAL ROUTINE.)
    !!--..
    !!--..    MORE INFO ABOUT ARGUMENTS
    !!--..       RELCON - CONVERGENCE CRITERION. (INPUT) THIS IS SATISFIED IF, ON TWO
    !!--..                SUCCESSIVE ITERATIONS, THE PARAMETER ESTIMATES (I.E., X(I),
    !!--..                I=1,...,N) OR THE FUNCTION VALUE DIFFERS,COMPONENT BY COMPONENT,
    !!--..                BY AT MOST RELCON.
    !!--..       MAXFN  - MAXIMUM NUMBER OF FUNCTION EVALUATIONS (I.E.,CALLS TO SUBROUTINE FUNCT)
    !!--..                ALLOWED. (INPUT)
    !!--..                THE ACTUAL NUMBER OF CALLS TO FUNCT MAY EXCEED MAXFN SLIGHTLY.
    !!--..
    !!--..       X      - VECTOR OF LENGTH N CONTAINING PARAMETER VALUES.
    !!--..                ON INPUT, X SHOULD CONTAIN THE INITIAL ESTIMATE OF THE
    !!--..                LOCATION OF THE MINIMUM.
    !!--..                ON OUTPUT, X CONTAINS THE FINAL ESTIMATE OF THE LOCATION
    !!--..                OF THE MINIMUM.
    !!--..       F      - A SCALAR CONTAINING THE VALUE OF THE OBJECTIVE
    !!--..                FUNCTION AT THE FINAL PARAMETER ESTIMATES.
    !!--..                ON INPUT, F SHOULD CONTAIN THE FUNCTION VALUE
    !!--..                AT THE INITIAL PARAMETER ESTIMATES.
    !!--..                ON OUTPUT, F CONTAINS THE FUNCTION VALUE
    !!--..                AT THE FINAL PARAMETER ESTIMATES.
    !!--..       NFEV   - THE NUMBER OF FUNCTION EVALUATIONS (OUTPUT)
    !!--..       MINi   - A VECTOR OF LENGTH N CONTAINING SCALING
    !!--..                FACTORS SUPPLIED BY THE GLOBAL ROUTINE
    !!--..                (INPUT)
    !!--..       MAXi   - A VECTOR OF LENGTH N CONTAINING SCALING
    !!--..                FACTORS SUPPLIED BY THE GLOBAL ROUTINE
    !!--..                (INPUT)
    !!--..
    !!--..   REFERENCE
    !!--..      TIMO JARVI: A RANDOM SEARCH OPTIMIZER WITH AN APPLICATION TO A MAX-MIN
    !!--..                  PROBLEM. A SPECIAL TRAJECTORY ESTIMATION PROBLEM, PUBLICATIONS
    !!--..                  OF THE INSTITUTE FOR APPLIED MATHEMATICS, NO. 3, UNIVERSITY
    !!--..                  OF TURKU,1973.
    !!--..
    !!----
    !!---- Update: February - 2005
    !!
    Subroutine Local_Rand(N,Relcon,Maxfn,X,F,Nfev,Mini,Maxi,Model_Functn)
       !---- Arguments ----!
       integer,                     intent(in)     :: n
       real(kind=cp),               intent(in)     :: relcon
       integer,                     intent(in)     :: maxfn
       real(kind=cp), dimension(:), intent(in out) :: x
       real(kind=cp),               intent(in out) :: f
       integer,                     intent(out)    :: nfev
       real(kind=cp), dimension(:), intent(in)     :: mini
       real(kind=cp), dimension(:), intent(in)     :: maxi
       Interface
          Subroutine Model_Functn(n,x,f,g)
             use CFML_GlobalDeps,  only: cp
             integer,                             intent(in) :: n
             real(kind=cp),dimension(:),          intent(in) :: x
             real(kind=cp),                       intent(out):: f
             real(kind=cp),dimension(:),optional, intent(out):: g
          End Subroutine Model_Functn
       End Interface

       !---- Local variables ----!
       integer                            :: i,itest, irndm
       real(kind=cp)                      :: h, deltf, eps, a, f1
       real(kind=cp), dimension(n)        :: x1
       real(kind=cp), dimension(100,n)    :: r
       real(kind=cp), parameter           :: zero=0.0_cp, onen3=0.001_cp, half=0.5_cp, one=1.0_cp, two=2.0_cp

       h = onen3                   !First executable statement
       deltf = one                 !initial step length
       itest = 0
       nfev = 0
       eps = relcon

       do_out: do
          call random_number(r)              !  Evaluate 100 random vectors r(100,n)
          irndm = 0

          do_int: do
             irndm = irndm+1
             IF (irndm > 100) cycle do_out
             a = zero                            !Select a random vector having norm
             DO  i=1,n                           !less or equal to 0.5
               r(irndm,i) = r(irndm,i)-half
               a = a+r(irndm,i)*r(irndm,i)
             END DO
             IF (a <= zero) cycle do_int
             a = SQRT(a)
             !IF (a > half) cycle do_int
             r(irndm,1:n) = r(irndm,1:n)/a       !New trial point
             x1(1:n) = x(1:n)+h*r(irndm,1:n)
             CALL Fun(x1, f1, n, Mini, Maxi, Model_Functn)
             nfev = nfev+1
             IF (f1 >= f) then
                IF (nfev > maxfn) exit do_out
                h = -h                           !Step in the opposite direction
                DO  i=1,n
                  x1(i) = x(i)+h*r(irndm,i)
                END DO
                call Fun(x1, f1, n, Mini, Maxi, Model_Functn)
                nfev = nfev+1

               IF (f1 >= f) then
                   IF (nfev > maxfn) exit do_out
                   itest = itest+1
                   IF (itest < 2) cycle do_int
                   h = h*half                    !Decrease step length
                   itest = 0
                   IF (deltf < eps) exit do_out  !Relative convergence test for the
                                                 !objective function
                   IF (ABS(h)-relcon < 0.0_cp) THEN !Convergence test for the step length
                     exit  do_out
                   ELSE
                     cycle do_int
                   END IF
                END IF
             END IF

             do
                x(1:n) = x1(1:n)
                deltf = (f-f1)/ABS(f1)
                f = f1
                h = h*two   ! Increase step length
                x1(1:n) = x(1:n)+h*r(irndm,1:n)
                call Fun(x1, f1, n, Mini, Maxi, Model_Functn)
                nfev = nfev+1
                IF (f1 >= f) exit
             end do
             IF (nfev > maxfn) exit do_out  ! Check tolerance maxfn
             h = ABS(h*half)  ! Decrease step length
          End do do_int
       End do do_out

    End Subroutine Local_Rand


End Submodule OPT_Global_Csendes