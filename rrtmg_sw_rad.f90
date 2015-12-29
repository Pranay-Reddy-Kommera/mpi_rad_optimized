! KGEN-generated Fortran source file
!
! Filename    : rrtmg_sw_rad.f90
! Generated at: 2015-07-07 00:48:23
! KGEN version: 0.4.13



    MODULE rrtmg_sw_rad
        USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check
        !  --------------------------------------------------------------------------
        ! |                                                                          |
        ! |  Copyright 2002-2007, Atmospheric & Environmental Research, Inc. (AER).  |
        ! |  This software may be used, copied, or redistributed as long as it is    |
        ! |  not sold and this copyright notice is reproduced on each copy made.     |
        ! |  This model is provided as is without any express or implied warranties. |
        ! |                       (http://www.rtweb.aer.com/)                        |
        ! |                                                                          |
        !  --------------------------------------------------------------------------
        !
        ! ****************************************************************************
        ! *                                                                          *
        ! *                             RRTMG_SW                                     *
        ! *                                                                          *
        ! *                                                                          *
        ! *                                                                          *
        ! *                 a rapid radiative transfer model                         *
        ! *                  for the solar spectral region                           *
        ! *           for application to general circulation models                  *
        ! *                                                                          *
        ! *                                                                          *
        ! *           Atmospheric and Environmental Research, Inc.                   *
        ! *                       131 Hartwell Avenue                                *
        ! *                       Lexington, MA 02421                                *
        ! *                                                                          *
        ! *                                                                          *
        ! *                          Eli J. Mlawer                                   *
        ! *                       Jennifer S. Delamere                               *
        ! *                        Michael J. Iacono                                 *
        ! *                        Shepard A. Clough                                 *
        ! *                                                                          *
        ! *                                                                          *
        ! *                                                                          *
        ! *                                                                          *
        ! *                                                                          *
        ! *                                                                          *
        ! *                      email:  miacono@aer.com                             *
        ! *                      email:  emlawer@aer.com                             *
        ! *                      email:  jdelamer@aer.com                            *
        ! *                                                                          *
        ! *       The authors wish to acknowledge the contributions of the           *
        ! *       following people:  Steven J. Taubman, Patrick D. Brown,            *
        ! *       Ronald E. Farren, Luke Chen, Robert Bergstrom.                     *
        ! *                                                                          *
        ! ****************************************************************************
        ! --------- Modules ---------
        USE shr_kind_mod, ONLY: r8 => shr_kind_r8
        !      use parkind, only : jpim, jprb
        USE rrtmg_sw_cldprmc, ONLY: cldprmc_sw
        ! Move call to rrtmg_sw_ini and following use association to
        ! GCM initialization area
        !      use rrtmg_sw_init, only: rrtmg_sw_ini
        USE rrtmg_sw_setcoef, ONLY: setcoef_sw
        USE rrtmg_sw_spcvmc, ONLY: spcvmc_sw
        IMPLICIT NONE
        ! public interfaces/functions/subroutines
        !      public :: rrtmg_sw, inatm_sw, earth_sun
        PUBLIC rrtmg_sw
        !------------------------------------------------------------------
        CONTAINS

        ! write subroutines
        ! No subroutines
        ! No module extern variables
        !------------------------------------------------------------------
        !------------------------------------------------------------------
        ! Public subroutines
        !------------------------------------------------------------------

	!dir$ attributes offload:mic :: rrtmg_sw
      subroutine rrtmg_sw &
            (lchnk   ,ncol    ,nlay    ,icld    ,          &
             play    ,plev    ,tlay    ,tlev    ,tsfc    , &
             h2ovmr  ,o3vmr   ,co2vmr  ,ch4vmr  ,o2vmr   ,n2ovmr  , &
             asdir   ,asdif   ,aldir   ,aldif   , &
             coszen  ,adjes   ,dyofyr  ,solvar, &
             inflgsw ,iceflgsw,liqflgsw, &
             cldfmcl ,taucmcl ,ssacmcl ,asmcmcl ,fsfcmcl, &
             ciwpmcl ,clwpmcl ,reicmcl ,relqmcl , &
             tauaer  ,ssaaer  ,asmaer  , &
             swuflx  ,swdflx  ,swhr    ,swuflxc ,swdflxc ,swhrc, &
             dirdnuv, dirdnir, difdnuv, difdnir, ninflx, ninflxc, &
             swuflxs, swdflxs)


! ------- Description -------

! This program is the driver for RRTMG_SW, the AER SW radiation model for 
!  application to GCMs, that has been adapted from RRTM_SW for improved
!  efficiency and to provide fractional cloudiness and cloud overlap
!  capability using McICA.
!
! Note: The call to RRTMG_SW_INI should be moved to the GCM initialization 
!  area, since this has to be called only once. 
!
! This routine
!    b) calls INATM_SW to read in the atmospheric profile;
!       all layering in RRTMG is ordered from surface to toa. 
!    c) calls CLDPRMC_SW to set cloud optical depth for McICA based
!       on input cloud properties
!    d) calls SETCOEF_SW to calculate various quantities needed for 
!       the radiative transfer algorithm
!    e) calls SPCVMC to call the two-stream model that in turn 
!       calls TAUMOL to calculate gaseous optical depths for each 
!       of the 16 spectral bands and to perform the radiative transfer
!       using McICA, the Monte-Carlo Independent Column Approximation,
!       to represent sub-grid scale cloud variability
!    f) passes the calculated fluxes and cooling rates back to GCM
!
! Two modes of operation are possible:
!     The mode is chosen by using either rrtmg_sw.nomcica.f90 (to not use
!     McICA) or rrtmg_sw.f90 (to use McICA) to interface with a GCM.
!
!    1) Standard, single forward model calculation (imca = 0); this is 
!       valid only for clear sky or fully overcast clouds
!    2) Monte Carlo Independent Column Approximation (McICA, Pincus et al., 
!       JC, 2003) method is applied to the forward model calculation (imca = 1)
!       This method is valid for clear sky or partial cloud conditions.
!
! This call to RRTMG_SW must be preceeded by a call to the module
!     mcica_subcol_gen_sw.f90 to run the McICA sub-column cloud generator,
!     which will provide the cloud physical or cloud optical properties
!     on the RRTMG quadrature point (ngptsw) dimension.
!
! Two methods of cloud property input are possible:
!     Cloud properties can be input in one of two ways (controlled by input 
!     flags inflag, iceflag and liqflag; see text file rrtmg_sw_instructions
!     and subroutine rrtmg_sw_cldprop.f90 for further details):
!
!    1) Input cloud fraction, cloud optical depth, single scattering albedo 
!       and asymmetry parameter directly (inflgsw = 0)
!    2) Input cloud fraction and cloud physical properties: ice fracion,
!       ice and liquid particle sizes (inflgsw = 1 or 2);  
!       cloud optical properties are calculated by cldprop or cldprmc based
!       on input settings of iceflgsw and liqflgsw
!
! Two methods of aerosol property input are possible:
!     Aerosol properties can be input in one of two ways (controlled by input 
!     flag iaer, see text file rrtmg_sw_instructions for further details):
!
!    1) Input aerosol optical depth, single scattering albedo and asymmetry
!       parameter directly by layer and spectral band (iaer=10)
!    2) Input aerosol optical depth and 0.55 micron directly by layer and use
!       one or more of six ECMWF aerosol types (iaer=6)
!
!
! ------- Modifications -------
!
! This version of RRTMG_SW has been modified from RRTM_SW to use a reduced
! set of g-point intervals and a two-stream model for application to GCMs. 
!
!-- Original version (derived from RRTM_SW)
!     2002: AER. Inc.
!-- Conversion to F90 formatting; addition of 2-stream radiative transfer
!     Feb 2003: J.-J. Morcrette, ECMWF
!-- Additional modifications for GCM application
!     Aug 2003: M. J. Iacono, AER Inc.
!-- Total number of g-points reduced from 224 to 112.  Original
!   set of 224 can be restored by exchanging code in module parrrsw.f90 
!   and in file rrtmg_sw_init.f90.
!     Apr 2004: M. J. Iacono, AER, Inc.
!-- Modifications to include output for direct and diffuse 
!   downward fluxes.  There are output as "true" fluxes without
!   any delta scaling applied.  Code can be commented to exclude
!   this calculation in source file rrtmg_sw_spcvrt.f90.
!     Jan 2005: E. J. Mlawer, M. J. Iacono, AER, Inc.
!-- Revised to add McICA capability.
!     Nov 2005: M. J. Iacono, AER, Inc.
!-- Reformatted for consistency with rrtmg_lw.
!     Feb 2007: M. J. Iacono, AER, Inc.
!-- Modifications to formatting to use assumed-shape arrays. 
!     Aug 2007: M. J. Iacono, AER, Inc.
!-- Modified to output direct and diffuse fluxes either with or without
!   delta scaling based on setting of idelm flag
!     Dec 2008: M. J. Iacono, AER, Inc.

! --------- Modules ---------

      use parrrsw, only : nbndsw, ngptsw,  nmol, mxmol, &
                          jpband, jpb1, jpb2
		!!dir$ attributes offload:mic :: heatfac
		!!dir$ attributes offload:mic :: oneminus
		!!dir$ attributes offload:mic :: pi
      use rrsw_con, only : heatfac, oneminus, pi
      use rrsw_wvn, only : wavenum2

! ------- Declarations

! ----- Input -----
      integer, intent(in) :: lchnk                      ! chunk identifier
      integer, intent(in) :: ncol                       ! Number of horizontal columns     
      integer, intent(in) :: nlay                       ! Number of model layers
      integer, intent(inout) :: icld                    ! Cloud overlap method
                                                        !    0: Clear only
                                                        !    1: Random
                                                        !    2: Maximum/random
                                                        !    3: Maximum
      real(kind=r8), intent(in) :: play(:,:)            ! Layer pressures (hPa, mb)
                                                        !    Dimensions: (ncol,nlay)
      real(kind=r8), intent(in) :: plev(:,:)            ! Interface pressures (hPa, mb)
                                                        !    Dimensions: (ncol,nlay+1)
      real(kind=r8), intent(in) :: tlay(:,:)            ! Layer temperatures (K)
                                                        !    Dimensions: (ncol,nlay)
      real(kind=r8), intent(in) :: tlev(:,:)            ! Interface temperatures (K)
                                                        !    Dimensions: (ncol,nlay+1)
      real(kind=r8), intent(in) :: tsfc(:)              ! Surface temperature (K)
                                                        !    Dimensions: (ncol)
      real(kind=r8), intent(in) :: h2ovmr(:,:)          ! H2O volume mixing ratio
                                                        !    Dimensions: (ncol,nlay)
      real(kind=r8), intent(in) :: o3vmr(:,:)           ! O3 volume mixing ratio
                                                        !    Dimensions: (ncol,nlay)
      real(kind=r8), intent(in) :: co2vmr(:,:)          ! CO2 volume mixing ratio
                                                        !    Dimensions: (ncol,nlay)
      real(kind=r8), intent(in) :: ch4vmr(:,:)          ! Methane volume mixing ratio
                                                        !    Dimensions: (ncol,nlay)
      real(kind=r8), intent(in) :: o2vmr(:,:)           ! O2 volume mixing ratio
                                                        !    Dimensions: (ncol,nlay)
      real(kind=r8), intent(in) :: n2ovmr(:,:)          ! Nitrous oxide volume mixing ratio
                                                        !    Dimensions: (ncol,nlay)
      real(kind=r8), intent(in) :: asdir(:)             ! UV/vis surface albedo direct rad
                                                        !    Dimensions: (ncol)
      real(kind=r8), intent(in) :: aldir(:)             ! Near-IR surface albedo direct rad
                                                        !    Dimensions: (ncol)
      real(kind=r8), intent(in) :: asdif(:)             ! UV/vis surface albedo: diffuse rad
                                                        !    Dimensions: (ncol)
      real(kind=r8), intent(in) :: aldif(:)             ! Near-IR surface albedo: diffuse rad
                                                        !    Dimensions: (ncol)

      integer, intent(in) :: dyofyr                     ! Day of the year (used to get Earth/Sun
                                                        !  distance if adjflx not provided)
      real(kind=r8), intent(in) :: adjes                ! Flux adjustment for Earth/Sun distance
      real(kind=r8), intent(in) :: coszen(:)            ! Cosine of solar zenith angle
                                                        !    Dimensions: (ncol)
      real(kind=r8), intent(in) :: solvar(1:nbndsw)     ! Solar constant (Wm-2) scaling per band

      integer, intent(in) :: inflgsw                    ! Flag for cloud optical properties
      integer, intent(in) :: iceflgsw                   ! Flag for ice particle specification
      integer, intent(in) :: liqflgsw                   ! Flag for liquid droplet specification

      real(kind=r8), intent(in) :: cldfmcl(:,:,:)       ! Cloud fraction
                                                        !    Dimensions: (ngptsw,ncol,nlay)
      real(kind=r8), intent(in) :: taucmcl(:,:,:)       ! Cloud optical depth
                                                        !    Dimensions: (ngptsw,ncol,nlay)
      real(kind=r8), intent(in) :: ssacmcl(:,:,:)       ! Cloud single scattering albedo
                                                        !    Dimensions: (ngptsw,ncol,nlay)
      real(kind=r8), intent(in) :: asmcmcl(:,:,:)       ! Cloud asymmetry parameter
                                                        !    Dimensions: (ngptsw,ncol,nlay)
      real(kind=r8), intent(in) :: fsfcmcl(:,:,:)       ! Cloud forward scattering parameter
                                                        !    Dimensions: (ngptsw,ncol,nlay)
      real(kind=r8), intent(in) :: ciwpmcl(:,:,:)       ! Cloud ice water path (g/m2)
                                                        !    Dimensions: (ngptsw,ncol,nlay)
      real(kind=r8), intent(in) :: clwpmcl(:,:,:)       ! Cloud liquid water path (g/m2)
                                                        !    Dimensions: (ngptsw,ncol,nlay)
      real(kind=r8), intent(in) :: reicmcl(:,:)         ! Cloud ice effective radius (microns)
                                                        !    Dimensions: (ncol,nlay)
      real(kind=r8), intent(in) :: relqmcl(:,:)         ! Cloud water drop effective radius (microns)
                                                        !    Dimensions: (ncol,nlay)
      real(kind=r8), intent(in) :: tauaer(:,:,:)        ! Aerosol optical depth (iaer=10 only)
                                                        !    Dimensions: (ncol,nlay,nbndsw)
                                                        ! (non-delta scaled)      
      real(kind=r8), intent(in) :: ssaaer(:,:,:)        ! Aerosol single scattering albedo (iaer=10 only)
                                                        !    Dimensions: (ncol,nlay,nbndsw)
                                                        ! (non-delta scaled)      
      real(kind=r8), intent(in) :: asmaer(:,:,:)        ! Aerosol asymmetry parameter (iaer=10 only)
                                                        !    Dimensions: (ncol,nlay,nbndsw)
                                                        ! (non-delta scaled)      
!      real(kind=r8), intent(in) :: ecaer(:,:,:)         ! Aerosol optical depth at 0.55 micron (iaer=6 only)
                                                        !    Dimensions: (ncol,nlay,naerec)
                                                        ! (non-delta scaled)      

! ----- Output -----

      real(kind=r8), intent(out) :: swuflx(:,:)         ! Total sky shortwave upward flux (W/m2)
                                                        !    Dimensions: (ncol,nlay+1)
      real(kind=r8), intent(out) :: swdflx(:,:)         ! Total sky shortwave downward flux (W/m2)
                                                        !    Dimensions: (ncol,nlay+1)
      real(kind=r8), intent(out) :: swhr(:,:)           ! Total sky shortwave radiative heating rate (K/d)
                                                        !    Dimensions: (ncol,nlay)
      real(kind=r8), intent(out) :: swuflxc(:,:)        ! Clear sky shortwave upward flux (W/m2)
                                                        !    Dimensions: (ncol,nlay+1)
      real(kind=r8), intent(out) :: swdflxc(:,:)        ! Clear sky shortwave downward flux (W/m2)
                                                        !    Dimensions: (ncol,nlay+1)
      real(kind=r8), intent(out) :: swhrc(:,:)          ! Clear sky shortwave radiative heating rate (K/d)
                                                        !    Dimensions: (ncol,nlay)

      real(kind=r8), intent(out) :: dirdnuv(:,:)        ! Direct downward shortwave flux, UV/vis
      real(kind=r8), intent(out) :: difdnuv(:,:)        ! Diffuse downward shortwave flux, UV/vis
      real(kind=r8), intent(out) :: dirdnir(:,:)        ! Direct downward shortwave flux, near-IR
      real(kind=r8), intent(out) :: difdnir(:,:)        ! Diffuse downward shortwave flux, near-IR

      real(kind=r8), intent(out) :: ninflx(:,:)         ! Net shortwave flux, near-IR
      real(kind=r8), intent(out) :: ninflxc(:,:)        ! Net clear sky shortwave flux, near-IR

      real(kind=r8), intent(out)  :: swuflxs(:,:,:)     ! shortwave spectral flux up
      real(kind=r8), intent(out)  :: swdflxs(:,:,:)     ! shortwave spectral flux down

! ----- Local -----

! Control
      integer :: istart                         ! beginning band of calculation
      integer :: iend                           ! ending band of calculation
      integer :: icpr                           ! cldprop/cldprmc use flag
      integer :: iout = 0                       ! output option flag (inactive)
      integer :: iaer                           ! aerosol option flag
      integer :: idelm                          ! delta-m scaling flag
                                                ! [0 = direct and diffuse fluxes are unscaled]
                                                ! [1 = direct and diffuse fluxes are scaled]
                                                ! (total downward fluxes are always delta scaled)
      integer :: isccos                         ! instrumental cosine response flag (inactive)
      integer :: iplon                          ! column loop index
      integer :: i                              ! layer loop index                       ! jk
      integer :: ib                             ! band loop index                        ! jsw
      integer :: ia, ig                         ! indices
      integer :: k                              ! layer loop index
      integer :: ims                            ! value for changing mcica permute seed
      integer :: imca                           ! flag for mcica [0=off, 1=on]

      real(kind=r8) :: zepsec, zepzen           ! epsilon
      real(kind=r8) :: zdpgcp                   ! flux to heating conversion ratio

! Atmosphere
      real(kind=r8) :: pavel(ncol,nlay)            ! layer pressures (mb)
      real(kind=r8) :: tavel(ncol,nlay)            ! layer temperatures (K)
      real(kind=r8) :: pz(ncol,0:nlay)             ! level (interface) pressures (hPa, mb)
      real(kind=r8) :: tz(ncol,0:nlay)             ! level (interface) temperatures (K)
      real(kind=r8) :: tbound(ncol)                   ! surface temperature (K)
      real(kind=r8) :: pdp(ncol,nlay)              ! layer pressure thickness (hPa, mb)
      real(kind=r8) :: coldry(ncol,nlay)           ! dry air column amount
      real(kind=r8) :: wkl(ncol,mxmol,nlay)        ! molecular amounts (mol/cm-2)

!      real(kind=r8) :: earth_sun               ! function for Earth/Sun distance factor
      real(kind=r8) :: cossza(ncol)             ! Cosine of solar zenith angle
      real(kind=r8) :: adjflux(ncol,jpband)     ! adjustment for current Earth/Sun distance
!      real(kind=r8) :: solvar(jpband)           ! solar constant scaling factor from rrtmg_sw
                                                !  default value of 1368.22 Wm-2 at 1 AU
      real(kind=r8) :: albdir(ncol,nbndsw)      ! surface albedo, direct          ! zalbp
      real(kind=r8) :: albdif(ncol,nbndsw)      ! surface albedo, diffuse         ! zalbd

      real(kind=r8) :: taua(ncol,nlay,nbndsw)   ! Aerosol optical depth
      real(kind=r8) :: ssaa(ncol,nlay,nbndsw)   ! Aerosol single scattering albedo
      real(kind=r8) :: asma(ncol,nlay,nbndsw)   ! Aerosol asymmetry parameter

! Atmosphere - setcoef
      integer :: laytrop                        ! tropopause layer index
      integer :: layswtch(ncol)                       !
      integer :: laylow(ncol)                         !
      integer :: jp(ncol,nlay)                     !
      integer :: jt(ncol,nlay)                     !
      integer :: jt1(ncol,nlay)                    !

      real(kind=r8) :: colh2o(ncol,nlay)           ! column amount (h2o)
      real(kind=r8) :: colco2(ncol,nlay)           ! column amount (co2)
      real(kind=r8) :: colo3(ncol,nlay)            ! column amount (o3)
      real(kind=r8) :: coln2o(ncol,nlay)           ! column amount (n2o)
      real(kind=r8) :: colch4(ncol,nlay)           ! column amount (ch4)
      real(kind=r8) :: colo2(ncol,nlay)            ! column amount (o2)
      real(kind=r8) :: colmol(ncol,nlay)           ! column amount
      real(kind=r8) :: co2mult(ncol,nlay)          ! column amount

      integer :: indself(ncol,nlay)
      integer :: indfor(ncol,nlay)
      real(kind=r8) :: selffac(ncol,nlay)
      real(kind=r8) :: selffrac(ncol,nlay)
      real(kind=r8) :: forfac(ncol,nlay)
      real(kind=r8) :: forfrac(ncol,nlay)

      real(kind=r8) :: fac00(ncol,nlay)
      real(kind=r8) :: fac01(ncol,nlay)
      real(kind=r8) :: fac11(ncol,nlay)
      real(kind=r8) :: fac10(ncol,nlay)

! Atmosphere/clouds - cldprop
      integer :: inflag                         ! flag for cloud property method
      integer :: iceflag                        ! flag for ice cloud properties
      integer :: liqflag                        ! flag for liquid cloud properties

!      real(kind=r8) :: cldfrac(nlay)            ! layer cloud fraction
!      real(kind=r8) :: tauc(nlay)               ! cloud optical depth (non-delta scaled)
!      real(kind=r8) :: ssac(nlay)               ! cloud single scattering albedo (non-delta scaled)
!      real(kind=r8) :: asmc(nlay)               ! cloud asymmetry parameter (non-delta scaled)
!      real(kind=r8) :: ciwp(nlay)               ! cloud ice water path
!      real(kind=r8) :: clwp(nlay)               ! cloud liquid water path
!      real(kind=r8) :: rei(nlay)                ! cloud ice particle size
!      real(kind=r8) :: rel(nlay)                ! cloud liquid particle size

!      real(kind=r8) :: taucloud(nlay,jpband)    ! cloud optical depth
!      real(kind=r8) :: taucldorig(nlay,jpband)  ! cloud optical depth (non-delta scaled)
!      real(kind=r8) :: ssacloud(nlay,jpband)    ! cloud single scattering albedo
!      real(kind=r8) :: asmcloud(nlay,jpband)    ! cloud asymmetry parameter

! Atmosphere/clouds - cldprmc [mcica]
      real(kind=r8) :: cldfmc(ncol,ngptsw,nlay)    ! cloud fraction [mcica]
      real(kind=r8) :: ciwpmc(ncol,ngptsw,nlay)    ! cloud ice water path [mcica]
      real(kind=r8) :: clwpmc(ncol,ngptsw,nlay)    ! cloud liquid water path [mcica]
      real(kind=r8) :: relqmc(ncol,nlay)           ! liquid particle size (microns)
      real(kind=r8) :: reicmc(ncol,nlay)           ! ice particle effective radius (microns)
      real(kind=r8) :: dgesmc(ncol,nlay)           ! ice particle generalized effective size (microns)
      real(kind=r8) :: taucmc(ncol,ngptsw,nlay)    ! cloud optical depth [mcica]
      real(kind=r8) :: taormc(ngptsw,nlay)    ! unscaled cloud optical depth [mcica]
      real(kind=r8) :: ssacmc(ncol,ngptsw,nlay)    ! cloud single scattering albedo [mcica]
      real(kind=r8) :: asmcmc(ncol,ngptsw,nlay)    ! cloud asymmetry parameter [mcica]
      real(kind=r8) :: fsfcmc(ncol,ngptsw,nlay)    ! cloud forward scattering fraction [mcica]
! Atmosphere/clouds/aerosol - spcvrt,spcvmc

      real(kind=r8) :: ztaua(ncol,nlay,nbndsw)	  ! total aerosol optical depth
      real(kind=r8) :: zasya(ncol,nlay,nbndsw)	  ! total aerosol asymmetry parameter
      real(kind=r8) :: zomga(ncol,nlay,nbndsw)	  ! total aerosol single scattering albedo
      real(kind=r8) :: zcldfmc(ncol,nlay,ngptsw)   ! cloud fraction [mcica]
      real(kind=r8) :: ztaucmc(ncol,nlay,ngptsw)   ! cloud optical depth [mcica]
      real(kind=r8) :: ztaormc(ncol,nlay,ngptsw)   ! unscaled cloud optical depth [mcica]
      real(kind=r8) :: zasycmc(ncol,nlay,ngptsw)   ! cloud asymmetry parameter [mcica]
      real(kind=r8) :: zomgcmc(ncol,nlay,ngptsw)   ! cloud single scattering albedo [mcica]

!      real(kind=r8) :: zbbfu(ncol,nlay+2)          ! temporary upward shortwave flux (w/m2)
!      real(kind=r8) :: zbbfd(ncol,nlay+2)          ! temporary downward shortwave flux (w/m2)
!      real(kind=r8) :: zbbcu(ncol,nlay+2)          ! temporary clear sky upward shortwave flux (w/m2)
!      real(kind=r8) :: zbbcd(ncol,nlay+2)          ! temporary clear sky downward shortwave flux (w/m2)
      real(kind=r8) :: zbbfddir(ncol,nlay+2)       ! temporary downward direct shortwave flux (w/m2)
      real(kind=r8) :: zbbcddir(ncol,nlay+2)       ! temporary clear sky downward direct shortwave flux (w/m2)
      real(kind=r8) :: zuvfd(ncol,nlay+2)          ! temporary UV downward shortwave flux (w/m2)
      real(kind=r8) :: zuvcd(ncol,nlay+2)          ! temporary clear sky UV downward shortwave flux (w/m2)
      !real(kind=r8) :: zuvfddir(ncol,nlay+2)       ! temporary UV downward direct shortwave flux (w/m2)
      real(kind=r8) :: zuvcddir(ncol,nlay+2)       ! temporary clear sky UV downward direct shortwave flux (w/m2)
      real(kind=r8) :: znifd(ncol,nlay+2)          ! temporary near-IR downward shortwave flux (w/m2)
      real(kind=r8) :: znicd(ncol,nlay+2)          ! temporary clear sky near-IR downward shortwave flux (w/m2)
      !real(kind=r8) :: znifddir(ncol,nlay+2)       ! temporary near-IR downward direct shortwave flux (w/m2)
      real(kind=r8) :: znicddir(ncol,nlay+2)       ! temporary clear sky near-IR downward direct shortwave flux (w/m2)
! Added for near-IR flux diagnostic
      real(kind=r8) :: znifu(ncol,nlay+2)          ! temporary near-IR downward shortwave flux (w/m2)
      real(kind=r8) :: znicu(ncol,nlay+2)          ! temporary clear sky near-IR downward shortwave flux (w/m2)

! Optional output fields 
      real(kind=r8) :: swnflx(nlay+2)         ! Total sky shortwave net flux (W/m2)
      real(kind=r8) :: swnflxc(nlay+2)        ! Clear sky shortwave net flux (W/m2)
      real(kind=r8) :: dirdflux(nlay+2)       ! Direct downward shortwave surface flux
      real(kind=r8) :: difdflux(nlay+2)       ! Diffuse downward shortwave surface flux
      real(kind=r8) :: uvdflx(nlay+2)         ! Total sky downward shortwave flux, UV/vis   
      real(kind=r8) :: nidflx(nlay+2)         ! Total sky downward shortwave flux, near-IR  
!      real(kind=r8) :: zbbfsu(ncol,nbndsw,nlay+2)  ! temporary upward shortwave flux spectral (w/m2)
!      real(kind=r8) :: zbbfsd(ncol,nbndsw,nlay+2)  ! temporary downward shortwave flux spectral (w/m2)

! Output - inactive
!      real(kind=r8) :: zuvfu(nlay+2)         ! temporary upward UV shortwave flux (w/m2)
!      real(kind=r8) :: zuvfd(nlay+2)         ! temporary downward UV shortwave flux (w/m2)
!      real(kind=r8) :: zuvcu(nlay+2)         ! temporary clear sky upward UV shortwave flux (w/m2)
!      real(kind=r8) :: zuvcd(nlay+2)         ! temporary clear sky downward UV shortwave flux (w/m2)
!      real(kind=r8) :: zvsfu(nlay+2)         ! temporary upward visible shortwave flux (w/m2)
!      real(kind=r8) :: zvsfd(nlay+2)         ! temporary downward visible shortwave flux (w/m2)
!      real(kind=r8) :: zvscu(nlay+2)         ! temporary clear sky upward visible shortwave flux (w/m2)
!      real(kind=r8) :: zvscd(nlay+2)         ! temporary clear sky downward visible shortwave flux (w/m2)
!      real(kind=r8) :: znifu(nlay+2)         ! temporary upward near-IR shortwave flux (w/m2)
!      real(kind=r8) :: znifd(nlay+2)         ! temporary downward near-IR shortwave flux (w/m2)
!      real(kind=r8) :: znicu(nlay+2)         ! temporary clear sky upward near-IR shortwave flux (w/m2)
!      real(kind=r8) :: znicd(nlay+2)         ! temporary clear sky downward near-IR shortwave flux (w/m2)

! Initializations

      zepsec = 1.e-06_r8
      zepzen = 1.e-10_r8
      oneminus = 1.0_r8 - zepsec
      pi = 2._r8 * asin(1._r8)

      istart = jpb1
      iend = jpb2
      icpr = 0
      ims = 2

! In a GCM with or without McICA, set nlon to the longitude dimension
!
! Set imca to select calculation type:
!  imca = 0, use standard forward model calculation (clear and overcast only)
!  imca = 1, use McICA for Monte Carlo treatment of sub-grid cloud variability
!            (clear, overcast or partial cloud conditions)

! *** This version uses McICA (imca = 1) ***

! Set icld to select of clear or cloud calculation and cloud 
! overlap method (read by subroutine readprof from input file INPUT_RRTM):  
! icld = 0, clear only
! icld = 1, with clouds using random cloud overlap (McICA only)
! icld = 2, with clouds using maximum/random cloud overlap (McICA only)
! icld = 3, with clouds using maximum cloud overlap (McICA only)
      if (icld.lt.0.or.icld.gt.3) icld = 2

! Set iaer to select aerosol option
! iaer = 0, no aerosols
! iaer = 6, use six ECMWF aerosol types
!           input aerosol optical depth at 0.55 microns for each aerosol type (ecaer)
! iaer = 10, input total aerosol optical depth, single scattering albedo 
!            and asymmetry parameter (tauaer, ssaaer, asmaer) directly
      iaer = 10

! Set idelm to select between delta-M scaled or unscaled output direct and diffuse fluxes
! NOTE: total downward fluxes are always delta scaled
! idelm = 0, output direct and diffuse flux components are not delta scaled
!            (direct flux does not include forward scattering peak)
! idelm = 1, output direct and diffuse flux components are delta scaled (default)
!            (direct flux includes part or most of forward scattering peak)
      idelm = 1

! Call model and data initialization, compute lookup tables, perform
! reduction of g-points from 224 to 112 for input absorption
! coefficient data and other arrays.
!
! In a GCM this call should be placed in the model initialization
! area, since this has to be called only once.  
!      call rrtmg_sw_ini

! This is the main longitude/column loop in RRTMG.
! Modify to loop over all columns (nlon) or over daylight columns


! Prepare atmosphere profile from GCM for use in RRTMG, and define
! other input parameters

	!!dir$ attributes offload:mic :: inatm_sw
         call inatm_sw (1,ncol,nlay, icld, iaer, &
              play, plev, tlay, tlev, tsfc, &
              h2ovmr, o3vmr, co2vmr, ch4vmr, o2vmr, n2ovmr, adjes, dyofyr, solvar, &
              inflgsw, iceflgsw, liqflgsw, &
              cldfmcl, taucmcl, ssacmcl, asmcmcl, fsfcmcl, ciwpmcl, clwpmcl, &
              reicmcl, relqmcl, tauaer, ssaaer, asmaer, &
              pavel, pz, pdp, tavel, tz, tbound, coldry, wkl, &
              adjflux, inflag, iceflag, liqflag, cldfmc, taucmc, &
              ssacmc, asmcmc, fsfcmc, ciwpmc, clwpmc, reicmc, dgesmc, relqmc, &
              taua, ssaa, asma)

!  For cloudy atmosphere, use cldprop to set cloud optical properties based on
!  input cloud physical properties.  Select method based on choices described
!  in cldprop.  Cloud fraction, water path, liquid droplet and ice particle
!  effective radius must be passed in cldprop.  Cloud fraction and cloud
!  optical properties are transferred to rrtmg_sw arrays in cldprop.

	!!dir$ attributes offload:mic :: cldprmc_sw
         call cldprmc_sw(ncol,nlay, inflag, iceflag, liqflag, cldfmc, &
                         ciwpmc, clwpmc, reicmc, dgesmc, relqmc, &
                         taormc, taucmc, ssacmc, asmcmc, fsfcmc)
         icpr = 1
! Calculate coefficients for the temperature and pressure dependence of the
! molecular absorption coefficients by interpolating data from stored
      do iplon = 1, ncol         ! reference atmospheres.

	!!dir$ attributes offload:mic :: setcoef_sw
         call setcoef_sw(nlay, pavel(iplon,:), tavel(iplon,:), pz(iplon,:), &
                        tz(iplon,:), tbound(iplon), coldry(iplon,:), wkl(iplon,:,:), &
                        laytrop, layswtch(iplon), laylow(iplon), &
                        jp(iplon,:), jt(iplon,:), jt1(iplon,:), &
                         co2mult(iplon,:), colch4(iplon,:), colco2(iplon,:),&
                         colh2o(iplon,:), colmol(iplon,:), coln2o(iplon,:), &
                         colo2(iplon,:), colo3(iplon,:), fac00(iplon,:),&
                         fac01(iplon,:), fac10(iplon,:), fac11(iplon,:), &
                         selffac(iplon,:), selffrac(iplon,:), indself(iplon,:),&
                         forfac(iplon,:), forfrac(iplon,:), indfor(iplon,:))
      end do
! Cosine of the solar zenith angle
!  Prevent using value of zero; ideally, SW model is not called from host model when sun
!  is below horizon
      do iplon = 1, ncol
         cossza(iplon) = coszen(iplon)

         if (cossza(iplon) .lt. zepzen) cossza(iplon) = zepzen
      enddo

! Transfer albedo, cloud and aerosol properties into arrays for 2-stream radiative transfer

! Surface albedo
!  Near-IR bands 16-24 and 29 (1-9 and 14), 820-16000 cm-1, 0.625-12.195 microns
!         do ib=1,9
      do ib=1,8
         do iplon = 1, ncol
            albdir(iplon,ib) = aldir(iplon)
            albdif(iplon,ib) = aldif(iplon)
	 enddo
      enddo
      do iplon = 1, ncol
         albdir(iplon,nbndsw) = aldir(iplon)
         albdif(iplon,nbndsw) = aldif(iplon)
!  Set band 24 (or, band 9 counting from 1) to use linear average of UV/visible
!  and near-IR values, since this band straddles 0.7 microns:
         albdir(iplon,9) = 0.5*(aldir(iplon) + asdir(iplon))
         albdif(iplon,9) = 0.5*(aldif(iplon) + asdif(iplon))
      enddo
!  UV/visible bands 25-28 (10-13), 16000-50000 cm-1, 0.200-0.625 micron

      do ib=10,13
	 do iplon = 1, ncol
            albdir(iplon,ib) = asdir(iplon)
            albdif(iplon,ib) = asdif(iplon)
	 enddo
      enddo


! Clouds
         if (icld.eq.0) then
	 do iplon = 1, ncol
            zcldfmc(iplon,:,:) = 0._r8
            ztaucmc(iplon,:,:) = 0._r8
            ztaormc(iplon,:,:) = 0._r8
            zasycmc(iplon,:,:) = 0._r8
            zomgcmc(iplon,:,:) = 1._r8
	 enddo
         elseif (icld.ge.1) then
            do i=1,nlay
               do ig=1,ngptsw
	       do iplon = 1, ncol
                  zcldfmc(iplon,i,ig) = cldfmc(iplon,ig,i)
                  ztaucmc(iplon,i,ig) = taucmc(iplon,ig,i)
                  ztaormc(iplon,i,ig) = taormc(ig,i)
                  zasycmc(iplon,i,ig) = asmcmc(iplon,ig,i)
                  zomgcmc(iplon,i,ig) = ssacmc(iplon,ig,i)
               enddo
            enddo
         enddo
         endif   

! Aerosol
! IAER = 0: no aerosols
         if (iaer.eq.0) then
         do iplon = 1, ncol
            ztaua(iplon,:,:) = 0._r8
            zasya(iplon,:,:) = 0._r8
            zomga(iplon,:,:) = 1._r8
	 enddo
! IAER = 6: Use ECMWF six aerosol types. See rrsw_aer.f90 for details.
! Input aerosol optical thickness at 0.55 micron for each aerosol type (ecaer), 
! or set manually here for each aerosol and layer.
         elseif (iaer.eq.6) then

!            do i = 1, nlay
!               do ia = 1, naerec
!                  ecaer(iplon,i,ia) = 1.0e-15_r8
!               enddo
!            enddo

!            do i = 1, nlay
!               do ib = 1, nbndsw
!                  ztaua(i,ib) = 0._r8
!                  zasya(i,ib) = 0._r8
!                  zomga(i,ib) = 1._r8
!                  do ia = 1, naerec
!                     ztaua(i,ib) = ztaua(i,ib) + rsrtaua(ib,ia) * ecaer(iplon,i,ia)
!                     zomga(i,ib) = zomga(i,ib) + rsrtaua(ib,ia) * ecaer(iplon,i,ia) * &
!                                   rsrpiza(ib,ia)
!                     zasya(i,ib) = zasya(i,ib) + rsrtaua(ib,ia) * ecaer(iplon,i,ia) * &
!                                   rsrpiza(ib,ia) * rsrasya(ib,ia)
!                  enddo
!                  if (zomga(i,ib) /= 0._r8) then
!                     zasya(i,ib) = zasya(i,ib) / zomga(i,ib)
!                  endif
!                  if (ztaua(i,ib) /= 0._r8) then
!                     zomga(i,ib) = zomga(i,ib) / ztaua(i,ib)
!                  endif
!               enddo
!            enddo

! IAER=10: Direct specification of aerosol optical properties from GCM
         elseif (iaer.eq.10) then

         do i = 1 ,nlay
            do ib = 1 ,nbndsw
	       do iplon = 1, ncol
                  ztaua(iplon,i,ib) = taua(iplon,i,ib)
                  zasya(iplon,i,ib) = asma(iplon,i,ib)
                  zomga(iplon,i,ib) = ssaa(iplon,i,ib)
	       enddo
            enddo
         enddo
      endif


! Call the 2-stream radiation transfer model

	!!dir$ attributes offload:mic :: spcvmc_sw
      call spcvmc_sw &
             (lchnk, ncol, nlay, istart, iend, icpr, idelm, iout, &
              pavel, tavel, pz, tz, tbound, albdif, albdir, &
              zcldfmc, ztaucmc, zasycmc, zomgcmc, ztaormc, &
              ztaua, zasya, zomga, cossza, coldry, wkl, adjflux, &	 
              laytrop, layswtch, laylow, jp, jt, jt1, &
              co2mult, colch4, colco2, colh2o, colmol, coln2o, colo2, colo3, &
              fac00, fac01, fac10, fac11, &
              selffac, selffrac, indself, forfac, forfrac, indfor, &
              swdflx, swuflx, swdflxc, swuflxc, zuvfd, zuvcd, znifd, znicd, znifu, znicu, &
              zbbfddir, zbbcddir, dirdnuv, zuvcddir, dirdnir, znicddir, swuflxs, swdflxs)

! Transfer up and down, clear and total sky fluxes to output arrays.
! Vertical indexing goes from bottom to top

      do i = 1, nlay+1
         do iplon=1,ncol
!            swuflxs(:,iplon,i) = zbbfsu(iplon,:,i)
!            swdflxs(:,iplon,i) = zbbfsd(iplon,:,i)
            uvdflx(i) = zuvfd(iplon,i)
            nidflx(i) = znifd(iplon,i)
!  Direct/diffuse fluxes
            dirdflux(i) = zbbfddir(iplon,i)
            difdflux(i) = swdflx(iplon,i) - dirdflux(i)
!  UV/visible direct/diffuse fluxes
            difdnuv(iplon,i) = zuvfd(iplon,i) - dirdnuv(iplon,i)
!  Near-IR direct/diffuse fluxes
            difdnir(iplon,i) = znifd(iplon,i) - dirdnir(iplon,i)
!  Added for net near-IR diagnostic
            ninflx(iplon,i) = znifd(iplon,i) - znifu(iplon,i)
            ninflxc(iplon,i) = znicd(iplon,i) - znicu(iplon,i)
         enddo
      enddo
      do iplon=1,ncol
!  Total and clear sky net fluxes
         do i = 1, nlay+1
            swnflxc(i) = swdflxc(iplon,i) - swuflxc(iplon,i)
            swnflx(i) = swdflx(iplon,i) - swuflx(iplon,i)
         enddo

!  Total and clear sky heating rates
!  Heating units are in K/d. Flux units are in W/m2.
         do i = 1, nlay
            zdpgcp = heatfac / pdp(iplon,i)
            swhrc(iplon,i) = (swnflxc(i+1) - swnflxc(i)) * zdpgcp
            swhr(iplon,i) = (swnflx(i+1) - swnflx(i)) * zdpgcp
         enddo
         swhrc(iplon,nlay) = 0._r8
         swhr(iplon,nlay) = 0._r8

! End longitude loop
      enddo

      end subroutine rrtmg_sw

!*************************************************************************
!dir$ attributes offload:mic :: earth_sun
      real(kind=r8) function earth_sun(idn)
!*************************************************************************
!
!  Purpose: Function to calculate the correction factor of Earth's orbit
!  for current day of the year

!  idn        : Day of the year
!  earth_sun  : square of the ratio of mean to actual Earth-Sun distance

! ------- Modules -------
!!dir$ attributes offload:mic :: pi
      use rrsw_con, only : pi

      integer, intent(in) :: idn

      real(kind=r8) :: gamma

      gamma = 2._r8*pi*(idn-1)/365._r8

! Use Iqbal's equation 1.2.1

      earth_sun = 1.000110_r8 + .034221_r8 * cos(gamma) + .001289_r8 * sin(gamma) + &
                   .000719_r8 * cos(2._r8*gamma) + .000077_r8 * sin(2._r8*gamma)

      end function earth_sun

!***************************************************************************
!dir$ attributes offload:mic :: inatm_sw
      subroutine inatm_sw (istart, iend, nlay, icld, iaer, &
            play, plev, tlay, tlev, tsfc, &
            h2ovmr, o3vmr, co2vmr, ch4vmr, o2vmr, n2ovmr, adjes, dyofyr, solvar, &
            inflgsw, iceflgsw, liqflgsw, &
            cldfmcl, taucmcl, ssacmcl, asmcmcl, fsfcmcl, ciwpmcl, clwpmcl, &
            reicmcl, relqmcl, tauaer, ssaaer, asmaer, &
            pavel, pz, pdp, tavel, tz, tbound, coldry, wkl, &
            adjflux, inflag, iceflag, liqflag, cldfmc, taucmc, &
            ssacmc, asmcmc, fsfcmc, ciwpmc, clwpmc, reicmc, dgesmc, relqmc, &
            taua, ssaa, asma)
!***************************************************************************
!
!  Input atmospheric profile from GCM, and prepare it for use in RRTMG_SW.
!  Set other RRTMG_SW input parameters.  
!
!***************************************************************************

! --------- Modules ----------

      use parrrsw, only : nbndsw, ngptsw, nmol, mxmol, &
                          jpband, jpb1, jpb2
		!!dir$ attributes offload:mic :: avogad
		!!dir$ attributes offload:mic :: grav
      use rrsw_con, only : heatfac, oneminus, pi, grav, avogad
      use rrsw_wvn, only : nspa, nspb, wavenum2

! ------- Declarations -------

! ----- Input -----
      integer, intent(in) :: istart                     ! column start index
      integer, intent(in) :: iend                       ! column end index
      integer, intent(in) :: nlay                       ! number of model layers
      integer, intent(in) :: icld                       ! clear/cloud and cloud overlap flag
      integer, intent(in) :: iaer                       ! aerosol option flag

      real(kind=r8), intent(in) :: play(:,:)            ! Layer pressures (hPa, mb)
                                                        ! Dimensions: (ncol,nlay)
      real(kind=r8), intent(in) :: plev(:,:)            ! Interface pressures (hPa, mb)
                                                        ! Dimensions: (ncol,nlay+1)
      real(kind=r8), intent(in) :: tlay(:,:)            ! Layer temperatures (K)
                                                        ! Dimensions: (ncol,nlay)
      real(kind=r8), intent(in) :: tlev(:,:)            ! Interface temperatures (K)
                                                        ! Dimensions: (ncol,nlay+1)
      real(kind=r8), intent(in) :: tsfc(:)              ! Surface temperature (K)
                                                        ! Dimensions: (ncol)
      real(kind=r8), intent(in) :: h2ovmr(:,:)          ! H2O volume mixing ratio
                                                        ! Dimensions: (ncol,nlay)
      real(kind=r8), intent(in) :: o3vmr(:,:)           ! O3 volume mixing ratio
                                                        ! Dimensions: (ncol,nlay)
      real(kind=r8), intent(in) :: co2vmr(:,:)          ! CO2 volume mixing ratio
                                                        ! Dimensions: (ncol,nlay)
      real(kind=r8), intent(in) :: ch4vmr(:,:)          ! Methane volume mixing ratio
                                                        ! Dimensions: (ncol,nlay)
      real(kind=r8), intent(in) :: o2vmr(:,:)           ! O2 volume mixing ratio
                                                        ! Dimensions: (ncol,nlay)
      real(kind=r8), intent(in) :: n2ovmr(:,:)          ! Nitrous oxide volume mixing ratio
                                                        ! Dimensions: (ncol,nlay)

      integer, intent(in) :: dyofyr                     ! Day of the year (used to get Earth/Sun
                                                        !  distance if adjflx not provided)
      real(kind=r8), intent(in) :: adjes                ! Flux adjustment for Earth/Sun distance
      real(kind=r8), intent(in) :: solvar(jpb1:jpb2)    ! Solar constant (Wm-2) scaling per band

      integer, intent(in) :: inflgsw                    ! Flag for cloud optical properties
      integer, intent(in) :: iceflgsw                   ! Flag for ice particle specification
      integer, intent(in) :: liqflgsw                   ! Flag for liquid droplet specification

      real(kind=r8), intent(in) :: cldfmcl(:,:,:)       ! Cloud fraction
                                                        ! Dimensions: (ngptsw,ncol,nlay)
      real(kind=r8), intent(in) :: taucmcl(:,:,:)       ! Cloud optical depth (optional)
                                                        ! Dimensions: (ngptsw,ncol,nlay)
      real(kind=r8), intent(in) :: ssacmcl(:,:,:)       ! Cloud single scattering albedo
                                                        ! Dimensions: (ngptsw,ncol,nlay)
      real(kind=r8), intent(in) :: asmcmcl(:,:,:)       ! Cloud asymmetry parameter
                                                        ! Dimensions: (ngptsw,ncol,nlay)
      real(kind=r8), intent(in) :: fsfcmcl(:,:,:)       ! Cloud forward scattering fraction
                                                        ! Dimensions: (ngptsw,ncol,nlay)
      real(kind=r8), intent(in) :: ciwpmcl(:,:,:)       ! Cloud ice water path (g/m2)
                                                        ! Dimensions: (ngptsw,ncol,nlay)
      real(kind=r8), intent(in) :: clwpmcl(:,:,:)       ! Cloud liquid water path (g/m2)
                                                        ! Dimensions: (ngptsw,ncol,nlay)
      real(kind=r8), intent(in) :: reicmcl(:,:)         ! Cloud ice effective radius (microns)
                                                        ! Dimensions: (ncol,nlay)
      real(kind=r8), intent(in) :: relqmcl(:,:)         ! Cloud water drop effective radius (microns)
                                                        ! Dimensions: (ncol,nlay)

      real(kind=r8), intent(in) :: tauaer(:,:,:)        ! Aerosol optical depth
                                                        ! Dimensions: (ncol,nlay,nbndsw)
      real(kind=r8), intent(in) :: ssaaer(:,:,:)        ! Aerosol single scattering albedo
                                                        ! Dimensions: (ncol,nlay,nbndsw)
      real(kind=r8), intent(in) :: asmaer(:,:,:)        ! Aerosol asymmetry parameter
                                                        ! Dimensions: (ncol,nlay,nbndsw)

! Atmosphere

      real(kind=r8), intent(out) :: pavel(:,:)            ! layer pressures (mb)
                                                          ! Dimensions: (ncol,nlay)
      real(kind=r8), intent(out) :: tavel(:,:)            ! layer temperatures (K)
                                                          ! Dimensions: (ncol,nlay)
      real(kind=r8), intent(out) :: pz(:,0:)              ! level (interface) pressures (hPa, mb)
                                                          ! Dimensions: (ncol,0:nlay)
      real(kind=r8), intent(out) :: tz(:,0:)              ! level (interface) temperatures (K)
                                                          ! Dimensions: (ncol,0:nlay)
      real(kind=r8), intent(out) :: tbound(:)             ! surface temperature (K)
                                                          ! Dimensions: (ncol)
      real(kind=r8), intent(out) :: pdp(:,:)              ! layer pressure thickness (hPa, mb)
                                                          ! Dimensions: (ncol,nlay)
      real(kind=r8), intent(out) :: coldry(:,:)           ! dry air column density (mol/cm2)
                                                          ! Dimensions: (ncol,nlay)
      real(kind=r8), intent(out) :: wkl(:,:,:)            ! molecular amounts (mol/cm-2)
                                                          ! Dimensions: (ncol,mxmol,nlay)

      real(kind=r8), intent(out) :: adjflux(:,:)         ! adjustment for current Earth/Sun distance
                                                         ! Dimensions: (ncol,jpband)
!      real(kind=r8), intent(out) :: solvar(:)           ! solar constant scaling factor from rrtmg_sw
                                                         ! Dimensions: (jpband)
                                                         !  default value of 1368.22 Wm-2 at 1 AU
      real(kind=r8), intent(out) :: taua(:,:,:)          ! Aerosol optical depth
                                                         ! Dimensions: (ncol,nlay,nbndsw)
      real(kind=r8), intent(out) :: ssaa(:,:,:)          ! Aerosol single scattering albedo
                                                         ! Dimensions: (ncol,nlay,nbndsw)
      real(kind=r8), intent(out) :: asma(:,:,:)          ! Aerosol asymmetry parameter
                                                         ! Dimensions: (ncol,nlay,nbndsw)

! Atmosphere/clouds - cldprop
      integer, intent(out) :: inflag                    ! flag for cloud property method
                                                        ! Dimensions: (ncol)
      integer, intent(out) :: iceflag                   ! flag for ice cloud properties
                                                        ! Dimensions: (ncol)
      integer, intent(out) :: liqflag                   ! flag for liquid cloud properties
                                                        ! Dimensions: (ncol)
      real(kind=r8), intent(out) :: cldfmc(:,:,:)       ! layer cloud fraction
                                                        ! Dimensions: (ncol,ngptsw,nlay)
      real(kind=r8), intent(out) :: taucmc(:,:,:)       ! cloud optical depth (non-delta scaled)
                                                        ! Dimensions: (ncol,ngptsw,nlay)
      real(kind=r8), intent(out) :: ssacmc(:,:,:)       ! cloud single scattering albedo (non-delta-scaled)
                                                        ! Dimensions: (ncol,ngptsw,nlay)
      real(kind=r8), intent(out) :: asmcmc(:,:,:)       ! cloud asymmetry parameter (non-delta scaled)
      real(kind=r8), intent(out) :: fsfcmc(:,:,:)       ! cloud forward scattering fraction (non-delta scaled)
                                                        ! Dimensions: (ncol,ngptsw,nlay)
      real(kind=r8), intent(out) :: ciwpmc(:,:,:)       ! cloud ice water path
                                                        ! Dimensions: (ncol,ngptsw,nlay)
      real(kind=r8), intent(out) :: clwpmc(:,:,:)       ! cloud liquid water path
                                                        ! Dimensions: (ncol,ngptsw,nlay)
      real(kind=r8), intent(out) :: reicmc(:,:)         ! cloud ice particle effective radius
                                                        ! Dimensions: (ncol,nlay)
      real(kind=r8), intent(out) :: dgesmc(:,:)         ! cloud ice particle effective radius
                                                        ! Dimensions: (ncol,nlay)
      real(kind=r8), intent(out) :: relqmc(:,:)         ! cloud liquid particle size
                                                        ! Dimensions: (ncol,nlay)

! ----- Local -----
      real(kind=r8), parameter :: amd = 28.9660_r8      ! Effective molecular weight of dry air (g/mol)
      real(kind=r8), parameter :: amw = 18.0160_r8      ! Molecular weight of water vapor (g/mol)
!      real(kind=r8), parameter :: amc = 44.0098_r8      ! Molecular weight of carbon dioxide (g/mol)
!      real(kind=r8), parameter :: amo = 47.9998_r8      ! Molecular weight of ozone (g/mol)
!      real(kind=r8), parameter :: amo2 = 31.9999_r8     ! Molecular weight of oxygen (g/mol)
!      real(kind=r8), parameter :: amch4 = 16.0430_r8    ! Molecular weight of methane (g/mol)
!      real(kind=r8), parameter :: amn2o = 44.0128_r8    ! Molecular weight of nitrous oxide (g/mol)

! Set molecular weight ratios (for converting mmr to vmr)
!  e.g. h2ovmr = h2ommr * amdw)
      real(kind=r8), parameter :: amdw = 1.607793_r8    ! Molecular weight of dry air / water vapor
      real(kind=r8), parameter :: amdc = 0.658114_r8    ! Molecular weight of dry air / carbon dioxide
      real(kind=r8), parameter :: amdo = 0.603428_r8    ! Molecular weight of dry air / ozone
      real(kind=r8), parameter :: amdm = 1.805423_r8    ! Molecular weight of dry air / methane
      real(kind=r8), parameter :: amdn = 0.658090_r8    ! Molecular weight of dry air / nitrous oxide

      real(kind=r8), parameter :: sbc = 5.67e-08_r8     ! Stefan-Boltzmann constant (W/m2K4)

      integer :: isp, l, ix, n, imol, ib, ig, iplon   ! Loop indices
      real(kind=r8) :: amm, summol                      ! 
      real(kind=r8) :: adjflx                           ! flux adjustment for Earth/Sun distance
!      real(kind=r8) :: earth_sun                        ! function for Earth/Sun distance adjustment
!      real(kind=r8) :: solar_band_irrad(jpb1:jpb2) ! rrtmg assumed-solar irradiance in each sw band


! Set flux adjustment for current Earth/Sun distance (two options).
! 1) Use Earth/Sun distance flux adjustment provided by GCM (input as adjes);
      adjflx = adjes
!
! 2) Calculate Earth/Sun distance from DYOFYR, the cumulative day of the year.
!    (Set adjflx to 1. to use constant Earth/Sun distance of 1 AU). 
      if (dyofyr .gt. 0) then
	  !!dir$ attributes offload:mic :: earth_sun
         adjflx = earth_sun(dyofyr)
      endif

! Set incoming solar flux adjustment to include adjustment for
! current Earth/Sun distance (ADJFLX) and scaling of default internal
! solar constant (rrsw_scon = 1368.22 Wm-2) by band (SOLVAR).  SOLVAR can be set 
! to a single scaling factor as needed, or to a different value in each 
! band, which may be necessary for paleoclimate simulations. 
! 
      !DIR$ VECTOR ALIGNED
      !DIR$ SIMD
      do iplon=istart,iend
         adjflux(iplon,:) = 0._r8
      enddo
      do ib = jpb1,jpb2
         !DIR$ VECTOR ALIGNED
	 !DIR$ SIMD
	 do iplon=istart,iend
	    adjflux(iplon,ib) = adjflx * solvar(ib)
	 enddo
      enddo
      !DIR$ VECTOR ALIGNED
      !DIR$ SIMD
      do iplon=istart,iend
!  Set surface temperature.
         tbound(iplon) = tsfc(iplon)

!  Install input GCM arrays into RRTMG_SW arrays for pressure, temperature,
!  and molecular amounts.  
!  Pressures are input in mb, or are converted to mb here.
!  Molecular amounts are input in volume mixing ratio, or are converted from 
!  mass mixing ratio (or specific humidity for h2o) to volume mixing ratio
!  here. These are then converted to molecular amount (molec/cm2) below.  
!  The dry air column COLDRY (in molec/cm2) is calculated from the level 
!  pressures, pz (in mb), based on the hydrostatic equation and includes a 
!  correction to account for h2o in the layer.  The molecular weight of moist 
!  air (amm) is calculated for each layer.  
!  Note: In RRTMG, layer indexing goes from bottom to top, and coding below
!  assumes GCM input fields are also bottom to top. Input layer indexing
!  from GCM fields should be reversed here if necessary.
         pz(iplon,0) = plev(iplon,nlay+1)
         tz(iplon,0) = tlev(iplon,nlay+1)
      enddo

      do l = 1, nlay
         !DIR$ VECTOR ALIGNED
         !DIR$ SIMD
         do iplon=istart,iend
            pavel(iplon,l) = play(iplon,nlay-l+1)
            tavel(iplon,l) = tlay(iplon,nlay-l+1)
            pz(iplon,l) = plev(iplon,nlay-l+1)
            tz(iplon,l) = tlev(iplon,nlay-l+1)
            pdp(iplon,l) = pz(iplon,l-1) - pz(iplon,l)
         end do
      enddo
      do iplon=istart,iend
         do l = 1, nlay

! For h2o input in vmr:
            wkl(iplon,1,l) = h2ovmr(iplon,nlay-l+1)
! For h2o input in mmr:
           !wkl(1,l) = h2o(iplon,nlayers-l)*amdw
! For h2o input in specific humidity;
            !wkl(1,l) = (h2o(iplon,nlayers-l)/(1._r8 - h2o(iplon,nlayers-l)))*amdw
            wkl(iplon,2,l) = co2vmr(iplon,nlay-l+1)
            wkl(iplon,3,l) = o3vmr(iplon,nlay-l+1)
            wkl(iplon,4,l) = n2ovmr(iplon,nlay-l+1)
            wkl(iplon,5,l) = 0._r8
            wkl(iplon,6,l) = ch4vmr(iplon,nlay-l+1)
            wkl(iplon,7,l) = o2vmr(iplon,nlay-l+1) 
            amm = (1._r8 - wkl(iplon,1,l)) * amd + wkl(iplon,1,l) * amw            
            coldry(iplon,l) = (pz(iplon,l-1)-pz(iplon,l)) * 1.e3_r8 * avogad / &
                     (1.e2_r8 * grav * amm * (1._r8 + wkl(iplon,1,l)))
         enddo

         coldry(iplon,nlay) = (pz(iplon,nlay-1)) * 1.e3_r8 * avogad / &
                        (1.e2_r8 * grav * amm * (1._r8 + wkl(iplon,1,nlay-1)))

! At this point all molecular amounts in wkl are in volume mixing ratio;
! convert to molec/cm2 based on coldry for use in rrtm.

         do l = 1, nlay
            do imol = 1, nmol
               wkl(iplon,imol,l) = coldry(iplon,l) * wkl(iplon,imol,l)
            enddo
         enddo
      enddo

! Transfer aerosol optical properties to RRTM variables;
! modify to reverse layer indexing here if necessary.

      if (iaer .ge. 1) then 
         do l = 1, nlay-1
            do ib = 1, nbndsw
               do iplon=istart,iend
                  taua(iplon,l,ib) = tauaer(iplon,nlay-l,ib)
                  ssaa(iplon,l,ib) = ssaaer(iplon,nlay-l,ib)
                  asma(iplon,l,ib) = asmaer(iplon,nlay-l,ib)
               enddo
            enddo
         enddo
      endif

! Transfer cloud fraction and cloud optical properties to RRTM variables;
! modify to reverse layer indexing here if necessary.

      if (icld .ge. 1) then 
         do iplon=istart,iend
            inflag = inflgsw
            iceflag = iceflgsw
            liqflag = liqflgsw
         enddo
! Move incoming GCM cloud arrays to RRTMG cloud arrays.
! For GCM input, incoming reice is in effective radius; for Fu parameterization (iceflag = 3)
! convert effective radius to generalized effective size using method of Mitchell, JAS, 2002:

         do l = 1, nlay-1
            do ig = 1, ngptsw
               do iplon=istart,iend
                  cldfmc(iplon,ig,l) = cldfmcl(ig,iplon,nlay-l)
                  taucmc(iplon,ig,l) = taucmcl(ig,iplon,nlay-l)
                  ssacmc(iplon,ig,l) = ssacmcl(ig,iplon,nlay-l)
                  asmcmc(iplon,ig,l) = asmcmcl(ig,iplon,nlay-l)
                  fsfcmc(iplon,ig,l) = fsfcmcl(ig,iplon,nlay-l)
                  ciwpmc(iplon,ig,l) = ciwpmcl(ig,iplon,nlay-l)
                  clwpmc(iplon,ig,l) = clwpmcl(ig,iplon,nlay-l)
               enddo
            enddo
            do iplon=istart,iend
               reicmc(iplon,l) = reicmcl(iplon,nlay-l)
               if (iceflag .eq. 3) then
                  dgesmc(iplon,l) = 1.5396_r8 * reicmcl(iplon,nlay-l)
               endif
               relqmc(iplon,l) = relqmcl(iplon,nlay-l)
            enddo
         enddo
! If an extra layer is being used in RRTMG, set all cloud properties to zero in the extra layer.
         do iplon=istart,iend
            cldfmc(iplon,:,nlay) = 0.0_r8
            taucmc(iplon,:,nlay) = 0.0_r8
            ssacmc(iplon,:,nlay) = 1.0_r8
            asmcmc(iplon,:,nlay) = 0.0_r8
            fsfcmc(iplon,:,nlay) = 0.0_r8
            ciwpmc(iplon,:,nlay) = 0.0_r8
            clwpmc(iplon,:,nlay) = 0.0_r8
            reicmc(iplon,nlay) = 0.0_r8
            dgesmc(iplon,nlay) = 0.0_r8
            relqmc(iplon,nlay) = 0.0_r8
            taua(iplon,nlay,:) = 0.0_r8
            ssaa(iplon,nlay,:) = 1.0_r8
            asma(iplon,nlay,:) = 0.0_r8
         enddo
      endif

      end subroutine inatm_sw

      end module rrtmg_sw_rad


