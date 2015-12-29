
! KGEN-generated Fortran source file
!
! Filename    : radsw.F90
! Generated at: 2015-09-28 23:28:16
! KGEN version: 0.5.1



    MODULE radsw
        USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check
        !-----------------------------------------------------------------------
        !
        ! Purpose: Solar radiation calculations.
        !
        !-----------------------------------------------------------------------
		USE omp_lib
        USE shr_kind_mod, ONLY: r8 => shr_kind_r8
        USE ppgrid, ONLY: pcols
        USE parrrsw, ONLY: nbndsw
        USE parrrsw, ONLY: ngptsw
        USE rrtmg_sw_rad, ONLY: rrtmg_sw
        !use perf_mod,        only: t_startf, t_stopf
        IMPLICIT NONE
        PRIVATE
        PUBLIC rad_rrtmg_sw
        ! fraction of solar irradiance in each band
        ! rrtmg-assumed solar irradiance in each sw band
        ! Public methods
        ! initialize constants
        ! driver for solar radiation code
        !===============================================================================
        CONTAINS

        ! write subroutines
        ! No subroutines
        ! No module extern variables
        !===============================================================================

        SUBROUTINE rad_rrtmg_sw(lchnk, nday, eccf, rrtmg_levs, kgen_unit)
                USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check
				!use omp_lib
            !-----------------------------------------------------------------------
            !
            ! Purpose:
            ! Solar radiation code
            !
            ! Method:
            ! mji/rrtmg
            ! RRTMG, two-stream, with McICA
            !
            ! Divides solar spectrum into 14 intervals from 0.2-12.2 micro-meters.
            ! solar flux fractions specified for each interval. allows for
            ! seasonally and diurnally varying solar input.  Includes molecular,
            ! cloud, aerosol, and surface scattering, along with h2o,o3,co2,o2,cloud,
            ! and surface absorption. Computes delta-eddington reflections and
            ! transmissions assuming homogeneously mixed layers. Adds the layers
            ! assuming scattering between layers to be isotropic, and distinguishes
            ! direct solar beam from scattered radiation.
            !
            ! Longitude loops are broken into 1 or 2 sections, so that only daylight
            ! (i.e. coszrs > 0) computations are done.
            !
            ! Note that an extra layer above the model top layer is added.
            !
            ! mks units are used.
            !
            ! Special diagnostic calculation of the clear sky surface and total column
            ! absorbed flux is also done for cloud forcing diagnostics.
            !
            !-----------------------------------------------------------------------
            ! Minimum cloud amount (as a fraction of the grid-box area) to
            ! distinguish from clear sky
            integer, intent(in) :: kgen_unit
            INTEGER*8 :: kgen_intvar, start_clock, stop_clock, rate_clock, start_clock1, stop_clock1, rate_clock1
            INTEGER, PARAMETER :: maxiter=1000, number_of_threads=3, print_values=100
            character(len=80), parameter :: kname = "rrtmg_sw"
            TYPE(check_t):: check_status
            REAL(KIND=kgen_dp) :: tolerance
            ! Decimal precision of cloud amount (0 -> preserve full resolution;
            ! 10^-n -> preserve n digits of cloud amount)
            ! Input arguments
            INTEGER, intent(in) :: lchnk ! chunk identifier
            ! number of atmospheric columns
            INTEGER, intent(in) :: rrtmg_levs ! number of levels rad is applied
            INTEGER, intent(in) :: nday ! Number of daylight columns
            ! Number of night columns
            ! Indicies of daylight coumns
            ! Indicies of night coumns
            ! Level pressure (Pascals)
            ! Fractional cloud cover
            ! aerosol optical depth
            ! aerosol OD * ssa
            ! aerosol OD * ssa * asm
            ! aerosol OD * ssa * fwd
            REAL(KIND=r8), intent(in) :: eccf ! Eccentricity factor (1./earth-sun dist^2)
            ! Cosine solar zenith angle
            ! 0.2-0.7 micro-meter srfc alb: direct rad
            ! 0.7-5.0 micro-meter srfc alb: direct rad
            ! 0.2-0.7 micro-meter srfc alb: diffuse rad
            ! 0.7-5.0 micro-meter srfc alb: diffuse rad
            ! factor to account for solar variability in each band
            ! cloud optical depth
            ! cloud optical
            ! cloud optical
            ! cloud optical
            ! Output arguments
            ! Incident solar flux
            ! Solar heating rate
            ! Clearsky solar heating rate
            ! Surface absorbed solar flux
            ! Total column absorbed solar flux
            ! Net solar flux at TOA
            ! Upward solar flux at TOA
            ! Flux shortwave downwelling surface
            ! Clear sky surface absorbed solar flux
            ! Clear sky surface downwelling solar flux
            ! Clear sky total column absorbed solar flx
            ! Clear sky net solar flx at TOA
            ! Direct solar rad on surface (< 0.7)
            ! Direct solar rad on surface (>= 0.7)
            ! Diffuse solar rad on surface (< 0.7)
            ! Diffuse solar rad on surface (>= 0.7)
            ! Near-IR flux absorbed at toa
            ! Clear sky near-IR flux absorbed at toa
            ! Net near-IR flux at toa >= 0.7 microns
            ! net flux at interfaces
            ! net clear-sky flux at interfaces
            ! shortwave spectral flux up
            ! shortwave spectral flux down
            !---------------------------Local variables-----------------------------
            ! Local and reordered copies of the intent(in) variables
            ! Level pressure (Pascals)
            ! Fractional cloud cover
            ! in-cloud cloud ice water path
            ! in-cloud cloud liquid water path
            REAL(KIND=r8) :: rel(pcols,rrtmg_levs-1) ! Liquid effective drop size (microns)
            REAL(KIND=r8) :: rei(pcols,rrtmg_levs-1) ! Ice effective drop size (microns)
            REAL(KIND=r8) :: coszrs(pcols) ! Cosine solar zenith angle
            REAL(KIND=r8) :: asdir(pcols) ! 0.2-0.7 micro-meter srfc alb: direct rad
            REAL(KIND=r8) :: aldir(pcols) ! 0.7-5.0 micro-meter srfc alb: direct rad
            REAL(KIND=r8) :: asdif(pcols) ! 0.2-0.7 micro-meter srfc alb: diffuse rad
            REAL(KIND=r8) :: aldif(pcols) ! 0.7-5.0 micro-meter srfc alb: diffuse rad
            REAL(KIND=r8) :: h2ovmr(pcols,rrtmg_levs) ! h2o volume mixing ratio
            REAL(KIND=r8) :: o3vmr(pcols,rrtmg_levs) ! o3 volume mixing ratio
            REAL(KIND=r8) :: co2vmr(pcols,rrtmg_levs) ! co2 volume mixing ratio
            REAL(KIND=r8) :: ch4vmr(pcols,rrtmg_levs) ! ch4 volume mixing ratio
            REAL(KIND=r8) :: o2vmr(pcols,rrtmg_levs) ! o2  volume mixing ratio
            REAL(KIND=r8) :: n2ovmr(pcols,rrtmg_levs) ! n2o volume mixing ratio
            REAL(KIND=r8) :: tsfc(pcols) ! surface temperature
            INTEGER :: inflgsw ! flag for cloud parameterization method
            INTEGER :: iceflgsw ! flag for ice cloud parameterization method
            INTEGER :: liqflgsw ! flag for liquid cloud parameterization method
            INTEGER :: icld
            INTEGER :: ref_icld ! Flag for cloud overlap method
            ! 0=clear, 1=random, 2=maximum/random, 3=maximum
            INTEGER :: dyofyr ! Set to day of year for Earth/Sun distance calculation in
            ! rrtmg_sw, or pass in adjustment directly into adjes
            REAL(KIND=r8) :: solvar(nbndsw) ! solar irradiance variability in each band
            INTEGER, parameter :: nsubcsw = ngptsw ! rrtmg_sw g-point (quadrature point) dimension
            ! permute seed for sub-column generator
            ! cloud optical depth - diagnostic temp variable
            ! cloud optical depth
            ! cloud single scat. albedo
            ! cloud asymmetry parameter
            ! cloud forward scattering fraction
            REAL(KIND=r8) :: tau_aer_sw(pcols, rrtmg_levs-1, nbndsw) ! aer optical depth
            REAL(KIND=r8) :: ssa_aer_sw(pcols, rrtmg_levs-1, nbndsw) ! aer single scat. albedo
            REAL(KIND=r8) :: asm_aer_sw(pcols, rrtmg_levs-1, nbndsw) ! aer asymmetry parameter
            REAL(KIND=r8) :: cld_stosw(nsubcsw, pcols, rrtmg_levs-1) ! stochastic cloud fraction
            ! stochastic ice particle size
            ! stochastic liquid particle size
            REAL(KIND=r8) :: cicewp_stosw(nsubcsw, pcols, rrtmg_levs-1) ! stochastic cloud ice water path
            REAL(KIND=r8) :: cliqwp_stosw(nsubcsw, pcols, rrtmg_levs-1) ! stochastic cloud liquid wter path
            REAL(KIND=r8) :: tauc_stosw(nsubcsw, pcols, rrtmg_levs-1) ! stochastic cloud optical depth (optional)
            REAL(KIND=r8) :: ssac_stosw(nsubcsw, pcols, rrtmg_levs-1) ! stochastic cloud single scat. albedo (optional)
            REAL(KIND=r8) :: asmc_stosw(nsubcsw, pcols, rrtmg_levs-1) ! stochastic cloud asymmetry parameter (optional)
            REAL(KIND=r8) :: fsfc_stosw(nsubcsw, pcols, rrtmg_levs-1) ! stochastic cloud forward scattering fraction (optional)
            ! Inverse of seconds per day
            REAL(KIND=r8) :: swuflx(pcols,rrtmg_levs+1)
            REAL(KIND=r8) :: ref_swuflx(pcols,rrtmg_levs+1) ! Total sky shortwave upward flux (W/m2)
            REAL(KIND=r8) :: swdflx(pcols,rrtmg_levs+1)
            REAL(KIND=r8) :: ref_swdflx(pcols,rrtmg_levs+1) ! Total sky shortwave downward flux (W/m2)
            REAL(KIND=r8) :: swhr(pcols,rrtmg_levs)
            REAL(KIND=r8) :: ref_swhr(pcols,rrtmg_levs) ! Total sky shortwave radiative heating rate (K/d)
            REAL(KIND=r8) :: swuflxc(pcols,rrtmg_levs+1)
            REAL(KIND=r8) :: ref_swuflxc(pcols,rrtmg_levs+1) ! Clear sky shortwave upward flux (W/m2)
            REAL(KIND=r8) :: swdflxc(pcols,rrtmg_levs+1)
            REAL(KIND=r8) :: ref_swdflxc(pcols,rrtmg_levs+1) ! Clear sky shortwave downward flux (W/m2)
            REAL(KIND=r8) :: swhrc(pcols,rrtmg_levs)
            REAL(KIND=r8) :: ref_swhrc(pcols,rrtmg_levs) ! Clear sky shortwave radiative heating rate (K/d)
            REAL(KIND=r8) :: swuflxs(nbndsw,pcols,rrtmg_levs+1)
            REAL(KIND=r8) :: ref_swuflxs(nbndsw,pcols,rrtmg_levs+1) ! Shortwave spectral flux up
            REAL(KIND=r8) :: swdflxs(nbndsw,pcols,rrtmg_levs+1)
            REAL(KIND=r8) :: ref_swdflxs(nbndsw,pcols,rrtmg_levs+1) ! Shortwave spectral flux down
            REAL(KIND=r8) :: dirdnuv(pcols,rrtmg_levs+1)
            REAL(KIND=r8) :: ref_dirdnuv(pcols,rrtmg_levs+1) ! Direct downward shortwave flux, UV/vis
            REAL(KIND=r8) :: difdnuv(pcols,rrtmg_levs+1)
            REAL(KIND=r8) :: ref_difdnuv(pcols,rrtmg_levs+1) ! Diffuse downward shortwave flux, UV/vis
            REAL(KIND=r8) :: dirdnir(pcols,rrtmg_levs+1)
            REAL(KIND=r8) :: ref_dirdnir(pcols,rrtmg_levs+1) ! Direct downward shortwave flux, near-IR
            REAL(KIND=r8) :: difdnir(pcols,rrtmg_levs+1)
            REAL(KIND=r8) :: ref_difdnir(pcols,rrtmg_levs+1) ! Diffuse downward shortwave flux, near-IR
            ! Added for net near-IR diagnostic
            REAL(KIND=r8) :: ninflx(pcols,rrtmg_levs+1)
            REAL(KIND=r8) :: ref_ninflx(pcols,rrtmg_levs+1) ! Net shortwave flux, near-IR
            REAL(KIND=r8) :: ninflxc(pcols,rrtmg_levs+1)
            REAL(KIND=r8) :: ref_ninflxc(pcols,rrtmg_levs+1) ! Net clear sky shortwave flux, near-IR
            ! Other
            ! indices
            ! Cloud radiative property arrays
            ! water cloud extinction optical depth
            ! ice cloud extinction optical depth
            ! liquid cloud single scattering albedo
            ! liquid cloud asymmetry parameter
            ! liquid cloud forward scattered fraction
            ! ice cloud single scattering albedo
            ! ice cloud asymmetry parameter
            ! ice cloud forward scattered fraction
            ! Aerosol radiative property arrays
            ! aerosol extinction optical depth
            ! aerosol single scattering albedo
            ! aerosol assymetry parameter
            ! aerosol forward scattered fraction
            ! CRM
            ! Upward flux (added for CRM)
            ! Downward flux (added for CRM)
            ! Upward clear-sky flux (added for CRM)
            ! Downward clear-sky flux (added for CRM)
            REAL(KIND=r8) :: pmidmb(pcols,rrtmg_levs) ! Level pressure (hPa)
            REAL(KIND=r8) :: pintmb(pcols,rrtmg_levs+1) ! Model interface pressure (hPa)
            REAL(KIND=r8) :: tlay(pcols,rrtmg_levs) ! mid point temperature
            REAL(KIND=r8) :: tlev(pcols,rrtmg_levs+1) ! interface temperature
            REAL(KIND=r8) :: inatm_sw_elapsedTime=0.0
            REAL(KIND=r8) :: cldprmc_sw_elapsedTime=0.0
            REAL(KIND=r8) :: setcoef_sw_elapsedTime=0.0
            REAL(KIND=r8) :: spcvmc_sw_elapsedTime=0.0
            REAL(KIND=r8) :: reftra_sw_elapsedTime=0.0
            REAL(KIND=r8) :: taumol_sw_elapsedTime=0.0
            REAL(KIND=r8) :: vrtqdr_sw_elapsedTime=0.0


            !-----------------------------------------------------------------------
            ! START OF CALCULATION
            !-----------------------------------------------------------------------
            ! Initialize output fields:
            ! If night everywhere, return:
            ! Rearrange input arrays
            ! These fields are no longer input by 1.
            ! Aerosol daylight map
            ! Also convert to optical properties of rrtmg interface, even though
            !   these quantities are later multiplied back together inside rrtmg !
            ! Why does rrtmg use the factored quantities?
            ! There are several different ways this factoring could be done.
            ! Other ways might allow for better optimization
            ! Define solar incident radiation
            ! Calculate cloud optical properties here if using 1 method, or if using one of the
            ! methods in RRTMG_SW, then pass in cloud physical properties and zero out cloud optical
            ! properties here
            ! Zero optional cloud optical property input arrays tauc_sw, ssac_sw, asmc_sw,
            ! if inputting cloud physical properties to RRTMG_SW
            !tauc_sw(:,:,:) = 0.0_r8
            !ssac_sw(:,:,:) = 1.0_r8
            !asmc_sw(:,:,:) = 0.0_r8
            !fsfc_sw(:,:,:) = 0.0_r8
            !
            ! Or, calculate and pass in 1 cloud shortwave optical properties to RRTMG_SW
            !if (present(old_convert)) print *, 'old_convert',old_convert
            !if (present(ancientmethod)) print *, 'ancientmethod',ancientmethod
            ! Call mcica sub-column generator for RRTMG_SW
            ! Call sub-column generator for McICA in radiation
            !call t_startf('mcica_subcol_sw')
            ! Select cloud overlap approach (1=random, 2=maximum-random, 3=maximum)
            ! Set permute seed (must be offset between LW and SW by at least 140 to insure
            ! effective randomization)
            !call t_stopf('mcica_subcol_sw')
            !call t_startf('rrtmg_sw')
            ! Call RRTMG_SW for all layers for daylight columns
            ! Select parameterization of cloud ice and liquid optical depths
            ! Use 1 shortwave cloud optical properties directly
            ! Use E&C param for ice to mimic CAM3 for now
            !   inflgsw = 2
            !   iceflgsw = 1
            !   liqflgsw = 1
            ! Use merged Fu and E&C params for ice
            !   inflgsw = 2
            !   iceflgsw = 3
            !   liqflgsw = 1
            ! Set day of year for Earth/Sun distance calculation in rrtmg_sw, or
            ! set to zero and pass E/S adjustment (eccf) directly into array adjes
            tolerance = 1.E-10
            CALL kgen_init_check(check_status, tolerance)
            READ(UNIT=kgen_unit) icld
            READ(UNIT=kgen_unit) pmidmb
            READ(UNIT=kgen_unit) pintmb
            READ(UNIT=kgen_unit) tlay
            READ(UNIT=kgen_unit) tlev
            READ(UNIT=kgen_unit) tsfc
            READ(UNIT=kgen_unit) h2ovmr
            READ(UNIT=kgen_unit) o3vmr
            READ(UNIT=kgen_unit) co2vmr
            READ(UNIT=kgen_unit) ch4vmr
            READ(UNIT=kgen_unit) o2vmr
            READ(UNIT=kgen_unit) n2ovmr
            READ(UNIT=kgen_unit) asdir
            READ(UNIT=kgen_unit) asdif
            READ(UNIT=kgen_unit) aldir
            READ(UNIT=kgen_unit) aldif
            READ(UNIT=kgen_unit) coszrs
            READ(UNIT=kgen_unit) dyofyr
            READ(UNIT=kgen_unit) solvar
            READ(UNIT=kgen_unit) inflgsw
            READ(UNIT=kgen_unit) iceflgsw
            READ(UNIT=kgen_unit) liqflgsw
            READ(UNIT=kgen_unit) cld_stosw
            READ(UNIT=kgen_unit) tauc_stosw
            READ(UNIT=kgen_unit) ssac_stosw
            READ(UNIT=kgen_unit) asmc_stosw
            READ(UNIT=kgen_unit) fsfc_stosw
            READ(UNIT=kgen_unit) cicewp_stosw
            READ(UNIT=kgen_unit) cliqwp_stosw
            READ(UNIT=kgen_unit) rei
            READ(UNIT=kgen_unit) rel
            READ(UNIT=kgen_unit) tau_aer_sw
            READ(UNIT=kgen_unit) ssa_aer_sw
            READ(UNIT=kgen_unit) asm_aer_sw
            READ(UNIT=kgen_unit) swuflx
            READ(UNIT=kgen_unit) swdflx
            READ(UNIT=kgen_unit) swhr
            READ(UNIT=kgen_unit) swuflxc
            READ(UNIT=kgen_unit) swdflxc
            READ(UNIT=kgen_unit) swhrc
            READ(UNIT=kgen_unit) dirdnuv
            READ(UNIT=kgen_unit) dirdnir
            READ(UNIT=kgen_unit) difdnuv
            READ(UNIT=kgen_unit) difdnir
            READ(UNIT=kgen_unit) ninflx
            READ(UNIT=kgen_unit) ninflxc
            READ(UNIT=kgen_unit) swuflxs
            READ(UNIT=kgen_unit) swdflxs

            READ(UNIT=kgen_unit) ref_icld
            READ(UNIT=kgen_unit) ref_swuflx
            READ(UNIT=kgen_unit) ref_swdflx
            READ(UNIT=kgen_unit) ref_swhr
            READ(UNIT=kgen_unit) ref_swuflxc
            READ(UNIT=kgen_unit) ref_swdflxc
            READ(UNIT=kgen_unit) ref_swhrc
            READ(UNIT=kgen_unit) ref_dirdnuv
            READ(UNIT=kgen_unit) ref_dirdnir
            READ(UNIT=kgen_unit) ref_difdnuv
            READ(UNIT=kgen_unit) ref_difdnir
            READ(UNIT=kgen_unit) ref_ninflx
            READ(UNIT=kgen_unit) ref_ninflxc
            READ(UNIT=kgen_unit) ref_swuflxs
            READ(UNIT=kgen_unit) ref_swdflxs
			
	!Transfer data to Co-processor
	!!dir$ offload_transfer target(mic:3) in(lchnk, nday, rrtmg_levs, icld, pmidmb, pintmb, tlay, tlev, tsfc, &
	!!dir$ h2ovmr, o3vmr, co2vmr, ch4vmr, o2vmr, n2ovmr, asdir, asdif, aldir, aldif, coszrs, eccf, dyofyr, solvar, &
	!!dir$ inflgsw, iceflgsw, liqflgsw, cld_stosw, tauc_stosw, ssac_stosw, asmc_stosw, fsfc_stosw, cicewp_stosw, cliqwp_stosw, rei, rel, &
	!!dir$ tau_aer_sw, ssa_aer_sw, asm_aer_sw, swuflx, swdflx, swhr, swuflxc, swdflxc, swhrc, dirdnuv, dirdnir, difdnuv, difdnir, ninflx, ninflxc, swuflxs, swdflxs) &
	!!dir$ signal(lchnk)
			
            ! call to kernel
			WRITE(*,*) 'The number of threads is :',omp_get_num_threads()
			CALL system_clock(start_clock1, rate_clock1)
   call rrtmg_sw(lchnk, Nday, rrtmg_levs, icld,         &
                 pmidmb, pintmb, tlay, tlev, tsfc, &
                 h2ovmr, o3vmr, co2vmr, ch4vmr, o2vmr, n2ovmr, &
                 asdir, asdif, aldir, aldif, &
                 coszrs, eccf, dyofyr, solvar, &
                 inflgsw, iceflgsw, liqflgsw, &
                 cld_stosw, tauc_stosw, ssac_stosw, asmc_stosw, fsfc_stosw, &
                 cicewp_stosw, cliqwp_stosw, rei, rel, &
                 tau_aer_sw, ssa_aer_sw, asm_aer_sw, &
                 swuflx, swdflx, swhr, swuflxc, swdflxc, swhrc, &
                 dirdnuv, dirdnir, difdnuv, difdnir, ninflx, ninflxc, swuflxs, swdflxs)
				 CALL system_clock(stop_clock1, rate_clock1)
				 WRITE(*,*)
				 
				 !Transfer data to Co-processor
	!!dir$ offload_transfer target(mic:3) in(lchnk, nday, rrtmg_levs, icld, pmidmb, pintmb, tlay, tlev, tsfc, &
	!!dir$ h2ovmr, o3vmr, co2vmr, ch4vmr, o2vmr, n2ovmr, asdir, asdif, aldir, aldif, coszrs, eccf, dyofyr, solvar, &
	!!dir$ inflgsw, iceflgsw, liqflgsw, cld_stosw, tauc_stosw, ssac_stosw, asmc_stosw, fsfc_stosw, cicewp_stosw, cliqwp_stosw, rei, rel, &
	!!dir$ tau_aer_sw, ssa_aer_sw, asm_aer_sw, swuflx, swdflx, swhr, swuflxc, swdflxc, swhrc, dirdnuv, dirdnir, difdnuv, difdnir, ninflx, ninflxc, swuflxs, swdflxs) &
	!!dir$ signal(lchnk)
				 
            PRINT *, "rrtmg_sw : Time per call (usec) one call: ", 1.0e6*(stop_clock1 - start_clock1)/REAL(rate_clock1)
            ! kernel verification for output variables
            CALL kgen_verify_integer( "icld", check_status, icld, ref_icld)
            CALL kgen_verify_real_r8_dim2( "swuflx", check_status, swuflx, ref_swuflx)
            CALL kgen_verify_real_r8_dim2( "swdflx", check_status, swdflx, ref_swdflx)
            CALL kgen_verify_real_r8_dim2( "swhr", check_status, swhr, ref_swhr)
            CALL kgen_verify_real_r8_dim2( "swuflxc", check_status, swuflxc, ref_swuflxc)
            CALL kgen_verify_real_r8_dim2( "swdflxc", check_status, swdflxc, ref_swdflxc)
            CALL kgen_verify_real_r8_dim2( "swhrc", check_status, swhrc, ref_swhrc)
            CALL kgen_verify_real_r8_dim2( "dirdnuv", check_status, dirdnuv, ref_dirdnuv)
            CALL kgen_verify_real_r8_dim2( "dirdnir", check_status, dirdnir, ref_dirdnir)
            CALL kgen_verify_real_r8_dim2( "difdnuv", check_status, difdnuv, ref_difdnuv)
            CALL kgen_verify_real_r8_dim2( "difdnir", check_status, difdnir, ref_difdnir)
            CALL kgen_verify_real_r8_dim2( "ninflx", check_status, ninflx, ref_ninflx)
            CALL kgen_verify_real_r8_dim2( "ninflxc", check_status, ninflxc, ref_ninflxc)
            CALL kgen_verify_real_r8_dim3( "swuflxs", check_status, swuflxs, ref_swuflxs)
            CALL kgen_verify_real_r8_dim3( "swdflxs", check_status, swdflxs, ref_swdflxs)
            CALL kgen_print_check("rrtmg_sw", check_status)
            inatm_sw_elapsedTime=0.0
            cldprmc_sw_elapsedTime=0.0
            setcoef_sw_elapsedTime=0.0
            spcvmc_sw_elapsedTime=0.0
            reftra_sw_elapsedTime=0.0
            taumol_sw_elapsedTime=0.0
            vrtqdr_sw_elapsedTime=0.0
            
			!Transfer data to Co-processor
	!dir$ offload_transfer target(mic:3) in(lchnk, nday, rrtmg_levs, icld, pmidmb, pintmb, tlay, tlev, tsfc, &
	!dir$ h2ovmr, o3vmr, co2vmr, ch4vmr, o2vmr, n2ovmr, asdir, asdif, aldir, aldif, coszrs, eccf, dyofyr, solvar, &
	!dir$ inflgsw, iceflgsw, liqflgsw, cld_stosw, tauc_stosw, ssac_stosw, asmc_stosw, fsfc_stosw, cicewp_stosw, cliqwp_stosw, rei, rel, &
	!dir$ tau_aer_sw, ssa_aer_sw, asm_aer_sw, swuflx, swdflx, swhr, swuflxc, swdflxc, swhrc, dirdnuv, dirdnir, difdnuv, difdnir, ninflx, ninflxc, swuflxs, swdflxs) &
	!dir$ signal(lchnk)
			
	!Call the procedure in coprocessor
	!!dir$ attributes offload:mic :: rrtmg_sw
			
            WRITE(*,*) 'The maximum number of threads in xeon is :',OMP_get_max_threads()
			CALL system_clock(start_clock, rate_clock)
			!dir$ offload begin target(mic:3) wait(lchnk)
			WRITE(*,*) 'The maximum number of threads before set in phi is :',OMP_get_max_threads()
			WRITE(*,*) 'The number of threads before set using is :',OMP_get_num_threads()
			call OMP_set_num_threads(200)
			!WRITE(*,*) 'The maximum number of threads in phi is :',OMP_get_max_threads()
			!WRITE(*,*) 'The number of threads using is :',OMP_get_num_threads()
			!$omp parallel do 
			!WRITE(*,*) 'The maximum number of threads in phi is :',OMP_get_max_threads()
			!WRITE(*,*) 'The number of threads using is :',OMP_get_num_threads()
            DO kgen_intvar=1,maxiter
			if (kgen_intvar==(maxiter-250)) Then
			WRITE(*,*) 'The maximum number of threads in phi is :',OMP_get_max_threads()
			WRITE(*,*) 'The total number of threads using is :',OMP_get_num_threads()
			WRITE(*,*) 'The thread id :',OMP_get_thread_num()
			end if
                CALL rrtmg_sw(lchnk, nday, rrtmg_levs, icld, pmidmb, pintmb, tlay, tlev, tsfc, h2ovmr, o3vmr, co2vmr, ch4vmr, o2vmr, n2ovmr, asdir, asdif, aldir, aldif, coszrs, eccf, dyofyr, solvar, inflgsw, iceflgsw, liqflgsw, cld_stosw, tauc_stosw, ssac_stosw, asmc_stosw, fsfc_stosw, cicewp_stosw, cliqwp_stosw, rei, rel, tau_aer_sw, ssa_aer_sw, asm_aer_sw, swuflx, swdflx, swhr, swuflxc, swdflxc, swhrc, dirdnuv, dirdnir, difdnuv, difdnir, ninflx, ninflxc, swuflxs, swdflxs)
            END DO
			!$omp end parallel do
			!dir$ end offload
            CALL system_clock(stop_clock, rate_clock)
            WRITE(*,*)
            PRINT *, "rrtmg_sw : Time per call (usec): ", 1.0e6*(stop_clock - start_clock)/REAL(rate_clock*maxiter)
			
#ifdef INATMSW
            PRINT *, TRIM(kname), ": Elapsed time for inatm_sw (usec):",1.0e6*inatm_sw_elapsedTime/REAL(maxiter)
#else
#endif
#ifdef CLDPRMCSW
            PRINT *, TRIM(kname), ": Elapsed time for cldprmc_sw (usec):",1.0e6*cldprmc_sw_elapsedTime/REAL(maxiter)
#else
#endif
#ifdef SETCOEFSW
            PRINT *, TRIM(kname), ": Elapsed time for setcoef_sw(usec):",1.0e6*setcoef_sw_elapsedTime/REAL(maxiter)
#else
#endif
#ifdef SPCVMCSW
            PRINT *, TRIM(kname), ": Elapsed time for spcvmc_sw (usec):",1.0e6*spcvmc_sw_elapsedTime/REAL(maxiter)
#else
#endif
#ifdef REFTRASW
            PRINT *, TRIM(kname), ": Elapsed time for reftra_sw(usec):",1.0e6*reftra_sw_elapsedTime/REAL(maxiter)
#else
#endif
#ifdef TAUMOLSW
            PRINT *, TRIM(kname), ": Elapsed time for taumol_sw(usec):",1.0e6*taumol_sw_elapsedTime/REAL(maxiter)
#else
#endif
#ifdef VRTQDRSW
            PRINT *, TRIM(kname), ": Elapsed time for vrtqdr_sw(usec):",1.0e6*vrtqdr_sw_elapsedTime/REAL(maxiter)
#else
#endif
            ! Flux units are in W/m2 on output from rrtmg_sw and contain output for
            ! extra layer above model top with vertical indexing from bottom to top.
            !
            ! Heating units are in J/kg/s on output from rrtmg_sw and contain output
            ! for extra layer above model top with vertical indexing from bottom to top.
            !
            ! Reverse vertical indexing to go from top to bottom for 1 output.
            ! Set the net absorted shortwave flux at TOA (top of extra layer)
            ! Set net near-IR flux at top of the model
            ! Set the net absorbed shortwave flux at the model top level
            ! Set the downwelling flux at the surface
            ! Set the net shortwave flux at the surface
            ! Set the UV/vis and near-IR direct and dirruse downward shortwave flux at surface
            ! Set the net, up and down fluxes at model interfaces
            ! Set solar heating, reverse layering
            ! Pass shortwave heating to 1 arrays and convert from K/d to J/kg/s
            ! Set spectral fluxes, reverse layering
            ! order=(/3,1,2/) maps the first index of swuflxs to the third index of su.
            !call t_stopf('rrtmg_sw')
            ! Rearrange output arrays.
            !
            ! intent(out)
            !  these outfld calls don't work for spmd only outfield in scm mode (nonspmd)
        CONTAINS

        ! write subroutines
            SUBROUTINE kgen_read_real_r8_dim2(var, kgen_unit, printvar)
                INTEGER, INTENT(IN) :: kgen_unit
                CHARACTER(*), INTENT(IN), OPTIONAL :: printvar
                real(KIND=r8), INTENT(OUT), ALLOCATABLE, DIMENSION(:,:) :: var
                LOGICAL :: is_true
                INTEGER :: idx1,idx2
                INTEGER, DIMENSION(2,2) :: kgen_bound

                READ(UNIT = kgen_unit) is_true

                IF ( is_true ) THEN
                    READ(UNIT = kgen_unit) kgen_bound(1, 1)
                    READ(UNIT = kgen_unit) kgen_bound(2, 1)
                    READ(UNIT = kgen_unit) kgen_bound(1, 2)
                    READ(UNIT = kgen_unit) kgen_bound(2, 2)
                    ALLOCATE(var(kgen_bound(2, 1) - kgen_bound(1, 1) + 1, kgen_bound(2, 2) - kgen_bound(1, 2) + 1))
                    READ(UNIT = kgen_unit) var
                    IF ( PRESENT(printvar) ) THEN
                        PRINT *, "** KGEN DEBUG: " // printvar // " **", var
                    END IF
                ELSE
                    IF ( PRESENT(printvar) ) THEN
                        PRINT *, "** KGEN DEBUG: " // printvar // " ** is NOT present"
                    END IF
                END IF
            END SUBROUTINE kgen_read_real_r8_dim2

            SUBROUTINE kgen_read_real_r8_dim3(var, kgen_unit, printvar)
                INTEGER, INTENT(IN) :: kgen_unit
                CHARACTER(*), INTENT(IN), OPTIONAL :: printvar
                real(KIND=r8), INTENT(OUT), ALLOCATABLE, DIMENSION(:,:,:) :: var
                LOGICAL :: is_true
                INTEGER :: idx1,idx2,idx3
                INTEGER, DIMENSION(2,3) :: kgen_bound

                READ(UNIT = kgen_unit) is_true

                IF ( is_true ) THEN
                    READ(UNIT = kgen_unit) kgen_bound(1, 1)
                    READ(UNIT = kgen_unit) kgen_bound(2, 1)
                    READ(UNIT = kgen_unit) kgen_bound(1, 2)
                    READ(UNIT = kgen_unit) kgen_bound(2, 2)
                    READ(UNIT = kgen_unit) kgen_bound(1, 3)
                    READ(UNIT = kgen_unit) kgen_bound(2, 3)
                    ALLOCATE(var(kgen_bound(2, 1) - kgen_bound(1, 1) + 1, kgen_bound(2, 2) - kgen_bound(1, 2) + 1, kgen_bound(2, 3) - kgen_bound(1, 3) + 1))
                    READ(UNIT = kgen_unit) var
                    IF ( PRESENT(printvar) ) THEN
                        PRINT *, "** KGEN DEBUG: " // printvar // " **", var
                    END IF
                ELSE
                    IF ( PRESENT(printvar) ) THEN
                        PRINT *, "** KGEN DEBUG: " // printvar // " ** is NOT present"
                    END IF
                END IF
            END SUBROUTINE kgen_read_real_r8_dim3


        ! verify subroutines
            SUBROUTINE kgen_verify_integer( varname, check_status, var, ref_var)
                character(*), intent(in) :: varname
                type(check_t), intent(inout) :: check_status
                integer, intent(in) :: var, ref_var
                check_status%numTotal = check_status%numTotal + 1
                IF ( var == ref_var ) THEN
                    check_status%numIdentical = check_status%numIdentical + 1
                    if(check_status%verboseLevel > 1) then
                        WRITE(*,*)
                        WRITE(*,*) trim(adjustl(varname)), " is IDENTICAL( ", var, " )."
                    endif
                ELSE
                    if(check_status%verboseLevel > 0) then
                        WRITE(*,*)
                        WRITE(*,*) trim(adjustl(varname)), " is NOT IDENTICAL."
                        if(check_status%verboseLevel > 2) then
                            WRITE(*,*) "KERNEL: ", var
                            WRITE(*,*) "REF.  : ", ref_var
                        end if
                    end if
                    check_status%numFatal = check_status%numFatal + 1
                END IF
            END SUBROUTINE kgen_verify_integer

            SUBROUTINE kgen_verify_real_r8_dim2( varname, check_status, var, ref_var)
                character(*), intent(in) :: varname
                type(check_t), intent(inout) :: check_status
                real(KIND=r8), intent(in), DIMENSION(:,:) :: var, ref_var
                real(KIND=r8) :: nrmsdiff, rmsdiff
                real(KIND=r8), allocatable, DIMENSION(:,:) :: temp, temp2
                integer :: n
                check_status%numTotal = check_status%numTotal + 1
                IF ( ALL( var == ref_var ) ) THEN
                
                    check_status%numIdentical = check_status%numIdentical + 1            
                    if(check_status%verboseLevel > 1) then
                        WRITE(*,*)
                        WRITE(*,*) "All elements of ", trim(adjustl(varname)), " are IDENTICAL."
                        !WRITE(*,*) "KERNEL: ", var
                        !WRITE(*,*) "REF.  : ", ref_var
                        IF ( ALL( var == 0 ) ) THEN
                            if(check_status%verboseLevel > 2) then
                                WRITE(*,*) "All values are zero."
                            end if
                        END IF
                    end if
                ELSE
                    allocate(temp(SIZE(var,dim=1),SIZE(var,dim=2)))
                    allocate(temp2(SIZE(var,dim=1),SIZE(var,dim=2)))
                
                    n = count(var/=ref_var)
                    where(abs(ref_var) > check_status%minvalue)
                        temp  = ((var-ref_var)/ref_var)**2
                        temp2 = (var-ref_var)**2
                    elsewhere
                        temp  = (var-ref_var)**2
                        temp2 = temp
                    endwhere
                    nrmsdiff = sqrt(sum(temp)/real(n))
                    rmsdiff = sqrt(sum(temp2)/real(n))
                
                    if(check_status%verboseLevel > 0) then
                        WRITE(*,*)
                        WRITE(*,*) trim(adjustl(varname)), " is NOT IDENTICAL."
                        WRITE(*,*) count( var /= ref_var), " of ", size( var ), " elements are different."
                        if(check_status%verboseLevel > 1) then
                            WRITE(*,*) "Average - kernel ", sum(var)/real(size(var))
                            WRITE(*,*) "Average - reference ", sum(ref_var)/real(size(ref_var))
                        endif
                        WRITE(*,*) "RMS of difference is ",rmsdiff
                        WRITE(*,*) "Normalized RMS of difference is ",nrmsdiff
                    end if
                
                    if (nrmsdiff > check_status%tolerance) then
                        check_status%numFatal = check_status%numFatal+1
                    else
                        check_status%numWarning = check_status%numWarning+1
                    endif
                
                    deallocate(temp,temp2)
                END IF
            END SUBROUTINE kgen_verify_real_r8_dim2

            SUBROUTINE kgen_verify_real_r8_dim3( varname, check_status, var, ref_var)
                character(*), intent(in) :: varname
                type(check_t), intent(inout) :: check_status
                real(KIND=r8), intent(in), DIMENSION(:,:,:) :: var, ref_var
                real(KIND=r8) :: nrmsdiff, rmsdiff
                real(KIND=r8), allocatable, DIMENSION(:,:,:) :: temp, temp2
                integer :: n
                check_status%numTotal = check_status%numTotal + 1
                IF ( ALL( var == ref_var ) ) THEN
                
                    check_status%numIdentical = check_status%numIdentical + 1            
                    if(check_status%verboseLevel > 1) then
                        WRITE(*,*)
                        WRITE(*,*) "All elements of ", trim(adjustl(varname)), " are IDENTICAL."
                        !WRITE(*,*) "KERNEL: ", var
                        !WRITE(*,*) "REF.  : ", ref_var
                        IF ( ALL( var == 0 ) ) THEN
                            if(check_status%verboseLevel > 2) then
                                WRITE(*,*) "All values are zero."
                            end if
                        END IF
                    end if
                ELSE
                    allocate(temp(SIZE(var,dim=1),SIZE(var,dim=2),SIZE(var,dim=3)))
                    allocate(temp2(SIZE(var,dim=1),SIZE(var,dim=2),SIZE(var,dim=3)))
                
                    n = count(var/=ref_var)
                    where(abs(ref_var) > check_status%minvalue)
                        temp  = ((var-ref_var)/ref_var)**2
                        temp2 = (var-ref_var)**2
                    elsewhere
                        temp  = (var-ref_var)**2
                        temp2 = temp
                    endwhere
                    nrmsdiff = sqrt(sum(temp)/real(n))
                    rmsdiff = sqrt(sum(temp2)/real(n))
                
                    if(check_status%verboseLevel > 0) then
                        WRITE(*,*)
                        WRITE(*,*) trim(adjustl(varname)), " is NOT IDENTICAL."
                        WRITE(*,*) count( var /= ref_var), " of ", size( var ), " elements are different."
                        if(check_status%verboseLevel > 1) then
                            WRITE(*,*) "Average - kernel ", sum(var)/real(size(var))
                            WRITE(*,*) "Average - reference ", sum(ref_var)/real(size(ref_var))
                        endif
                        WRITE(*,*) "RMS of difference is ",rmsdiff
                        WRITE(*,*) "Normalized RMS of difference is ",nrmsdiff
                    end if
                
                    if (nrmsdiff > check_status%tolerance) then
                        check_status%numFatal = check_status%numFatal+1
                    else
                        check_status%numWarning = check_status%numWarning+1
                    endif
                
                    deallocate(temp,temp2)
                END IF
            END SUBROUTINE kgen_verify_real_r8_dim3
			
			
        END SUBROUTINE rad_rrtmg_sw
        !-------------------------------------------------------------------------------

        !-------------------------------------------------------------------------------
    END MODULE radsw
