
! KGEN-generated Fortran source file
!
! Filename    : rrtmg_sw_taumol.f90
! Generated at: 2015-07-31 20:45:42
! KGEN version: 0.4.13



    MODULE rrtmg_sw_taumol
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
        ! ------- Modules -------
        USE shr_kind_mod, ONLY: r8 => shr_kind_r8
        !      use parkind, only : jpim, jprb
        !      use parrrsw, only : mg, jpband, nbndsw, ngptsw
        USE rrsw_con, ONLY: oneminus
        USE rrsw_wvn, ONLY: nspa
        USE rrsw_wvn, ONLY: nspb
        USE rrsw_vsn, ONLY: hvrtau
        IMPLICIT NONE
        CONTAINS

        ! write subroutines
        ! No subroutines
        ! No module extern variables
        !----------------------------------------------------------------------------

		!dir$ attributes offload:mic :: taumol_sw
      subroutine taumol_sw(ncol, nlayers, &
                           colh2o, colco2, colch4, colo2, colo3, colmol, &
                           laytrop, jp, jt, jt1, &
                           fac00, fac01, fac10, fac11, &
                           selffac, selffrac, indself, forfac, forfrac, indfor, &
                           sfluxzen, taug, taur)
!----------------------------------------------------------------------------

! ******************************************************************************
! *                                                                            *
! *                 Optical depths developed for the                           *
! *                                                                            *
! *               RAPID RADIATIVE TRANSFER MODEL (RRTM)                        *
! *                                                                            *
! *                                                                            *
! *           ATMOSPHERIC AND ENVIRONMENTAL RESEARCH, INC.                     *
! *                       131 HARTWELL AVENUE                                  *
! *                       LEXINGTON, MA 02421                                  *
! *                                                                            *
! *                                                                            *
! *                          ELI J. MLAWER                                     *
! *                        JENNIFER DELAMERE                                   *
! *                        STEVEN J. TAUBMAN                                   *
! *                        SHEPARD A. CLOUGH                                   *
! *                                                                            *
! *                                                                            *
! *                                                                            *
! *                                                                            *
! *                      email:  mlawer@aer.com                                *
! *                      email:  jdelamer@aer.com                              *
! *                                                                            *
! *       The authors wish to acknowledge the contributions of the             *
! *       following people:  Patrick D. Brown, Michael J. Iacono,              *
! *       Ronald E. Farren, Luke Chen, Robert Bergstrom.                       *
! *                                                                            *
! ******************************************************************************
! *    TAUMOL                                                                  *
! *                                                                            *
! *    This file contains the subroutines TAUGBn (where n goes from            *
! *    1 to 28).  TAUGBn calculates the optical depths and Planck fractions    *
! *    per g-value and layer for band n.                                       *
! *                                                                            *
! * Output:  optical depths (unitless)                                         *
! *          fractions needed to compute Planck functions at every layer       *
! *              and g-value                                                   *
! *                                                                            *
! *    COMMON /TAUGCOM/  TAUG(MXLAY,MG)                                        *
! *    COMMON /PLANKG/   FRACS(MXLAY,MG)                                       *
! *                                                                            *
! * Input                                                                      *
! *                                                                            *
! *    PARAMETER (MG=16, MXLAY=203, NBANDS=14)                                 *
! *                                                                            *
! *    COMMON /FEATURES/ NG(NBANDS),NSPA(NBANDS),NSPB(NBANDS)                  *
! *    COMMON /PRECISE/  ONEMINUS                                              *
! *    COMMON /PROFILE/  NLAYERS,PAVEL(MXLAY),TAVEL(MXLAY),                    *
! *   &                  PZ(0:MXLAY),TZ(0:MXLAY),TBOUND                        *
! *    COMMON /PROFDATA/ LAYTROP,LAYSWTCH,LAYLOW,                              *
! *   &                  COLH2O(MXLAY),COLCO2(MXLAY),                          *
! *   &                  COLO3(MXLAY),COLN2O(MXLAY),COLCH4(MXLAY),             *
! *   &                  COLO2(MXLAY),CO2MULT(MXLAY)                           *
! *    COMMON /INTFAC/   FAC00(MXLAY),FAC01(MXLAY),                            *
! *   &                  FAC10(MXLAY),FAC11(MXLAY)                             *
! *    COMMON /INTIND/   JP(MXLAY),JT(MXLAY),JT1(MXLAY)                        *
! *    COMMON /SELF/     SELFFAC(MXLAY), SELFFRAC(MXLAY), INDSELF(MXLAY)       *
! *                                                                            *
! *    Description:                                                            *
! *    NG(IBAND) - number of g-values in band IBAND                            *
! *    NSPA(IBAND) - for the lower atmosphere, the number of reference         *
! *                  atmospheres that are stored for band IBAND per            *
! *                  pressure level and temperature.  Each of these            *
! *                  atmospheres has different relative amounts of the         *
! *                  key species for the band (i.e. different binary           *
! *                  species parameters).                                      *
! *    NSPB(IBAND) - same for upper atmosphere                                 *
! *    ONEMINUS - since problems are caused in some cases by interpolation     *
! *               parameters equal to or greater than 1, for these cases       *
! *               these parameters are set to this value, slightly < 1.        *
! *    PAVEL - layer pressures (mb)                                            *
! *    TAVEL - layer temperatures (degrees K)                                  *
! *    PZ - level pressures (mb)                                               *
! *    TZ - level temperatures (degrees K)                                     *
! *    LAYTROP - layer at which switch is made from one combination of         *
! *              key species to another                                        *
! *    COLH2O, COLCO2, COLO3, COLN2O, COLCH4 - column amounts of water         *
! *              vapor,carbon dioxide, ozone, nitrous ozide, methane,          *
! *              respectively (molecules/cm**2)                                *
! *    CO2MULT - for bands in which carbon dioxide is implemented as a         *
! *              trace species, this is the factor used to multiply the        *
! *              band's average CO2 absorption coefficient to get the added    *
! *              contribution to the optical depth relative to 355 ppm.        *
! *    FACij(LAY) - for layer LAY, these are factors that are needed to        *
! *                 compute the interpolation factors that multiply the        *
! *                 appropriate reference k-values.  A value of 0 (1) for      *
! *                 i,j indicates that the corresponding factor multiplies     *
! *                 reference k-value for the lower (higher) of the two        *
! *                 appropriate temperatures, and altitudes, respectively.     *
! *    JP - the index of the lower (in altitude) of the two appropriate        *
! *         reference pressure levels needed for interpolation                 *
! *    JT, JT1 - the indices of the lower of the two appropriate reference     *
! *              temperatures needed for interpolation (for pressure           *
! *              levels JP and JP+1, respectively)                             *
! *    SELFFAC - scale factor needed to water vapor self-continuum, equals     *
! *              (water vapor density)/(atmospheric density at 296K and        *
! *              1013 mb)                                                      *
! *    SELFFRAC - factor needed for temperature interpolation of reference     *
! *               water vapor self-continuum data                              *
! *    INDSELF - index of the lower of the two appropriate reference           *
! *              temperatures needed for the self-continuum interpolation      *
! *                                                                            *
! * Data input                                                                 *
! *    COMMON /Kn/ KA(NSPA(n),5,13,MG), KB(NSPB(n),5,13:59,MG), SELFREF(10,MG) *
! *       (note:  n is the band number)                                        *
! *                                                                            *
! *    Description:                                                            *
! *    KA - k-values for low reference atmospheres (no water vapor             *
! *         self-continuum) (units: cm**2/molecule)                            *
! *    KB - k-values for high reference atmospheres (all sources)              *
! *         (units: cm**2/molecule)                                            *
! *    SELFREF - k-values for water vapor self-continuum for reference         *
! *              atmospheres (used below LAYTROP)                              *
! *              (units: cm**2/molecule)                                       *
! *                                                                            *
! *    DIMENSION ABSA(65*NSPA(n),MG), ABSB(235*NSPB(n),MG)                     *
! *    EQUIVALENCE (KA,ABSA),(KB,ABSB)                                         *
! *                                                                            *
! *****************************************************************************
!
! Modifications
!
! Revised: Adapted to F90 coding, J.-J.Morcrette, ECMWF, Feb 2003
! Revised: Modified for g-point reduction, MJIacono, AER, Dec 2003
! Revised: Reformatted for consistency with rrtmg_lw, MJIacono, AER, Jul 2006
!
! ------- Declarations -------

! ----- Input -----
      integer, intent(in) :: nlayers            ! total number of layers
      integer, intent(in) :: ncol
      integer, intent(in) :: laytrop            ! tropopause layer index
      integer, intent(in) :: jp(ncol,nlayers)              ! 
                                                           !   Dimensions: (nlayers)
      integer, intent(in) :: jt(ncol,nlayers)              !
                                                           !   Dimensions: (nlayers)
      integer, intent(in) :: jt1(ncol,nlayers)             !
                                                           !   Dimensions: (nlayers)

      real(kind=r8), intent(in) :: colh2o(ncol,nlayers)             ! column amount (h2o)
                                                           !   Dimensions: (nlayers)
      real(kind=r8), intent(in) :: colco2(ncol,nlayers)             ! column amount (co2)
                                                           !   Dimensions: (nlayers)
      real(kind=r8), intent(in) :: colo3(ncol,nlayers)              ! column amount (o3)
                                                           !   Dimensions: (nlayers)
      real(kind=r8), intent(in) :: colch4(ncol,nlayers)             ! column amount (ch4)
                                                           !   Dimensions: (nlayers)
                                                           !   Dimensions: (nlayers)
      real(kind=r8), intent(in) :: colo2(ncol,nlayers)              ! column amount (o2)
                                                           !   Dimensions: (nlayers)
      real(kind=r8), intent(in) :: colmol(ncol,nlayers)             ! 
                                                           !   Dimensions: (nlayers)

      integer, intent(in) :: indself(ncol,nlayers)    
                                                           !   Dimensions: (nlayers)
      integer, intent(in) :: indfor(ncol,nlayers)
                                                           !   Dimensions: (nlayers)
      real(kind=r8), intent(in) :: selffac(ncol,nlayers)
                                                           !   Dimensions: (nlayers)
      real(kind=r8), intent(in) :: selffrac(ncol,nlayers)
                                                           !   Dimensions: (nlayers)
      real(kind=r8), intent(in) :: forfac(ncol,nlayers)
                                                           !   Dimensions: (nlayers)
      real(kind=r8), intent(in) :: forfrac(ncol,nlayers)
                                                           !   Dimensions: (nlayers)

      real(kind=r8), intent(in) :: &                     !
                         fac00(ncol,nlayers), fac01(ncol,nlayers), &             !   Dimensions: (ncol,nlayers)
                         fac10(ncol,nlayers), fac11(ncol,nlayers) 

! ----- Output -----
      real(kind=r8), intent(out) :: sfluxzen(:,:)          ! solar source function
                                                           !   Dimensions: (ngptsw)
      real(kind=r8), intent(out) :: taug(:,:,:)            ! gaseous optical depth
                                                           !   Dimensions: (nlayers,ngptsw)
      real(kind=r8), intent(out) :: taur(:,:,:)            ! Rayleigh 
                                                           !   Dimensions: (nlayers,ngptsw)
!      real(kind=r8), intent(out) :: ssa(:,:)             ! single scattering albedo (inactive)
                                                           !   Dimensions: (nlayers,ngptsw)

      hvrtau = '$Revision: 1.2 $'

! Calculate gaseous optical depth and planck fractions for each spectral band.

!!dir$ attributes offload:mic :: TAUMOL16
      call taumol16
	  !!dir$ attributes offload:mic :: taumol17
      call taumol17
	  !!dir$ attributes offload:mic :: taumol18
      call taumol18
	  !!dir$ attributes offload:mic :: taumol19
      call taumol19
	  !!dir$ attributes offload:mic :: taumol20
      call taumol20
	  !!dir$ attributes offload:mic :: taumol21
      call taumol21
	  !!dir$ attributes offload:mic :: taumol22
      call taumol22
	  !!dir$ attributes offload:mic :: taumol23
      call taumol23
	  !!dir$ attributes offload:mic :: taumol24
      call taumol24
	  !!dir$ attributes offload:mic :: taumol25
      call taumol25
	  !!dir$ attributes offload:mic :: taumol26
      call taumol26
	  !!dir$ attributes offload:mic :: taumol27
      call taumol27
	  !!dir$ attributes offload:mic :: taumol28
      call taumol28
	  !!dir$ attributes offload:mic :: taumol29
      call taumol29

!-------------
      contains
!-------------

!----------------------------------------------------------------------------
!dir$ attributes offload:mic :: TAUMOL16
      subroutine taumol16
!----------------------------------------------------------------------------
!
!     band 16:  2600-3250 cm-1 (low - h2o,ch4; high - ch4)
!
!----------------------------------------------------------------------------

! ------- Modules -------

      use parrrsw, only : ng16
	  !!dir$ attributes offload:mic :: absa, absb, forref, selfref, &
       !!dir$ sfluxref, rayl, layreffr, strrat1
      use rrsw_kg16, only : absa, absb, forref, selfref, &
                            sfluxref, rayl, layreffr, strrat1

! ------- Declarations -------

! Local

      integer :: ig, lay, laysolfr, iplon
      integer, dimension(ncol) :: ind0, ind1, inds, indf, js
      real(kind=r8), dimension(ncol) :: fac000, fac001, fac010, fac011, fac100, fac101, &
                         fac110, fac111, fs, speccomb, specmult, specparm, &
                         tauray

! Compute the optical depth by interpolating in ln(pressure), 
! temperature, and appropriate species.  Below LAYTROP, the water
! vapor self-continuum is interpolated (in temperature) separately.  

! Lower atmosphere loop
      do lay = 1, laytrop
         !dir$ SIMD
         do iplon =1,ncol
            speccomb(iplon) = colh2o(iplon,lay) + strrat1*colch4(iplon,lay)
            specparm(iplon) = colh2o(iplon,lay)/speccomb(iplon)
            if (specparm(iplon) .ge. oneminus) specparm(iplon) = oneminus
            specmult(iplon) = 8._r8*(specparm(iplon))
         enddo
         !dir$ SIMD
         do iplon =1,ncol
            js(iplon) = 1 + int(specmult(iplon))
            fs(iplon) = mod(specmult(iplon), 1._r8 )
         enddo
         !dir$ SIMD
         do iplon =1,ncol
            fac000(iplon) = (1._r8 - fs(iplon)) * fac00(iplon,lay)
            fac010(iplon) = (1._r8 - fs(iplon)) * fac10(iplon,lay)
            fac100(iplon) = fs(iplon) * fac00(iplon,lay)
            fac110(iplon) = fs(iplon) * fac10(iplon,lay)
            fac001(iplon) = (1._r8 - fs(iplon)) * fac01(iplon,lay)
            fac011(iplon) = (1._r8 - fs(iplon)) * fac11(iplon,lay)
            fac101(iplon) = fs(iplon) * fac01(iplon,lay)
            fac111(iplon) = fs(iplon) * fac11(iplon,lay)
            ind0(iplon) = ((jp(iplon,lay)-1)*5+(jt(iplon,lay)-1))*nspa(16) + js(iplon)
            ind1(iplon) = (jp(iplon,lay)*5+(jt1(iplon,lay)-1))*nspa(16) + js(iplon)
            inds(iplon) = indself(iplon,lay)
            indf(iplon) = indfor(iplon,lay)
            tauray(iplon) = colmol(iplon,lay) * rayl
         enddo

         do ig = 1, ng16
            !dir$ SIMD
            do iplon=1, ncol
               taug(iplon,lay,ig) = speccomb(iplon) * &
                   (fac000(iplon) * absa(ind0(iplon)   ,ig) + &
                    fac100(iplon) * absa(ind0(iplon) +1,ig) + &
                    fac010(iplon) * absa(ind0(iplon) +9,ig) + &
                    fac110(iplon) * absa(ind0(iplon)+10,ig) + &
                    fac001(iplon) * absa(ind1(iplon)   ,ig) + &
                    fac101(iplon) * absa(ind1(iplon) +1,ig) + &
                    fac011(iplon) * absa(ind1(iplon) +9,ig) + &
                    fac111(iplon) * absa(ind1(iplon)+10,ig)) + &
                    colh2o(iplon,lay) * &
                    (selffac(iplon,lay) * (selfref(inds(iplon),ig) + &
                    selffrac(iplon,lay) * &
                    (selfref(inds(iplon)+1,ig) - selfref(inds(iplon),ig))) + &
                    forfac(iplon,lay) * (forref(indf(iplon),ig) + &
                    forfrac(iplon,lay) * &
                    (forref(indf(iplon)+1,ig) - forref(indf(iplon),ig)))) 
!            ssa(lay,ig) = tauray/taug(lay,ig)
               taur(iplon,lay,ig) = tauray(iplon)
            enddo
         enddo
      enddo

      laysolfr = nlayers

! Upper atmosphere loop
      do lay = laytrop+1, nlayers
         !dir$ SIMD
         do iplon =1,ncol
            if (jp(iplon,lay-1) .lt. layreffr .and. jp(iplon,lay) .ge. layreffr) &
               laysolfr = lay
         enddo
         do iplon =1,ncol
            ind0(iplon) = ((jp(iplon,lay)-13)*5+(jt(iplon,lay)-1))*nspb(16) + 1
            ind1(iplon) = ((jp(iplon,lay)-12)*5+(jt1(iplon,lay)-1))*nspb(16) + 1
            tauray(iplon) = colmol(iplon,lay) * rayl
         enddo

         do ig = 1, ng16
            !dir$ SIMD
            do iplon =1,ncol
               taug(iplon,lay,ig) = colch4(iplon,lay) * &
                   (fac00(iplon,lay) * absb(ind0(iplon)  ,ig) + &
                    fac10(iplon,lay) * absb(ind0(iplon)+1,ig) + &
                    fac01(iplon,lay) * absb(ind1(iplon)  ,ig) + &
                    fac11(iplon,lay) * absb(ind1(iplon)+1,ig)) 
!            ssa(lay,ig) = tauray/taug(lay,ig)
            enddo
            !dir$ SIMD
            do iplon =1,ncol
               if (lay .eq. laysolfr) sfluxzen(iplon,ig) = sfluxref(ig)
               taur(iplon,lay,ig) = tauray(iplon)

            enddo

         enddo

      enddo

      end subroutine taumol16

!----------------------------------------------------------------------------
!dir$ attributes offload:mic :: taumol17
      subroutine taumol17
!----------------------------------------------------------------------------
!
!     band 17:  3250-4000 cm-1 (low - h2o,co2; high - h2o,co2)
!
!----------------------------------------------------------------------------

! ------- Modules -------

      use parrrsw, only : ng17, ngs16
      use rrsw_kg17, only : absa, absb, forref, selfref, &
                            sfluxref, rayl, layreffr, strrat

! ------- Declarations -------

! Local

      integer :: ig, lay, laysolfr, iplon
      integer, dimension(ncol) :: ind0, ind1, inds, indf, js
      real(kind=r8), dimension(ncol) :: fac000, fac001, fac010, fac011, fac100, fac101, &
                         fac110, fac111, fs, speccomb, specmult, specparm, &
                         tauray

! Compute the optical depth by interpolating in ln(pressure), 
! temperature, and appropriate species.  Below LAYTROP, the water
! vapor self-continuum is interpolated (in temperature) separately.  

! Lower atmosphere loop
      do lay = 1, laytrop
         !dir$ SIMD
         do iplon =1,ncol
            speccomb(iplon) = colh2o(iplon,lay) + strrat*colco2(iplon,lay)
            specparm(iplon) = colh2o(iplon,lay)/speccomb(iplon)
            if (specparm(iplon) .ge. oneminus) specparm(iplon) = oneminus
            specmult(iplon) = 8._r8*(specparm(iplon))
         enddo
         !dir$ SIMD
         do iplon =1,ncol
            js(iplon) = 1 + int(specmult(iplon))
            fs(iplon) = mod(specmult(iplon), 1._r8 )
         enddo
         !dir$ SIMD
         do iplon =1,ncol
            fac000(iplon) = (1._r8 - fs(iplon)) * fac00(iplon,lay)
            fac010(iplon) = (1._r8 - fs(iplon)) * fac10(iplon,lay)
            fac100(iplon) = fs(iplon) * fac00(iplon,lay)
            fac110(iplon) = fs(iplon) * fac10(iplon,lay)
            fac001(iplon) = (1._r8 - fs(iplon)) * fac01(iplon,lay)
            fac011(iplon) = (1._r8 - fs(iplon)) * fac11(iplon,lay)
            fac101(iplon) = fs(iplon) * fac01(iplon,lay)
            fac111(iplon) = fs(iplon) * fac11(iplon,lay)
            ind0(iplon) = ((jp(iplon,lay)-1)*5+(jt(iplon,lay)-1))*nspa(17) + js(iplon)
            ind1(iplon) = (jp(iplon,lay)*5+(jt1(iplon,lay)-1))*nspa(17) + js(iplon)
            inds(iplon) = indself(iplon,lay)
            indf(iplon) = indfor(iplon,lay)
            tauray(iplon) = colmol(iplon,lay) * rayl
         enddo

         do ig = 1, ng17
            do iplon =1,ncol
               taug(iplon,lay,ngs16+ig) = speccomb(iplon) * &
                   (fac000(iplon) * absa(ind0(iplon),ig) + &
                    fac100(iplon) * absa(ind0(iplon)+1,ig) + &
                    fac010(iplon) * absa(ind0(iplon)+9,ig) + &
                    fac110(iplon) * absa(ind0(iplon)+10,ig) + &
                    fac001(iplon) * absa(ind1(iplon),ig) + &
                    fac101(iplon) * absa(ind1(iplon)+1,ig) + &
                    fac011(iplon) * absa(ind1(iplon)+9,ig) + &
                    fac111(iplon) * absa(ind1(iplon)+10,ig)) + &
                    colh2o(iplon,lay) * &
                    (selffac(iplon,lay) * (selfref(inds(iplon),ig) + &
                    selffrac(iplon,lay) * &
                    (selfref(inds(iplon)+1,ig) - selfref(inds(iplon),ig))) + &
                    forfac(iplon,lay) * (forref(indf(iplon),ig) + &
                    forfrac(iplon,lay) * &
                    (forref(indf(iplon)+1,ig) - forref(indf(iplon),ig)))) 
!             ssa(lay,ngs16+ig) = tauray/taug(lay,ngs16+ig)
               taur(iplon,lay,ngs16+ig) = tauray(iplon)
            enddo
         enddo
      enddo
      laysolfr = nlayers

! Upper atmosphere loop
      do lay = laytrop+1, nlayers
         !dir$ SIMD
         do iplon =1,ncol
            if (jp(iplon,lay-1) .lt. layreffr .and. jp(iplon,lay) .ge. layreffr) &
               laysolfr = lay
            speccomb(iplon) = colh2o(iplon,lay) + strrat*colco2(iplon,lay)
            specparm(iplon) = colh2o(iplon,lay)/speccomb(iplon)
            if (specparm(iplon) .ge. oneminus) specparm(iplon) = oneminus
            specmult(iplon) = 4._r8*(specparm(iplon))
         enddo
         !dir$ SIMD
         do iplon =1,ncol
            js(iplon) = 1 + int(specmult(iplon))
            fs(iplon) = mod(specmult(iplon), 1._r8 )
         enddo
         !dir$ SIMD
         do iplon =1,ncol
            fac000(iplon) = (1._r8 - fs(iplon)) * fac00(iplon,lay)
            fac010(iplon) = (1._r8 - fs(iplon)) * fac10(iplon,lay)
            fac100(iplon) = fs(iplon) * fac00(iplon,lay)
            fac110(iplon) = fs(iplon) * fac10(iplon,lay)
            fac001(iplon) = (1._r8 - fs(iplon)) * fac01(iplon,lay)
            fac011(iplon) = (1._r8 - fs(iplon)) * fac11(iplon,lay)
            fac101(iplon) = fs(iplon) * fac01(iplon,lay)
            fac111(iplon) = fs(iplon) * fac11(iplon,lay)
            ind0(iplon) = ((jp(iplon,lay)-13)*5+(jt(iplon,lay)-1))*nspb(17) + js(iplon)
            ind1(iplon) = ((jp(iplon,lay)-12)*5+(jt1(iplon,lay)-1))*nspb(17) + js(iplon)
            indf(iplon) = indfor(iplon,lay)
            tauray(iplon) = colmol(iplon,lay) * rayl
         enddo
         do ig = 1, ng17
            !dir$ SIMD
            do iplon =1,ncol
               taug(iplon,lay,ngs16+ig) = speccomb(iplon) * &
                   (fac000(iplon) * absb(ind0(iplon),ig) + &
                    fac100(iplon) * absb(ind0(iplon)+1,ig) + &
                    fac010(iplon) * absb(ind0(iplon)+5,ig) + &
                    fac110(iplon) * absb(ind0(iplon)+6,ig) + &
                    fac001(iplon) * absb(ind1(iplon),ig) + &
                    fac101(iplon) * absb(ind1(iplon)+1,ig) + &
                    fac011(iplon) * absb(ind1(iplon)+5,ig) + &
                    fac111(iplon) * absb(ind1(iplon)+6,ig)) + &
                    colh2o(iplon,lay) * &
                    forfac(iplon,lay) * (forref(indf(iplon),ig) + &
                    forfrac(iplon,lay) * &
                    (forref(indf(iplon)+1,ig) - forref(indf(iplon),ig)))
!            ssa(lay,ngs16+ig) = tauray/taug(lay,ngs16+ig)
            enddo
            !dir$ SIMD
            do iplon =1,ncol
               if (lay .eq. laysolfr) sfluxzen(iplon,ngs16+ig) = sfluxref(ig,js(iplon)) &
                   + fs(iplon) * (sfluxref(ig,js(iplon)+1) - sfluxref(ig,js(iplon)))
            enddo
            !dir$ SIMD
            do iplon =1,ncol
               taur(iplon,lay,ngs16+ig) = tauray(iplon)
            enddo
         enddo
      enddo
      end subroutine taumol17

!----------------------------------------------------------------------------
!dir$ attributes offload:mic :: taumol18
      subroutine taumol18
!----------------------------------------------------------------------------
!
!     band 18:  4000-4650 cm-1 (low - h2o,ch4; high - ch4)
!
!----------------------------------------------------------------------------

! ------- Modules -------

      use parrrsw, only : ng18, ngs17
      use rrsw_kg18, only : absa, absb, forref, selfref, &
                            sfluxref, rayl, layreffr, strrat

! ------- Declarations -------

! Local

      integer :: ig, lay,  iplon
      integer, dimension(ncol) :: ind0, ind1, inds, indf, js, laysolfr
      real(kind=r8), dimension(ncol) :: fac000, fac001, fac010, fac011, fac100, fac101, &
                         fac110, fac111, fs, speccomb, specmult, specparm, &
                         tauray

! Compute the optical depth by interpolating in ln(pressure), 
! temperature, and appropriate species.  Below LAYTROP, the water
! vapor self-continuum is interpolated (in temperature) separately.  

      !dir$ SIMD
      do iplon =1,ncol
         laysolfr(iplon) = laytrop
      end do
      
! Lower atmosphere loop

      do lay = 1, laytrop
         !dir$ SIMD
         do iplon =1,ncol
            if (jp(iplon,lay) .lt. layreffr .and. jp(iplon,lay+1) .ge. layreffr) &
               laysolfr(iplon) = min(lay+1,laytrop)
            speccomb(iplon) = colh2o(iplon,lay) + strrat*colch4(iplon,lay)
            specparm(iplon) = colh2o(iplon,lay)/speccomb(iplon)
            if (specparm(iplon) .ge. oneminus) specparm(iplon) = oneminus
            specmult(iplon) = 8._r8*(specparm(iplon))
         enddo
         !dir$ SIMD
         do iplon =1,ncol
            js(iplon) = 1 + int(specmult(iplon))
            fs(iplon) = mod(specmult(iplon), 1._r8 )
         enddo
         !dir$ SIMD
         do iplon =1,ncol
            fac000(iplon) = (1._r8 - fs(iplon)) * fac00(iplon,lay)
            fac010(iplon) = (1._r8 - fs(iplon)) * fac10(iplon,lay)
            fac100(iplon) = fs(iplon) * fac00(iplon,lay)
            fac110(iplon) = fs(iplon) * fac10(iplon,lay)
            fac001(iplon) = (1._r8 - fs(iplon)) * fac01(iplon,lay)
            fac011(iplon) = (1._r8 - fs(iplon)) * fac11(iplon,lay)
            fac101(iplon) = fs(iplon) * fac01(iplon,lay)
            fac111(iplon) = fs(iplon) * fac11(iplon,lay)
            ind0(iplon) = ((jp(iplon,lay)-1)*5+(jt(iplon,lay)-1))*nspa(18) + js(iplon)
            ind1(iplon) = (jp(iplon,lay)*5+(jt1(iplon,lay)-1))*nspa(18) + js(iplon)
            inds(iplon) = indself(iplon,lay)
            indf(iplon) = indfor(iplon,lay)
            tauray(iplon) = colmol(iplon,lay) * rayl
         enddo
         do ig = 1, ng18
            !dir$ SIMD
            do iplon =1,ncol
               taug(iplon,lay,ngs17+ig) = speccomb(iplon) * &
                   (fac000(iplon) * absa(ind0(iplon),ig) + &
                    fac100(iplon) * absa(ind0(iplon)+1,ig) + &
                    fac010(iplon) * absa(ind0(iplon)+9,ig) + &
                    fac110(iplon) * absa(ind0(iplon)+10,ig) + &
                    fac001(iplon) * absa(ind1(iplon),ig) + &
                    fac101(iplon) * absa(ind1(iplon)+1,ig) + &
                    fac011(iplon) * absa(ind1(iplon)+9,ig) + &
                    fac111(iplon) * absa(ind1(iplon)+10,ig)) + &
                    colh2o(iplon,lay) * &
                    (selffac(iplon,lay) * (selfref(inds(iplon),ig) + &
                    selffrac(iplon,lay) * &
                    (selfref(inds(iplon)+1,ig) - selfref(inds(iplon),ig))) + &
                    forfac(iplon,lay) * (forref(indf(iplon),ig) + &
                    forfrac(iplon,lay) * &
                    (forref(indf(iplon)+1,ig) - forref(indf(iplon),ig)))) 
!            ssa(lay,ngs17+ig) = tauray/taug(lay,ngs17+ig)
               if (lay .eq. laysolfr(iplon)) sfluxzen(iplon,ngs17+ig) = sfluxref(ig,js(iplon)) &
                  + fs(iplon) * (sfluxref(ig,js(iplon)+1) - sfluxref(ig,js(iplon)))
               taur(iplon,lay,ngs17+ig) = tauray(iplon)
            enddo
         enddo
      enddo
! Upper atmosphere loop
      do lay = laytrop+1, nlayers
         !dir$ SIMD
         do iplon =1,ncol
            ind0(iplon) = ((jp(iplon,lay)-13)*5+(jt(iplon,lay)-1))*nspb(18) + 1
            ind1(iplon) = ((jp(iplon,lay)-12)*5+(jt1(iplon,lay)-1))*nspb(18) + 1
            tauray(iplon) = colmol(iplon,lay) * rayl
         enddo
         do ig = 1, ng18
            !dir$ SIMD
            do iplon =1,ncol
               taug(iplon,lay,ngs17+ig) = colch4(iplon,lay) * &
                   (fac00(iplon,lay) * absb(ind0(iplon),ig) + &
                    fac10(iplon,lay) * absb(ind0(iplon)+1,ig) + &
                    fac01(iplon,lay) * absb(ind1(iplon),ig) + &	
                    fac11(iplon,lay) * absb(ind1(iplon)+1,ig)) 
!           ssa(lay,ngs17+ig) = tauray/taug(lay,ngs17+ig)
               taur(iplon,lay,ngs17+ig) = tauray(iplon)
            enddo
          enddo
       enddo
       end subroutine taumol18

!----------------------------------------------------------------------------
!dir$ attributes offload:mic :: taumol19
      subroutine taumol19
!----------------------------------------------------------------------------
!
!     band 19:  4650-5150 cm-1 (low - h2o,co2; high - co2)
!
!----------------------------------------------------------------------------

! ------- Modules -------

      use parrrsw, only : ng19, ngs18
      use rrsw_kg19, only : absa, absb, forref, selfref, &
                            sfluxref, rayl, layreffr, strrat

! ------- Declarations -------

! Local

      integer :: ig, lay , iplon
      integer, dimension(ncol) :: ind0, ind1, inds, indf, js, laysolfr
      real(kind=r8), dimension(ncol) :: fac000, fac001, fac010, fac011, fac100, fac101, &
                         fac110, fac111, fs, speccomb, specmult, specparm, &
                         tauray

! Compute the optical depth by interpolating in ln(pressure), 
! temperature, and appropriate species.  Below LAYTROP, the water
! vapor self-continuum is interpolated (in temperature) separately.  

      !dir$ SIMD
      do iplon =1,ncol
         laysolfr(iplon) = laytrop
      end do
! Lower atmosphere loop      
      do lay = 1, laytrop
         !dir$ SIMD
         do iplon =1,ncol
            if (jp(iplon,lay) .lt. layreffr .and. jp(iplon,lay+1) .ge. layreffr) &
               laysolfr(iplon) = min(lay+1,laytrop)
            speccomb(iplon) = colh2o(iplon,lay) + strrat*colco2(iplon,lay)
            specparm(iplon) = colh2o(iplon,lay)/speccomb(iplon)
            if (specparm(iplon) .ge. oneminus) specparm(iplon) = oneminus
            specmult(iplon) = 8._r8*(specparm(iplon))
         enddo
         !dir$ SIMD
         do iplon =1,ncol
            js(iplon) = 1 + int(specmult(iplon))
            fs(iplon) = mod(specmult(iplon), 1._r8 )
         enddo
         !dir$ SIMD
         do iplon =1,ncol
            fac000(iplon) = (1._r8 - fs(iplon)) * fac00(iplon,lay)
            fac010(iplon) = (1._r8 - fs(iplon)) * fac10(iplon,lay)
            fac100(iplon) = fs(iplon) * fac00(iplon,lay)
            fac110(iplon) = fs(iplon) * fac10(iplon,lay)
            fac001(iplon) = (1._r8 - fs(iplon)) * fac01(iplon,lay)
            fac011(iplon) = (1._r8 - fs(iplon)) * fac11(iplon,lay)
            fac101(iplon) = fs(iplon) * fac01(iplon,lay)
            fac111(iplon) = fs(iplon) * fac11(iplon,lay)
            ind0(iplon) = ((jp(iplon,lay)-1)*5+(jt(iplon,lay)-1))*nspa(19) + js(iplon)
            ind1(iplon) = (jp(iplon,lay)*5+(jt1(iplon,lay)-1))*nspa(19) + js(iplon)
            inds(iplon) = indself(iplon,lay)
            indf(iplon) = indfor(iplon,lay)
            tauray(iplon) = colmol(iplon,lay) * rayl
         enddo
         do ig = 1 , ng19
! Longitude Loop
            !dir$ SIMD
            do iplon =1,ncol
               taug(iplon,lay,ngs18+ig) = speccomb(iplon) * &
                   (fac000(iplon) * absa(ind0(iplon),ig) + &
                    fac100(iplon) * absa(ind0(iplon)+1,ig) + &
                    fac010(iplon) * absa(ind0(iplon)+9,ig) + &
                    fac110(iplon) * absa(ind0(iplon)+10,ig) + &
                    fac001(iplon) * absa(ind1(iplon),ig) + &
                    fac101(iplon) * absa(ind1(iplon)+1,ig) + &
                    fac011(iplon) * absa(ind1(iplon)+9,ig) + &
                    fac111(iplon) * absa(ind1(iplon)+10,ig)) + &
                    colh2o(iplon,lay) * &
                    (selffac(iplon,lay) * (selfref(inds(iplon),ig) + &
                    selffrac(iplon,lay) * &
                    (selfref(inds(iplon)+1,ig) - selfref(inds(iplon),ig))) + &
                    forfac(iplon,lay) * (forref(indf(iplon),ig) + &
                    forfrac(iplon,lay) * &
                    (forref(indf(iplon)+1,ig) - forref(indf(iplon),ig)))) 
!            ssa(lay,ngs18+ig) = tauray/taug(lay,ngs18+ig)
               if (lay .eq. laysolfr(iplon)) sfluxzen(iplon,ngs18+ig) = sfluxref(ig,js(iplon)) &
                  + fs(iplon) * (sfluxref(ig,js(iplon)+1) - sfluxref(ig,js(iplon)))
               taur(iplon,lay,ngs18+ig) = tauray(iplon)   
            enddo
         enddo
      enddo
! Upper atmosphere loop
      do lay = laytrop+1, nlayers
         !dir$ SIMD
         do iplon =1,ncol
            ind0(iplon) = ((jp(iplon,lay)-13)*5+(jt(iplon,lay)-1))*nspb(19) + 1
            ind1(iplon) = ((jp(iplon,lay)-12)*5+(jt1(iplon,lay)-1))*nspb(19) + 1
            tauray(iplon) = colmol(iplon,lay) * rayl
         enddo
         do ig = 1 , ng19
! Longitude Loop
            !dir$ SIMD
            do iplon =1,ncol
               taug(iplon,lay,ngs18+ig) = colco2(iplon,lay) * &
                   (fac00(iplon,lay) * absb(ind0(iplon),ig) + &
                    fac10(iplon,lay) * absb(ind0(iplon)+1,ig) + &
                    fac01(iplon,lay) * absb(ind1(iplon),ig) + &
                    fac11(iplon,lay) * absb(ind1(iplon)+1,ig)) 
!            ssa(lay,ngs18+ig) = tauray/taug(lay,ngs18+ig) 
               taur(iplon,lay,ngs18+ig) = tauray(iplon)   
            enddo
         enddo
      enddo
      end subroutine taumol19

!----------------------------------------------------------------------------
!dir$ attributes offload:mic :: taumol20
      subroutine taumol20
!----------------------------------------------------------------------------
!
!     band 20:  5150-6150 cm-1 (low - h2o; high - h2o)
!
!----------------------------------------------------------------------------

! ------- Modules -------

      use parrrsw, only : ng20, ngs19
      use rrsw_kg20, only : absa, absb, forref, selfref, &
                            sfluxref, absch4, rayl, layreffr

      implicit none

! ------- Declarations -------

! Local
      integer :: ig, lay,  iplon
      integer, dimension(ncol) :: ind0, ind1, inds, indf, js, laysolfr
      real(kind=r8), dimension(ncol) :: fac000, fac001, fac010, fac011, fac100, fac101, &
                         fac110, fac111, fs, speccomb, specmult, specparm, &
                         tauray

! Compute the optical depth by interpolating in ln(pressure), 
! temperature, and appropriate species.  Below LAYTROP, the water
! vapor self-continuum is interpolated (in temperature) separately.  

      !dir$ SIMD
      do iplon =1,ncol
         laysolfr(iplon) = laytrop
      end do
! Lower atmosphere loop
      do lay = 1, laytrop
         !dir$ SIMD
         do iplon =1,ncol
            if (jp(iplon,lay) .lt. layreffr .and. jp(iplon,lay+1) .ge. layreffr) &
               laysolfr(iplon) = min(lay+1,laytrop)
         enddo
         !dir$ SIMD
         do iplon =1,ncol
            ind0(iplon) = ((jp(iplon,lay)-1)*5+(jt(iplon,lay)-1))*nspa(20) + 1
            ind1(iplon) = (jp(iplon,lay)*5+(jt1(iplon,lay)-1))*nspa(20) + 1
            inds(iplon) = indself(iplon,lay)
            indf(iplon) = indfor(iplon,lay)
            tauray(iplon) = colmol(iplon,lay) * rayl
         enddo
         do ig = 1, ng20
            !dir$ SIMD
            do iplon =1,ncol
               taug(iplon,lay,ngs19+ig) = colh2o(iplon,lay) * &
                  ((fac00(iplon,lay) * absa(ind0(iplon),ig) + &
                    fac10(iplon,lay) * absa(ind0(iplon)+1,ig) + &
                    fac01(iplon,lay) * absa(ind1(iplon),ig) + &
                    fac11(iplon,lay) * absa(ind1(iplon)+1,ig)) + &
                    selffac(iplon,lay) * (selfref(inds(iplon),ig) + &
                    selffrac(iplon,lay) * &
                    (selfref(inds(iplon)+1,ig) - selfref(inds(iplon),ig))) + &
                    forfac(iplon,lay) * (forref(indf(iplon),ig) + &
                    forfrac(iplon,lay) * &
                    (forref(indf(iplon)+1,ig) - forref(indf(iplon),ig)))) &
                    + colch4(iplon,lay) * absch4(ig)
!            ssa(lay,ngs19+ig) = tauray/taug(lay,ngs19+ig)
               taur(iplon,lay,ngs19+ig) = tauray(iplon)
               if (lay .eq. laysolfr(iplon)) sfluxzen(iplon,ngs19+ig) = sfluxref(ig) 
            enddo
         enddo
      enddo
! Upper atmosphere loop
      do lay = laytrop+1, nlayers
         !dir$ SIMD
         do iplon =1,ncol
            ind0(iplon) = ((jp(iplon,lay)-13)*5+(jt(iplon,lay)-1))*nspb(20) + 1
            ind1(iplon) = ((jp(iplon,lay)-12)*5+(jt1(iplon,lay)-1))*nspb(20) + 1
            indf(iplon) = indfor(iplon,lay)
            tauray(iplon) = colmol(iplon,lay) * rayl
         enddo
         do ig = 1, ng20
            !dir$ SIMD
            do iplon =1,ncol
               taug(iplon,lay,ngs19+ig) = colh2o(iplon,lay) * &
                   (fac00(iplon,lay) * absb(ind0(iplon),ig) + &
                    fac10(iplon,lay) * absb(ind0(iplon)+1,ig) + &
                    fac01(iplon,lay) * absb(ind1(iplon),ig) + &
                    fac11(iplon,lay) * absb(ind1(iplon)+1,ig) + &
                    forfac(iplon,lay) * (forref(indf(iplon),ig) + &
                    forfrac(iplon,lay) * &
                    (forref(indf(iplon)+1,ig) - forref(indf(iplon),ig)))) + &
                    colch4(iplon,lay) * absch4(ig)
!            ssa(lay,ngs19+ig) = tauray/taug(lay,ngs19+ig)
               taur(iplon,lay,ngs19+ig) = tauray(iplon) 
            enddo
         enddo
      enddo
      end subroutine taumol20

!----------------------------------------------------------------------------
!dir$ attributes offload:mic :: taumol21
      subroutine taumol21
!----------------------------------------------------------------------------
!
!     band 21:  6150-7700 cm-1 (low - h2o,co2; high - h2o,co2)
!
!----------------------------------------------------------------------------

! ------- Modules -------

      use parrrsw, only : ng21, ngs20
      use rrsw_kg21, only : absa, absb, forref, selfref, &
                            sfluxref, rayl, layreffr, strrat

! ------- Declarations -------

! Local

      integer :: ig, lay, iplon
      integer, dimension(ncol) :: ind0, ind1, inds, indf, js, laysolfr
      real(kind=r8), dimension(ncol) :: fac000, fac001, fac010, fac011, fac100, fac101, &
                         fac110, fac111, fs, speccomb, specmult, specparm, &
                         tauray

! Compute the optical depth by interpolating in ln(pressure), 
! temperature, and appropriate species.  Below LAYTROP, the water
! vapor self-continuum is interpolated (in temperature) separately.  

      !dir$ SIMD
      do iplon =1,ncol
         laysolfr(iplon) = laytrop
      end do
      
! Lower atmosphere loop
      do lay = 1, laytrop
         !dir$ SIMD
         do iplon =1,ncol
            if (jp(iplon,lay) .lt. layreffr .and. jp(iplon,lay+1) .ge. layreffr) &
               laysolfr(iplon) = min(lay+1,laytrop)
         enddo
         !dir$ SIMD
         do iplon =1,ncol
            speccomb(iplon) = colh2o(iplon,lay) + strrat*colco2(iplon,lay)
            specparm(iplon) = colh2o(iplon,lay)/speccomb(iplon)
            if (specparm(iplon) .ge. oneminus) specparm(iplon) = oneminus
            specmult(iplon) = 8._r8*(specparm(iplon))
         enddo
         !dir$ SIMD
         do iplon =1,ncol
            js(iplon) = 1 + int(specmult(iplon))
            fs(iplon) = mod(specmult(iplon), 1._r8 )
            fac000(iplon) = (1._r8 - fs(iplon)) * fac00(iplon,lay)
            fac010(iplon) = (1._r8 - fs(iplon)) * fac10(iplon,lay)
            fac100(iplon) = fs(iplon) * fac00(iplon,lay)
            fac110(iplon) = fs(iplon) * fac10(iplon,lay)
            fac001(iplon) = (1._r8 - fs(iplon)) * fac01(iplon,lay)
            fac011(iplon) = (1._r8 - fs(iplon)) * fac11(iplon,lay)
            fac101(iplon) = fs(iplon) * fac01(iplon,lay)
            fac111(iplon) = fs(iplon) * fac11(iplon,lay)
            ind0(iplon) = ((jp(iplon,lay)-1)*5+(jt(iplon,lay)-1))*nspa(21) + js(iplon)
            ind1(iplon) = (jp(iplon,lay)*5+(jt1(iplon,lay)-1))*nspa(21) + js(iplon)
            inds(iplon) = indself(iplon,lay)
            indf(iplon) = indfor(iplon,lay)
            tauray(iplon) = colmol(iplon,lay) * rayl
         enddo
         do ig = 1, ng21
            !dir$ SIMD
            do iplon =1,ncol
               taug(iplon,lay,ngs20+ig) = speccomb(iplon) * &
                   (fac000(iplon) * absa(ind0(iplon),ig) + &
                    fac100(iplon) * absa(ind0(iplon)+1,ig) + &
                    fac010(iplon) * absa(ind0(iplon)+9,ig) + &
                    fac110(iplon) * absa(ind0(iplon)+10,ig) + &
                    fac001(iplon) * absa(ind1(iplon),ig) + &
                    fac101(iplon) * absa(ind1(iplon)+1,ig) + &
                    fac011(iplon) * absa(ind1(iplon)+9,ig) + &
                    fac111(iplon) * absa(ind1(iplon)+10,ig)) + &
                    colh2o(iplon,lay) * &
                    (selffac(iplon,lay) * (selfref(inds(iplon),ig) + &
                    selffrac(iplon,lay) * &
                    (selfref(inds(iplon)+1,ig) - selfref(inds(iplon),ig))) + &
                    forfac(iplon,lay) * (forref(indf(iplon),ig) + &
                    forfrac(iplon,lay) * &
                    (forref(indf(iplon)+1,ig) - forref(indf(iplon),ig))))
!            ssa(lay,ngs20+ig) = tauray/taug(lay,ngs20+ig)
               if (lay .eq. laysolfr(iplon)) sfluxzen(iplon,ngs20+ig) = sfluxref(ig,js(iplon)) &
                  + fs(iplon) * (sfluxref(ig,js(iplon)+1) - sfluxref(ig,js(iplon)))
               taur(iplon,lay,ngs20+ig) = tauray(iplon)
            enddo
         enddo
      enddo
! Upper atmosphere loop
      do lay = laytrop+1, nlayers
         !dir$ SIMD
         do iplon =1,ncol
            speccomb(iplon) = colh2o(iplon,lay) + strrat*colco2(iplon,lay)
            specparm(iplon) = colh2o(iplon,lay)/speccomb(iplon)
            if (specparm(iplon) .ge. oneminus) specparm(iplon) = oneminus
            specmult(iplon) = 4._r8*(specparm(iplon))
         enddo
         !dir$ SIMD
         do iplon =1,ncol
            js(iplon) = 1 + int(specmult(iplon))
            fs(iplon) = mod(specmult(iplon), 1._r8 )
         enddo
         !dir$ SIMD
         do iplon =1,ncol
            fac000(iplon) = (1._r8 - fs(iplon)) * fac00(iplon,lay)
            fac010(iplon) = (1._r8 - fs(iplon)) * fac10(iplon,lay)
            fac100(iplon) = fs(iplon) * fac00(iplon,lay)
            fac110(iplon) = fs(iplon) * fac10(iplon,lay)
            fac001(iplon) = (1._r8 - fs(iplon)) * fac01(iplon,lay)
            fac011(iplon) = (1._r8 - fs(iplon)) * fac11(iplon,lay)
            fac101(iplon) = fs(iplon) * fac01(iplon,lay)
            fac111(iplon) = fs(iplon) * fac11(iplon,lay)
            ind0(iplon) = ((jp(iplon,lay)-13)*5+(jt(iplon,lay)-1))*nspb(21) + js(iplon)
            ind1(iplon) = ((jp(iplon,lay)-12)*5+(jt1(iplon,lay)-1))*nspb(21) + js(iplon)
            indf(iplon) = indfor(iplon,lay)
            tauray(iplon) = colmol(iplon,lay) * rayl
         enddo
         do ig = 1, ng21
            !dir$ SIMD
            do iplon =1,ncol
               taug(iplon,lay,ngs20+ig) = speccomb(iplon) * &
                   (fac000(iplon) * absb(ind0(iplon),ig) + &
                    fac100(iplon) * absb(ind0(iplon)+1,ig) + &
                    fac010(iplon) * absb(ind0(iplon)+5,ig) + &
                    fac110(iplon) * absb(ind0(iplon)+6,ig) + &
                    fac001(iplon) * absb(ind1(iplon),ig) + &
                    fac101(iplon) * absb(ind1(iplon)+1,ig) + &
                    fac011(iplon) * absb(ind1(iplon)+5,ig) + &
                    fac111(iplon) * absb(ind1(iplon)+6,ig)) + &
                    colh2o(iplon,lay) * &
                    forfac(iplon,lay) * (forref(indf(iplon),ig) + &
                    forfrac(iplon,lay) * &
                    (forref(indf(iplon)+1,ig) - forref(indf(iplon),ig)))
!            ssa(lay,ngs20+ig) = tauray/taug(lay,ngs20+ig)
               taur(iplon,lay,ngs20+ig) = tauray(iplon)
            enddo
         enddo
      enddo
      end subroutine taumol21

!----------------------------------------------------------------------------
!dir$ attributes offload:mic :: taumol22
      subroutine taumol22
!----------------------------------------------------------------------------
!
!     band 22:  7700-8050 cm-1 (low - h2o,o2; high - o2)
!
!----------------------------------------------------------------------------

! ------- Modules -------

      use parrrsw, only : ng22, ngs21
      use rrsw_kg22, only : absa, absb, forref, selfref, &
                            sfluxref, rayl, layreffr, strrat

! ------- Declarations -------

! Local

      integer :: ig, lay, icol
      integer :: ind0(ncol), ind1(ncol), inds(ncol), indf(ncol), js(ncol), laysolfr(ncol)
      real(kind=r8), dimension(ncol) :: fac000, fac001, fac010, fac011, fac100, fac101, &
                         fac110, fac111, fs, speccomb, specmult, specparm, &
                         tauray, o2cont
      real(kind=r8) :: o2adj
! The following factor is the ratio of total O2 band intensity (lines 
! and Mate continuum) to O2 band intensity (line only).  It is needed
! to adjust the optical depths since the k's include only lines.
      o2adj = 1.6_r8
      
! Compute the optical depth by interpolating in ln(pressure), 
! temperature, and appropriate species.  Below LAYTROP, the water
! vapor self-continuum is interpolated (in temperature) separately.  

      !dir$ SIMD
      do icol=1,ncol
         laysolfr(icol) = laytrop
      end do

! Lower atmosphere loop
      do lay = 1, laytrop
         !dir$ SIMD
         do icol=1,ncol
            if (jp(icol,lay) .lt. layreffr .and. jp(icol,lay+1) .ge. layreffr) &
               laysolfr(icol) = min(lay+1,laytrop)
            o2cont(icol) = 4.35e-4_r8*colo2(icol,lay)/(350.0_r8*2.0_r8)
         end do
         !dir$ SIMD
         do icol=1,ncol
            speccomb(icol) = colh2o(icol,lay) + o2adj*strrat*colo2(icol,lay)
            specparm(icol) = colh2o(icol,lay)/speccomb(icol)
            if (specparm(icol) .ge. oneminus) specparm(icol) = oneminus
            specmult(icol) = 8._r8*(specparm(icol))
!         odadj = specparm + o2adj * (1._r8 - specparm)
         end do
         !dir$ SIMD
         do icol=1,ncol
            js(icol) = 1 + int(specmult(icol))
            fs(icol) = mod(specmult(icol), 1._r8 )
            fac000(icol) = (1._r8 - fs(icol)) * fac00(icol,lay)
            fac010(icol) = (1._r8 - fs(icol)) * fac10(icol,lay)
            fac100(icol) = fs(icol) * fac00(icol,lay)
            fac110(icol) = fs(icol) * fac10(icol,lay)
            fac001(icol) = (1._r8 - fs(icol)) * fac01(icol,lay)
            fac011(icol) = (1._r8 - fs(icol)) * fac11(icol,lay)
            fac101(icol) = fs(icol) * fac01(icol,lay)
            fac111(icol) = fs(icol) * fac11(icol,lay)
            ind0(icol) = ((jp(icol,lay)-1)*5+(jt(icol,lay)-1))*nspa(22) + js(icol)
            ind1(icol) = (jp(icol,lay)*5+(jt1(icol,lay)-1))*nspa(22) + js(icol)
            inds(icol) = indself(icol,lay)
            indf(icol) = indfor(icol,lay)
            tauray(icol) = colmol(icol,lay) * rayl
         end do
         do ig = 1, ng22
            !dir$ SIMD
            do icol=1,ncol
               taug(icol,lay,ngs21+ig) = speccomb(icol) * &
                   (fac000(icol) * absa(ind0(icol),ig) + &
                    fac100(icol) * absa(ind0(icol)+1,ig) + &
                    fac010(icol) * absa(ind0(icol)+9,ig) + &
                    fac110(icol) * absa(ind0(icol)+10,ig) + &
                    fac001(icol) * absa(ind1(icol),ig) + &
                    fac101(icol) * absa(ind1(icol)+1,ig) + &
                    fac011(icol) * absa(ind1(icol)+9,ig) + &
                    fac111(icol) * absa(ind1(icol)+10,ig)) + &
                    colh2o(icol,lay) * &
                    (selffac(icol,lay) * (selfref(inds(icol),ig) + &
                    selffrac(icol,lay) * &
                     (selfref(inds(icol)+1,ig) - selfref(inds(icol),ig))) + &
                    forfac(icol,lay) * (forref(indf(icol),ig) + &
                    forfrac(icol,lay) * &
                    (forref(indf(icol)+1,ig) - forref(indf(icol),ig)))) &
                    + o2cont(icol)
!            ssa(lay,ngs21+ig) = tauray/taug(lay,ngs21+ig)
               if (lay .eq. laysolfr(icol)) sfluxzen(icol,ngs21+ig) = sfluxref(ig,js(icol)) &
                   + fs(icol) * (sfluxref(ig,js(icol)+1) - sfluxref(ig,js(icol)))
               taur(icol,lay,ngs21+ig) = tauray(icol)
            enddo
         enddo
      end do
! Upper atmosphere loop
      do lay = laytrop+1, nlayers
         !dir$ SIMD
         do icol=1,ncol
            o2cont(icol) = 4.35e-4_r8*colo2(icol,lay)/(350.0_r8*2.0_r8)
            ind0(icol) = ((jp(icol,lay)-13)*5+(jt(icol,lay)-1))*nspb(22) + 1
            ind1(icol) = ((jp(icol,lay)-12)*5+(jt1(icol,lay)-1))*nspb(22) + 1
            tauray(icol) = colmol(icol,lay) * rayl
         end do
         do ig = 1, ng22
            !dir$ SIMD
            do icol=1,ncol
               taug(icol,lay,ngs21+ig) = colo2(icol,lay) * o2adj * &
                   (fac00(icol,lay) * absb(ind0(icol),ig) + &
                    fac10(icol,lay) * absb(ind0(icol)+1,ig) + &
                    fac01(icol,lay) * absb(ind1(icol),ig) + &
                    fac11(icol,lay) * absb(ind1(icol)+1,ig)) + &
                    o2cont(icol)
!            ssa(lay,ngs21+ig) = tauray/taug(lay,ngs21+ig)
               taur(icol,lay,ngs21+ig) = tauray(icol)
            enddo
         enddo
      enddo
      end subroutine taumol22

!----------------------------------------------------------------------------
!dir$ attributes offload:mic :: taumol23
      subroutine taumol23
!----------------------------------------------------------------------------
!
!     band 23:  8050-12850 cm-1 (low - h2o; high - nothing)
!
!----------------------------------------------------------------------------

! ------- Modules -------

      use parrrsw, only : ng23, ngs22
      use rrsw_kg23, only : absa, forref, selfref, &
                            sfluxref, rayl, layreffr, givfac

! ------- Declarations -------

! Local

      integer :: ig, lay, iplon
      integer, dimension(ncol) :: ind0, ind1, inds, indf, js, laysolfr
      real(kind=r8), dimension(ncol) :: fac000, fac001, fac010, fac011, fac100, fac101, &
                         fac110, fac111, fs, speccomb, specmult, specparm, &
                         tauray

! Compute the optical depth by interpolating in ln(pressure), 
! temperature, and appropriate species.  Below LAYTROP, the water
! vapor self-continuum is interpolated (in temperature) separately.  

      !dir$ SIMD
      do iplon =1,ncol
         laysolfr(iplon) = laytrop

      end do
! Lower atmosphere loop
      do lay = 1, laytrop
         !dir$ SIMD
         do iplon =1,ncol
            if (jp(iplon,lay) .lt. layreffr .and. jp(iplon,lay+1) .ge. layreffr) &
               laysolfr(iplon) = min(lay+1,laytrop)
         enddo
         !dir$ SIMD
         do iplon =1,ncol
            ind0(iplon) = ((jp(iplon,lay)-1)*5+(jt(iplon,lay)-1))*nspa(23) + 1
            ind1(iplon) = (jp(iplon,lay)*5+(jt1(iplon,lay)-1))*nspa(23) + 1
            inds(iplon) = indself(iplon,lay)
            indf(iplon) = indfor(iplon,lay)

         enddo
         do ig = 1, ng23
            !dir$ SIMD
            do iplon =1,ncol
               tauray(iplon) = colmol(iplon,lay) * rayl(ig)
               taug(iplon,lay,ngs22+ig) = colh2o(iplon,lay) * &
                   (givfac * (fac00(iplon,lay) * absa(ind0(iplon),ig) + &
                    fac10(iplon,lay) * absa(ind0(iplon)+1,ig) + &
                    fac01(iplon,lay) * absa(ind1(iplon),ig) + &
                    fac11(iplon,lay) * absa(ind1(iplon)+1,ig)) + &
                    selffac(iplon,lay) * (selfref(inds(iplon),ig) + &
                    selffrac(iplon,lay) * &
                    (selfref(inds(iplon)+1,ig) - selfref(inds(iplon),ig))) + &
                    forfac(iplon,lay) * (forref(indf(iplon),ig) + &
                    forfrac(iplon,lay) * &
                    (forref(indf(iplon)+1,ig) - forref(indf(iplon),ig)))) 
!            ssa(lay,ngs22+ig) = tauray/taug(lay,ngs22+ig)
               if (lay .eq. laysolfr(iplon)) sfluxzen(iplon,ngs22+ig) = sfluxref(ig)
               taur(iplon,lay,ngs22+ig) = tauray(iplon)
            enddo
         enddo
      enddo
! Upper atmosphere loop
      do lay = laytrop+1, nlayers
         do ig = 1, ng23
!            taug(lay,ngs22+ig) = colmol(lay) * rayl(ig)
!            ssa(lay,ngs22+ig) = 1.0_r8
! Longitude Loop
            !dir$ SIMD
            do iplon =1,ncol
               taug(iplon,lay,ngs22+ig) = 0._r8
               taur(iplon,lay,ngs22+ig) = colmol(iplon,lay) * rayl(ig)
            enddo
         enddo
      enddo

      end subroutine taumol23

!----------------------------------------------------------------------------
!dir$ attributes offload:mic :: taumol24
      subroutine taumol24
!----------------------------------------------------------------------------
!
!     band 24:  12850-16000 cm-1 (low - h2o,o2; high - o2)
!
!----------------------------------------------------------------------------

! ------- Modules -------

      use parrrsw, only : ng24, ngs23
      use rrsw_kg24, only : absa, absb, forref, selfref, &
                            sfluxref, abso3a, abso3b, rayla, raylb, &
                            layreffr, strrat

! ------- Declarations -------

! Local

      integer :: ig, lay, iplon
      integer, dimension(ncol) :: ind0, ind1, inds, indf, js, laysolfr
      real(kind=r8), dimension(ncol) :: fac000, fac001, fac010, fac011, fac100, fac101, &
                         fac110, fac111, fs, speccomb, specmult, specparm, &
                         tauray

! Compute the optical depth by interpolating in ln(pressure), 
! temperature, and appropriate species.  Below LAYTROP, the water
! vapor self-continuum is interpolated (in temperature) separately.  

      !dir$ SIMD
      do iplon =1,ncol
         laysolfr(iplon) = laytrop

      end do
! Lower atmosphere loop
      do lay = 1, laytrop
         !dir$ SIMD
         do iplon =1,ncol
            if (jp(iplon,lay) .lt. layreffr .and. jp(iplon,lay+1) .ge. layreffr) &
               laysolfr(iplon) = min(lay+1,laytrop)
         enddo
         !dir$ SIMD
         do iplon =1,ncol
            speccomb(iplon) = colh2o(iplon,lay) + strrat*colo2(iplon,lay)
            specparm(iplon) = colh2o(iplon,lay)/speccomb(iplon)
            if (specparm(iplon) .ge. oneminus) specparm(iplon) = oneminus
            specmult(iplon) = 8._r8*(specparm(iplon))
         enddo
         !dir$ SIMD
         do iplon =1,ncol
            js(iplon) = 1 + int(specmult(iplon))
            fs(iplon) = mod(specmult(iplon), 1._r8 )
         enddo
         !dir$ SIMD
         do iplon =1,ncol
            fac000(iplon) = (1._r8 - fs(iplon)) * fac00(iplon,lay)
            fac010(iplon) = (1._r8 - fs(iplon)) * fac10(iplon,lay)
            fac100(iplon) = fs(iplon) * fac00(iplon,lay)
            fac110(iplon) = fs(iplon) * fac10(iplon,lay)
            fac001(iplon) = (1._r8 - fs(iplon)) * fac01(iplon,lay)
            fac011(iplon) = (1._r8 - fs(iplon)) * fac11(iplon,lay)
            fac101(iplon) = fs(iplon) * fac01(iplon,lay)
            fac111(iplon) = fs(iplon) * fac11(iplon,lay)
            ind0(iplon) = ((jp(iplon,lay)-1)*5+(jt(iplon,lay)-1))*nspa(24) + js(iplon)
            ind1(iplon) = (jp(iplon,lay)*5+(jt1(iplon,lay)-1))*nspa(24) + js(iplon)
            inds(iplon) = indself(iplon,lay)
            indf(iplon) = indfor(iplon,lay)
         enddo
         do ig = 1, ng24
            !dir$ SIMD
            do iplon =1,ncol
               tauray(iplon) = colmol(iplon,lay) * (rayla(ig,js(iplon)) + &
                  fs(iplon) * (rayla(ig,js(iplon)+1) - rayla(ig,js(iplon))))
               taug(iplon,lay,ngs23+ig) = speccomb(iplon) * &
                   (fac000(iplon) * absa(ind0(iplon),ig) + &
                    fac100(iplon) * absa(ind0(iplon)+1,ig) + &
                    fac010(iplon) * absa(ind0(iplon)+9,ig) + &
                    fac110(iplon) * absa(ind0(iplon)+10,ig) + &
                    fac001(iplon) * absa(ind1(iplon),ig) + &
                    fac101(iplon) * absa(ind1(iplon)+1,ig) + &
                    fac011(iplon) * absa(ind1(iplon)+9,ig) + &
                    fac111(iplon) * absa(ind1(iplon)+10,ig)) + &
                    colo3(iplon,lay) * abso3a(ig) + &
                    colh2o(iplon,lay) * &
                    (selffac(iplon,lay) * (selfref(inds(iplon),ig) + &
                    selffrac(iplon,lay) * &
                    (selfref(inds(iplon)+1,ig) - selfref(inds(iplon),ig))) + &
                    forfac(iplon,lay) * (forref(indf(iplon),ig) + &
                    forfrac(iplon,lay) * &
                    (forref(indf(iplon)+1,ig) - forref(indf(iplon),ig))))
!            ssa(lay,ngs23+ig) = tauray/taug(lay,ngs23+ig)
               if (lay .eq. laysolfr(iplon)) sfluxzen(iplon,ngs23+ig) = sfluxref(ig,js(iplon)) &
                  + fs(iplon) * (sfluxref(ig,js(iplon)+1) - sfluxref(ig,js(iplon)))
               taur(iplon,lay,ngs23+ig) = tauray(iplon)
            enddo
         enddo
      enddo
! Upper atmosphere loop
      do lay = laytrop+1, nlayers
         !dir$ SIMD
         do iplon =1,ncol
            ind0(iplon) = ((jp(iplon,lay)-13)*5+(jt(iplon,lay)-1))*nspb(24) + 1
            ind1(iplon) = ((jp(iplon,lay)-12)*5+(jt1(iplon,lay)-1))*nspb(24) + 1
         enddo
         do ig = 1, ng24
            !dir$ SIMD
            do iplon =1,ncol
               tauray(iplon) = colmol(iplon,lay) * raylb(ig)
               taug(iplon,lay,ngs23+ig) = colo2(iplon,lay) * &
                   (fac00(iplon,lay) * absb(ind0(iplon),ig) + &
                    fac10(iplon,lay) * absb(ind0(iplon)+1,ig) + &
                    fac01(iplon,lay) * absb(ind1(iplon),ig) + &
                    fac11(iplon,lay) * absb(ind1(iplon)+1,ig)) + &
                    colo3(iplon,lay) * abso3b(ig)
!            ssa(lay,ngs23+ig) = tauray/taug(lay,ngs23+ig)
               taur(iplon,lay,ngs23+ig) = tauray(iplon)
            enddo
         enddo
      enddo
      end subroutine taumol24

!----------------------------------------------------------------------------
!dir$ attributes offload:mic :: taumol25
      subroutine taumol25
!----------------------------------------------------------------------------
!
!     band 25:  16000-22650 cm-1 (low - h2o; high - nothing)
!
!----------------------------------------------------------------------------

! ------- Modules -------

      use parrrsw, only : ng25, ngs24
      use rrsw_kg25, only : absa, &
                            sfluxref, abso3a, abso3b, rayl, layreffr

! ------- Declarations -------

! Local

      integer :: ig, lay, iplon
      integer, dimension(ncol) :: ind0, ind1, inds, indf, js, laysolfr
      real(kind=r8), dimension(ncol) :: fac000, fac001, fac010, fac011, fac100, fac101, &
                         fac110, fac111, fs, speccomb, specmult, specparm, &
                         tauray

! Compute the optical depth by interpolating in ln(pressure), 
! temperature, and appropriate species.  Below LAYTROP, the water
! vapor self-continuum is interpolated (in temperature) separately.  

      !dir$ SIMD
      do iplon =1,ncol
         laysolfr(iplon) = laytrop

      end do
! Lower atmosphere loop
      do lay = 1, laytrop
         !dir$ SIMD
         do iplon =1,ncol
            if (jp(iplon,lay) .lt. layreffr .and. jp(iplon,lay+1) .ge. layreffr) &
               laysolfr(iplon) = min(lay+1,laytrop)
         enddo
         !dir$ SIMD
         do iplon =1,ncol
            ind0(iplon) = ((jp(iplon,lay)-1)*5+(jt(iplon,lay)-1))*nspa(25) + 1
            ind1(iplon) = (jp(iplon,lay)*5+(jt1(iplon,lay)-1))*nspa(25) + 1

         enddo
         do ig = 1, ng25
            !dir$ SIMD
            do iplon =1,ncol
               tauray(iplon) = colmol(iplon,lay) * rayl(ig)
               taug(iplon,lay,ngs24+ig) = colh2o(iplon,lay) * &
                   (fac00(iplon,lay) * absa(ind0(iplon),ig) + &
                    fac10(iplon,lay) * absa(ind0(iplon)+1,ig) + &
                    fac01(iplon,lay) * absa(ind1(iplon),ig) + &
                    fac11(iplon,lay) * absa(ind1(iplon)+1,ig)) + &
                    colo3(iplon,lay) * abso3a(ig) 
!            ssa(lay,ngs24+ig) = tauray/taug(lay,ngs24+ig)
               if (lay .eq. laysolfr(iplon)) sfluxzen(iplon,ngs24+ig) = sfluxref(ig)
               taur(iplon,lay,ngs24+ig) = tauray(iplon)
            enddo
         enddo
      enddo
! Upper atmosphere loop
      do lay = laytrop+1, nlayers
         do ig = 1, ng25
            !dir$ SIMD
            do iplon =1,ncol
               tauray(iplon) = colmol(iplon,lay) * rayl(ig)
               taug(iplon,lay,ngs24+ig) = colo3(iplon,lay) * abso3b(ig) 
!            ssa(lay,ngs24+ig) = tauray/taug(lay,ngs24+ig)
               taur(iplon,lay,ngs24+ig) = tauray(iplon)
            enddo
         enddo
      enddo
      end subroutine taumol25

!----------------------------------------------------------------------------
!dir$ attributes offload:mic :: taumol26
      subroutine taumol26
!----------------------------------------------------------------------------
!
!     band 26:  22650-29000 cm-1 (low - nothing; high - nothing)
!
!----------------------------------------------------------------------------

! ------- Modules -------

      use parrrsw, only : ng26, ngs25
      use rrsw_kg26, only : sfluxref, rayl

! ------- Declarations -------

! Local

      integer :: ig, lay, iplon
      integer, dimension(ncol) :: ind0, ind1, inds, indf, js, laysolfr
      real(kind=r8), dimension(ncol) :: fac000, fac001, fac010, fac011, fac100, fac101, &
                         fac110, fac111, fs, speccomb, specmult, specparm, &
                         tauray

! Compute the optical depth by interpolating in ln(pressure), 
! temperature, and appropriate species.  Below LAYTROP, the water
! vapor self-continuum is interpolated (in temperature) separately.  
      !dir$ SIMD
      do iplon =1,ncol

         laysolfr(iplon) = laytrop

      end do
! Lower atmosphere loop
      do lay = 1, laytrop
         do ig = 1, ng26 
!            taug(lay,ngs25+ig) = colmol(lay) * rayl(ig)
!            ssa(lay,ngs25+ig) = 1.0_r8
            !dir$ SIMD
            do iplon =1,ncol
               if (lay .eq. laysolfr(iplon)) sfluxzen(iplon,ngs25+ig) = sfluxref(ig)
               taug(iplon,lay,ngs25+ig) = 0._r8
               taur(iplon,lay,ngs25+ig) = colmol(iplon,lay) * rayl(ig) 
            enddo
         enddo
      enddo
! Upper atmosphere loop
      do lay = laytrop+1, nlayers
         do ig = 1, ng26
!            taug(lay,ngs25+ig) = colmol(lay) * rayl(ig)
!            ssa(lay,ngs25+ig) = 1.0_r8
            !dir$ SIMD
            do iplon =1,ncol
               taug(iplon,lay,ngs25+ig) = 0._r8
               taur(iplon,lay,ngs25+ig) = colmol(iplon,lay) * rayl(ig) 
            enddo
         enddo
      enddo
      end subroutine taumol26

!----------------------------------------------------------------------------
!dir$ attributes offload:mic :: taumol27
      subroutine taumol27
!----------------------------------------------------------------------------
!
!     band 27:  29000-38000 cm-1 (low - o3; high - o3)
!
!----------------------------------------------------------------------------

! ------- Modules -------

      use parrrsw, only : ng27, ngs26
      use rrsw_kg27, only : absa, absb, &
                            sfluxref, rayl, layreffr, scalekur

! ------- Declarations -------

! Local

      integer :: ig, lay, iplon
      integer, dimension(ncol) :: ind0, ind1, inds, indf, js, laysolfr
      real(kind=r8), dimension(ncol) :: fac000, fac001, fac010, fac011, fac100, fac101, &
                         fac110, fac111, fs, speccomb, specmult, specparm, &
                         tauray

! Compute the optical depth by interpolating in ln(pressure), 
! temperature, and appropriate species.  Below LAYTROP, the water
! vapor self-continuum is interpolated (in temperature) separately.  

! Lower atmosphere loop
      do lay = 1, laytrop
         !dir$ SIMD
         do iplon =1,ncol
            ind0(iplon) = ((jp(iplon,lay)-1)*5+(jt(iplon,lay)-1))*nspa(27) + 1
            ind1(iplon) = (jp(iplon,lay)*5+(jt1(iplon,lay)-1))*nspa(27) + 1

         enddo
         do ig = 1, ng27
            !dir$ SIMD
            do iplon =1,ncol
               tauray(iplon) = colmol(iplon,lay) * rayl(ig)
               taug(iplon,lay,ngs26+ig) = colo3(iplon,lay) * &
                   (fac00(iplon,lay) * absa(ind0(iplon),ig) + &
                    fac10(iplon,lay) * absa(ind0(iplon)+1,ig) + &
                    fac01(iplon,lay) * absa(ind1(iplon),ig) + &
                    fac11(iplon,lay) * absa(ind1(iplon)+1,ig))
!            ssa(lay,ngs26+ig) = tauray/taug(lay,ngs26+ig)
               taur(iplon,lay,ngs26+ig) = tauray(iplon)
            enddo
         enddo
      enddo
      !dir$ SIMD
      do iplon =1,ncol

         laysolfr(iplon) = nlayers

      end do
! Upper atmosphere loop
      do lay = laytrop+1, nlayers
         !dir$ SIMD
         do iplon =1,ncol
            if (jp(iplon,lay-1) .lt. layreffr .and. jp(iplon,lay) .ge. layreffr) &
               laysolfr(iplon) = lay
            ind0(iplon) = ((jp(iplon,lay)-13)*5+(jt(iplon,lay)-1))*nspb(27) + 1
            ind1(iplon) = ((jp(iplon,lay)-12)*5+(jt1(iplon,lay)-1))*nspb(27) + 1
         enddo
         do ig = 1, ng27
            !dir$ SIMD
            do iplon =1,ncol
               tauray(iplon) = colmol(iplon,lay) * rayl(ig)
               taug(iplon,lay,ngs26+ig) = colo3(iplon,lay) * &
                   (fac00(iplon,lay) * absb(ind0(iplon),ig) + &
                    fac10(iplon,lay) * absb(ind0(iplon)+1,ig) + &
                    fac01(iplon,lay) * absb(ind1(iplon),ig) + &
                    fac11(iplon,lay) * absb(ind1(iplon)+1,ig))
!            ssa(lay,ngs26+ig) = tauray/taug(lay,ngs26+ig)
               if (lay.eq.laysolfr(iplon)) sfluxzen(iplon,ngs26+ig) = scalekur * sfluxref(ig)
               taur(iplon,lay,ngs26+ig) = tauray(iplon)
            enddo
         enddo
      enddo
      end subroutine taumol27

!----------------------------------------------------------------------------
!dir$ attributes offload:mic :: taumol28
      subroutine taumol28
!----------------------------------------------------------------------------
!
!     band 28:  38000-50000 cm-1 (low - o3,o2; high - o3,o2)
!
!----------------------------------------------------------------------------

! ------- Modules -------

      use parrrsw, only : ng28, ngs27
      use rrsw_kg28, only : absa, absb, &
                            sfluxref, rayl, layreffr, strrat

! ------- Declarations -------

! Local

      integer :: ig, lay, iplon
      integer, dimension(ncol) :: ind0, ind1, inds, indf, js, laysolfr
      real(kind=r8), dimension(ncol) :: fac000, fac001, fac010, fac011, fac100, fac101, &
                         fac110, fac111, fs, speccomb, specmult, specparm, &
                         tauray

! Compute the optical depth by interpolating in ln(pressure), 
! temperature, and appropriate species.  Below LAYTROP, the water
! vapor self-continuum is interpolated (in temperature) separately.  

! Lower atmosphere loop
      do lay = 1, laytrop
         !dir$ SIMD
         do iplon =1,ncol
            speccomb(iplon) = colo3(iplon,lay) + strrat*colo2(iplon,lay)
            specparm(iplon) = colo3(iplon,lay)/speccomb(iplon)
            if (specparm(iplon) .ge. oneminus) specparm(iplon) = oneminus
            specmult(iplon) = 8._r8*(specparm(iplon))
         enddo
         !dir$ SIMD
         do iplon =1,ncol
            js(iplon) = 1 + int(specmult(iplon))
            fs(iplon) = mod(specmult(iplon), 1._r8 )
         enddo
         !dir$ SIMD
         do iplon =1,ncol
            fac000(iplon) = (1._r8 - fs(iplon)) * fac00(iplon,lay)
            fac010(iplon) = (1._r8 - fs(iplon)) * fac10(iplon,lay)
            fac100(iplon) = fs(iplon) * fac00(iplon,lay)
            fac110(iplon) = fs(iplon) * fac10(iplon,lay)
            fac001(iplon) = (1._r8 - fs(iplon)) * fac01(iplon,lay)
            fac011(iplon) = (1._r8 - fs(iplon)) * fac11(iplon,lay)
            fac101(iplon) = fs(iplon) * fac01(iplon,lay)
            fac111(iplon) = fs(iplon) * fac11(iplon,lay)
            ind0(iplon) = ((jp(iplon,lay)-1)*5+(jt(iplon,lay)-1))*nspa(28) + js(iplon)
            ind1(iplon) = (jp(iplon,lay)*5+(jt1(iplon,lay)-1))*nspa(28) + js(iplon)
            tauray(iplon) = colmol(iplon,lay) * rayl
         enddo
         do ig = 1, ng28
            !dir$ SIMD
            do iplon =1,ncol
               taug(iplon,lay,ngs27+ig) = speccomb(iplon) * &
                   (fac000(iplon) * absa(ind0(iplon),ig) + &
                    fac100(iplon) * absa(ind0(iplon)+1,ig) + &
                    fac010(iplon) * absa(ind0(iplon)+9,ig) + &
                    fac110(iplon) * absa(ind0(iplon)+10,ig) + &
                    fac001(iplon) * absa(ind1(iplon),ig) + &
                    fac101(iplon) * absa(ind1(iplon)+1,ig) + &
                    fac011(iplon) * absa(ind1(iplon)+9,ig) + &
                    fac111(iplon) * absa(ind1(iplon)+10,ig)) 
!            ssa(lay,ngs27+ig) = tauray/taug(lay,ngs27+ig)
               taur(iplon,lay,ngs27+ig) = tauray(iplon)
            enddo
         enddo
      enddo
      !dir$ SIMD
      do iplon =1,ncol
         laysolfr(iplon) = nlayers
      end do
! Upper atmosphere loop
      do lay = laytrop+1, nlayers
         !dir$ SIMD
         do iplon =1,ncol
            if (jp(iplon,lay-1) .lt. layreffr .and. jp(iplon,lay) .ge. layreffr) &
               laysolfr(iplon) = lay
            speccomb(iplon) = colo3(iplon,lay) + strrat*colo2(iplon,lay)
            specparm(iplon) = colo3(iplon,lay)/speccomb(iplon)
            if (specparm(iplon) .ge. oneminus) specparm(iplon) = oneminus
            specmult(iplon) = 4._r8*(specparm(iplon))
         enddo
         !dir$ SIMD
         do iplon =1,ncol
            js(iplon) = 1 + int(specmult(iplon))
            fs(iplon) = mod(specmult(iplon), 1._r8 )
         enddo
         !dir$ SIMD
         do iplon =1,ncol
            fac000(iplon) = (1._r8 - fs(iplon)) * fac00(iplon,lay)
            fac010(iplon) = (1._r8 - fs(iplon)) * fac10(iplon,lay)
            fac100(iplon) = fs(iplon) * fac00(iplon,lay)
            fac110(iplon) = fs(iplon) * fac10(iplon,lay)
            fac001(iplon) = (1._r8 - fs(iplon)) * fac01(iplon,lay)
            fac011(iplon) = (1._r8 - fs(iplon)) * fac11(iplon,lay)
            fac101(iplon) = fs(iplon) * fac01(iplon,lay)
            fac111(iplon) = fs(iplon) * fac11(iplon,lay)
            ind0(iplon) = ((jp(iplon,lay)-13)*5+(jt(iplon,lay)-1))*nspb(28) + js(iplon)
            ind1(iplon) = ((jp(iplon,lay)-12)*5+(jt1(iplon,lay)-1))*nspb(28) + js(iplon)
            tauray(iplon) = colmol(iplon,lay) * rayl
         enddo
         do ig = 1, ng28
            !dir$ SIMD
            do iplon =1,ncol
               taug(iplon,lay,ngs27+ig) = speccomb(iplon) * &
                   (fac000(iplon) * absb(ind0(iplon),ig) + &
                    fac100(iplon) * absb(ind0(iplon)+1,ig) + &
                    fac010(iplon) * absb(ind0(iplon)+5,ig) + &
                    fac110(iplon) * absb(ind0(iplon)+6,ig) + &
                    fac001(iplon) * absb(ind1(iplon),ig) + &
                    fac101(iplon) * absb(ind1(iplon)+1,ig) + &
                    fac011(iplon) * absb(ind1(iplon)+5,ig) + &
                    fac111(iplon) * absb(ind1(iplon)+6,ig)) 
!            ssa(lay,ngs27+ig) = tauray/taug(lay,ngs27+ig)
               if (lay .eq. laysolfr(iplon)) sfluxzen(iplon,ngs27+ig) = sfluxref(ig,js(iplon)) &
                  + fs(iplon) * (sfluxref(ig,js(iplon)+1) - sfluxref(ig,js(iplon)))
               taur(iplon,lay,ngs27+ig) = tauray(iplon)
            enddo
         enddo
      enddo
      end subroutine taumol28

!----------------------------------------------------------------------------
!dir$ attributes offload:mic :: taumol29
      subroutine taumol29
!----------------------------------------------------------------------------
!
!     band 29:  820-2600 cm-1 (low - h2o; high - co2)
!
!----------------------------------------------------------------------------

! ------- Modules -------

      use parrrsw, only : ng29, ngs28
      use rrsw_kg29, only : absa, absb, forref, selfref, &
                            sfluxref, absh2o, absco2, rayl, layreffr

! ------- Declarations -------

! Local

      integer :: ig, lay, iplon
      integer, dimension(ncol) :: ind0, ind1, inds, indf, js, laysolfr
      real(kind=r8), dimension(ncol) :: fac000, fac001, fac010, fac011, fac100, fac101, &
                         fac110, fac111, fs, speccomb, specmult, specparm, &
                         tauray

! Compute the optical depth by interpolating in ln(pressure), 
! temperature, and appropriate species.  Below LAYTROP, the water
! vapor self-continuum is interpolated (in temperature) separately.  

! Lower atmosphere loop
      do lay = 1, laytrop
         !dir$ SIMD
         do iplon =1,ncol
            ind0(iplon) = ((jp(iplon,lay)-1)*5+(jt(iplon,lay)-1))*nspa(29) + 1
            ind1(iplon) = (jp(iplon,lay)*5+(jt1(iplon,lay)-1))*nspa(29) + 1
         enddo
         !dir$ SIMD
         do iplon =1,ncol
            inds(iplon) = indself(iplon,lay)
            indf(iplon) = indfor(iplon,lay)
            tauray(iplon) = colmol(iplon,lay) * rayl
         enddo
         do ig = 1, ng29
            !dir$ SIMD
            do iplon =1,ncol
               taug(iplon,lay,ngs28+ig) = colh2o(iplon,lay) * &
                  ((fac00(iplon,lay) * absa(ind0(iplon),ig) + &
                    fac10(iplon,lay) * absa(ind0(iplon)+1,ig) + &
                    fac01(iplon,lay) * absa(ind1(iplon),ig) + &
                    fac11(iplon,lay) * absa(ind1(iplon)+1,ig)) + &
                    selffac(iplon,lay) * (selfref(inds(iplon),ig) + &
                    selffrac(iplon,lay) * &
                    (selfref(inds(iplon)+1,ig) - selfref(inds(iplon),ig))) + &
                    forfac(iplon,lay) * (forref(indf(iplon),ig) + &
                    forfrac(iplon,lay) * &
                    (forref(indf(iplon)+1,ig) - forref(indf(iplon),ig)))) &
                    + colco2(iplon,lay) * absco2(ig) 
!            ssa(lay,ngs28+ig) = tauray/taug(lay,ngs28+ig)
               taur(iplon,lay,ngs28+ig) = tauray(iplon)
            enddo
         enddo
      enddo
      !dir$ SIMD
      do iplon =1,ncol

         laysolfr(iplon) = nlayers

      end do
! Upper atmosphere loop
      do lay = laytrop+1, nlayers
         !dir$ SIMD
         do iplon =1,ncol
            if (jp(iplon,lay-1) .lt. layreffr .and. jp(iplon,lay) .ge. layreffr) &
                        laysolfr(iplon) = lay
            ind0(iplon) = ((jp(iplon,lay)-13)*5+(jt(iplon,lay)-1))*nspb(29) + 1
            ind1(iplon) = ((jp(iplon,lay)-12)*5+(jt1(iplon,lay)-1))*nspb(29) + 1
            tauray(iplon) = colmol(iplon,lay) * rayl
         enddo
         do ig = 1, ng29
            !dir$ SIMD
            do iplon =1,ncol
               taug(iplon,lay,ngs28+ig) = colco2(iplon,lay) * &
                   (fac00(iplon,lay) * absb(ind0(iplon),ig) + &
                    fac10(iplon,lay) * absb(ind0(iplon)+1,ig) + &
                    fac01(iplon,lay) * absb(ind1(iplon),ig) + &
                    fac11(iplon,lay) * absb(ind1(iplon)+1,ig)) &
                    + colh2o(iplon,lay) * absh2o(ig) 
!            ssa(lay,ngs28+ig) = tauray/taug(lay,ngs28+ig)
               if (lay .eq. laysolfr(iplon)) sfluxzen(iplon,ngs28+ig) = sfluxref(ig)
               taur(iplon,lay,ngs28+ig) = tauray(iplon)
            enddo
         enddo
      enddo
      end subroutine taumol29

      end subroutine taumol_sw

      end module rrtmg_sw_taumol

