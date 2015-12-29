
! KGEN-generated Fortran source file
!
! Filename    : rrtmg_sw_cldprmc.f90
! Generated at: 2015-07-07 00:48:24
! KGEN version: 0.4.13



    MODULE rrtmg_sw_cldprmc
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
        USE parrrsw, ONLY: ngptsw
        USE rrsw_cld, ONLY: abari
        USE rrsw_cld, ONLY: bbari
        USE rrsw_cld, ONLY: dbari
        USE rrsw_cld, ONLY: cbari
        USE rrsw_cld, ONLY: ebari
        USE rrsw_cld, ONLY: fbari
        USE rrsw_cld, ONLY: extice2
        USE rrsw_cld, ONLY: ssaice2
        USE rrsw_cld, ONLY: asyice2
        USE rrsw_cld, ONLY: extice3
        USE rrsw_cld, ONLY: ssaice3
        USE rrsw_cld, ONLY: asyice3
        USE rrsw_cld, ONLY: fdlice3
        USE rrsw_cld, ONLY: extliq1
        USE rrsw_cld, ONLY: ssaliq1
        USE rrsw_cld, ONLY: asyliq1
        USE rrsw_wvn, ONLY: ngb
        USE rrsw_wvn, ONLY: wavenum2
        USE rrsw_vsn, ONLY: hvrclc
        IMPLICIT NONE
        CONTAINS

        ! write subroutines
        ! No subroutines
        ! No module extern variables
        ! ----------------------------------------------------------------------------
! ----------------------------------------------------------------------------
!dir$ attributes offload:mic :: cldprmc_sw
      subroutine cldprmc_sw(ncol,nlayers, inflag, iceflag, liqflag, cldfmc, &
                            ciwpmc, clwpmc, reicmc, dgesmc, relqmc, &
                            taormc, taucmc, ssacmc, asmcmc, fsfcmc)
! ----------------------------------------------------------------------------

! Purpose: Compute the cloud optical properties for each cloudy layer
! and g-point interval for use by the McICA method.  
! Note: Only inflag = 0 and inflag=2/liqflag=1/iceflag=2,3 are available;
! (Hu & Stamnes, Key, and Fu) are implemented. 

! ------- Input -------

      integer, intent(in) :: nlayers         ! total number of layers
      integer, intent(in) :: ncol            ! total number of layers
      integer, intent(in) :: inflag     ! see definitions
      integer, intent(in) :: iceflag         ! see definitions
      integer, intent(in) :: liqflag         ! see definitions

      real(kind=r8), intent(in) :: cldfmc(:,:,:)        ! cloud fraction [mcica]
                                                        !    Dimensions: (ngptsw,nlayers)
      real(kind=r8), intent(in) :: ciwpmc(:,:,:)        ! cloud ice water path [mcica]
                                                        !    Dimensions: (ngptsw,nlayers)
      real(kind=r8), intent(in) :: clwpmc(:,:,:)        ! cloud liquid water path [mcica]
                                                        !    Dimensions: (ngptsw,nlayers)
      real(kind=r8), intent(in) :: relqmc(:,:)          ! cloud liquid particle effective radius (microns)
                                                        !    Dimensions: (nlayers)
      real(kind=r8), intent(in) :: reicmc(:,:)          ! cloud ice particle effective radius (microns)
                                                        !    Dimensions: (nlayers)
      real(kind=r8), intent(in) :: dgesmc(:,:)          ! cloud ice particle generalized effective size (microns)
                                                        !    Dimensions: (nlayers)
      real(kind=r8), intent(in) :: fsfcmc(:,:,:)        ! cloud forward scattering fraction 
                                                        !    Dimensions: (ngptsw,nlayers)

! ------- Output -------

      real(kind=r8), intent(inout) :: taucmc(:,:,:)     ! cloud optical depth (delta scaled)
                                                        !    Dimensions: (ngptsw,nlayers)
      real(kind=r8), intent(inout) :: ssacmc(:,:,:)     ! single scattering albedo (delta scaled)
                                                        !    Dimensions: (ngptsw,nlayers)
      real(kind=r8), intent(inout) :: asmcmc(:,:,:)     ! asymmetry parameter (delta scaled)
                                                        !    Dimensions: (ngptsw,nlayers)
      real(kind=r8), intent(out) :: taormc(:,:)       ! cloud optical depth (non-delta scaled)
                                                        !    Dimensions: (ngptsw,nlayers)

! ------- Local -------

!      integer :: ncbands
      integer :: lay, ig, iplon
      integer, dimension(ncol) :: ib(ncol), istr(ncol),index(ncol), icx(ncol)
      real(kind=r8), parameter :: eps = 1.e-06_r8     ! epsilon
      real(kind=r8), parameter :: cldmin = 1.e-80_r8  ! minimum value for cloud quantities
      real(kind=r8) :: cwp(ncol)                            ! total cloud water path
      real(kind=r8) :: radliq(ncol)                         ! cloud liquid droplet radius (microns)
      real(kind=r8) :: radice(ncol)                         ! cloud ice effective radius (microns)
      real(kind=r8) :: dgeice(ncol)                         ! cloud ice generalized effective size (microns)
      real(kind=r8) :: factor(ncol)
      real(kind=r8) :: fint(ncol)

      real(kind=r8), dimension(ncol) :: taucldorig_a, taucloud_a, ssacloud_a, ffp, ffp1, ffpssa
      real(kind=r8), dimension(ncol) :: tauiceorig, scatice, ssaice, tauice, tauliqorig, scatliq, ssaliq, tauliq

      real(kind=r8) :: fdelta(ncol,ngptsw)
      real(kind=r8) :: extcoice(ncol,ngptsw), gice(ncol,ngptsw)
      real(kind=r8) :: ssacoice(ncol,ngptsw), forwice(ncol,ngptsw)
      real(kind=r8) :: extcoliq(ncol,ngptsw), gliq(ncol,ngptsw)
      real(kind=r8) :: ssacoliq(ncol,ngptsw), forwliq(ncol,ngptsw)

! Initialize

      hvrclc = '$Revision: 1.4 $'

! Initialize

! Some of these initializations are done in rrtmg_sw.f90.

! Main layer loop
      do lay = 1, nlayers

! Main g-point interval loop
         do ig = 1, ngptsw 
            do iplon=1, ncol
               taormc(ig,lay) = taucmc(iplon,ig,lay)
               cwp(iplon) = ciwpmc(iplon,ig,lay) + clwpmc(iplon,ig,lay)
               if (cldfmc(iplon,ig,lay) .ge. cldmin .and. &
               (cwp(iplon) .ge. cldmin .or. taucmc(iplon,ig,lay) .ge. cldmin)) then

! (inflag=0): Cloud optical properties input directly
                  if (inflag .eq. 0) then
! Cloud optical properties already defined in taucmc, ssacmc, asmcmc are unscaled;
! Apply delta-M scaling here (using Henyey-Greenstein approximation)
                     taucldorig_a(iplon) = taucmc(iplon,ig,lay)
                     ffp(iplon) = fsfcmc(iplon,ig,lay)
                     ffp1(iplon) = 1.0_r8 - ffp(iplon)
                     ffpssa(iplon) = 1.0_r8 - ffp(iplon) * ssacmc(iplon,ig,lay)
                     ssacloud_a(iplon) = ffp1(iplon) * ssacmc(iplon,ig,lay) / ffpssa(iplon)
                     taucloud_a(iplon) = ffpssa(iplon) * taucldorig_a(iplon)

                     taormc(ig,lay) = taucldorig_a(iplon)
                     ssacmc(iplon,ig,lay) = ssacloud_a(iplon)
                     taucmc(iplon,ig,lay) = taucloud_a(iplon)
                     asmcmc(iplon,ig,lay) = (asmcmc(iplon,ig,lay) - ffp(iplon)) / (ffp1(iplon))

                  elseif (inflag.eq. 1) then 
                     stop 'INFLAG = 1 OPTION NOT AVAILABLE WITH MCICA'

! (inflag=2): Separate treatement of ice clouds and water clouds.
                  elseif (inflag .eq. 2) then
                         radice(iplon) = reicmc(iplon,lay)

! Calculation of absorption coefficients due to ice clouds.
                     if (ciwpmc(iplon,ig,lay) .eq. 0.0) then
                        extcoice(iplon,ig) = 0.0_r8
                        ssacoice(iplon,ig) = 0.0_r8
                        gice(iplon,ig)     = 0.0_r8
                        forwice(iplon,ig)  = 0.0_r8

! (iceflag = 1): 
! Note: This option uses Ebert and Curry approach for all particle sizes similar to
! CAM3 implementation, though this is somewhat unjustified for large ice particles
                     elseif (iceflag .eq. 1) then
                        ib(iplon) = ngb(ig)
                        if (wavenum2(ib(iplon)) .gt. 1.43e04_r8) then
                           icx(iplon) = 1
                        elseif (wavenum2(ib(iplon)) .gt. 7.7e03_r8) then
                           icx(iplon) = 2
                        elseif (wavenum2(ib(iplon)) .gt. 5.3e03_r8) then
                           icx(iplon) = 3
                        elseif (wavenum2(ib(iplon)) .gt. 4.0e03_r8) then
                           icx(iplon) = 4
                        elseif (wavenum2(ib(iplon)) .ge. 2.5e03_r8) then
                           icx(iplon) = 5
                        endif
                        extcoice(iplon,ig) = (abari(icx(iplon)) + bbari(icx(iplon))/radice(iplon))
                        ssacoice(iplon,ig) = 1._r8 - cbari(icx(iplon)) - dbari(icx(iplon)) * radice(iplon)
                        gice(iplon,ig) = ebari(icx(iplon)) + fbari(icx(iplon)) * radice(iplon)
! Check to ensure upper limit of gice is within physical limits for large particles
                        if (gice(iplon,ig).ge.1._r8) gice(iplon,ig) = 1._r8 - eps
                        forwice(iplon,ig) = gice(iplon,ig)*gice(iplon,ig)
! Check to ensure all calculated quantities are within physical limits.
                        if (extcoice(iplon,ig) .lt. 0.0_r8) stop 'ICE EXTINCTION LESS THAN 0.0'
                        if (ssacoice(iplon,ig) .gt. 1.0_r8) stop 'ICE SSA GRTR THAN 1.0'
                        if (ssacoice(iplon,ig) .lt. 0.0_r8) stop 'ICE SSA LESS THAN 0.0'
                        if (gice(iplon,ig) .gt. 1.0_r8) stop 'ICE ASYM GRTR THAN 1.0'
                        if (gice(iplon,ig) .lt. 0.0_r8) stop 'ICE ASYM LESS THAN 0.0'

! For iceflag=2 option, combine with iceflag=0 option to handle large particle sizes.
! Use iceflag=2 option for ice particle effective radii from 5.0 to 131.0 microns
! and use iceflag=0 option for ice particles greater than 131.0 microns.
! *** NOTE: Transition between two methods has not been smoothed.

                     elseif (iceflag .eq. 2) then
                        if (radice(iplon) .lt. 5.0_r8) stop 'ICE RADIUS OUT OF BOUNDS'
                        if (radice(iplon) .ge. 5.0_r8 .and. radice(iplon) .le. 131._r8) then
                           factor(iplon) = (radice(iplon) - 2._r8)/3._r8
                           index(iplon) = int(factor(iplon))
                           if (index(iplon) .eq. 43) index(iplon) = 42
                           fint(iplon) = factor(iplon) - float(index(iplon))
                           ib(iplon) = ngb(ig)
                           extcoice(iplon,ig) = extice2(index(iplon),ib(iplon)) + fint(iplon) * &
                                         (extice2(index(iplon)+1,ib(iplon)) -  extice2(index(iplon),ib(iplon)))
                           ssacoice(iplon,ig) = ssaice2(index(iplon),ib(iplon)) + fint(iplon) * &
                                         (ssaice2(index(iplon)+1,ib(iplon)) -  ssaice2(index(iplon),ib(iplon)))
                           gice(iplon,ig) = asyice2(index(iplon),ib(iplon)) + fint(iplon) * &
                                         (asyice2(index(iplon)+1,ib(iplon)) -  asyice2(index(iplon),ib(iplon)))
                           forwice(iplon,ig) = gice(iplon,ig)*gice(iplon,ig)
! Check to ensure all calculated quantities are within physical limits.
                           if (extcoice(iplon,ig) .lt. 0.0_r8) stop 'ICE EXTINCTION LESS THAN 0.0'
                           if (ssacoice(iplon,ig) .gt. 1.0_r8) stop 'ICE SSA GRTR THAN 1.0'
                           if (ssacoice(iplon,ig) .lt. 0.0_r8) stop 'ICE SSA LESS THAN 0.0'
                           if (gice(iplon,ig) .gt. 1.0_r8) stop 'ICE ASYM GRTR THAN 1.0'
                           if (gice(iplon,ig) .lt. 0.0_r8) stop 'ICE ASYM LESS THAN 0.0'
                        elseif (radice(iplon) .gt. 131._r8) then
                           ib(iplon) = ngb(ig)
                           if (wavenum2(ib(iplon)) .gt. 1.43e04_r8) then
                              icx(iplon) = 1
                           elseif (wavenum2(ib(iplon)) .gt. 7.7e03_r8) then
                              icx(iplon) = 2
                           elseif (wavenum2(ib(iplon)) .gt. 5.3e03_r8) then
                              icx(iplon) = 3
                           elseif (wavenum2(ib(iplon)) .gt. 4.0e03_r8) then
                              icx(iplon) = 4
                           elseif (wavenum2(ib(iplon)) .ge. 2.5e03_r8) then
                              icx(iplon) = 5
                           endif
                           extcoice(iplon,ig) = (abari(icx(iplon)) + bbari(icx(iplon))/radice(iplon))
                           ssacoice(iplon,ig) = 1._r8 - cbari(icx(iplon)) - dbari(icx(iplon)) * radice(iplon)
                           gice(iplon,ig) = ebari(icx(iplon)) + fbari(icx(iplon)) * radice(iplon)
! Check to ensure upper limit of gice is within physical limits for large particles
                           if (gice(iplon,ig).ge.1.0_r8) gice(iplon,ig) = 1.0_r8-eps
                           forwice(iplon,ig) = gice(iplon,ig)*gice(iplon,ig)
! Check to ensure all calculated quantities are within physical limits.
                           if (extcoice(iplon,ig) .lt. 0.0_r8) stop 'ICE EXTINCTION LESS THAN 0.0'
                           if (ssacoice(iplon,ig) .gt. 1.0_r8) stop 'ICE SSA GRTR THAN 1.0'
                           if (ssacoice(iplon,ig) .lt. 0.0_r8) stop 'ICE SSA LESS THAN 0.0'
                           if (gice(iplon,ig) .gt. 1.0_r8) stop 'ICE ASYM GRTR THAN 1.0'
                           if (gice(iplon,ig) .lt. 0.0_r8) stop 'ICE ASYM LESS THAN 0.0'
                        endif

! For iceflag=3 option, combine with iceflag=0 option to handle large particle sizes
! Use iceflag=3 option for ice particle effective radii from 3.2 to 91.0 microns
! (generalized effective size, dge, from 5 to 140 microns), and use iceflag=0 option
! for ice particle effective radii greater than 91.0 microns (dge = 140 microns).
! *** NOTE: Fu parameterization requires particle size in generalized effective size.
! *** NOTE: Transition between two methods has not been smoothed. 

                     elseif (iceflag .eq. 3) then
                        dgeice(iplon) = dgesmc(iplon,lay)
                        if (dgeice(iplon) .lt. 5.0_r8) stop 'ICE GENERALIZED EFFECTIVE SIZE OUT OF BOUNDS'
                        if (dgeice(iplon) .ge. 5.0_r8 .and. dgeice(iplon) .le. 140._r8) then
                           factor(iplon) = (dgeice(iplon) - 2._r8)/3._r8
                           index(iplon) = int(factor(iplon))
                           if (index(iplon) .eq. 46) index(iplon) = 45
                           fint(iplon) = factor(iplon) - float(index(iplon))
                           ib(iplon) = ngb(ig)
                           extcoice(iplon,ig) = extice3(index(iplon),ib(iplon)) + fint(iplon) * &
                                         (extice3(index(iplon)+1,ib(iplon)) - extice3(index(iplon),ib(iplon)))
                           ssacoice(iplon,ig) = ssaice3(index(iplon),ib(iplon)) + fint(iplon) * &
                                      (ssaice3(index(iplon)+1,ib(iplon)) - ssaice3(index(iplon),ib(iplon)))
                           gice(iplon,ig) = asyice3(index(iplon),ib(iplon)) + fint(iplon) * &
                                     (asyice3(index(iplon)+1,ib(iplon)) - asyice3(index(iplon),ib(iplon)))
                           fdelta(iplon,ig) = fdlice3(index(iplon),ib(iplon)) + fint(iplon) * &
                                       (fdlice3(index(iplon)+1,ib(iplon)) - fdlice3(index(iplon),ib(iplon)))
                           if (fdelta(iplon,ig) .lt. 0.0_r8) stop 'FDELTA LESS THAN 0.0'
                           if (fdelta(iplon,ig) .gt. 1.0_r8) stop 'FDELTA GT THAN 1.0'
                           forwice(iplon,ig) = fdelta(iplon,ig) + 0.5_r8 / ssacoice(iplon,ig)
! See Fu 1996 p. 2067 
                           if (forwice(iplon,ig) .gt. gice(iplon,ig)) forwice(iplon,ig) = gice(iplon,ig)
! Check to ensure all calculated quantities are within physical limits.  
                           if (extcoice(iplon,ig) .lt. 0.0_r8) stop 'ICE EXTINCTION LESS THAN 0.0'
                           if (ssacoice(iplon,ig) .gt. 1.0_r8) stop 'ICE SSA GRTR THAN 1.0'
                           if (ssacoice(iplon,ig) .lt. 0.0_r8) stop 'ICE SSA LESS THAN 0.0'
                           if (gice(iplon,ig) .gt. 1.0_r8) stop 'ICE ASYM GRTR THAN 1.0'
                           if (gice(iplon,ig) .lt. 0.0_r8) stop 'ICE ASYM LESS THAN 0.0'
                        elseif (dgeice(iplon) .gt. 140._r8) then
                           ib(iplon) = ngb(ig)
                           if (wavenum2(ib(iplon)) .gt. 1.43e04_r8) then
                              icx(iplon) = 1
                           elseif (wavenum2(ib(iplon)) .gt. 7.7e03_r8) then
                              icx(iplon) = 2
                           elseif (wavenum2(ib(iplon)) .gt. 5.3e03_r8) then
                              icx(iplon) = 3
                           elseif (wavenum2(ib(iplon)) .gt. 4.0e03_r8) then
                              icx(iplon) = 4
                           elseif (wavenum2(ib(iplon)) .ge. 2.5e03_r8) then
                              icx(iplon) = 5
                           endif
                           extcoice(iplon,ig) = (abari(icx(iplon)) + bbari(icx(iplon))/radice(iplon))
                           ssacoice(iplon,ig) = 1._r8 - cbari(icx(iplon)) - dbari(icx(iplon)) * radice(iplon)
                           gice(iplon,ig) = ebari(icx(iplon)) + fbari(icx(iplon)) * radice(iplon)
! Check to ensure upper limit of gice is within physical limits for large particles
                           if (gice(iplon,ig).ge.1._r8) gice(iplon,ig) = 1._r8 - eps
                           forwice(iplon,ig) = gice(iplon,ig)*gice(iplon,ig)
! Check to ensure all calculated quantities are within physical limits.
                           if (extcoice(iplon,ig) .lt. 0.0_r8) stop 'ICE EXTINCTION LESS THAN 0.0'
                           if (ssacoice(iplon,ig) .gt. 1.0_r8) stop 'ICE SSA GRTR THAN 1.0'
                           if (ssacoice(iplon,ig) .lt. 0.0_r8) stop 'ICE SSA LESS THAN 0.0'
                           if (gice(iplon,ig) .gt. 1.0_r8) stop 'ICE ASYM GRTR THAN 1.0'
                           if (gice(iplon,ig) .lt. 0.0_r8) stop 'ICE ASYM LESS THAN 0.0'
                        endif
                     endif

! Calculation of absorption coefficients due to water clouds.
                     if (clwpmc(iplon,ig,lay) .eq. 0.0_r8) then
                        extcoliq(iplon,ig) = 0.0_r8
                        ssacoliq(iplon,ig) = 0.0_r8
                        gliq(iplon,ig) = 0.0_r8
                        forwliq(iplon,ig) = 0.0_r8

                     elseif (liqflag .eq. 1) then
                        radliq(iplon) = relqmc(iplon,lay)
                        if (radliq(iplon) .lt. 1.5_r8 .or. radliq(iplon) .gt. 60._r8) stop &
                           'liquid effective radius out of bounds'
                        index(iplon) = int(radliq(iplon) - 1.5_r8)
                        if (index(iplon) .eq. 0) index(iplon) = 1
                        if (index(iplon) .eq. 58) index(iplon) = 57
                        fint(iplon) = radliq(iplon) - 1.5_r8 - float(index(iplon))
                        ib(iplon) = ngb(ig)
                        extcoliq(iplon,ig) = extliq1(index(iplon),ib(iplon)) + fint(iplon) * &
                                      (extliq1(index(iplon)+1,ib(iplon)) - extliq1(index(iplon),ib(iplon)))
                        ssacoliq(iplon,ig) = ssaliq1(index(iplon),ib(iplon)) + fint(iplon) * &
                                      (ssaliq1(index(iplon)+1,ib(iplon)) - ssaliq1(index(iplon),ib(iplon)))
                        if (fint(iplon) .lt. 0._r8 .and. ssacoliq(iplon,ig) .gt. 1._r8) &
                                       ssacoliq(iplon,ig) = ssaliq1(index(iplon),ib(iplon))
                        gliq(iplon,ig) = asyliq1(index(iplon),ib(iplon)) + fint(iplon) * &
                                  (asyliq1(index(iplon)+1,ib(iplon)) - asyliq1(index(iplon),ib(iplon)))
                        forwliq(iplon,ig) = gliq(iplon,ig)*gliq(iplon,ig)
! Check to ensure all calculated quantities are within physical limits.
                        if (extcoliq(iplon,ig) .lt. 0.0_r8) stop 'LIQUID EXTINCTION LESS THAN 0.0'
                        if (ssacoliq(iplon,ig) .gt. 1.0_r8) stop 'LIQUID SSA GRTR THAN 1.0'
                        if (ssacoliq(iplon,ig) .lt. 0.0_r8) stop 'LIQUID SSA LESS THAN 0.0'
                        if (gliq(iplon,ig) .gt. 1.0_r8) stop 'LIQUID ASYM GRTR THAN 1.0'
                        if (gliq(iplon,ig) .lt. 0.0_r8) stop 'LIQUID ASYM LESS THAN 0.0'
                     endif
   
                     tauliqorig(iplon) = clwpmc(iplon,ig,lay) * extcoliq(iplon,ig)
                     tauiceorig(iplon) = ciwpmc(iplon,ig,lay) * extcoice(iplon,ig)
                     taormc(ig,lay) = tauliqorig(iplon) + tauiceorig(iplon)

                     ssaliq(iplon) = ssacoliq(iplon,ig) * (1._r8 - forwliq(iplon,ig)) / &
                             (1._r8 - forwliq(iplon,ig) * ssacoliq(iplon,ig))
                     tauliq(iplon) = (1._r8 - forwliq(iplon,ig) * ssacoliq(iplon,ig)) * tauliqorig(iplon)
                     ssaice(iplon) = ssacoice(iplon,ig) * (1._r8 - forwice(iplon,ig)) / &
                             (1._r8 - forwice(iplon,ig) * ssacoice(iplon,ig))
                     tauice(iplon) = (1._r8 - forwice(iplon,ig) * ssacoice(iplon,ig)) * tauiceorig(iplon)

                     scatliq(iplon) = ssaliq(iplon) * tauliq(iplon)
                     scatice(iplon) = ssaice(iplon) * tauice(iplon)
                     taucmc(iplon,ig,lay) = tauliq(iplon) + tauice(iplon)

! Ensure non-zero taucmc and scatice
                     if(taucmc(iplon,ig,lay).eq.0.) taucmc(iplon,ig,lay) = cldmin
                     if(scatice(iplon).eq.0.) scatice(iplon) = cldmin

                     ssacmc(iplon,ig,lay) = (scatliq(iplon) + scatice(iplon)) / taucmc(iplon,ig,lay)

                     if (iceflag .eq. 3) then
! In accordance with the 1996 Fu paper, equation A.3, 
! the moments for ice were calculated depending on whether using spheres
! or hexagonal ice crystals.
! Set asymetry parameter to first moment (istr(iplon)=1)
                        istr(iplon) = 1
                        asmcmc(iplon,ig,lay) = (1.0_r8/(scatliq(iplon)+scatice(iplon)))* &
                           (scatliq(iplon)*(gliq(iplon,ig)**istr(iplon) - forwliq(iplon,ig)) / &
                           (1.0_r8 - forwliq(iplon,ig)) + scatice(iplon) * ((gice(iplon,ig)-forwice(iplon,ig))/ &
                           (1.0_r8 - forwice(iplon,ig)))**istr(iplon))

                     else 
! This code is the standard method for delta-m scaling. 
! Set asymetry parameter to first moment (istr=1)
                        istr(iplon) = 1
                        asmcmc(iplon,ig,lay) = (scatliq(iplon) *  &
                           (gliq(iplon,ig)**istr(iplon) - forwliq(iplon,ig)) / &
                           (1.0_r8 - forwliq(iplon,ig)) + scatice(iplon) * (gice(iplon,ig)**istr(iplon) - forwice(iplon,ig)) / &
                           (1.0_r8 - forwice(iplon,ig)))/(scatliq(iplon) + scatice(iplon))
                     endif 

                  endif

               endif

! End g-point interval loop
            enddo

! End layer loop
         enddo
      end do
      end subroutine cldprmc_sw

      end module rrtmg_sw_cldprmc

