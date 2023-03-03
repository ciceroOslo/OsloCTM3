!//=========================================================================
!// Oslo CTM3
!//=========================================================================
!// Based on UCI CTM core p-7.1 (1/2013).
!//
!// Ole Amund Sovde, November 2015
!//=========================================================================
!// Routine for reading meteorological data from norESM, from nedCDF files.
!//=========================================================================
module metdata_noresm
  !//-----------------------------------------------------------------------
  !// MODULE: metdata_noresm
  !// DESCRIPTION: Routine for reading meteorological data from norESM,
  !//              stored on nedCDF format.
  !//
  !// Contains
  !//   subroutine update_metdata
  !//   subroutine fluxfilter2
  !//   subroutine data2mpblocks
  !//   subroutine gotData
  !//   subroutine skipData
  !//-----------------------------------------------------------------------
  use cmn_size, only: LPAR
  !//-----------------------------------------------------------------------
  implicit none
  !//-----------------------------------------------------------------------
  integer :: LMAP(LPAR+1)
  !// ----------------------------------------------------------------------
  character(len=*), parameter, private :: f90file = 'metdata_noresm.f90'
  !//-----------------------------------------------------------------------
  save
  private
  public update_metdata
  !//-----------------------------------------------------------------------

contains

  !//-----------------------------------------------------------------------
  subroutine update_metdata(ILOOP, DTMET, NMET)
    !//---------------------------------------------------------------------
    !// Read input data. This version reads netCDF4 files, where data for
    !// each day is divided into three files: one for spectral 
    !// data, one for 3-d gridpoint data and one for 2-d gridpoint data.
    !//
    !// Spectral transformation is done in this routine, and summation of
    !// physical fields from native to model resolution.
    !//
    !// This routine assumes meteorology is to be updated each NMET. E.g.
    !// ERA-40 had 6-hour fields, and was tweaked to be read every other
    !// NMET, but with the double time step. This has been removed for now.
    !//
    !// Ole Amund Sovde, April 2015
    !//---------------------------------------------------------------------
    use cmn_precision, only: r8, r4
    use cmn_size, only: IPARW, JPARW, LPARW, LWEPARW, &
         IPAR, JPAR, IDGRD, JDGRD, &
         LPAR, LWEPAR, LWDPAR, &
         FFTPTS, NRFFT, NRNM, NMMAX, NTRUNW, LOSLOCHEM, NRMETD, LDUST
    use cmn_ctm, only: JYEAR, JMON, JDAY, JDATE, GMTAU, &
         LMMAP, XLMMAP, TMON, TMET, &
         ALP, ALPV, TRIG, IFAX, LDEG, ZDEGI, ZDEGJ, IMAP, JMAP, &
         XYZA, XYZB, IMEPZ, DD, SS, ETAAW, ETABW, ETAA, ETAB, &
         WGLYE, WGLYG, YDGRD, AREAXYW, AREAXY, PLAND
    use cmn_met, only: P, U, V, T, Q, CWETN, CWETE, CWETD, CENTU, CENTD, &
         PRECCNV, PRECLS, ZOFLE, &
          CLDFR, CLDLWC, CLDIWC, &
         SLH, SHF, SMF, SFT, SFQ, SFU, SFV, BLH, SA, &
         PVU, UMS, VMS, CI, SD, SMLT, ES, SNFL, PhotActRad, MSLP, USTR, &
         PMEAN, PMEANW, MYEAR, &
         metTYPE, MPATH1, MPATH2, MFILE3
    use cmn_parameters, only: M_AIR, R_UNIV, R_AIR, R_H2O, A0, CPI
    use cloudjx, only: SWSTORE, CLDSTORE, TYPSTORE, RAN4, IRAN0, &
                       LCLDAVG, LCLDQMD, LCLDQMN, LCLDRANA, LCLDRANQ
    use regridding, only: TRUNG4, TRUNG8
    use utilities, only: ctmExitC, CFRMIN,CIWMIN
    use ncutils, only: get_netcdf_var_2d, get_netcdf_var_3d, &
         readnc_3d_from4d, readnc_2d_from3d
    use dust_oslo, only: dust_set_ssrd, dust_set_strd, dust_set_SWVL1
    use physics_oslo, only: get_pvu, ijlw2lij
    !//---------------------------------------------------------------------
    implicit none
    !//---------------------------------------------------------------------
    !// Input
    integer, intent(in) :: ILOOP, & ! <=0, init, > 0 ordinary run
                           NMET     ! the meteorological timestep
    real(r8), intent(in) :: DTMET     ! meteorological time step [s]
    !//---------------------------------------------------------------------
    logical, parameter :: VERBOSE = .false.
    real(r8), parameter :: EPS = 0.01_r8 ! minimum cloud fraction
    !// Limits for convective fluxes
    real(r8), parameter :: mincwetelim =  3.8e-4_r8
    real(r8), parameter :: maxcwetdlim = -2.0e-5_r8
    real(r8), parameter :: mincdetulim =  1.e-4_r8
    real(r8), parameter :: mincdetdlim =  1.e-6_r8

    !// Local parameters
    real(r8) :: &
         VEDGE, BAND, DETA, DETB, DELP, &
         ZCOS, ZDT, SFTD, LV, ESAT, SDEN, &
         PT, PB, &
         DELZ, LWC, IWC, TFACT, QMIN, &
         DMASS, ZAREA, PSRF, &
         POFLE(LPAR+1)

    !// Indices
    integer :: I,J,II,JJ,L,LL
    integer :: NF, NrOf3Dfields, NrOf2Dfields, ioerr, silent

    !// To be read from file
    integer :: FLD_ID
    logical :: fex

    logical :: LPVU !// Need to calculate PVU?
    real(r8) :: fU, fV

    !// Filename for metdata
    character(len=160) :: FNAME1, FNAME2, FNAME3, FNAME4
    character(len=2) :: CMON, CDATE
    character(len=4) :: CYEAR

    integer, parameter :: JPARWNE1 = JPARW+2
    integer, parameter :: JPARNE1 = JPAR+2

    !//---------------------------------------------------------------------
    !// Allocatable arrays - double precision
    real(r8), dimension(:), allocatable :: &
          UTMP(:)
    real(r8), dimension(:,:), allocatable :: &
         VTMP, inXY, inXY_b, &
         PW, EWSS, NSSS, LSPREC, CNVPREC, W2D, R8XY
    real(r8), dimension(:,:,:), allocatable :: &
         TW, QW, CLDFRW, CLDIWCW, CLDLWCW, &
         ZOFLEW, W3Da, W3Db, R8XYZ, &
         inXYZ, inXYm1Z, inXYZp1, W3Das
    !// --------------------------------------------------------------------
    character(len=*), parameter :: subr = 'update_metdata'
    !//---------------------------------------------------------------------
         
    !// Allocate 3D arrays - native resolution
    allocate( VTMP(IPARW,JPARW), UTMP(IPARW), &
              W3Da(IPARW,JPARW,LPARW), W3Db(IPARW,JPARW,LPARW), &
              W3Das(IPARW,JPARW-1,LPARW), &
              CLDFRW(IPARW,JPARW,LWEPAR), &
              CLDIWCW(IPARW,JPARW,LWEPAR), CLDLWCW(IPARW,JPARW,LWEPAR) )

    !// Allocate 3D arrays - native horizontal resolution
    allocate( ZOFLEW(LPAR+1,IPARW,JPARW), QW(IPARW,JPARW,LPAR), &
              TW(IPARW,JPARW,LPAR) )

    allocate ( inXYZ(IPARW,JPARWNE1,LPARW), inXYm1Z(IPARW,JPARWNE1-1,LPARW) )
    allocate ( inXYZp1(IPARW,JPARWNE1,LPARW+1) )
    allocate ( inXY(IPARW,JPARWNE1), inXY_b(IPARW,JPARWNE1) )


    !// Allocate 3D arrays - window resolution (IPAR/JPAR)
    allocate( R8XYZ(IPAR,JPAR,LPARW) )

    !// Allocate 2D arrays - native resolution
    allocate( PW(IPARW,JPARW), W2D(IPARW,JPARW) )

    !// Allocate 2D arrays - window resolution (IPAR/JPAR)
    allocate( LSPREC(IPAR,JPAR),CNVPREC(IPAR,JPAR), &
              EWSS(IPAR,JPAR), NSSS(IPAR,JPAR), R8XY(IPAR,JPAR) )
    !//---------------------------------------------------------------------

    !// Initialize
    !// Time step for meteorological data is DTMET
    ZDT    = 1._r8 / DTMET

    !// Define minimum humidity QMIN in kg/kg
    QMIN = 3.e-6_r8 * 18._r8 / M_AIR

!//UPDATE
    !// locate the position of random number sequence based on year/day/hour
    IRAN0 = 1 + 3*(JYEAR - 2000) + 7*JDAY + 11*nint(GMTAU)

    if (VERBOSE) then
       silent=1
    else
       silent = 0
    end if
    !// File name for this DAY/NMET
    write(CDATE(1:2),'(i2.2)') JDATE                  ! Date
    write(CMON(1:2),'(i2.2)') JMON                    ! Month
    write(MPATH2(1:4),'(i4.4)') MYEAR                 ! Year
    if (trim(MFILE3) .eq. 'EVA_OCN_REF') then
       write(CYEAR(1:4),'(i4.4)') MYEAR - 1999 + 70      ! Year 71-80
    else
       write(CYEAR(1:4),'(i4.4)') MYEAR - 1999 + 90      ! Year 91-100
    end if
    if (metTYPE(1:6) .eq. 'norESM') then
       !// norESM netcdf file
       FNAME1 = trim(MPATH1)//trim(MFILE3)// &
            '_MET.cam.h1.'//CYEAR//'-'//CMON//'-'//CDATE//'-10800nc4.nc'
       FNAME2 = trim(MPATH1)//trim(MFILE3)// &
            '_MET.cam.h2.'//CYEAR//'-'//CMON//'-'//CDATE//'-10800nc4.nc'
       FNAME3 = trim(MPATH1)//trim(MFILE3)// &
            '_MET.cam.h3.'//CYEAR//'-'//CMON//'-'//CDATE//'-10800nc4.nc'

       !FNAME1 = trim(MPATH1)//trim(MPATH2)//trim(MFILE3)// &
       !     '.cam.h1.'//CYEAR//'-'//CMON//'-'//CDATE//'-10800nc4.nc'
       !FNAME2 = trim(MPATH1)//trim(MPATH2)//trim(MFILE3)// &
       !     '.cam.h2.'//CYEAR//'-'//CMON//'-'//CDATE//'-10800nc4.nc'
       !FNAME3 = trim(MPATH1)//trim(MPATH2)//trim(MFILE3)// &
       !     '.cam.h3.'//CYEAR//'-'//CMON//'-'//CDATE//'-10800nc4.nc'
       ! This file is not available
       !FNAME4 = trim(MPATH1)//trim(MPATH2)//trim(MFILE3)// &
       !     '.clm2.h1.'//CYEAR//'-'//CMON//'-'//CDATE//'-10800_nc4.nc'
    else
       write(6,'(a)') f90file//':'//subr// &
            ': Not set up for metTYPE: '//trim(metTYPE)
       stop
    end if

    !// Check if files exist
    inquire(FILE=trim(FNAME1), exist=fex)
    if (.not. fex) then
       write(6,'(a)') f90file//':'//subr// &
            ': No such file: '//trim(FNAME1)
       stop
    end if



    !//---------------------------------------------------------------------
    !// Initial step - setup P, T and Q
    !//---------------------------------------------------------------------
    if (ILOOP .le. 0) then

       !// Set up level weightings if vertical resolution degraded
       do LL = LPARW+1, 1, -1
          L   = LMMAP(LL)
          LMAP(L) = LL
       end do

       write(6,'(a)') 'Initializing meteorological data'


       !// Pressure field --------------------------------------------------
       !// -----------------------------------------------------------------
       call readnc_2d_from3d(FNAME1, 'lon',IPARW,'lat',JPARWNE1,&
            'time',1,'PS',inXY,silent=silent)
       !// Pa -> hPa
       inXY(:,:) = inXY(:,:) * 1.d-2
  
       !// Combine polar pie pieces
       call combinePPP(inXY,PW,JPARWNE1,JPARW)

       !// Put PW into P, degrading or not
       call TRUNG8(PW, P, ZDEGI, ZDEGJ, IMAP, JMAP, IDGRD, &
            JDGRD, IPARW, JPARW, IPAR, JPAR, 1, 1)

       if (verbose) call gotData(FLD_ID,'SFC','Surface pressure (P)')


       !// Temperature -----------------------------------------------------
       !// -----------------------------------------------------------------
       call readnc_3d_from4d(FNAME1, 'lon',IPARW,'lat',JPARWNE1, &
            'lev',LPARW,'time',1,'T',inXYZ,silent=silent)
       !// Turn field upside down
       do L = 1, LPARW
          LL = LPARW + 1 - L
          !// Combine polar pie pieces
          call combinePPP(inXYZ(:,:,L),W3Da(:,:,LL),JPARWNE1,JPARW)
       end do

       !// Collapse layers
       TW(:,:,:) = 0._r8
       do L = 1, LPAR
         do LL = LMAP(L), LMAP(L+1) - 1
           do J = 1, JPARW
             do I = 1, IPARW
               TW(I,J,L) = TW(I,J,L) + W3Da(I,J,LL) * XLMMAP(LL)
             end do
           end do
         end do
       end do
       !// Put TW into T, degrading or not
       call TRUNG8(TW, T, ZDEGI, ZDEGJ, IMAP, JMAP, IDGRD, &
            JDGRD, IPARW, JPARW, IPAR, JPAR, LPAR, 1)

       !// Test to check temperature field
       if (minval(W3Da(:,:,1)) .le. 0._r8) then
          write(6,'(a)')  f90file//':'//subr// &
               ': Surface temperature is <= 0?'
          write(6,'(a,f9.3)') '  MIN sfc T: ',minval(W3Da(:,:,1))
          stop
       end if

       if (verbose) call gotData(FLD_ID,'3di','Temperature (T)')


       !// Water vapour ----------------------------------------------------
       !// -----------------------------------------------------------------
       call readnc_3d_from4d(FNAME1, 'lon',IPARW,'lat',JPARWNE1, &
            'lev',LPARW,'time',1,'Q',inXYZ,silent=silent)
       !// Turn field upside down
       do L = 1, LPARW
          LL = LPARW + 1 - L
          !// Combine polar pie pieces
          call combinePPP(inXYZ(:,:,L),W3Da(:,:,LL),JPARWNE1,JPARW)
       end do

       !// Collapse layers
       QW(:,:,:) = 0._r8
       do L = 1,LPAR
         do LL = LMAP(L),LMAP(L+1)-1
           do J = 1,JPARW
             do I = 1,IPARW
               QW(I,J,L) = QW(I,J,L) + W3Da(I,J,LL) * XLMMAP(LL)
             end do
           end do
         end do
       end do
       !// There may be some entries of small negative numbers
       if (minval(QW) .lt. 0._r8) then
          print*,'update_metdata: min QW:',minval(QW),', setting to',QMIN
          do L = 1, LPAR
            do J = 1, JPARW
              do I = 1, IPARW
                if (QW(I,J,L) .lt. 0._r8) QW(I,J,L) = QMIN
              end do
            end do
          end do
       end if

       !// Put QW into Q, degrading or not
       call TRUNG8(QW, Q, ZDEGI, ZDEGJ, IMAP, JMAP, IDGRD, &
            JDGRD, IPARW, JPARW, IPAR, JPAR, LPAR,1)

       if (verbose) call gotData(FLD_ID,'3di','Specific humidity (Q)')

       !// Polar cap filtering
       call EPZ_TQ(T, Q, XYZA, XYZB, P, IMEPZ,IPAR,JPAR,LPAR,IPAR,JPAR,LPAR)
       call EPZ_P(P, IMEPZ, IPAR,JPAR, IPAR, JPAR)

       !// Initialization is done - these steps are for all time steps
       return
    end if !// if (ILOOP .le. 0) then


    !//---------------------------------------------------------------------
    !// All time steps - update all meteorological fields
    !//---------------------------------------------------------------------
    if (verbose) then
       write(6,'(a,i5)') f90file//':'//subr// &
            ': Reading new metdata JDAY: '//TMET//', NMET:',NMET
    end if

    !// Clear arrays
    U(:,:,:) = 0._r8
    V(:,:,:) = 0._r8
    T(:,:,:) = 0._r8
    Q(:,:,:) = 0._r8
    CWETN(:,:,:) = 0._r8
    CWETE(:,:,:) = 0._r8
    CWETD(:,:,:) = 0._r8
    CENTU(:,:,:) = 0._r8
    CENTD(:,:,:) = 0._r8
    PRECCNV(:,:,:) = 0._r8
    PRECLS(:,:,:) = 0._r8
    CLDFR(:,:,:) = 0._r8
    CLDLWC(:,:,:) = 0._r8
    CLDIWC(:,:,:) = 0._r8
    !// No need to clear 2D arrays; they are fully updated below
    SLH(:,:) = 0._r8
    SHF(:,:) = 0._r8
    SMF(:,:) = 0._r8
    SFT(:,:) = 0._r8
    SFQ(:,:) = 0._r8
    SFU(:,:) = 0._r8
    SFV(:,:) = 0._r8
    BLH(:,:) = 0._r8
    MSLP(:,:) = 0._r8
    SA(:,:) = 0._r8



    !//---------------------------------------------------------------------
    !// 3D GRIDDED DATA - INSTANTANEOUS
    !//---------------------------------------------------------------------

    !// Pressure field -----------------------------------------------------
    !// --------------------------------------------------------------------
    call readnc_2d_from3d(FNAME1, 'lon',IPARW,'lat',JPARWNE1,&
         'time',NMET,'PS',inXY,silent=silent)
    !// Pa -> hPa
    inXY(:,:) = inXY(:,:) * 1.d-2
  
    !// Combine polar pie pieces
    call combinePPP(inXY,PW,JPARWNE1,JPARW)

    !// Put PW into P, degrading or not
    call TRUNG8(PW, P, ZDEGI, ZDEGJ, IMAP, JMAP, IDGRD, &
         JDGRD, IPARW, JPARW, IPAR, JPAR, 1, 1)

    if (verbose) call gotData(FLD_ID,'SFC','Surface pressure (P)')


    !// Temperature --------------------------------------------------------
    !// --------------------------------------------------------------------
    call readnc_3d_from4d(FNAME1, 'lon',IPARW,'lat',JPARWNE1, &
         'lev',LPARW,'time',NMET,'T',inXYZ,silent=silent)
    !// Turn field upside down
    do L = 1, LPARW
       LL = LPARW + 1 - L
       !// Combine polar pie pieces
       call combinePPP(inXYZ(:,:,L),W3Da(:,:,LL),JPARWNE1,JPARW)
    end do

    !// Collapse layers
    TW(:,:,:) = 0._r8
    do L = 1, LPAR
      do LL = LMAP(L), LMAP(L+1) - 1
        do J = 1, JPARW
          do I = 1, IPARW
            TW(I,J,L) = TW(I,J,L) + W3Da(I,J,LL) * XLMMAP(LL)
          end do
        end do
      end do
    end do
    !// Put TW into T, degrading or not
    call TRUNG8(TW, T, ZDEGI, ZDEGJ, IMAP, JMAP, IDGRD, &
         JDGRD, IPARW, JPARW, IPAR, JPAR, LPAR, 1)

    if (verbose) call gotData(FLD_ID,'3di','Temperature (T)')


    !// Zonal wind (U) -----------------------------------------------------
    !// --------------------------------------------------------------------
    !// m/s -> kg/s
    call readnc_3d_from4d(FNAME1, 'lon',IPARW,'lat',JPARWNE1, &
         'lev',LPARW,'time',NMET,'U',inXYZ,silent=silent)
    !// Turn field upside down
    do L = 1, LPARW
       LL = LPARW + 1 - L
       !// Combine polar pie pieces
       call combinePPP(inXYZ(:,:,L),W3Da(:,:,LL),JPARWNE1,JPARW)
    end do

    call readnc_3d_from4d(FNAME1, 'lon',IPARW,'slat',JPARWNE1-1, &
         'lev',LPARW,'time',NMET,'US',inXYm1Z,silent=silent)
    !// Turn field upside down
    do L = 1, LPARW
       LL = LPARW + 1 - L
       !// Combine polar pie pieces
       call combinePPP(inXYm1Z(:,:,L),W3Das(:,:,LL),JPARWNE1-1,JPARW-1)
    end do




    !// 1. What to do with winds
    !//    U needs to be shifted: U(I) is to be eastward flux into
    !//    box I, i.e. at the western edge of the box.
    !//      At poles, the grid point shifts halfway towards meridional
    !//      staggered location.
    !//    V: In CTM3, V(J) is wind (converted to flux) into grid box J.
    !//       It is by default set to zero at the poles, even though
    !//       NorESM has V at polar cap.
    !//       NorESM-V thus has to be shifted polewards to the
    !//       V-staggered positions:
    !//       V(1) = 0.d0 ! if possible: inV(1) !// wind into pie piece
    !//       do J=2,JPAR
    !//          V(J) = 0.5d0 * (inV(J-1) + inV(J))
    !//       end do
    !// 2. What to do with grid center polar cap values for other
    !//    species?
    !//    I will use the polar pie pieces as though they are given
    !//    in their new centers.

    !// Change grid centers from  polar cap to pie piece centers
    !// Use mean of polar cap wind and staggered grid wind.
    do L = 1, LPARW
       do I = 1, IPARW
          W3Da(I,1,L) = 0.5_r8 * (W3Da(I,1,L) + W3Das(I,1,L))
          W3Da(I,JPARW,L) = 0.5_r8 * (W3Da(I,JPARW,L) + W3Das(I,JPARW-1,L))
       end do
    end do


    !// Save grid center m/s
    W3Db(:,:,:) = W3Da(:,:,:)

    !// CTM3 needs U-wind on edges (INTO box from west); converted here:
    do L = 1, LPARW
       do J = 1, JPARW
          do I = 1, IPARW
             UTMP(I) = W3Da(I,J,L)
          end do
          do I = 2, IPARW
             W3Da(I,J,L) = 0.5_r8 * (UTMP(I-1) + UTMP(I))
          end do
          W3Da(1,J,L) = 0.5_r8 * (UTMP(IPARW) + UTMP(1))
       end do
    end do


    !// Convert U to flux out of grid box (to the east) [kg/s]
    do L = 1, LPARW
       DETA = ETAAW(L) - ETAAW(L+1)
       DETB = ETABW(L) - ETABW(L+1)
       do J = 1, JPARW
          BAND = A0 * (WGLYE(J+1) - WGLYE(J))
          !// Have U at edges (unit conversion needs pressure on edge)
          do I = 2, IPARW
             DELP = DETA + DETB * (PW(I-1,J) + PW(I,J)) * 0.5_r8
             W3Da(I,J,L) = W3Da(I,J,L) * DELP * BAND
          end do
          DELP = DETA + DETB * (PW(1,J) + PW(IPARW,J)) * 0.5_r8
          W3Da(1,J,L) = W3Da(1,J,L) * DELP * BAND
       end do
    end do

    if (ldeg) then
       do L = 1, LPAR
         do LL = LMAP(L),LMAP(L+1)-1
           do I = 1, IPAR
             II = IMAP(1,I)
             do J = 1, JPAR
               do JJ = 1, JDGRD
                 U(I,J,L) = U(I,J,L) + W3Da(II,JMAP(JJ,J),LL)
               end do
             end do
           end do
         end do
       end do
    else
       do L = 1, LPAR
         do LL = LMAP(L),LMAP(L+1)-1
           do J = 1, JPAR
             do I = 1, IPAR
               U(I,J,L) = U(I,J,L) + W3Da(I,J,LL)
             end do
           end do
         end do
       end do
    end if

    if (verbose) call gotData(FLD_ID,'3di','Zonal wind (U)')

    !// Save UMS -----------------------------------------------------------
    !//---------------------------------------------------------------------
    !// Degrade vertically and horizontally, put into (LPAR,IPAR,JPAR)
    call ijlw2lij(W3Db, LMAP, XLMMAP, IPAR, JPAR, LPAR, IPARW, &
         JPARW, LPARW, ZDEGI, ZDEGJ, IMAP, JMAP, IDGRD, JDGRD, UMS)
    if (verbose) then
       write(*,'(a,2es12.3)') &
            '   Min/max UMS  ', minval(UMS),maxval(UMS)
    end if

    !// Meridional wind (V) ------------------------------------------------
    !// --------------------------------------------------------------------
    call readnc_3d_from4d(FNAME1, 'lon',IPARW,'lat',JPARWNE1, &
         'lev',LPARW,'time',NMET,'V',inXYZ,silent=silent)
    !// Turn field upside down
    do L = 1, LPARW
       LL = LPARW + 1 - L
       !// Combine polar pie pieces
       call combinePPP(inXYZ(:,:,L),W3Db(:,:,LL),JPARWNE1,JPARW)
    end do

    !// First prepare V-wind; this is center values when
    !// read from NorESM data. In VMSW we save the polar pie
    !// piece centers, just as for U.
    !// These centers are 1/3 distance from polar cap and 2/3 from
    !// the next grid box center, so I use these as weightings.
    W3Da(:,:,:) = W3Db(:,:,:)
    do L = 1, LPARW
       do I = 1, IPARW
          W3Da(I,1,L) = 1._r8/3._r8 * W3Da(I,2,L) &
                        + 2._r8/3._r8 * W3Da(I,1,L)
          W3Da(I,JPARW,L) = 1._r8/3._r8 * W3Da(I,JPARW-1,L) &
                        + 2._r8/3._r8 * W3Da(I,JPAR,L)
       end do
    end do

    !// But CTM3 needs V-wind on edges, so we convert to this.
    !// V is to be flux INTO the box, so V(I,2,L) is out of polar pie
    !// piece and into J=2. CTM3 disregards V at polar cap, even though
    !// there are wind data at the polar cap in NorESM.
    !// I have no work-around at the moment.
    do L = 1, LPARW
       do I = 1, IPARW
          do J = 1, JPARW
             VTMP(J,1) = W3Db(I,J,L)
          end do
          !// South Pole polar cap; will not be used
          W3Db(I,1,L) = 0.d0
          do J = 2, JPARW
             W3Db(I,J,L) = 0.5_r8 * (VTMP(J-1,1) + VTMP(J,1))
          end do
          !// At North Pole (V(JPARW+1), we will override with zero,
          !// so array does not cover this value.
       end do
    end do


    !// scale from V=v*cos() to v (remember V is on edge already)
    do L = 1, LPARW
       DETA = ETAAW(L) - ETAAW(L+1)
       DETB = ETABW(L) - ETABW(L+1)

       VTMP(:,:) = 0._r8 !// Needed for getting VMSW
       !// W3Db does not start at SP, but at the J=2. It means
       !// W3Db is flux out of box J, not into J.
       !// Given all edge points, the number of boxes should be
       !// JPARW+1, covering 1:JPARW+1. But both the poles should
       !// have zero wind, so we only need JPARW-1 boxes.

       do J = 2, JPARW
          VEDGE  = 2._r8 * CPI * A0 * COS(WGLYE(J)) / real(IPARW, r8)
          do I = 1, IPARW
             DELP = DETA + DETB * (PW(I,J-1) + PW(I,J)) * 0.5_r8
             W3Db(I,J,L) = W3Db(I,J,L) * VEDGE * DELP
          end do
       end do
    end do

    if (ldeg) then
       do L = 1, LPAR
         do LL = LMAP(L),LMAP(L+1)-1
           do J = 1, JPAR
             JJ = JMAP(1,J)
             do I = 1, IPAR
               do II = 1, IDGRD
                 V(I,J,L) = V(I,J,L) + W3Db(IMAP(II,I),JJ,LL)
               end do
             end do
           end do
         end do
       end do
    else
       do L = 1, LPAR
         do LL = LMAP(L),LMAP(L+1)-1
           do J = 1, JPAR
             do I = 1, IPAR
               V(I,J,L) = V(I,J,L) + W3Db(I,J,LL)
             end do
           end do
         end do
       end do
    end if

    if (verbose) call gotData(FLD_ID,'3di','Meridional wind (V)')


    !// Save VMS -----------------------------------------------------------
    !//---------------------------------------------------------------------
    !// Degrade vertically and horizontally, put into (LPAR,IPAR,JPAR)
    call ijlw2lij(W3Da, LMAP, XLMMAP, IPAR, JPAR, LPAR, IPARW, &
         JPARW, LPARW, ZDEGI, ZDEGJ, IMAP, JMAP, IDGRD, JDGRD, VMS)

    if (verbose) then
       write(*,'(a,2es12.3)') &
            '   Min/max VMS  ', minval(VMS),maxval(VMS)
    end if



    !//---------------------------------------------------------------------
    !// 3-d GRID POINT DATA
    !//---------------------------------------------------------------------


    !// Water vapour -------------------------------------------------------
    !// --------------------------------------------------------------------
    call readnc_3d_from4d(FNAME1, 'lon',IPARW,'lat',JPARWNE1, &
         'lev',LPARW,'time',1,'Q',inXYZ,silent=silent)
    !// Turn field upside down
    do L = 1, LPARW
       LL = LPARW + 1 - L
       !// Combine polar pie pieces
       call combinePPP(inXYZ(:,:,L),W3Da(:,:,LL),JPARWNE1,JPARW)
    end do
    !// Collapse layers
    QW(:,:,:) = 0._r8
    do L = 1, LPAR
      do LL = LMAP(L), LMAP(L+1)-1
        do J = 1, JPARW
          do I = 1, IPARW
            QW(I,J,L) = QW(I,J,L) + W3Da(I,J,LL) * XLMMAP(LL)
          end do
        end do
      end do
    end do
    if (minval(QW) .lt. 0._r8) then
       print*,'update_metdata: min QW:',minval(QW),', setting to',QMIN
       do L = 1, LPAR
         do J = 1, JPARW
           do I = 1, IPARW
             if (QW(I,J,L) .lt. 0._r8) QW(I,J,L) = QMIN
           end do
         end do
       end do
    end if

    !// Put QW into Q, degrading or not
    call TRUNG8(QW, Q, ZDEGI, ZDEGJ, IMAP, JMAP, IDGRD, &
         JDGRD, IPARW, JPARW, IPAR, JPAR, LPAR, 1)

    if (verbose) call gotData(FLD_ID,'3di','Specific humidity (Q)')


    !// Altitudes - calculated ---------------------------------------------
    !// --------------------------------------------------------------------
    !// R = 287.*(1-Q) + 461.5*Q -- assume 0.5% bl w.v.==> R = 288.
    !// delta-z (m) = dln(P) * R * T / g   where R/g = 288/9.81 = 29.36
    !// CTM3: We use Q directly instead of assuming 0.5%;
    !// Using Rd=287 and Tv=T*(1 + 0.6*q); 287/9.80665=29.26586
    do J = 1, JPAR
      do I = 1, IPAR
        PSRF  = P(I,J)
        !// Surface pressure; must be set for DELZ to be calculated
        POFLE(1)  = PSRF
        !// Height of box bottom, above sea level (acts as topography)
        ZOFLE(1,I,J)  = 16.e3_r8 * log10(1013.25_r8 / PMEAN(I,J))
        if (ZOFLE(1,I,J) .ne. ZOFLE(1,I,J)) then
           write(6,'(a,i3,2i5,2es12.2)')  f90file//':'//subr// &
                ': ZOFLE a',1,i,j,PMEAN(I,J),PMEANW(I,J)
           write(6,*) pmean(:,j)
           stop
        end if
        do L = 2, LPAR + 1
          !// Pressure of box bottom
          POFLE(L)  = ETAA(L) + ETAB(L) * PSRF
          !DELZ  = -29.36_r8 * T(I,J,L-1) * log(POFLE(L)/POFLE(L-1))
          !// Thickness of layer (L-1) (remember ZOFLE starts at topography)
          DELZ = -29.26586_r8 * T(I,J,L-1) * (1._r8 + 0.6_r8 * Q(I,J,L-1)) &
                             * log(POFLE(L)/POFLE(L-1))
          !// Add DELZ of (L-1) to get box bottom height of L
          ZOFLE(L,I,J) = ZOFLE(L-1,I,J) + DELZ
          if (ZOFLE(L,I,J) .ne. ZOFLE(L,I,J)) then
             write(6,'(a,i3,2i5,4es12.2)')  f90file//':'//subr// &
                  ': ZOFLE b',l,i,j,delz,q(i,j,l-1),&
                  T(I,J,L-1),psrf
             stop
          end if
        end do
      end do
    end do

    !// Also set up ZOFLEW for metdata grid
    do J = 1, JPARW
      do I = 1, IPARW
        PSRF  = PW(I,J)
        POFLE(1)  = PSRF
        ZOFLEW(1,I,J)  = 16e3_r8 * log10(1013.25_r8 / PMEANW(I,J))
        do L = 2, LPAR + 1
          POFLE(L)  = ETAA(L) + ETAB(L)*PSRF
          !DELZ  = -29.36_r8 * TW(I,J,L-1) * log(POFLE(L)/POFLE(L-1))
          !// Thickness of layer below
          DELZ = -29.26586_r8 * TW(I,J,L-1)*(1._r8 + 0.6_r8*QW(I,J,L-1)) &
                             * log(POFLE(L)/POFLE(L-1))
          ZOFLEW(L,I,J) = ZOFLEW(L-1,I,J) + DELZ
        end do
      end do
    end do


    !// Potantial vorticity ------------------------------------------------
    !// --------------------------------------------------------------------
    !// Not available yet
    !// Convert to PVU
    !// W3Da(:,:,:) = W3Da(:,:,:) * 1.e6_r8
    W3Da(:,:,:) = 0._r8
    if (maxval(W3Da) .eq. 0._r8) then
       !// PV was not on available.
       !// Generate PV on model resolution and convert to PVU
       call get_pvu()
    else
       !// Transform into PVU-array
       call ijlw2lij(W3Da, LMAP, XLMMAP, IPAR, JPAR, LPAR, IPARW, &
            JPARW, LPARW, ZDEGI, ZDEGJ, IMAP, JMAP, IDGRD, JDGRD, PVU)
    end if


    !// Cloud Liquid Water Content [Kg/Kg] - midpoint value ----------------
    !// --------------------------------------------------------------------
    !// Cloud routine needs CLDLWCW in any case, so we calculate
    !// that first. Could repeat with R8XYZ, but rather do
    !// degradation of CLDLWCW.
    call readnc_3d_from4d(FNAME2, 'lon',IPARW,'lat',JPARWNE1, &
            'lev',LPARW,'time',NMET,'CLDLIQ',inXYZ,silent=silent)
    !// Turn field upside down
    do L = 1,LPARW
       LL = LPARW + 1 - L
       !// Combine polar pie pieces
       call combinePPP(inXYZ(:,:,L),W3Da(:,:,LL),JPARWNE1,JPARW)
    end do

    CLDLWCW(:,:,:)  = 0._r8
    do L = 1, LWEPAR
      do LL = LMAP(L), LMAP(L+1)-1
        do J = 1, JPARW
          do I = 1, IPARW
            CLDLWCW(I,J,L) = CLDLWCW(I,J,L) + W3Da(I,J,LL) * XLMMAP(LL)
          end do
        end do
      end do
    end do
    !// Put CLDLWCW into LCDLWC, degrading or not
    call TRUNG8(CLDLWCW, CLDLWC, ZDEGI, ZDEGJ, IMAP, JMAP, IDGRD, &
         JDGRD, IPARW, JPARW, IPAR, JPAR, LWEPAR, 1)


    !// Cloud Ice Water Content [Kg/Kg] - midpoint value -------------------
    !// --------------------------------------------------------------------
    !// Cloud routine needs CLDIWCW in any case, so we calculate
    !// that first.
    call readnc_3d_from4d(FNAME2, 'lon',IPARW,'lat',JPARWNE1, &
            'lev',LPARW,'time',NMET,'CLDICE',inXYZ,silent=silent)
    !// Turn field upside down
    do L = 1,LPARW
       LL = LPARW + 1 - L
       !// Combine polar pie pieces
       call combinePPP(inXYZ(:,:,L),W3Da(:,:,LL),JPARWNE1,JPARW)
    end do

    CLDIWCW(:,:,:)  = 0._r8
    do L = 1, LWEPAR
      do LL = LMAP(L), LMAP(L+1)-1
        do J = 1, JPARW
          do I = 1, IPARW
            CLDIWCW(I,J,L) = CLDIWCW(I,J,L) + W3Da(I,J,LL) * XLMMAP(LL)
          end do
        end do
      end do
    end do
    !// Put CLDIWCW into LCDIWC, degrading or not
    call TRUNG8(CLDIWCW, CLDIWC, ZDEGI, ZDEGJ, IMAP, JMAP, IDGRD, &
         JDGRD, IPARW, JPARW, IPAR, JPAR, LWEPAR, 1)


    !// Cloud Fraction [0,1] - midpoint value ------------------------------
    !// --------------------------------------------------------------------
    !// Cloud routine needs CLDFRW in any case, so we calculate
    !// that first.
    call readnc_3d_from4d(FNAME2, 'lon',IPARW,'lat',JPARWNE1, &
            'lev',LPARW,'time',NMET,'CLOUD',inXYZ,silent=silent)
    !// Turn field upside down
    do L = 1,LPARW
       LL = LPARW + 1 - L
       !// Combine polar pie pieces
       call combinePPP(inXYZ(:,:,L),W3Da(:,:,LL),JPARWNE1,JPARW)
    end do

    CLDFRW(:,:,:) = 0._r8
    do L = 1, LWEPAR
      do LL = LMAP(L), LMAP(L+1)-1
        do J = 1, JPARW
          do I = 1, IPARW
            CLDFRW(I,J,L) = CLDFRW(I,J,L) &
                    + max(min(W3Da(I,J,LL),1._r8),0._r8) * XLMMAP(LL)
          end do
        end do
      end do
    end do
    !// Put CLDFRW into LCDFR, degrading or not
    call TRUNG8(CLDFRW, CLDFR, ZDEGI, ZDEGJ, IMAP, JMAP, IDGRD, &
         JDGRD, IPARW, JPARW, IPAR, JPAR, LWEPAR, 1)


    !//---------------------------------------------------------------------
    !// 3D GRIDDED DATA - ACCUMULATED
    !//---------------------------------------------------------------------

    !// Mass Flux updrafts [accumulated kg/(m^2*s)] ------------------------
    !// --------------------------------------------------------------------
    !// - Mass flux is through BOTTOM EDGE of grid box (i.e. model
    !//   half layers).
    !//
    !// Consists of deep convection and shallow convection, given on
    !// separate fields. Fields are averaged over time step, so there
    !// is no need to divide by DT.
    !//
    !// Deep convection is on lev, and has non-zero values at surface
    !// layer, both for updraft (positive values) and downdraft
    !// (negative). This means that flux is on upper edges/interfaces.
    !// NorESM thus has zero ZMMU at model top (no flux out to space).
    !//
    !// Moist shallow convection mass flux is net of up-down, and is
    !// given on ilev. Thus, the values are flux into the box from
    !// below, and values are zero at surface and at LPARW+1.
    !//
    !// Read both datasets and add together:
    !// Deep convection updraft mass flux
    call readnc_3d_from4d(FNAME1, 'lon',IPARW,'lat',JPARWNE1, &
         'lev',LPARW,'time',NMET,'ZMMU',inXYZ,silent=silent)

    !// Moist shallow convection mass flux (net of up-down)
    call readnc_3d_from4d(FNAME1, 'lon',IPARW,'lat',JPARWNE1, &
         'ilev',LPARW+1,'time',NMET,'SHLWFLX',inXYZp1,silent=silent)

    !// Add fields
    inXYZ(:,:,:) = inXYZ(:,:,:) + inXYZp1(:,:,2:LPARW+1)

    !// CTM3: Mass flux is through BOTTOM EDGE of grid box
    !//       (i.e. model half layers), so surface layer must
    !//       have zero flux.
    !//       - Scale with area --> [kg/s]
    !//       - Data are averages, not accumulated, so we do
    !//         not need to divide by DT.
    !//
    !// Turn field upside down, and do the vertical shift
    !// at the same time so that values are flux through bottom
    !// of grid box
    W3Da(:,:,1) = 0.d0
    do L = 2,LPARW !// From 2 to account for the shifting
       LL = LPARW + 1 - L
       !// L=LPARW -> LL=1 (could add 1, but rather add to LL below)
       !// ZMMD: Shift the CTM3 values up 1 level
       !// Combine polar pie pieces
       call combinePPP(inXYZ(:,:,L),W3Da(:,:,LL+1),JPARWNE1,JPARW)
    end do

    !// Override fluxes in stratosphere?
    !W3Da(:,:,LWEPARW+1:LPARW) = 0._r8
    !// Possibly degrade without collapsing
    call TRUNG8(W3Da, R8XYZ, ZDEGI, ZDEGJ, IMAP, JMAP, IDGRD, &
         JDGRD,IPARW,JPARW,IPAR,JPAR,LPARW,1)

    CWETE(:,:,:) = 0._r8 !// No flux up from surface
    !// Start retrieval at layer 1, since degrWK3D is already
    !// converted to CTM definition
    do L = 1, LWEPAR
       !// LMAP is full-level, but for since mass fluxes are not stored
       !// for surface, and starts at layer 2, we need to subtract 1.
       !// What comes into L=2, comes from LMAP(2)-1.
       !// For L40, L=2 and LMAP(2)-1 = 1, while
       !// for L37, L=2 and LMAP(2)-1 = 3
       LL = LMAP(L)
       do J = 1, JPAR
          do I = 1, IPAR
             !// Filter values. File data has minimum values less than zero.
             !// Hence we treat values less that that as as zero
             if (R8XYZ(I,J,LL) .gt. mincwetelim) then
                CWETE(I,J,L) = R8XYZ(I,J,LL) * AREAXY(I,J) !// kg/s
             else
                CWETE(I,J,L) = 0._r8
             end if
          end do
       end do
    end do

    !// Need to scale down convection to avoid too short time steps
    !CWETE(:,:,:) = CWETE(:,:,:) * 0.5_r8

    if (verbose) call gotData(212,'3da','ZMMU')

    !// Mass Flux downdrafts [accumulated kg/(m^2*s)] ----------------------
    !// --------------------------------------------------------------------
    !// - Mass flux is through BOTTOM EDGE of grid box (i.e. model
    !//   half layers).
    !//   This means that the flux for layer 1 is zero, and that
    !//   downward flux is NEGATIVE, going out at the bottom of the box.
    !// - EC-data are stored from half layer 2, since the flux into
    !//   layer 1 is always zero (no flux in through surface).
    !// - For a grid box in layer L, -FD(L) goes out at bottom
    !//   and -FD(L+1) comes in from above (FD is negative). The
    !//   balance with entrainment E and detrainment D is:
    !//   FD(L) - FD(L+1) = E - D
    call readnc_3d_from4d(FNAME1, 'lon',IPARW,'lat',JPARWNE1, &
         'lev',LPARW,'time',NMET,'ZMMD',inXYZ,silent=silent)

    !// Turn field upside down, and do the vertical shift
    !// at the same time so that values are flux through bottom
    !// of grid box
    W3Da(:,:,1) = 0.d0
    do L = 2,LPARW !// From 2 to account for the shifting
       LL = LPARW + 1 - L
       !// L=LPARW -> LL=1 (could add 1, but rather add to LL below)
       !// ZMMD: Shift the CTM3 values up 1 level
       !// Combine polar pie pieces
       call combinePPP(inXYZ(:,:,L),W3Da(:,:,LL+1),JPARWNE1,JPARW)
    end do

    W3Da(:,:,LWEPARW+1:LPARW) = 0._r8
    !// Possibly degrade without collapsing
    call TRUNG8(W3Da, R8XYZ, ZDEGI, ZDEGJ, IMAP, JMAP, IDGRD, &
         JDGRD,IPARW,JPARW,IPAR,JPAR,LPARW,1)

    CWETD(:,:,:) = 0._r8 !// No flux down to surface
    !// Start retrieval at layer 1, since degrWK3D is already
    !// converted to CTM definition
    do L = 1, LWDPAR
       LL = LMAP(L)
       do J = 1, JPAR
          do I = 1, IPAR
             if (R8XYZ(I,J,LL) .lt. maxcwetdlim) then
                CWETD(I,J,L) = R8XYZ(I,J,LL) * AREAXY(I,J) !// kg/s
             else
                CWETD(I,J,L) = 0._r8
             end if
          end do
       end do
    end do

    if (verbose) call gotData(213,'3da','ZMMD')


    !// Updrafts entrainment rate [accumulated kg/(m2*s)] ------------------
    !// --------------------------------------------------------------------
    !// Entrainment is read from file.
    !// The rate kg/m2/s, as average over the meteorological time step.
    !// Detrainment must be summed up when collapsing layers !
    call readnc_3d_from4d(FNAME1, 'lon',IPARW,'lat',JPARWNE1, &
         'lev',LPARW,'time',NMET,'ZMEU',inXYZ,silent=silent)

    !// Turn field upside down
    do L = 1,LPARW
       LL = LPARW + 1 - L
       !// Combine polar pie pieces
       call combinePPP(inXYZ(:,:,L),W3Da(:,:,LL),JPARWNE1,JPARW)
    end do

    W3Da(:,:,LWEPARW+1:LPARW) = 0._r8
    !// Possibly degrade without collapsing
    call TRUNG8(W3Da, R8XYZ, ZDEGI, ZDEGJ, IMAP, JMAP, IDGRD, &
         JDGRD,IPARW,JPARW,IPAR,JPAR,LPARW,1)

    CENTU(:,:,:) = 0._r8
    do J = 1, JPAR
       do I = 1, IPAR
          do L = 1, LWEPAR
             do LL = LMAP(L), LMAP(L+1) - 1
                !// Detrainment must be summed up when collapsing layers
                !if (R8XYZ(I,J,LL) .gt. mincdetulim) then
                   CENTU(I,J,L) = CENTU(I,J,L) &
                        + R8XYZ(I,J,LL) * XLMMAP(LL) &
                          * AREAXY(I,J)
                !end if
             end do
          end do
       end do
    end Do

    if (verbose) call gotData(214,'3da','ZMEU')


    !// Downdrafts entrainment rate [acc. kg/(m2*s)] -----------------------
    !// --------------------------------------------------------------------
    call readnc_3d_from4d(FNAME1, 'lon',IPARW,'lat',JPARWNE1, &
         'lev',LPARW,'time',NMET,'ZMED',inXYZ,silent=silent)

    !// Turn field upside down
    do L = 1,LPARW
       LL = LPARW + 1 - L
       !// Combine polar pie pieces
       call combinePPP(inXYZ(:,:,L),W3Da(:,:,LL),JPARWNE1,JPARW)
    end do

    W3Da(:,:,LWEPARW+1:LPARW) = 0._r8
    !// Possibly degrade without collapsing
    call TRUNG8(W3Da, R8XYZ, ZDEGI, ZDEGJ, IMAP, JMAP, IDGRD, &
         JDGRD,IPARW,JPARW,IPAR,JPAR,LPARW,1)

    CENTD(:,:,:) = 0._r8
    do J = 1, JPAR
       do I = 1, IPAR
          do L = 1, LWDPAR
             do LL = LMAP(L),LMAP(L+1)-1
                !if (R8XYZ(I,J,LL) .gt. mincdetdlim) then
                   CENTD(I,J,L) = CENTD(I,J,L) &
                        + R8XYZ(I,J,LL) * XLMMAP(LL) &
                          * AREAXY(I,J)
                !end if
             end do
          end do
       end do
    end do

    if (verbose) call gotData(215,'3da','ZMED')

    !// Filter fluxes for wrong signs etc.
    !// call fluxfilter(cdetu, cdetd)
    call fluxfilter_ENT()
         
    if (verbose) then
       write(*,'(a)')'*** Done fluxfilter_ENT ***'
       write(*,'(a,2es12.3)') &
            '   Read conv. updr.mass flx.(CWETE)  ',&
            minval(CWETE),maxval(CWETE)
       write(*,'(a,2es12.3)')&
            '   Read conv. downdr.mass flx.(CWETD)',&
            minval(CWETD),maxval(CWETD)
       write(*,'(a,2es12.3)')&
            '   Read updr.entr.flx.(CENTU)        ',&
            minval(CENTU),maxval(CENTU)
       write(*,'(a,2es12.3)')&
            '   Read downdr.entr.flx.(CENTD)      ',&
            minval(CENTD),maxval(CENTD)
    end if


    !// CONVECTIVE RAINFALL [kg/(m^2)/s] (stored as mean)
    !// --------------------------------------------------------------------
    !// Rain out of grid box bottom
    !// Deep convection plus shallow convection

    !// Deep:  ZMFLXPRC(time, ilev, lat, lon)
    call readnc_3d_from4d(FNAME1, 'lon',IPARW,'lat',JPARWNE1, &
         'ilev',LPARW+1,'time',NMET,'ZMFLXPRC',inXYZp1,silent=silent)
    !// Pick edge values
    inXYZ(:,:,1:LPAR) = inXYZp1(:,:,2:LPAR+1)

    !// Shallow: UWFLXPRC(time, ilev, lat, lon)
    call readnc_3d_from4d(FNAME1, 'lon',IPARW,'lat',JPARWNE1, &
         'ilev',LPARW+1,'time',NMET,'UWFLXPRC',inXYZp1,silent=silent)
    !// Add edge values
    inXYZ(:,:,1:LPAR) = inXYZ(:,:,1:LPAR) + inXYZp1(:,:,2:LPAR+1)

    !// Turn field upside down
    do L = 1,LPARW
       LL = LPARW + 1 - L
       !// Combine polar pie pieces
       call combinePPP(inXYZ(:,:,L),W3Da(:,:,LL),JPARWNE1,JPARW)
    end do

    W3Da(:,:,LWEPARW+1:LPARW) = 0._r8
    !// Possibly degrade without collapsing
    call TRUNG8(W3Da, R8XYZ, ZDEGI, ZDEGJ, IMAP, JMAP, IDGRD, &
         JDGRD,IPARW,JPARW,IPAR,JPAR,LPARW,1)

    PRECCNV(:,:,:) = 0._r8
    do L = 1, LWEPAR
       LL = LMAP(L)
       do J = 1, JPAR
          do I = 1, IPAR
             PRECCNV(I,J,L) = &
                  max(0._r8, R8XYZ(I,J,LL) * AREAXY(I,J))
          end do
       end do
    end do

    if (verbose) then
       write(*,'(a,2es12.3)') &
            '   Read conv. precip  ',&
            minval(PRECCNV),maxval(PRECCNV)
    end if



    !// LARGE SCALE RAINFALL [kg/(m^2*s)] (written as mean) -----------
    !// --------------------------------------------------------------------
    !// Rain out of grid box bottom
    !// LS_FLXPRC(time, ilev, lat, lon)
    call readnc_3d_from4d(FNAME1, 'lon',IPARW,'lat',JPARWNE1, &
         'ilev',LPARW+1,'time',NMET,'LS_FLXPRC',inXYZp1,silent=silent)

    !// Turn field upside down
    do L = 2,LPARW+1
       LL = LPARW + 1 - L + 1
       !// Combine polar pie pieces
       call combinePPP(inXYZp1(:,:,L),W3Da(:,:,LL),JPARWNE1,JPARW)
    end do

    W3Da(:,:,LWEPARW+1:LPARW) = 0._r8
    !// Possibly degrade without collapsing
    call TRUNG8(W3Da, R8XYZ, ZDEGI, ZDEGJ, IMAP, JMAP, IDGRD, &
         JDGRD,IPARW,JPARW,IPAR,JPAR,LPARW,1)

    PRECLS(:,:,:) = 0._r8
    do L = 1, LWEPAR
       LL = LMAP(L)
       do J = 1, JPAR
          do I = 1, IPAR
             PRECLS(I,J,L) = &
                  max(0._r8, R8XYZ(I,J,LL) * AREAXY(I,J))
          end do
       end do
    end do

    if (verbose) then
       write(*,'(a,2es12.3)') &
            '   Read larg. precip  ',&
            minval(PRECLS),maxval(PRECLS)
    end if


    !//---------------------------------------------------------------------
    !// Cloud cover routines
    if (LOSLOCHEM) &
         !// New cloud treatment (qcode_60a)
         call CLOUD(CLDFRW,CLDIWCW,CLDLWCW,PW,TW,ETAA,ETAB,AREAXYW, &
                    ZOFLEW,ZDEGI,ZDEGJ,IMAP,JMAP, &
                    SWSTORE,CLDSTORE,TYPSTORE,RAN4,IRAN0, &
                    LCLDAVG,LCLDQMD,LCLDQMN,LCLDRANA,LCLDRANQ)
    !//---------------------------------------------------------------------





    !//---------------------------------------------------------------------
    !// 2-d SURFACE GRID POINT DATA
    !//---------------------------------------------------------------------

    !// SEA ICE COVER (CI) -------------------------------------------------
    !// --------------------------------------------------------------------
    call readnc_2d_from3d(FNAME3, 'lon',IPARW,'lat',JPARWNE1, &
         'time',NMET,'ICEFRAC',inXY,silent=silent)

    !// Combine polar pie pieces
    call combinePPP(inXY(:,:),W2D(:,:),JPARWNE1,JPARW)
    call TRUNG8(W2D, CI, ZDEGI, ZDEGJ, IMAP, JMAP, IDGRD, &
         JDGRD, IPARW, JPARW, IPAR, JPAR, 1, 1)
    if (verbose) call gotData(FLD_ID,'2di','Sea ice (CI)')


    !// Snow Evaporation (ES) (accumulated) --------------------------------
    !// --------------------------------------------------------------------
    !// Unit: m/s water equivalent, accumulated (m w.eq.)
    !// Not found in NorESM
    ES(:,:,:) = 0._r8
    if (verbose) call gotData(FLD_ID,'2da','Snow Evaporation (ES) not available')

    !// Snow Melt (SMLT) (accumulated) -------------------------------------
    !// --------------------------------------------------------------------
    !// Unit: mm/s (water equivalent?)
!    call readnc_2d_from3d(FNAME4, 'lon',IPARW,'lat',JPARWNE1, &
!         'time',NMET,'QSNOMELT',inXY,silent=silent)
!
!    do J = 1,JPARWNE1
!       do I = 1,IPARW
!          if (inXY(I,J) .gt. 1.e20_r8) inXY(I,J) = 0.d0
!       end do
!    end do
!
!    call TRUNG8(W2D, R8XY, ZDEGI, ZDEGJ, IMAP, JMAP, IDGRD, &
!         JDGRD, IPARW, JPARW, IPAR, JPAR, 1, 1)
!    R8XY(:,:) = max(0._r8, R8XY(:,:))  !// Limit to positive values
!    call data2mpblocks(R8XY, SMLT)
!    if (verbose) call gotData(FLD_ID,'2da','Snow Melt (SMLT)')
    SMLT(:,:,:) = 0._r8
    if (verbose) call gotData(FLD_ID,'2da','Snow Melt (SMLT) not available')


    !// Photosynthetically active radiation @ sfc (PAR) (accumulated) ------
    !// --------------------------------------------------------------------
    !// Unit: (W/m2)
    !// CTM3 does not distinguish on leaves in the sun or in
    !// the shade. Cannot use sum, but possibly a weighting.
    !// No possibility to check this, but we can perhaps
    !// assume that at least 50% of forest is in shade.
!    call readnc_2d_from3d(FNAME4, 'lon',IPARW,'lat',JPARWNE1, &
!         'time',NMET,'PARSUN',inXY,silent=silent)
!    call readnc_2d_from3d(FNAME4, 'lon',IPARW,'lat',JPARWNE1, &
!         'time',NMET,'PARSHA',inXY_b,silent=silent)
!
!    do J = 1,JPARWNE1
!       do I = 1,IPARW
!          if (inXY(I,J) .gt. 1.e20_r8) inXY(I,J) = 0.d0
!       end do
!    end do
!    do J = 1,JPARWNE1
!       do I = 1,IPARW
!          if (inXY_b(I,J) .gt. 1.e20_r8) inXY_b(I,J) = 0.d0
!       end do
!    end do
!
!    !// Combine polar pie pieces
!    call combinePPP(inXY(:,:),W2D(:,:),JPARWNE1,JPARW)
!    call TRUNG8(W2D, PAR, ZDEGI, ZDEGJ, IMAP, JMAP, IDGRD, &
!         JDGRD, IPARW, JPARW, IPAR, JPAR, 1, 1)
!    if (verbose) call gotData(FLD_ID,'2da','Photosyn. act. rad. sfc (PAR)')

    PhotActRad(:,:) = 0._r8
    if (verbose) call gotData(FLD_ID,'2da','Photosynthetically active radiation not available')


    !// Snow depth (SD) ----------------------------------------------------
    !// --------------------------------------------------------------------
    !// Unit: m water equivalent
    !// Separate fields for snow on (sea) ice and for land.
    !// SNOWHICE(time, lat, lon) (Water equivalent snow depth)
    call readnc_2d_from3d(FNAME3, 'lon',IPARW,'lat',JPARWNE1, &
         'time',NMET,'SNOWHICE',inXY,silent=silent)
    call readnc_2d_from3d(FNAME3, 'lon',IPARW,'lat',JPARWNE1, &
         'time',NMET,'SNOWHLND',inXY_b,silent=silent)
    do J = 1,JPARWNE1
       do I = 1,IPARW
          if (inXY(I,J) .gt. 1.e20_r8) inXY(I,J) = 0.d0
       end do
    end do
    do J = 1,JPARWNE1
       do I = 1,IPARW
          if (inXY_b(I,J) .gt. 1.e20_r8) inXY_b(I,J) = 0.d0
       end do
    end do

    inXY(:,:) = inXY(:,:) + inXY_b(:,:)

    !// Combine polar pie pieces
    call combinePPP(inXY(:,:),W2D(:,:),JPARWNE1,JPARW)
    call TRUNG8(W2D, SD, ZDEGI, ZDEGJ, IMAP, JMAP, IDGRD, &
         JDGRD, IPARW, JPARW, IPAR, JPAR, 1, 1)
    if (verbose) call gotData(FLD_ID,'2di','Snow depth (SD)')


    !// Large Scale Precipitation (stratiform) (accumulated) ---------------
    !// --------------------------------------------------------------------
    !call get_netcdf_var_2d(FNAME, 'LSPREC',W2D, IPARW, JPARW)
    !call TRUNG8(W2D, R8XY, ZDEGI, ZDEGJ, IMAP, JMAP, IDGRD, &
    !     JDGRD, IPARW, JPARW, IPAR, JPAR, 1, 1)
    !LSPREC(:,:) = R8XY(:,:) * ZDT
    !if (verbose) call gotData(FLD_ID,'2da','Large Scale Precip. (LSPREC)')


    !// Convective Precipitation (accumulated) -----------------------------
    !// --------------------------------------------------------------------
    !call get_netcdf_var_2d(FNAME, 'CONVPREC',W2D, IPARW, JPARW)
    !call TRUNG8(W2D, R8XY, ZDEGI, ZDEGJ, IMAP, JMAP, IDGRD, &
    !     JDGRD, IPARW, JPARW, IPAR, JPAR, 1, 1)
    !CNVPREC(:,:) = R8XY(:,:) * ZDT
    !if (verbose) call gotData(FLD_ID,'2da','Conv. Precip. (CONVPREC)')


    !// SnowFall SF (accumulated) ------------------------------------------
    !// --------------------------------------------------------------------
    !// Unit: m/s
    !// Separated into snow fall for large scale and convective
    call readnc_2d_from3d(FNAME3, 'lon',IPARW,'lat',JPARWNE1, &
         'time',NMET,'PRECSL',inXY,silent=silent)
    call readnc_2d_from3d(FNAME3, 'lon',IPARW,'lat',JPARWNE1, &
         'time',NMET,'PRECSC',inXY_b,silent=silent)

    inXY(:,:) = inXY(:,:) + inXY_b(:,:)

    !// Combine polar pie pieces
    call combinePPP(inXY(:,:),W2D(:,:),JPARWNE1,JPARW)
    call TRUNG8(W2D, R8XY, ZDEGI, ZDEGJ, IMAP, JMAP, IDGRD, &
         JDGRD, IPARW, JPARW, IPAR, JPAR, 1, 1)
    call data2mpblocks(R8XY, SNFL)
    if (verbose) call gotData(FLD_ID,'2da','Snow fall (SNFL)')


    !// Surface Sensible Heat Flux SSHF (accumulated) ----------------------
    !// --------------------------------------------------------------------
    !// Unit: W/m2
    call readnc_2d_from3d(FNAME3, 'lon',IPARW,'lat',JPARWNE1, &
         'time',NMET,'SHFLX',inXY,silent=silent)

    !// Combine polar pie pieces
    call combinePPP(inXY(:,:),W2D(:,:),JPARWNE1,JPARW)
    call TRUNG8(W2D, SHF, ZDEGI, ZDEGJ, IMAP, JMAP, IDGRD, &
         JDGRD, IPARW, JPARW, IPAR, JPAR, 1, 1)
    !// Fluxes of zero cause grief with deposition, PBL.
    !// SHF numbers from -400 to 100, set a small number to avoid
    !// overflow, not worry about small negative going to small positive
    do J = 1, JPAR
       do I = 1, IPAR
          if (abs(SHF(I,J)) .lt. 1.e-6_r8) SHF(I,J) = 1.e-6_r8
       end do
    end do
    if (verbose) call gotData(FLD_ID,'2da','Sfc. sens. heat flux (SHF)')


    !// Surface Latent Heat Flux SLHF (accumulated) ------------------------
    !// --------------------------------------------------------------------
    !// Unit: W/m2
    call readnc_2d_from3d(FNAME3, 'lon',IPARW,'lat',JPARWNE1, &
         'time',NMET,'LHFLX',inXY,silent=silent)

    !// Combine polar pie pieces
    call combinePPP(inXY(:,:),W2D(:,:),JPARWNE1,JPARW)
    call TRUNG8(W2D, SLH, ZDEGI, ZDEGJ, IMAP, JMAP, IDGRD, &
         JDGRD, IPARW, JPARW, IPAR, JPAR, 1, 1)
    !// Fluxes of zero cause grief with deposition, PBL.
    !// SLH: set small value if zero.
    do J = 1, JPAR
       do I = 1, IPAR
          if (SLH(I,J) .eq. 0._r8) SLH(I,J) = 1.e-30_r8
       end do
    end do
    if (verbose) call gotData(FLD_ID,'2da','Sfc. lat. heat flux (SLH)')


    !// Mean Sealevel Pressure (Diagnostic only - not in old 19-layer) -----
    !// --------------------------------------------------------------------
    !// Unit: Pa -> hPa
    !call get_netcdf_var_2d(FNAME, 'MSLP',W2D, IPARW, JPARW)
    !call TRUNG8(W2D, R8XY, ZDEGI, ZDEGJ, IMAP, JMAP, IDGRD, &
    !     JDGRD, IPARW, JPARW, IPAR, JPAR, 1, 1)
    !MSLP(:,:) = R8XY(:,:) * 1.e-2_r8
    !if (verbose) call gotData(FLD_ID,'2di','MSLP')


    !// Boundary Layer Height BLH ------------------------------------------
    !// --------------------------------------------------------------------
    !// Unit: m
    call readnc_2d_from3d(FNAME3, 'lon',IPARW,'lat',JPARWNE1, &
         'time',NMET,'PBLH',inXY,silent=silent)

    !// Combine polar pie pieces
    call combinePPP(inXY(:,:),W2D(:,:),JPARWNE1,JPARW)
    call TRUNG8(W2D, BLH, ZDEGI, ZDEGJ, IMAP, JMAP, IDGRD, &
         JDGRD, IPARW, JPARW, IPAR, JPAR, 1, 1)
    !// Average BLH at poles
    do J = 1, JPAR, JPAR-1
       DMASS = 0._r8
       do I = 1, IPAR
          DMASS = DMASS + BLH(I,J)
       end do
       DMASS = DMASS / real(IPAR, r8)
       do I = 1, IPAR
          BLH(I,J) = DMASS
       end do
    end do
    if (verbose) call gotData(FLD_ID,'2di','Boundary Layer Height (BLH)')


    !// 10m U wind ---------------------------------------------------------
    !// --------------------------------------------------------------------
    !// Unit: m/s
    call readnc_2d_from3d(FNAME3, 'lon',IPARW,'lat',JPARWNE1, &
         'time',NMET,'U10',inXY,silent=silent)

    !// Combine polar pie pieces
    call combinePPP(inXY(:,:),W2D(:,:),JPARWNE1,JPARW)
    call TRUNG8(W2D, SFU, ZDEGI, ZDEGJ, IMAP, JMAP, IDGRD, &
         JDGRD, IPARW, JPARW, IPAR, JPAR, 1, 1)

    !// Use UMS and VMS at surface layer to decompose U10;
    !// these are cos(angle) and sin(angle), and retains
    !// the sign of wind speeds.
    do J = 1, JPAR
       do I = 1, IPAR
          fU = UMS(1,I,J)/(sqrt(UMS(1,I,J)**2 + VMS(1,I,J)**2))
          fV = VMS(1,I,J)/(sqrt(UMS(1,I,J)**2 + VMS(1,I,J)**2))
          SFV(I,J) = SFU(I,J) * fV
          SFU(I,J) = SFU(I,J) * fU
       end do
    end do

    !// Combine polar pie pieces
    if (verbose) call gotData(FLD_ID,'2di','10m U-wind (SFU)')
    if (verbose) call gotData(FLD_ID,'2di','10m V-wind (SFV)')


    !// 2m Temperature T2M -------------------------------------------------
    !// --------------------------------------------------------------------
    !// Unit: K
    call readnc_2d_from3d(FNAME3, 'lon',IPARW,'lat',JPARWNE1, &
         'time',NMET,'TREFHT',inXY,silent=silent)

    !// Combine polar pie pieces
    call combinePPP(inXY(:,:),W2D(:,:),JPARWNE1,JPARW)
    call TRUNG8(W2D, SFT, ZDEGI, ZDEGJ, IMAP, JMAP, IDGRD, &
         JDGRD, IPARW, JPARW, IPAR, JPAR, 1, 1)
    if (minval(SFT) .eq. 0._r8) then
       write(6,'(a)')  f90file//':'//subr//': SFT zero: VERY WRONG!'
       stop
    end if
    if (verbose) call gotData(FLD_ID,'SFC','2m Temperature (SFT)')


    !// 2m specific humidity (SFQ) -----------------------------------------
    !// --------------------------------------------------------------------
    !// Unit: kg/kg
    call readnc_2d_from3d(FNAME3, 'lon',IPARW,'lat',JPARWNE1, &
         'time',NMET,'QREFHT',inXY,silent=silent)

    !// Combine polar pie pieces
    call combinePPP(inXY(:,:),W2D(:,:),JPARWNE1,JPARW)
    call TRUNG8(W2D, SFQ, ZDEGI, ZDEGJ, IMAP, JMAP, IDGRD, &
         JDGRD, IPARW, JPARW, IPAR, JPAR, 1, 1)

    if (verbose) call gotData(FLD_ID,'2di','2m specific humidity (SFQ)')


    !// E/W Surface Stress EWSS (accumulated) ------------------------------
    !// --------------------------------------------------------------------
    !// Unit: N/m2
    call readnc_2d_from3d(FNAME3, 'lon',IPARW,'lat',JPARWNE1, &
         'time',NMET,'TAUX',inXY,silent=silent)

    !// Combine polar pie pieces
    call combinePPP(inXY(:,:),W2D(:,:),JPARWNE1,JPARW)
    call TRUNG8(W2D, EWSS, ZDEGI, ZDEGJ, IMAP, JMAP, IDGRD, &
         JDGRD, IPARW, JPARW, IPAR, JPAR, 1, 1)
    if (verbose) call gotData(FLD_ID,'2da','E/W sfc stress (EWSS)')


    !// N/S Surface Stress NSSS (accumulated) ------------------------------
    !// --------------------------------------------------------------------
    !// Unit: N/m2
    call readnc_2d_from3d(FNAME3, 'lon',IPARW,'lat',JPARWNE1, &
         'time',NMET,'TAUY',inXY,silent=silent)

    !// Combine polar pie pieces
    call combinePPP(inXY(:,:),W2D(:,:),JPARWNE1,JPARW)
    call TRUNG8(W2D, NSSS, ZDEGI, ZDEGJ, IMAP, JMAP, IDGRD, &
         JDGRD, IPARW, JPARW, IPAR, JPAR, 1, 1)
    if (verbose) call gotData(FLD_ID,'2da','N/S sfc stress (NSSS)')

    !// Friction velocity --------------------------------------------------
    !// --------------------------------------------------------------------
    call readnc_2d_from3d(FNAME3, 'lon',IPARW,'lat',JPARWNE1, &
         'time',NMET,'USTAR',inXY,silent=silent)

    !// Combine polar pie pieces
    call combinePPP(inXY(:,:),W2D(:,:),JPARWNE1,JPARW)
    call TRUNG8(W2D, USTR, ZDEGI, ZDEGJ, IMAP, JMAP, IDGRD, &
         JDGRD, IPARW, JPARW, IPAR, JPAR, 1, 1)
    if (verbose) call gotData(FLD_ID,'2di','Surface friction velocity (USTR)')


    !// Forecast Albedo ----------------------------------------------------
    !// --------------------------------------------------------------------
    !// Unit: fraction
    call readnc_2d_from3d(FNAME3, 'lon',IPARW,'lat',JPARWNE1, &
         'time',NMET,'ASDIR',inXY,silent=silent)

    !// Combine polar pie pieces
    call combinePPP(inXY(:,:),W2D(:,:),JPARWNE1,JPARW)
    call TRUNG8(W2D, SA, ZDEGI, ZDEGJ, IMAP, JMAP, IDGRD, &
         JDGRD, IPARW, JPARW, IPAR, JPAR, 1, 1)
    if (verbose) call gotData(FLD_ID,'2di','Forecast Albedo (SA)')


    !// --------------------------------------------------------------------
    !// Read some additional fields for DUST module
    !// --------------------------------------------------------------------

    !// Surface Solar Radiation Downwards (SSRD) ---------------------------
    !// --------------------------------------------------------------------
    !// Unit: W/m2
    !// CAM5: FSDS
    if (LDUST) then
!       call get_netcdf_var_2d(FNAME, 'SSRD',W2D, IPARW, JPARW)
!       !// Limit to positive values just in case
!       W2D(:,:) = max(W2D(:,:), 0._r8)
!       call TRUNG8(W2D, R8XY, ZDEGI, ZDEGJ, IMAP, JMAP, IDGRD, &
!            JDGRD, IPARW, JPARW, IPAR, JPAR, 1, 1)
!       R8XY(:,:) = R8XY(:,:) * ZDT
!       call dust_set_ssrd(R8XY)
!       if (verbose) call gotData(FLD_ID,'2da','Surface Solar Radiation Downwards (SSRD)')
!    else
       !// Surface Solar Radiation Downwards (SSRD) (accumulated)
       if (verbose) call skipData(FLD_ID,'SFC','Surface Solar Radiation Downwards (SSRD)')
    end if

    !// Surface Thermal Radiation Downwards (STRD) ---------------------------
    !// --------------------------------------------------------------------
    !// Unit: W/m2
    !// CAM5: FLDS
    if (LDUST) then
!       call get_netcdf_var_2d(FNAME, 'STRD',W2D, IPARW, JPARW)
!       !// Limit to positive values just in case
!       W2D(:,:) = max(W2D(:,:), 0._r8)
!       call TRUNG8(W2D, R8XY, ZDEGI, ZDEGJ, IMAP, JMAP, IDGRD, &
!            JDGRD, IPARW, JPARW, IPAR, JPAR, 1, 1)
!       R8XY(:,:) = R8XY(:,:) * ZDT
!       call dust_set_strd(R8XY)
!       if (verbose) call gotData(FLD_ID,'2da','Surface Thermal Radiation Downwards (SSRD)')
!    else
       !// Surface Thermal Radiation Downwards (STRD) (accumulated)
       if (verbose) call skipData(FLD_ID,'SFC','Surface Thermal Radiation Downwards (STRD)')
    end if

    !// Volumetric soil water layer 1 (SWVL1) ------------------------------
    !// --------------------------------------------------------------------
    !// Unit: m3/m3
    !// NorESM: not available
   if (LDUST) then
!       call get_netcdf_var_2d(FNAME, 'SWVL1',W2D, IPARW, JPARW)
!       !// Limit to positive values just in case
!       W2D(:,:) = max(W2D(:,:), 0._r8)
!       call TRUNG8(W2D, R8XY, ZDEGI, ZDEGJ, IMAP, JMAP, IDGRD, &
!            JDGRD, IPARW, JPARW, IPAR, JPAR, 1, 1)
!       call dust_set_SWVL1(R8XY)
!       if (verbose) call gotData(FLD_ID,'2di','Volumetric soil water layer 1 (SWVL1)')
!    else
!       !// Volumetric soil water layer 1 (SWVL1)
       if (verbose) call skipData(FLD_ID,'SFC','Volumetric soil water layer 1 (SWVL1)')
    end if




    !// Other fields which may or may not be available ---------------------
    !// --------------------------------------------------------------------
    !// Snow Albedo (ASN)
    if (verbose) call skipData(FLD_ID,'SFC','Snow Albedo (ASN)')
    !// Snow Density (RSN)
    if (verbose) call skipData(FLD_ID,'SFC','Snow Density (RSN)')
    !// Sea Surface Temperature-Kelvin (SSTK)
    if (verbose) call skipData(FLD_ID,'SFC','SSTK')
    !// Ice surface temperature Layer 1 (ISTL1)
    if (verbose) call skipData(FLD_ID,'SFC','ISTL1')
    !// Ice surface temperature Layer 2 (ISTL2) 
    if (verbose) call skipData(FLD_ID,'SFC','ISTL2')
    !// Ice surface temperature Layer 3 (ISTL3) 
    if (verbose) call skipData(FLD_ID,'SFC','ISTL3')
    !// Ice surface temperature Layer 4 (ISTL4) 
    if (verbose) call skipData(FLD_ID,'SFC','ISTL4')
    !// Volumetric soil water layer 2 (SWVL2)
    if (verbose) call skipData(FLD_ID,'SFC','SWVL2')
    !// Volumetric soil water layer 3 (SWVL3)
    if (verbose) call skipData(FLD_ID,'SFC','SWVL3')
    !// Volumetric soil water layer 4 (SWVL4)
    if (verbose) call skipData(FLD_ID,'SFC','SWVL4')
    !// 10m wind gust
    if (verbose) call skipData(FLD_ID,'SFC','10m wind gust')
    !// Large-scale precipitation fraction (accumulated)
    if (verbose) call skipData(FLD_ID,'SFC','Large-scale prec. frac.')
    !// Downward UV radiation at the surface
    if (verbose) call skipData(FLD_ID,'SFC','Downw. UV rad. sfc')
    !// Convective available potential energy
    if (verbose) call skipData(FLD_ID,'SFC','CAPE')
    !// Total column liquid water
    if (verbose) call skipData(FLD_ID,'SFC','Tot. col. liq. water')
    !// Total column ice water
    if (verbose) call skipData(FLD_ID,'SFC','Tot. col. ice water')
    !// Surface Geopotential (Z)
    if (verbose) call skipData(FLD_ID,'SFC','Sfc. geopot. (Z)')
    !// Total Column Water (TCW)
    if (verbose) call skipData(FLD_ID,'SFC','Tot. col. water')
    !// Total Column Water Vapor (TCWV)
    if (verbose) call skipData(FLD_ID,'SFC','Tot. col. water vap.')
    !// Soil Temperature Level 1 (STL1)
    if (verbose) call skipData(FLD_ID,'SFC','Soil temp. lev. 1')
    !// Surface stress (eller Charnock)
    if (verbose) call skipData(FLD_ID,'SFC','Sfc stress/Charnock')
    !// Boundary Layer Dissipation (BLD) (accumulated)
    if (verbose) call skipData(FLD_ID,'SFC','Boundary Layer Dissipation')
    !// Total Cloud Cover
    if (verbose) call skipData(FLD_ID,'SFC','Tot. cloud cover')
    !// Soil Temperature Level 2 (STL2)
    if (verbose) call skipData(FLD_ID,'SFC','Soil temp. lev. 2')
    !// Land/Sea mask (LSM), NOTE: {0,1}
    !// Not so useful; it is 1 if land fraction > 0.5.
    if (verbose) call skipData(FLD_ID,'SFC','Land/sea mask')
    !// Surface Thermal Radiation Downwards (STRD) (accumulated)
    if (verbose) call skipData(FLD_ID,'SFC','Sfc. thermal rad. downwards')
    !// Surface Solar Radiation (SSR) (accumulated)
    if (verbose) call skipData(FLD_ID,'SFC','Sfc. solar rad.')
    !// Surface Thermal Radiation (STR) (accumulated)
    if (verbose) call skipData(FLD_ID,'SFC','Sfc. thermal rad.')
    !// Top Solar Radiation (TSR) (accumulated)
    if (verbose) call skipData(FLD_ID,'SFC','Top Solar Radiation (TSR)')
    !// Top Thermal Radiation (TTR) (accumulated)
    if (verbose) call skipData(FLD_ID,'SFC','Top Thermal Radiation (TTR)')
    !// Evaporation (E) (accumulated)
    if (verbose) call skipData(FLD_ID,'SFC','Evaporation')
    !// Soil Temperature Level 3 (STL3)
    if (verbose) call skipData(FLD_ID,'SFC','Soil temp. lev. 3')
    !// Soil Wetness Level 3 (SWL3)
    if (verbose) call skipData(FLD_ID,'SFC','Soil wetness level 3')
    !// Convective Cloud Cover (CCC)
    if (verbose) call skipData(FLD_ID,'SFC','Convective cloud cover')
    !// Low Cloud Cover (LCC)
    if (verbose) call skipData(FLD_ID,'SFC','Low cloud cover')
    !// Medium Cloud Cover (MCC)
    if (verbose) call skipData(FLD_ID,'SFC','Medium cloud cover')
    !// High Cloud Cover (HCC)
    if (verbose) call skipData(FLD_ID,'SFC','High cloud cover')
    !// Sun Shine Duration (SUND) (accumulated)
    if (verbose) call skipData(FLD_ID,'SFC','Sunshine duration')
    !// Latitudinal Gravity Wave Stress (LGWS) (accumulated)
    if (verbose) call skipData(FLD_ID,'SFC','LGWS')
    !// Meridional Gravity Wave Stress (MGWS)
    if (verbose) call skipData(FLD_ID,'SFC','MGWS')
    !// Gravity Wave Dissipation (GWD)
    if (verbose) call skipData(FLD_ID,'SFC','GWD')
    !// Skin Reservoir Content (SRC)
    if (verbose) call skipData(FLD_ID,'SFC','Skin Reservoir Content (SRC)')
    !// Maximum Temperature at 2m (MX2T)
    if (verbose) call skipData(FLD_ID,'SFC','MX2T')
    !// Minimum Temperature at 2m (MN2T)
    if (verbose) call skipData(FLD_ID,'SFC','MN2T')
    !// RunOff (RO)
    if (verbose) call skipData(FLD_ID,'SFC','RunOff')
    !// Top Net Solar Radiation Clear Sky (TSRC) (accumulated)
    if (verbose) call skipData(FLD_ID,'SFC','TSRC')
    !// Top Net Thermal Radiation Clear Sky (TTRC) (accumulated)
    if (verbose) call skipData(FLD_ID,'SFC','TTRC')
    !// Surface net solar radiation, clear sky (accumulated)
    if (verbose) call skipData(FLD_ID,'SFC','SNSRCS')
    !// Surface net thermal radiation, clear sky (accumulated)
    if (verbose) call skipData(FLD_ID,'SFC','SNTRCS')
    !// Log Surface roughness for Heat
    if (verbose) call skipData(FLD_ID,'SFC','Log(sfc. roughness for heat)')
    !// Skin Temperature (SKT)
    if (verbose) call skipData(FLD_ID,'SFC','Skin Temperature (SKT)')
    !// Soil Temperature Level 4 (STL4)
    if (verbose) call skipData(FLD_ID,'SFC','Soil temp. lev. 4')
    !// Temperature of Snow Layer (TSN)
    if (verbose) call skipData(FLD_ID,'SFC','Temperature of snow layer')
    !// Forecast Surface Roughness (FSR) 
    if (verbose) call skipData(FLD_ID,'SFC','Forecast sfc. roughness')
    !// Forecast Log(Surface Roughness Heat)
    if (verbose) call skipData(FLD_ID,'SFC','Forecast log(Sfc. roughness heat)')





    !// Additional calculations
    !//---------------------------------------------------------------------

    !// USTAR from Surface Stress NSSS and EWSS
    do J = 1, JPAR
      do I = 1, IPAR
        SMF(I,J) = sqrt(NSSS(I,J)*NSSS(I,J) + EWSS(I,J)*EWSS(I,J))
        !// ideal gas density [kg/m3]
        SDEN      = M_AIR * 100._r8*P(I,J) / (R_UNIV * 1.e3_r8 * SFT(I,J))
        USTR(I,J) = sqrt(SMF(I,J)/SDEN)
        if (USTR(I,J) .eq. 0._r8) then
          !// When zero, override with minimum value
          USTR(I,J) = max(5.e-3_r8, USTR(I,J))
          !// Also override SMF
          SMF(I,J) = (USTR(I,J)**2) * SDEN
        end if
      end do
    end do

    !// Filter fluxes for wrong signs etc.
    !call fluxfilter2(cdetu, cdetd)

    !// Put low level limits to CLDFR, CLDLWC, and CLDIWC, and set junk
    !// values to zero.
    do L = 1, LWEPAR
      do J = 1, JPAR
        do I = 1, IPAR
          if (CLDFR(I,J,L) .gt. EPS) then
            CLDLWC(I,J,L) = CLDLWC(I,J,L) / CLDFR(I,J,L)
            CLDIWC(I,J,L) = CLDIWC(I,J,L) / CLDFR(I,J,L)

            call CIWMIN(T(I,J,L), CLDLWC(I,J,L), CLDIWC(I,J,L))

            CLDLWC(I,J,L) = CLDLWC(I,J,L) * CLDFR(I,J,L)
            CLDIWC(I,J,L) = CLDIWC(I,J,L) * CLDFR(I,J,L)
          else
            CLDFR(I,J,L) = 0._r8
            call CFRMIN(T(I,J,L), CLDFR(I,J,L), CLDLWC(I,J,L), &
                        CLDIWC(I,J,L), EPS)
          end if
        end do
      end do
    end do

    !// Done reading all meteorological data
    deallocate( VTMP, UTMP, W3Da, W3Db, ZOFLEW, QW, TW, R8XYZ, &
         CLDFRW, CLDIWCW, CLDLWCW, &
         PW, LSPREC, CNVPREC, EWSS, NSSS, W2D, R8XY, &
         inXYZ, inXYm1Z, inXYZp1, inXY, inXY_b )

    !//---------------------------------------------------------------------
  end subroutine update_metdata
  !//-----------------------------------------------------------------------



  !//-----------------------------------------------------------------------
  subroutine fluxfilter2(cdetu,cdetd)
    !//---------------------------------------------------------------------
    !// Filter convective mass fluxes.
    !// Based on filtering in p-wind_ec.f.
    !//
    !// Ole Amund Sovde, March 2015, March 2010
    !//---------------------------------------------------------------------
    use cmn_precision, only: r8
    use cmn_size, only: IPAR, JPAR, LWEPAR, LWDPAR
    use cmn_met, only: CENTU, CENTD, CWETE, CWETD
    !//---------------------------------------------------------------------
    !// Input
    real(r8), intent(inout) :: cdetu(ipar,jpar,lwepar)
    real(r8), intent(inout) :: cdetd(ipar,jpar,lwdpar)

    !// Local variables
    real(r8) :: sden
    integer :: KCTOP(ipar,jpar), KDBOT(ipar,jpar), i,j,l,LL
    !//---------------------------------------------------------------------

    !// CWETE and CWETD are already filtered for wrong signs.
    !// There may still be some places where a grid box have
    !// flux, but there is none above or below. This is a bit strange,
    !// but fairly ok; such a small convective flux will then be
    !// detrained above. No need to filter these away.

    !// CDETU and CDETD are also already filtered for wrong signs.


    !// More CDETU and CDETD filters

    !// Find top of convection
    KCTOP(:,:) = 1
    do J = 1, JPAR
       do I = 1, IPAR
          do L = LWEPAR-1,1,-1
             !// Loop downwards
             if (CWETE(i,j,l) .gt. 0._r8) then
                KCTOP(i,j) = L
                exit
             else
                !// No flux; no detrainment
                cdetu(i,j,l) = 0._r8
             end if
          end do
       end do
    end do

    !// Find bottom of downdrafts
    KDBOT(:,:) = LWDPAR-1
    do J = 1,JPAR
       do I = 1, IPAR
          do L = 1, LWDPAR-1
             !// Loop downwards
             if (cwetd(i,j,l+1) .lt. 0._r8) then
                !// Mass comes in at top
                KDBOT(i,j) = L
                exit
             else
                !// No flux in at top; no detrainment
                cdetd(i,j,l) = 0._r8
             end if
          end do
       end do
    end do

    !// Detrainment cannot be larger than flux in at bottom
    do J = 1,JPAR
       do I = 1, IPAR
          do l = 1, KCTOP(I,J)
             !// Loop downwards; cdetu and cwete are non-negative
             !// due to filtering above
             if (cdetu(i,j,l) .gt. cwete(i,j,l)) then
                !// CDETU cannot be larger than the flux in!
                if (cwete(i,j,l) .eq. 0._r8) then
                   CDETU(I,J,L) = 0._r8
                else
                   CDETU(i,j,l) = CWETE(i,j,l)
                end if
             end if
          end do
       end do
    end do

    !// Detrainment cannot be larger than flux in at top
    do J = 1,JPAR
       do I = 1, IPAR
          !// Make sure top is zero
          CDETD(i,j,LWDPAR) = 0._r8
          do L = LWDPAR-1, KDBOT(I,J), -1
             !// Loop downwards; cdetd is non-negative and cwetd is
             !// non-positive due to filtering above
             if (CDETD(i,j,l) .gt. -CWETD(i,j,l+1)) then
                if (CWETD(i,j,l+1) .lt. 0._r8) then
                   CDETD(i,j,l) = -CWETD(i,j,l+1)
                else
                   CDETD(i,j,l) = 0._r8
                end if
             end if
          end do
       end do
    end do

    !// FINALLY
    !// Build entrainment - updrafts (detrainment is positive)
    do J = 1,JPAR
       do I = 1, IPAR
          do L = 1, KCTOP(I,J)
             !// F(L) + E = F(L+1) + D
             sden = cwete(i,j,l+1) - cwete(i,j,l) + cdetu(i,j,l)
             !// Only allow positive entrainment
             centu(i,j,l) = max(0._r8, sden)
          end do
       end do
    end do
    !// Build entrainment - downdrafts (flux is negative)
    do L = 1, LWDPAR-1
       do J = 1, JPAR
          do I = 1, IPAR
             !// -F(L+1) + E = -F(L) + D
             sden = cwetd(i,j,l+1) - cwetd(i,j,l) + cdetd(i,j,l)
             !// Only allow positive entrainment
             centd(i,j,l) = max(0._r8, sden)
          end do
       end do
    end do

    !//---------------------------------------------------------------------
  end subroutine fluxfilter2
  !//-----------------------------------------------------------------------


  !//-----------------------------------------------------------------------
  subroutine fluxfilter_ENT()
    !//---------------------------------------------------------------------
    !// Filter convective mass fluxes.
    !// Based on filtering in p-wind_ec.f.
    !//
    !// Amund Sovde, Novemeber 2014
    !//---------------------------------------------------------------------
    use cmn_precision, only: r8
    use cmn_size, only: IPAR, JPAR, LWEPAR, LWDPAR
    use cmn_met, only: CENTU, CENTD, CWETE, CWETD
    use cmn_ctm, only: AREAXY
    !//---------------------------------------------------------------------
    implicit none
    !// Input: none

    !// Local variables
    real(r8) :: sden
    integer :: KCTOP(ipar,jpar), KDBOT(ipar,jpar), i,j,l,LL
    !//---------------------------------------------------------------------

    ! Remove noise and wrong sign convections
    ! Noise filter for CWETE
    !   the min cwete is -2.3e-5 kg/s/m^2, the neg fluxes are often seen as
    !  'dipoles' with an equally small positive updraft flux in a neighboring layer
    do l = 1, lwepar
       do j = 1,jpar
          do i = 1,ipar
             sden = cwete(i,j,l) / areaxy(i,j)
             if (sden .lt. 2.3e-5_r8) cwete(i,j,l) = 0._r8
          enddo
       enddo
    enddo
    !// Add more filter for boxes where the box above and below have no flux 
    do l = 2,lwepar-1
       do j = 1,jpar
          do i = 1,ipar
             if (cwete(i,j,l-1).eq.0._r8 .and. cwete(i,j,l).gt.0._r8 &
                  .and.cwete(i,j,l+1).eq.0._r8) then
                !// Some boxes have flux, but no flux below. Filter these
                !// if smaller than 2.d-4
                sden = CWETE(i,j,l) / areaxy(i,j)
                if (sden .lt. 2.e-4_r8) cwete(i,j,l) = 0._r8
             endif
          enddo
       enddo
    enddo

    ! Noise filter for CWETD
    !  the max cwetd is 7.1e-6 kg/s/m^2, the + fluxes are often seen as 'dipoles'
    !  with an equally small negative downdraft flux in a neighboring layer
    do l = 1,lwdpar
       do j = 1,jpar
          do i = 1,ipar
             sden = cwetd(i,j,l) / areaxy(i,j)
             if (sden .gt. -7.1e-6_r8) cwetd(i,j,l) = 0._r8
          enddo
       enddo
    enddo
    !// Add more filter for boxes where the box above and below have no flux 
    do l = 2,lwdpar-1
       do j = 1,jpar
          do i = 1,ipar
             if (cwetd(i,j,l-1).eq.0._r8 .and. cwetd(i,j,l).lt.0._r8 &
                  .and.cwetd(i,j,l+1).eq.0._r8) then
                sden = cwetd(i,j,l) / areaxy(i,j)
                if (sden .gt. -1.d-5) cwetd(i,j,l) = 0._r8
             endif
          enddo
       enddo
    enddo

    !Noise filter for CENTU
    do l = 1,lwepar
       do j = 1,jpar
          do i = 1,ipar
             sden = centu(i,j,l) / areaxy(i,j)
             if (sden .lt. 2.3e-5_r8) centu(i,j,l) = 0._r8
          enddo
       enddo
    enddo

    !Noise filter for CENTD
    do l = 1,lwdpar
       do j = 1,jpar
          do i = 1,ipar
             sden = centd(i,j,l) / areaxy(i,j)
             if (sden .lt. 7.1e-6_r8) centd(i,j,l) = 0._r8
          enddo
       enddo
    enddo

    !more filter for updrafts/downdrafts

    !// Find top of convection
    KCTOP(:,:) = 1
    do j = 1,jpar
       do i = 1,ipar
          do l = lwepar-1,1,-1
             !// Loop downwards
             if (cwete(i,j,l).gt. 0._r8) then
                KCTOP(i,j) = L
                exit
             else
                !// No flux; no entrainment
                centu(i,j,l) = 0._r8
             endif
          enddo
       enddo
    enddo

    !// Find bottom of downdrafts
    KDBOT(:,:) = LWDPAR-1
    do j = 1,jpar
       do i = 1,ipar
          do l = 1,lwdpar-1
             !// Loop downwards
             if (cwetd(i,j,l+1).lt. 0._r8) then
                !// Mass comes in at top
                KDBOT(i,j) = L
                exit
             else
                !// No flux in at top; no entrainment
                centd(i,j,l) = 0._r8
             endif
          enddo
       enddo
    enddo

    !//---------------------------------------------------------------------
  end subroutine fluxfilter_ENT
  !//-----------------------------------------------------------------------




  !//-----------------------------------------------------------------------
  subroutine data2mpblocks(r8data, mpdata)
    !//---------------------------------------------------------------------
    !// Puts real*8 data of size (ipar,jpar) into real*8 MP-block structure
    !// mpdata(idblk,jdblk,mpblk).
    !//
    !// Ole Amund Sovde, April 2013
    !//---------------------------------------------------------------------
    use cmn_precision, only: r8
    use cmn_size, only: IPAR, JPAR, IDBLK, JDBLK, MPBLK
    use cmn_ctm, only: MPBLKIB, MPBLKIE, MPBLKJB, MPBLKJE
    !//---------------------------------------------------------------------
    implicit none
    !//---------------------------------------------------------------------
    !// Input
    real(r8), intent(in) :: r8data(ipar,jpar)
    !// Output
    real(r8), intent(out) :: mpdata(idblk,jdblk,mpblk)
    !// Locals
    integer :: I, J, II, JJ, MP
    !//---------------------------------------------------------------------
    do MP = 1, MPBLK
      !// Loop over latitude (J is global, JJ is block)
      do J = MPBLKJB(MP),MPBLKJE(MP)
        JJ    = J - MPBLKJB(MP) + 1
        !// Loop over longitude (I is global, II is block)
        do I = MPBLKIB(MP),MPBLKIE(MP)
          II    = I - MPBLKIB(MP) + 1
          !// Change structure
          mpdata(II,JJ,MP) = r8data(I,J)
        end do
      end do
    end do
    !//---------------------------------------------------------------------
  end subroutine data2mpblocks
  !//-----------------------------------------------------------------------



  !//-----------------------------------------------------------------------
  subroutine gotData(ID,TYP,LABEL)
    !//---------------------------------------------------------------------
    !// Print info about 2D field read.
    !//---------------------------------------------------------------------
    implicit none
    !//---------------------------------------------------------------------
    integer, intent(in) :: ID
    character(len=3) :: TYP
    character(len=*) :: LABEL
    !//---------------------------------------------------------------------
    write(6,'(a,i5,1x,a)') ' update_metdata: Read '//TYP//':   ', &
         ID, trim(LABEL)
    !//---------------------------------------------------------------------
  end subroutine gotData
  !//-----------------------------------------------------------------------

  !//-----------------------------------------------------------------------
  subroutine skipData(ID,TYP,LABEL)
    !//---------------------------------------------------------------------
    !// Print info about 2D field skipped.
    !//---------------------------------------------------------------------
    implicit none
    !//---------------------------------------------------------------------
    integer, intent(in) :: ID
    character(len=3) :: TYP
    character(len=*) :: LABEL
    !//---------------------------------------------------------------------
    write(6,'(a,i5,1x,a)') ' update_metdata: Skipped '//TYP//':', &
         ID,trim(LABEL)
    !//---------------------------------------------------------------------
  end subroutine skipData
  !//-----------------------------------------------------------------------



  !//-----------------------------------------------------------------------
  subroutine combinePPP(inXY,wkXY,Jin,Jout)
    !//---------------------------------------------------------------------
    !// Combines the NorESM polar pie piece boxes with their Equator-
    !// ward neighbor boxes.
    !// Amund Sovde, November 2014
    !//---------------------------------------------------------------------
    use cmn_size, only: IPARW
    use cmn_precision, only: r8, r4
    use cmn_ctm, only: AREAXY
    !//---------------------------------------------------------------------
    implicit none
    !//---------------------------------------------------------------------
    !// Input
    integer, intent(in) :: Jout,Jin !// Jin should be Jout+2
    real(r8), intent(in)  :: inXY(IPARW,Jin)
    !// Output
    real(r8), intent(out) :: wkXY(IPARW,Jout)
    !// Locals
    integer :: I,J
    real(r8) :: APP, APN
    !//---------------------------------------------------------------------

    !// Will use area as weighting function (equal in EW-direction)
    APP = areaxy(1,1) !// Pole (same for NH as SH)
    APN = areaxy(1,2) !// Neighbor equatorwards (same for NH as SH)

    !// SH polar pie box
    do I = 1, IPARW
       wkXY(I,1) = (APP*inXY(I,1) + APN*inXY(I,2)) /(APP+APN)
    end do
    !// NH polar pie box
    do I=1,IPARW
       wkXY(I,Jout) = (APP*inXY(I,Jin) + APN*inXY(I,Jin-1)) / (APP+APN)
    end do
    !// The rest of the boxes
    do J = 2, Jout-1
       do I = 1, IPARW
          wkXY(I,J) = inXY(I,J+1)
       end do
    end do
    !//---------------------------------------------------------------------
  end subroutine combinePPP
  !//-----------------------------------------------------------------------



  !//-----------------------------------------------------------------------
end module metdata_noresm
!//=========================================================================
