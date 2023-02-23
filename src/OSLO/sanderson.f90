!//=========================================================================
!// Oslo CTM3
!//=========================================================================
!// Ragnhild Bieltvedt Skeie, January 2021
!//=========================================================================
!// Routines for the H2 deposition. Based on OsloCTM2 module used in the 
!// EUROHYDRO project (2006-2009) and created by Amund Sovde, September 2009
!//=========================================================================
module sanderson
  !//-----------------------------------------------------------------------
  !// MODULE: sanderson
  !// DESCRIPTION: Routines for H2 deoisition
  !// ----------------------------------------------------------------------
  !// Oslo CTM3 - Module for H2 deposition
  !// Contains H2 deposition routines routines.
  !//   - subroutine h2dep_sanderson2003   (NOT READY)
  !//   - subroutine h2dep_price2006 (NOT READY)
  !//   - subroutine h2dep_eh2009 
  !//   - subroutine h2drydep_init
  !//   - subroutine h2drydep 
  !//   - update_h2_soiluptake (Choose which routine to be used here)
  !//   - subroutine get_cover
  !// Ragnhild Bieltvedt Skeie, January 2021
  !// ----------------------------------------------------------------------
  use cmn_size, only: IPAR, JPAR, MPBLK
  use cmn_precision, only: r8
  use cmn_ctm, only: PLAND  !land fraction (m2/m2)
  !// ----------------------------------------------------------------------
  implicit none
  !// ----------------------------------------------------------------------

  !// Variables
  real(r8), dimension(6,IPAR,JPAR) :: RH2
  real(r8), dimension(IPAR,JPAR,7)  :: landcover   ! get_cover()
  real(r8), dimension(IPAR,JPAR)  :: landice ! get_cover()
  integer, parameter :: H2dep = 3 !//See subroutine h2drydep_init
  !// 1: H2 drydeposition routine: h2dep_eh2009
  !// 2: H2 drydeposition routine: h2dep_price2006
  !// 3: H2 drydeposition routine: h2dep_sanderson2003

  !// ----------------------------------------------------------------------
  character(len=*), parameter, private :: f90file = 'sanderson.f90'
  !// ----------------------------------------------------------------------

  save !// All variables are to be saved.
  private
  public h2dep_sanderson2003,h2dep_price2006,h2dep_eh2009, &
       h2drydep_init,update_h2_soiluptake, h2drydep,get_cover
  !// ----------------------------------------------------------------------
contains
  !// ----------------------------------------------------------------------

  !// ----------------------------------------------------------------------
  subroutine h2dep_sanderson2003() !(MP)
    !// --------------------------------------------------------------------
    !// Calculate H2 deposition according to Sanderson (2003):
    !// Simulation of Global Hydrogen Levels Using a Langrangian 
    !// Three-Dimensional Model, J. Atm. Chem. 46, 15-28, 2003
    !// --------------------------------------------------------------------
    use cmn_met, only: SFT, SD, SWVL1  !// Volumetric soil water layer 1 (SWVL1)        
    use cmn_ctm, only: YDGRD, JDAY,MPBLKIB, MPBLKIE, MPBLKJB, MPBLKJE
    !use cmn_sfc, only: landSurfTypeFrac
  
    !// --------------------------------------------------------------------
    implicit none
    !// --------------------------------------------------------------------
 
!    integer, intent(in) :: MP

    character(len=*), parameter :: subr='h2dep_sanderson2003'
    !// Scale to modify the drydepostion
    !//real(r8), parameter :: scalefact= 2.5
    real(r8), parameter :: scalefact= 1.76

    !// --------------------------------------------------------------------

    !// xx = soil moisture content (volume water per unit volume of soil)
    !// zz = ln(xx)
    !//
    !// Velocities calculated are given in [10^-4m/s] and must be multiplied with
    !// 1.d-2 to get [cm/s].
    !// --------------------------------------------------------------------

    !// parameters for calculating velocities
    real(r8), parameter :: &
         ! Savannah:
         !   v_sav = sav_a*zz^2 + sav_b*zz + sav_c
         sav_a =  0.270,   &
         sav_b = -0.472,   &
         sav_c = 1.235,    &
         ! Agricultural land:
         !   v_agr = agr_b*xx + arg_c
         agr_b = -41.39,   &
         agr_c = 16.85,    &
         ! Forest:
         !   v_for = for_b*xx + for_c
         for_b = -41.9,    &
         for_c =  19.7,    &
         ! Grasslands:
         !   v_gras = gras_b*xx + gras_c
         gras_b = -41.39,   &
         gras_c = 17.7
    
    !// Local variables
    integer :: i,j
    real(r8) :: xx, zz, &
         v_sav,      &
         v_agr,      &
         v_for,      &
         v_gras,     &
         v_tun,     &
         v_sem
    !// -------------------------------------------------------------------- 
    RH2(:,:,:) = 0.d0

    !// now calculate the deposition rate
    do j=1,JPAR
       do i=1,IPAR
    !do j = MPBLKJB(MP),MPBLKJE(MP)
    !   !JJ    = J - MPBLKJB(MP) + 1
    !   !// Loop over longitude (I is global, II is block)
    !   do i = MPBLKIB(MP),MPBLKIE(MP)
          !// Only if land!!!
          xx = SWVL1(i,j)
          if (PLAND(i,j) .gt. 0. .and. xx .gt. 0.) then
             zz = log(xx)
             !// Check none-forest snow cover
             if (SD(i,j) .lt. 0.004) then
                v_sav  = sav_a*zz**2.d0 + sav_b*zz + sav_c
                v_agr  = agr_b*xx + agr_c
                v_gras = gras_b*xx + gras_c
                v_tun  = 2.9 ! constants
                v_sem  = 1.0 ! constants
             else
                !// 4 cm snow covers none-forest
                v_sav = 0.
                v_gras= 0.
                v_agr = 0.
                v_tun = 0.
                v_sem = 0.
             endif
             !// Check forest snow cover
             if (SD(i,j) .lt. 0.03) then
                !// asusme not covered
                v_for  = for_b*xx + for_c
             else
                !// forest ground is snow covered
                v_for = 0.
             endif
             !// need to weigh the values against land cover.
             !// Units are [10^-4 m/s]
             if(v_sav  .gt.  0._r8) RH2(1,i,j) = scalefact * v_sav  * landcover(i,j,1)
             if(v_agr  .gt.  0._r8) RH2(2,i,j) = scalefact * v_agr  * landcover(i,j,2)
             if(v_for  .gt.  0._r8) RH2(3,i,j) = scalefact * v_for  * landcover(i,j,3)
             if(v_gras .gt.  0._r8) RH2(4,i,j) = scalefact * v_gras * landcover(i,j,4)
             if(v_tun  .gt.  0._r8) RH2(5,i,j) = scalefact * v_tun  * landcover(i,j,5)
             if(v_sem  .gt.  0._r8) RH2(6,i,j) = scalefact * v_sem  * landcover(i,j,6)
             !// reduce rate according to H. Price (2006) when cold surface
             if (SFT(i,j).lt.273.15 .and.SFT(i,j).gt.258.15) RH2(:,i,j)=RH2(:,i,j)*0.5
             if (SFT(i,j).le.258.15) RH2(:,i,j)=RH2(:,i,j)*0.25
          endif
       enddo
    enddo
    ! Convert from 10^-4m/s into cm/s Done above.
    RH2(:,:,:) = RH2(:,:,:)*1.d-2
    
    !print*,v_for,snowcover(3,25),PLAND(3,25),swvl1(3,25),for_b*swvl1(3,25) + for_c
    !do i=1,18
    !   print*,i,lcov(3,25,i)
    !enddo
    !// ----------------------------------------------------------------------    
end subroutine h2dep_sanderson2003


!// ----------------------------------------------------------------------
subroutine h2dep_price2006() !(MP)
  !// ----------------------------------------------------------------------
  !//Calculate H2 deposition according to Price (2006):
  !// ----------------------------------------------------------------------
  use cmn_met, only: SFT, SD
  !// ----------------------------------------------------------------------     
  implicit none
  !// ----------------------------------------------------------------------

  !//Local variables
!  real(r8), parameter :: h2dep_price=3.94d-2 ! given in cm/s
  !// Modified, multiply with 2
  real(r8), parameter :: h2dep_price=7.88d-2 ! given in cm/s

  integer :: i,j
  real(r8) :: &
       v_sav,      &
       v_agr,      &
       v_for,      &
       v_gras,     &
       v_tun,     &
       v_sem
!  integer, intent(in) :: MP
  !// ----------------------------------------------------------------------
  character(len=*), parameter :: subr='h2dep_price2006'
  !// ----------------------------------------------------------------------

  RH2(:,:,:) = 0.d0
  !// now calculate the deposition rate
  do j=1,JPAR
     do i=1,IPAR
        !// Only if land!!!
        if (PLAND(i,j) .gt. 0.) then ! .and. xx .gt. 0.) then
           !// Check none-forest snow cover (allow 4cm) (0.4cm water equivalent)
           if (SD(i,j) .lt. 0.004) then
              v_sav  = h2dep_price
              v_agr  = h2dep_price
              v_gras = h2dep_price
              v_tun  = h2dep_price
              v_sem  = h2dep_price
           else
              !// 4 cm snow covers none-forest
              v_sav = 0.
              v_gras= 0.
              v_agr = 0.
              v_tun = 0.
              v_sem = 0.
           endif
           !// Check forest snow cover (allow 30cm)
           !// SD is meter water equivalents, i.e. 0.01 is approx 0.1m snow,
           !// and 0.1 is approx 1m snow.
           if (SD(i,j) .lt. 0.03) then
              !// asusme not covered
              v_for  = h2dep_price
           else
              !// forest ground is snow covered
              v_for = 0.
           endif
           !// need to weigh the values against land cover.
           !// Units are [cm/s]
           RH2(1,i,j) = v_sav  * landcover(i,j,1)
           RH2(2,i,j) = v_agr  * landcover(i,j,2)
           RH2(3,i,j) = v_for  * landcover(i,j,3)
           RH2(4,i,j) = v_gras * landcover(i,j,4)
           RH2(5,i,j) = v_tun  * landcover(i,j,5)
           RH2(6,i,j) = v_sem  * landcover(i,j,6)

           !// reduce rate according to H. Price (2006) when cold surface
           if (SFT(i,j).lt.273.15 .and.SFT(i,j).gt.258.15) RH2(:,i,j)=RH2(:,i,j)*0.5
           if (SFT(i,j).le.258.15) RH2(:,i,j)=RH2(:,i,j)*0.25
        endif
     enddo
  enddo

  !//---------------------------------------------------------------------
end subroutine h2dep_price2006
!//-----------------------------------------------------------------------

!//-----------------------------------------------------------------------
subroutine h2dep_eh2009() !(MP)
  !//---------------------------------------------------------------------
  !// Calculate H2 deposition according to Eurohydros soil workshop 2009:
  !//---------------------------------------------------------------------
  use cmn_parameters, only: CPI
  use cmn_met, only: SFT, SD ! SD: Snow depth [m water equivalent]
  use cmn_ctm, only: YDGRD, JDAY,MPBLKIB, MPBLKIE, MPBLKJB, MPBLKJE
  
  !//---------------------------------------------------------------------
  implicit none
  !//---------------------------------------------------------------------

!  integer, intent(in) :: MP

  !// Local variables
  
  !real(r8), parameter :: h2dep_mean =3.52d-2 ! Eurohydros mean value [cm/s]
  !// RBS enhanced this value by 2:
  !real(r8), parameter :: h2dep_mean =7.04d-2 ! Eurohydros mean value [cm/s]
  !// RBS enhanced this value by 2.5:
  real(r8), parameter :: h2dep_mean =8.8d-2 ! Eurohydros mean value [cm/s]
  real(r8) :: h2dep_eh

  integer :: i,j
  real(r8) :: v_d, svar_sh, svar_nh, dsnw
  !//---------------------------------------------------------------------


  RH2(:,:,:) = 0.d0

  !// Seasonal variation: min/max at 1Feb / 1Jul (-31 days)
  !// Assume +- 2mm/s
  svar_nh = 2.d-2*sin((-0.5 + 2.d0*(JDAY-1.d0 - 31.d0)/365.d0)*CPI)
  svar_sh = 2.d-2*sin((0.5 + 2.d0*(JDAY-1.d0 - 31.d0)/365.d0)*CPI)
  !// Possible outcomes:
  !// - H2-loss too high in summer, but ok in winter
  !//   - Higher mean
  !//   - Use higher svar_nh for summer (e.g. 3/2*svar_nh for positive svar_nh)
  !// x H2-loss too high in winter
  !//   x Constrain with temperature ala Price
  !//   x Halve v_d for each 20cm
  !// x H2-loss too low in winter
  !//   - Lower svar_nh for winter (e.g. 0.75*svar_nh for negative svar_nh)
  !//     - Maybe only poleward of 60N
  !// x H2-loss too low in winter and summer
  !//   - Raise mean value
  !// x H2 is fine
  !//   - Hooray

  !// now calculate the deposition rate
  !// Loop over latitude (J is global, JJ is block)
 ! do J = MPBLKJB(MP),MPBLKJE(MP)
     !JJ    = J - MPBLKJB(MP) + 1
     !// Loop over longitude (I is global, II is block)
 !    do I = MPBLKIB(MP),MPBLKIE(MP)
        !II    = I - MPBLKIB(MP) + 1
  do j=1,JPAR
     do i=1,IPAR
        !// Only if land!!!
        if (PLAND(i,j) .gt. 0.) then
           
           !// Deposition depend on latitude; Check grid box center
           if (YDGRD(j) .gt. -30. .and. YDGRD(j) .lt. 30.) then
              !// Use mean between 30S and 30N
              v_d  = h2dep_mean
           else if (YDGRD(j) .ge. 30.) then
              !// Use Eurohydros cm/s Northern Hemisphere (top in summer)
              v_d = h2dep_mean + svar_nh
           else if (YDGRD(j) .le. -30.) then
              !// Use Eurohydros cm/s Southern Hemisphere (top in summer)
              v_d = h2dep_mean + svar_sh
           endif
           
           
           !// EH2: both T and snow reduce v_d
           !// EH3: snow reduce v_d
           !// EH4: As EH2 but skin temperature and landice correction.
           
           !// Reduce rate according to H. Price (2006) when cold surface
           !// Apply this north of 60N when snow cover is below 40cm (first test value)
           if (abs(YDGRD(j)) .gt. 60. .and. SD(i,j).lt.0.04) then
              if (SFT(i,j).lt.273.15 .and.SFT(i,j).gt.258.15) v_d = v_d * 0.5d0
              if (SFT(i,j).le.258.15) v_d = v_d * 0.25d0
           endif
           
           !// Check forest snow cover (halve it when more than 20cm)
           !// Assume 20cm snow equals 2cm water equivalent:
           !if (SD(i,j) .gt. 0.02) then
           !   v_d = v_d * 0.5d0
           !endif
           !// To halve for each 20cm
           dsnw = SD(i,j)
           do while (dsnw .gt. 0.02)
              v_d = v_d * 0.5d0
              dsnw = dsnw - 0.02
           enddo
           !// Linearly reducing v_d, halving each 20cm
           !// v_d = v_d / (2^(dsnw/0.02))
           !v_d = v_d * (2.d0**(-dsnw*50.d0))
           
           
           !// Scale with landtype fractions
           !// - v_d applies for all land types defined in 1-6, so we must treat
           !//   wetlands separately (10% of mean velocity).
           v_d = v_d * sum(landcover(i,j,1:6)) &
                + h2dep_mean * 0.1d0 * landcover(i,j,7)
           
           !// Check landice
           if (landice(i,j) .gt. 0. .and. SD(i,j) .le. 0.001) then
              !write(*,'(a,i3,i3,2(1x,f8.5),1x,es9.3)') 'Landice',j,i,landice(i,j),SD(i,j),v_d
              v_d = v_d * (1.d0 - landice(i,j))
           endif
           
           !// Lower limit for v_d
           if (v_d .lt. 1.d-12) v_d = 0.d0
           
           !// Units are [cm/s]
           RH2(1,i,j) = v_d
           
        endif
     enddo
  enddo
  
  !//---------------------------------------------------------------------
end subroutine h2dep_eh2009
!//-----------------------------------------------------------------------



!//-----------------------------------------------------------------------
subroutine h2drydep_init()
  !//---------------------------------------------------------------------
  !// Initialize H2 dry deposition 
  !// --------------------------------------------------------------------
  implicit none
  !// --------------------------------------------------------------------
  character(len=*), parameter :: subr = 'h2drydep_init'
  !//---------------------------------------------------------------------
  




  write(6,'(a)') f90file//':'//subr// &
     ': Initialize H2 drydeposition'

  RH2(:,:,:) = 0.d0



  if (H2dep .eq. 1) then 
     write(6,'(a)') 'H2 drydeposition routine: h2dep_eh2009'
  else if (H2dep .eq.2) then
     write(6,'(a)') 'H2 drydeposition routine: h2dep_price2006'
  else if (H2dep .eq. 3) then
     write(6,'(a)') 'H2 drydeposition routine: h2dep_sanderson2003'
  else
     write(6,'(a)') f90file//':'//subr// &
          'H2 drydepostion routine not implemented'
     stop
  endif




  !//---------------------------------------------------------------------
end subroutine h2drydep_init
!//----------------------------------------------------------------------

subroutine update_h2_soiluptake()
 !// --------------------------------------------------------------------
  !// Main H2 drydeposition routine. Choose the routine to be used here.
  !// Sets up VDEP for H2.
  !// Called from update_drydepvariables in drydeposition_oslo.f90
  !// --------------------------------------------------------------------

  !// --------------------------------------------------------------------
  implicit none
  !// --------------------------------------------------------------------
   character(len=*), parameter :: subr = 'update_h2_soiluptake'
  !//---------------------------------------------------------------------
  
  write(6,'(a)') f90file//':'//subr// &
     ': H2 drydeposition, scheme values updated'

  !// Here to choose which deposition routine to be used,
  !// Set as parameter.
  if (H2dep .eq. 1) then 
     call h2dep_eh2009()
  else if (H2dep .eq.2) then
     call h2dep_price2006()
  else if (H2dep .eq. 3) then
     call h2dep_sanderson2003()
  else
     write(6,'(a)') f90file//':'//subr// &
     ': H2deposition scheme not implemented'
     print*, H2dep
     stop
  endif




end subroutine update_h2_soiluptake


!//-----------------------------------------------------------------------
subroutine h2drydep(VDEP,MP)
  !// --------------------------------------------------------------------
  !// Main H2 drydeposition routine. Choose the routine to be used here.
  !// Sets up VDEP for H2.
  !// Called from setdrydep in drydeposition_oslo.f90
  !// --------------------------------------------------------------------
  use cmn_size, only: NPAR
  use cmn_oslo, only: trsp_idx
  use cmn_ctm, only: YDGRD, JDAY,MPBLKIB, MPBLKIE, MPBLKJB, MPBLKJE
  !// --------------------------------------------------------------------
  implicit none
  !// --------------------------------------------------------------------
  
  !// Output
  real(r8), intent(inout) :: VDEP(NPAR,IPAR,JPAR)
  integer :: NTR, I, J
  integer, intent(in) :: MP
  !// --------------------------------------------------------------------
  character(len=*), parameter :: subr = 'h2drydep'
  !//---------------------------------------------------------------------
  

  !Return if hydrogen not the component
  NTR = trsp_idx(113)
  if (NTR .le. 0) return

!Moved to a different subroutine.

!  !// Here to choose which deposition routine to be used,
!  if (H2dep .eq. 1) then 
!     call h2dep_eh2009(MP)
!  else if (H2dep .eq.2) then
!     call h2dep_price2006(MP)
!  else if (H2dep .eq. 3) then
!     call h2dep_sanderson2003(MP)
!  else
!     write(6,'(a)') f90file//':'//subr// &
!     ': H2deposition scheme not implemented'
!     print*, H2dep
!     stop
!  endif

  !// For this routine, all values are put in 1:
  !// Convert from [cm/s] to [m/s]
  !// Loop over latitude (J is global, JJ is block)
  do J = MPBLKJB(MP),MPBLKJE(MP)
     !// Loop over longitude (I is global, II is block)
     do I = MPBLKIB(MP),MPBLKIE(MP)
        !Already weighetd by landfrac.
        VDEP(NTR,I,J) = RH2(1,I,J)*1.e-2_r8 + &
             RH2(2,I,J)*1.e-2_r8 + &
             RH2(3,I,J)*1.e-2_r8 + &
             RH2(4,I,J)*1.e-2_r8 + &
             RH2(5,I,J)*1.e-2_r8 + &
             RH2(6,I,J)*1.e-2_r8 + &
             RH2(6,I,J)*1.e-2_r8 
     end do
  end do

  !// --------------------------------------------------------------------
end subroutine h2drydep
!//-----------------------------------------------------------------------


!//-----------------------------------------------------------------------
subroutine get_cover() 
  !// --------------------------------------------------------------------
  use cmn_sfc, only: landSurfTypeFrac
  !// --------------------------------------------------------------------
  implicit none
  !// --------------------------------------------------------------------

  real(r8) :: tlat,deltaj
  integer :: i,j
  !// --------------------------------------------------------------------

  !//------------------------------------------------------------------
  !// ISLSCP2 MODIS land fraction and type data (unit: %)
  !// into
  !// PLAND   = land fraction (m2/m2)
  !// landSurfTypeFrac  = land surface type (up to 18, which defined as following)
  !// Note: type 0 represents water, which is used as land fraction
  !//       and stored in PLAND
  !//------------------------------------------------------------------
  !// The following 17 IGBP classes are used.
  !//  0=Water Bodies                        1=Evergreen Needleleaf Forests
  !//  2=Evergreen Broadleaf Forests         3=Deciduous Needleleaf Forests
  !//  4=Deciduous Broadleaf Forests         5=Mixed Forests
  !//  6=Closed Shrublands                   7=Open Shrublands
  !//  8=Woody Savannas                      9=Savannas
  !// 10=Grasslands                         11=Permanent Wetlands
  !// 12=Croplands                          13=Urban and Built-Up
  !// 14=Cropland/Natural Vegetation Mosaic 15=Permanent Snow and Ice
  !// 16=Barren or Sparsely Vegetated       17=Unclassified
  !//------------------------------------------------------------------
  
  !// Initialize
  landcover(:,:,:)=0.d0
  landice(:,:)=0.d0

  !//
  !// Find the land cover (the six different ones)
  !// Original indices
  !//  1  0 Water
  !//  2  1 Evergreen Needleleaf Forest 
  !//  3  2 Evergreen Broadleaf Forest 
  !//  4  3 Deciduous Needleleaf Forest
  !//  5  4 Deciduous Broadleaf Forest
  !//  6  5 Mixed Forest  
  !//  7  6 Closed Shrubland
  !//  8  7 Open Shrubland 
  !//  9  8 Woody Savannas
  !// 10  9 Savannas
  !// 11 10 Grasslands
  !// 12 11 Permanent Wetlands
  !// 13 12 Croplands
  !// 14 13 Urban and Built-Up 
  !// 15 14 Cropland/Natural Vegetation Mosaic
  !// 16 15 Snow and Ice
  !// 17 16 Barren or Sparsely Vegetated 
  !// 18 17 Unclassified
  !//
  !// New indices, giving the fraction of each type in each grid box
  !//  1 Savannah (9+10 up to 30N)
  !//  2 Agricultural land (13+15)
  !//  3 Forest (2+3+4+5+6)
  !//  4 Grasslands (11)
  !//  5 Peat/Tundra (7+8 north of 50N + 9+10 north of 30N.)
  !//  6 Semi desert (7+8 60S-60N)
  !//  7 Wetlands (12), to be used in Eurohydros simple scheme
  !//  - Other, not used for H2 (12+14+16+17+18)


  !// Adding the fractions
  !//  1 Savannah (9+10 up to 30N)
  landcover(:,:,1)=landSurfTypeFrac(9,:,:)+landSurfTypeFrac(10,:,:)
  !//  2 Agricultural land (13+15)
  landcover(:,:,2)=landSurfTypeFrac(13,:,:)+landSurfTypeFrac(15,:,:)
  !//  3 Forest (2+3+4+5+6)
  landcover(:,:,3)=landSurfTypeFrac(2,:,:)+landSurfTypeFrac(3,:,:)+ &
       landSurfTypeFrac(4,:,:)+landSurfTypeFrac(5,:,:)+landSurfTypeFrac(6,:,:)
  !//  4 Grasslands (11)
  landcover(:,:,4)=landSurfTypeFrac(11,:,:)
  !//  5 Peat/Tundra (7+8 north of 50N + 9+10 north of 30N.)
  landcover(:,:,5)=landSurfTypeFrac(7,:,:)+landSurfTypeFrac(8,:,:)+ &
       landSurfTypeFrac(17,:,:) !// to be modified below
  !//  6 Semi desert (7+8 60S-60N)
  landcover(:,:,6)=landSurfTypeFrac(7,:,:)+landSurfTypeFrac(8,:,:)+ &
       landSurfTypeFrac(17,:,:) !// to be modified below
  !//  7 Wetlands (12), to be used in Eurohydros simple scheme
  landcover(:,:,7)=landSurfTypeFrac(12,:,:)
  !//  - Other, not used for H2 (12+14+16+17+18)
  !// 12 11 Permanent Wetlands
  !// 14 13 Urban and Built-Up 
  !// 16 15 Snow and Ice
  !// 18 17 Unclassified

  !15=Permanent Snow and Ice
  landice(:,:) =  landSurfTypeFrac(16,:,:)

  !// Peat/Tundra
  do j=1,JPAR-1
     deltaj = 180./JPAR
     !// roughly ...
     tlat=-90+deltaj/2. + j*deltaj
     if (tlat .lt. 55.) then
        ! assume semi desert, peat/tundra is zero
        landcover(:,j,5)=0.
     else
        ! assume peat/tundra, not semi desert
        landcover(:,j,6)=0.
     endif
     if (tlat .gt. 30.) then
        ! assume savannah is really grass/steppe north of 30N
        landcover(:,j,4)=landcover(:,j,4)+landcover(:,j,1)
        landcover(:,j,1)=0.
     endif
  enddo

  write(*,'(A)') '** Fetched MODIS land cover for H2 deposition'

  
  do i=1,7
       print*,i,landcover(37,62,i)
  enddo

  do i=1,7
       print*,i,landcover(35,50,i)
  enddo


  !// --------------------------------------------------------------------
end subroutine get_cover
!// ----------------------------------------------------------------------





  !// ----------------------------------------------------------------------
end module sanderson
!//=========================================================================


