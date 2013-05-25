!---------------------------------------------------------------------------
!  
!   PowerManager.  
!
!---------------------------------------------------------------------------
!License
!    This file is part of PowerManager.
!
!    PowerManager is free software; you can redistribute it and/or modify it
!    under the terms of the GNU General Public License as published by the
!    Free Software Foundation; either version 2 of the License, or (at your
!    option) any later version.
!
!    PowerManager is distributed in the hope that it will be useful, but WITHOUT
!    ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
!    FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
!    for more details.
!
!    You should have received a copy of the GNU General Public License
!    along with PowerManager; if not, write to the Free Software Foundation,
!    Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA
!
!>\file 
!>\brief File  prototype. 
!>\details this is the prototype for all the files of the PowerManger project.
!> Copy, rename, and modify this this file to create a new procedure or module.
!>\Author 
!>     Andrea Facci.
!
!---------------------------------------------------------------------------

module sun

use inputVar

real(kind=prec), parameter, private                 :: pi  = 3.1415927
real(kind=prec), parameter, private                 :: deg = pi/180.0!1.7453293e-02
real(kind=prec), allocatable, dimension(:), private :: hrad, cosZ, Z, cosTh, totRad
real(kind=prec), private                            :: orientation, slope , decl, lat  
logical , allocatable, dimension(:), private        :: night

contains

  function photo()

    implicit none
 
    !---Declare Local Variables---
    real(kind=prec),dimension(nTime)  :: photo
    real(kind=prec)                   :: rd, rb, rr, gt
    integer                           :: i

    !---function body-----

    slope       = slopePV*deg
    orientation = AzimutPV*deg
    
    allocate(totRad(nTime))

    call Astronomy

    if(radMod.eq.'LiuJordan') then
       call liuJordan
    endif
    
    TotRad = BeamRad + DiffRad
    
    do i=1,nTime
       if(.not.night(i)) then
          rd = 0.5*(1.0 + cos(slope))
          rb = cosTh(i)/cosZ(i)
          rr = rhoPV*0.5*(1.0 - cos(slope))
          gt = BeamRad(i)*rb + rd*DiffRad(i) + TotRad(i)*rr
          if(gt.gt.cutOffPV) then 
             photo(i) = etaPV*surfPV*etaAuxPV*gt
          else
             photo(i) = zero
          endif
       else
          photo(i) = zero
       endif
    enddo

    deallocate(totRad)
    deallocate(hrad, cosZ, Z, cosTh, night)

    return
  
  end function photo
  
!===============================================================================================
  
  subroutine LiuJordan
  
    implicit none
  
    real(kind=prec), parameter :: sun = 1367.0e-3
    real(kind=prec)            :: kt, kd, g0h, gh
    integer                    :: i
  
    kt = clouds
    if(kt.lt.0.21) then
       kd = 0.995 - 0.081*kt
    elseif(kt.ge.0.21.and.kt.le.0.76) then
       kd = 0.724 + 2.738*kt - 8.32*kt**2 + 4.93*kt**3
    elseif(kt.gt.0.76) then
       kd = 0.180
    endif
  
    do i=1,nTime
       if(night(i)) then
          DiffRad(i) = zero
          BeamRad(i) = zero
       else
          g0h = sun*cosZ(i)!(sin(lat)*sin(decl) +  cos(lat)*cos(decl)*cos(hrad(i)))
          gh  = g0h*kt
          DiffRad(i) = gh*kd
          BeamRad(i) = gh - DiffRad(i)
       endif
    enddo

    return
  
  end subroutine LiuJordan
 
!======================================================================================================

  subroutine Astronomy

    implicit none
 
    real(kind=prec) :: ang, As, cosAlfa, dh
    integer         :: i
 
    allocate(hrad(nTime), cosZ(nTime), Z(nTime), cosTh(nTime), night(nTime))
    night = .false.

    lat      = latitude*deg                                            !Latitude
    ang      = 2.0*pi*(284.0 + day)/365.0                 
    decl     = deg*23.45*sin(ang)                                      !Solar declination
    if(summerTime) then
       dh = 1
    else
       dh = 0     
    endif
    do i=1,nTime
       hrad(i) = pi/12.0*(time(i) + dh - 12.0)                         !hourly angle
       cosZ(i) = sin(lat)*sin(decl) + cos(lat)*cos(decl)*cos(hrad(i))  !Cosine of Zenith angle
       Z(i)    = acos(cosZ(i))                                         !Zenith angle
       if(cosZ(i).le.zero) night(i) = .true.
       As      = asin(cos(decl)*sin(hrad(i))/cos(0.5*pi - Z(i)))       !Solar Azimuth angle
       cosAlfa = cos(As - orientation)
       cosTh(i)= sin(slope)*sin(Z(i))*cosAlfa + cos(slope)*cosZ(i)
    enddo

    return

  end subroutine Astronomy

!=========================================================================================

  function thermalCollector()
    
    use mathTools

    implicit none
 
    !---Declare Local Variables---
    real(kind=prec),dimension(nTime)  :: thermalCollector, kappa, gt, eff
    real(kind=prec)                   :: rd, rb, rr
    integer                           :: i

    !---function body-----
    allocate(totRad(nTime))

    slope       = slopeSC*deg
    orientation = AzimutSC*deg

    call Astronomy

    if(radMod.eq.'LiuJordan') then
       call liuJordan
    endif
    
    kappa = 1.0

    select case(SCkind)
       case('FlatPlate')
          TotRad = BeamRad + DiffRad
          do i=1,nTime
             rd = 0.5*(1.0 + cos(slope))
             if(night(i)) then
                gt(i) = zero
             else
                rb = cosTh(i)/cosZ(i)
                rr = rhoSC*0.5*(1.0 - cos(slope))
                gt(i) = BeamRad(i)*rb + rd*DiffRad(i) + TotRad(i)*rr
                kappa(i) = (TinSC - Tamb(i))/(gt(i)*1000.0) 
             endif
          enddo
       case('Concentration')
          do i=i,nTime
             if(night(i)) then
                gt(i) = zero
             else   
                rb = cosTh(i)/cosZ(i)
                gt(i) = BeamRad(i)*rb 
                kappa(i) = (TinSC - Tamb(i))/(gt(i)*1000.0) 
             endif
          enddo
    end select

    eff = interpolation(etaSC(:,1),etaSC(:,2),nEtaSC,kappa,nTime) 
    eff = max(eff,zero)
    do i=1,nTime
       if(night(i)) then
         thermalCollector(i) = zero
       else
         thermalCollector(i) = eff(i)*gt(i)*surfSC
       endif
    enddo

    deallocate(totRad)
    deallocate(hrad, cosZ, Z, cosTh, night)

    return
  
  end function thermalCollector

end module sun

