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
!    along with OpenFOAM; if not, write to the Free Software Foundation,
!    Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA
!
!>\file buildPlant.f90
!>\brief 
!> Collects all the informations relative to the power plant.
!>\author 
!  Andrea Facci.
!
!---------------------------------------------------------------------------

!>\brief 
!> Collects all the informations relative to the power plant.
!>\details
!> Collects all the informations relative to the power plant starting from the 
!> Inputs of the single machinery. Calculates the efficiencies for each given
!> set-point and store them in a single array.
!> Perform units of measure conversions when necessary.
!>\author 
!  Andrea Facci.
subroutine buildPlantRev

!---Declare Module usage---

use cmdVar
use inputVar
use plantVar
use mathTools
use interfaces
use myArithmetic
use sun
!use eolo
use addSection

!---Declare Local Variables---
implicit none

interface
  function timestep()
    use shared 
    use inputvar, only : nTime, time, dt1
    implicit none
    real(kind=prec), dimension(0:nTime) :: timestep
  end function
end interface

integer                                          :: i,j, maxsp, k, k1, k2
real(kind = prec)                                :: kJ_kWh, dsp, dtmin, dsoc, dsocEl, dspEl
real(kind = prec), allocatable, dimension(:,:)   :: upTimeVinc, downTimeVinc
real(kind = prec), dimension(1)                  :: rdummy1, rdummy2
real(kind = prec), dimension(:,:,:), allocatable :: tCorr, pCorr
real(kind = prec), dimension(:,:), allocatable   :: aCorr
integer                                          :: nMax, n1, n2, n3,n4

!--subroutine body-----

call allocateVar(16)

dt        = timestep()
upTime0   = upTime0*3.6e3
downTime0 = downTime0*3.6e3

nm0  = nTrig + nBoi + nChi + nHP                     !total number of machinery without storage
nm   = nm0 + 1
nx = nm0
if(capacityTS.gt.zero.and.PmaxTS.gt.zero) nx  = nx + 1                                 
!if(capacityES.gt.zero.and.PmaxES.gt.zero) nx  = nx + 1                                 
!if(capacityIS.gt.zero.and.PmaxIS.gt.zero) nx  = nx  + 1

nSpTot = 0
if(nTrig.gt.0) then 
   nSpTot = nSpTot + sum(nSpT)
   n1     = maxval(nSpT)
else
   n1 = 0
endif
if(nBoi.gt.0) then 
   nSpTot = nSpTot + sum(nSpB)
   n2     = maxval(nSpB)
else
   n2 = 0
endif
if(nChi.gt.0) then 
   nSpTot = nSpTot + sum(nSpC)
   n3 = maxval(nSpC)
else
   n3 = 0
endif
if(nHP.gt.0) then 
   nSpTot = nSpTot + sum(nSpHP)
   n4 = maxval(nSpHP)
else
   n4 = 0
endif
if(capacityTS.gt.zero.and.pmaxTs.gt.zero) then 
   nSpTot = nSpTot + 2*nSpTS + 1
else
   nSpTS = 1
   nSpTot = nSpTot + 1
endif
!if(capacityES.gt.zero.and.pmaxEs.gt.zero) then 
!   nSpTot = nSpTot + 2*nSpES + 1
!else
!   nSpES = 1
!   nSpTot = nSpTot + 1
!endif
!if(capacityIS.gt.zero.and.pmaxIs.gt.zero) then 
!   nSpTot = nSpTot + 2*nSpIS + 1
!else
!   nSpIS = 1
!   nSpTot = nSpTot + 1

!endif
call allocateVar(17)
capacityEs = zero
pmaxEs = zero
capacityIS = zero
pMaxIS = zero
nSpIS = 0
nSpEs = 0


!nMax = max(n1, n2, n3, 2*nSpTS + 1,2*nSpES + 1,2*nSpIS + 1)
nMax = max(n1, n2, n3,n4, 2*nSpTS + 1)
call allocateVar(18,nMax)
call allocateVar(32)

sp(:,:) = rNan(rVal)
eSource(:) = -1
envCorr = rNaN(rVal)
cr(:,:) = 0

is(iT) = 1                   !Trigeneration
ie(iT) = nTrig                             
call addTrigenerative
is(iB) = is(iT) + nTrig      !Boilers
ie(iB) = ie(iT) + nBoi
call addBoilers
is(iC) = is(iB) + nBoi       !Chillers
ie(iC) = ie(iB) + nChi
call addChillers
is(iHP) = is(iC) + nChi
ie(iHP) = ie(iC) + nHP
call addHeatPump

is(iTs)= nm0 + 1
call addThermalStorage
!if(capacityES.gt.zero.and.PmaxES.gt.zero) then 
!  is(iEs)= is(iTs) + 1
!if(capacityES.gt.zero.and.PmaxES.gt.zero) then 
!  elStor = .true.
!endif
!call addElectricalStorage
!is(iIs)= is(iEs) + 1
!iceStor = .true.
!call addIceStorage

!------renewable stuff--------------------------------
allocate(sunEl(nTime), sunTh(nTime), windEl(nTime))
if(surfPV.gt.zero) then
  sunEl = photo()
else
  sunEl = zero
endif

if(surfSC.gt.zero) then
  sunTh = thermalCollector()
else
  sunTh = zero
endif

!if(nwf.gt.0) then
!  windEl = windPower()
!else
  windEl = zero
!endif

!---time-dependent constraints---
call allocateVar(21)
k1 = maxval(minUpTime)/3.6e3 + 1
k2 = maxval(minDownTime)/3.6e3 + 1
allocate(upTimeVinc(max(k1,k2),nm), downTimeVinc(max(k1,k2),nm))
!min running time
timeVinc(:,:) = zero
do i=1,nm0
   nTv(i) = minUpTime(i)/3.6e3 + 1
   do j=1,ntv(i)
      timeVinc(j,i) = (j - 1)*3.6e3
   enddo
enddo
!min non-running time
do i=1,nm0
   k = nm0 + i
   nTv(k) = minDownTime(i)/3.6e3 + 1
   do j=1,ntv(k)
      timeVinc(j,k) = (j - 1)*3.6e3
   enddo
enddo
!Thermal storage capacity constraint
k = 2*nm0 + 1
nTv(k) = nSoc
do j=1,nsoc
   timeVinc(j,k) = soc(j)
enddo
!k = 2*nm0 + 2
!nTv(k) = nSocEl
!do j=1,nSocEL
!   timeVinc(j,k) = Esoc(j)
!enddo
!k = 2*nm0 + 3
!nTv(k) = nSocIce
!do j=1,nSocIce
!   timeVinc(j,k) = IceSoc(j)
!enddo

!--- check that the power plant is able to satisfie the required loads
call checkPlant

!--- convert all the prices in €/kJ---
kJ_kWh = 1.0/3.6e3
gridBuyCost(:)  = gridBuyCost(:)*kJ_kWh
gridSellCost(:) = gridSellCost(:)*kJ_kWh
cEl(:,:)        = cEl(:,:)*kJ_kWh
cTh(:,:)        = cTh(:,:)*kJ_kWh
cCh(:,:)        = cCh(:,:)*kJ_kWh
c_st = c_st*kJ_kWh
c_tr = c_tr*kJ_kWh

call deallocateVar(1)

end subroutine buildPlantRev

!===============================================================0

