
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
!>\file inputVar.f90
!>\brief 
!> Input variables collection.
!>\author 
!     Andrea Facci.
!
!---------------------------------------------------------------------------

!>\brief 
!> Input variables collection.
!>\details 
!> This module collects all the variables read from input files. Include this module To use these
!> variable anywhere in the code. 
!>\author 
!> Andrea Facci.

module inputVar

use shared

logical, dimension(3) :: iPrio = .false.

!---General input variables---
character(len=20)  :: gridConnection 
integer            :: nTimes
logical            :: iDeg 
real(kind = prec), allocatable, dimension(:) :: startPoint, upTime0, downTime0
real(kind = prec)                            :: dt1, k_tr, k_el, c_tr, c_st, pefGrid, iSocTh, eSocTh
character(len=100) :: obj, method, strategy
logical :: writePower       = .false.
logical :: writeEnergy      = .false.
logical :: writeEfficiency  = .false.
logical :: writeElectricRev = .false.
logical :: writeThermalRev  = .false.
logical :: writeChillingRev = .false. 
logical :: writeDemand      = .false.
logical :: writeInput       = .false.
logical :: writeCosts       = .false.
logical :: writeTrig        = .false.
logical :: writeBoi         = .false.
logical :: writeChi         = .false.
logical :: writePec         = .false.
logical :: writeRen         = .false.
logical :: global           = .false.
logical :: useEuristics     = .false.
logical , dimension(5) :: kPEC = .false.
logical :: iPec = .false.

!---Trigeneration.inp variables
integer                                         :: nTrig
integer, allocatable, dimension(:)              :: nSpT, nEtaElT, nEtaThT, nEtaChT, ntcT, npcT, nacT, &
                                                   TrigPriority
real(kind= prec),allocatable, dimension(:)      :: pMaxT, fuelCostT, fuelLHVT, lifeT, pefT, pecOnT,   &
                                                   fireCostT, maintCostT,minUpTimeT, minDownTimeT
real(kind= prec),allocatable, dimension(:,:)    :: spT
real(kind= prec), allocatable, dimension(:,:,:) :: etaElT, etaThT, etaChT, tempCorrT, presCorrT, altCorrT
character(len=50), allocatable, dimension(:)    :: tecT

!---Boilers.inp variables
integer                                         :: nBoi
integer, allocatable, dimension(:)              :: nSpB, nEtaB, ntcB, npcB, nacB, BoiPriority
real(kind= prec),allocatable, dimension(:)      :: pMaxB ,fuelCostB, fuelLHVB, pefB,  pecOnB,          & 
                                                   lifeB, fireCostB, maintCostB , minUpTimeB, minDownTimeB 
real(kind= prec),allocatable, dimension(:,:)    :: spB
real(kind= prec), allocatable, dimension(:,:,:) :: etaB, tempCorrB,  presCorrB, altCorrB
character(len=50), allocatable, dimension(:)    :: tecB

!---Chillers.inp variables
integer                                         :: nChi
integer, allocatable, dimension(:)              :: nSpC, nSizeC, nEtaC, ntcC, npcC, nacC, ChiPriority
real(kind= prec),allocatable, dimension(:)      :: pMaxC , fireCostC, maintCostC, minUpTimeC, minDownTimeC  
real(kind= prec),allocatable, dimension(:,:)    :: spC
real(kind= prec), allocatable, dimension(:,:,:) :: etaC, tempCorrC, presCorrC, altCorrC
character(len=50), allocatable, dimension(:)    :: tecC

!---Load.inp variables
integer                                       :: nTime, nLoad, iTime, iEl, iTh, iCh, iElp, iThp, iChp
real(kind= prec), allocatable, dimension(:,:) :: uEl, uTh, uCh, cEl, cTh, cCh
real(kind= prec), allocatable, dimension(:)   :: time, gridBuyCost, gridSellCost
integer, allocatable, dimension(:)            :: nld, nlp

!--environment.inp variables
real(kind= prec), allocatable, dimension(:) :: pAmb, tAmb
real(kind= prec), dimension(1)              :: Altitude

!---Thermal storage----

real(kind=prec)                              :: PmaxTS, CapacityTS
integer                                      :: nSpTS , nEtaTsIn, nEtaTsOut
!real(kind=prec), allocatable, dimension(:,:) :: etaTsOut, etaTsIn
real(kind=prec)                              :: etaTsOut, etaTsIn

!---Photovoltaic field ---------------

real(kind=prec)                              :: surfPV, etaPV, slopePV, azimutPV, cutOffPV, latitude, clouds, rhoPV &
                                               ,etaAuxPV
real(kind=prec), dimension(24)               :: BeamRad, DiffRad
real(kind=prec), allocatable, dimension(:,:) :: pvCorr
integer                                      :: Day
logical                                      :: summerTime
character(len=100)                           :: radMod
integer                                      :: ntPv

!-----Solar Collectors field ---------------

real(kind=prec)                              :: surfSC, slopeSC, azimutSC, rhoSC , TinSC
real(kind=prec), allocatable, dimension(:,:) :: etaSC
character(len=100)                           :: SCkind
integer                                      :: nEtaSc

end module inputVar
