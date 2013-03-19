
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

!---General input variables---
character(len=20)  :: gridConnection 
integer            :: nTimes
logical            :: iDeg 
real(kind = prec), allocatable, dimension(:) :: startPoint, upTime0, downTime0
real(kind = prec)                            :: dt1, k_tr, k_el, c_tr, c_st
character(len=100) :: obj, method
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
logical :: global           = .false.
logical :: useEuristics     = .false.

!---Trigeneration.inp variables
integer                                         :: nTrig
integer, allocatable, dimension(:)              :: nSpT, nEtaElT, nEtaThT, nEtaChT, ntcT, npcT, nacT
real(kind= prec),allocatable, dimension(:)      :: pMaxT, fuelCostT, fuelLHVT, lifeT, &
                                                   fireCostT, maintCostT,minUpTimeT, minDownTimeT
real(kind= prec),allocatable, dimension(:,:)    :: spT
real(kind= prec), allocatable, dimension(:,:,:) :: etaElT, etaThT, etaChT, tempCorrT, presCorrT, altCorrT
character(len=50), allocatable, dimension(:)    :: tecT

!---Boilers.inp variables
integer                                         :: nBoi
integer, allocatable, dimension(:)              :: nSpB, nEtaB, ntcB, npcB, nacB
real(kind= prec),allocatable, dimension(:)      :: pMaxB ,fuelCostB, fuelLHVB,               & 
                                                   lifeB, fireCostB, maintCostB , minUpTimeB, minDownTimeB 
real(kind= prec),allocatable, dimension(:,:)    :: spB
real(kind= prec), allocatable, dimension(:,:,:) :: etaB, tempCorrB,  presCorrB, altCorrB
character(len=50), allocatable, dimension(:)    :: tecB

!---Chillers.inp variables
integer                                         :: nChi
integer, allocatable, dimension(:)              :: nSpC, nSizeC, nEtaC, ntcC, npcC, nacC
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

end module inputVar
