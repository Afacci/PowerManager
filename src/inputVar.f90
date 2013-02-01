
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

!---General input variables---
character(len=20)  :: gridConnection 
integer            :: nTimes
logical            :: iDeg 
real(kind(1.d0)), allocatable, dimension(:) :: startPoint, upTime0, downTime0
real(kind(1.d0))                            :: dt1
character(len=100) :: obj, method
logical :: writePower, writeEnergy, writeEfficiency, writeElectricRev,writeThermalRev,writeChillingRev &
           ,writeFuelCost

!---Trigeneration.inp variables
integer                                         :: nTrig
integer, allocatable, dimension(:)              :: nSpT, nSizeT, nEtaElT, nEtaThT, nEtaChT
real(kind(1.d0)),allocatable, dimension(:)      :: pMaxT , degRateT, fuelCostT, fuelLHVT, invT, lifeT, &
                                                   fireCostT, maintCostT,minUpTimeT, minDownTimeT
real(kind(1.d0)),allocatable, dimension(:,:)    :: spT, kSizeT
real(kind(1.d0)), allocatable, dimension(:,:,:) :: etaElT, etaThT, etaChT
character(len=50), allocatable, dimension(:)    :: tecT

!---Boilers.inp variables
integer                                         :: nBoi
integer, allocatable, dimension(:)              :: nSpB, nSizeB, nEtaB
real(kind(1.d0)),allocatable, dimension(:)      :: pMaxB , degRateB, fuelCostB, fuelLHVB,               & 
                                                   invB, lifeB, fireCostB, maintCostB , minUpTimeB, minDownTimeB 
real(kind(1.d0)),allocatable, dimension(:,:)    :: spB, kSizeB
real(kind(1.d0)), allocatable, dimension(:,:,:) :: etaB
character(len=50), allocatable, dimension(:)    :: tecB

!---Chillers.inp variables
integer                                         :: nChi
integer, allocatable, dimension(:)              :: nSpC, nSizeC, nEtaC
real(kind(1.d0)),allocatable, dimension(:)      :: pMaxC , degRateC, invC, LifeC, fireCostC, maintCostC, minUpTimeC, minDownTimeC  
real(kind(1.d0)),allocatable, dimension(:,:)    :: spC, kSizeC
real(kind(1.d0)), allocatable, dimension(:,:,:) :: etaC
character(len=50), allocatable, dimension(:)    :: tecC

!---Load.inp variables
integer                                       :: nTime, nLoad, iTime, iEl, iTh, iCh, iElp, iThp, iChp
real(kind(1.d0)), allocatable, dimension(:,:) :: uEl, uTh, uCh, cEl, cTh, cCh
real(kind(1.d0)), allocatable, dimension(:)   :: time, gridBuyCost, gridSellCost
integer, allocatable, dimension(:)            :: nld, nlp


end module inputVar