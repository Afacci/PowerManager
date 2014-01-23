
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
!>\file plantVar.f90
!>\brief collection of variables relative to the power plant structure.
!>\author 
!  Andrea Facci.
!
!---------------------------------------------------------------------------

!>\brief collection of variables relative to the power plant structure.
!>\details collection of variables relative to the power plant structure.
!>\author 
!  Andrea Facci.
module plantVar   

use shared
use shared
use inputVar

real(kind = prec), allocatable, dimension(:,:)   :: etaElT_, etaThT_, etaChT_, etaB_, etaC_, &
                                                   sp!,cRef
real(kind = prec), allocatable, dimension(:,:,:) :: envCorr
real(kind = prec), allocatable, dimension(:,:)   :: etaEl, etaTh, etaCh, timeVinc
integer         , allocatable, dimension(:,:)    :: cr
real(kind = prec), allocatable, dimension(:)     :: Pmax, dt, cf, lhv, onOffCost, OeMCost, minUpTime, minDownTime, pef, pecOn, &
                                                    soc, socTh, socEl, Esoc, socIS, iceSoc
character(len=4), allocatable, dimension(:)      :: pes
integer                                          :: nSpTot,nm,  nm0, nsoc, nSocEl, nSocIce, nx
integer,                       dimension(6)      :: is,ie
integer,          allocatable, dimension(:)      :: nSp, nTv, eSource
character(len=50), allocatable, dimension(:)     :: tec
real(kind=prec), allocatable, dimension(:)       :: sunEl, sunTh , windEl
logical                                          :: elStor = .false., iceStor = .false.
integer, allocatable,dimension(:,:)              :: cPred
real(kind=prec), allocatable, dimension(:)       :: etaEsIn_, etaEsOut_

integer, parameter :: iT = 1, iB = 2, iC = 3, iTS = 4, iES = 5, iIs = 6


end module plantVar

