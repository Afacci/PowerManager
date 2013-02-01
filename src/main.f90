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
!>\file main.f90
!>\brief this is the main driver.
!!\details
!!    main driver
!!@author
!!    Andrea Facci.
!
!---------------------------------------------------------------------------

!>\brief this is the main driver.
!!\details
!!    main driver
!!@author
!!    Andrea Facci.
program main


!---Declare Module usage---
use mathTools
use inputVar
use cmdVar

use plantVar
use interfaces
use graphTools
use myArithmetic

use economy

implicit none

real(kind(1.d0)) :: rdummy
character(len=100) :: cDummy
integer :: nComb, i, dummy(2),k,j, kBw, idummy(3)
real(kind(1.d0)) :: cost
integer,allocatable, dimension(:,:) :: setPoint,setPointBw
real(kind(1.d0)),allocatable, dimension(:,:) :: load, loadBw, upTime
integer,allocatable, dimension(:) :: minPathBw
real               :: tempo, t2
real, dimension(2) :: tVec

 
call system('clear')

print*
print*, '!-----------------------------------------------------------!'
print*, '!                        PowerManager                       !'
print*, '!-----------------------------------------------------------!'
print* 

call commandLine

!----Read Input Files----
print*, ' ---Reading Input Files---'
call readTrigen
call readBoiler
call readChillers
call readLoads
call readGeneral

!---build the energy coonection inside the plant---
print*, ' ---Checking Input Coherence---'
call buildPlant
call etime(tVEc,tempo)
print*, ' ---Elapsed Time: ', tempo, ' sec'

!---Build the graph-----
print*, ' ---Building the graph---'
cDummy = 'set-point'
call allCombin(icm=cr,imax=nSp,m=nm,targ=cDummy) 
cDummy = 'state'
call allCombin(dcm=sp,imax=nSp,m=nm,targ=cDummy) 
call graphPoints
call graphArcs
call etime(tVEc,tempo)
print*, ' ---Elapsed Time: ', tempo, ' sec'
print*

!-----optimization -------------!
print*, ' ---Finiding minumum path---'
select case(method)
   case('Forward')
      allocate(setPoint(0:nTime+1,nm), load(0:nTime+1,nm))
      call minPathTopoFw(setPoint, cost)
   case('Backward')
      cDummy = 'time-constraints' 
      call allCombin(dcm=timeVinc,imax=nTv,m=2*nm,targ=cDummy) 
!      allocate(setPointBw(0:nTime+1,nm), loadBw(0:nTime+1,nm))
      allocate(setPoint(0:nTime+1,nm), load(0:nTime+1,nm))
      allocate(upTime(0:nTime+1,2*nm))
      allocate(minPathBw(0:nTime+1))
      call minPathTopoBw(setPoint, cost, upTime, minPathBw)
end select
!---print some data to screen----
if(verb) then
   do i=0,nTime+1
      select case(method)
         case('Forward')
           print*, i,'pointFW = ', setPoint(i,:)
         case('Backward')
           print*, i,'pointBW = ', setPoint(i,:), 'vertex = ', minPathBw(i)
      end select
   enddo
endif
call etime(tVEc,tempo)
print*, ' ---Elapsed Time: ', tempo, ' sec'

!----write output files---
call output(setPoint)

!---end the execution----
call endExecution

end program main