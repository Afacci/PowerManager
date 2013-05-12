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
use shared
use mathTools
use inputVar
use cmdVar

use plantVar
use interfaces
use graphTools
use myArithmetic

use economy

use strategies

implicit none

real(kind = prec)                             :: rdummy
character(len=100)                            :: cDummy
integer                                       :: i, dummy(2),k,j, kBw, idummy(3)
real(kind = prec)                             :: cost
real(kind = prec)                             :: tempo, t2
real, dimension(2)                            :: tVec
integer,          allocatable, dimension(:,:) :: setPoint,setPointBw
real(kind = prec),allocatable, dimension(:,:) :: load, loadBw, upTime
integer,          allocatable, dimension(:)   :: minPathBw

 
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
print*,'     --> Trigeneration.inp read'
call readBoiler
print*,'     --> Boiler.inp read'
call readChillers
print*,'     --> Chillers.inp read'
call readLoads
print*,'     --> Loads.inp read'
call readGeneral
print*,'     --> General.inp read'
call readEnv
print*,'     --> Enivronment.inp read'
call readThStorage
print*,'     --> ThermalStorage.inp read'

!---build the energy coonection inside the plant---
print*, ' ---Checking Input Coherence---'
call buildPlant
call etime(tVEc,tempo)
allocate(setPoint(0:nTime+1,nm), load(0:nTime+1,nm))
print*, ' ---Elapsed Time: ', tempo, ' sec'

select case(strategy)
  case('ThermalTrack') 
      call allCombin(icm=cr,imax=nSp,m=nm,targ='set-point') 
      call allCombin(dcm=sp,imax=nSp,m=nm,targ='state') 
      setPoint = thermalTracking()
  case('ElectricalTrack')
      call allCombin(icm=cr,imax=nSp,m=nm,targ='set-point') 
      call allCombin(dcm=sp,imax=nSp,m=nm,targ='state') 
      setPoint = electricalTracking()
  case('Optimized')
      !---Build the graph-----
      print*, ' ---Building the graph---'
      cDummy = 'set-point'
      call allCombin(icm=cr,imax=nSp,m=nm,targ=cDummy) 
      cDummy = 'state'
      call allCombin(dcm=sp,imax=nSp,m=nm,targ=cDummy) 
      print*, '    --> Vertices'
      call graphPoints
      print*, '    --> Arcs'
      call graphArcs
      call etime(tVEc,tempo)
      print*, ' ---Elapsed Time: ', tempo, ' sec'
      print*
      
      !-----optimization -------------!
      print*, ' ---Finiding minumum path---'
      select case(method)
         case('Forward')
            call minPathTopoFw(setPoint, cost)
         case('Backward')
            cDummy = 'time-constraints' 
            call allCombin(dcm=timeVinc,imax=nTv,m=2*nm0,targ=cDummy) 
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
end select

call etime(tVEc,tempo)
print*, ' ---Elapsed Time: ', tempo, ' sec'

!----write output files---
call output(setPoint,.false.,'Results')

!---end the execution----
call endExecution

end program main
