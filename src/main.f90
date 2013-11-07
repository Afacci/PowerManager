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
integer                                       :: i, dummy(2),k,j, kBw, idummy(3), nr
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
print*,'     --> Reading Trigeneration.inp     ... OK '
call readBoiler
print*,'     --> Reading Boiler.inp            ... OK '
call readChillers
print*,'     --> Reading Chillers.inp          ... OK '
call readThStorage
print*,'     --> Reading ThermalStorage.inp    ... OK '
call readIceStorage
print*,'     --> Reading IceStorage.inp        ... OK '
call readElStorage
print*,'     --> Reading ElectricalStorage.inp ... OK '
call readLoads
print*,'     --> Reading Loads.inp             ... OK '
call readGeneral
print*,'     --> Reading General.inp           ... OK '
call readPV
print*,'     --> Reading Photovoltaic.inp      ... OK '
call readSC
print*,'     --> Reading Photovoltaic.inp      ... OK '
call readEnv
print*,'     --> Reading Enivronment.inp       ... OK '
call readWind
print*,'     --> Reading WindTurbines.inp      ... OK '


!---build the energy coonection inside the plant---
print*, ' ---Checking Input Coherence---'
call buildPlantRev
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
  case('FullPower')
      call allCombin(icm=cr,imax=nSp,m=nm,targ='set-point') 
      call allCombin(dcm=sp,imax=nSp,m=nm,targ='state') 
      setPoint = fullLoad()
  case('Optimized')
      !---Build the graph-----
      print*, ' ---Building the graph---'
      call allCombin(icm=cr,imax=nSp,m=nm,targ='set-point') 
      call allCombin(dcm=sp,imax=nSp,m=nm,targ='state') 
      if(method.eq.'Reduced-Backward') then
         nr = maxval(nSp(nm0+1:nm))
         call allCombin(icm=cr(1:nr,nm0+1:nm),imax=nSp(nm0+1:nm),m=3,targ='storageSp') 
         nr = maxval(nSp(1:nm0))
         call allCombin(icm=cr(1:nr,1:nm0),imax=nSp(1:nm0),m=nm0,targ='prodSp') 
         print*, '    --> Vertices'
         call graphPointsLocalMin
         print*, '    --> Arcs'
         call graphArcs
      else
          print*, '    --> Vertices'
          call graphPoints
          print*, '    --> Arcs'
          call graphArcs
      endif
      call etime(tVEc,tempo)
      print*, ' ---Elapsed Time: ', tempo, ' sec'
      print*
      
      !-----optimization -------------!
      print*, ' ---Finiding minumum path---'
      select case(method)
         case('Forward')
            call minPathTopoFw(setPoint, cost)
         case('Backward', 'Reduced-Backward')
!            cDummy = 'time-constraints' 
            call allCombin(dcm=timeVinc,imax=nTv,m=2*nm0 + 3,targ='time-constraints') 
            allocate(upTime(0:nTime+1,2*nm0 + 3))
            allocate(minPathBw(0:nTime+1))
            call minPathTopoBw(setPoint, cost, upTime, minPathBw)
      end select
      !---print some data to screen----
      if(verb) then
         do i=0,nTime+1
            select case(method)
               case('Forward')
                 print*, i,'pointFW = ', setPoint(i,:)
               case('Backward', 'Reduced-Backward')
                 print*, i,'pointBW = ', setPoint(i,:), 'vertex = ', minPathBw(i)
            end select
         enddo
      endif
end select

call etime(tVEc,tempo)
print*, ' ---Elapsed Time: ', tempo, ' sec'

!----write output files---
call output(setPoint,.false.,'Results')

if (allocated(upTime)) deallocate(upTime)
if (allocated(minPathBw)) deallocate(minPathBw)
!---end the execution----
call endExecution

end program main
