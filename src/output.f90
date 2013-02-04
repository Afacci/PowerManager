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
!>\brief Print the output. 
!>\details Print the output.
!>\Author 
!>     Andrea Facci.
!
!---------------------------------------------------------------------------

subroutine output(setPoint)

!---Declare Module usage---
  use inputvar
  use cmdVar , only : out
  use interfaces
  use plantVar
  use energy
  use economy


!---Declare Local Variables---
  implicit none

  integer, dimension(0:nTime+1,nm), intent(in) :: setPoint
  integer :: i, u,j,k,n
  real(kind(1.d0)), dimension(nTime,nm) :: c
  integer, dimension(nm) :: kk, kko
  logical :: ex, oldRes
  character(len=100) :: folder
  real(kind(1.d0)), dimension(0:nTime) :: t
  real(kind(1.d0)), dimension(nm) :: gg
  real(kind(1.d0)) :: g
  real(kind(1.d0)),dimension(2) :: gGrid
  
  inquire(file='Results/.', exist=oldRes)
  if(oldRes) then
     select case(out)
        case('Overwrite')
           call warning(5)
           call system('rm -rf Results')
        case('Rename') 
           i = 1
           do 
              write(folder,100) i
              inquire(file=trim(folder)//'/.', exist=ex)
              if(.not.ex) exit
              i = i + 1
           enddo
           call warning(6,word=folder)
           call system('mv Results '//folder)
        end select
     endif
     call system('mkdir Results')
  
  u = 500
  call prepareFile(u,'setPoint','Results')
  write(u,*) '# setPoint.dat, contains the optimal set point for all the'
  write(u,*) '# equipment and time-step'
  write(u,*) '#--------------------------------------------------------------------------#'
  write(u,*)
  write(u,'(A8,2X)', advance='no') 'Time [h]'
  do i=1,nTrig
     write(u,'(A5,2X)', advance='no') trim(tecT(i))
  enddo
  do i=1,nBoi
     write(u,'(A5,1X,A6,2X)', advance='no') trim(tecB(i)),'Boiler'
  enddo
  do i=1,nChi
     write(u,'(A,1X,A,2X)', advance='no') trim(tecC(i)),'Chiller'
  enddo
  write(u,*)
  write(u,*)
  t(0) = 0.d0
  do i=1,nTime
     t(i) = t(i-1) + dt(i-1)/3.6e3
     write(u,'(ES8.2E2,5X)', advance='no') t(i)
     do j=1,nm
        k = setPoint(i,j)
        c(i,j) = sp(k,j)
        write(u,'(F4.2,5X)', advance='no') c(i,j)
     enddo
     write(u,*)
  enddo

  if(writePower) then
     u = u + 1
     call prepareFile(u,'powerProduction','Results')
     write(u,*) '# powerProduction.dat, contains the power production for each'
     write(u,*) '# vector (electricity, thermal energy, and, chilling energy) and primary energy input'
     write(u,*) '#--------------------------------------------------------------------------#'
     write(u,*)
     write(u,'(A8,2X,A16,2X,A16,2X,A16,2X,A16)') 'Time [h]', 'Electricity [kW]', 'Thermal [kW]', 'Chilling [kW]', 'Input [kW]'
     do i=1,nTime
        write(u,'(ES8.2E2,5X)', advance='no') t(i)
        kk = setPoint(i,:)
        write(u,'(4ES15.3E2,3X)'), elProd(kk), thProd(kk), chProd(kk),sum(fuelCons(kk))
     enddo
  endif

  if(writeEnergy) then
     u = u + 1
     call prepareFile(u,'energyProduction','Results')
     write(u,*) '# energuProduction.dat, contains the energy production for each'
     write(u,*) '# vector (electricity, thermal energy, and, chilling energy) and primary energy input'
     write(u,*) '#--------------------------------------------------------------------------#'
     write(u,*)
     write(u,'(A8,2X,A16,2X,A16,2X,A16,2X,A16)') 'Time [h]', 'Electricity [kJ]', 'Thermal [kJ]', 'Chilling [kJ]', 'Input [kJ]'
     do i=1,nTime
        write(u,'(ES8.2E2,5X)', advance='no') t(i)
        kk = setPoint(i,:)
        write(u,'(3ES15.3E2,3X,ES20.3E3)'), elProd(kk)*dt(i), thProd(kk)*dt(i), chProd(kk)*dt(i),sum(fuelCons(kk))*dt(i)
     enddo
  endif

  if(writeEfficiency) then
     u = u + 1
     call prepareFile(u,'efficiency','Results')
     write(u,*) '# efficiency.dat, contains the efficincy at the optimal set-point for each'
     write(u,*) '# equipment and time-step.   '
     write(u,*) '#--------------------------------------------------------------------------#'
     write(u,*)
     write(u,'(A8,2X)', advance='no') 'Time [h]'
     do i=1,nTrig
        write(u,'(13X,A5,15X)', advance='no') trim(tecT(i))
     enddo
     do i=1,nBoi
        write(u,'(A5,1X,A6,5X)', advance='no') trim(tecB(i)),'Boiler'
     enddo
     do i=1,nChi
        write(u,'(A,1X,A,5X)', advance='no') trim(tecC(i)),'Chiller'
     enddo
     write(u,*)
     write(u,'(5X)',advance='no')
     do i=1,nTrig
        write(u,'(3X,3A10)', advance='no') 'Electric', 'Thermal', 'Chilling'
     enddo
     write(u,*)
     do i=1,nTime
        write(u,'(ES8.2E2,5X)', advance='no') t(i)
        do j=is(iT),ie(iT)
           k = setPoint(i,j)
           write(u,'(F6.2,4X,F6.2,4X,F6.2,5X)', advance='no') etaEl(k,j), etaTh(k,j), etaCh(k,j)
        enddo
        do j=is(iB),ie(iB)
           k = setPoint(i,j)
           write(u,'(F4.2,10X)', advance='no') etaTh(k,j)
        enddo
        do j=is(iC),ie(iC)
           k = setPoint(i,j)
           write(u,'(F4.2,10X)', advance='no') etaCh(k,j)
        enddo
       write(u,*)
     enddo
  endif
  
  if(writeElectricRev) then
     u = u + 1
     call prepareFile(u,'electricRevenues','Results')
     write(u,*) '# electricRevenues.dat, contains the revenues from electric energy selling.'
     write(u,*) '#--------------------------------------------------------------------------#'
     write(u,*)
     n = size(uEl,2)
     write(u,'(A8,7X)', advance='no') 'Time [h]'
     do i=1,n
        write(u,'(A6,1X,I3.3,7X)', advance='no') 'Client', i
     enddo
     write(u,'(A8,10X,A8,2X)') 'Grid Sell', 'Grid Buy'
     write(u,*)
     do i=1,nTime
        write(u,'(ES8.2E2,2X)', advance='no') t(i)
        kk = setPoint(i,:)
        do j=1,n
           g= uEl(i,j)*cEl(i,j)*dt(i)
           write(u,'(ES15.3,2X)',advance='no') g
        enddo
        gGrid = gridEconomy(kk,i)
        write(u,'(ES15.3,2X)',advance='no') gGrid(1)
        write(u,'(ES15.3,2X)') gGrid(2)
     enddo
  endif

  if(writeThermalRev) then
     u = u + 1
     call prepareFile(u,'thermalRevenues','Results')
     write(u,*) '# thermalRevenues.dat, contains the revenues from thermal energy selling.'
     write(u,*) '#--------------------------------------------------------------------------#'
     write(u,*)
     n = size(uTh,2)
     write(u,'(A8,7X)', advance='no') 'Time [h]'
     do i=1,n
        write(u,'(A6,1X,I3.3,7X)', advance='no') 'Client', i
     enddo
     write(u,*)
     write(u,*)
     do i=1,nTime
        write(u,'(ES8.2E2,2X)', advance='no') t(i)
        kk = setPoint(i,:)
        do j=1,n
           g= uTh(i,j)*cTh(i,j)*dt(i)
           write(u,'(ES15.3,2X)',advance='no') g
        enddo
        write(u,*)
     enddo
  endif
  do i=500,u
     close(i)
  enddo

  if(writeChillingRev) then
     u = u + 1
     call prepareFile(u,'chillingRevenues','Results')
     write(u,*) '# chillingRevenues.dat, contains the revenues from thermal energy selling.'
     write(u,*) '#--------------------------------------------------------------------------#'
     write(u,*)
     n = size(uCh,2)
     write(u,'(A8,7X)', advance='no') 'Time [h]'
     do i=1,n
        write(u,'(A6,1X,I3.3,7X)', advance='no') 'Client', i
     enddo
     write(u,*)
     write(u,*)
     do i=1,nTime
        write(u,'(ES8.2E2,2X)', advance='no') t(i)
        do j=1,n
           g= uCh(i,j)*cCh(i,j)*dt(i)
           write(u,'(ES15.3,2X)',advance='no') g
        enddo
        write(u,*)
     enddo
  endif

  if(writeDemand) then
     u = u + 1
     call prepareFile(u,'energyDemand','Results')
     write(u,*) '# energyDemand.dat, contains the energy demand for each vector           '
     write(u,*) '#--------------------------------------------------------------------------#'
     write(u,*)
     write(u,'(A8,2X,A16,2X,A16,2X,A16,2X,A16)') 'Time [h]', 'Electricity [kW]', 'Thermal [kW]', 'Chilling [kW]'
     write(u,*)
     do i=1,nTime
        write(u,'(ES8.2E2,2X)', advance='no') t(i)
        write(u,'(3ES15.3E2,3X,ES20.3E3)'), sum(uEl(i,:)), sum(uTh(i,:)), sum(uCh(i,:))
     enddo
  endif

  if(writeInput) then
     u = u+1
     call prepareFile(u,'inputEnergy','Results')
     write(u,*) '# inpuEnergy.dat, contains the energy consumption for each '
     write(u,*) '# equipment and time-step'
     write(u,*) '#--------------------------------------------------------------------------#'
     write(u,*)
     write(u,'(A8,2X)', advance='no') 'Time [h]'
     do i=1,nTrig
        write(u,'(A5,2X,A4,2X)', advance='no') trim(tecT(i)), '[kW]'
     enddo
     do i=1,nBoi
        write(u,'(A5,1X,A12,2X)', advance='no') trim(tecB(i)),'Boiler [kW]'
     enddo
     do i=1,nChi
     write(u,'(A,1X,A,2X)', advance='no') trim(tecC(i)),'Chiller [kW]'
     enddo
     write(u,*)
     write(u,*)
     do i=1,nTime
        write(u,'(ES8.2E2,2X)', advance='no') t(i)
        kk = setPoint(i,:)
        gg  = energyInput(kk)
        do j=1,nm
           write(u,'(ES8.2E2,10X)', advance='no') gg(j)
        enddo
        write(u,*)
     enddo
  endif

  if(writeCosts) then
     u = u + 1
     call prepareFile(u,'costs','Results')
     write(u,*) '# cost.dat, contains all the costs'
     write(u,*) '# equipment and time-step'
     write(u,*) '#--------------------------------------------------------------------------#'
     write(u,*)
     write(u,'(A8,2X,A16,2X,A16,2X,A16,2X,A16)') 'Time [h]', 'Fuel [€]', 'Maintenance [€]', 'OnOff [€]'
     write(u,*)
     do i=1,nTime
        write(u,'(ES8.2E2,2X)', advance='no') t(i)
        kk = setPoint(i,:)
        kko = setPoint(i-1,:) 
        write(u,'(3ES15.2E2)') fuelCost(kk,i),maintenanceCost(kk,i), fireCost(kk, kko)
     enddo
  endif

  do i=500,u
     close(i)
  enddo
  100 format('Results_',I3.3)
end subroutine output

!========================================================================================

subroutine prepareFile(u,nome, folder)

   implicit none

   integer, intent(in) :: u
   character(len=*), intent(in) :: nome, folder
   character(len=100) :: filename

   filename = trim(folder)//'/'//trim(nome)//'.dat'
   open(unit=u,file=trim(filename))

   write(u,*) '#--------------------------------------------------------------------------#'
   write(u,*) '#                                                                          #'
   write(u,*) '#                   PowerManager output file                               #'
   write(u,*) '#                                                                          #'
   write(u,*) '#--------------------------------------------------------------------------#'


end subroutine prepareFile
