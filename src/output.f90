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

subroutine output(setPoint,postProcessing,path)

!---Declare Module usage---
  use shared
  use inputvar
  use cmdVar , only : out
  use interfaces
  use plantVar
  use energy
  use economy
  use globalResults


!---Declare Local Variables---
  implicit none

  integer, dimension(0:nTime+1,nm), intent(in) :: setPoint
  logical                         , intent(in) :: postProcessing
  character(len=*)                , intent(in) :: path
  integer                                      :: i, u,j,k,n,l, nO
  real(kind = prec ), dimension(nTime,nm)        :: c
  integer,          dimension(nm)              :: kk, kko
  logical                                      :: ex, oldRes
  character(len=100)                           :: folder, filename
  real(kind = prec ), dimension(0:nTime)         :: t
  real(kind = prec), dimension(nm)              :: gg
  real(kind = prec)                             :: g
  real(kind = prec),dimension(2)                :: gGrid
  character(len=20), dimension(100)            :: buffer20
  real(kind = prec), dimension(100)             :: rbuffer
  integer                                       :: stat

!---Function body---
  
  inquire(file='Results/.', exist=oldRes)
  if(.not.postProcessing) then
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
     call system('mkdir -v Results', status=stat)
  endif

  t(0) = zero
  do i=1,nTime
     t(i) = t(i-1) + dt(i-1)/3.6e3
     do j=1,nm
        k = setPoint(i,j)
        c(i,j) = sp(k,j)
     enddo
  enddo

  u = 500
  if(.not.postProcessing) then
    call prepareFile(u,'setPoint',path)
    write(u,*) '# setPoint.dat, contains the optimal set point for all the'
    write(u,*) '# equipment and time-step'
    write(u,*) '#--------------------------------------------------------------------------#'
    write(u,*)
    write(u,'(A8,2X)', advance='no') 'Time [h]'
    do i=1,nTrig
       j = is(iT) + i - 1
       write(u,'(A5,2X)', advance='no') trim(tec(j))
    enddo
    do i=1,nBoi
       j = is(iB) + i - 1
       write(u,'(A5,1X,A6,2X)', advance='no') trim(tec(j)),'Boiler'
    enddo
    do i=1,nChi
       j = is(iC) + i - 1
       write(u,'(A,1X,A,2X)', advance='no') trim(tec(j)),'Chiller'
    enddo
    write(u,*)
    write(u,*)
    t(0) = 0.d0
    do i=1,nTime
       write(u,'(ES8.2E2,5X)', advance='no') t(i)
       do j=1,nm
          write(u,'(F4.2,5X)', advance='no') c(i,j)
       enddo
       write(u,*)
    enddo
  endif

  if(writePower) then
     u = u + 1
     call prepareFile(u,'powerProduction',path)
     write(u,*) '# powerProduction.dat, contains the power production for each'
     write(u,*) '# vector (electricity, thermal energy, and, chilling energy) and primary energy input'
     write(u,*) '#--------------------------------------------------------------------------#'
     write(u,*)
     buffer20(1) = 'Time [h]            '
     buffer20(2) = 'Electricity [kW]    '
     buffer20(3) = 'Thermal  [kW]       '
     buffer20(4) = 'Chilling [kW]       '
     buffer20(5) = 'Input [kW]          '
     write(u,'(5A)') (buffer20(i), i=1,5)
     write(u,*)
     do i=1,nTime
        kk = setPoint(i,:)
        write(u,'(5(ES9.2E2,11X))') t(i), elProd(kk,i), thProd(kk,i), chProd(kk,i), sum(fuelCons(kk,i))
     enddo
  endif

  if(writeEnergy) then
     u = u + 1
     call prepareFile(u,'energyProduction',path)
     write(u,*) '# energuProduction.dat, contains the energy production for each'
     write(u,*) '# vector (electricity, thermal energy, and, chilling energy) and primary energy input'
     write(u,*) '#--------------------------------------------------------------------------#'
     write(u,*)
     buffer20(1) = 'Time [h]            '
     buffer20(2) = 'Electricity [kJ]    '
     buffer20(3) = 'Thermal  [kJ]       '
     buffer20(4) = 'Chilling [kJ]       '
     buffer20(5) = 'Input [kJ]          '
     write(u,'(5A)') (buffer20(i), i=1,5)
     write(u,*)
     do i=1,nTime
        kk = setPoint(i,:)
        write(u,'(5(ES9.2E2,11X))') t(i), elProd(kk,i)*dt(i), thProd(kk,i)*dt(i), chProd(kk,i)*dt(i),sum(fuelCons(kk,i))*dt(i)
     enddo
  endif

  if(writeEfficiency) then
     u = u + 1
     call prepareFile(u,'efficiency',path)
     write(u,*) '# efficiency.dat, contains the efficincy at the optimal set-point for each'
     write(u,*) '# equipment and time-step.   '
     write(u,*) '#--------------------------------------------------------------------------#'
     write(u,*)
     write(u,'(A8,2X)', advance='no') 'Time [h]'
     do i=1,nTrig
        j = is(iT) + i - 1
        write(u,'(13X,A5,15X)', advance='no') trim(tec(j))
     enddo
     do i=1,nBoi
        j = is(iB) + i - 1
        write(u,'(A5,1X,A6,5X)', advance='no') trim(tec(j)),'Boiler'
     enddo
     do i=1,nChi
        j = is(iC) + i - 1
        write(u,'(A,1X,A,5X)', advance='no') trim(tec(j)),'Chiller'
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
     call prepareFile(u,'electricRevenues',path)
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
     call prepareFile(u,'thermalRevenues',path)
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
     call prepareFile(u,'chillingRevenues',path)
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
     call prepareFile(u,'energyDemand',path)
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
     call prepareFile(u,'inputEnergy',path)
     write(u,*) '# inpuEnergy.dat, contains the energy consumption for each '
     write(u,*) '# equipment and time-step'
     write(u,*) '#--------------------------------------------------------------------------#'
     write(u,*)
     write(u,'(A8,2X)', advance='no') 'Time [h]'
     do i=1,nTrig
        j = is(iT) + i - 1
        write(u,'(A5,2X,A4,2X)', advance='no') trim(tec(j)), '[kW]'
     enddo
     do i=1,nBoi
        j = is(iB) + i - 1
        write(u,'(A5,1X,A12,2X)', advance='no') trim(tec(j)),'Boiler [kW]'
     enddo
     do i=1,nChi
        j = is(iC) + i - 1
        write(u,'(A,1X,A,2X)', advance='no') trim(tec(j)),'Chiller [kW]'
     enddo
     write(u,*)
     write(u,*)
     do i=1,nTime
        write(u,'(ES8.2E2,2X)', advance='no') t(i)
        kk = setPoint(i,:)
        gg  = energyInput(kk,i)
        do j=1,nm
           write(u,'(ES8.2E2,10X)', advance='no') gg(j)
        enddo
        write(u,*)
     enddo
  endif

  if(writeCosts) then
     u = u + 1
     call prepareFile(u,'costs',path)
     write(u,*) '# cost.dat, contains all the costs'
     write(u,*) '# equipment and time-step'
     write(u,*) '#--------------------------------------------------------------------------#'
     write(u,*)
     write(u,'(A8,2X,A16,2X,A17,2X,A16,2X,A16)') 'Time [h]', 'Fuel [€]', 'Maintenance [€]', 'OnOff [€]'
     write(u,*)
     do i=1,nTime
        write(u,'(ES8.2E2,2X)', advance='no') t(i)
        kk = setPoint(i,:)
        kko = setPoint(i-1,:) 
        write(u,'(3ES15.2E2)') fuelCost(kk,i),maintenanceCost(kk,i), fireCost(kk, kko)
     enddo
  endif

  if(writeTrig) then
     call system("mkdir "//trim(path)//"/Trigenerative")
     k = is(iT)
     do j=1,nTrig
        u = u + 1
        write(filename, '(A,I3.3)'),trim(tec(j)),j
        call prepareFile(u,filename,'Results/Trigenerative')
        write(u,*) '# ', trim(filename),'.dat, contains detailed informations about trigenerative equipment'
        write(u,*) '#--------------------------------------------------------------------------#'
        write(u,*)
        buffer20(1) = 'Time [h]            '
        buffer20(2) = 'Set-point           '
        buffer20(3) = 'Electrical P. [kW]  '
        buffer20(4) = 'Thermal P.  [kW]    '
        buffer20(5) = 'Chilling P. [kW]    '
        buffer20(6) = 'eta El              '
        buffer20(7) = 'eta Th              '
        buffer20(8) = 'eta Ch              '
        buffer20(9) = 'fuel cons. [kW]     '
        buffer20(10)= 'fuel cons. [kg/s]   '
        buffer20(11)= 'fuel cost [€]       '
        buffer20(12)= 'maint. cost [€]     '
        buffer20(13)= 'on-off cost [€]     '
        write(u,'(13A)') (buffer20(i), i=1,13)
        write(u,*)
        do i=1,nTime
           n  = setPoint(i,k)
           nO = setPoint(i-1,k) 
           rbuffer(1) = (t(i))
           rbuffer(2) = sp(n,k)
           call performances(n, nO,'Trigeneration', j,i, rbuffer(3), rbuffer(4), rbuffer(5), rbuffer(9), & 
                rbuffer(10), rbuffer(11),rbuffer(12),rbuffer(13))
           rbuffer(6) = etaEl(n,k)
           rbuffer(7) = etaTh(n,k)
           rbuffer(8) = etaCh(n,k)
           write(u,'(13(ES9.2E2,11X))') (rbuffer(l), l=1,13)
        enddo
        k = k + 1
     enddo
  endif

  if(writeBoi) then
     call system("mkdir "//trim(path)//"/Boilers")
     k = is(iB)
     do j=1,nBoi
        u = u + 1
        write(filename, '(A,I3.3)'),'Boiler',j
        call prepareFile(u,filename,'Results/Boilers')
        write(u,*) '# ', trim(filename),'.dat, contains detailed informations about Boilers'
        write(u,*) '#--------------------------------------------------------------------------#'
        write(u,*)
        buffer20(1) = 'Time [h]            '
        buffer20(2) = 'Set-point           '
        buffer20(3) = 'Thermal P.  [kW]    '
        buffer20(4) = 'eta Th              '
        buffer20(5) = 'fuel cons. [kW]     '
        buffer20(6) = 'fuel cons. [kg/s]   '
        buffer20(7) = 'fuel cost [€]       '
        buffer20(8) = 'maint. cost [€]     '
        buffer20(9) = 'on-off cost [€]     '
        write(u,'(9A)') (buffer20(i), i=1,9)
        write(u,*)
        do i=1,nTime
           n  = setPoint(i,k)
           nO = setPoint(i-1,k) 
           rbuffer(1) = (t(i))
           rbuffer(2) = sp(n,k)
           call performances(n, nO,'Boilers', j,i, pTh = rbuffer(3), eIn = rbuffer(5), mf = rbuffer(6), & 
                             cfu = rbuffer(7),cm = rbuffer(8), cOn = rbuffer(9))
           rbuffer(4) = etaTh(n,k)
           write(u,'(13(ES9.2E2,11X))') (rbuffer(l), l=1,9)
        enddo
     enddo
     k = k + 1
  endif

  if(writeChi) then
     call system("mkdir "//trim(path)//"/Chillers")
     k = is(iC)
     do j=1,nBoi
        u = u + 1
        write(filename, '(A,I3.3)'),'Chiller',j
        call prepareFile(u,filename,'Results/Chillers')
        write(u,*) '# ', trim(filename),'.dat, contains detailed informations about Boilers'
        write(u,*) '#--------------------------------------------------------------------------#'
        write(u,*)
        buffer20(1) = 'Time [h]            '
        buffer20(2) = 'Set-point           '
        buffer20(3) = 'Chilling P. [kW]    '
        buffer20(4) = 'eta Ch              '
        buffer20(5) = 'input Energy[kW]    '
        buffer20(6) = 'maint. cost [€]     '
        buffer20(7) = 'on-off cost [€]     '
        write(u,'(7A)') (buffer20(i), i=1,9)
        write(u,*)
        do i=1,nTime
           n  = setPoint(i,k)
           nO = setPoint(i-1,k) 
           rbuffer(1) = (t(i))
           rbuffer(2) = sp(n,k)
           call performances(n, nO,'Chiller', j,i, pCh = rbuffer(3), eIn = rbuffer(5), cm = rbuffer(6), cOn = rbuffer(6))
           rbuffer(4) = etaCh(n,k)
           write(u,'(13(ES9.2E2,11X))') (rbuffer(l), l=1,9)
        enddo
     enddo
     k = k + 1
  endif

  if(global) then
     u = u + 1
     call prepareFile(u,'global','Results')
     write(u,*) '# global.dat, contains global results '
     write(u,*) '#--------------------------------------------------------------------------#'
     write(u,*)
     
     write(u,*) '---------------Revenues---------------------------------'
     write(u,*) 'Electric Revenues: ' , globElRev(setPoint),'€'
     write(u,*) 'Thermal Revenues : ' , globThRev(setPoint),'€'
     write(u,*) 'Chilling Revenues: ' , globChRev(setPoint),'€'
     write(u,*) 'Total Revenues   : ' , globRevenues(setPoint),'€'

     write(u,*) '---------------Costs---------------------------------'
     write(u,*) 'Fuel Costs       : ' , globFuelCost(setPoint),'€'
     write(u,*) 'Maintenace Costs : ' , globMaintCost(setPoint),'€'
     write(u,*) 'On-Off Costs     : ' , globFireCost(setPoint),'€'
     write(u,*) 'Totat Costs      : ' , globCost(SetPoint),'€'

     write(u,*) '---------------Profits---------------------------------'
     write(u,*) 'Profits          : ' , globProfit(setPoint),'€'

     if(writePec) then 
        write(u,*) '--------------Primary ebergy consumption----------------------'
        write(u,*) 'Pec              : ' , globPec(setPoint),'kJ'
     endif
  endif

  if(writePec) then
     u = u + 1
     call prepareFile(u,'pec',path)
     write(u,*) '# pec.dat: primary energy consumption.     '
     write(u,*) '#--------------------------------------------------------------------------#'
     write(u,*)
     buffer20(1) = 'Time [h]            '
     buffer20(2) = 'Pec  [kJ]           '
     write(u,'(7A)') (buffer20(i), i=1,2)
     do i=1,nTime
        kk = setPoint(i,:)
        kko = setPoint(i-1,:) 
        rbuffer(1) = (t(i))   
        rbuffer(2) = pec(kk,i) + pecPenalty(kk,kkO,i)
        write(u,'(2(ES9.2E2,11X))') (rbuffer(l), l=1,9)
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
