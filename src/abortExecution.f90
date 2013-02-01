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
!>\file abortExecution.f90
!!\brief terminates the execution in case of error.
!!
!!    aborts the program exectuion in case of error and print the error message
!!    according to the error code given as input.
!!\author 
!!     Andrea Facci.
!
!---------------------------------------------------------------------------

!>\brief abort the program execution.
!>\details
!> aborts the program exectuion in case of error and print the error message
!> according to the error code given as input. 
!>\param[in] i,j    integers that identify the error.
!>\param[in] line   integer that identifies the line affected by the mistake in an input file
!>\param[in] word   character input to report the mispelled or unexpected sentence
!>\param[in] r1, r2 double precision reals. Useful to report incoherence between plant parameters

subroutine abortExecution(i,j,line,word,r1,r2)

!---Declare Unit usage---
use inputVar

implicit none

!---Declare Local Variables---
integer, intent(in), optional          :: i,j
integer, intent(in), optional          :: line
character(len=*), intent(in), optional :: word
real(kind(1.d0)), intent(in), optional :: r1, r2
character(len=18), dimension(8)        :: general
character(len=18), dimension(17)       :: trigeneration
character(len=18), dimension(15)       :: boilers
character(len=18), dimension(13)       :: Chiller
character(len=24), dimension(5)        :: files
character(len=11), dimension(3)        :: eLoads 
character(len=16), dimension(3)        :: gConn
character(len=8),  dimension(2)        :: algo

print*
print*, '!---------------FATAL ERROR-------------------!'
print*

general       = (/'GridConnection    ', 'Degradation       ', 'objective         ', 'StartPoint        '  &
                 ,'FirstTimeStep     ', 'UpTime0           ', 'DownTime0         ', 'Algorithm         '  /)
trigeneration = (/'Number            ', 'Power             ', 'DegradationRate   ', 'FuelCost          '  &
                 ,'FuelLHV           ', 'Investment        ', 'Lifetime          ', 'OnLifetime        '  &
                 ,'OeMCost           ', 'SetPoint          ', 'Size              ', 'ElettrEfficiency  '  &
                 ,'ThermalEfficiency ', 'ChillingEfficiency', 'Technology        ', 'MinUpTime         '  &
                 ,'MinDownTime       '/)

boilers       = (/'Number            ', 'Power             ', 'DegradationRate   ', 'FuelCost          '  &
                 ,'FuelLHV           ', 'Investment        ', 'Lifetime          ', 'OnLifetime        '  &
                 ,'OeMCost           ', 'SetPoint          ', 'Size              ', 'ThermalEfficiency '  &
                 ,'Technology        ', 'MinUpTime         ', 'MinDownTime       '/)

Chiller       = (/'Number            ', 'Technology        ', 'Power             ', 'DegradationRate   '  &
                 ,'Investment        ', 'Lifetime          ', 'OnLifetime        '                        &
                 ,'OeMCost           ', 'SetPoint          ', 'Size              ', 'Efficiency        '  &
                 ,'MinUpTime         ', 'MinDownTime       '/)

files         = (/'./Input/General.inp      ',             & 
                  './Input/Trigeneratoin.inp',             &
                  './Input/Boliers.inp      ',             &
                  './Input/Chillers.inp     ',             &
                  './Input/Loads.inp        '/)
eLoads        = (/'Electricity', 'Thermal    ', 'Chilling   '/)
gConn         = (/'NetMetering    ','DedicatadRetire','StandAlone     '/)
algo          = (/'Backward','Forward '/)
select case(i)
    case(0)
            print*,'Mispelled entry in file ', trim(files(j)), ' in line ', line 
            print*,'Missing one of "|" delimiters'
    case(1)
      select case(j)
           case(1)
               print*,'    Missing ./Input/General.inp' 
           case(2)  
               print*,'    Missing ./Input/Trigeneration.inp '
           case(3) 
               print*,'    Missing ./Input/Boilers.inp ' 
           case(4)
               print*,'    Missing ./Input/Chillers.inp ' 
           case(5)
               print*,'    Missing ./Input/Loads.inp ' 
       end select
    case(2)
       print*,' Wrong or mispelled entry '
       select case(j)
           case(1)
               print*,' Expected one of, ', gConn, ' ,found  (',  word, ')'
           case(2)
               print*,' Expected one of, ', algo, ' ,found  (',  word, ')'
           case(3)
               print*,' Expected one of ".true." or ".false.", found ',  trim(word)
       end select
           print*,' in line', line, ' of file General.inp'
    case(3:6)
        write(*,'(A)',advance='no'),'  Missing input entry: '
        select case(i)
            case(3)
                print*,'Could not find "', trim(general(j)) ,'" in General.inp'
            case(4)
                print*,'Could not find "', trim(trigeneration(j)) ,'" in Trigeneration.inp'
            case(5)
                print*,'Could not find "', trim(boilers(j)) ,'" in Boilers.inp'
            case(6)
                print*,'Could not find "', trim(chiller(j)) ,'" in Chillers.inp'
        end select
    case(7)
        print*, ' The power plant is not able to satisfie the load'
        select case(j)
            case(1) 
                print*, ' Maximum thermal power = ', r1, ' < Maximum thermal load = ', r2
                print*, ' Absorption chillers heat consumption is added to thermal load'
            case(2)
                print*, ' Maximum chilling power = ', r1, ' < Maximum chilling load = ', r2
            case(3)
                print*, ' Maximum electric power = ', r1, ' < Maximum electric load = ', r2
                print*, ' for a stant alone power plant'
                print*, ' Mechanical chillers consumption is added to electric load'
        end select               
    case(8)
        print*,'Time must be the first entry in the "Loads", "Prices", and GridPrice tables"'
    case(9)
        print*,'Wrong number of entries for ', trim(eLoads(j)), ' load at time ', r1
    case(10)
        print*,'The number of columns for the "Prices" line is different from the one for the the "Loads" line'
    case(11)
        print*,'The number of ', trim(eLoads(j)), ' prices is different from the number of ', trim(eLoads(j)), ' loads'
    case(12)
        print*,'Wrong number of entries for ', trim(eLoads(j)), ' prices at time ', r1
    case(13)
        print*,'The vector associated to " ',trim(word), ' " has wrogn size. Expected ', line, ' found ', j, ' .'
        print*,'Length{',trim(word),'} should be equal to the total number of equipments in the plant.'
    case(14)
        print*,'Unfeasible values in "StartPoint" field in "General.inp" file.'
        print*,'The initial state of the plant must be coherent with the feasible states defined for each equipment.'
    case(15)
        print*,'Incoherent initial state: ', trim(word), '(',j,') = ', r1, 'while the set point is', r2
    case default
        continue
end select
if(i.le.6) print*, ' Note that the code is case-sensitive'


print*
print*, '!---------------RUN ABORTING------------------!'
print*,

stop

end subroutine abortExecution