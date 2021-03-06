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
!>\file warning.f90
!>\brief prints warnings to standard output
!>\author 
!>     Andrea Facci.
!
!---------------------------------------------------------------------------

!>\brief print warnings to standard output
!>\details print warnings to standard output
!>\author Andrea Facci
!>\param[in] i,j,k error codes
!>\param[in] line  line to locate the error position
!>\param[in] word  mispelled or unrecognized word
!>\param[in] r1,r  real numbers for unexpected values.

subroutine warning(i,j,k,line,word,r1,r2)

!---Declare Unit usage---
use inputVar
use shared

implicit none

!---Declare Local Variables---
integer, intent(in) ,optional          :: i,j,k
integer, intent(in), optional          :: line
character(len=*), intent(in), optional :: word
real(kind = prec), intent(in), optional :: r1,r2
character(len=18), dimension(21)        :: general
character(len=18), dimension(17)       :: trigeneration
character(len=18), dimension(15)       :: boilers
character(len=18), dimension(13)       :: Chiller
character(len=24), dimension(6)        :: files
character(len=13), dimension(3)        :: equip
character(len=17), dimension(13)       :: output
character(len=18), dimension(5)        :: ThStorage
integer                                :: l

print*
print*,'!---------------------------Warning-------------------------------------------------!'
print*

general(1:8)  = (/'GridConnection    ', 'Degradation       ', 'objective         ', 'StartPoint        '  &
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
                  './Input/Loads.inp        ',             & 
                  './Input/ThermalStorage   ' /)
equip         = (/'Trigenerator ', 'Boiler       ', 'Chiller      '/)

output        = (/'writePower      ','writeEnergy     ','writeEfficiency ','writeElectricRev', &
                  'writeThermalRev ','writeChillingRev','writeDemand     ','writeInput      ', &   
                  'writeCosts      ','writeTrig       ','writeChiller    ','writeBoiler     ', &
                  'global          '/)

thStorage     = (/'Power             ', 'Capacity          ', 'SetPoint          ', 'InputEfficiency   ', 'OutputEfficiency  '/)

general(9:21) = output



select case(i)
    case(0)
        continue
    case(1) 
        print*, ' Unrecognized entry in file "', trim(files(j)), '" at line', line
        print*, ' Found "', trim(word), '" recognized entry are: '
        select case(j)
            case(1)
                write(*,'(5x,A18)')  (general(l), l=1,size(general,1))
            case(2)
                write(*,'(5x,A18)')  (trigeneration(l),l=1,size(trigeneration,1))
            case(3)
                write(*,'(5x,A18)')  (boilers(l), l=1,13)
            case(4)
                write(*,'(5x,A18)')  (Chiller(l), l=1,12)
            case(6)
                write(*,'(5x,A18)')  (ThStorage(l), l=1,12)
        end select
        print*, ' Note that the code is case-sensitive'
    case(2,3)
        print*,'The ', trim(word), ' Efficiency  is extrapolated for one or more  &
                set-points for ', trim(equip(j)), ' number', k    
        select case(i)
            case(2)
                print*,  ' Because maximum set point is grater than the &
                corresponding value in the efficiency table'
            case(3)
                print*,  ' Because minimum set point is lower than the &
                corresponding value in the efficiency table'
        end select
   case(4) 
       print*, 'skipping blank keyword on line ', line,' , of file', trim(files(j))
   case(5)
       print*, 'Removing the "Results" folder according to -overwrite option.'
       print*, 'All the old results will be lost forever.'
   case(6)
       print*, 'The existing "Results" folder will be moved to ', trim(word)
   case(7)
       print*, 'Cannot find "', trim(output(j)), '" in file "General.inp". Assuming "', trim(output(j)), '" = .false. '
end select
print*
print*,'!-----------------------------------------------------------------------------------!'

end subroutine warning
