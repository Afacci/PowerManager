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
!>\file readBoiler.f90
!>\brief Reads Boiler.inp file
!>\author 
!>     Andrea Facci.
!
!---------------------------------------------------------------------------

!>\brief Reads Photovoltaic.inp file
!>\details 
!> This subroutine reads the file "Photovoltaic.inp". The procedure looks for each specific entry
!> in the "keyword field", and associates the value in the "value" field, to the
!> corresponding variable. If the desired entry is not present returns an error
!> message and aborts the execution. The structure of the input file is
!> clarified in the following example along with the meaning of "keyword" and
!> "value" field.\n
!> \verbatim
!> A Lot of usless stuff because only text between begin and end is read.
!>    begin 
!>       !Commented line
!>       KeywordField  | ValueField | !Comment two
!>
!>       KeywordField  | ValueField |
!>       ScalarValue   | 1          |
!>       Vector        | 1 2 n      |
!>       VectorSeries  |(a b c ) (d e f)|
!>       Matrix        |(11 12 13  |
!>                     | 21 22 23  |
!>                     | 31 32 33) |
!>    end
!> A lof really usless text
!>\endverbatim
!> Note that only the text between "begin" and "end" is read. Blank lines are
!> automatically discarded, while any unrecognized entry is discarded returning
!> a warning code. Line beginning with "!" are considered comments and
!> discarded.
!>\author 
!>     Andrea Facci.

subroutine readPV

!---Declare Unit usage---
use shared
use inputVar
use fileTools
use interfaces
use cmdVar
use myArithmetic

implicit none

!---Declare Local Variables---
integer                :: genUnit = 112
character(len=50)      :: inputFile = './Input/Photovoltaic.inp'
logical                :: filePresent
character(len=500)     :: buffer, keyword, value 
integer                :: firstLine, line, i, nInp, nl
logical,dimension(8)   :: isPresent = .false.
integer                :: error
integer                :: nb, nd

character(len=100), dimension(100)             :: dummy
real(kind = prec), allocatable, dimension(:,:) :: matrix

!---Check File Presence---
inquire(file = inputFile, exist = filePresent)
if(.not.filePresent) then
   call abortExecution(1,8)
else
   open(unit = genUnit, file = inputFile)
endif

!---Skip all the lines befor the keyword "begin".
firstLine = 0;
do 
    read(genUnit,100) buffer
    buffer = adjustl(buffer)
    firstLine = firstLine + 1
    if(buffer(1:5).eq.'begin') exit
enddo

line = firstLine

!buffer = 'Model'
!call cFindEntry(buffer,1,genUnit,.true.,dummy(1),isPresent(6))
!ModelPV = dummy(1)
!if(.not.isPresent(6)) call abortExecution(7,6)


!---read the input list---
do 
    call readKeyword(genUnit,.false., keyword,value,error, nl)
    if (error.eq.1) call abortExecution(0,3)
    line = line + nl
    select case(keyword)
       case('Model')
         continue
       case('end')
          exit
       case('Surface')
          read(value,*) surfPV
          isPresent(1) = .true.
       case('Efficiency')
          read(value,*) etaPV
          isPresent(2) = .true.
       case('Slope')
          read(value,*) slopePV
          isPresent(3) = .true.
       case('Orientation')
          read(value,*) azimutPV
          isPresent(4) = .true.
       case('CutOff')
          read(value,*) cutOffPV
          isPresent(5) = .true.
       case('AuxEfficiency')
          read(value,*) etaAuxPV
          isPresent(7) = .true.
       case('TemperatureDerating')
          isPresent(6) = .true.
          backspace(genUnit)
          line = line - 1
          nTPV= vCount(genUnit,.false.)
          call allocateVar(33)
          call rewUnit(genUnit,nTPV)
          pvCorr(:,:) =  dmatrixRead(genUnit,nTPV,2)
          line = line + nTPV  
       case('Reflection')
          read(value,*) rhoPV
          isPresent(8) = .true.
       case(' ') 
          if(verb) call warning(4,3,line=line)
       case default
          if(.not.silent) call warning(1,7,line=line,word=keyword)
    end select
enddo


!---check if all the variablea were read---
nInp = size(isPresent)
do i = 1,nInp
    if(.not.isPresent(i)) call abortExecution(32,i)
enddo


close(genUnit)
100 format(A500)

end subroutine readPV
