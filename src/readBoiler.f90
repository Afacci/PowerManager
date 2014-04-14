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

!>\brief Reads Boiler.inp file
!>\details 
!> This subroutine reads the file "Boilers.inp". The procedure looks for each specific entry
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

subroutine readBoiler

!---Declare Unit usage---
use shared
use inputVar
use fileTools
use interfaces
use cmdVar
use myArithmetic

implicit none

!---Declare Local Variables---
integer              :: genUnit = 107
character(len=50)    :: inputFile = './Input/Boilers.inp'
logical              :: filePresent
character(len=500)   :: buffer, keyword, value,vector,elements,value_
integer              :: firstLine, line, i, nInp, nRow, n1, n2, x, j, il, nl
logical,dimension(15)  :: isPresent = .false.
integer,dimension(100) :: dummy
integer              :: error
character(len=20), dimension(3) :: param
integer :: n, nt, nee, net, nec, np
real(kind = prec), allocatable, dimension(:,:) :: matrix

!---Check File Presence---
inquire(file = inputFile, exist = filePresent)
if(.not.filePresent) then
   call abortExecution(1,3)
else
   open(unit = genUnit, file = inputFile)
endif

isPresent(3) = .true.
isPresent(6) = .true.
isPresent(7) = .true.
isPresent(11) = .true.
!---Skip all the lines befor the keyword "begin".
firstLine = 0;
do 
    read(genUnit,100) buffer
    buffer = adjustl(buffer)
    firstLine = firstLine + 1
    if(buffer(1:5).eq.'begin') exit
enddo

!---Look for the the "Number" entry that is neede for most of the other entries---
buffer = 'Number'
call iFindEntry(buffer,1,genUnit,.true.,dummy(1),isPresent(1))
nBoi = dummy(1)
if(nBoi.eq.0) return
if(.not.isPresent(1)) call abortExecution(5,1)
call allocateVar(7)

line = firstLine
!---read the input list---
do 
    call readKeyword(genUnit,.false., keyword,value,error, nl)
    if (error.eq.1) call abortExecution(0,3)
    line = line + nl
    select case(keyword)
       case('end')
          exit
       case('Power')
          read(value,*) (pMaxB(i), i=1,nBoi)
          isPresent(2) = .true.
       case('Priority')
          read(value,*) (BoiPriority(i), i=1,nBoi)
          iPrio(2) = .true.
!       case('DegradationRate')
!          read(value,*) (degRateB(i), i=1,nBoi)
!          isPresent(3) = .true.
       case('FuelCost')
          read(value,*) (fuelCostB(i), i=1,nBoi)
          isPresent(4) = .true.
       case('FuelLHV')
          read(value,*) (fuelLHVB(i), i=1,nBoi)
          isPresent(5) = .true.
!       case('Investment')
!          read(value,*) (invB(i), i=1,nBoi)
!          isPresent(6) = .true.
!       case('Lifetime')
!          read(value,*) (lifeB(i), i=1,nBoi)
!          isPresent(7) = .true.
       case('OnOffCost')
          read(value,*) (fireCostB(i), i=1,nBoi)
          isPresent(8) = .true.
       case('OeMCost')
          read(value,*) (maintCostB(i), i=1,nBoi)
          isPresent(9) = .true.
       case('SetPoint')
          isPresent(10) = .true.
          value_ = value
          do i=1,nBoi
              nSpB(i) = hCount(value_)
              n2 = index(value_,')') + 1
              value_ =  value_(n2 + 1:)
          enddo
          call allocateVar(8)
          do i = 1, nBoi
             n1 = index(value,'(') + 1
             n2 = index(value,')') - 1
             vector = trim(value(n1:n2))
             read(vector,*) (spB(j,i), j=1,nSpB(i))
             value =  value(n2 + 2:)
          enddo
!       case('Size')
!           value_ = value
!           isPresent(11) = .true.
!           do i=1,nBoi
!               nSizeB(i) = hCount(value_)
!               n2 = index(value_,')') + 1
!               value_ =  value_(n2 + 1:)
!           enddo
!           call allocateVar(9)
!           do i = 1, nBoi
!              n1 = index(value,'(') + 1
!              n2 = index(value,')') - 1
!              vector = trim(value(n1:n2))
!              read(vector,*) (kSizeB(i,j), j=1,nSizeB(i))
!              value =  value(n2 + 2:)
!           enddo
       case('ThermalEfficiency')
              isPresent(12) = .true.
              backspace(genUnit)
              line = line - 1
              do i = 1,nBoi
                  nEtaB(i) = vCount(genUnit,.false.)
              enddo
              j = sum(nEtaB)
              call allocateVar(10)
              call rewUnit(genUnit,j)
              do i = 1, nBoi
                 etaB(:,:,i) =  dmatrixRead(genUnit,nEtaB(i),2)
              enddo
              line = line + j
       case('Number')
             continue
       case('HeatSource')
             isPresent(13) = .true.
             read(value,*) (tecB(i), i=1,nBoi)
       case('MinUpTime')
             isPresent(14) = .true.
             read(value,*) (minUpTimeB(i), i=1,nBoi)
       case('MinDownTime')
             isPresent(15) = .true.
             read(value,*) (minDownTimeB(i), i=1,nBoi)
       case('TempCorrection')
            read(value,*) (param(i), i=1,3)
            do i=1,3
               select case(param(i))
                  case('temp')
                     nt = i
                  case('eta')
                     net = i
                  case('pmax')
                     np = i
               end select
            enddo
            do i=1,nBoi
               ntcB(i) = vCount(genUnit,.false.)
            enddo
            call allocateVar(26,maxval(ntcB))
            allocate(matrix(maxval(ntcB),3))
            n = sum(ntcB)
            call rewUnit(genUnit,n)
            do i=1,nBoi
               matrix = rNaN(rVal)
               matrix = dmatrixRead(genUnit,ntcB(i),3)
               tempCorrB(:,1,i) = matrix(:,nt)
               tempCorrB(:,2,i) = matrix(:,net)
               tempCorrB(:,3,i) = matrix(:,np)
            enddo
            deallocate(matrix)
       case('PresCorrection')
            read(value,*) (param(i), i=1,3)
            do i=1,3
               select case(param(i))
                  case('pres')
                     nt = i
                  case('eta')
                     net = i
                  case('pmax')
                     np = i
               end select
            enddo
            do i=1,nBoi
               npcB(i) = vCount(genUnit,.false.)
            enddo
            call allocateVar(27,maxval(npcB))
            allocate(matrix(maxval(npcB),3))
            n = sum(npcB)
            call rewUnit(genUnit,n)
            do i=1,nBoi
               matrix = rNaN(rVal)
               matrix = dmatrixRead(genUnit,npcB(i),3)
               presCorrB(:,1,i) = matrix(:,nt)
               presCorrB(:,2,i) = matrix(:,net)
               presCorrB(:,3,i) = matrix(:,np)
            enddo
            deallocate(matrix)
       case('AltCorrection')
            read(value,*) (param(i), i=1,3)
            do i=1,3
               select case(param(i))
                  case('alt')
                     nt = i
                  case('eta')
                     net = i
                  case('pmax')
                     np = i
               end select
            enddo
            do i=1,nBoi
               nacB(i) = vCount(genUnit,.false.)
            enddo
            call allocateVar(28,maxval(nacB))
            allocate(matrix(maxval(nacB),3))
            n = sum(nacB)
            call rewUnit(genUnit,n)
            do i=1,nBoi
               matrix = rNaN(rVal)
               matrix = dmatrixRead(genUnit,nacB(i),3)
               altCorrB(:,1,i) = matrix(:,nt)
               altCorrB(:,2,i) = matrix(:,net)
               altCorrB(:,3,i) = matrix(:,np)
            enddo
            deallocate(matrix)
       case('PEF')
           kPEC(3) = .true.
           read(value,*) (pefB(i), i=1,nBoi)
       case('onOffPec')
           kPEC(5) = .true.
           read(value,*) (pecOnB(i),i=1,nBoi)
       case(' ') 
            if(verb) call warning(4,3,line=line)
       case('Tin')
             read(value,*) (TinBoi(i), i=1,nBoi)
       case('Tout')
             read(value,*) (ToutBoi(i), i=1,nBoi)
       case('Condensation')
             read(value,*) (isCondensation(i), i=1,nBoi)
       case default
            if(.not.silent) call warning(1,3,line=line,word=keyword)
    end select
enddo

!---check if all the variablea were read---
nInp = size(isPresent)
do i = 1,nInp
    if(.not.isPresent(i)) call abortExecution(5,i)
enddo

close(genUnit)
100 format(A500)

end subroutine readBoiler
