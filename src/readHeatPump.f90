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
!    ; if not, write to the Free Software Foundation,
!    Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA
!
!>\file readHeatPump.f90
!>\brief Reads HeatPump.inp file
!>\author 
!>     Andrea Facci.
!
!---------------------------------------------------------------------------

!>\brief Reads HeatPump.inp file
!>\details 
!> This subroutine reads the file "HeatPump.inp". The procedure looks for each specific entry
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

subroutine readHeatPump

!---Declare Unit usage---
use shared
use inputVar, only: nHP, nSpHP, nSizeHP, nEtaHP, ntcHP, npcHP, nacHP, &
                    pMaxHP , fireCostHP, maintCostHP, minUpTimeHP, minDownTimeHP, &
                    spHP, etaHP, tempCorrHP, presCorrHP, altCorrHP, tecHP, hpCorrectionP & 
                    ,envTempHP, outTempHP, nOutTHP, nTenvHP, TutileHP, hpCorrectionE
use fileTools
use interfaces
use cmdVar
use myArithmetic

implicit none

!---Declare Local Variables---
integer              :: genUnit = 208, dinte
character(len=50)    :: inputFile = './Input/HeatPump.inp'
logical              :: filePresent
character(len=500)   :: buffer, keyword, value,vector,elements,value_
integer              :: firstLine, line, nl, i, nInp, nRow, n1, n2, x, j, il
logical,dimension(13):: isPresent = .false.
character(len=100)   :: chKind(2)
integer, dimension(10) :: dummy
integer              :: error
character(len=20), dimension(3) :: param
integer :: n, nt, nee, net, nec, np
real(kind = prec), allocatable, dimension(:,:) :: matrix

!---Check File Presence---
inquire(file = inputFile, exist = filePresent)
if(.not.filePresent) then
   call abortExecution(1,11)
else
   open(unit = genUnit, file = inputFile)
endif

isPresent(4) = .true.
isPresent(5) = .true.
isPresent(6) = .true.
isPresent(10) = .true.

!---Skip all the lines before the keyword "begin".
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
nHP = dummy(1)
if(nHP.eq.0) return
if(.not.isPresent(1)) call abortExecution(6,1)
call allocateVar(41)

!---read the input list---
line = firstLine
do 
    call readKeyword(genUnit,.false., keyword,value,error,nl)
    if(error.eq.1) call abortExecution(0,11)
    line = line + nl
    select case(keyword)
       case('end')
          exit
       case('Power')
          read(value,*) (pMaxHP(i), i=1,nHP)
          isPresent(3) = .true.
!       case('Priority')
!          read(value,*) (ChiPriority(i), i=1,nHP)
!          iPrio(3) = .true.
!       case('DegradationRate')
!          read(value,*) (degRateC(i), i=1,nHP)
!          isPresent(4) = .true.
!       case('Investment')
!          read(value,*) (invC(i), i=1,nHP)
!          isPresent(5) = .true.
!       case('Lifetime')
!          read(value,*) (lifeC(i), i=1,nHP)
!          isPresent(6) = .true.
       case('OnOffCost')
          read(value,*) (fireCostHP(i), i=1,nHP)
          isPresent(7) = .true.
       case('OeMCost')
          read(value,*) (maintCostHP(i), i=1,nHP)
          isPresent(8) = .true.
       case('SetPoint')
          isPresent(9) = .true.
          value_ = value
          do i=1,nHP
              nSpHP(i) = hCount(value_)
              n2 = index(value_,')') + 1
              value_ =  value_(n2 + 1:)
          enddo
          call allocateVar(42)
          do i = 1, nHP
             n1 = index(value,'(') + 1
             n2 = index(value,')') - 1
             vector = trim(value(n1:n2))
             read(vector,*) (spHP(j,i), j=1,nSpHP(i))
             value =  value(n2 + 2:)
          enddo
!       case('Size')
!           value_ = value
!           isPresent(10) = .true.
!           do i=1,nHP
!               nSizeC(i) = hCount(value_)
!               n2 = index(value_,')') + 1
!               value_ =  value_(n2 + 1:)
!           enddo
!           call allocateVar(13)
!          do i = 1, nHP
!             n1 = index(value,'(') + 1
!             n2 = index(value,')') - 1
!             vector = trim(value(n1:n2))
!             read(vector,*) (kSizeC(i,j), j=1,nSizeC(i))
!             value =  value(n2 + 2:)
!           enddo
       case('Efficiency')
              isPresent(11) = .true.
              backspace(genUnit)
              line = line - 1
              do i = 1,nHP
                  nEtaHP(i) = vCount(genUnit,.false.)
              enddo
              j = sum(nEtaHP)
              call allocateVar(43)
              call rewUnit(genUnit,j)
              do i = 1, nHP
                 etaHP(:,:,i) =  dmatrixRead(genUnit,nEtaHP(i),2)
              enddo
              line = line + j
       case('Number')
             continue
       case('Technology')
             isPresent(2) = .true.
             read(value,*) (tecHP(i),i=1,nHP)
       case('MinUpTime')
             isPresent(12) = .true.
             read(value,*) (minUpTimeHP(i), i=1,nHP)
       case('MinDownTime')
             isPresent(13) = .true.
             read(value,*) (minDownTimeHP(i), i=1,nHP)
       case('OutletTemp')
          allocate(nOutTHP(nHP))
          value_ = value
          do i=1,nHP
              nOutTHP(i) = hCount(value_)
              n2 = index(value_,')') + 1
              value_ =  value_(n2 + 1:)
          enddo
          allocate(outTempHP(maxval(nOutTHP), nHP))
          do i = 1, nHP
             n1 = index(value,'(') + 1
             n2 = index(value,')') - 1
             vector = trim(value(n1:n2))
             read(vector,*) (outTempHP(j,i), j=1,nOutTHP(i))
             value =  value(n2 + 2:)
          enddo
       case('EnvTemp')
          allocate(nTenvHP(nHP))
          value_ = value
          do i=1,nHP
              nTenvHP(i) = hCount(value_)
              n2 = index(value_,')') + 1
              value_ =  value_(n2 + 1:)
          enddo
          allocate(envTempHP(maxval(nTenvHP), nHP))
          do i = 1, nHP
             n1 = index(value,'(') + 1
             n2 = index(value,')') - 1
             vector = trim(value(n1:n2))
             read(vector,*) (envTempHP(j,i), j=1,nTenvHP(i))
             value =  value(n2 + 2:)
          enddo
      case('PowerCorrection')
            do i=1,nHP
               n1 = maxval(nOutTHP)
               n2 = maxval(nTenvHP)
               allocate(hpCorrectionP(n1,n2,nHP))
               call rewUnit(genUnit,1)
               hpCorrectionP(:,:,i) = dmatrixRead(genUnit,nOutThp(i),nTenvHP(i))
            enddo
      case('CopCorrection')
            do i=1,nHP
               n1 = maxval(nOutTHP)
               n2 = maxval(nTenvHP)
               allocate(hpCorrectionE(n1,n2,nHP))
               call rewUnit(genUnit,1)
               hpCorrectionE(:,:,i) = dmatrixRead(genUnit,nOutThp(i),nTenvHP(i))
            enddo

!       case('TempCorrection')
!            read(value,*) (param(i), i=1,3)
!            do i=1,3
!               select case(param(i))
!                  case('temp')
!                     nt = i
!                  case('eta')
!                     net = i
!                  case('pmax')
!                     np = i
!               end select
!            enddo
!            do i=1,nHP
!               ntcHP(i) = vCount(genUnit,.false.)
!            enddo
!            call allocateVar(44,maxval(ntcHP))
!            allocate(matrix(maxval(ntcHP),3))
!            n = sum(ntcHP)
!            call rewUnit(genUnit,n)
!            do i=1,nHP
!               matrix = rNaN(rVal)
!               matrix = dmatrixRead(genUnit,ntcHP(i),3)
!               tempCorrHP(:,1,i) = matrix(:,nt)
!               tempCorrHP(:,2,i) = matrix(:,net)
!               tempCorrHP(:,3,i) = matrix(:,np)
!            enddo
!            deallocate(matrix)
!       case('PresCorrection')
!            read(value,*) (param(i), i=1,3)
!!            do i=1,3
!               select case(param(i))
!                  case('pres')
!                     nt = i
!                  case('eta')
!                     net = i
!                  case('pmax')
!                     np = i
!               end select
!            enddo
!            do i=1,nHP
!               npcHP(i) = vCount(genUnit,.false.)
!            enddo
!            call allocateVar(45,maxval(npcHP))
!            allocate(matrix(maxval(npcHP),3))
!            n = sum(npcHP)
!            call rewUnit(genUnit,n)
!            do i=1,nHP
!               matrix = rNaN(rVal)
!               matrix = dmatrixRead(genUnit,npcHP(i),3)
!               presCorrHP(:,1,i) = matrix(:,nt)
!               presCorrHP(:,2,i) = matrix(:,net)
!               presCorrHP(:,3,i) = matrix(:,np)
!            enddo
!            deallocate(matrix)
!       case('AltCorrection')
!            read(value,*) (param(i), i=1,3)
!            do i=1,3
!               select case(param(i))
!                  case('alt')
!                     nt = i
!                  case('eta')
!                     net = i
!                  case('pmax')
!                     np = i
!               end select
!            enddo
!            do i=1,nHP
!               nacHP(i) = vCount(genUnit,.false.)
!            enddo
!            call allocateVar(46,maxval(nacHP))
!            allocate(matrix(maxval(nacHP),3))
!            n = sum(nacHP)
!            call rewUnit(genUnit,n)
!            do i=1,nHP
!               matrix = rNaN(rVal)
!               matrix = dmatrixRead(genUnit,nacHP(i),3)
!               altCorrHP(:,1,i) = matrix(:,nt)
!               altCorrHP(:,2,i) = matrix(:,net)
!               altCorrHP(:,3,i) = matrix(:,np)
!            enddo
!            deallocate(matrix)
     case('Tmandata')
            read(value,*) (TutileHP(i),i=1,nHP)
       case(' ')
            if(verb) call warning(4,4,line=line)
       case default
            if(.not.silent)  call warning(1,4,line=line,word=keyword)
    end select
enddo

!---check if all the variablea were read---
nInp = size(isPresent)
do i = 1,nInp
    if(.not.isPresent(i)) call abortExecution(6,i)
enddo

close(genUnit)
100 format(A500)

end subroutine readHeatPump
