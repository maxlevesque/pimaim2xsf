MODULE module_ions
    use iso_fortran_env, only: dp => real64
    implicit none
    type typeions
        character(2) :: symbol
        integer :: nb
    end type
    type (typeions), dimension(:), allocatable :: ion
END MODULE

MODULE module_io
    use iso_fortran_env, only: dp => real64
    implicit none
    integer, parameter :: inputFileUnit=10, outputFileUnit=11
    character(*), parameter :: inputFileName="positions.out", outputFileName="positions.xyz"
    integer :: inputFileRecordsNumber
END MODULE

PROGRAM pimaim2xsf

    use iso_fortran_env, only: dp => real64
    implicit none
    
    call readNbOfIons
    call readSymbolAndQuantityOfIons
    call countNumberOfLinesInInputFile
    call testNumberOfStepsWrittenInInputFile
    call openInputAndOutputFiles
    call translate


    
    CONTAINS
    
    
    SUBROUTINE countNumberOfLinesInInputFile
        use module_io, only: inputFileUnit, inputFileName, inputFileRecordsNumber
        integer :: ios
        character(len=1) :: junk
        open(inputFileUnit, file=inputFileName)
        ios = 0
        inputFileRecordsNumber = 0
        do while (ios == 0)
            read(inputFileUnit,*,IOSTAT=ios) junk
            if (ios == 0) inputFileRecordsNumber = inputFileRecordsNumber + 1
        end do
        close(inputFileUnit)
    END SUBROUTINE

    SUBROUTINE translate
        use module_io
        use module_ions, only: ion
        integer :: i, j, k
        real(dp) :: x, y, z
        character(2), dimension(sum(ion%nb)) :: nature
        k = 0
        do i= 1, size(ion)
            do j= 1, ion(i)%nb
                k = k +1
                nature(k) = ion(i)%symbol
            end do
        end do
        do k= 1, inputFileRecordsNumber/sum(ion%nb)
            do i= 1, sum(ion%nb)
                if ( i == 1) then
                    write(outputFileUnit,*) sum(ion%nb)
                    write(outputFileUnit,*)
                end if
                read(inputFileUnit,*) x, y, z
                write(outputFileUnit,*) nature(i), x*0.529177211_dp, y*0.529177211_dp, z*0.529177211_dp ! convert bohr to angstroms
            end do
        end do
    END SUBROUTINE
    
    SUBROUTINE readNbOfIons
        use module_ions, only: ion
        integer :: nbOfIons
        print*,'How many ions (e.g. 3 for F, Y, Li)?'
        read(*,*) nbOfIons
        allocate( ion(nbOfIons) )
    END SUBROUTINE

    SUBROUTINE readSymbolAndQuantityOfIons
        use module_ions, only: ion
        integer :: i
        do i= 1, size(ion)
            print*,'Symbol, e.g. Cl, of the ion number ',i
            read(*,*) ion(i)%symbol
            print*,'How many ions of this type in the supercell?'
            read(*,*) ion(i)%nb
        end do
    END SUBROUTINE

    SUBROUTINE openInputAndOutputFiles
        use module_io, only: inputFileUnit, outputFileUnit, inputFileName, outputFileName
        open(inputFileUnit, file=inputFileName)
        open(outputFileUnit, file=outputFileName)
    END SUBROUTINE
    
    SUBROUTINE testNumberOfStepsWrittenInInputFile
        use module_io, only: inputFileRecordsNumber
        use module_ions, only: ion
        if( modulo(inputFileRecordsNumber,sum(ion%nb)) /= 0 ) then
            print*,'STOP'
            print*,'Incompatible record numbers in input file and total number of ions in the supercell.'
            print*,'Their modulo should be 0.'
            stop
        end if
    END SUBROUTINE
    
END PROGRAM
