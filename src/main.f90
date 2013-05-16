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

END MODULE

PROGRAM pimaim2xsf

    use iso_fortran_env, only: dp => real64
    use module_ions, only: ion
    implicit none
    
    call readNbOfIons
    call readSymbolAndQuantityOfIons
    call openInputAndOutputFiles
    call translate


    
    CONTAINS

    SUBROUTINE translate
    
    END SUBROUTINE
    
    SUBROUTINE readNbOfIons
        integer :: nbOfIons
        print*,'How many ions (e.g. 3 for F, Y, Li)?'
        read(*,*) nbOfIons
        allocate( ion(nbOfIons) )
    END SUBROUTINE

    SUBROUTINE readSymbolAndQuantityOfIons
        integer :: i
        do i= 1, size(ion)
            print*,'Symbol, e.g. Cl, of the ion number ',i
            read(*,*) ion(i)%symbol
            print*,'Number of this type of ions in the supercell?'
            read(*,*) ion(i)%nb
        end do
    END SUBROUTINE

    SUBROUTINE openInputAndOutputFiles
        use module_io, only: inputFileUnit, outputFileUnit, inputFileName, outputFileName
        open(inputFileUnit, file=inputFileName)
        open(outputFileUnit, file=outputFileName)
    END SUBROUTINE
    
END PROGRAM
