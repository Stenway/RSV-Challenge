! (C) Stefan John / Stenway / Stenway.com / 2023

program rsv
    implicit none
    
    type :: TOptionalString
        character(len=:), allocatable :: value
    end type TOptionalString

    type :: TRow
        type(TOptionalString), allocatable :: values(:)
    end type TRow

    type :: TRows
        type(TRow), allocatable :: rows(:)
    end type TRows
    
    type(TRows) :: rows
    type(TRows) :: loadedRows
    type(TRows) :: appendrows
    type(TOptionalString) :: decodeError
    allocate(rows%rows(4))
    allocate(rows%rows(1)%values(4))
    allocate(rows%rows(2)%values(2))
    allocate(rows%rows(3)%values(0))
    allocate(rows%rows(4)%values(1))
    
    rows%rows(1)%values(1)%value = "Hello"
    rows%rows(1)%values(2)%value = "🌎"
    rows%rows(1)%values(4)%value = ""
    
    rows%rows(2)%values(1)%value = "A" // char(0) // "B" // char(10) // "C"
    rows%rows(2)%values(2)%value = "Test 𝄞"
    
    rows%rows(4)%values(1)%value = ""
    
    print *,rsvToJsonString(rows)
    call saveRsv(rows, "Test.rsv")

    loadedRows = loadRsv("Test.rsv", decodeError)
    if (hasValue(decodeError)) then
        stop decodeError%value
    end if
    print *,rsvToJsonString(loadedRows)
    
    call saveRsv(loadedRows, "TestResaved.rsv")
    
    allocate(appendRows%rows(1))
    allocate(appendRows%rows(1)%values(1))
    appendRows%rows(1)%values(1)%value = "ABC"
    
    call appendRsv(appendRows, "Append.rsv", .false.)
    
    call checkTestFiles()
contains

    function intToStrWithPadding(i) result(result)
        character(len=:), allocatable :: result
        integer, intent(in) :: i
        character(10) :: temp
        write(temp,'(i3.3)') i
        result = trim(temp)
    end function

    subroutine checkTestFiles()
        type(TRows) :: loadedRows
        character(len=:), allocatable :: filePath
        character(len=:), allocatable :: jsonStr
        character(len=:), allocatable :: loadedJsonStr
        integer :: i
        type(TOptionalString) :: decodeError
                
        do i = 1, 79
            filePath = "./../TestFiles/Valid_" // intToStrWithPadding(i)
            print *, "Checking valid test file: ", filePath
            loadedRows = loadRsv(filePath // ".rsv", decodeError)
            if (hasValue(decodeError)) then
                STOP decodeError%value
            end if
            jsonStr = rsvToJsonString(loadedRows)
            loadedJsonStr = loadFile(filePath // ".json")
            if (jsonStr .ne. loadedJsonStr) then
                stop "JSON mismatch"
            end if
        end do
        
        do i = 1, 22
            filePath = "./../TestFiles/Invalid_" // intToStrWithPadding(i)
            print *, "Checking invalid test file: ", filePath
            loadedRows = loadRsv(filePath // ".rsv", decodeError)
            if (.not. hasValue(decodeError)) then
                stop "RSV document is valid"
            end if
        end do
    end subroutine

    ! ----------------------------------------------------------------------
    
    function isValidUtf8(str) result(result)
        character(len=*), intent(in) :: str
        logical :: result
        integer :: i, currentByte, currentByteClass, newStateLookupIndex
        integer :: lastState = 1
        
        integer, dimension(256) :: utf8ByteClassLookup
        integer, dimension(108) :: utf8StateTransitionLookup
        
        utf8ByteClassLookup = (/ &
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, &
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, &
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, &
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, &
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, &
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, &
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, &
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, &
            2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, &
            3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, &
            4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, &
            4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, &
            0, 0, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, &
            5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, &
            6, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 8, 7, 7, &
            9, 10, 10, 10, 11, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 &
        /)
        utf8StateTransitionLookup = (/ &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 1, 0, 0, 0, 2, 3, 5, 4, 6, 7, 8, &
            0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 2, 2, 2, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 5, 5, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 5, 5, 5, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0 &
        /)
        do i = 1, len(str)
            currentByte = ichar(str(i:i))
            currentByteClass = utf8ByteClassLookup(currentByte+1)
            newStateLookupIndex = lastState*12 + currentByteClass
            lastState = utf8StateTransitionLookup(newStateLookupIndex+1)
            if (lastState == 0) then
                result = .false.
                return
            endif
        end do
        result = (lastState == 1)
    end function

    ! ----------------------------------------------------------------------
    
    function getCount(str, char) result(result)
        character(len=*), Intent(In) :: str
        character(1) :: char
        integer :: result
        integer :: i
        result = 0
        do i = 1, len(str)
            if (str(i:i) == char) then
                result = result + 1
            end if
        end do
    end
    
    function getNextIndex(str, char, startIndex) result(result)
        character(len=*), Intent(In) :: str
        character(1) :: char
        integer :: result
        integer, Intent(In) :: startIndex
        integer :: i
        result = 0
        do i = startIndex, len(str)
            if (str(i:i) == char) then
                result = i
                return
            end if
        end do
    end

    ! ----------------------------------------------------------------------
    
    function hasValue(value) result(result)
        type(TOptionalString), Intent(In) :: value
        logical :: result
        result = allocated(value%value)
    end function
    
    ! ----------------------------------------------------------------------

    function encodeRsv(rows) result(result)
        type(TRows), Intent(In) :: rows
        type(TRow) :: row
        type(TOptionalString) :: value
        character(len=:), allocatable :: result
        integer :: i, j
        
        result = ""
        do i = 1, size(rows%rows)
            row = rows%rows(i)
            do j = 1, size(row%values)
                value = row%values(j)
                if (.not. allocated(value%value)) then
                    result = result // char(254)
                else
                    if (.not. isValidUtf8(value%value)) then
                        stop "Invalid string value"
                    end if
                    result = result // value%value
                end if
                result = result // char(255)
            end do
            result = result // char(253)
        end do
    end function

    function decodeRsv(bytesString, error) result(result)
        character(len=*), Intent(In) :: bytesString
        type(TOptionalString), Intent(Out) :: error
        type(TRows) :: result
        integer :: numRows, i, lastRowEnd, currentRowEnd
        integer :: numValues, j, lastValueEnd, currentValueEnd
        character(len=:), allocatable :: rowString
        character(len=:), allocatable :: valueString
        
        if (len(bytesString) > 0 .and. ichar(bytesString(len(bytesString):len(bytesString))) .ne. 253) then
            error%value = "Incomplete RSV document"
            return
        end if
        numRows = getCount(bytesString, char(253))
        allocate(result%rows(numRows))
        lastRowEnd = 0
        do i = 1, numRows
            currentRowEnd = getNextIndex(bytesString, char(253), lastRowEnd+1)
            if (currentRowEnd == 0) then
                currentRowEnd = len(bytesString)+1
            end if
            rowString = bytesString(lastRowEnd+1:currentRowEnd-1)
            if (len(rowString) > 0 .and. ichar(rowString(len(rowString):len(rowString))) .ne. 255) then
                error%value = "Incomplete RSV row"
                return
            end if
            lastRowEnd = currentRowEnd
            numValues = getCount(rowString, char(255))
            allocate(result%rows(i)%values(numValues))
            lastValueEnd = 0
            do j = 1, numValues
                currentValueEnd = getNextIndex(rowString, char(255), lastValueEnd+1)
                if (currentValueEnd == 0) then
                    currentValueEnd = len(rowString)+1
                end if
                valueString = rowString(lastValueEnd+1:currentValueEnd-1)
                lastValueEnd = currentValueEnd
                if (.not. (len(valueString) == 1 .and. ichar(valueString(1:1)) == 254)) then
                    if (.not. isValidUtf8(valueString)) then
                        error%value = "Invalid string value"
                        return
                    end if
                    result%rows(i)%values(j)%value = valueString
                end if
            end do
        end do
    end function
    
    ! ----------------------------------------------------------------------
    
    function loadFile(filePath) result(result)
        character(len=*), Intent(In) :: filePath
        character(len=:), allocatable :: result
        integer :: unit, fileSize
        
        open(newunit=unit, file=filePath, action="read", form="unformatted", access="stream")
        inquire(unit=unit, size=fileSize)
        allocate(character(len=fileSize) :: result)
        read(unit) result
        close(unit)
    end function
    
    ! ----------------------------------------------------------------------

    subroutine saveRsv(rows, filePath)
        type(TRows), Intent(In) :: rows
        character(len=*), Intent(In) :: filePath
        integer :: unit
        character(len=:), allocatable :: bytes
        
        bytes = encodeRsv(rows)
        open(newunit=unit, file=filePath)
        write(unit, "(a$)") bytes
        close(unit)
    end subroutine
    
    function loadRsv(filePath, decodeError) result(result)
        character(len=*), Intent(In) :: filePath
        type(TOptionalString), Intent(Out) :: decodeError
        type(TRows) :: result
        character(len=:), allocatable :: bytes
        
        bytes = loadFile(filePath)
        result = decodeRsv(bytes, decodeError)
    end function
    
    subroutine appendRsv(rows, filePath, continueLastRow)
        type(TRows), Intent(In) :: rows
        character(len=*), Intent(In) :: filePath
        integer :: unit, fileSize
        character(len=:), allocatable :: bytes
        logical :: continueLastRow
        character :: lastByte
        
        bytes = encodeRsv(rows)
        open(newunit=unit, file=filePath, action="readwrite")
        inquire(unit=unit, size=fileSize)
        if (continueLastRow .and. fileSize > 0) then
            call fseek(unit, -1, 2)
            read(unit, *) lastByte
            if (ichar(lastByte) .ne. 253) then
                stop "Incomplete RSV document"
            end if
            if (size(rows%rows) == 0) then
                return
            end if
            call fseek(unit, -1, 2)
        else
            call fseek(unit, 0, 2)
        end if
        write(unit, "(a$)") bytes
        close(unit)
    end subroutine

    ! ----------------------------------------------------------------------

    function getHexChar(i) result(result)
        character :: result
        integer :: i
        if (i <= 9) then
            result = char(48+i)
        else
            result = char(87+i)
        end if
    end function
    
    function getTwoDigitsHexString(i) result(result)
        character(len=2) :: result
        integer :: i
        result = getHexChar(i/16) // getHexChar(modulo(i, 16))
    end function
    
    function needsEscaping(str) result(result)
        character(len=*), Intent(In) :: str
        logical :: result
        integer :: i, c
        
        result = .false.
        do i = 1, len(str)
            c = ichar(str(i:i))
            if (c == 8 .or. &
                c == 9 .or. &
                c == 10 .or. &
                c == 12 .or. &
                c == 13 .or. &
                c == 34 .or. &
                c == 92 .or. &
                (c >= 0 .and. c <= 31)) then
                result = .true.
                return
            end if
        end do
    end function
    
    function escapeString(str) result(result)
        character(len=*), Intent(In) :: str
        character(len=:), allocatable :: result
        integer :: i, c
        
        if (.not. isValidUtf8(str)) then
            stop "Invalid string value"
        end if
        result = char(34)
        if (.not. needsEscaping(str)) then
            result = result // str
        else
            do i = 1, len(str)
                c = ichar(str(i:i))
                if (c == 8) then
                    result = result // "\b"
                else if (c == 9) then
                    result = result // "\t"
                else if (c == 10) then
                    result = result // "\n"
                else if (c == 12) then
                    result = result // "\f"
                else if (c == 13) then
                    result = result // "\r"
                else if (c == 34) then
                    result = result // "\" // char(34)
                else if (c == 92) then
                    result = result // "\\"
                else if (c >= 0 .and. c <= 31) then
                    result = result // "\u00" // getTwoDigitsHexString(c)
                else
                    result = result // char(c)
                end if
            end do
        end if
        result = result // char(34)
    end function
    
    function rsvToJsonString(rows) result(result)
        type(TRows), Intent(In) :: rows
        type(TRow) :: row
        type(TOptionalString) :: value
        character(len=:), allocatable :: result
        integer :: i, j
        logical :: isFirstRow, isFirstValue
        
        result = "["
        isFirstRow = .true.
        do i = 1, size(rows%rows)
            if (.not. isFirstRow) then
                result = result // ","
            end if
            isFirstRow = .false.
            result = result // char(10) // "  ["
            isFirstValue = .true.
            row = rows%rows(i)
            do j = 1, size(row%values)
                if (.not. isFirstValue) then
                    result = result // ", "
                end if
                isFirstValue = .false.
                value = row%values(j)
                if (.not. allocated(value%value)) then
                    result = result // "null"
                else
                    result = result // escapeString(value%value)
                end if
            end do
            result = result // "]"
        end do
        result = result // char(10) // "]"
    end function
end program