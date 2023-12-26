PROGRAM aoc23day2a
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: e => ERROR_UNIT
  IMPLICIT NONE

  INTEGER, PARAMETER :: ncolors = 3
  INTEGER, PARAMETER :: color_strlen = 5

  TYPE :: color
    CHARACTER(LEN=color_strlen) :: name
    INTEGER :: limit
  END TYPE color

  TYPE(color) :: colors(ncolors)
  CHARACTER(LEN=:), ALLOCATABLE :: ifile

  INTEGER :: i, tot

  CALL get_inputfile(ifile)

  ! Initialize colors and limits for each
  colors(1) = color('red  ', 12)
  colors(2) = color('green', 13)
  colors(3) = color('blue ', 14)


CONTAINS

  SUBROUTINE usage
    USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: e => ERROR_UNIT
    IMPLICIT NONE

    INTRINSIC :: GET_COMMAND_ARGUMENT
    CHARACTER(LEN=:), ALLOCATABLE :: exe

    CALL GET_COMMAND_ARGUMENT(0, VALUE=exe)
    WRITE(e,'(3A)') 'Usage: ', exe, ' <inputfile>'

    RETURN
  END SUBROUTINE usage

  SUBROUTINE get_inputfile(ifile)
    USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: e => ERROR_UNIT
    IMPLICIT NONE

    CHARACTER(LEN=:), ALLOCATABLE, INTENT(OUT) :: ifile

    INTRINSIC :: COMMAND_ARGUMENT_COUNT, GET_COMMAND_ARGUMENT
    LOGICAL :: exist

    IF (COMMAND_ARGUMENT_COUNT() /= 1) THEN
      CALL usage
      STOP
    END IF

    CALL GET_COMMAND_ARGUMENT(1, VALUE=ifile)
    INQUIRE(FILE=TRIM(ifile), EXIST=exist)
    IF (.NOT. exist) THEN
      WRITE(e, '(2A)') 'Cannot open input file ', TRIM(ifile)
      ERROR STOP 1
    END IF

    RETURN
  END SUBROUTINE get_inputfile

  FUNCTION parse_game(line) RESULT(game_id)
    IMPLICIT NONE

    INTRINSIC :: LEN, SCAN, COUNT, FINDLOC

    CHARACTER(LEN=:), ALLOCATABLE, INTENT(IN) :: line
    INTEGER :: game_id

    INTEGER :: c, i, j, ncubes, curi, curf, game_nrounds, round_ncolors
    INTEGER :: current_round(ncolors), cur_color(ncolors)
    INTEGER :: pos_colon
    INTEGER, ALLOCATABLE :: pos_semicolons(:), pos_commas(:)
    CHARACTER(LEN=color_strlen) :: colornames(ncolors)
    CHARACTER(LEN=1), ALLOCATABLE :: strarr(:)
    CHARACTER(LEN=:), ALLOCATABLE :: word, round_str, color_str

    colornames = colors(:)%name

    pos_colon = SCAN(line, ':')
    READ(line(1:pos_colon-1), *) word, game_id
    PRINT *, 'game id ', game_id

    ALLOCATE(strarr(LEN(line)))
    strarr = [(line(c:c), c = 1, LEN(line))]
    pos_semicolons = [pos_colon, PACK([(c, c = 1, LEN(line))], strarr == ';'), LEN(line)+1]
    game_nrounds = SIZE(pos_semicolons) - 1
    PRINT *, game_nrounds, ' rounds'

    DO i = 1, game_nrounds
      curi = pos_semicolons(i)
      curf = pos_semicolons(i+1)
      round_str = line(curi+1:curf-1)
      PRINT *, 'round ', i, ' = ', round_str
      pos_commas = [curi, PACK([(c, c = curi+1, curf-1)], strarr(curi+1:curf-1) == ','), curf]
      round_ncolors = SIZE(pos_commas) - 1
      DO j = 1, round_ncolors
        curi = pos_commas(j)
        curf = pos_commas(j+1)
        color_str = line(curi+1:curf-1)
        PRINT *, j, ' ', color_str
        READ(color_str, *) ncubes, word
        cur_color = PACK([(c, c = 1, ncolors)], [(TRIM(colornames(c)), c = 1, ncolors)] == word)
        IF (ncubes > colors(cur_color(1))%limit) THEN
          PRINT *, 'Limit break' 
          game_id = 0
          RETURN
        END IF
      END DO ! j
    END DO ! i

    DEALLOCATE(strarr)

    RETURN
  END FUNCTION parse_game

END PROGRAM aoc23day2a