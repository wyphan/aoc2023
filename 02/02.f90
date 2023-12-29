PROGRAM aoc23day2a
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: e => ERROR_UNIT
  IMPLICIT NONE

  INTRINSIC :: CEILING, LOG10, REAL, SCAN

  INTEGER, PARAMETER :: ncolors = 3
  INTEGER, PARAMETER :: color_strlen = 5
  INTEGER, PARAMETER :: line_maxlen = 1024

  TYPE :: color
    CHARACTER(LEN=color_strlen) :: name
    INTEGER :: limit
  END TYPE color

  TYPE(color) :: colors(ncolors)
  CHARACTER(LEN=:), ALLOCATABLE :: ifile, logfile
  CHARACTER(LEN=line_maxlen) :: line
  CHARACTER(LEN=CEILING(LOG10(REAL(line_maxlen)))+3) :: fmt
  INTEGER :: u, l, eol, eof, cur_id, cur_power, tot_valid, tot_power
  LOGICAL :: valid

  CALL get_inputfile(ifile)
  !PRINT *, 'Reading input from ', ifile
  logfile = ifile(1:SCAN(ifile, '.')) // 'log'
  !PRINT *, 'Logging to ', logfile

  ! Initialize colors and limits for each
  colors(1) = color('red  ', 12)
  colors(2) = color('green', 13)
  colors(3) = color('blue ', 14)

  ! Main loop
  WRITE(fmt, '(A,I0,A)') '(A', line_maxlen, ')'
  OPEN(NEWUNIT=u, FILE=ifile, STATUS='old', ACTION='read', IOSTAT=eof)
  OPEN(NEWUNIT=l, FILE=logfile, STATUS='replace', ACTION='write')

  eof = 0
  eol = 0
  tot_valid = 0
  tot_power = 0
  fileloop: DO
    READ(u, fmt, IOSTAT=eol) line
    IF (eof /= 0 .OR. eol /= 0) EXIT fileloop
    CALL parse_game(line, cur_id, cur_power)
    tot_valid = tot_valid + cur_id
    tot_power = tot_power + cur_power
  END DO fileloop
  PRINT *, 'a', tot_valid
  PRINT *, 'b', tot_power

  CLOSE(u)
  CLOSE(l)

CONTAINS

  SUBROUTINE usage
    USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: e => ERROR_UNIT
    IMPLICIT NONE

    INTRINSIC :: GET_COMMAND_ARGUMENT
    CHARACTER(LEN=:), ALLOCATABLE :: exe
    INTEGER :: strlen

    CALL GET_COMMAND_ARGUMENT(0, LENGTH=strlen)
    ALLOCATE(CHARACTER(LEN=strlen) :: exe)
    CALL GET_COMMAND_ARGUMENT(0, VALUE=exe)

    WRITE(e,'(3A)') 'Usage: ', exe, ' <inputfile>'

    RETURN
  END SUBROUTINE usage

  SUBROUTINE get_inputfile(ifile)
    USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: e => ERROR_UNIT
    IMPLICIT NONE

    CHARACTER(LEN=:), ALLOCATABLE, INTENT(OUT) :: ifile

    INTRINSIC :: COMMAND_ARGUMENT_COUNT, GET_COMMAND_ARGUMENT
    INTEGER :: strlen
    LOGICAL :: exist

    IF (COMMAND_ARGUMENT_COUNT() /= 1) THEN
      CALL usage
      STOP
    END IF

    CALL GET_COMMAND_ARGUMENT(1, LENGTH=strlen)
    ALLOCATE(CHARACTER(LEN=strlen) :: ifile)
    CALL GET_COMMAND_ARGUMENT(1, VALUE=ifile)

    INQUIRE(FILE=ifile, EXIST=exist)
    IF (.NOT. exist) THEN
      WRITE(e, '(2A)') 'Cannot open input file ', TRIM(ifile)
      ERROR STOP 1
    END IF

    RETURN
  END SUBROUTINE get_inputfile

  SUBROUTINE parse_game(line, game_id, game_power)
    USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: e => ERROR_UNIT
    IMPLICIT NONE

    INTRINSIC :: LEN, SCAN, SIZE, COUNT, PACK

    CHARACTER(LEN=line_maxlen), INTENT(IN) :: line
    INTEGER, INTENT(OUT) :: game_id, game_power

    INTEGER :: line_len
    INTEGER :: c, i, j, ncubes, curi, curf, game_nrounds, round_ncolors
    INTEGER :: current_round(ncolors), cur_color(ncolors), cur_mincubes(ncolors)
    INTEGER :: pos_colon
    INTEGER, ALLOCATABLE :: pos_semicolons(:), pos_commas(:)
    CHARACTER(LEN=color_strlen) :: colornames(ncolors)
    CHARACTER(LEN=MIN(5, color_strlen)) :: word
    CHARACTER(LEN=1), ALLOCATABLE :: strarr(:)
    CHARACTER(LEN=:), ALLOCATABLE :: line_trim, game_str, round_str, color_str, valid_str
    LOGICAL :: valid

    colornames = colors(:)%name
    line_trim = TRIM(line)
    line_len = LEN(line_trim)

    pos_colon = SCAN(line_trim, ':')
    game_str = line_trim(1:pos_colon-1)
    READ(game_str, *) word, game_id

    strarr = [(line_trim(c:c), c = 1, line_len)]
    pos_semicolons = [pos_colon, PACK([(c, c = 1, line_len)], strarr(1:line_len) == ';'), line_len+1]
    game_nrounds = SIZE(pos_semicolons) - 1
    !WRITE(e,*) 'game id ', game_id, ': ', game_nrounds, ' rounds'

    valid = .TRUE.
    cur_mincubes(:) = 0
    DO i = 1, game_nrounds
      curi = pos_semicolons(i)
      curf = pos_semicolons(i+1)
      round_str = line_trim(curi+1:curf-1)
      !WRITE(e,*) 'round ', i, ' = ', round_str
      pos_commas = [curi, PACK([(c, c = curi+1, curf-1)], strarr(curi+1:curf-1) == ','), curf]
      round_ncolors = SIZE(pos_commas) - 1
      DO j = 1, round_ncolors
        curi = pos_commas(j)
        curf = pos_commas(j+1)
        color_str = line_trim(curi+1:curf-1)
        !WRITE(e,*) 'round ', i, ', ' color ', j, ' = ', color_str
        READ(color_str, *) ncubes, word
        cur_color = PACK([(c, c = 1, ncolors)], [(colornames(c), c = 1, ncolors)] == word(1:color_strlen))
        IF (ncubes > colors(cur_color(1))%limit) THEN
          valid = .FALSE.
        END IF
        IF (ncubes > cur_mincubes(cur_color(1))) THEN
          cur_mincubes(cur_color(1)) = ncubes
        END IF
      END DO ! j
    END DO ! i

    IF (valid) THEN
      valid_str = 'valid'
    ELSE
      valid_str = 'invalid'
    END IF
    WRITE(l,*) 'Game ', game_id, ' is ', valid_str, ', mincubes = ', (cur_mincubes(c), ' ', colornames(c), ' ', c = 1, ncolors)

    IF (.NOT. valid) game_id = 0
    game_power = PRODUCT(cur_mincubes)  

    RETURN
  END SUBROUTINE parse_game

END PROGRAM aoc23day2a