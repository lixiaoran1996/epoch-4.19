! Copyright (C) 2009-2019 University of Warwick
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <http://www.gnu.org/licenses/>.

MODULE custom_laser

  USE shared_data

  IMPLICIT NONE

  LOGICAL :: loaded = .FALSE.
  INTEGER :: npts = 0
  REAL(num), ALLOCATABLE :: t_arr(:), E_arr(:)
  REAL(num) :: dt_file, t0   ! dt_file = 文件的 dt；t0 = 起始时间

CONTAINS

  FUNCTION custom_laser_time_profile(laser)

    TYPE(laser_block), INTENT(IN) :: laser
    REAL(num) :: custom_laser_time_profile

    custom_laser_time_profile = 1.0_num

  END FUNCTION custom_laser_time_profile

  ! FUNCTION get_custom_E(time)

  !   REAL(num), INTENT(IN) :: time
  !   REAL(num) :: get_custom_E

  !   get_custom_E = 1.0_num

  ! END FUNCTION get_custom_E


  SUBROUTINE load_custom_file()
    INTEGER :: ios, i
    REAL(num) :: idx, t, e
    CHARACTER(LEN=string_length):: filename

    IF (loaded) RETURN

    filename = TRIM(E_location)

    OPEN(10, FILE=filename, STATUS="old", ACTION="read", IOSTAT=ios)
    IF (ios /= 0) THEN
        WRITE(*,*) "ERROR: cannot open file: ", TRIM(filename)
        STOP "File open failed"
    END IF

    npts = 0
    DO
      READ(10, *, IOSTAT=ios) idx, t, e
      IF (ios /= 0) EXIT
      npts = npts + 1
    END DO

    ALLOCATE(t_arr(npts), E_arr(npts))

    REWIND(10)
    DO i = 1, npts
      READ(10, *) idx, t_arr(i), E_arr(i)
    END DO
    CLOSE(10)

    t0 = t_arr(1)
    dt_file = t_arr(2) - t_arr(1)

    loaded = .TRUE.
    WRITE(*,*) "Using E_location: ", filename
  END SUBROUTINE load_custom_file


  FUNCTION get_custom_E(time) RESULT(E_now)
    REAL(num), INTENT(IN) :: time
    REAL(num) :: E_now
    INTEGER :: idx

    IF (.NOT. loaded) CALL load_custom_file()

    ! 越界判断
    IF (time <= t0) THEN
      E_now = E_arr(1)
      RETURN
    ELSEIF (time >= t_arr(npts)) THEN
      E_now = 0.0_num
      RETURN
    END IF

    ! 直接按等间隔换算成索引（极快）
    idx = FLOOR( (time - t0) / dt_file )

    ! 线性插值（或只用最近点也行）
    E_now = E_arr(idx)  +  (E_arr(idx+1) - E_arr(idx)) * &
           (time - t_arr(idx)) / dt_file
  END FUNCTION get_custom_E

END MODULE custom_laser
