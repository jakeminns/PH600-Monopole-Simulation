PROGRAM electron_monopole_simulation

IMPLICIT NONE

!-------Defining double precision, integer and real variables----------!

DOUBLE PRECISION :: electron_charge, electron_mass, velocity_vector_x, velocity_vector_y, position_r_x, position_r_y
DOUBLE PRECISION :: time_step, time_stop, time, force_x, force_y, acceleration_x, acceleration_y, velocity_perp
INTEGER :: i
REAL :: magnetic_field

!-------Define physical constants----------!

electron_charge = 1.60217657D-19

electron_mass = 9.10938291D-31

!------Reading time step and stop-----------!

WRITE(*,*) 'Write time step:'
READ(*,*) time_step

WRITE(*,*) 'Write time stop:'
READ(*,*) time_stop

!------Reading Initial velocity and Position and magnetic field strength-----------!

WRITE(*,*) 'Write initial velocity x componant:'
READ(*,*) velocity_vector_x

WRITE(*,*) 'Write initial velocity y componant:'
velocity_vector_y = 1000000


WRITE(*,*) 'Write magnetic field strength:'
magnetic_field = 0.34

velocity_perp = ((velocity_vector_x**2)+(velocity_vector_y**2))**0.5
position_r_y = (electron_mass*velocity_perp)/(electron_charge*magnetic_field)
position_r_x = 0

!------Do loop position tracking-----------!

open (10, file='output_file.dat', status='unknown')

time = 0

DO i = 0, 100000, 1

	IF (time > time_stop) THEN 

		stop

	ELSE

		time = time + time_step

		force_x = (electron_charge)*velocity_vector_y*magnetic_field
		force_y = -(electron_charge)*velocity_vector_x*magnetic_field

		acceleration_x = force_x/electron_mass
		acceleration_y = force_y/electron_mass

		velocity_vector_x = velocity_vector_x + (acceleration_x*time)
		velocity_vector_y = velocity_vector_y + (acceleration_y*time)


		position_r_x = position_r_x +(velocity_vector_x*time)
		position_r_y = position_r_y +(velocity_vector_y*time)

		write(10,*) position_r_x, position_r_y
		write(*,*) position_r_x, position_r_y

	END IF

END DO

	close(10)

END PROGRAM electron_monopole_simulation