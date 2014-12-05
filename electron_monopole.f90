PROGRAM electron_monopole_simulation

IMPLICIT NONE

!-------Defining double precision, integer and real variables----------!

DOUBLE PRECISION :: electron_charge, electron_mass, velocity_vector_x, velocity_vector_y, position_r_x, position_r_y
DOUBLE PRECISION :: time_step, time_stop, time, force_x, force_y, acceleration_x, acceleration_y, velocity_perp, monopole_distance
DOUBLE PRECISION :: permability_free, root, radius 
Integer :: i
DOUBLE PRECISION :: magnetic_field_z, magnetic_field_r, magnetic_field, magnetic_field_y, magnetic_field_x
DOUBLE PRECISION :: pi

!-------Define physical constants----------!

electron_charge = 1.60217657D-19

electron_mass = 9.10938291D-31

permability_free = 1.2566D-6

pi = 3.14159265359D0

!------Reading time step and stop-----------!

WRITE(*,*) 'Write time step:'
time_step = 1D-3

WRITE(*,*) 'Write time stop:'
time_stop = 1D2

!------Reading Initial velocity and Position and magnetic field strength-----------!

WRITE(*,*) 'Write initial velocity x componant:'
velocity_vector_x = 0

WRITE(*,*) 'Write initial velocity y componant:'
velocity_vector_y = ((2.0*0.1466D-24)/electron_mass)**(0.5)
WRITE(*,*) velocity_vector_y

WRITE(*,*) 'Write distance to monopole:'
monopole_distance = 3.117D-6

WRITE(*,*) 'Write magnetic field strength:'
magnetic_field = 1.3164489D-8
position_r_x = -2.403D-6
position_r_y = 0

!------Do loop position tracking-----------!

open (10, file='output_file.txt', status='unknown')

time = 0


DO i = 0, 1000000000, 1

	IF (time > time_stop) THEN 

		stop

	ELSE

		time = time + time_step

		radius = ((position_r_y**2.0)+(position_r_x**2.0))**0.5

		root = (4.0*pi*(((monopole_distance**2.0)+(radius**2.0))**(3.0/2.0)))
		magnetic_field_z = (permability_free*magnetic_field*monopole_distance)/root

		force_x = (electron_charge)*velocity_vector_y*magnetic_field_z
		force_y = -(electron_charge)*velocity_vector_x*magnetic_field_z

		acceleration_x = force_x/electron_mass
		acceleration_y = force_y/electron_mass

		velocity_vector_x = velocity_vector_x + (acceleration_x*time_step)
		velocity_vector_y = velocity_vector_y + (acceleration_y*time_step)


		position_r_x = position_r_x +(velocity_vector_x*time_step)
		position_r_y = position_r_y +(velocity_vector_y*time_step)

		write(10,*) position_r_x, position_r_y



	END IF

END DO
















	close(10)

END PROGRAM electron_monopole_simulation