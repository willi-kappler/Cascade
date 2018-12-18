! This version of CASCADE is a fork from the original version published by Braun and Sambridge, 1997 Basin Research. The modifications made to the original version of CASCADE are described in Yanites and Ehlers, 2012, 2016 EPSL, and Eizenhoefer et al., 2019 JGR-ES.
! If you published results from this version of the program we would appreciate that you reference:
! 1. Publications listed by J. Braun in the cascade.f90 file.
! 2. Yanites, B.J., and Ehlers, T.A., 2012, Global climate and tectonic controls on the denudation of glaciated mountains, Earth and Planetary Science Letters, v. 325-326, pp. 63-75. doi.org/10.1016/j.epsl.2012.01.030.
! 3. Yanites, B.J., and Ehlers, T.A., 2016, Intermittent glacial sliding velocities explain variations in long-timescale denudation, SW British Columbia. Earth and Planetary Science Letters, 450, pp. 52-61. 
! 3. Eizenhoefer, P.R., McQuarrie, N., Shelef, E., and Ehlers, T.A., 2019 (in press), Fluvial responses to horizontal displacements in convergent orogens over geologic time scales, Journal of Geophysical Research - Earth Surface.

! Please direct all inquiries concerning this fork of the source code to Todd Ehlers (Univ. Tuebingen, Germany) tehlers@icloud.com

        module m_update_height_comsol
            contains
            subroutine update_height_comsol ()

! This runs the COMSOL model over a limited domain
! This model uses inputs that were created in export_to_comsol.txt
! 	Inputs: ice thickness (before reg. update_height is run), bed topography, bed temperature, and mass balance
! This outputs import_to_comsol.txt
!	Outputs: updated ice thickness, sliding	
!
!-----------------------------------------------------------------
!
! 2012.08.28, RH: I updated this file so that it's only the system call and necessary comments
! Future additions might include calculation of bed & ice temperature within COMSOL
!
! 2012.01.18, RH: Added del_gr to input for this file
! added comsol script call 1111
! added write file 2222

! 2011.12.20, WK: Shallow Ice Approximation
! do a system call for COMSOL in the future:
! system('comsol -a -b -c ...')
!-----------------------------------------------------------------
!
! If using a full COMSOL version, compiling and running the Java files will allow for more flexibility. With the current 
! version being used, only .mph files can be run. They don't need to be compiled
! Compiles the Comsol java file. 
! This is done to use the updated geometry files. (needs further testing)
! call system('module load java_1.6.0.33')
! call system('/usr/local/comsol42a/bin/comsol compile comsol/glacierice.java')
!/data/esd03/rheadley/src/com4ice/example_glacier3.java')

! Runs the Comsol class file
! This saves a text file containing the velocities
! This will probably be updated to irreg. output
call system('/usr/local/comsol42a/bin/comsol -ckl batch -inputfile comsol/glacierice.class')

! Runs the COMSOL mph file
! make sure input file is the right one
!call system('/usr/local/comsol42a/bin/comsol batch -inputfile /comsol/update_height.mph -outputfile /comsol/update_heightout.mph')

! Future: add COMSOL file name into the icecascade.in & parameters list, so that it can automatically update it when different models are run
          end subroutine update_height_comsol
        end module m_update_height_comsol

