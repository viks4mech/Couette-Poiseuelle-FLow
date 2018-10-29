# Couette-Poiseuelle-FLow
2-D simulation to perform Couette Poiseulle Flow simulation using mixing length model

objective:
1)model coutte poiselle flow, copute results and find out how good the results are?
2)check for 18 cases for numerical results with experimental resluts.
3) go to the imp/webpage/laval,lecture 
  to download the reference papers
  go to coutte-poiselle practice
  a) experimental data
  b) fortra program to generate
  c) reference papers

capture important details from the papers such as formulaes

final results expected:
1)report
2)make program using finite volume (should behypothese: readable/ with comments)
3)make 1 programm for all 18 cases taken for 18 papers
4)make computation for 15 cases of EL.TELBANY
5) make comptation for 3 cases of GILLIOT
6) get results for veloity and other parameters

compare results and analyse the results, if there is ant deviation from the standard try to give reasons for the deviation.
reasons for deviation:
1)experimental data not correct
2)numerical model used has imperfection

couette flow:
has linear velocity gradients due to a fixed and a moving wall

poiseulle floe:
has a parabolic shape of velocity profile, both wall a considered fixed

couette-poiseulle flow:
has a combination of linear and parabolic shape

all 18 cases use mixing length model to compute the results of tubulent model

hypothes:
1)visocous
2)steady
3)1-directional
4)parallel flow
5)isothermal to make the viscosity constant


form continuity we get du/dx is zero
problem 

solution with finite volume:
1)discretize in the y-direction the mesh-(frinctional velocity related to the dy near wall) in 1-d
2)finite volume works on conservation laws.
3)solve equation mesh by mesh
4)integrate the equation the final equation wrt y-direction, use S-south and N-north for the integration limits
5)effect the equation to the centre of the mesh.
6)program the centre of the grid points of the discretised points
)
program structure:
1) routine of thomas that takes 3 values and gets answer
2) main program to use different cases and gets the 
3) a routine for turbulent model to compute turbulent viscosity


TO COMPILE:
1) to compile the modules:

gfortran -c grid.f90
gfortran -c nu_turbulent.f90
gfortran -c thomas.f90

2) To compile the main program:

gfortran grid.o nu_turbulent.o thomas.o couettegilcommented.f90 -o couette.exe

3) To run the program:

./couette.exe


There is a separate code written to get the plots using python. you can use the code with file name python_code and copy and paste after you put all the data of the computed values in the plot folder.

