# DECIDE


## 1 DECIDE - an anti-ballistic missile interceptor program
### 1.1 What is it?

DECIDE is a program implementing a hypothetical anti-ballistic missile interceptor program. The program returns a boolean signal indicating whether an incoming missile should be intercepted.

### 1.2 How it works

DECIDE consists of two main steps. They are the following:

1. Assess which combination of the fifteen Launch Interceptor Conditions (LICs) are relevant to the situation.
2. If all relevant LIC combinations are met, the launch button will be unlocked.

By analyzing up to 100 planar data points representing radar echoes, the program will decide whether each of the fifteen LICs evaluate to true (meaning that the condition is met).

Furthermore, an intermediate Conditions Met Vector (CMV) of length 15 is assigned boolean values; each element corresponding to one LIC condition.

An intermediate matrix (15x15), the Logical Connector Matrix (LCM) decides which individual LICs must be considered together. The LICs can be considered with logical operators (AND, OR), and can also be marked (NOTUSED) which disregards the condition.

By combining the LCM and the CMV, the Preliminary Unlocking Matrix (PUM) is created (dimensions 15x15). The PUM is combined with the Preliminary Unlocking Vector (PUV) (length 15), which decides which LICs matters in the given situation. The elements in the PUV will combine the PUM values in order to create the Final Unlocking Vector (FUV).

The FUV is a 15-element boolean vector which indicates if the launch button should be unlocked. Only if _all_ the values are _true_ is the launch button unlocked.

### 1.3 How to use it

The program in its current state exists as a proof of concept for a university assignment.
All major functionality is implemented and tested, but the program can currently only be tested, not run directly. Thus, the program can not be run without modification if custom input is desired.

#### 1.3.1 How to run tests

To run all tests, from the outermost directory run:

```
python3 -m unittest
```

If you desire to run a specific test file, run the same command but specify which file to test. For instance, to test `pum.py`:

```
python3 -m unittest pum.py
```

### 1.4 Statement of contributions

__Adam Henriksson__ 

- LICs [0-3, 7-14] + testing, refactoring

__Zino Kader__

- LICs 7-14, PUM + testing, refactoring, documentation

__Hasan Kalzi__ 

- LICs [0,1] [4-6], Testing [0-9] + Functions + refactoring

### 1.5 Essence Standard - way of working

In the way-of-working state diagram, the group is in the "In Use" state. This is motivated by the fact that practices and tools are used, scrutinized and evaluated during the course of work, the practices have been adapter to the team's context (when we work), and the group regularly handles feedback from Pull Requests, to name a few.

The main obstacle that hinders the group from reaching the next state is that not all members are currently using and adapting the way-of-working. To reach the next state, all members would need to contribute.

## 2 Assignment Details

### 2.1 Input Variables

The values of the following global variables are available to your function:

- **NUMPOINTS**: The number of planar data points.
- **POINTS**: Array containing the coordinates of data points.
- **PARAMETERS**: Struct holding parameters for LIC’s (see below).
- **LCM**: Logical Connector Matrix.
- **PUV**: Preliminary Unlocking Vector.

### 2.2 Output Variable

The ouput is:

- **LAUNCH**: Final launch / no launch decision encoded as ”YES”, ”NO” on the standard output. In addition, the following intermediate results are computed.
- **CMV**: Conditions Met Vector.
- **PUM**: Preliminary Unlocking Matrix.
- **FUV**: Final Unlocking Vector.

## 3 Required computations

Implementation details can be obtained from the specification [PDF](https://canvas.kth.se/courses/31884/files/4932282/download).

## Glossary

- **angle**: An angle is formed by two rays which share a common endpoint called a vertex. If one ray is rotated about the vertex until it coincides with the other ray,the amount of rotation required is the measure of the angle. Three points can be used to determine an angle by drawing a ray from the second point through the first point and another ray from the second point through the third point. Note that different angles are described according to whether the ray is rotated clockwise or counterclockwise. Either can be used in this problem because of the way the LIC’s are defined.
- **CMV**: (Conditions Met Vector) The CMV is a boolean vector whose elements have a one-to-one correspondence with the launch interceptor conditions. If the radar tracking data satisfy a certain LIC, then the corresponding element of the CMV is to be set to true.
- **consecutive**: Two points are consecutive if they are adjacent in the input data vectors X and Y.Thus(X[i],Y[i]) and (X[i+1],Y[i+1]) are adjacent.
- **FUV**: (Final Unlocking Vector) The FUV is a boolean vector which is the basis for deciding whether to launch an interceptor.If all elements of the FUV are true, a launch should occur.
- **LCM**: (Logical Connector Matrix) The LCM describes how individual LIC’s should be logically combined. For example, the value of LCM[i,j] indicates whether LIC #i should combine with LIC #j by the logical AND, OR, or not at all.
- **LIC**: (Launch Interceptor Condition) If radar tracking data exhibit a certain combination of characteristics, then an interceptor should be launched. Each characteristic is an LIC.
- **matrix**: For purposes of this problem, a matrix can be considered to be a two-dimensional array. planar data points Planar data points are points that are all located within the same plane.
- **PUM**: (Preliminary Unlocking Matrix) Every element of the boolean PUM corresponds to an element of the LCM. If the logical connection dictated by the LCM element gives the value “true”, the corresponding PUM element is to be set to true.
- **quadrant**: The x and y axes of the Cartesian coordinate system divide a plane into four areas called quadrants. They are labeled I, II, III, IV, beginning with the area where both coordinates are positive and numbering counterclockwise.
- **radius**: The length of the radius of a circle is the distance from the center of the circle to any point on the circle’s circumference.
- **ray**: A ray is a straight line that extends from a point.
- **vector**: For purposes of this problem, a vector may be considered to be a one-dimensional array.
- **vertex**: When two rays originate from a common point to form an angle, the point of their origination is called the vertex of that angle.
