# DECIDE

## 1 Introductions

### 1.1 Input Variables

The values of the following global variables are available to your function:

- **NUMPOINTS**: The number of planar data points.
- **POINTS**: Array containing the coordinates of data points.
- **PARAMETERS**: Struct holding parameters for LIC’s (see below).
- **LCM**: Logical Connector Matrix.
- **PUV**: Preliminary Unlocking Vector.

### 1.2 Output Variable

The ouput is:

- **LAUNCH**: Final launch / no launch decision encoded as ”YES”, ”NO” on the standard output. In addition, the following intermediate results are computed.
- **CMV**: Conditions Met Vector.
- **PUM**: Preliminary Unlocking Matrix.
- **FUV**: Final Unlocking Vector.

## 2 Required computations

Implementation details can be obtained from the PDF.

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
