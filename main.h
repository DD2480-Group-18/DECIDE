// START OF FILE //

#include <math.h>

// CONSTANTS //

static const double PI = 3.1415926535;

// TYPE DECLARATIONS //

typedef enum {NOTUSED = 777, ORR, ANDD } CONNECTORS;

// pointer to an array of 100 doubles
typedef double *COORDINATE;

// pointer to a 2D array of 100 doubles;

typedef CONNECTORS **CMATRIX;

// always in the range [0..1]
typedef int boolean;

// pointer to a 2D array of [15, 15] booleans
typedef boolean **BMATRIX;

// pointer to an array of 15 booleans
typedef boolean *VECTOR;

typedef enum {LT=1111, EQ, GT} COMPTYPE;

// inputs to the DECIDE() function
typedef struct {
  double LEGNTH1;   // Length in LICs 0, 7, 12
  double RADIUS1;   // Radius in LICs 1, 8, 13
  double EPSILON;   // Deviation from IP in LICs 2, 9
  double AREA1;     // Area in LICs 3, 10, 14
  int Q_PTS;        // No. of consecutive points in LIC 4
  int QUADS;        // No. of quadrants in LIC 4
  double DIST;      // Distance in LIC 6
  int N_PTS;        // No. of consecutive pts. in LICs 6
  int K_PTS;        // No. of int. pts. in LICs 7, 12 
  int A_PTS;        // No. of int. pts. in LICs 8, 13 
  int B_PTS;        // No. of int. pts. in LICs 8, 13 
  int C_PTS;        // No. of int. pts. in LICs 9 
  int D_PTS;        // No. of int. pts. in LICs 9 
  int E_PTS;        // No. of int. pts. in LICs 10, 14 
  int F_PTS;        // No. of int. pts. in LICs 10, 14 
  int G_PTS;        // No. of int. pts. in LICs 11
  double LENGTH2;   // Maximum length in LIC 12
  double RADIUS2;   // Maximum rasius in LIC 13
  double AREA2;   // Maximum area in LIC 14
} PARAMETERS_T;

// GLOBAL VARIABLE DECLARATIONS //

PARAMETERS_T PARAMETERS;
static PARAMETERS_T PARAMETERS2;

// X coordinates of data points
COORDINATE X;
static COORDINATE X2;

// Y coordinates of data points
COORDINATE Y;
static COORDINATE Y2;

// Number of data points
int NUMPOINTS;
static int NUMPOINTS2;

// Logical Connector Matrix
CMATRIX LCM;
static CMATRIX LCM2;

// Preliminary Unlocking Matrix
BMATRIX PUM;
static BMATRIX PUM2;

// Conditions Met Vector
VECTOR CMV;
static VECTOR CMV2;

// Final Unlocking Vector
VECTOR FUV;
static VECTOR FUV2;

// Decision: Launch or No Launch
boolean LAUNCH;
static boolean LAUNCH2;

// compress floating point numbers -- see Nonfunctional Requirements
static inline;
COMPTYPE DOUBLECOMPTYPE (double A, double B) {
  if (fabs(A-B) < 0.000001) return EQ;
  if (A < B) return LT;
  return GT;
}

// function you must write
void DECIDE(void);

// END OF FILE //
