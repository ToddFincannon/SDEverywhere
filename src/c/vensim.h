#pragma once

#ifdef __cplusplus
extern "C" {
#endif

//
// Helpers
//
#define _NA_ (-DBL_MAX)
#define bool_cond(cond) ((double)(cond) != 0.0)

//
// Vensim functions
// See the Vensim Reference Manual for descriptions of the functions.
// http://www.vensim.com/documentation/index.html?22300.htm
//
#define _ABS(x) fabs(x)
#define _COS(x) cos(x)
#define _EXP(x) exp(x)
#define _GAME(x) (x)
#define _IF_THEN_ELSE(c, t, f) (bool_cond(c) ? (t) : (f))
#define _INTEG(value, rate) ((value) + (rate) * _time_step)
#define _INTEGER(x) trunc(x)
#define _LN(x) log(x)
#define _MAX(a, b) fmax(a, b)
#define _MIN(a, b) fmin(a, b)
#define _MODULO(a, b) fmod(a, b)
#define _QUANTUM(a, b) (b <= 0 ? a : b * trunc(a / b))

#define _SAMPLE_IF_TRUE(current, condition, input) (bool_cond(condition) ? (input) : (current))
#define _SIN(x) sin(x)
#define _SQRT(x) sqrt(x)
#define _STEP(height, step_time) (fgt(_time + _time_step / 2.0, (step_time)) ? (height) : 0.0)
double _PULSE(double start, double width);
double _PULSE_TRAIN(double start, double width, double interval, double end);
double _RAMP(double slope, double start_time, double end_time);
double* _VECTOR_SORT_ORDER(double* vector, size_t size, double direction);
double _XIDZ(double a, double b, double x);
double _ZIDZ(double a, double b);

//
// Lookups
//
typedef enum {
  Interpolate, Forward, Backward
} LookupMode;

typedef struct {
  double* data;
  size_t n;
  double* inverted_data;
  bool data_is_owned;
  double last_input;
  size_t last_hit_index;
} Lookup;

Lookup* __new_lookup(size_t size, bool copy, double* data);
void __delete_lookup(Lookup* lookup);
void __print_lookup(Lookup* lookup);

double __lookup(Lookup *lookup, double input, bool use_inverted_data, LookupMode mode);
#define _LOOKUP(lookup, x) __lookup(lookup, x, false, Interpolate)
#define _LOOKUP_FORWARD(lookup, x) __lookup(lookup, x, false, Forward)
#define _LOOKUP_BACKWARD(lookup, x) __lookup(lookup, x, false, Backward)
#define _WITH_LOOKUP(x, lookup) __lookup(lookup, x, false, Interpolate)
double _LOOKUP_INVERT(Lookup* lookup, double y);

double __get_data_between_times(double *data, size_t n, double input, LookupMode mode);
#define _GET_DATA_MODE_TO_LOOKUP_MODE(mode) ((mode) >= 1) ? Forward : (((mode) <= -1) ? Backward : Interpolate)
#define _GET_DATA_BETWEEN_TIMES(lookup, x, mode) __get_data_between_times((lookup)->data, (lookup)->n, x, _GET_DATA_MODE_TO_LOOKUP_MODE(mode))

#ifdef __cplusplus
}
#endif
