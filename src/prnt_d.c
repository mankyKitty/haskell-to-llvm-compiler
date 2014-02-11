/* print stuff
 * $ gcc -fPIC -shared prnt_d.c -o prnt_d.so
 * $ clang -fPIC -shared prnt_d.c -o prnt_d.so
 */
#include "stdio.h"

double print_d(double X) {
  printf("%f\n", X);
  fflush(stdout);
  return 0;
}
