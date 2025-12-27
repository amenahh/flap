#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <inttypes.h>

int equal_string(const char* s1, const char* s2) {
  return (strcmp (s1, s2) == 0 ? 1 : 0);
}

int equal_char(char c1, char c2) {
  return (c1 == c2 ? 1 : 0);
}

void print_string(const char* s) {
  printf("%s", s);
}

// void print_int(int64_t n) {
//     fprintf(stderr, "Students! This is your job!\n");
// }


void print_int(int64_t n) {
    printf("%" PRId64 "\n", n);
    printf("fin prinf\n");
}

// void observe_int(int64_t n) {
//   print_int(n);
// }

int64_t observe_int(int64_t n) {
  print_int(n);
  return n;
}

int64_t add_eight_int(int64_t n1,int64_t n2,int64_t n3,int64_t n4,int64_t n5,int64_t n6,int64_t n7,int64_t n8){
  return n1+n2+n3+n4+n5+n6+n7+n8;
}

intptr_t* allocate_block (int64_t n) {
  return (intptr_t*)malloc (n * sizeof (int64_t));
}

intptr_t read_block (intptr_t* block, int64_t n) {
  return block[n];
}

int64_t write_block (intptr_t* block, int64_t n, intptr_t v) {
  block[n] = v;
  return 0;
}
