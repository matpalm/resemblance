#ifndef TESTS
#define TESTS

#include <iostream>
using namespace std;

void assert_same(int a, int b, string err) {
    if (a!=b) cerr << "FAIL " << err << endl;
}

void test_sbm(int bits[], int bits_length, int max, long expected_first, string msg) {
    long *vals;
    int vals_length;
    set_bits_multiple(bits, bits_length, max, vals, vals_length);
    assert_same(vals_length, 1, (msg+"a"));
/*
    cout << "vals[0]=" << vals[0] << "(";
    print_bits(vals[0]);
    cout << ") expected_first=" << expected_first << "(";
    print_bits(expected_first);
    cout << ")" << endl;
*/
    assert_same(vals[0], expected_first, (msg+"b"));
}

void test_sbm_2(int bits[], int bits_length, int max, long expected_first, long expected_second, string msg) {
    long *vals;
    int vals_length;
    set_bits_multiple(bits, bits_length, max, vals, vals_length);
    assert_same(vals_length, 2, (msg+"a"));
/*
    cout << "vals     ";
    print_bits(vals[0]);
    cout << "  ";
    print_bits(vals[1]);
    cout << endl;
    cout << "expected ";
    print_bits(expected_first);
    cout << "  ";
    print_bits(expected_second);
    cout << endl;
*/
    assert_same(vals[0], expected_first, (msg+"b"));
    assert_same(vals[1], expected_second, (msg+"c"));

}

void test_pop() {
    // count bits set
    assert_same(0, pop(0), "pop0");
    assert_same(1, pop(1), "pop1");
    assert_same(1, pop(2), "pop2");
    assert_same(2, pop(3), "pop3");
}

void test_union() {
    assert_same(0, union_size(0,0), "union 00 00");
    assert_same(1, union_size(1,0), "union 01 00");
    assert_same(1, union_size(0,1), "union 00 01");
    assert_same(1, union_size(1,1), "union 01 01");
    assert_same(2, union_size(1,2), "union 01 10");
    assert_same(2, union_size(2,1), "union 10 01");
    assert_same(1, union_size(2,2), "union 10 10");
    assert_same(2, union_size(3,2), "union 11 10");
    assert_same(2, union_size(2,3), "union 10 11");
    assert_same(2, union_size(3,3), "union 11 11");
}

void test_intersection() {
    assert_same(0, intersection_size(0,0), "intersection 00 00");
    assert_same(0, intersection_size(1,0), "intersection 01 00");
    assert_same(0, intersection_size(0,1), "intersection 00 01");
    assert_same(1, intersection_size(1,1), "intersection 01 01");
    assert_same(0, intersection_size(1,2), "intersection 01 10");
    assert_same(0, intersection_size(2,1), "intersection 10 01");
    assert_same(1, intersection_size(2,2), "intersection 10 10");
    assert_same(1, intersection_size(3,2), "intersection 11 10");
    assert_same(1, intersection_size(2,3), "intersection 10 11");
    assert_same(2, intersection_size(3,3), "intersection 11 11");
}

void test_set_bits_multiple_with_one_long() {
    int bits[4];

    bits[0] = 0;
    test_sbm(bits,1,0, 0x1, "sbm1"); // 2^0 = 1

    bits[0] = 1;
    test_sbm(bits,1,1, 0x2, "sbm2"); // 2^2 = 2

    bits[0] = 1;
    bits[1] = 5;
    test_sbm(bits,2,5, 0x22, "sbm3"); // 0010 0010

    bits[0] = 5;
    bits[1] = 1;
    test_sbm(bits,2,5, 0x22, "sbm4"); // 0010 0010

    bits[0] = 30;
    test_sbm(bits,1,30, 0x40000000, "sbm5"); // 0100 0000 0000 0000 0000 0000 0000 0000

    bits[0] = 31;
    test_sbm(bits,1,30, 0x80000000, "sbm6"); // 1000 0000 0000 0000 0000 0000 0000 0000

    bits[0] = 30;
    bits[1] = 31;
    test_sbm(bits,2,31, 0xc0000000, "sbm7"); // 1100 0000 0000 0000 0000 0000 0000 0000

}

void test_set_bits_multiple_with_two_longs() {
    int bits[4];

    bits[0] = 8;
    // 0000 0000 0000 0000 0000 0000 0000 0000   0000 0000 0000 0000 0000 0001 0000 0000
    test_sbm_2(bits,1,50, 0x00000000, 0x00000100, "sbm8");

    bits[0] = 32;
    // 0000 0000 0000 0000 0000 0000 0000 0001   0000 0000 0000 0000 0000 0000 0000 0000
    test_sbm_2(bits,1,32, 0x00000001, 0x00000000, "sbm9");

    bits[0] = 31;
    bits[1] = 32;
    bits[2] = 33;
    // 0000 0000 0000 0000 0000 0000 0000 0011   1000 0000 0000 0000 0000 0000 0000 0000
    test_sbm_2(bits,3,33, 0x00000003, 0x80000000, "sbm10");

}

void run_tests() {
    test_pop();
    test_union();
    test_intersection();
    //test_set_bits_multiple_with_one_long();
    test_set_bits_multiple_with_two_longs();

/*
    long *vals;
    int vals_length;
    int bits[3] = { 0 };
    set_bits_multiple(bits, 1, 10, vals, vals_length);
    assert_same(vals_length, 1, "sbm1");
    assert_same(vals[0], 1, "sbm2");
*/

    // done
    exit(0);
}

#endif
