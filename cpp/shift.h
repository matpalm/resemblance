#include <stdio.h>

#ifndef SHIFT_FUNCS_INCLUDED
#define SHIFT_FUNCS_INCLUDED

#define N 32

void print_bits(long l) {
    for (int i=N-1; i>=0; i--) {
        printf("%s",((l >> i) & 1) ? "1" : "0");
        if (i%4==0 && i!=0) printf(" ");
    }
    //printf("\n");
}

void out(int i[], int i_length) {
    for(int j=0;j<i_length;j++)
        printf("%d ",i[j]);
    printf("\n");
}

// count number of bits set in a long
// brian kernighan is the king
int pop(long l) {
    unsigned int c;
    for(c=0;l;c++)
        l &= l-1;
    return c;
}

int union_size(long l1, long l2) {
    //print_bits(l1 | l2);
    return pop(l1 | l2);
}

int intersection_size(long l1, long l2) {
    //print_bits(l1 & l2);
    return pop(l1 & l2);
}

int max(int vals[], int vals_length) {
    int max = vals[0];
    int i;
    for(i=1;i<vals_length;i++)
        if (vals[i] > max)
            max = vals[i];
    return max;
}

void set_bits_multiple(int bits[], int bits_length, int max_bit, long*& vals, int& vals_length) {
    vals_length = (max_bit / N) +1;
    vals = new long[vals_length];
    int i;
    for(i=0; i<bits_length; i++) {
        // in the case of vals_length >1 we want to pack
        // higher order bits in the left most array elements, eg higher order
        // bits in a lower array index
        int long_to_set = vals_length-1 - (bits[i] / N);
        int bit_to_set = bits[i] % N;
        printf("vals_length %d bit %d long_to_set %d bit_to_set %d\n",vals_length,bits[i],long_to_set,bit_to_set);

        // create mask and apply it to actual value
        long mask = 1;
        mask <<= bit_to_set;
        vals[long_to_set] |= mask;
    }
}

#endif // SHIFT_FUNCS_INCLUDED
