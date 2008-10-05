#ifndef SHINGLE_H_INCLUDED
#define SHINGLE_H_INCLUDED

#include <iostream>
#include <set>

#include "word_idx.h"
#include "bit_utils.h"

using namespace std;

#define N_GRAM_LENGTH 2

class Shingle {

    public:
        Shingle(WordIdx &wordIdx, const string line);
        ~Shingle();
        void build_bit_representation(const int max_bit);
        float resemblance_to(Shingle &other);

    private:
        int numShingles;
        string line;
        set<int> shingles;

        long *bit_representation;
        int bit_representation_length;

        // bit utils
        int union_size(Shingle &other);
        int intersection_size(Shingle &other);

        inline int count_number_bits_set(long l)  {
            unsigned int c;
            for(c=0;l;c++)
                l &= l-1;
            return c;
        }

        void set_bits_multiple(int bits[], int bits_length, int max_bit);
        void print_bits(long l);
};

#endif // SHINGLE_H_INCLUDED
