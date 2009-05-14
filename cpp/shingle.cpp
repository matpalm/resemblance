#include <stdlib.h>
#include "shingle.h"
#include "word_idx.h"

#define N 32

//#include "shift.h"

Shingle::Shingle(WordIdx &wordIdx, const string id, const string line) :
    line(line), id(id), bit_representation(0), bit_representation_length(0) {

    // get ngram shingles and store indexes from word idx
    for(uint i=0; i<line.length()-N_GRAM_LENGTH+1; i++) {
        string shingle = line.substr(i,N_GRAM_LENGTH);
        int shingle_idx = wordIdx.idx_for_word(shingle);
        shingles.insert(shingle_idx);
    }

//    cout << "shingles... ";
//    for (set<int>::iterator i=shingles.begin(); i!=shingles.end(); ++i)
//        cout << "[" << *i << "] ";
//    cout << endl;
}

Shingle::~Shingle() {
    delete [] bit_representation;
}

ostream &operator <<(ostream &os, const Shingle &obj) {
    os << "line[" << obj.line << "] ";
    os << hex;
    for(int i=0; i<obj.bit_representation_length; i++) {
        os << obj.bit_representation[i] << " ";
    }
    return os;
}

string Shingle::getId() {
    return id;
}

void Shingle::build_bit_representation(const int max_bit) {
    //cout << "building shingle bits for [" << line << "]" << endl;

    // convert shingles list<int> to int[]
    const int as_array_length = shingles.size();
    int as_array[as_array_length];
    int idx = 0;
    for (set<int>::iterator iter=shingles.begin(); iter!=shingles.end(); ++iter)
        as_array[idx++] = *iter;

//    for(int i=0;i<as_array_length;i++)
//        cout << as_array[i] << " ";
//    cout << endl;

    // use ints to set bits in longs
    set_bits_multiple(as_array, as_array_length, max_bit);

}

float Shingle::resemblance_to(Shingle &other) {
    //if (bit_representation_length != other.bit_representation_length) cerr << "expected the same!";
    //cout << "is=" << intersection_size(other) << " us=" << union_size(other) << endl;
    return (float) intersection_size(other) / union_size(other);
}

int Shingle::union_size(Shingle &other) {
    int num_bits = 0;
    long *ours = bit_representation;
    long *others = other.bit_representation;
    for(int i=0; i<bit_representation_length; i++)
        num_bits += count_number_bits_set(*ours++ | *others++);
    return num_bits;
}

int Shingle::intersection_size(Shingle &other) {
    int num_bits = 0;
    long *ours = bit_representation;
    long *others = other.bit_representation;
    for(int i=0; i<bit_representation_length; i++)
        num_bits += count_number_bits_set(*ours++ & *others++);
    return num_bits;
}

void Shingle::set_bits_multiple(int bits[], int bits_length, int max_bit) {
    bit_representation_length = (max_bit / N) +1;
    bit_representation = new long[bit_representation_length];
    for(int i=0;i<bit_representation_length;i++)
        bit_representation[i]=0;

    for(int i=0; i<bits_length; i++) {
        // in the case of vals_length >1 we want to pack
        // higher order bits in the left most array elements, eg higher order
        // bits in a lower array index
        int long_to_set = bit_representation_length-1 - (bits[i] / N);
        int bit_to_set = bits[i] % N;
//        printf("bit_representation_length %d bit %d long_to_set %d bit_to_set %d\n",bit_representation_length,bits[i],long_to_set,bit_to_set);

        // create mask and apply it to actual value
        long mask = 1;
        mask <<= bit_to_set;
        bit_representation[long_to_set] |= mask;

//        for(int i=0; i<bit_representation_length; i++) {
//            print_bits(bit_representation[i]);
//            printf("|");
//        }
//        printf("\n");

    }
}

void Shingle::print_bits(long l) {
    for (int i=N-1; i>=0; i--) {
        printf("%s",((l >> i) & 1) ? "1" : "0");
        if (i%4==0 && i!=0) printf(" ");
    }
    //printf("\n");
}
