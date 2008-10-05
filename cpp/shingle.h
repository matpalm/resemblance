#ifndef SHINGLE_H_INCLUDED
#define SHINGLE_H_INCLUDED

#include <iostream>
#include <list>

#include "word_idx.h"

using namespace std;

#define N_GRAM_LENGTH 2

class Shingle {

    public:
        Shingle(WordIdx &wordIdx, string line);
        ~Shingle();

    private:
        int numShingles;
        list<string> shingles;
        string line;

};

#endif // SHINGLE_H_INCLUDED
