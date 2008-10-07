#ifndef PARSER_H_INCLUDED
#define PARSER_H_INCLUDED

#include <vector>
#include "shingle.h"
#include "word_idx.h"

class Parser {
    public:
        void parse(vector<Shingle*> &shingles, WordIdx &word_idx);
};

#endif // PARSER_H_INCLUDED
