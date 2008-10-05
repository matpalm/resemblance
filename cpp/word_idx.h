#ifndef WORDIDX_H_INCLUDED
#define WORDIDX_H_INCLUDED

#include <iostream>
#include <map>

using namespace std;

typedef map<string,int> word_idx_map;
typedef map<string,int>::iterator word_idx_it;

class WordIdx {
    public:
        WordIdx();
        int idx_for_word(string word);
    private:
        int sequence;
        word_idx_map word_idx;
};

#endif //WORDIDX_H_INCLUDED
