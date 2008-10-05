#include "shingle.h"
#include "word_idx.h"

Shingle::Shingle(WordIdx &wordIdx, string line) : line(line) {
    cout << "making a shingle from [" << line << "]" << endl;

    for(uint i=0; i<line.length()-N_GRAM_LENGTH+1; i++) {
        string shingle = line.substr(i,N_GRAM_LENGTH);
        cout << "shingle [" << shingle << "] maps to " << wordIdx.idx_for_word(shingle) << endl;
        shingles.push_back(shingle);
    }

    for (list<string>::iterator i=shingles.begin(); i!=shingles.end(); ++i)
        cout << "next shingle is [" << *i << "]" << endl;


}

Shingle::~Shingle() {
}


