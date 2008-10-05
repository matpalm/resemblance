#include "word_idx.h"

WordIdx::WordIdx() : sequence(-1) {
}

int WordIdx::idx_for_word(const string &word) {
    word_idx_it lookup = word_idx.find(word);
    if (lookup == word_idx.end()) {
        // not found
        int idx = ++sequence;
        word_idx[word] = idx;
        return idx;
    }
    else {
        // found!
        return lookup->second;
    }
}

int WordIdx::max_idx() {
    return sequence;
}


