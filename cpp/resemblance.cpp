#include <iostream>
#include <vector>
#include <set>

#include "shingle.h"
#include "word_idx.h"
#include "parser.h"

using namespace std;

int main(int argc, char *argv[]) {

    const float min_resemblance = argc==1 ? 0 : atof(argv[1]);

    //run_tests();

    // lookup from shingles to idx
    WordIdx word_idx;

    // list of shingles
    vector<Shingle*> shingles;

    // parse stdin
    Parser parser;
    parser.parse(shingles, word_idx);

    // build bit representation
    for (vector<Shingle*>::iterator iter=shingles.begin(); iter!=shingles.end(); ++iter)
        (*iter)->build_bit_representation(word_idx.max_idx());

    // convert from list back to array
    const int number_lines = shingles.size();
    Shingle **shingles_array = new Shingle*[number_lines];
    for (int i=0;i<number_lines;i++)
        shingles_array[i] = shingles[i];

    // compare each pair output high resemblances
    for(int i=0;i<number_lines;i++) {
        for (int j=i+1;j<number_lines;j++) {
            float resemblance = shingles_array[i]->resemblance_to(*shingles_array[j]);
            if (resemblance > min_resemblance)
                cout << i << " " << j << " " << resemblance << endl;
        }
    }

}
