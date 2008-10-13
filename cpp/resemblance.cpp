#include <iostream>
#include <sstream>
#include <fstream>
#include <vector>
#include <set>

#include "shingle.h"
#include "word_idx.h"
#include "parser.h"

#ifdef _OPENMP
#include <omp.h>
#endif

using namespace std;

const int WINDOW_SIZE = 300;

string output_filename_for_thread() {
    stringstream str;
    str << "resemblance";
#ifdef _OPENMP
    str << "." << omp_get_thread_num();
#endif
    str << ".out";
    return str.str();
}

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

    #pragma omp parallel num_threads(4)
    {

        ofstream file(output_filename_for_thread().c_str());

        #pragma omp for schedule(dynamic, 1000)
        for(int i=0;i<number_lines;i++) {

            int window_start = i+1;
            int window_finish = window_start + WINDOW_SIZE;
            if (window_finish > number_lines) window_finish = number_lines;

            // compare each pair output high resemblances
            for (int j=window_start; j<window_finish; j++) {
                float resemblance = shingles_array[i]->resemblance_to(*shingles_array[j]);
                if (resemblance > min_resemblance)
                    file << i << " " << j << " " << resemblance << endl;
            }
        }

        file.close();
    }
}
