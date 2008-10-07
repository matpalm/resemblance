#include <iostream>
#include <vector>
#include <regex.h>
#include <set>

#include "shingle.h"
#include "word_idx.h"

//#include "test.h"

using namespace std;

int main(int argc, char *argv[]) {

    const float min_resemblance = argc==1 ? 0 : atof(argv[1]);

    //run_tests();

    // splitting into shingles

    WordIdx word_idx;

    vector<Shingle*> data;
    typedef vector<Shingle*>::iterator s_iterator;

    // make a regex;
    regex_t re;
    regcomp(&re, "^[0-9]+", REG_EXTENDED);
    // read in all lines from stdin
    string nextLine;
    regmatch_t matches[1];
    int numMatches = 1;
    while(std::getline(cin, nextLine)) {
        // match line
        if (regexec(&re, nextLine.c_str(), numMatches, matches, 0)) {
            cerr << "expected each line to start with a number ?!?! [" << nextLine << "]" << endl;
            exit(1);
        }
        // bit after number is data to compare
        string nameAndAddr = nextLine.substr(matches[0].rm_eo+1);
        data.push_back(new Shingle(word_idx, nameAndAddr));
    }
    regfree(&re);

    // build bit representation
    for (s_iterator iter=data.begin(); iter!=data.end(); ++iter)
        (*iter)->build_bit_representation(word_idx.max_idx());

    // convert from list back to array
    const int number_lines = data.size();
    Shingle **shingles = new Shingle*[number_lines];
    for (int i=0;i<number_lines;i++)
        shingles[i] = data[i];

    for(int i=0;i<number_lines;i++) {
        for (int j=i+1;j<number_lines;j++) {
            float resemblance = shingles[i]->resemblance_to(*shingles[j]);
            if (resemblance > min_resemblance)
                cout << i << " " << j << " " << resemblance << endl;
        }
    }

//
//    for (s_iterator iter=data.begin(); iter!=data.end(); ++iter) {
//        iter -> build_bit_representation(word_idx.max_idx());
//        cout << *iter << endl;
//    }


}
