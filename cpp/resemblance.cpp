#include <iostream>
#include <vector>
#include <regex.h>
#include <set>

#include "shingle.h"
#include "word_idx.h"
#include "bit_utils.h"

//#include "test.h"

using namespace std;

// all lines of input

int main(int argc, char *argv[]) {

    //run_tests();

    // splitting into shingles

    WordIdx word_idx;

//    Shingle s1(word_idx, "abcdefdeghidifvuhdfivuhwejbrkjv");
//    Shingle s2(word_idx, "defdesdfghisyvuksdfkjxhfkxsbhdf");
//    Shingle s3(word_idx, "983749534");
//    cout << "max " << word_idx.max_idx() << endl;
//    s1.build_bit_representation(word_idx.max_idx());
//    s2.build_bit_representation(word_idx.max_idx());
//    s3.build_bit_representation(word_idx.max_idx());
//    cout << s1.resemblance_to(s1) << endl; // 1
//    cout << s2.resemblance_to(s2) << endl; // 1
//    cout << s1.resemblance_to(s2) << endl; // mid
//    cout << s2.resemblance_to(s1) << endl; // mid
//    cout << s1.resemblance_to(s3) << endl; // 0
//    cout << s2.resemblance_to(s3) << endl; // 0
//    exit(0);

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
            cout << i << " " << j << " " << shingles[i]->resemblance_to(*shingles[j]) << endl;
        }
    }

//
//    for (s_iterator iter=data.begin(); iter!=data.end(); ++iter) {
//        iter -> build_bit_representation(word_idx.max_idx());
//        cout << *iter << endl;
//    }


}
