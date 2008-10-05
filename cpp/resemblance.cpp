#include <iostream>
#include <vector>
#include <regex.h>
#include <set>

#include "shift.h"
#include "shingle.h"
#include "word_idx.h"

#include "test.h"

using namespace std;

// all lines of input

int main(int argc, char *argv[]) {

    //run_tests();

    // splitting into shingles

    WordIdx wordIdx;

    string s = "abcdefdeghi";
    Shingle b(wordIdx, s);
    cout << s  << endl;

    exit(1);

/*
vector<Shingle> data;
typedef vector<Shingle>::iterator s_iterator;

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

        // first part is id
        //string numStr = nextLine.substr(matches[0].rm_so, matches[0].rm_eo);
        //int num = atoi(numStr.c_str());

        // bit after number is data to compare
        string nameAndAddr = nextLine.substr(matches[0].rm_eo+1);
        data.push_back(Shingle(nameAndAddr));
    }

    for (s_iterator iter=data.begin(); iter!=data.end(); ++iter) {
        cout << iter->line << endl;
    }

    regfree(&re);
*/
}
