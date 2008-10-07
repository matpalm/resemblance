#include "parser.h"
#include <regex.h>

void Parser::parse(vector<Shingle*> &shingles, WordIdx &word_idx) {
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
        shingles.push_back(new Shingle(word_idx, nameAndAddr));
    }
    regfree(&re);
}

