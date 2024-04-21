/* 20230490, Donghyun Lee, ee209_assignment1 */

#include <stdio.h>
#include <ctype.h>

int main(void) {
    int c;  // c: each char
    int lines = 0, words = 0, characters = 0;  // lines: # of line, words: # of words, characters: # of characters
    int is_word = 0;  // decide increase word or not;
                        // record pre_char if it is blank(is_word = 0) or not(is_word = 1) -> increase word if (is_word && isspace(c))
    int state = 0;  // 0: normal, 1: / (prepare comment), 2: // (comment_1), 3: /* (comment_2), 4: * (prepare end of comment_2)
    int comment_line = 0; // record the last comment_2 start; use while print error (comment_2 didn't terminate)

    while ((c = getchar()) != EOF) {    // get each char until txt.file END
        /* \n always increase line and char*/
        if (c == '\n') {
            lines++;
            characters++;
        }

        switch (state) {
            /* normal state */
            case 0:
                words += (is_word && isspace(c));
                if (c == '/') {
                    state = 1;
                }
                else {
                    characters++;
                    /* set is_word */
                    if (isspace(c)) {
                        is_word = 0;
                    }
                    else {
                        is_word = 1;
                    }
                }
                /* consider \n increase line and char before SWITCH */
                if (c == '\n') {
                    characters--;
                }
                break;
            /* prepare comment */
            case 1:
                if (c == '/') {
                    state = 2;
                }
                else if (c == '*') {
                    characters++;
                    state = 3;
                    words += is_word;
                    is_word = 0;
                    comment_line = lines + 1;
                }
                else {
                    state = 0;
                    characters += 2;
                    if (isspace(c)) {
                        words++;
                        is_word = 0;
                        if (c == '\n') {
                            characters--;
                        }
                    }
                    else {
                        is_word = 1;
                    }
                }
                break;
            /* comment_1 */
            case 2:
                if (c == '\n') {
                    words += is_word;
                    is_word = 0;
                    state = 0;
                }
                break;
            /* comment_2 */
            case 3:
                if (c == '*') {
                    state = 4;
                }
                break;
            /* end the comment_2 */
            case 4:
                if (c == '/') {
                    state = 0;
                }
                else if (c != '*') {
                    state = 3;
                }
                break;
        }
    }

    if (state == 3 || state == 4) { // ends until comment not fin.
        fprintf(stderr, "Error: line %d: unterminated comment\n", comment_line);
        return 0;
    }
    printf("%d %d %d\n", lines, words + is_word, characters);
    return 0;
}