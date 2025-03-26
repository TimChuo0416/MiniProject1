#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>


// for lex
#define MAXLEN 256

// Token types
typedef enum {
    UNKNOWN, END, ENDFILE,
    INT, ID,
    ADDSUB, MULDIV,
    ASSIGN,
    LPAREN, RPAREN, 
    OR, XOR, AND, INCDEC
} TokenSet;

TokenSet getToken(void);
TokenSet curToken = UNKNOWN;
char lexeme[MAXLEN];
char now[MAXLEN];
int reg_num,mem_num;
int assign_left = 0;//assign left
int divide_right = 0;
int right_id = 0;

// Test if a token matches the current token
int match(TokenSet token);
// Get the next token
void advance(void);
// Get the lexeme of the current token
char *getLexeme(void);


// for parser
#define TBLSIZE 64
// Set PRINTERR to 1 to print error message while calling error()
// Make sure you set PRINTERR to 0 before you submit your code
#define PRINTERR 1

// Call this macro to print error message and exit the program
// This will also print where you called it in your program
#define error(errorNum) { \
    if (PRINTERR) \
        fprintf(stderr, "error() called at %s:%d: ", __FILE__, __LINE__); \
    err(errorNum); \
}

// Error types
typedef enum {
    UNDEFINED, MISPAREN, NOTNUMID, NOTFOUND, RUNOUT, NOTLVAL, DIVZERO, SYNTAXERR
} ErrorType;

// Structure of the symbol table
typedef struct {
    int val;
    char name[MAXLEN];
} Symbol;

// Structure of a tree node
typedef struct _Node {
    TokenSet data;
    int val;
    int reg,mem;
    char lexeme[MAXLEN];
    struct _Node *left;
    struct _Node *right;
} BTNode;

int sbcount = 0;
Symbol table[TBLSIZE];

// Initialize the symbol table with builtin variables
void initTable(void);
// Get the value of a variable
int getval(char *str);
// Set the value of a variable
int setval(char *str, int val);
// Make a new node according to token type and lexeme
BTNode *makeNode(TokenSet tok, const char *lexe);
// Free the syntax tree
void freeTree(BTNode *root);
BTNode *factor(void);
BTNode *term(void);
BTNode *term_tail(BTNode *left);
BTNode *expr(void);
BTNode *expr_tail(BTNode *left);
//start

BTNode *assign_expr(void);
BTNode *or_expr(void);
BTNode *or_expr_tail(BTNode *left);
BTNode *xor_expr(void);
BTNode *xor_expr_tail(BTNode *left);
BTNode *and_expr(void);
BTNode *and_expr_tail(BTNode *left);
BTNode *addsub_expr(void);
BTNode *addsub_expr_tail(BTNode *left);
BTNode *muldiv_expr(void);
BTNode *muldiv_expr_tail(BTNode *left);
BTNode *unary_expr(void);
BTNode *new_factor();
void new_statement(void);
void printToken(void);
void clean();
//end
void statement(void);
// Print error message and exit the program
void err(ErrorType errorNum);


// for codeGen
// Evaluate the syntax tree
int evaluateTree(BTNode *root);
// Print the syntax tree in prefix
void printPrefix(BTNode *root);


/*============================================================================================
lex implementation
============================================================================================*/

TokenSet getToken(void)
{
    int i = 0;
    char c = '\0';

    while ((c = fgetc(stdin)) == ' ' || c == '\t');

    if (isdigit(c)) {
        lexeme[0] = c;
        c = fgetc(stdin);
        i = 1;
        while (isdigit(c) && i < MAXLEN) {
            lexeme[i] = c;
            ++i;
            c = fgetc(stdin);
        }
        ungetc(c, stdin);
        lexeme[i] = '\0';
        return INT;
    } else if (c == '+' || c == '-') {
        lexeme[0] = c;
        
        //
        c = fgetc(stdin);
        if (c == lexeme[0]) {
            lexeme[1] = c;
            ++i;
            lexeme[2] = '\0';
            return INCDEC;
        } else ungetc(c, stdin);
        //
        lexeme[1] = '\0';
        return ADDSUB;

    } else if (c == '*' || c == '/') {
        lexeme[0] = c;
        lexeme[1] = '\0';
        return MULDIV;
    } else if (c == '\n') {
        lexeme[0] = '\0';
        return END;
    } else if (c == '=') {
        strcpy(lexeme, "=");
        return ASSIGN;
    } else if (c == '(') {
        strcpy(lexeme, "(");
        return LPAREN;
    } else if (c == ')') {
        strcpy(lexeme, ")");
        return RPAREN;
    } else if (isalpha(c)) {
        lexeme[0] = c;
        //
        c = fgetc(stdin);
        i = 1;
        while (isalpha(c) || isdigit(c) || (c == '_') && i < MAXLEN) {
            lexeme[i] = c;
            ++i;
            c = fgetc(stdin);
        }
        ungetc(c, stdin);
        lexeme[i] = '\0';
        //
        return ID;
    } else if (c == EOF) {
        return ENDFILE;
    } 
    
    //
    else if (c == '|'){
        strcpy(lexeme,"|");
        return OR;
    } else if (c == '&'){
        strcpy(lexeme,"&");
        return AND;
    } else if (c == '^'){
        strcpy(lexeme,"^");
        return XOR;
    } 
    // 
    else {
        return UNKNOWN;
    }
}

void advance(void) {
    curToken = getToken();
    //printlex();
}

int match(TokenSet token) {
    if (curToken == UNKNOWN)
        advance();
    return token == curToken;
}

char *getLexeme(void) {
    return lexeme;
}


/*============================================================================================
parser implementation
============================================================================================*/

void initTable(void) {
    strcpy(table[0].name, "x");
    table[0].val = 0;
    strcpy(table[1].name, "y");
    table[1].val = 0;
    strcpy(table[2].name, "z");
    table[2].val = 0;
    sbcount = 3;
}

int getval(char *str) {
    int i = 0;

    for (i = 0; i < sbcount; i++){
        if (strcmp(str, table[i].name) == 0){
            mem_num = 4*i;
            return table[i].val;
        }
    }
    if (sbcount >= TBLSIZE)
        error(RUNOUT);
    if(!assign_left){
        error(UNDEFINED);
    }
    else{
        strcpy(table[sbcount].name, str);
        table[sbcount].val = 0;
        mem_num = (sbcount*4);
        sbcount++;
    }
    return 0;
}

int setval(char *str, int val) {
    int i = 0;

    for (i = 0; i < sbcount; i++) {
        if (strcmp(str, table[i].name) == 0) {
            table[i].val = val;
            return val;
        }
    }

    if (sbcount >= TBLSIZE)
        error(RUNOUT);

    strcpy(table[sbcount].name, str);
    table[sbcount].val = val;
    mem_num = (sbcount*4);
    sbcount++;
    return val;
}

BTNode *makeNode(TokenSet tok, const char *lexe) {
    BTNode *node = (BTNode*)malloc(sizeof(BTNode));
    strcpy(node->lexeme, lexe);
    node->data = tok;
    node->val = 0;
    node->left = NULL;
    node->right = NULL;
    return node;
}

void freeTree(BTNode *root) {
    if (root != NULL) {
        freeTree(root->left);
        freeTree(root->right);
        free(root);
    }
}

// factor := INT | ADDSUB INT |
//		   	 ID  | ADDSUB ID  |
//		   	 ID ASSIGN expr |
//		   	 LPAREN expr RPAREN |
//		   	 ADDSUB LPAREN expr RPAREN
BTNode *factor(void) {
    BTNode *retp = NULL, *left = NULL;

    if (match(INT)) {
        retp = makeNode(INT, getLexeme());
        advance();
    } else if (match(ID)) {
        left = makeNode(ID, getLexeme());
        advance();
        if (!match(ASSIGN)) {
            retp = left;
        } else {
            retp = makeNode(ASSIGN, getLexeme());
            advance();
            retp->left = left;
            retp->right = expr();
        }
    } else if (match(ADDSUB)) {
        retp = makeNode(ADDSUB, getLexeme());
        retp->left = makeNode(INT, "0");
        advance();
        if (match(INT)) {
            retp->right = makeNode(INT, getLexeme());
            advance();
        } else if (match(ID)) {
            retp->right = makeNode(ID, getLexeme());
            advance();
        } else if (match(LPAREN)) {
            advance();
            retp->right = expr();
            if (match(RPAREN))
                advance();
            else
                error(MISPAREN);
        } else {
            error(NOTNUMID);
        }
    } else if (match(LPAREN)) {
        advance();
        retp = expr();
        if (match(RPAREN))
            advance();
        else
            error(MISPAREN);
    } 
    //
    
    else {
        error(NOTNUMID);
    }
    return retp;
}

// term := factor term_tail
BTNode *term(void) {
    BTNode *node = factor();
    return term_tail(node);
}

// term_tail := MULDIV factor term_tail | NiL
BTNode *term_tail(BTNode *left) {
    BTNode *node = NULL;

    if (match(MULDIV)) {
        node = makeNode(MULDIV, getLexeme());
        advance();
        node->left = left;
        node->right = factor();
        return term_tail(node);
    } else {
        return left;
    }
}

// expr := term expr_tail
BTNode *expr(void) {
    BTNode *node = term();
    return expr_tail(node);
}

// expr_tail := ADDSUB term expr_tail | NiL
BTNode *expr_tail(BTNode *left) {
    BTNode *node = NULL;

    if (match(ADDSUB)) {
        node = makeNode(ADDSUB, getLexeme());
        advance();
        node->left = left;
        node->right = term();
        return expr_tail(node);
    } else {
        return left;
    }
}

//start

// assign_expr := ID ASSIGN assign_expr | or_expr 
BTNode *assign_expr(void){
    BTNode *retp = NULL,*left = NULL;
    //printlex();
    if(match(ID)){
        left = makeNode(ID, getLexeme());
        strcpy(now,getLexeme());
        advance();
        if(match(ASSIGN)){
            strcpy(now,"\0");
            retp = makeNode(ASSIGN, getLexeme());
            advance();
            retp->left = left;
            retp->right = assign_expr();
            return retp;
        }
    }
    retp = or_expr();
    //retp->left = left;

    return retp;
}
// or_expr := xor_expr or_expr_tail 
BTNode *or_expr(void){
    BTNode *node = xor_expr();
    return or_expr_tail(node);
}
// or_expr_tail := OR xor_expr or_expr_tail | NiL
BTNode *or_expr_tail(BTNode *left){
    BTNode *node = NULL;

    if(match(OR)){
        node = makeNode(OR,getLexeme());
        advance();
        node->left = left;
        node->right = xor_expr();
        return or_expr_tail(node);
    } else{
        return left;
    }
} 
// xor_expr := and_expr xor_expr_tail 
BTNode *xor_expr(void){
    BTNode *node = and_expr();
    return xor_expr_tail(node);
}
// xor_expr_tail := XOR and_expr xor_expr_tail | NiL 
BTNode *xor_expr_tail(BTNode *left){
    BTNode *node = NULL;

    if(match(XOR)){
        node = makeNode(XOR,getLexeme());
        advance();
        node->left = left;
        node->right = and_expr();
        return xor_expr_tail(node);
    } else{
        return left;
    }
}
// and_expr := addsub_expr and_expr_tail 
BTNode *and_expr(void){
    BTNode *node = addsub_expr();
    return and_expr_tail(node);
}
// and_expr_tail := AND addsub_expr and_expr_tail | NiL 
BTNode *and_expr_tail(BTNode *left){
    BTNode *node = NULL;
    
    if(match(AND)){
        node = makeNode(AND,getLexeme());
        advance();
        node->left = left;
        node->right = addsub_expr();
        return and_expr_tail(node);
    } else{
        return left;
    }
}
// addsub_expr := muldiv_expr addsub_expr_tail
BTNode *addsub_expr(void){
    BTNode *node = muldiv_expr();
    return addsub_expr_tail(node);
}
// addsub_expr_tail := ADDSUB muldiv_expr addsub_expr_tail | NiL 
BTNode *addsub_expr_tail(BTNode *left){
    BTNode *node = NULL;
    
    if(match(ADDSUB)){
        node = makeNode(ADDSUB,getLexeme());
        advance();
        node->left = left;
        node->right = muldiv_expr();
        return addsub_expr_tail(node);
    } else{
        return left;
    }
}
// muldiv_expr := unary_expr muldiv_expr_tail 
BTNode *muldiv_expr(void){
    BTNode *node = unary_expr();
    return muldiv_expr_tail(node);
}
// muldiv_expr_tail := MULDIV unary_expr muldiv_expr_tail | NiL 
BTNode *muldiv_expr_tail(BTNode *left){
    BTNode *node = NULL;
    
    if(match(MULDIV)){
        node = makeNode(MULDIV,getLexeme());
        advance();
        node->left = left;
        node->right = unary_expr();
        return muldiv_expr_tail(node);
    } else{
        return left;
    }
}
// unary_expr := ADDSUB unary_expr | factor 
BTNode* unary_expr(void){
    BTNode* node = NULL,*left = NULL;
    //printlex();
    if(strcmp(now,"\0") != 0){
        left = makeNode(ID,now);
        strcpy(now,"\0");
        return left;
    } else if(match(ADDSUB)){
        node = makeNode(ADDSUB,getLexeme());
        node->left = makeNode(INT,"0");
        advance();
        node->right = unary_expr();
    } else{
        node = new_factor();
    }
    return node;
}
// factor := INT | ID | INCDEC ID | LPAREN assign_expr RPAREN
BTNode *new_factor(){
    BTNode *retp = NULL, *left = NULL;
    if (match(INT)) {
        retp = makeNode(INT, getLexeme());
        advance();
    } else if(match(ID)){
        retp = makeNode(ID, getLexeme());
        advance();
    } else if(match(INCDEC)){
        retp = makeNode(INCDEC, getLexeme());
        retp->left = makeNode(INT, "0");
        advance();
        if(match(ID)){
            retp->right = makeNode(ID, getLexeme());
            advance();
        } else
            error(SYNTAXERR);
        
    } else if (match(LPAREN)) {
        advance();
        retp = assign_expr();
        if (match(RPAREN))
            advance();
        else
            error(MISPAREN);
    } else {
        error(UNDEFINED);
    }
    return retp;
}


//statement := ENDFILE | END | assign_expr END 
void new_statement(void) {
    BTNode *retp = NULL;
    int ans;
    if (match(ENDFILE)) {
        for(int i = 0; i <= 2;i++){
            printf("MOV r%d [%d]\n",i,4*i);
        }
        printf("EXIT 0");
        exit(0);
    } else if (match(END)) {
        //printf(">> ");
        advance();
    } else {
        retp = assign_expr();
        if (match(END)) {
            ans = evaluateTree(retp);
            /*
            printf("%d\n", ans);
            printf("Prefix traversal: ");
            printPrefix(retp);
            printf("\n");
            freeTree(retp);
            printf(">> ");
            */
            freeTree(retp);
            advance();
        } else {
            //printf("%c",*getLexeme());
            error(SYNTAXERR); 
        }
    }
}

// end

// statement := ENDFILE | END | expr END
void statement(void) {
    BTNode *retp = NULL;

    if (match(ENDFILE)) {
        exit(0);
    } else if (match(END)) {
        printf(">> ");
        advance();
    } else {
        retp = expr();
        if (match(END)) {
            printf("%d\n", evaluateTree(retp));
            printf("Prefix traversal: ");
            printPrefix(retp);
            printf("\n");
            freeTree(retp);
            printf(">> ");
            advance();
        } else {
            error(SYNTAXERR);
        }
    }
}
void printlex(){
    printf("lex:%c\n",*getLexeme());
}
void printToken(){
    for(TokenSet i = 0;i <= INCDEC;i++){
        if(curToken == i){
            printf("cur : %d\n",i);
            break;
        }
    }
}

void err(ErrorType errorNum) {
    if (PRINTERR) {
        fprintf(stderr, "error: ");
        switch (errorNum) {
            case MISPAREN:
                fprintf(stderr, "mismatched parenthesis\n");
                break;
            case NOTNUMID:
                fprintf(stderr, "number or identifier expected\n");
                break;
            case NOTFOUND:
                fprintf(stderr, "variable not defined\n");
                break;
            case RUNOUT:
                fprintf(stderr, "out of memory\n");
                break;
            case NOTLVAL:
                fprintf(stderr, "lvalue required as an operand\n");
                break;
            case DIVZERO:
                fprintf(stderr, "divide by constant zero\n");
                break;
            case SYNTAXERR:
                fprintf(stderr, "syntax error\n");
                break;
            default:
                fprintf(stderr, "undefined error\n");
                break;
        }
    }
    printf("EXIT 1\n");
    exit(0);
}


/*============================================================================================
codeGen implementation
============================================================================================*/

int evaluateTree(BTNode *root) {
    int retval = 0, lv = 0, rv = 0;
    if (root != NULL) {

        switch (root->data) {
            case ID:
                if(divide_right)
                    right_id = 1;
                retval = getval(root->lexeme);
                
                root->reg = reg_num;
                root->mem = mem_num;
                if(!assign_left){
                    if(root->reg >= 8){
                        error(UNDEFINED);
                    }
                    printf("MOV r%d [%d]\n",root->reg,root->mem);
                    
                    reg_num++;
                }
                break;

            case INT:
                
                retval = atoi(root->lexeme);
                root->reg = reg_num;
                if(root->reg >= 8){
                    error(UNDEFINED);
                }
                printf("MOV r%d %d\n",root->reg,retval);
                
                reg_num++;

                break;

            case ASSIGN:
                
                rv = evaluateTree(root->right);
                assign_left = 1;
                lv = evaluateTree(root->left);
                retval = setval(root->left->lexeme, rv);
                printf("MOV [%d] r%d\n",root->left->mem,root->right->reg);
                reg_num--;
                root->reg = root->right->reg;
                break;

            case ADDSUB:
                
                lv = evaluateTree(root->left);
                rv = evaluateTree(root->right);
                
                if (strcmp(root->lexeme, "+") == 0) {
                    retval = lv + rv;
                    printf("ADD r%d r%d\n", root->left->reg ,root->right->reg);
                    root->reg = root->left->reg;
                    reg_num --;
                } else if (strcmp(root->lexeme, "-") == 0) {
                    retval = lv - rv;
                    printf("SUB r%d r%d\n", root->left->reg ,root->right->reg);
                    root->reg = root->left->reg;
                    reg_num --;
                } break;

            case MULDIV:
                
                
                
                if (strcmp(root->lexeme, "*") == 0) {
                    lv = evaluateTree(root->left);
                    rv = evaluateTree(root->right);
                    retval = lv * rv;
                    printf("MUL r%d r%d\n", root->left->reg ,root->right->reg);
                    root->reg = root->left->reg;
                    reg_num --;

                } else if (strcmp(root->lexeme, "/") == 0) {
                    lv = evaluateTree(root->left);
                    divide_right = 1;
                    rv = evaluateTree(root->right);
                    if (rv == 0 && right_id == 0){
                        error(DIVZERO);
                    }
                    else if(rv != 0){
                        retval = lv / rv;
                    }
                    printf("DIV r%d r%d\n", root->left->reg ,root->right->reg);
                    root->reg = root->left->reg;
                    reg_num --;
                } break;

            //start
            case OR:
                
                lv = evaluateTree(root->left);
                rv = evaluateTree(root->right);
                if (strcmp(root->lexeme, "|") == 0) {
                    retval = lv | rv;
                    printf("OR r%d r%d\n", root->left->reg ,root->right->reg);
                    root->reg = root->left->reg;
                    reg_num --;
                } break;

            case XOR:
                
                lv = evaluateTree(root->left);
                rv = evaluateTree(root->right);
                if (strcmp(root->lexeme, "^") == 0) {
                    retval = lv ^ rv;
                    printf("XOR r%d r%d\n", root->left->reg ,root->right->reg);
                    root->reg = root->left->reg;
                    reg_num --;
                } break;

            case AND:
                
                lv = evaluateTree(root->left);
                rv = evaluateTree(root->right);
                if (strcmp(root->lexeme, "&") == 0) {
                    retval = lv & rv;
                    printf("AND r%d r%d\n", root->left->reg ,root->right->reg);
                    root->reg = root->left->reg;
                    reg_num --;
                } break;

            case INCDEC:
                
                rv = evaluateTree(root->right);
                printf("MOV r%d 1\n",root->right->reg + 1);
                if(strcmp(root->lexeme, "++") == 0){
                    
                    retval = setval(root->right->lexeme,rv + 1);  
                    printf("ADD r%d r%d\n",root->right->reg,root->right->reg + 1);
                    printf("MOV [%d] r%d\n",root->right->mem,root->right->reg);
                    root->reg = root->right->reg;

                } else if (strcmp(root->lexeme, "--") == 0){
                    
                    retval = setval(root->right->lexeme,rv-1);       
                    printf("SUB r%d r%d\n",root->right->reg, root->right->reg + 1);
                    printf("MOV [%d] r%d\n",root->right->mem,root->right->reg);
                    root->reg = root->right->reg;

                } break;
                
            default:
                retval = 0;
        }
    }
    return retval;
}

void printPrefix(BTNode *root) {
    if (root != NULL) {
        printf("%s ", root->lexeme);
        printPrefix(root->left);
        printPrefix(root->right);
    }
}

void clean(){
    
    reg_num = 0;
    mem_num = 0;
    right_id = 0;
    assign_left = 0;

    divide_right = 0;
}


/*============================================================================================
main
============================================================================================*/

// This package is a calculator
// It works like a Python interpretor
// Example:
// >> y = 2
// >> z = 2
// >> x = 3 * y + 4 / (2 * z)
// It will print the answer of every line
// You should turn it into an expression compiler
// And print the assembly code according to the input

// This is the grammar used in this package
// You can modify it according to the spec and the slide
// statement  :=  ENDFILE | END | expr END
// expr    	  :=  term expr_tail
// expr_tail  :=  ADDSUB term expr_tail | NiL
// term 	  :=  factor term_tail
// term_tail  :=  MULDIV factor term_tail| NiL
// factor	  :=  INT | ADDSUB INT |
//		   	      ID  | ADDSUB ID  |
//		   	      ID ASSIGN expr |
//		   	      LPAREN expr RPAREN |
//		   	      ADDSUB LPAREN expr RPAREN

int main() {
    freopen("input.txt","r",stdin);
    freopen("output.txt","w",stdout);
    initTable();
    //printf(">> ");
    while (1) {
        new_statement();
        clean();
    }
    return 0;
}
