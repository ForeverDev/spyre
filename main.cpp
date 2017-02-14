#include <iostream>
#include "lex.h"
#include "tree.h"

int main() {
	
	std::string filename = "demo.spy";
	std::vector<Token> tokens = Lexer::generateTokens(filename);
	Parse_Context* tree = Parse_Context::generateSyntaxTree(filename, tokens);

}
