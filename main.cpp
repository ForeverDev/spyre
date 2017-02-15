#include <iostream>
#include "lex.h"
#include "tree.h"
#include "generate_c.h"

int main() {
	
	std::string filename = "demo.spy";
	std::string output = "demo.c";

	std::vector<Token> tokens = Lexer::generateTokens(filename);
	Parse_Context* tree = Parse_Context::generateSyntaxTree(filename, tokens);
	C_Code_Generator::generateCode(output, tree);

}
