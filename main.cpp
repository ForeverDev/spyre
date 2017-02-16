#include <iostream>
#include <stdio.h>
#include "lex.h"
#include "tree.h"
#include "generate_c.h"

int main() {
	
	std::string filename = "demo.spy";
	std::string output = "demo.c";

	std::vector<Token> tokens = Lexer::generateTokens("demo.spy");
	Parse_Context* tree = Parse_Context::generateSyntaxTree("demo.spy", tokens);
	C_Code_Generator::generateCode("demo.c", tree);

	system("gcc demo.c -o demo.exe -w -O0 -g");

}
