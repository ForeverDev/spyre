#include <stdlib.h>
#include <iostream>
#include <cctype>
#include "lex.h"

Token::Token() {

}

Token::~Token() {

}

Lexer::Lexer(const std::string& filename): handle(filename) {
	line = 1;
	if (!handle.is_open()) {
		std::cout << "couldn't open '" << filename << "' for reading!\n";
		exit(1);
	}
}

Lexer::~Lexer() {
	handle.close();
}

void
Lexer::die(const std::string& message) {
	std::cout << "!!! LEX ERROR !!!\n";
	std::cout << "\tmessage: " << message << std::endl;
	std::cout << "\tline: " << line << std::endl;
	exit(1);
}

char
Lexer::get() {
	char c = handle.get();
	if (!handle.good()) {
		die("unexpected EOF");
	}
	return c;
}

void
Lexer::eat() {
	handle.get();
}

void
Lexer::eat(char c) {
	if (handle.get() != c) {
		std::cout << "expected character '" << c << "'\n";
		exit(1);
	}
}

char
Lexer::peek() {
	return handle.peek();
}

void
Lexer::handleNumber() {
	std::string buf;
	Token t;
	t.line = line;
	/* can't define type yet, we don't know if it's a float or integer */
	while (std::isdigit(peek())) {
		buf += get();
	}
	t.type = (peek() == '.') ? TOK_FLOAT : TOK_INTEGER;
	if (t.type == TOK_INTEGER) {
		t.i = std::stoll(buf);
	} else {
		/* if it's a double, continue eating digits */
		buf += get(); /* append the '.' */
		if (!std::isdigit(peek())) {
			die("expected digit to follow token '.'");
		}
		while (std::isdigit(peek())) {
			buf += get();
		}
		t.f = std::stod(buf);
	}
	t.word = buf;
	t.line = line;
	tokens.push_back(t);
}

void
Lexer::handleIdentifier() {
	Token t;
	t.line = line;
	t.type = TOK_IDENTIFIER;
	std::string buf;
	while (true) {
		char p = peek();
		if (!isalnum(p) && p != '_') {
			break;
		}
		buf += get();
	}
	t.id = buf;
	t.word = buf;
	t.line = line;
	if (t.word == "new") {
		t.type = TOK_OPERATOR;
		t.i = 1000; // idk
	}
	tokens.push_back(t);
}

void
Lexer::handleOperator() {
	
	static const std::vector<std::string> long_ops {
		">>",  // 100
		"<<",  // 101
		"&&",
		"||",
		"==",
		"!=",
		"+=",
		":=",
		"-=",
		"*=",
		"/=",
		"%=",
		"&=",
		"|=",
		"^=",
		"->",
		">=",
		"<=",
		""
	};

	auto is_long = [](const std::string& op_name) -> int {
		int i = 0;
		for (const std::string& check: long_ops) {
			if (op_name == check) {
				return i + 100;	
			}
			i++;
		}
		return -1;
	};

	Token t;
	t.line = line;
	t.type = TOK_OPERATOR;
	char c = get();
	char p = peek();
	char index;
	std::string both;
	both += c;
	both += p;
	t.word += c;
	if ((index = is_long(both)) != -1) {
		eat();	
		t.i = index;
		t.word += p;
	} else {
		t.i = static_cast<long long>(c); // cast prob not needed, autopromote
	}
	
			
	tokens.push_back(t);

}

void
Lexer::handleString() {
	Token t;
	t.line = line;
	t.type = TOK_STRING;
	std::string buf;
	eat(); // eat "
	while (true) {
		char p = peek();
		if (p == '"') {
			break;
		}
		buf += get();
	}
	eat(); // eat closing "
	t.id = buf;
	t.word += '"';
	t.word += buf;
	t.word += '"';
	t.line = line;
	tokens.push_back(t);
}

void
Lexer::printTokens() {
	for (auto& t: tokens) {
		std::cout << (int)t.type << " " << t.word << std::endl;
	}
}

void
Lexer::dispatchFromChar(char c) {
	if (std::isspace(c)) {
		if (c == '\n') {
			line++;
		}
		eat();
	} else if (c == '"') {
		handleString();
	} else if (std::isdigit(c)) {
		handleNumber();
	} else if (std::ispunct(c)) {
		handleOperator();
	} else if (std::isalpha(c) || c == '_') {
		handleIdentifier();
	} else {
		eat();
	}
}

std::vector<Token>
Lexer::generateTokens(const std::string& filename) {
	
	Lexer lex(filename);	
	char c;

	while (lex.handle.good()) {
		lex.dispatchFromChar(lex.peek());
	}

	return lex.tokens;

}
