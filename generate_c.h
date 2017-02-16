#ifndef GENERATE_C_H
#define GENERATE_C_H

#include <string>
#include <fstream>
#include "tree.h"

class C_Code_Generator {

	public:
		C_Code_Generator();
		static void generateCode(const std::string&, Parse_Context*);
		void writeHeaders();
		void writeTypedefs();
		void writeRegisters();
		void writeProcedure();
		void writeBlock();
		void writeStatement();
		void writeComment(const std::string&);
		void writeLinedComment(const std::string&);
		int writeExpression(Expression*);
		void writeNode(Ast_Node*);
		void writeReturn();
		void writeIf();
		void writeLiterals();
		void writeWhile();
		void writeFor();
		void writeBreak();
		void writeContinue();
		void writeGoto(int);
		std::string constructDatatype(Datatype_Base*);
		std::string constructDatatypeForC(Datatype_Base*);
		std::string constructDeclaration(Var_Declaration*);
		std::string constructRegister(int);
		std::string constructLabel(int);
		std::string constructLiteral(int);
		std::string registerBare(int);
		std::string registerField(int, const std::string&);
		void zeroRegister(int);
		void setRegisterValue(int, const std::string&, const std::string&);
		void setRegisterValueWithCast(int, const std::string&, const std::string&, const std::string&);
		void incrementRegisterValue(int, const std::string&, const std::string&);
		void simpleRegisterMove(int, int);
		void defineLabel(int);
		std::string dereferenceRegister(int, const std::string&);
		
		static const int NUM_REGS = 12;
		static const bool OPT_USELESS = true;
		int label_count = 0;
		int break_label = 0;
		int continue_label = 0;
		std::string union_typename;	
		std::string c_int_type;
		std::string c_float_type;
		std::string c_byte_type;
		Parse_Context* context;
		std::ofstream output; 

};

#endif
