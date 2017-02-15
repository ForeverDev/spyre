#include <iostream>
#include <functional>
#include <stdlib.h>
#include "generate_c.h"

std::string
C_Code_Generator::registerBare(int reg_index) {
	return constructRegister(reg_index);
}

std::string
C_Code_Generator::registerField(int reg_index, const std::string& field) {
	return constructRegister(reg_index) + "." + field;
}

void
C_Code_Generator::zeroRegister(int reg_index) {
	output << registerField(reg_index, "i") << " = 0;\n";
}

void
C_Code_Generator::setRegisterValue(int reg_index, const std::string& field, const std::string& value) {
	output << registerField(reg_index, field) << " = " << value << ";\n";
}

void
C_Code_Generator::setRegisterValueWithCast(int reg_index, const std::string& field, const std::string& value, const std::string& target) {
	output << registerField(reg_index, field) << " = (" << target << ")" << value << ";\n";
}

void
C_Code_Generator::incrementRegisterValue(int reg_index, const std::string& field, const std::string& amount) {
	output << registerField(reg_index, field) << " += " << amount << ";\n";
}

void
C_Code_Generator::simpleRegisterMove(int target, int from) {
	output << registerField(target, "i") << " = " << registerField(from, "i") << ";\n";
}

std::string
C_Code_Generator::dereferenceRegister(int reg_index, const std::string& ctype) {
	return "*(" + ctype + " *)" + registerField(reg_index, "i");
}

C_Code_Generator::C_Code_Generator() {

}

std::string
C_Code_Generator::constructDatatypeForC(Datatype_Base* dt) {
	if (dt->is_ptr) {
		return "void*";
	} else if (dt->isRawFloat()) {
		return c_float_type;
	} else if (dt->isRawByte()) {
		return c_byte_type;
	}
	return c_int_type;
}

std::string
C_Code_Generator::constructDatatype(Datatype_Base* dt) {
	std::string ret = "";
	switch (dt->type) {
		case DATA_INTEGER:
		case DATA_FLOAT:
		case DATA_BYTE:
		case DATA_STRUCT:
			ret += union_typename;
			break;
		case DATA_VOID:
			return "void";
	}
	size_t combined = dt->ptr_dim + dt->size_array.size();
	/*
	if (combined > 0) {
		ret += " ";
	}
	for (int i = 0; i < dt->ptr_dim + dt->size_array.size(); i++) {
		ret += "*";
	}
	*/
	return ret;
}

std::string
C_Code_Generator::constructDeclaration(Var_Declaration* decl) {
	std::string basic = constructDatatype(decl->dt);
	if (basic[basic.length() - 1] != '*') {
		basic += " ";
	}
	basic += decl->identifier;
	if (decl->dt->isRawStruct()) {
		basic += "[";
		basic += std::to_string(dynamic_cast<Datatype_Struct *>(decl->dt)->total_members);
		basic += "]";
	}
	return basic;
}

std::string
C_Code_Generator::constructRegister(int reg_index) {
	std::string ret = "R[";
	ret += std::to_string(reg_index);
	ret += "]";
	return ret;
}

std::string
C_Code_Generator::constructLiteral(int index) {
	return "LIT[" + std::to_string(index) + "]";
}

std::string
C_Code_Generator::constructLabel(int label) {
	return "L" + std::to_string(label);
}

void
C_Code_Generator::defineLabel(int label) {
	output << constructLabel(label) << ":\n"; 
}

void
C_Code_Generator::writeHeaders() {
	output << "#include <stdio.h>\n";
	output << "#include <stdlib.h>\n";
	output << "#include <stdint.h>\n\n";
}

void
C_Code_Generator::writeTypedefs() {
	output << (
		"typedef union _SPY_UNION_TAG_ {\n"
		"\tint64_t i;\n"
		"\tdouble f;\n"
		"\tuint8_t b;\n"
		"} " + union_typename + ";\n\n"
	);
}

void
C_Code_Generator::writeLiterals() {
	output << "char* LIT[] = {\n";
	int size = context->string_literals.size();
	for (int i = 0; i < size; i++) {
		std::string& lit = context->string_literals[i];
		output << "\t" << lit;
		if (i < size - 1) {
			output << ",";
		}
		output << std::endl;
	}
	output << "};\n\n";
}	

void
C_Code_Generator::writeRegisters() {
	output << union_typename << " " << constructRegister(NUM_REGS) << " = { 0 };\n\n";
}

void
C_Code_Generator::writeComment(const std::string& message) {
	output << "// " << message << std::endl;
}

void
C_Code_Generator::writeLinedComment(const std::string& message) {
	output << "// line " << context->current_node->line << "; " << message << std::endl;
}

void
C_Code_Generator::writeProcedure() {
	Ast_Procedure* proc = context->current_procedure;
	Datatype_Procedure* desc = proc->desc;
	
	int length;
	int cut;
	std::string header;	
	writeLinedComment("procedure implementation");
	writeComment(proc->identifier + ": " + desc->as_string);
	if (proc->identifier != "main") {
		header += "static ";
	}
	header += constructDatatype(desc->ret);	
	header += "\n";
	cut = header.length();
	header += proc->identifier;
	header += "(";
	length = header.length() - cut;
	output << header;

	size_t arg_count = desc->args.size();
	for (int i = 0; i < arg_count; i++) {
		output << constructDeclaration(desc->args[i]);
		if (i < arg_count - 1) {
			output << ", ";
			output << std::endl;
			for (int j = 0; j < length; j++) {
				output << " ";
			}
		}
	}

	output << ") {\n\n";

	writeRegisters();
	
	std::function<void (Ast_Node*)> declare_all_locals = [&](Ast_Node* parent) -> void {
		switch (parent->type) {
			case AST_BLOCK: {
				Ast_Block* block = dynamic_cast<Ast_Block *>(parent);
				for (Var_Declaration* decl: block->locals) {
					output << constructDeclaration(decl) << ";\n";
				}
				for (Ast_Node* child: block->children) {
					declare_all_locals(child);
				}
				break;
			}
			case AST_IF:
				declare_all_locals(dynamic_cast<Ast_If *>(parent)->child);
				break;
			case AST_WHILE:
				declare_all_locals(dynamic_cast<Ast_While *>(parent)->child);
				break;
			case AST_FOR:
				declare_all_locals(dynamic_cast<Ast_For *>(parent)->child);
				break;
		}
	};

	declare_all_locals(proc->child);

	writeNode(proc->child);	
	writeComment("default return");
	zeroRegister(0);	
	output << "return_label:\n";
	if (desc->ret->type == DATA_VOID) {
		output << "return;\n";
	} else {
		// whatever is in R0 is returned
		output << "return " << registerBare(0) << ";\n";
	}

	output << "\n}\n\n";
}

void
C_Code_Generator::writeReturn() {
	Ast_Return* ret = dynamic_cast<Ast_Return *>(context->current_node);	
	writeLinedComment("return statement");
	if (ret->expression) {
		int result = writeExpression(ret->expression);
		simpleRegisterMove(0, result);
	}
	output << "goto return_label;\n";
}

void
C_Code_Generator::writeIf() {
	Ast_If* if_node = dynamic_cast<Ast_If *>(context->current_node);
	int bottom_label = label_count++;
	writeLinedComment("if statement");
	writeComment("bottom label: " + std::to_string(bottom_label));
	int result = writeExpression(if_node->cond);
	simpleRegisterMove(0, result);
	output << "if (!" << registerField(0, "i") << ") ";
	writeGoto(bottom_label);
	writeNode(if_node->child);
	defineLabel(bottom_label);
}

void
C_Code_Generator::writeWhile() {
	Ast_While* while_node = dynamic_cast<Ast_While *>(context->current_node);
	int top_label = label_count++;
	int bottom_label = label_count++;
	writeLinedComment("while loop");
	writeComment("top label:    " + std::to_string(top_label));
	writeComment("bottom label: " + std::to_string(bottom_label));
	defineLabel(top_label);
	int result = writeExpression(while_node->cond);
	simpleRegisterMove(0, result);
	output << "if (!" << registerField(0, "i") << ") ";
	writeGoto(bottom_label);
	writeNode(while_node->child);
	writeGoto(top_label);
	defineLabel(bottom_label);
}

void
C_Code_Generator::writeFor() {
	Ast_For* for_node = dynamic_cast<Ast_For *>(context->current_node);
	int top_label = label_count++;
	int bottom_label = label_count++;
	writeLinedComment("for loop");
	writeComment("top label:    " + std::to_string(top_label));
	writeComment("bottom label: " + std::to_string(bottom_label));
	writeExpression(for_node->init);
	defineLabel(top_label);
	int result = writeExpression(for_node->cond);
	simpleRegisterMove(0, result);
	output << "if (!" << registerField(0, "i") << ") ";
	writeGoto(bottom_label);
	writeNode(for_node->child);
	writeComment("bottom for loop");
	writeExpression(for_node->each);
	writeGoto(top_label);
	defineLabel(bottom_label);
}

void
C_Code_Generator::writeGoto(int label) {
	output << "goto " << constructLabel(label) << ";\n";
}

void
C_Code_Generator::writeBlock() {
	for (Ast_Node* child: context->current_block->children) {
		writeNode(child);
	}
}

void
C_Code_Generator::writeStatement() {
	Ast_Statement* state = dynamic_cast<Ast_Statement *>(context->current_node);
	writeLinedComment("statement");
	writeExpression(state->expression);
}

int
C_Code_Generator::writeExpression(Expression* exp) {

	bool occupied_registers[NUM_REGS] = { false };

	auto get_register = [&]() -> int {
		for (int i = 0; i < NUM_REGS; i++) {
			if (!occupied_registers[i]) {
				occupied_registers[i] = true;
				return i;
			}
		}
		context->die("!!! out of register space !!!");
	};

	auto free_register = [&](int reg_index) -> void {
		occupied_registers[reg_index] = false;
	};

	auto make_function_cast = [&](Datatype_Procedure* target) -> std::string {
		// when we do a function call, the address of the function is stored
		// in a __SPYRE_UNION__, therefore, the type information has been lost.
		// Therefore, we need to create a cast back to the correct type
		bool is_foreign = (target->mods & MOD_FOREIGN) != 0x0;
		std::string func = "";
		if (is_foreign) {
			func += constructDatatypeForC(target->ret);
		} else {
			func += constructDatatype(target->ret);
		}
		func += "(*)(";
		size_t nargs = target->args.size();
		for (int i = 0; i < nargs; i++) {
			if (is_foreign) {
				func += constructDatatypeForC(target->args[i]->dt);
			} else {
				func += constructDatatype(target->args[i]->dt);
			}
			if (i < nargs - 1) {
				func += ", ";
			} else if (is_foreign && target->is_vararg) {
				func += ", ...";
			}
		}
		func += ")";
		return func;
	};
	
	auto get_prefix = [&](Datatype_Base* dt) -> std::string {
		if (!dt) {
			return "i";
		}
		if (dt->isRawFloat()) {
			return "f";
		}
		return "i";
	};
	
	// must be explicit, do_generate is recursive
	std::function<int (Expression*)> do_generate = [&](Expression* exp) -> int {
		Expression* parent = exp->parent;
		bool is_left_assign = parent != nullptr && parent->isAssignment() && exp->side == LEAF_LEFT;
		bool parent_is_dot = parent != nullptr && parent->isBinaryType(BINARY_MEMBER_DEREFERENCE);
		bool parent_parent_is_dot = parent_is_dot && parent->parent != nullptr && parent->parent->isBinaryType(BINARY_MEMBER_DEREFERENCE);
		bool dont_dereference = is_left_assign || parent_is_dot;
		switch (exp->type) {
			case EXP_INTEGER: {
				Expression_Integer* i = dynamic_cast<Expression_Integer *>(exp);
				int reg = get_register();
				setRegisterValue(reg, "i", std::to_string(i->value));
				return reg;
			}
			case EXP_FLOAT: {
				Expression_Float* f = dynamic_cast<Expression_Float *>(exp);
				int reg = get_register();
				setRegisterValue(reg, "f", std::to_string(f->value));
				return reg;
			}
			case EXP_IDENTIFIER: {
				Expression_Identifier* i = dynamic_cast<Expression_Identifier *>(exp);
				if (!i->var) {
					// this should never happen.... right?
					context->die("idfk panic!!!");
				}
				int reg = get_register();
				if (i->eval->isRawProcedure()) {
					setRegisterValueWithCast(reg, "i", i->value, "intptr_t");
				} else if (is_left_assign || parent_is_dot) {
					if (i->eval->isRawStruct()) {
						setRegisterValueWithCast(reg, "i", i->value, "intptr_t");	
					} else {
						setRegisterValueWithCast(reg, "i", "&" + i->value, "intptr_t");	
					}
				} else {
					setRegisterValue(reg, "i", i->value + ".i");
				}
				return reg;
			}
			case EXP_STRING: {
				int reg = get_register();
				Expression_String* str = dynamic_cast<Expression_String *>(exp);
				setRegisterValueWithCast(reg, "i", constructLiteral(str->literal_index), "intptr_t");
				return reg;
			}
			case EXP_CALL: {
				Expression_Call* call = dynamic_cast<Expression_Call *>(exp);
				Datatype_Procedure* proc = dynamic_cast<Datatype_Procedure *>(call->procedure->eval);
				std::string cast = make_function_cast(proc);
				int r0 = do_generate(call->procedure);	
				std::string built_call = "";
				if (call->argument) {
					// base register for call is r1, final register is (r1 + num_args)
					int r1 = do_generate(call->argument);
					built_call += "((";
					built_call += cast;
					built_call += ")";
					built_call += registerField(r0, "i");
					built_call += ")(";
					for (int i = 0; i < call->num_args; i++) {
						int arg_reg = r1 + i;
						built_call += registerBare(arg_reg);
						if (proc->mods & MOD_FOREIGN) {
							built_call += ".i";
						}
						if (i < call->num_args - 1) {
							built_call += ",";
						}
					}
					built_call += ")";
					if (!(proc->mods & MOD_FOREIGN) && proc->ret->type != DATA_VOID) {
						built_call += ".";
						built_call += get_prefix(proc->ret);
					}
					for (int i = 0; i < call->num_args; i++) {
						free_register(r1 + i);
					}
					if (proc->ret->type != DATA_VOID) {
						setRegisterValue(r0, "i", built_call);
					} else {
						output << built_call << ";\n";
					}
				}
				if (proc->ret->type == DATA_VOID) {
					free_register(r0);
					return -1;
				}
				return r0;
			}
			case EXP_BINARY: {
				Expression_Binary* binop = dynamic_cast<Expression_Binary *>(exp);
				std::string prefix_result = get_prefix(binop->eval);
				std::string prefix_left = get_prefix(binop->left->eval);
				std::string prefix_right = get_prefix(binop->right->eval);
				switch (binop->optype) {
					case BINARY_COMMA: {
						int r0 = do_generate(binop->left);
						int r1 = do_generate(binop->right);
						return r0;
					}
					case BINARY_ADDITION: 
					case BINARY_SUBTRACTION:
					case BINARY_MULTIPLICATION:
					case BINARY_DIVISION: 
					case BINARY_MODULUS:
					case BINARY_SHIFT_LEFT:
					case BINARY_SHIFT_RIGHT:
					case BINARY_BITWISE_AND:
					case BINARY_BITWISE_OR:
					case BINARY_BITWISE_XOR:
					case BINARY_GREATER_THAN:
					case BINARY_LESS_THAN:
					case BINARY_GREATER_THAN_OR_EQUAL:
					case BINARY_LESS_THAN_OR_EQUAL:
					case BINARY_COMPARE:
					case BINARY_COMPARE_NOT:
					{
						int r0 = do_generate(binop->left);
						int r1 = do_generate(binop->right);
						setRegisterValue(r0, prefix_result, registerField(r0, prefix_left) + " " + binop->word + " " + registerField(r1, prefix_right));
						free_register(r1);
						return r0;
					}
					case BINARY_ASSIGNMENT: {
						int r0 = do_generate(binop->left);
						int r1 = do_generate(binop->right);
						if (prefix_result == "i") {
							output << "*(int64_t *)";
						} else if (prefix_result == "f") {
							output << "*(double *)";
						}
						setRegisterValue(r0, prefix_left, registerField(r1, prefix_right));
						free_register(r0);
						free_register(r1);
						return r0;
					}
					case BINARY_MEMBER_DEREFERENCE: {
						Expression_Identifier* f_exp = dynamic_cast<Expression_Identifier *>(binop->right);
						Datatype_Struct* str = dynamic_cast<Datatype_Struct *>(binop->left->eval);
						Var_Declaration* field = str->getField(f_exp->value);
						int r0 = do_generate(binop->left);
						incrementRegisterValue(r0, "i", std::to_string(field->offset) + "*sizeof(" + union_typename + ")");
						if (!dont_dereference) {
							if (prefix_result == "i" ) {
								setRegisterValue(r0, "i", dereferenceRegister(r0, c_int_type));	
							} else if (prefix_result == "f") {
								setRegisterValue(r0, "f", dereferenceRegister(r0, c_float_type));
							}
						}
						return r0;
					}
				}
				break;
			}
		}
	};

	return do_generate(exp);
}

void
C_Code_Generator::writeNode(Ast_Node* node) {

	if (!node) {
		return;
	}

	Ast_Node* save_node = context->current_node;
	Ast_Procedure* save_procedure = context->current_procedure;
	Ast_Block* save_block = context->current_block;

	context->current_node = node;

	switch (node->type) {
		case AST_PROCEDURE:
			context->current_procedure = dynamic_cast<Ast_Procedure *>(node);
			writeProcedure();
			break;
		case AST_BLOCK:
			context->current_block = dynamic_cast<Ast_Block *>(node);
			writeBlock();
			break;
		case AST_STATEMENT:
			writeStatement();
			break;
		case AST_RETURN:
			writeReturn();
			break;
		case AST_IF:
			writeIf();
			break;
		case AST_WHILE:
			writeWhile();
			break;
		case AST_FOR:
			writeFor();
			break;
	}

	context->current_node = save_node;
	context->current_procedure = save_procedure;
	context->current_block = save_block;
}

void
C_Code_Generator::generateCode(const std::string& output_name, Parse_Context* context) {

	C_Code_Generator* gen = new C_Code_Generator();
	gen->context = context;
	gen->output.open(output_name);
	gen->union_typename = "_T_";
	gen->c_int_type = "int64_t";
	gen->c_float_type = "double";
	gen->c_byte_type = "uint8_t";
	
	if (!gen->output.is_open()) {
		std::cout << "couldn't open '" << output_name << "' for writing.\n";
		exit(1);	
	}

	context->current_node = context->root;
	context->current_block = context->root;
	context->current_procedure = nullptr;
	
	gen->writeHeaders();
	gen->writeTypedefs();
	gen->writeLiterals();
	gen->writeNode(context->root);
	
	gen->output.close();

	delete gen;	

}
