#include <stdlib.h>
#include <iostream>
#include "tree.h"

/*
static const OpEntry prec[127] = {
	[',']				= {1, ASSOC_LEFT, OP_BINARY},
	['=']				= {2, ASSOC_RIGHT, OP_BINARY},
	[SPEC_INC_BY]		= {2, ASSOC_RIGHT, OP_BINARY},
	[SPEC_DEC_BY]		= {2, ASSOC_RIGHT, OP_BINARY},
	[SPEC_MUL_BY]		= {2, ASSOC_RIGHT, OP_BINARY},
	[SPEC_DIV_BY]		= {2, ASSOC_RIGHT, OP_BINARY},
	[SPEC_MOD_BY]		= {2, ASSOC_RIGHT, OP_BINARY},
	[SPEC_SHL_BY]		= {2, ASSOC_RIGHT, OP_BINARY},
	[SPEC_SHR_BY]		= {2, ASSOC_RIGHT, OP_BINARY},
	[SPEC_AND_BY]		= {2, ASSOC_RIGHT, OP_BINARY},
	[SPEC_OR_BY]		= {2, ASSOC_RIGHT, OP_BINARY},
	[SPEC_XOR_BY]		= {2, ASSOC_RIGHT, OP_BINARY},
	[SPEC_LOG_AND]		= {3, ASSOC_LEFT, OP_BINARY},
	[SPEC_LOG_OR]		= {3, ASSOC_LEFT, OP_BINARY},
	[SPEC_EQ]			= {4, ASSOC_LEFT, OP_BINARY},
	[SPEC_NEQ]			= {4, ASSOC_LEFT, OP_BINARY},
	['>']				= {6, ASSOC_LEFT, OP_BINARY},
	[SPEC_GE]			= {6, ASSOC_LEFT, OP_BINARY},
/['<']				= {6, ASSOC_LEFT, OP_BINARY},
	[SPEC_LE]			= {6, ASSOC_LEFT, OP_BINARY},
	['|']				= {7, ASSOC_LEFT, OP_BINARY},
	[SPEC_SHL]			= {7, ASSOC_LEFT, OP_BINARY},
	[SPEC_SHR]			= {7, ASSOC_LEFT, OP_BINARY},
	['+']				= {8, ASSOC_LEFT, OP_BINARY},
	['-']				= {8, ASSOC_LEFT, OP_BINARY},
	['*']				= {9, ASSOC_LEFT, OP_BINARY},
	['%']				= {9, ASSOC_LEFT, OP_BINARY},
	['/']				= {9, ASSOC_LEFT, OP_BINARY},
	['@']				= {10, ASSOC_RIGHT, OP_UNARY},
	['$']				= {10, ASSOC_RIGHT, OP_UNARY},
	['!']				= {10, ASSOC_RIGHT, OP_UNARY},
	[SPEC_TYPENAME]		= {10, ASSOC_RIGHT, OP_UNARY},
	[SPEC_CAST]			= {10, ASSOC_RIGHT, OP_UNARY},
	['.']				= {11, ASSOC_LEFT, OP_BINARY},
	[SPEC_INC_ONE]		= {11, ASSOC_LEFT, OP_UNARY},
	[SPEC_DEC_ONE]		= {11, ASSOC_LEFT, OP_UNARY},
	[SPEC_CALL]			= {11, ASSOC_LEFT, OP_UNARY},
	[SPEC_INDEX]		= {11, ASSOC_LEFT, OP_UNARY}
};
*/

static const std::vector<Operator_Descriptor> operator_table {
	Operator_Descriptor(",",        1, ASSOC_LEFT, OP_BINARY),
	Operator_Descriptor("=",        2, ASSOC_LEFT, OP_BINARY),
	Operator_Descriptor("+=",       2, ASSOC_LEFT, OP_BINARY),
	Operator_Descriptor("-=",       2, ASSOC_LEFT, OP_BINARY),
	Operator_Descriptor("*=",       2, ASSOC_LEFT, OP_BINARY),
	Operator_Descriptor("/=",       2, ASSOC_LEFT, OP_BINARY),
	Operator_Descriptor("%=",       2, ASSOC_LEFT, OP_BINARY),
	Operator_Descriptor("&=",       2, ASSOC_LEFT, OP_BINARY),
	Operator_Descriptor("|=",       2, ASSOC_LEFT, OP_BINARY),
	Operator_Descriptor("^=",       2, ASSOC_LEFT, OP_BINARY),
	Operator_Descriptor("&&",       3, ASSOC_LEFT, OP_BINARY),
	Operator_Descriptor("||",       3, ASSOC_LEFT, OP_BINARY),
	Operator_Descriptor("==",       4, ASSOC_LEFT, OP_BINARY),
	Operator_Descriptor("!=",       4, ASSOC_LEFT, OP_BINARY),
	Operator_Descriptor(">",        6, ASSOC_LEFT, OP_BINARY),
	Operator_Descriptor(">=",       6, ASSOC_LEFT, OP_BINARY),
	Operator_Descriptor("<",        6, ASSOC_LEFT, OP_BINARY),
	Operator_Descriptor("<=",       6, ASSOC_LEFT, OP_BINARY),
	Operator_Descriptor("<<",       7, ASSOC_LEFT, OP_BINARY),
	Operator_Descriptor(">>",       7, ASSOC_LEFT, OP_BINARY),
	Operator_Descriptor("+",        8, ASSOC_LEFT, OP_BINARY),
	Operator_Descriptor("-",        8, ASSOC_LEFT, OP_BINARY),
	Operator_Descriptor("*",        9, ASSOC_LEFT, OP_BINARY),
	Operator_Descriptor("/",        9, ASSOC_LEFT, OP_BINARY),
	Operator_Descriptor("__CAST__", 10, ASSOC_RIGHT, OP_UNARY),
	Operator_Descriptor("@",        10, ASSOC_RIGHT, OP_UNARY),
	Operator_Descriptor("$",        10, ASSOC_RIGHT, OP_UNARY),
	Operator_Descriptor("!",        10, ASSOC_RIGHT, OP_UNARY),
	Operator_Descriptor("new",      10, ASSOC_RIGHT, OP_UNARY),
	Operator_Descriptor(".",        11, ASSOC_LEFT, OP_BINARY)
};

static const Operator_Descriptor*
get_operator_descriptor(const std::string& word) {
	for (const Operator_Descriptor& op: operator_table) {
		if (op.str == word) {
			return &op;
		}
	}
	return nullptr;
}

/* ------ VAR_DECLARATION IMPL ------ */
std::string
Var_Declaration::toString(const Var_Declaration& var) {
	var.dt->as_string = Datatype_Base::toString(*var.dt);
	return var.identifier + ": " + var.dt->as_string;
}

/* ------ EXPRESSION IMPL ------ */
bool
Expression::isBinaryType(Expression_Binary_Type type) const {
	const Expression_Binary* b = dynamic_cast<const Expression_Binary *>(this);
	if (!b) {
		return false;
	}
	return b->optype == type;
}

bool
Expression::isAssignment() const {
	if (!this) return false;
	return (
		isBinaryType(BINARY_ASSIGNMENT) ||
		isBinaryType(BINARY_INCREMENT_BY) ||
		isBinaryType(BINARY_DECREMENT_BY) ||
		isBinaryType(BINARY_MULTIPLY_BY) ||
		isBinaryType(BINARY_DIVIDE_BY) ||
		isBinaryType(BINARY_MODULUS_BY) ||
		isBinaryType(BINARY_AND_BY) ||
		isBinaryType(BINARY_OR_BY) ||
		isBinaryType(BINARY_XOR_BY) ||
		isBinaryType(BINARY_SHIFT_LEFT_BY) ||
		isBinaryType(BINARY_SHIFT_RIGHT_BY)
	);
}

bool
Expression::isUnaryType(Expression_Unary_Type type) const {
	const Expression_Unary* u = dynamic_cast<const Expression_Unary *>(this);
	if (!u) {
		return false;
	}
	return u->optype == type;
}

Expression_Binary_Type
Expression_Binary::wordToBinaryType(const std::string& word) {
	if (word == "+")
		return BINARY_ADDITION;
	else if (word == "-")
		return BINARY_SUBTRACTION;
	else if (word == "*")
		return BINARY_MULTIPLICATION;
	else if (word == "/")
		return BINARY_DIVISION;
	else if (word == "&")
		return BINARY_MODULUS;
	else if (word == "&")
		return BINARY_BITWISE_AND;
	else if (word == ">")
		return BINARY_GREATER_THAN;
	else if (word == "<")
		return BINARY_LESS_THAN;
	else if (word == ">=")
		return BINARY_GREATER_THAN_OR_EQUAL;
	else if (word == "<=")
		return BINARY_LESS_THAN_OR_EQUAL;
	else if (word == "<<")
		return BINARY_SHIFT_LEFT;
	else if (word == ">>")
		return BINARY_SHIFT_RIGHT;
	else if (word == "|")
		return BINARY_BITWISE_OR;
	else if (word == "^")
		return BINARY_BITWISE_XOR;
	else if (word == "&&")
		return BINARY_LOGICAL_AND;
	else if (word == "||")
		return BINARY_LOGICAL_OR;
	else if (word == "+=")
		return BINARY_INCREMENT_BY;
	else if (word == "-=")
		return BINARY_DECREMENT_BY;
	else if (word == "*=")
		return BINARY_MULTIPLY_BY;
	else if (word == "/=")
		return BINARY_DIVIDE_BY;
	else if (word == "%=")
		return BINARY_MODULUS_BY;
	else if (word == "&=")
		return BINARY_AND_BY;
	else if (word == "|=")
		return BINARY_OR_BY;
	else if (word == "^=")
		return BINARY_XOR_BY;
	else if (word == "<<=")
		return BINARY_SHIFT_LEFT_BY;
	else if (word == ">>=")
		return BINARY_SHIFT_RIGHT_BY;
	else if (word == "==")
		return BINARY_COMPARE;
	else if (word == "!=")
		return BINARY_COMPARE_NOT;
	else if (word == "=")
		return BINARY_ASSIGNMENT;
	else if (word == ".")
		return BINARY_MEMBER_DEREFERENCE;
	else if (word == ",")
		return BINARY_COMMA;
}

Expression_Unary_Type
Expression_Unary::wordToUnaryType(const std::string& word) {
	if (word == "!")
		return UNARY_LOGICAL_NOT;
	else if (word == "~")
		return UNARY_BITWISE_NOT;
	else if (word == "(")
		return UNARY_OPEN_PARENTHESIS;
	else if (word == ")")
		return UNARY_CLOSE_PARENTHESIS;
	else if (word == "$") 
		return UNARY_DEREFERENCE;
	else if (word == "@")
		return UNARY_ADDRESS_OF;
	else if (word == "new")
		return UNARY_NEW;
}

Datatype_Base*
Expression_Integer::typecheck(Parse_Context* context) {
	return (eval = context->getTypeInfo("int"));	
}

Datatype_Base*
Expression_Float::typecheck(Parse_Context* context) {
	return (eval = context->getTypeInfo("float"));
}

Datatype_Base*
Expression_Datatype::typecheck(Parse_Context* context) {
	return (eval = dt);
}

Datatype_Base*
Expression_Cast::typecheck(Parse_Context* context) {
	operand->typecheck(context);
	return (eval = target);
}

Datatype_Base*
Expression_Identifier::typecheck(Parse_Context* context) {
	Var_Declaration* local = context->getLocal(value);
	var = local;
	if (!local) {
		return (eval = context->getTypeInfo("void"));
	}
	return (eval = local->dt);
}

Datatype_Base*
Expression_Call::typecheck(Parse_Context* context) {
	if (argument) {
		argument->typecheck(context);
	}
	Datatype_Procedure* proc;
	Expression_Identifier* id;
	Datatype_Base* proc_exp = procedure->typecheck(context);
	std::string func_name = "";
	if (!proc_exp->isRawProcedure()) {
		context->die("attempt to call a non-procedure (got type '" + proc_exp->as_string + "')");
	}
	id = dynamic_cast<Expression_Identifier *>(procedure);
	func_name += (id ? id->value : "<computed_call>");
	proc = dynamic_cast<Datatype_Procedure *>(proc_exp);
	int expected_args = proc->args.size();
	if ((proc->is_vararg && num_args < expected_args) || (!proc->is_vararg && num_args != proc->args.size())) {
		std::string err = "passing incorrect number of arguments to function '";
		err += func_name;
		err += "' expected ";
		err += std::to_string(expected_args);
		err += ", got ";
		err += std::to_string(num_args);
		context->die(err);
	}
	
	auto compare_args = [&context, &proc, &func_name](Datatype_Base* expected, Datatype_Base* check, int index) -> void {
		Var_Declaration* arg = proc->args[index]; 
		if (!expected->matches(*check)) {
			std::string err = "argument type mismatch when calling function '";
			err += func_name;
			err += "'; argument '";
			err += arg->identifier;
			err += "' (arg #";
			err += std::to_string(index + 1);
			err += ") should be of type '";
			err += expected->as_string;
			err += "', got type '";
			err += check->as_string;
			err += "'";
			context->die(err); 
		}
	};

	if (num_args == 1) {
		compare_args(proc->args[0]->dt, argument->eval, 0);
	} else if (expected_args > 1) {
		Expression_Binary* comma = dynamic_cast<Expression_Binary *>(argument);
		// walk down to the lowest comma
		for (int i = 0; i < num_args - 2; i++) {
			comma = dynamic_cast<Expression_Binary *>(comma->left);
		}
		compare_args(proc->args[0]->dt, comma->left->eval, 0);
		compare_args(proc->args[1]->dt, comma->right->eval, 1);
		for (int i = 0; i < expected_args - 2; i++) {
			comma = dynamic_cast<Expression_Binary *>(comma->parent);
			compare_args(proc->args[i + 2]->dt, comma->right->eval, i + 2);
		}
	}
	
	return (eval = proc->ret);

}

Datatype_Base*
Expression_String::typecheck(Parse_Context* context) {
	return (eval = context->type_string);
}

void
Expression_Binary::assertMatch(Parse_Context* context) {
	Datatype_Base* lhs = left->typecheck(context);
	Datatype_Base* rhs = right->typecheck(context);
	if (!lhs->matches(*rhs)) {
		std::string err = "non-matching operands to operator '";
		err += word;
		err += "' (got '";
		err += lhs->as_string;
		err += "' and '";
		err += rhs->as_string;
		err += "')";
		context->die(err);
	}
}

Datatype_Base*
Expression_Binary::typecheck(Parse_Context* context) {
	switch (optype) {
		case BINARY_COMMA:
			left->typecheck(context);
			right->typecheck(context);
			break;
		case BINARY_ADDITION:
		case BINARY_SUBTRACTION:	
		case BINARY_MULTIPLICATION:
		case BINARY_DIVISION:
			assertMatch(context);
			return (eval = left->eval);
		case BINARY_MODULUS:
		case BINARY_MODULUS_BY:
			assertMatch(context);
			if (!left->eval->matches(*context->type_int)) {
				std::string err = "modulus operator '";
				err += word;
				err += "' can only have integers as operands (got '";
				err += left->eval->as_string;
				err += "' and '";
				err += right->eval->as_string;
				err += "')";
				context->die(err);
			}
			return (eval = left->eval);
		case BINARY_LOGICAL_AND:
		case BINARY_LOGICAL_OR:
			assertMatch(context);
			if (!left->eval->matches(*context->type_bool)) {
				std::string err = "logical operator '";
				err += word;
				err += "' can only have bools as operands (got '";
				err += left->eval->as_string;
				err += "' and '";
				err += right->eval->as_string;
				err += "')";
				context->die(err);
			}
			return (eval = context->type_bool);
		case BINARY_GREATER_THAN:
		case BINARY_LESS_THAN:
		case BINARY_GREATER_THAN_OR_EQUAL:
		case BINARY_LESS_THAN_OR_EQUAL:
		case BINARY_COMPARE:
		case BINARY_COMPARE_NOT:
			assertMatch(context);
			return (eval = context->type_bool);
		case BINARY_SHIFT_LEFT:
		case BINARY_SHIFT_RIGHT:
		case BINARY_BITWISE_AND:
		case BINARY_BITWISE_OR:
		case BINARY_BITWISE_XOR:
		case BINARY_AND_BY:
		case BINARY_OR_BY:
		case BINARY_XOR_BY:
		case BINARY_SHIFT_LEFT_BY:
		case BINARY_SHIFT_RIGHT_BY:
			assertMatch(context);
			if (!left->eval->matches(*context->type_int)) {
				std::string err = "bitwise operator '";
				err += word;
				err += "' can only have integers as operands (got '";
				err += left->eval->as_string;
				err += "' and '";
				err += right->eval->as_string;
				err += "')";
				context->die(err);
			}
			return (eval = context->type_int);
		case BINARY_ASSIGNMENT:
		case BINARY_INCREMENT_BY:
		case BINARY_DECREMENT_BY:
		case BINARY_MULTIPLY_BY:
		case BINARY_DIVIDE_BY: {
			bool is_valid_lhs = (
				left->isUnaryType(UNARY_DEREFERENCE) ||
				left->isBinaryType(BINARY_MEMBER_DEREFERENCE) ||
				left->isAssignment() ||
				left->type == EXP_IDENTIFIER
			);	
			if (!is_valid_lhs) {
				context->die("invalid lhs of assignment operator '" + word + "'");
			}
			if (left->type == EXP_IDENTIFIER) {
				Expression_Identifier* id = dynamic_cast<Expression_Identifier *>(left);
				Var_Declaration* local = context->getLocal(id->value);
				if (!local) {
					context->undeclaredIdentifier(id->value);
				}
				if (local->dt->type == DATA_STRUCT && !local->dt->is_ptr) {
					context->die("attempt to assign to an object");
				}
				id->eval = local->dt;
				id->var = local;
				if (!local->dt->matches(*right->typecheck(context))) {
					context->typeMismatch(word, *local->dt, *right->eval);
				}
			} else {
				assertMatch(context);
			}
			return (eval = left->eval);
		}
		case BINARY_MEMBER_DEREFERENCE: {
			Datatype_Struct* str = nullptr;
			if (left->type == EXP_IDENTIFIER) {
				// if the left side is an identifier, we're indexing
				// a local variable, make sure it exists
				Expression_Identifier* id = dynamic_cast<Expression_Identifier *>(left);
				Var_Declaration* local = context->getLocal(id->value);
				if (!local) {
					context->undeclaredIdentifier(id->value);
				}
				if (local->dt->type != DATA_STRUCT) {
					context->die("attempt to use operator '.' on a non-struct local variable");
				}
				if (local->dt->ptr_dim > 1) {
					context->die("operator '.' can only be used on structs and pointers to a struct (pointer level too great)");
				}
				id->eval = local->dt;
				id->var = local;
				str = dynamic_cast<Datatype_Struct *>(local->dt);
			} else {
				str = dynamic_cast<Datatype_Struct *>(left->typecheck(context));
				if (!str) {
					context->die("the left side of the '.' operator must evaluate to an object (got type '" + left->eval->as_string + "')");
				}
			}
			Expression_Identifier* field_name = dynamic_cast<Expression_Identifier *>(right);
			if (!field_name) {
				context->die("the right side of the '.' operator must be an identifier");
			}
			Var_Declaration* found_field = nullptr;
			for (Var_Declaration* field: str->members) {
				if (field->identifier == field_name->value) {
					found_field = field;
					break;
				}
			}
			if (!found_field) {
				std::string err = "'";
				err += field_name->value;
				err += "' is not a valid field of struct '";
				err += str->as_string;
				err += "'";
				context->die(err);
			}
			return (eval = found_field->dt);
		}
	}
}

Datatype_Base*
Expression_Unary::typecheck(Parse_Context* context) {
	Datatype_Base* op = operand->typecheck(context);
	switch (optype) {
		case UNARY_NEW:
			break;	
	}
	return (eval = op);
}

void
Expression::print(int ind = 0) const {

	if (!this) {
		return;
	}

	auto indent = [](int amount) -> void {
		for (int i = 0; i < amount; i++) {
			std::cout << "  ";
		}
	};

	// ... is the another way to do this?
	const Expression_Integer*    a = dynamic_cast<const Expression_Integer *>(this);
	const Expression_Float*      b = dynamic_cast<const Expression_Float *>(this);
	const Expression_Identifier* c = dynamic_cast<const Expression_Identifier *>(this);
	const Expression_Binary*     d = dynamic_cast<const Expression_Binary *>(this);
	const Expression_Unary*      e = dynamic_cast<const Expression_Unary *>(this);
	const Expression_Datatype*   f = dynamic_cast<const Expression_Datatype* >(this);
	const Expression_Call*	     g = dynamic_cast<const Expression_Call *>(this);
	const Expression_Cast*	     h = dynamic_cast<const Expression_Cast *>(this);
	const Expression_String*     i = dynamic_cast<const Expression_String *>(this);

	indent(ind);

	if (a) {
		std::cout << a->value << std::endl;
	} else if (b) {
		std::cout << b->value << std::endl;
	} else if (c) {
		std::cout << c->value << std::endl;
	} else if (d) {
		std::cout << word << std::endl;
		d->left->print(ind + 1);
		d->right->print(ind + 1);
	} else if (e) {
		std::cout << word << std::endl;
		e->operand->print(ind + 1);
	} else if (f) {
		std::cout << f->dt->as_string << std::endl;
	} else if (g) {
		std::cout << "CALL" << std::endl;
		g->procedure->print(ind + 1);
		g->argument->print(ind + 1);
	} else if (h) {
		std::cout << "CAST" << std::endl;
		indent(ind + 1);
		std::cout << h->target->as_string << std::endl;
		h->operand->print(ind + 1);
	} else if (i) {
		std::cout << i->value << std::endl;
	}

}

/* ------ AST_NODE IMPL ------ */
void
Ast_Node::print(int ind = 0) const {

	if (!this) {
		return;
	}

	auto indent = [](int amount) -> void {
		for (int i = 0; i < amount; i++) {
			std::cout << "  ";
		}
	};
	
	// this is nasty but oh well
	const Ast_If*        a = dynamic_cast<const Ast_If *>(this);
	const Ast_Block*     b = dynamic_cast<const Ast_Block *>(this);
	const Ast_Statement* c = dynamic_cast<const Ast_Statement *>(this);
	const Ast_While*     d = dynamic_cast<const Ast_While *>(this);
	const Ast_For*       e = dynamic_cast<const Ast_For *>(this);
	const Ast_Procedure* f = dynamic_cast<const Ast_Procedure *>(this);
	const Ast_Return*    g = dynamic_cast<const Ast_Return *>(this);

	indent(ind);
	
	if (a) {
		std::cout << "IF: [\n";
		indent(ind + 1);
		std::cout << "CONDITION: [\n";
		a->cond->print(ind + 2);
		indent(ind + 1);
		std::cout << "]\n";
		indent(ind + 1);
		std::cout << "CHILD: [\n";
		a->child->print(ind + 2);
		indent(ind + 1);
		std::cout << "]\n";
		indent(ind);
		std::cout << "]\n";
	} else if (b) {
		std::cout << "BLOCK: [\n";
		indent(ind + 1);
		std::cout << "LOCALS: [\n";
		for (const Var_Declaration* local: b->locals) {
			indent(ind + 2);
			std::cout << local->as_string << std::endl;
		}
		indent(ind + 1);
		std::cout << "]\n";
		indent(ind + 1);
		std::cout << "CHILDREN: [\n";
		for (const Ast_Node* child: b->children) {
			child->print(ind + 2);
		}
		indent(ind + 1);
		std::cout << "]\n";
		indent(ind);
		std::cout << "]\n";
	} else if (c) {
		std::cout << "STATEMENT: [\n";
		c->expression->print(ind + 1);
		indent(ind);
		std::cout << "]\n";
	} else if (d) {
		std::cout << "WHILE: [\n";
		indent(ind + 1);
		std::cout << "CONDITION: [\n";
		d->cond->print(ind + 2);
		indent(ind + 1);
		std::cout << "]\n";
		indent(ind + 1);
		std::cout << "CHILD: [\n";
		d->child->print(ind + 2);
		indent(ind + 1);
		std::cout << "]\n";
		indent(ind);
		std::cout << "]\n";
	} else if (e) {
		std::cout << "FOR: [\n";
		indent(ind + 1);
		std::cout << "INITIALIZER: [\n";
		e->init->print(ind + 2);
		indent(ind + 1);
		std::cout << "]\n";
		indent(ind + 1);
		std::cout << "CONDITION: [\n";
		e->cond->print(ind + 2);
		indent(ind + 1);
		std::cout << "]\n";
		indent(ind + 1);
		std::cout << "EACH: [\n";
		e->each->print(ind + 2);
		indent(ind + 1);
		std::cout << "]\n";
		indent(ind + 1);
		std::cout << "CHILD: [\n";
		e->child->print(ind + 2);
		indent(ind + 1);
		std::cout << "]\n";
		indent(ind);
		std::cout << "]\n";
	} else if (f) {
		std::cout << "PROCEDURE: [\n";
		indent(ind + 1);
		std::cout << "IDENTIFIER: " << f->identifier << std::endl;
		indent(ind + 1);
		std::cout << "RETURN_TYPE: " << f->desc->ret->as_string << std::endl;
		indent(ind + 1);
		std::cout << "ARGUMENTS: [\n";
		for (const Var_Declaration* arg: f->desc->args) {
			indent(ind + 2);
			std::cout << arg->as_string << std::endl;
		}
		indent(ind + 1);
		std::cout << "]\n";
		indent(ind + 1);
		std::cout << "CHILD: [\n";
		f->child->print(ind + 2);
		indent(ind + 1);
		std::cout << "]\n";
		indent(ind);
		std::cout << "]\n"; 	
	} else if (g) {
		std::cout << "RETURN: [\n";
		indent(ind + 1);
		std::cout << "EXPRESSION: [\n";
		g->expression->print(ind + 2);
		indent(ind + 1);
		std::cout << "]\n";
		indent(ind);
		std::cout << "]\n";
	}

}

/* ------ DATATYPE IMPL ------ */
bool
Datatype_Base::matches(const Datatype_Base& other) const {
	if (type != other.type) {
		return false;
	}
	if (ptr_dim != other.ptr_dim) {
		return false;
	}
	if (size_array.size() != other.size_array.size()) {
		return false;
	}
	if (type == DATA_PROCEDURE) {
		const Datatype_Procedure* a = dynamic_cast<const Datatype_Procedure *>(this);
		const Datatype_Procedure* b = dynamic_cast<const Datatype_Procedure *>(&other);
		if (a->args.size() != b->args.size()) {
			return false;
		}
		for (int i = 0; i < a->args.size(); i++) {
			if (!a->args[i]->dt->matches(*b->args[i]->dt)) {
				return false;
			}
		}
	} else if (type == DATA_STRUCT) {
		const Datatype_Struct* a = dynamic_cast<const Datatype_Struct *>(this);
		const Datatype_Struct* b = dynamic_cast<const Datatype_Struct *>(this);
		if (a->members.size() != b->members.size()) {
			return false;
		}
		for (int i = 0; i < a->members.size(); i++) {
			if (!a->members[i]->dt->matches(*b->members[i]->dt)) {
				return false;
			}
		}
	}
	return true;
}

std::string
Datatype_Base::toString(const Datatype_Base& d) {
	std::string ret = "";		
	
	if (d.mods & MOD_STATIC) {
		ret += "static ";
	}
	if (d.mods & MOD_CONST) {
		ret += "const ";
	}
	if (d.mods & MOD_FOREIGN) {
		ret += "foreign ";
	}
	
	for (int i = 0; i < d.ptr_dim; i++) {
		ret += "^";
	}

	for (int i = 0; i < d.size_array.size(); i++) {
		ret += "[]";
	}

	switch (d.type) {
		case DATA_PROCEDURE: {
			const Datatype_Procedure* proc = dynamic_cast<const Datatype_Procedure*>(&d);
			size_t size = proc->args.size();
			ret += "(";
			for (int i = 0; i < size; i++) {
				ret += proc->args[i]->dt->as_string;
				if (i < size - 1) {
					ret += ", ";
				}	
			}
			ret += ") -> ";
			ret += proc->ret->as_string;
			break;
		}
		case DATA_STRUCT: {
			const Datatype_Struct* str = dynamic_cast<const Datatype_Struct *>(&d);
			ret += "{ ";
			for (const Var_Declaration* member: str->members) {
				ret += member->dt->as_string;
				ret += "; ";
			}
			ret += "}";
			break;	
		}
		case DATA_INTEGER:
			ret += "int";
			break;
		case DATA_FLOAT:
			ret += "float";
			break;
		case DATA_BYTE:
			ret += "byte";
			break;
		case DATA_BOOL:
			ret += "bool";
			break;
		case DATA_VOID:
			ret += "void";
			break;
	}

	return ret;
}

std::string
Datatype_Struct::tostr() const {
	std::string str = "";
	for (const Var_Declaration* field: members) {
		str += field->dt->as_string;
		str += ";";
	}
	return str;
}

Var_Declaration*
Datatype_Struct::getField(const std::string& field) {
	for (Var_Declaration* member: members) {
		if (member->identifier == field) {
			return member;
		}
	}
	return nullptr;
}


/* ------ PARSER IMPL ------ */

Parse_Context::Parse_Context() {
	num_tokens = 0; // TBI
	token_index = 0;
	marked = 0;
	current_block = new Ast_Block();
	root = current_block;

	/* create default types */
	Var_Declaration* t_int = new Var_Declaration();
	t_int->dt = new Datatype_Integer();
	t_int->identifier = "int";
	t_int->as_string = Var_Declaration::toString(*t_int);
	defined_types.push_back(t_int);
	type_int = t_int->dt;

	Var_Declaration* t_float = new Var_Declaration();
	t_float->dt = new Datatype_Float();
	t_float->identifier = "float";
	t_float->as_string = Var_Declaration::toString(*t_float);
	defined_types.push_back(t_float);
	type_float = t_float->dt;

	Var_Declaration* t_byte = new Var_Declaration();
	t_byte->dt = new Datatype_Byte();
	t_byte->identifier = "byte";
	t_byte->as_string = Var_Declaration::toString(*t_byte);
	defined_types.push_back(t_byte);
	type_byte = t_byte->dt;

	Var_Declaration* t_bool = new Var_Declaration();
	t_bool->dt = new Datatype_Bool();
	t_bool->identifier = "bool";
	t_bool->as_string = Var_Declaration::toString(*t_bool);
	defined_types.push_back(t_bool);
	type_bool = t_bool->dt;

	Var_Declaration* t_string = new Var_Declaration();
	t_string->dt = new Datatype_Byte();
	t_string->dt->ptr_dim = 1;
	t_string->identifier = "string";
	t_string->as_string = Var_Declaration::toString(*t_string);
	defined_types.push_back(t_string);
	type_string = t_string->dt;

	Var_Declaration* t_void = new Var_Declaration();
	t_void->dt = new Datatype_Void();
	t_void->identifier = "void";
	t_void->as_string = Var_Declaration::toString(*t_void);
	defined_types.push_back(t_void);
	type_void = t_void->dt;

}

void
Parse_Context::die(const std::string& message) {
	Token* f = focus ?: prev_focus;
	std::cout << "!!! PARSE ERROR !!!\n";
	std::cout << "\tmessage: " << message << std::endl;
	std::cout << "\tfile:    " << filename << std::endl;
	std::cout << "\tline:    " << (f ? f->line : 0) << std::endl;
	exit(1);
}

void
Parse_Context::undeclaredIdentifier(const std::string& name) {
	die("unexpected identifier '" + name + "'");
}

void
Parse_Context::typeMismatch(const std::string& op_word, const Datatype_Base& left, const Datatype_Base& right) {
	std::string err = "operator '";
	err += op_word;
	err += "' type mismatch (got '";
	err += left.as_string;
	err += "' and '";
	err += right.as_string;
	err += "')";
	die(err);
}

void 
Parse_Context::focusTokens(std::vector<Token>& toks) {
	tokens = &toks;
	num_tokens = toks.size();
	token_index = 0;	
	focus = &toks[0];
	prev_focus = &toks[0];
}

Token*
Parse_Context::peek() {
	if (!focus) {
		return nullptr;
	}
	return &((*tokens)[token_index + 1]);
}

void
Parse_Context::next() {
	if (!focus) {
		die("unexpected EOF");
	}
	prev_focus = focus;
	token_index++;
	if (token_index >= num_tokens) {
		focus = nullptr;
	} else {
		focus = &((*tokens)[token_index]);
	}
}

void
Parse_Context::setIndex(int index) {
	token_index = index;
	if (token_index >= num_tokens) {
		focus = nullptr;
	} else {
		focus = &((*tokens)[token_index]);
	}
}

Token*
Parse_Context::atIndex(int index) {
	return &((*tokens)[index]);
}

bool
Parse_Context::isOperator() {
	return focus->type == TOK_OPERATOR;
}

bool 
Parse_Context::isIdentifier() {
	return focus->type == TOK_IDENTIFIER;
}

bool 
Parse_Context::onOperator(const std::string& word) {
	return isOperator() && focus->word == word;
}

bool
Parse_Context::onIdentifier(const std::string& identifier) {
	return isIdentifier() && focus->id == identifier;
}

void 
Parse_Context::eatOperator(const std::string& word) {
	if (!onOperator(word)) {
		die("expected operator '" + word + "', got token '" + focus->word + "'");
	}
	next();
}

void
Parse_Context::eatIdentifier(const std::string& expected) {
	if (!onIdentifier(expected)) {
		die("expected identifier '" + expected + "', got token '" + focus->word + "'");
	}
	next();
}

void
Parse_Context::printOn() {
	std::cout << "ON: (word: " << focus->word << ", line: " << focus->line << ")\n";
}

void
Parse_Context::markOperator(const std::string& inc, const std::string& dec) {
	int start = token_index;
	int count = 1;
	while (true) {
		if (onOperator(inc)) {
			count++;
		} else if (onOperator(dec)) {
			count--;
		} 
		if (count == 0) {
			marked = token_index;
			setIndex(start);
			return;
		}
		next();	
	}
}

Datatype_Base*
Parse_Context::getTypeInfo(const std::string& type_name) {
	for (Var_Declaration* decl: defined_types) {
		if (decl->identifier == type_name) {
			return decl->dt;
		}
	}
	return nullptr;
}

Var_Declaration*
Parse_Context::getLocal(const std::string& identifier) {
	Ast_Block* block = current_block;
	if (current_procedure) {
		for (Var_Declaration* arg: current_procedure->desc->args) {
			if (arg->identifier == identifier) {
				return arg;
			}
		}
	}
	while (block) {
		for (Var_Declaration* var: block->locals) {
			if (var->identifier == identifier) {
				return var;
			}
		}
		Ast_Node* check = block->parent;
		while (check && check->type != AST_BLOCK) {
			check = check->parent;
		}	
		block = check ? dynamic_cast<Ast_Block *>(check) : nullptr;
	}
	return nullptr;
}

uint32_t
Parse_Context::parseModifiers() {
	
	static const std::vector<std::string> modifiers {
		"static",
		"const",
		"foreign"
	};

	uint32_t mod = 0;

	while (isIdentifier()) {
		bool passed = false;
		for (int i = 0; i < modifiers.size(); i++) {
			if (onIdentifier(modifiers[i])) {
				passed = true;
				mod |= (0x1 << i);
				break;
			}
		}
		if (!passed) {
			break;
		}
		next();
	}

	return mod;
}

Datatype_Base*
Parse_Context::parseDatatype() {

	Datatype_Base* ret;
	Datatype_Base* type_info;
	uint32_t mod;
	int ptr_dim = 0;
	int arr_dim = 0;
	
	mod = parseModifiers();	

	while (onOperator("^")) {
		ptr_dim++;
		next();
	}

	while (onOperator("[")) {
		next();
		eatOperator("]");
		arr_dim++;
	}
	
	if (onIdentifier("int")) {
		ret = new Datatype_Integer();
		next();
	} else if (onIdentifier("float")) {
		ret = new Datatype_Float();
		next();
	} else if (onIdentifier("byte")) {
		ret = new Datatype_Byte();	
		next();
	} else if (onIdentifier("void")) {
		ret = new Datatype_Void();
		next();
	} else if (onOperator("(")) {
		Datatype_Procedure* proc = new Datatype_Procedure();
		eatOperator("(");
		while (matchesDeclaration() || matchesDatatype() || onOperator("..")) {
			bool has_name = matchesDeclaration();
			Var_Declaration* arg;
			if (onOperator("..")) {
				proc->is_vararg = true;	
				next();
				break;
			} else if (has_name) {
				arg = parseDeclaration();
			} else {
				arg = new Var_Declaration();
				arg->named = false;
				arg->dt = parseDatatype();
				arg->as_string = Var_Declaration::toString(*arg); 
			}
			proc->args.push_back(arg);
			if (focus->word == ",") {
				next();
			}
		}
		if (!onOperator(")")) {
			die("expected ')' to close argument list, got token '" + focus->word + "'");
		}
		eatOperator(")");
		eatOperator("->");
		proc->ret = parseDatatype();
		ret = proc;
	} else {
		// check if its a defined struct
		Datatype_Base* found_type = nullptr;
		for (Var_Declaration* d: defined_types) {
			if (onIdentifier(d->identifier)) {
				found_type = d->dt;
				break;	
			}
		}
		if (!found_type) {
			die("unknown typename '" + focus->word + "'");
		}
		ptr_dim += found_type->ptr_dim;
		// TODO we REALLY need copy contructors
		switch (found_type->type) {
			case DATA_INTEGER:
				ret = new Datatype_Integer();
				*ret = *dynamic_cast<Datatype_Integer *>(found_type);
				break;
			case DATA_FLOAT:
				ret = new Datatype_Float();
				*ret = *dynamic_cast<Datatype_Float *>(found_type);
				break;
			case DATA_BYTE:
				ret = new Datatype_Byte();
				*ret = *dynamic_cast<Datatype_Byte *>(found_type);
				break;
			case DATA_BOOL:
				ret = new Datatype_Bool();
				*ret = *dynamic_cast<Datatype_Bool *>(found_type);
				break;
			case DATA_VOID:
				ret = new Datatype_Void();
				*ret = *dynamic_cast<Datatype_Void *>(found_type);
				break;
			case DATA_PROCEDURE: {
				// do we need to deep copy here?
				Datatype_Procedure* proc = dynamic_cast<Datatype_Procedure *>(found_type);
				Datatype_Procedure* new_proc = new Datatype_Procedure();
				*new_proc = *proc;
				*new_proc->ret = *proc->ret;
				ret = new_proc;
				break;
			}
			case DATA_STRUCT: {
				Datatype_Struct* str = dynamic_cast<Datatype_Struct *>(found_type);
				Datatype_Struct* new_str = new Datatype_Struct();
				*new_str = *str;
				// todo deep copy
				new_str->members = str->members;
				ret = new_str;
				break;
			}
		}
		next();
	}
	
	ret->mods = mod;
	ret->is_ptr = ptr_dim > 0;
	ret->ptr_dim = ptr_dim;
	ret->is_array = arr_dim > 0;
	ret->as_string = Datatype_Base::toString(*ret);

	if (ret->is_ptr) {
		ret->size = 8;
	}

	for (int i = 0; i < arr_dim; i++) {
		ret->size_array.push_back(0);
	}
	
	bool is_void = dynamic_cast<Datatype_Void *>(ret) != nullptr;
	if (is_void && ret->is_ptr) {
		die("cannot have a pointer to void");
	} else if (is_void && ret->is_array) {
		die("cannot have an array of void");
	}
	
	return ret;

}

Expression_Datatype*
Parse_Context::parseDatatypeAsExpression() {
	
	Expression_Datatype* dt = new Expression_Datatype();
	dt->dt = parseDatatype();
	dt->dt->as_string = Datatype_Base::toString(*dt->dt);
	dt->word = dt->dt->as_string;

	return dt;

}

bool
Parse_Context::matchesDeclaration() {
	Token* p = peek();
	if (!p) {
		return false;
	}
	return isIdentifier() && p->word == ":"; 
}

// TODO make this better
bool
Parse_Context::matchesDatatype() {
	std::string& word = focus->word;
	if (word == "static" || word == "const" || word == "foreign") {
		return true;
	}
	if (word == "^" || word == "[") {
		return true;
	}
	if (getTypeInfo(word)) {
		return true;
	}
	return false;
}

Var_Declaration*
Parse_Context::parseDeclaration() {
	
	Var_Declaration* decl = new Var_Declaration();	
	decl->identifier = focus->word;
	
	next(); // skip id
	eatOperator(":");

	decl->dt = parseDatatype();
	if (decl->dt->type == DATA_VOID) {
		die("cannot declare a variable of type 'void'");
	}
	decl->as_string = decl->identifier + ": " + decl->dt->as_string;

	return decl;


}

Expression*
Parse_Context::parseExpression() {

	std::vector<Token> raw;
	std::vector<Expression_Operator*> ops;
	std::vector<Expression*> postfix;
	Expression* exp;

	if (token_index == marked) {
		return nullptr;
	}
	
	// first, gather the raw expression
	for (int i = token_index; i < marked; i++) {
		raw.push_back(*atIndex(i));
	}

	auto shunting_pops = [&ops, &postfix](const Operator_Descriptor* desc) -> void {
		Expression_Operator* back;
		while (true) {
			if (ops.size() == 0) {
				break;
			}
			back = ops.back();
			// bug?
			if (back->isUnaryType(UNARY_OPEN_PARENTHESIS)) {
				break;
			}
			if (desc->assoc == ASSOC_LEFT) {
				if (desc->prec > back->desc->prec) break;
			} else {
				if (desc->prec >= back->desc->prec) break;
			}
			postfix.push_back(back);
			ops.pop_back();
		}
	};

	// now scan the expression and use shunting yard
	int start = token_index;
	while (token_index < marked) {
		Token& tok = *focus;
		Token* prev = nullptr;
		if (token_index - 1 >= start) {
			prev = atIndex(token_index - 1);
		}
		if (matchesDatatype()) {
			Expression_Datatype* dt = parseDatatypeAsExpression();
			postfix.push_back(dt);
		} else {
			next();
			switch (tok.type) {
				case TOK_INTEGER: {
					Expression_Integer* push = new Expression_Integer();
					push->value = tok.i;
					push->word = tok.word;
					postfix.push_back(push);
					break;
				}
				case TOK_FLOAT: {
					Expression_Float* push = new Expression_Float();
					push->value = tok.f;
					push->word = tok.word;
					postfix.push_back(push);
					break;
				}
				case TOK_STRING: {
					Expression_String* push = new Expression_String();
					push->value = tok.word;
					push->word = tok.word;
					push->literal_index = string_literals.size();
					postfix.push_back(push);
					string_literals.push_back(push->value);
					break;
				}
				case TOK_IDENTIFIER: {
					Expression_Identifier* push = new Expression_Identifier();
					push->value = tok.word;
					push->word = tok.word;
					postfix.push_back(push);
					break;
				}
				case TOK_OPERATOR: {
					switch (tok.i) {
						case '(': { 
							if (prev && prev->type == TOK_IDENTIFIER) {
								int finish_save = marked;
								Expression_Call* push = new Expression_Call();
								markOperator("(", ")");
								push->argument = parseExpression();
								next(); // skip )
								marked = finish_save;

								// count the number of arguments to the call by walking the comma tree
								if (push->argument) {
									push->num_args++;
									Expression* scan = push->argument;
									while (scan && scan->isBinaryType(BINARY_COMMA)) {
										push->num_args++;
										scan = dynamic_cast<Expression_Binary *>(scan)->left;
									}
								}		
								shunting_pops(push->desc);
								ops.push_back(push);
							} else {
								Expression_Unary* push = new Expression_Unary();
								push->optype = UNARY_OPEN_PARENTHESIS;
								push->word = "(";
								ops.push_back(push);
							}
							break;
						}
						case ')': {
							Expression* back;
							while (true) {
								if (ops.size() == 0) {
									die("unexpected closing parenthesis");
								}
								back = ops.back();
								if (back->isUnaryType(UNARY_OPEN_PARENTHESIS)) {
									break;
								}
								postfix.push_back(back);
								ops.pop_back();
							}
							ops.pop_back();
							break;
						}
						case '#': {
							Expression_Cast* push = new Expression_Cast();
							push->word = "__CAST__";
							push->target = parseDatatype();
							shunting_pops(push->desc);
							ops.push_back(push);
							break;
						}
						default: {
							const Operator_Descriptor* desc = get_operator_descriptor(tok.word);
							Expression_Operator* push;
							if (!desc) {
								std::string err = "unknown operator '";
								err += tok.word;
								err += "'";
								die(err);
							}
							if (desc->operand_type == OP_BINARY) {
								Expression_Binary* op = new Expression_Binary();
								op->optype = Expression_Binary::wordToBinaryType(tok.word);
								push = op;
							} else if (desc->operand_type == OP_UNARY) {
								Expression_Unary* op = new Expression_Unary();
								op->optype = Expression_Unary::wordToUnaryType(tok.word);
								push = op;
							}
							push->desc = desc;
							push->word = tok.word;
							shunting_pops(desc);
							ops.push_back(push);
							break;
						}
					}
					break;
				}
			}
		}
	}
	while (ops.size() > 0) {
		Expression_Operator* back = ops.back();
		if (back->isUnaryType(UNARY_OPEN_PARENTHESIS)) {
			die("mismatched parentheses");
		}
		postfix.push_back(back);
		ops.pop_back(); 
	}	

	std::vector<Expression*> last;

	for (Expression* e: postfix) {
		switch (e->type) {
			case EXP_INTEGER:
			case EXP_FLOAT:
			case EXP_IDENTIFIER:
			case EXP_STRING:
			case EXP_DATATYPE:
				last.push_back(e);
				break;	
			case EXP_BINARY: {
				Expression* operands[2];
				Expression_Binary* binary_op = dynamic_cast<Expression_Binary *>(e);
				for (int i = 0; i < 2; i++) {
					if (last.size() == 0) {
						// TODO make a better error message
						die("malformed expression");
					}
					Expression* op = last.back();
					op->parent = e;
					last.pop_back();
					operands[i] = op;
				}
				operands[0]->side = LEAF_RIGHT;
				operands[1]->side = LEAF_LEFT;
				binary_op->right = operands[0];
				binary_op->left = operands[1];
				last.push_back(e);
				break;
			}
			case EXP_UNARY: {
				Expression_Unary* unary_op = dynamic_cast<Expression_Unary *>(e);
				Expression* operand = last.back();
				last.pop_back();
				unary_op->operand = operand;
				operand->parent = e;
				last.push_back(e);
				break;
			}
			case EXP_CAST: {
				Expression_Cast* cast_op = dynamic_cast<Expression_Cast *>(e);
				Expression* operand = last.back();
				last.pop_back();
				cast_op->operand = operand;
				operand->parent = e;
				last.push_back(e);
				break;
			}
			case EXP_CALL: {
				Expression_Call* call_op = dynamic_cast<Expression_Call *>(e);
				Expression* operand = last.back();
				last.pop_back();
				call_op->procedure = operand;
				operand->parent = e;
				last.push_back(e);
				break;
			}
		}
	}
	
	if (last.size() != 1) {
		die("an expression must have exactly one result");
	}

	setIndex(marked);

	return last.back();
}

Expression*
Parse_Context::parseAndTypecheckExpression() {
	Expression* exp = parseExpression();
	if (exp) {
		//exp->print();
		exp->typecheck(this);
	}
	return exp;
}

void
Parse_Context::handleIf() {
	Ast_If* node = new Ast_If();
	eatIdentifier("if");
	eatOperator("(");
	markOperator("(", ")");
	node->cond = parseAndTypecheckExpression();
	eatOperator(")");
	appendNode(node);
}

void
Parse_Context::handleWhile() {
	Ast_While* node = new Ast_While();
	eatIdentifier("while");
	eatOperator("(");
	markOperator("(", ")");
	node->cond = parseAndTypecheckExpression();
	eatOperator(")");
	appendNode(node);
}

void
Parse_Context::handleFor() {
	Ast_For* node = new Ast_For();
	eatIdentifier("for");
	eatOperator("(");
	markOperator("", ";");
	node->init = parseAndTypecheckExpression();
	eatOperator(";");
	markOperator("", ";");
	node->cond = parseAndTypecheckExpression();
	eatOperator(";");
	markOperator("(", ")");
	node->each = parseAndTypecheckExpression();
	eatOperator(")");
	appendNode(node);
}

void
Parse_Context::handleBlock() {
	Ast_Block* node = new Ast_Block();
	eatOperator("{");
	appendNode(node);
	current_block = node;
}

void
Parse_Context::handleCloseBlock() {
	Ast_Node* parent = current_block->parent;
	while (parent && !dynamic_cast<Ast_Block *>(parent)) {
		parent = parent->parent;
	}
	current_block = dynamic_cast<Ast_Block *>(parent);
	if (!current_block) {
		die("unexpected closing bracket '}'");
	}
	next();
}

void
Parse_Context::handleStatement() {
	Ast_Statement* node = new Ast_Statement();
	markOperator("", ";");
	node->expression = parseAndTypecheckExpression();
	appendNode(node);
	eatOperator(";");
}

void
Parse_Context::handleDeclaration() {

	Var_Declaration* decl = parseDeclaration();
	Datatype_Base* dt = decl->dt;

	// if its a procedure followed by {, it's an implementation */
	if (dt->type == DATA_PROCEDURE && onOperator("{")) {
		Datatype_Procedure* proc_desc = dynamic_cast<Datatype_Procedure *>(dt);

		if (proc_desc->mods & MOD_FOREIGN) {
			die("attempt to implement a foreign procedure");
		}

		for (const Var_Declaration* arg: proc_desc->args) {
			if (!arg->named) {
				die("all arguments must be named in a procedure implementation");
			}
		}

		// wrap the procedure in a node
		Ast_Procedure* node = new Ast_Procedure();
		node->implemented = true;
		node->identifier = decl->identifier;
		node->desc = proc_desc;
		current_procedure = node;
		
		for (const Ast_Procedure* p: defined_procedures) {
			if (p->identifier == node->identifier) {
				die("re-implementation of procedure '" + p->identifier + "'");
			}	
		}
		
		defined_procedures.push_back(node);
		appendNode(node);
		registerLocal(decl);

	} else if (dt->type == DATA_PROCEDURE) {
		Datatype_Procedure* proc_desc = dynamic_cast<Datatype_Procedure *>(dt);
		
		if (proc_desc->mods & MOD_FOREIGN) {
			Ast_Procedure* node = new Ast_Procedure();
			node->implemented = true;
			node->identifier = decl->identifier;
			node->desc = proc_desc;
			defined_procedures.push_back(node);
		}

		registerLocal(decl);
	
	// declaration and assignment?
	} else if (onOperator("=")) {
		Ast_Statement* state;
		Expression_Identifier* id;
		Expression_Binary* assign;
		
		registerLocal(decl);

		// manually patch together an assignment
		state = new Ast_Statement();
		id = new Expression_Identifier();
		assign = new Expression_Binary();

		next(); // skip '='
		markOperator("", ";");
		assign->optype = BINARY_ASSIGNMENT;
		assign->left = id;
		assign->right = parseAndTypecheckExpression(); 	
		assign->left->parent = assign;
		assign->right->parent = assign;
		assign->right->side = LEAF_RIGHT;
		assign->word = "=";
		state->expression = assign;
		id->word = decl->identifier;
		id->value = decl->identifier;
		id->var = decl;
		id->eval = assign->right->eval;
		id->side = LEAF_LEFT;
		if (!assign->right->eval->matches(*decl->dt)) {
			typeMismatch("=", *decl->dt, *assign->right->eval);
		}
		appendNode(state);
	} else {
		registerLocal(decl);
	}

}

void
Parse_Context::handleInferredDeclaration() {
	
	Var_Declaration* decl = new Var_Declaration();
	decl->identifier = focus->word;
	next(); // skip identifier
	next(); // skip ':='
	markOperator("", ";");
	Expression* exp = parseAndTypecheckExpression();
	decl->dt = exp->eval;
	decl->as_string = Var_Declaration::toString(*decl);
	
	if (exp->eval->type == DATA_STRUCT && !exp->eval->is_ptr) {
		die("attempt to assign to an object");
	}
	if (exp->eval->type == DATA_VOID) {
		die("attempt to assign expression that evaluates to type 'void'");
	}
	if (getLocal(decl->identifier)) {
		die("duplicate variable '" + decl->identifier + "'");
	}

	// now we need to manually make an expression and append it
	Ast_Statement* state;
	Expression_Identifier* id;
	Expression_Binary* assign;
	state = new Ast_Statement();
	id = new Expression_Identifier();
	assign = new Expression_Binary();
	id->value = decl->identifier;
	id->word = decl->identifier;
	id->var = decl;
	id->eval = exp->eval;
	assign->optype = BINARY_ASSIGNMENT;
	assign->word = "=";
	assign->right = exp;
	assign->left = id;
	assign->right->parent = assign;
	assign->left->parent = assign;
	id->side = LEAF_LEFT;
	exp->side = LEAF_RIGHT;
	state->expression = assign;
	appendNode(state);

	eatOperator(";");
	registerLocal(decl);
}

void
Parse_Context::handleReturn() {
	if (!current_procedure) {
		die("a return statement can only exist inside of a procedure body");
	}
	Ast_Return* node = new Ast_Return();
	eatIdentifier("return");
	markOperator("", ";");
	node->expression = parseAndTypecheckExpression();
	if (node->expression) {
		if (!current_procedure->desc->ret->matches(*node->expression->eval)) {
			std::string err = "attempt to return value of type '";
			err += node->expression->eval->as_string;
			err += "' from procedure '";
			err += current_procedure->identifier;
			err += "', (expected type '";
			err += current_procedure->desc->ret->as_string;
			err += "')";
			die(err);	
		}
	} else {
		if (current_procedure->desc->ret->type != DATA_VOID) {
			std::string err = "procedure '";
			err += current_procedure->identifier;
			err += "' must return a value of type '";
			err += current_procedure->desc->ret->as_string;
			err += "'";
			die(err);
		}
	}
	eatOperator(";");
	appendNode(node);
}

void
Parse_Context::handleStruct() {
	Var_Declaration* decl = new Var_Declaration();
	Datatype_Struct* str = new Datatype_Struct();
	decl->dt = str;
	eatIdentifier("struct");
	if (!isIdentifier()) {
		die("expected identifier following token 'struct'"); 
	}
	decl->identifier = focus->word;
	str->size = 0;
	next();
	eatOperator("{");
	while (matchesDeclaration()) {
		Var_Declaration* member = parseDeclaration();
		member->offset = str->total_members;
		str->size += member->dt->size;
		str->members.push_back(member);
		eatOperator(";");
		
		if (member->dt->isRawStruct()) {
			Datatype_Struct* mem_str = dynamic_cast<Datatype_Struct *>(member->dt);
			str->total_members += mem_str->total_members;
		} else {
			str->total_members++;
		}
	}
	eatOperator("}");
	eatOperator(";");
	str->as_string = str->tostr();
	defined_types.push_back(decl);
}

void
Parse_Context::handleDefine() {
	Var_Declaration* decl;
	eatIdentifier("define");
	if (!matchesDeclaration()) {
		die("expected declaration following token 'define'");
	}
	decl = parseDeclaration();
	eatOperator(";");
	defined_types.push_back(decl);	
}

void
Parse_Context::handleImport() {
	eatIdentifier("import");
	std::string import_name = focus->id;
	if (focus->type != TOK_STRING) {
		die("expected string to follow token 'import'");
	}
	next();
	eatOperator(";");

	if (current_block != root) {
		die("an import statement can only be used in the global scope");
	}
	
	std::vector<Token> tokens = Lexer::generateTokens(import_name);
	Parse_Context* import_context = Parse_Context::generateSyntaxTree(import_name, tokens);
	
	for (Ast_Procedure* proc: import_context->defined_procedures) {
		defined_procedures.push_back(proc);	
	}

	for (Var_Declaration* decl: import_context->defined_types) {
		defined_types.push_back(decl);
	}

	for (Var_Declaration* local: import_context->root->locals) {
		root->locals.push_back(local);
	}

	for	(Ast_Node* node: import_context->root->children) {
		root->children.push_back(node);	
	}

	delete import_context;
}

void
Parse_Context::registerLocal(Var_Declaration* local) {
	if (getLocal(local->identifier)) {
		die("duplicate variable '" + local->identifier + "'");
	}
	current_block->locals.push_back(local);
}

void
Parse_Context::appendNode(Ast_Node* node) {
	node->line = focus ? focus->line : 0;
	if (append_target) {
		switch (append_target->type) {
			case AST_IF:
				dynamic_cast<Ast_If *>(append_target)->child = node;
				break;	
			case AST_WHILE:
				dynamic_cast<Ast_While *>(append_target)->child = node;
				break;	
			case AST_FOR:
				dynamic_cast<Ast_For *>(append_target)->child = node;
				break;
			case AST_PROCEDURE:
				dynamic_cast<Ast_Procedure *>(append_target)->child = node;
				break;
		}
		node->parent = append_target;
	} else {
		current_block->children.push_back(node);
		node->parent = current_block;
	}
	switch (node->type) {
		case AST_IF:
		case AST_WHILE:
		case AST_FOR:
		case AST_PROCEDURE:
			append_target = node;
			break;	
		default:
			append_target = nullptr;
	}
	if (node->type == AST_BLOCK) {
		current_block = dynamic_cast<Ast_Block *>(node);
	}
}

Parse_Context*
Parse_Context::generateSyntaxTree(const std::string& filename, std::vector<Token>& tokens) {
	
	Parse_Context* parse = new Parse_Context();
	parse->focusTokens(tokens);
	parse->filename = filename;

	Token* p;
	
	while (parse->focus) {
		p = parse->peek();
		std::string& word = parse->focus->word;
		if (word == "if") {
			parse->handleIf();	
		} else if (word == "while") {
			parse->handleWhile();
		} else if (word == "for") {
			parse->handleFor();
		} else if (word == "return") {
			parse->handleReturn();
		} else if (word == "struct") {
			parse->handleStruct();
		} else if (word == "define") {
			parse->handleDefine();
		} else if (word == "import") {
			parse->handleImport();
		} else if (word == "{") {
			parse->handleBlock();
		} else if (word == "}") {
			parse->handleCloseBlock();
		} else if (parse->matchesDeclaration()) {
			parse->handleDeclaration();
		} else if (parse->isIdentifier() && p && p->word == ":=") {
			parse->handleInferredDeclaration();
		} else if (word == ";") {
			parse->next();
		} else {
			parse->handleStatement();
		}	
	}
	
	/*	
	parse->root->print();

	std::cout << "other information: \n";
	std::cout << "DECLARED FUNCTIONS: \n";
	for (const Ast_Procedure* proc: parse->defined_procedures) {
		std::cout << '\t' << proc->identifier << ": " << proc->desc->as_string << std::endl;	
	}
	std::cout << "DEFINED TYPES: \n";
	for (const Var_Declaration* def: parse->defined_types) {
		const Datatype_Base* dt = def->dt;
		const Datatype_Struct* str = dynamic_cast<const Datatype_Struct *>(dt);
		std::cout << "  typename: " << def->identifier << std::endl;
		std::cout << "  size:     " << dt->size << std::endl;
		if (str) {
			std::cout << "  struct {\n";
			for (const Var_Declaration* member: str->members) {
				std::cout << "    " << member->as_string << ";" << std::endl;
			}
			std::cout << "  }\n";
		} else {
			std::cout << "  " << dt->as_string << std::endl;
		}
		std::cout << std::endl;
	}
	*/

	return parse;

}
