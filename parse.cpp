#include <sstream>
#include <iostream>
#include <algorithm>
#include <iomanip>
#include "assert.h"
#include "parse.h"

using namespace Parser;
using namespace Lexer;

std::map<std::string, Binary_Operator_Type> Expression_Binary::word_map {
	{"+",   ADDITION},
	{"-",   SUBTRACTION},
	{"*",   MULTIPLICATION},
	{"/",   DIVISION},
	{"%",   MODULUS},
	{"&",   BITWISE_AND},
	{"|",   BITWISE_OR},
	{"<<",  SHIFT_LEFT},
	{">>",  SHIFT_RIGHT},
	{"^",   BITWISE_XOR},
	{"&&",  LOGICAL_AND},
	{"||",  LOGICAL_OR},
	{"=",   ASSIGN},
	{"+=",  ADDITION_BY},
	{"-=",  SUBTRACTION_BY},
	{"*=",  MULTIPLICATION_BY},
	{"/=",  DIVISION_BY},
	{"%=",  MODULUS_BY},
	{"&=",  BITWISE_AND_BY},
	{"|=",  BITWISE_OR_BY},
	{"^=",  BITWISE_XOR_BY},
	{"<<=", SHIFT_LEFT_BY},
	{">>=", SHIFT_RIGHT_BY},
	{"==",  COMPARE},
	{"!=",  COMPARE_NOT},
	{"<",   LESS_THAN},
	{"<=",  LESS_THAN_EQUAL},
	{">",   GREATER_THAN},
	{">=",  GREATER_THAN_EQUAL},
	{".",   FIND_MEMBER}
};

std::map<std::string, Unary_Operator_Type> Expression_Unary::word_map {
	{"!",   LOGICAL_NOT},
	{"~",   BITWISE_NOT},
	{"@",   ADDRESS_OF},
	{"$",   DEREFERENCE},
	{"(",   OPEN_PARENTHESIS},
	{")",   CLOSE_PARENTHESIS}
};

static const std::vector<Operator_Descriptor> operator_table {
	Operator_Descriptor(",",        1, ASSOC_LEFT, OP_BINARY, {IGNORE_ALL_RULES}),
	Operator_Descriptor("=",        2, ASSOC_LEFT, OP_BINARY, {ENFORCE_LEFT_L_VALUE}),
	Operator_Descriptor("+=",       2, ASSOC_LEFT, OP_BINARY, {ENFORCE_LEFT_L_VALUE, ALLOW_POINTER_LEFT_SIDE_INT}),
	Operator_Descriptor("-=",       2, ASSOC_LEFT, OP_BINARY, {ENFORCE_LEFT_L_VALUE, ALLOW_POINTER_LEFT_SIDE_INT}),
	Operator_Descriptor("*=",       2, ASSOC_LEFT, OP_BINARY, {ENFORCE_LEFT_L_VALUE, DISALLOW_POINTER}),
	Operator_Descriptor("/=",       2, ASSOC_LEFT, OP_BINARY, {ENFORCE_LEFT_L_VALUE, DISALLOW_POINTER}),
	Operator_Descriptor("%=",       2, ASSOC_LEFT, OP_BINARY, {ENFORCE_LEFT_L_VALUE, ENFORCE_INTEGER}),
	Operator_Descriptor("&=",       2, ASSOC_LEFT, OP_BINARY, {ENFORCE_LEFT_L_VALUE, ENFORCE_INTEGER}),
	Operator_Descriptor("|=",       2, ASSOC_LEFT, OP_BINARY, {ENFORCE_LEFT_L_VALUE, ENFORCE_INTEGER}),
	Operator_Descriptor("^=",       2, ASSOC_LEFT, OP_BINARY, {ENFORCE_LEFT_L_VALUE, ENFORCE_INTEGER}),
	Operator_Descriptor("&&",       3, ASSOC_LEFT, OP_BINARY, {ENFORCE_BOOL}),
	Operator_Descriptor("||",       3, ASSOC_LEFT, OP_BINARY, {ENFORCE_BOOL}),
	Operator_Descriptor("==",       4, ASSOC_LEFT, OP_BINARY, {}),
	Operator_Descriptor("!=",       4, ASSOC_LEFT, OP_BINARY, {}),
	Operator_Descriptor(">",        6, ASSOC_LEFT, OP_BINARY, {DISALLOW_POINTER}),
	Operator_Descriptor(">=",       6, ASSOC_LEFT, OP_BINARY, {DISALLOW_POINTER}),
	Operator_Descriptor("<",        6, ASSOC_LEFT, OP_BINARY, {DISALLOW_POINTER}),
	Operator_Descriptor("<=",       6, ASSOC_LEFT, OP_BINARY, {DISALLOW_POINTER}),
	Operator_Descriptor("<<",       7, ASSOC_LEFT, OP_BINARY, {ENFORCE_INTEGER}),
	Operator_Descriptor(">>",       7, ASSOC_LEFT, OP_BINARY, {ENFORCE_INTEGER}),
	Operator_Descriptor("+",        8, ASSOC_LEFT, OP_BINARY, {ALLOW_POINTER_INT, DISALLOW_POINTER}),
	Operator_Descriptor("-",        8, ASSOC_LEFT, OP_BINARY, {ALLOW_POINTER_INT, DISALLOW_POINTER}),
	Operator_Descriptor("*",        9, ASSOC_LEFT, OP_BINARY, {DISALLOW_POINTER}),
	Operator_Descriptor("/",        9, ASSOC_LEFT, OP_BINARY, {DISALLOW_POINTER}),
	Operator_Descriptor("__CAST__", 10, ASSOC_RIGHT, OP_UNARY, {IGNORE_ALL_RULES}),
	Operator_Descriptor("@",        10, ASSOC_RIGHT, OP_UNARY, {ENFORCE_L_VALUE}),
	Operator_Descriptor("$",        10, ASSOC_RIGHT, OP_UNARY, {DISALLOW_NON_POINTER}),
	Operator_Descriptor("!",        10, ASSOC_RIGHT, OP_UNARY, {ENFORCE_BOOL}),
	Operator_Descriptor("new",      10, ASSOC_RIGHT, OP_UNARY, {ENFORCE_DATATYPE}),
	Operator_Descriptor(".",        11, ASSOC_LEFT, OP_BINARY, {}) // special case...
};

// DATATYPE IMPLEMENTATION 
Datatype_Information::Datatype_Information(const Datatype_Information& to_copy) {
	ptr_dim = to_copy.ptr_dim;
	arr_dim = to_copy.arr_dim;
	size = to_copy.size;
	type_name = to_copy.type_name;
}

Datatype_Information*
Datatype_Information::clone() const {
	if (const Struct_Information* a = dynamic_cast<const Struct_Information *>(this)) {
		return new Struct_Information(*a);
	}
	return nullptr; // should never be reached
}

bool
Datatype_Information::matches(const Datatype_Information& other) const {
	if (type_name != other.type_name) {
		return false;
	}
	if (ptr_dim != other.ptr_dim) {
		return false;
	}
	if (arr_dim != other.arr_dim) {
		return false;
	}
	return true;
}

std::string
Datatype_Information::to_string() const {
	std::ostringstream buf;
	for (int i = 0; i < ptr_dim; i++) {
		buf << '^';
	}
	for (int i = 0; i < arr_dim; i++) {
		buf << "[]";
	}
	buf << type_name;
	return buf.str(); 
}

std::string
Procedure_Information::to_string() const {
	std::ostringstream buf;
	buf << "(";
	size_t nargs = args.size();
	for (int i = 0; i < nargs; i++) {
		buf << args[i]->dt->to_string();
		if (i < nargs - 1) {
			buf << ", ";
		}
	}
	buf << ") -> ";
	buf << ret->to_string();
	return buf.str();
}

Struct_Information::Struct_Information(const Struct_Information& to_copy): Datatype_Information(to_copy) {
	for (const auto field: to_copy.fields) {
		fields.push_back(new Struct_Field(*field));
	}
	is_complete = to_copy.is_complete;
}

Procedure_Information::Procedure_Information(const Procedure_Information& to_copy): Datatype_Information(to_copy) {
	for (const auto arg: to_copy.args) {
		args.push_back(new Variable_Declaration(*arg));
	}
	is_implemented = to_copy.is_implemented;
}

std::string 
Procedure_Information::make_signature(const std::vector<Variable_Declaration *>& call_info) {
	// ** RULES FOR SIGNATURE GENERATION **
	// the signature of a procedure is simply all the
	// types of its arguments concatenated
	std::stringstream sig;
	for (const auto arg: call_info) {
		sig << arg->dt->to_string();	
	}
	return sig.str();
}

std::string
Procedure_Information::get_signature() const {
	return Procedure_Information::make_signature(args);
}

bool
Procedure_Information::matches(const Datatype_Information& other) const {
	const auto proc_other = dynamic_cast<const Procedure_Information *>(&other);
	if (!proc_other) {
		return false;
	}
	if (args.size() != proc_other->args.size()) {
		return false;
	}
	for (int i = 0; i < args.size(); i++) {
		if (!args[i]->dt->matches(*proc_other->args[i]->dt)) {
			return false;
		}
	}
	if (!ret->matches(*proc_other->ret)) {
		return false;
	}
	return true;
}

Integer_Information::Integer_Information(const Integer_Information& to_copy): Datatype_Information(to_copy) {

}

Void_Information::Void_Information(const Void_Information& to_copy): Datatype_Information(to_copy) {

}

Bool_Information::Bool_Information(const Bool_Information& to_copy): Datatype_Information(to_copy) {

}

// VARIABLE DECLARATION IMPLEMENTATION
Variable_Declaration::Variable_Declaration(const Variable_Declaration& to_copy) {
	identifier = to_copy.identifier;
	dt = new Datatype_Information(*to_copy.dt);
}

Variable_Declaration*
Variable_Declaration::clone() const {
	return new Variable_Declaration(*this);
}

std::string
Variable_Declaration::to_string() const {
	std::ostringstream buf;
	buf << identifier;
	buf << ": ";
	buf << dt->to_string();
	return buf.str();
}

Struct_Field::Struct_Field(const Struct_Field& to_copy) {
	decl = new Variable_Declaration(*to_copy.decl);
	parent_struct = to_copy.parent_struct; // TODO bad?
}

// EXPRESSION IMPLEMENTATION
bool
Expression::is_binary_type(Binary_Operator_Type type) const {
	if (const Expression_Binary* binop = dynamic_cast<const Expression_Binary *>(this)) {
		return binop->value == type;
	}
	return false;	
}

bool
Expression::is_unary_type(Unary_Operator_Type type) const {
	if (const Expression_Unary* unop = dynamic_cast<const Expression_Unary *>(this)) {
		return unop->value == type;
	}
	return false;	
}

Binary_Operator_Type
Expression_Binary::word_to_type(const std::string& word) {
	return word_map.find(word)->second;
}

std::string
Expression_Binary::type_to_word(Binary_Operator_Type type) {
	for (auto iter = word_map.begin(); iter != word_map.end(); ++iter) {
		if (iter->second == type) {
			return iter->first;
		}
	}
	return "";
}

Unary_Operator_Type
Expression_Unary::word_to_type(const std::string& word) {
	return word_map.find(word)->second;
}

std::string
Expression_Unary::type_to_word(Unary_Operator_Type type) {
	for (auto iter = word_map.begin(); iter != word_map.end(); ++iter) {
		if (iter->second == type) {
			return iter->first;
		}
	}
	return "";
}

// ... to string methods ...
std::string
Expression_Unary::to_string() const {
	return type_to_word(value);
}

std::string
Expression_Binary::to_string() const {
	return type_to_word(value);
}

std::string
Expression_Integer_Literal::to_string() const {
	return std::to_string(value);
}

std::string
Expression_Float_Literal::to_string() const {
	return std::to_string(value);
}

std::string
Expression_String_Literal::to_string() const {
	return value;
}

std::string
Expression_Identifier::to_string() const {
	return value;
}

std::string
Expression_Datatype::to_string() const {
	return value->to_string();
}

std::string
Expression_Cast::to_string() const {
	return "#" + value->to_string();	
}

// ... print methods ...

// helper function
void
make_indent(int indent = 0) {
	for (int i = 0; i < indent; i++) {
		std::cout << " ";
	}
}

void
Expression::print(int indent = 0) const {
	make_indent(indent);
	std::cout << to_string() << std::endl;	
}

void
Expression_Binary::print(int indent = 0) const {
	Expression::print(indent);
	left->print(indent + 1);
	right->print(indent + 1);
}

void
Expression_Unary::print(int indent = 0) const {
	Expression::print(indent);
	operand->print(indent + 1);
}

void
Expression_Cast::print(int indent = 0) const {
	Expression::print(indent);
	operand->print(indent + 1);
}

// ... typecheck methods ...
Datatype_Information*
Expression_Binary::typecheck(Parse_Context* context) {

	auto has_rule = [this](Typecheck_Rule rule) -> bool {
		for (auto r: desc->rules) {
			if (rule == r) {
				return true;
			}
		}
		return false;	
	};
	
	Datatype_Information* evaluated = nullptr;
	auto left_eval = left->typecheck(context);

	// '.' operator is a special case
	if (value == FIND_MEMBER) {
		auto struct_data = dynamic_cast<Struct_Information *>(left_eval);
		if (!struct_data) {
			std::stringstream message;
			message << "the left side of the '.' operator must evaluate to an ";
			message << "object or a pointer to an object.  (got type '";
			message << left_eval->to_string();
			message << "')";
			context->report_error_at_indent(message.str(), token->col);
		}
		if (right->type != EXPRESSION_IDENTIFIER) {
			context->report_error_at_indent("the right side of the '.' operator must be an identifier",
											token->col);
		}
		auto right_expr = static_cast<Expression_Identifier *>(right);
		auto field = struct_data->get_field(right_expr->value);
		if (!field) {
			std::stringstream message;
			message << "'";
			message << right_expr->value;
			message << "' is not a valid field of struct '";
			message << struct_data->type_name;
			message << "'";
			context->report_error_at_indent(message.str(), token->col);
		}
		return eval = field->decl->dt;
	}
	
	// hold off on this until '.' has been checked....
	// we don't want to typecheck the right side of the 
	// '.' operator under all circumstances
	auto right_eval = right->typecheck(context);

	if (has_rule(IGNORE_ALL_RULES)) {
		// comma should be the only operator that has
		// ignore all rules for now... the evaluated
		// type of the comma doesn't really matter,
		// so for now just return the type of the left...
		// this may need to be changed in the future
		return eval = left_eval;
	}

	if (has_rule(ENFORCE_INTEGER) || has_rule(ENFORCE_BOOL)) {
		bool violated_left;
		bool violated_right;
		std::string expected_type;
		if (has_rule(ENFORCE_INTEGER)) {
			violated_left = !left_eval->is_int();
			violated_right = !right_eval->is_int();
			expected_type = "int";
		} else { 
			violated_left = !left_eval->is_bool();
			violated_right = !right_eval->is_bool();
			expected_type = "bool";
		}
		if (violated_left || violated_right) {
			auto side_name = violated_left ? "left" : "right";
			auto side_got  = violated_left ? left_eval->to_string() : right_eval->to_string();
			std::stringstream message;
			message << side_name;
			message << " operand of operator '";
			message << to_string();
			message << "' must be of type '";
			message << expected_type;
			message << "' (got type '";
			message << side_got;
			message << "')";
			context->report_error_at_indent(message.str(), tok->col);
		}
	}

	if (!evaluated && has_rule(ALLOW_POINTER_LEFT_SIDE_INT) && left_eval->is_pointer()) {
		if (!right_eval->is_int()) {
			std::stringstream message;
			message << "when the left operand of operator '";
			message << to_string();
			message << "' is a pointer, the right operand must be an integer.  (got type '";
			message << right_eval->to_string();
			message << "')";
			context->report_error_at_indent(message.str(), tok->col);
		} else {
			evaluated = left_eval;
		}
	}

	if (!evaluated && has_rule(ALLOW_POINTER_INT)) {
		bool left_is_int = left_eval->is_int();
		bool left_is_ptr = left_eval->is_pointer();	
		bool right_is_int = right_eval->is_int();
		bool right_is_ptr = right_eval->is_pointer();	
		if (left_is_int && right_is_ptr) {
			evaluated = right_eval;
		} else if (left_is_ptr && right_is_int) {
			evaluated = left_eval;
		}
	}

	if (has_rule(DISALLOW_POINTER)) {
		bool violated_left = left_eval->is_pointer();
		bool violated_right = left_eval->is_pointer();
		if (violated_left || violated_right) {
			std::stringstream message;
			auto side_name = violated_left ? "left" : "right";
			auto side_name_not = violated_left ? "right" : "left";
			auto side_ptr_not = violated_left ? right_eval : left_eval;
			if (has_rule(ALLOW_POINTER_INT)) {
				message << side_name;
				message << " operand of operator '";
				message << to_string();
				message << "' can only be a pointer if the ";
				message << side_name_not;
				message << " operand is an integer. (";
				message << side_name_not;
				message << " operand evalues to type '";
				message << side_ptr_not->to_string();
				message << "')";
			} else {
				message << side_name;
				message << " operand of operator '";
				message << to_string();
				message << "' cannot be a pointer.";
			}
			context->report_error_at_indent(message.str(), tok->col);
		}
	}

	if (has_rule(ENFORCE_LEFT_L_VALUE) && !left->is_l_value) {
		std::stringstream message;
		message << "the left operand of operator '";
		message << to_string();
		message << "' must be an L-value. (it is currently an R-value)";
		context->report_error_at_indent(message.str(), tok->col);
	}
	
	// for now, any binary operator results in an R-value
	is_l_value = false;	

	// implicit int->float cast
	if (!evaluated && left_eval->is_float() && right_eval->is_int()) {
		evaluated = left_eval;
	}
	
	// implicit int->float cast
	if (!evaluated && right_eval->is_float() && left_eval->is_int()) {
		evaluated = right_eval;
	}

	if (!evaluated && !left_eval->matches(*right_eval)) {
		std::stringstream message;
		message << "type mismatch at operator '";
		message << to_string();
		message << "'. (got '";
		message << left_eval->to_string();
		message << "' and '";
		message << right_eval->to_string();
		message << "')";
		context->report_error_at_indent(message.str(), tok->col);
	}

	// comparison operators always result in bool
	if (value == COMPARE || value == COMPARE_NOT
		|| value == LESS_THAN || value == LESS_THAN_EQUAL
		|| value == GREATER_THAN || value == GREATER_THAN_EQUAL) {
		evaluated = context->type_bool;	
	}

	if (!evaluated) {
		evaluated = left_eval;
	}

	return eval = evaluated;
	
}

Datatype_Information*
Expression_Unary::typecheck(Parse_Context* context) {

	auto has_rule = [this](Typecheck_Rule rule) -> bool {
		for (auto r: desc->rules) {
			if (rule == r) {
				return true;
			}
		}
		return false;	
	};

	auto operand_eval = operand->typecheck(context);

	if (auto proc = dynamic_cast<Procedure_Information *>(operand_eval)) {
		if (!proc->is_pointer()) {
			std::stringstream message;
			message << "operand of operator '";
			message << to_string();
			message << "' is an invalid type (got '";
			message << proc->to_string();
			message << "'";
			context->report_error_at_indent(message.str(), operand->token->col);
		}
	}

	if (has_rule(ENFORCE_L_VALUE) && !operand->is_l_value) {
		std::stringstream message;
		message << "operand of operator '";
		message << to_string();
		message << "' must be an L-value. (it is currently an R-value)";
		context->report_error_at_indent(message.str(), tok->col);
	}

	if (value == ADDRESS_OF) {
		is_l_value = true;
	} else {
		is_l_value = false;
	}

	return eval = operand_eval;

}

Datatype_Information*
Expression_Cast::typecheck(Parse_Context* context) {
	auto op_type = operand->typecheck(context);

	if (dynamic_cast<Procedure_Information *>(op_type) || dynamic_cast<Procedure_Information *>(value)) {
		context->report_error_at_indent("it is illegal to cast to or from a procedure pointer", token->col);
	}
	
	// no need to check if casting to the same type
	if (value->matches(*op_type)) {
		return eval = value;
	}

	// ensure that it is a valid cast
	if (value->is_int() || value->is_bool()) {
		// if we're casting to an int/bool, the only valid operand type
		// is a float, byte, bool, or pointer... (e.g. structs are not allowed,
		// as that does not make any sense)
		if (!op_type->is_float() && !op_type->is_byte() && !op_type->is_bool() && !op_type->is_int() && !op_type->is_pointer()) {
			std::stringstream message;
			message << "invalid cast from type '";
			message << op_type->to_string();
			message << "' to type '";
			message << value->to_string();
			message << "' (only floats, bytes, bools, and pointers can be cast to '";
			message << value->to_string();
			message << "')";
			context->report_error_at_indent(message.str(), token->col);
		}

	}

	return eval = value;
}

Datatype_Information*
Expression_Integer_Literal::typecheck(Parse_Context* context) {
	is_l_value = false;
	return eval = context->type_int;
}

Datatype_Information*
Expression_Float_Literal::typecheck(Parse_Context* context) {
	is_l_value = false;
	return eval = context->type_float;
}

Datatype_Information*
Expression_String_Literal::typecheck(Parse_Context* context) {
	is_l_value = false;
	return nullptr;
}

Datatype_Information*
Expression_Identifier::typecheck(Parse_Context* context) {
	is_l_value = true;
	if (value == "true" || value == "false") {
		return eval = context->type_bool;
	}
	if (auto var = context->get_local(value)) {
		variable = var;
		return eval = var->dt;	
	}
	auto all_procs = context->get_all_procedures(value);
	if (all_procs.size() == 1) {
		return eval = all_procs[0]->info;
	} else if (all_procs.size() > 1) {
		
		Procedure_Information* proc_context = nullptr;

		auto append_candidates = [&](std::stringstream& stream) -> void {

			int biggest_line = 0;
			int line_count;
			std::for_each(all_procs.begin(), all_procs.end(), [&biggest_line](auto at_proc) {
				if (at_proc->declared_line > biggest_line) {
					biggest_line = at_proc->declared_line;
				}
			});
			line_count = std::to_string(biggest_line).length();

			for (auto proc: all_procs) {
				stream << "\tline #" << std::setw(line_count) << std::setfill('0') << proc->declared_line;
				stream << ": " << proc->info->to_string() << std::endl;
			}
		};

		auto not_enough_context = [&]() -> void {
			std::stringstream message;
			message << "there is not enough context to determine what '";
			message << value;
			message << "' refers to.\npossible candidates:\n";
			append_candidates(message);
			context->report_error_at_indent(message.str(), token->col);
		};

		auto no_matching_overload = [&](Procedure_Information* got) -> void {
			std::stringstream message;
			message << "no matching overload found for procedure '";
			message << value;
			message << "'\n";
			message << "inferred type:\n\t";
			message << got->to_string();
			message << "\npossible candidates:\n";
			append_candidates(message);
			context->report_error_at_indent(message.str(), token->col);
		};
		
		// if reached here there is more than one possible
		// procedure... therefore some context is needed to
		// determine which one should be picked.
		if (!parent) {
			not_enough_context();
		}

		switch (parent->type) {
			case EXPRESSION_OPERATOR_BINARY: {
				auto bin = static_cast<Expression_Binary *>(parent);
				if (bin->value == ASSIGN) {
					switch (side) {
						case LEAF_LEFT:
							proc_context = dynamic_cast<Procedure_Information *>(bin->right->typecheck(context));	
							break;
						case LEAF_RIGHT:
							proc_context = dynamic_cast<Procedure_Information *>(bin->left->typecheck(context));	
							break;	
					}
				}
				break;
			}
			default:
				break;
		}

		if (!proc_context) {
			not_enough_context();
		}

		// now proc_context holds the type that we should
		// be referring to.....	
		auto inferred_descriptor = context->get_procedure(value, proc_context->get_signature());

		if (!inferred_descriptor) {
			no_matching_overload(proc_context);
		}

		return eval = inferred_descriptor->info;

	}
	std::stringstream message;
	message << "use of undeclared identifier '";
	message << value;
	message << "'";
	context->report_error_at_indent(message.str(), token->col);
	return nullptr;
}

Datatype_Information*
Expression_Datatype::typecheck(Parse_Context* context) {
	is_l_value = false;	
	return eval = value;
}

// AST NODE IMPLEMENTATION

void
Ast_Statement::print(int indent = 0) const {
	make_indent(indent);
	std::cout << "STATEMENT: [\n";
	make_indent(indent + 1);
	std::cout << "EXPRESSION: [\n";
	expression->print(indent + 2);
	make_indent(indent + 1);
	std::cout << "]\n";
	make_indent(indent);
	std::cout << "]\n";
}

void
Ast_Block::print(int indent = 0) const {
	make_indent(indent);
	std::cout << "BLOCK: [\n";
	for (auto child: children) {
		child->print(indent + 1);
	}
	make_indent(indent);
	std::cout << "]\n";
}

void
Ast_Procedure::print(int indent = 0) const {
	make_indent(indent);
	std::cout << "PROCEDURE: [\n";
	make_indent(indent + 1);
	std::cout << "NAME: " << info->type_name << std::endl;
	make_indent(indent + 1);
	std::cout << "RETURN TYPE: " << info->ret->to_string() << std::endl;
	make_indent(indent + 1);
	std::cout << "SIGNATURE: " << info->get_signature() << std::endl;
	make_indent(indent + 1);
	std::cout << "ARGS: [\n";
	for (auto arg: info->args) {
		make_indent(indent + 2);
		std::cout << arg->to_string() << std::endl; 
	}
	make_indent(indent + 1);
	std::cout << "]\n";
	make_indent(indent + 1);
	std::cout << "CHILD: [\n";
	if (child) {
		child->print(indent + 2);
	}
	make_indent(indent + 1);
	std::cout << "]\n";
	make_indent(indent);
	std::cout << "]\n";
}

void
Ast_If::print(int indent = 0) const {
	make_indent(indent);
	std::cout << "IF: [\n";
	make_indent(indent + 1);
	std::cout << "CONDITION: [\n";
	condition->print(indent + 2);
	make_indent(indent + 1);
	std::cout << "]\n";
	make_indent(indent + 1);
	std::cout << "CHILD: [\n";
	if (child) {
		child->print(indent + 2);
	}
	make_indent(indent + 1);
	std::cout << "]\n";
	make_indent(indent);
	std::cout << "]\n";
}

void
Ast_While::print(int indent = 0) const {
	make_indent(indent);
	std::cout << "WHILE: [\n";
	make_indent(indent + 1);
	std::cout << "CONDITION: [\n";
	condition->print(indent + 2);
	make_indent(indent + 1);
	std::cout << "]\n";
	make_indent(indent + 1);
	std::cout << "CHILD: [\n";
	if (child) {
		child->print(indent + 2);
	}
	make_indent(indent + 1);
	std::cout << "]\n";
	make_indent(indent);
	std::cout << "]\n";
}

void
Ast_For::print(int indent = 0) const {
	make_indent(indent);
	std::cout << "FOR: [\n";
	make_indent(indent + 1);
	std::cout << "INITIALIZER: [\n";
	if (initializer) {
		initializer->print(indent + 2);
	}
	make_indent(indent + 1);
	std::cout << "]\n";
	make_indent(indent + 1);
	std::cout << "CONDITION: [\n";
	if (condition) {
		condition->print(indent + 2);
	}
	make_indent(indent + 1);
	std::cout << "]\n";
	make_indent(indent + 1);
	std::cout << "STATEMENT: [\n";
	if (statement) {
		statement->print(indent + 2);
	}
	make_indent(indent + 1);
	std::cout << "]\n";
	make_indent(indent + 1);
	std::cout << "CHILD: [\n";
	if (child) {
		child->print(indent + 2);
	}
	make_indent(indent + 1);
	std::cout << "]\n";
	make_indent(indent);
	std::cout << "]\n";
}

void
Ast_Declaration::print(int indent = 0) const {
	make_indent(indent);
	std::cout << "DECLARATION: [\n";
	make_indent(indent + 1);
	std::cout << "IDENTIFIER: " << decl->identifier << std::endl;
	make_indent(indent + 1);
	std::cout << "DATATYPE:   " << decl->dt->to_string() << std::endl;
	make_indent(indent);
	std::cout << "]\n";
}


// PARSE CONTEXT IMPLEMENTATION
void
Parse_Context::report_error(const std::string& message) const {
	int line = 0;
	if (token) {
		line = token->line;
	} else {
		line = lex_context->raw_file.size();
	}
	std::cerr << "\n-------- SPYRE PARSE ERROR -------\n\n";
	std::cerr << "message: ";
	for (char c: message) {
		if (c == '\n') {
			std::cout << "\n         ";
		} else {
			std::cout << c;
		}
	}
	std::cout << std::endl;
	std::cerr << "line:    " << line << std::endl;
	std::cerr << "near:    " << lex_context->raw_file[line - 1] << std::endl;
	std::cerr << "         ";
	if (fail_indent > -1) {
		for (int i = 0; i < fail_indent; i++) {
			std::cerr << "_";
		}
		std::cerr << "^";
		for (int i = fail_indent + 1; i < lex_context->raw_file[line - 1].length() - 1; i++) {
			std::cerr << "_";
		}
	}
	std::cerr << std::endl << std::endl;
	std::exit(1);
}

void
Parse_Context::report_error_at_indent(const std::string& message, int indent) {
	fail_indent = indent;
	report_error(message);
}

void
Parse_Context::assert_token() const {
	if (!token) {
		report_error("unexpected end of file");
	}
}

void
Parse_Context::focus_token(int index) {
	token_index = index;
	if (token_index >= lex_context->tokens->size()) {
		token = nullptr;
	} else {
		token = &(*lex_context->tokens)[token_index];
	}
}

Token*
Parse_Context::get_token(int index) {
	if (index >= lex_context->tokens->size()) {
		return nullptr;
	}
	return &(*lex_context->tokens)[index];
}

void
Parse_Context::eat(const std::string& word, const std::string& message) {
	assert_token();
	if (token->word != word) {
		report_error(message + ", got token '" + token->word + "'");	
	}
	eat();
}

void
Parse_Context::eat(const std::string& word) {
	assert_token();
	if (token->word != word) {
		std::ostringstream err;
		err << "expected token '";
		err << word;
		err << "', got token '";
		err << token->word;
		err << "'";
		report_error(err.str());
	}
	eat();
}

void
Parse_Context::eat() {
	assert_token();
	token_index++;
	if (token_index >= lex_context->tokens->size()) {
		token = nullptr;
	} else {
		token = &(*lex_context->tokens)[token_index];
	}
}

std::string
Parse_Context::eat_and_get() {
	assert_token();
	std::string& at = token->word;
	eat();
	return at;
}

bool
Parse_Context::on(const std::string& word) const {
	return token->word == word;
}

bool
Parse_Context::on(int peek_index, const std::string& word) const {
	if (token_index + peek_index >= lex_context->tokens->size()) {
		return false;
	}
	return (*lex_context->tokens)[token_index + peek_index].word == word;	
}

bool
Parse_Context::is_identifier() const {
	return token->type == TOKEN_IDENTIFIER;
}

bool
Parse_Context::is_operator() const {
	return token->type == TOKEN_OPERATOR;
}

void
Parse_Context::register_type(Datatype_Information* dt) {
	if (get_type(dt->type_name)) {
		report_error("a type with the name '" + dt->type_name + "' already exists");
	}
	defined_types.push_back(dt);
}

void
Parse_Context::register_procedure(Ast_Procedure* proc) {
	auto info = proc->info;
	if (get_procedure(info->type_name, info->get_signature())) {
		std::stringstream message;
		message << "reimplementation of procedure '";
		message << info->type_name;
		message << "' - a infoedure with that name and signature already exists.";
		report_error(message.str());
	}
	defined_procedures.push_back(proc);
}

Ast_Procedure*
Parse_Context::get_procedure(const std::string& name, const std::string& signature) const {
	Ast_Procedure* found = nullptr;
	for (auto proc: defined_procedures) {
		// ... signatures must match
		if (proc->info->type_name == name && proc->info->get_signature() == signature) {
			found = proc;
			break;
		}
	}
	return found;
}

std::vector<Ast_Procedure *>
Parse_Context::get_all_procedures(const std::string& name) const {
	std::vector<Ast_Procedure *> found;
	for (auto proc: defined_procedures) {
		if (proc->info->type_name == name) {
			found.push_back(proc);
		}
	}
	return found;
}

Datatype_Information*
Parse_Context::get_type(const std::string& type_name) const {
	for (Datatype_Information* dt: defined_types) {
		if (dt->type_name == type_name) {
			return dt;
		}
	}
	return nullptr;
}

Variable_Declaration*
Parse_Context::get_local(const std::string& identifier) const {
	Ast_Block* block_check = current_block;
	while (true) {
		if (!block_check) {
			break;
		}
		for (auto node: block_check->children) {
			if (auto check = dynamic_cast<Ast_Declaration *>(node)) {
				if (check->decl->identifier == identifier) {
					return check->decl;
				}
			}
		}
		Ast_Node* scan_up = block_check->parent;
		while (scan_up) {
			if (scan_up->type == NODE_BLOCK) {
				block_check = static_cast<Ast_Block *>(scan_up);
				break;
			}
			scan_up = scan_up->parent;
		}
		if (!scan_up) {
			break;
		}
	}
	if (current_procedure) {
		for (auto arg: current_procedure->info->args) {
			if (arg->identifier == identifier) {
				return arg;
			}
		}
	}
	
	return nullptr;
}

bool
Parse_Context::matches_struct_declaration() const {
	return is_identifier() && on(1, "::") && on(2, "struct");
}

bool
Parse_Context::matches_procedure_declaration() const {
	return is_identifier() && on(1, "::") && on(2, "(");
}

bool
Parse_Context::matches_datatype() const {
	return on("^") || on("[") || on("(") || get_type(token->word);
}

bool
Parse_Context::matches_variable_declaration() const {
	return is_identifier() && on(1, ":");	
}

bool
Parse_Context::matches_inferred_variable_declaration() const {
	return is_identifier() && on(1, ":=");
}

void
Parse_Context::handle_struct_declaration() {
	// starts on identifier of struct
	// syntax:
	//   struct_name :: struct { ... members ... };
	Struct_Information* info = new Struct_Information;	
	info->type_name = eat_and_get();
	// no need to eat these, but might as well be explicit
	eat("::");
	eat("struct");
	if (on(";")) {
		// just a forward declaration... it isn't complete yet
		info->is_complete = false;
		register_type(info);
	} else if (on("{")) {
		info->is_complete = true;
		Datatype_Information* existing = get_type(info->type_name);
		Struct_Information* existing_str = dynamic_cast<Struct_Information *>(existing);
		if (existing) {
			if (!existing_str) {
				report_error("type '" + info->type_name + "' is not a struct");
			}
			if (existing_str->is_complete) {
				report_error("attempt to re-implement struct '" + info->type_name + "'");
			}
			// no need for the object we created anymore... just repoint to the declared one
			delete info;
			info = existing_str;
			info->is_complete = true;
		} else {
			register_type(info);
		}
		
		eat("{");
		// ... now read the fields
		while (matches_variable_declaration()) {
			Struct_Field* field = parse_struct_field(info);
			eat(";", "expected ';' to follow field declaration");
			info->fields.push_back(field);
		}
		eat("}", "expected token '}' to close struct declaration");
		eat(";", "expected token ';' to follow struct declaration");
	} else {
		report_error("invalid token following token 'struct'");
	}
}

void
Parse_Context::handle_procedure_declaration() {
	auto node = new Ast_Procedure;
	node->declared_line = token->line;
	auto type_name = eat_and_get();
	eat("::");
	auto info = parse_procedure_descriptor();
	info->type_name = type_name;
	node->info = info;
	if (on("{")) {

		// register now so the right line is reported
		register_procedure(node);
		current_procedure = node;

		//eat("{");
		info->is_implemented = true;			

		// if a procedure is being implemented, all arguments must
		// have an identifier... if not, die
		for (int i = 0; i < info->args.size(); i++) {
			if (!info->args[i]->has_name) {
				std::stringstream msg;
				msg << "when a procedure is being implemented, all arguments must be named (arg #";
				msg << (i + 1);
				msg << " is nameless)";
				report_error(msg.str());
			}
		}

	} else {
		eat(";");
	}

	append_node(node);

}

void
Parse_Context::handle_variable_declaration() {
	auto id_token = token;
	auto node = new Ast_Declaration;
	node->decl = parse_variable_declaration();
	append_node(node);
}

void
Parse_Context::handle_inferred_variable_declaration() {
	auto id_token = token;
	auto node = new Ast_Declaration;
	auto decl = new Variable_Declaration;
	const auto& identifier = token->word;

	eat();
	eat(":=");
	mark(";");
	
	// we need to check here ahead of time so the
	// typechecker doesn't throw some weird errors
	if (get_local(identifier)) {
		std::stringstream message;
		message << "redeclaration of variable '";
		message << identifier;
		message << "' (perhaps you meant to use '=' instead of ':=')";
		report_error_at_indent(message.str(), id_token->col);
	}

	// patch together an assignment expression 
	auto rhs = parse_expression_and_typecheck();
	auto lhs = new Expression_Identifier;
	auto assign = new Expression_Binary;
	assign->value = ASSIGN;
	lhs->parent = assign;
	lhs->value = identifier;
	rhs->parent = assign;
	assign->left = lhs;
	assign->right = rhs;
	assign->desc = &operator_table[1]; // '=' operator descriptor
	assign->token = nullptr; // this is kinda nasty but the typecheck will never throw
							 // an error because the assignment is always safe

	// !!! note we don't typecheck the assignment yet because
	// the declaration node hasn't yet been created
	
	// now that we have the expression, create a declaration
	// and append it.... the datatype is rhs->eval (however,
	// assign->eval would work as well)
	auto decl_node = new Ast_Declaration;
	decl_node->decl = new Variable_Declaration;
	decl_node->decl->identifier = identifier;
	decl_node->decl->identifier_token = id_token;
	append_node(decl_node);

	// made the declaration node, we are free to typecheck
	auto inference = rhs->typecheck(this);
	decl_node->decl->dt = rhs->eval;
	lhs->eval = inference;
	assign->eval = inference;

	// now create a statement node and append that
	auto statement = new Ast_Statement;
	statement->expression = assign;
	append_node(statement);
	
}

void
Parse_Context::handle_standalone_statement() {
	if (on(";")) {
		eat();
		return;
	}
	mark(";");
	
	auto statement = new Ast_Statement;
	statement->expression = parse_expression_and_typecheck();
	append_node(statement);
}

void
Parse_Context::handle_if() {
	auto node = new Ast_If;
	auto err_tok = token;
	eat("if");
	eat("(", "expected token '(' to follow token 'if'");
	mark("(", ")");
	node->condition = parse_expression_and_typecheck();
	eat(")");
	if (!node->condition->eval->matches(*type_bool)) {
		std::stringstream message;
		message << "if-statement condition must evaluate to type 'bool' (got type '";
		message << node->condition->eval->to_string();
		message << "')";
		report_error_at_indent(message.str(), err_tok->col);
	}
	append_node(node);
}

void
Parse_Context::handle_while() {
	auto node = new Ast_While;
	auto err_tok = token;
	eat("while");
	eat("(", "expected token '(' to follow token 'while'");
	mark("(", ")");
	node->condition = parse_expression_and_typecheck();
	eat(")");
	if (!node->condition->eval->matches(*type_bool)) {
		std::stringstream message;
		message << "while-loop condition must evaluate to type 'bool' (got type '";
		message << node->condition->eval->to_string();
		message << "')";
		report_error_at_indent(message.str(), err_tok->col);
	}
	append_node(node);
}

void
Parse_Context::handle_for() {
	auto node = new Ast_For;
	auto err_tok = token;
	eat("for");
	eat("(", "expected token '(' to follow 'for'");
	mark(";");
	node->initializer = parse_expression_and_typecheck();
	eat(";");
	mark(";");
	node->condition = parse_expression_and_typecheck();
	eat(";");
	if (!node->condition->eval->matches(*type_bool)) {
		std::stringstream message;
		message << "for-loop condition must evaluate to type 'bool' (got type '";
		message << node->condition->eval->to_string();
		message << "')";
		report_error_at_indent(message.str(), err_tok->col);
	}
	mark("(", ")");
	node->statement = parse_expression_and_typecheck();
	eat(")");
	append_node(node);
}

void
Parse_Context::handle_block() {
	Ast_Block* block = new Ast_Block;
	eat("{");
	current_block = block;
	append_node(block);
}

void
Parse_Context::handle_jump_out() {
	Token* err_tok = token;
	eat("}");
	if (!current_block->parent) {
		report_error_at_indent("token '}' doesn't close anything", err_tok->col);
	}
	Ast_Node* scan_up = current_block;
	while (scan_up) {
		scan_up = scan_up->parent;
		if (scan_up->type == NODE_BLOCK) {
			current_block = static_cast<Ast_Block *>(scan_up);
			break;
		}
	}
	if (!scan_up) {
		assert(false);
	}
}

void
Parse_Context::append_node(Ast_Node* node) {

	static Ast_Node* append_target = nullptr;

	if (node->type == NODE_DECLARATION) {
		auto decl = static_cast<Ast_Declaration *>(node)->decl;
		if (get_local(decl->identifier)) {
			std::stringstream message;
			message << "redeclaration of variable '";
			message << decl->identifier;
			message << "'";
			report_error(message.str()); 
		}
	}
	
	if (append_target) {

		if (node->type == NODE_DECLARATION) {
			report_error_at_indent("a declaration can only exist in the global scope or inside of a block",
			                       static_cast<Ast_Declaration *>(node)->decl->identifier_token->col);
		}

		switch (append_target->type) {
			case NODE_IF:
				static_cast<Ast_If *>(append_target)->child = node;
				break;
			case NODE_WHILE:
				static_cast<Ast_While *>(append_target)->child = node;
				break;
			case NODE_FOR:
				static_cast<Ast_For *>(append_target)->child = node;
				break;
			case NODE_PROCEDURE_IMPLEMENTATION:
				static_cast<Ast_Procedure *>(append_target)->child = node;
				break;
			default:
				break;
		}
		node->parent = append_target;
	} else {
		current_block->children.push_back(node);
		node->parent = current_block;
	}

	switch (node->type) {
		case NODE_IF:
		case NODE_FOR:
		case NODE_WHILE:
		case NODE_PROCEDURE_IMPLEMENTATION:
			append_target = node;
			break;
		default:
			append_target = nullptr;
			break;
	}
}

void
Parse_Context::mark(const std::string& inc, const std::string& dec) {
	int save = token_index;
	int counter = 1;
	while (true) {
		if (on(inc)) {
			counter++;
		} else if (on(dec)) {
			counter--;
		}
		if (counter == 0) {
			marked_index = token_index;
			focus_token(save);
			break;
		}
		eat();
	}
}

void
Parse_Context::mark(const std::string& simple) {
	mark("", simple);
}

Expression*
Parse_Context::parse_expression() {

	std::vector<Token *> raw;
	std::vector<Expression_Operator *> operators;
	std::vector<Expression *> postfix;

	auto get_operator_descriptor = [&](const std::string& op) -> const Operator_Descriptor* {
		for (const auto& desc: operator_table) {
			if (desc.op_string == op) {
				return &desc;
			}
		}
		return nullptr;
	};

	auto shunting_pops = [&](const Operator_Descriptor* desc) {
		Expression_Operator* back;
		while (true) {
			if (operators.size() == 0) {
				break;
			}
			back = operators.back();
			if (back->is_unary_type(OPEN_PARENTHESIS)) {
				break;
			}
			if (desc->assoc == ASSOC_LEFT) {
				if (desc->prec > back->desc->prec) break;
			} else {
				if (desc->prec >= back->desc->prec) break;
			}
			postfix.push_back(back);
			operators.pop_back();
		}
	};

	if (token_index == marked_index) {
		return nullptr;
	}

	// gather expression into raw vector
	for (int i = token_index; i <= marked_index; i++) {
		raw.push_back(get_token(i));
	} 

	// expects end of expression to be pointer to by 'marked'
	while (token_index != marked_index) {
		switch (token->type) {
			case TOKEN_INTEGER: {
				auto push = new Expression_Integer_Literal;
				push->value = token->i;
				push->token = token;
				postfix.push_back(push);
				break;
			}
			case TOKEN_FLOAT: {
				auto push = new Expression_Float_Literal;
				push->token = token;
				push->value = token->f;
				postfix.push_back(push);
				break;
			}
			case TOKEN_STRING: {
				auto push = new Expression_String_Literal;
				push->token = token;
				push->value = token->word;
				postfix.push_back(push);
				break;
			}
			case TOKEN_IDENTIFIER: {
				auto push = new Expression_Identifier;
				push->token = token;
				push->value = token->word;
				postfix.push_back(push);
				break;			
			}
			case TOKEN_OPERATOR: {
				if (token->word == "(") {
					auto push = new Expression_Unary;
					push->token = token;
					push->value = OPEN_PARENTHESIS;
					operators.push_back(push);
				} else if (token->word == ")") {
					Expression* back;
					while (true) {
						if (operators.size() == 0) {
							report_error("unexpected closing parenthesis");
						}
						back = operators.back();
						if (back->is_unary_type(OPEN_PARENTHESIS)) {
							break;	
						}
						postfix.push_back(back);
						operators.pop_back();
					}
					operators.pop_back();
				} else if (token->word == "#") {
					eat();
					if (!matches_datatype()) {
						report_error("expected datatype to follow cast operator '#'");
					}
					auto push = new Expression_Cast;
					push->token = token;
					push->value = parse_datatype();
					push->desc = get_operator_descriptor("__CAST__"); 
					focus_token(--token_index); // go back one token, end on datatype
					operators.push_back(push);
				} else {
					auto desc = get_operator_descriptor(token->word);
					Expression_Operator* push;
					if (!desc) {
						std::stringstream err("unknown operator '");
						err << token->word;
						err << "'";
						report_error(err.str());
					}
					if (desc->type == OP_BINARY) {
						auto op = new Expression_Binary();
						op->token = token;
						op->value = Expression_Binary::word_to_type(token->word);
						op->tok = token;
						push = op;
					} else if (desc->type == OP_UNARY) {
						auto op = new Expression_Unary();
						op->token = token;
						op->value = Expression_Unary::word_to_type(token->word);
						op->tok = token;
						push = op;
					}
					push->desc = desc;
					push->word = token->word;
					shunting_pops(desc);
					operators.push_back(push);
				}
				break;
			}
		}
		eat();
	}

	while (operators.size() > 0) {
		Expression_Operator* back = operators.back();
		if (back->is_unary_type(OPEN_PARENTHESIS)) {
			report_error("mismatched parentheses");
		}
		postfix.push_back(back);
		operators.pop_back(); 
	}
	
	// now postfix contains the expression in RPN.... convert to a tree now
	
	std::vector<Expression *> tree;

	auto safe_pop = [this, &tree]() -> Expression* {
		if (tree.size() == 0) {
			report_error("malformed expression");
		}
		auto pop = tree.back();
		tree.pop_back();
		return pop;
	};

	for (auto e: postfix) {
		switch (e->type) {
			case EXPRESSION_INTEGER_LITERAL:
			case EXPRESSION_FLOAT_LITERAL:
			case EXPRESSION_STRING_LITERAL:
			case EXPRESSION_IDENTIFIER:
			case EXPRESSION_DATATYPE:
				tree.push_back(e);
				break;
			case EXPRESSION_OPERATOR_BINARY: {
				Expression_Binary* bin = static_cast<Expression_Binary *>(e);
				Expression* pops[2];
				for (int i = 0; i < 2; i++) {
					pops[i] = safe_pop();
					pops[i]->parent = bin;
				}
				pops[0]->side = Expression::LEAF_RIGHT;
				pops[1]->side = Expression::LEAF_LEFT;
				bin->right = pops[0];
				bin->left = pops[1];
				tree.push_back(bin);
				break;
			}
			case EXPRESSION_OPERATOR_UNARY: {
				Expression_Unary* un = static_cast<Expression_Unary *>(e);
				un->operand = safe_pop();
				un->operand->parent = un;
				tree.push_back(un);
				break;
			}
			case EXPRESSION_CAST: {
				Expression_Cast* cast = static_cast<Expression_Cast *>(e);
				cast->operand = safe_pop();
				cast->operand->parent = cast;
				tree.push_back(cast);
				break;
			}
		}
	}

	if (tree.size() != 1) {
		report_error("an expression must have only once result");
	}

	return tree.back();

}

Expression*
Parse_Context::parse_expression_and_typecheck() {
	auto exp = parse_expression();
	if (exp) {
		exp->typecheck(this);
	}
	return exp;
}

Procedure_Information*
Parse_Context::parse_procedure_descriptor() {
	// expects to be on the '(' token
	auto info = new Procedure_Information;	
	
	eat("(");

	while (true) {
		if (matches_datatype()) {
			auto unnamed = new Variable_Declaration;
			unnamed->dt = parse_datatype();
			unnamed->has_name = false;
			info->args.push_back(unnamed);
		} else if (matches_variable_declaration()) {
			info->args.push_back(parse_variable_declaration());
		} else {
			break;
		}
		if (on(",")) {
			eat(",");
		}
	}	
	eat(")", "expected token ')' to close procedure argument list");
	eat("->", "expected token '->' following procedure declaration to specify return type");
	info->ret = parse_datatype();

	return info;	
}

Datatype_Information*
Parse_Context::parse_datatype() {

	int ptr_dim = 0;
	int arr_dim = 0;
	
	while (on("^")) {
		ptr_dim++;
		eat();
	}	

	while (on("[") && on(1, "]")) {
		arr_dim++;
		eat();
		eat();
	}

	Datatype_Information* templ;
	
	if (on("(")) {
		
		// TODO implement non-declarative parse?
		auto proc = parse_procedure_descriptor();
		proc->type_name = "__function_pointer__";
		return proc;

	} else {	
		templ = get_type(token->word);
		if (!templ) {
			report_error("invalid type name '" + token->word + "'");
		}
		eat();
		return templ; // TODO is this dangerous??????
	}

	return nullptr;

}

Variable_Declaration*
Parse_Context::parse_variable_declaration() {
	
	Variable_Declaration* decl = new Variable_Declaration;
	decl->identifier_token = token;
	decl->identifier = eat_and_get();
	eat(":");
	decl->dt = parse_datatype();

	return decl;

}

Struct_Field*
Parse_Context::parse_struct_field(Struct_Information* parent_struct) {

	Struct_Field* field = new Struct_Field;
	field->decl = parse_variable_declaration();
	field->parent_struct = parent_struct;

	return field;

}

void
Parse_Context::init_types() {
	Integer_Information* t_int = new Integer_Information;
	t_int->type_name = "int";
	t_int->size = 8;
	type_int = t_int;
	register_type(t_int);

	Float_Information* t_float = new Float_Information;
	t_float->type_name = "float";
	t_float->size = 8;
	type_float = t_float;
	register_type(t_float);

	Void_Information* t_void = new Void_Information;
	t_void->type_name = "void";
	t_void->size = 0;
	type_void = t_void;
	register_type(t_void);

	Bool_Information* t_bool = new Bool_Information;
	t_bool->type_name = "bool";
	t_bool->size = 8;
	type_bool = t_bool;
	register_type(t_bool);

}

Parse_Context*
Parser::generate_tree(Lex_Context* lex_context) {
	
	Parse_Context* parser = new Parse_Context;
	parser->lex_context = lex_context;
	parser->token = &lex_context->tokens->front();
	parser->token_index = 0;
	parser->init_types();
	parser->root_node = new Ast_Block;
	parser->focus = parser->root_node;
	parser->current_block = parser->root_node;

	while (parser->token) {
		if (parser->matches_struct_declaration()) {
			parser->handle_struct_declaration();
		} else if (parser->matches_procedure_declaration()) {
			parser->handle_procedure_declaration();
		} else if (parser->matches_variable_declaration()) {
			parser->handle_variable_declaration();
		} else if (parser->matches_inferred_variable_declaration()) {
			parser->handle_inferred_variable_declaration();
		} else if (parser->on("if")) {
			parser->handle_if();
		} else if (parser->on("while")) {
			parser->handle_while();
		} else if (parser->on("for")) {
			parser->handle_for();
		} else if (parser->on("{")) {
			parser->handle_block();
		} else if (parser->on("}")) {
			parser->handle_jump_out();
		} else {
			parser->handle_standalone_statement();
		}
	}

	parser->root_node->print();

	return parser;

}
