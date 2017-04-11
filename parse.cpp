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
	{".",   FIND_MEMBER},
	{",",   COMMA}
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
	Operator_Descriptor("__CAST__", 10, ASSOC_RIGHT, OP_UNARY, {}), // special case...
	Operator_Descriptor("@",        10, ASSOC_RIGHT, OP_UNARY, {ENFORCE_L_VALUE}),
	Operator_Descriptor("$",        10, ASSOC_RIGHT, OP_UNARY, {DISALLOW_NON_POINTER}),
	Operator_Descriptor("!",        10, ASSOC_RIGHT, OP_UNARY, {ENFORCE_BOOL}),
	Operator_Descriptor("new",      10, ASSOC_RIGHT, OP_UNARY, {ENFORCE_DATATYPE}),
	Operator_Descriptor(".",        11, ASSOC_LEFT, OP_BINARY, {}), // special case...
	Operator_Descriptor("__CALL__", 11, ASSOC_LEFT, OP_UNARY, {}), // special case...
	Operator_Descriptor("__INDX__", 11, ASSOC_LEFT, OP_UNARY, {}) // special case...
};

void
Datatype_Information::fill_fields(const Datatype_Information& dt) {
	ptr_dim = dt.ptr_dim;
	arr_dim = dt.arr_dim;
	arr_size = dt.arr_size;
	type_name = dt.type_name; 
}

bool
Datatype_Information::matches(const Datatype_Information& other) const {
	if (type_name != other.type_name) {
		std::cout << type_name << " " << other.type_name << std::endl;
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
Procedure_Information::make_signature(const std::vector<Datatype_Information *>& call_info) {
	std::stringstream sig;
	for (const auto arg: call_info) {
		sig << arg->to_string();	
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

Procedure_Information*
Procedure_Information::clone() const {
	auto copy = new Procedure_Information;
	copy->fill_fields(*this);
	for (auto arg: args) {
		copy->args.push_back(new Variable_Declaration(*arg));	
	}
	copy->ret = ret->clone();
	return copy;
}

Struct_Information*
Struct_Information::clone() const {
	auto copy = new Struct_Information;
	copy->fill_fields(*this);
	for (auto field: fields) {
		auto new_field = new Struct_Field;
		new_field->decl = new Variable_Declaration(*field->decl);
		new_field->parent_struct = copy;
		copy->fields.push_back(new_field);
	}
	copy->is_complete = true;
	return copy;
}

Integer_Information*
Integer_Information::clone() const {
	auto copy = new Integer_Information;
	copy->fill_fields(*this);
	return copy;
}

Float_Information*
Float_Information::clone() const {
	auto copy = new Float_Information;
	copy->fill_fields(*this);
	return copy;
}

Void_Information*
Void_Information::clone() const {
	auto copy = new Void_Information;
	copy->fill_fields(*this);
	return copy;
}

Bool_Information*
Bool_Information::clone() const {
	auto copy = new Bool_Information;
	copy->fill_fields(*this);
	return copy;
}

Byte_Information*
Byte_Information::clone() const {
	auto copy = new Byte_Information;
	copy->fill_fields(*this);
	return copy;
}

// VARIABLE DECLARATION IMPLEMENTATION
Variable_Declaration::Variable_Declaration(const Variable_Declaration& to_copy) {
	identifier = to_copy.identifier;
	dt = to_copy.dt->clone();
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

std::string
Expression_Call::to_string() const {
	return "CALL";
}

std::string
Expression_Array_Index::to_string() const {
	return "ARRAY_INDEX";
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

void
Expression_Call::print(int indent = 0) const {
	Expression::print(indent);
	proc->print(indent + 1);
	if (argument) {
		argument->print(indent + 1);
	}
}

void
Expression_Array_Index::print(int indent = 0) const {
	Expression::print(indent);
	array->print(indent + 1);
	index->print(indent + 1);
}

bool
Expression_Binary::is_assign() const {
	return (
		value == ADDITION_BY ||
		value == SUBTRACTION_BY ||
		value == MULTIPLICATION_BY ||
		value == DIVISION_BY ||
		value == MODULUS_BY ||
		value == BITWISE_AND_BY ||
		value == BITWISE_OR_BY ||
		value == BITWISE_XOR_BY ||
		value == SHIFT_LEFT_BY ||
		value == SHIFT_RIGHT_BY
	);
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
	
	// arrays are not assignable
	if (is_assign() && left_eval->arr_dim > 0) {
		context->report_error_at_indent("arrays are not assignable", tok->col);
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
		context->report_error_at_indent(message.str(), operand->token->col);
	}
	
	switch (value) {
		case DEREFERENCE:
			is_l_value = true;
			break;
		default:
			is_l_value = false;
			break;
	}

	switch (value) {
		case DEREFERENCE: {
			if (!operand_eval->is_pointer()) {
				std::stringstream message;
				message << "attempt to dereference a non-pointer (got type '";
				message << operand_eval->to_string();
				message << "')";
				context->report_error_at_indent(message.str(), tok->col);
			}
			auto copy = operand_eval->clone();
			copy->ptr_dim -= 1;
			return eval = copy;
		}
		case ADDRESS_OF: {
			if (!operand->is_l_value) {
				context->report_error_at_indent("attempt to take the address of an R-value", tok->col);
			}
			auto copy = operand_eval->clone();
			copy->ptr_dim += 1;
			return eval = copy;
		}	
		default:
			return eval = operand_eval;		
	}

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

	auto illegal_cast = [&]() {
		std::stringstream message;
		message << "illegal cast from type '";
		message << op_type->to_string();
		message << "' to type '";
		message << value->to_string();
		message << "'";
		context->report_error_at_indent(message.str(), token->col);
	};

	// ensure that it is a valid cast
	if (value->is_int() || value->is_bool()) {
		// if we're casting to an int/bool, the only valid operand type
		// is a float, byte, bool, or pointer... (e.g. structs are not allowed,
		// as that does not make any sense)
		if (!op_type->is_float() && !op_type->is_byte() && !op_type->is_bool() && !op_type->is_int() && !op_type->is_pointer()) {
			illegal_cast();
		}
	}

	if (!op_type->is_int() && !op_type->is_pointer() && value->is_pointer()) {
		illegal_cast();
	}

	if (value->is_array() || op_type->is_pointer()) {
		illegal_cast();
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
	return eval = context->type_string;
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

		Expression* illegal_operator = nullptr;

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
				} else {
					illegal_operator = parent;
				}
				break;
			}
			case EXPRESSION_CALL: {
				auto call = static_cast<Expression_Call *>(parent);

				// if the parent is a call, we know that the call's 'call_sig' field
				// has already been determined.  we can use that field to find
				// a matching procedure
				auto ast_proc = context->get_procedure(value, call->call_sig);
				if (ast_proc) {
					proc_context = ast_proc->info;
				} else {
					// if a procedure with that name exists but we didn't find
					// a matching signature, then we are calling an invalid overload
					if (context->get_all_procedures(value).size() > 0) {
						// make a dummy to report the error
						Procedure_Information dummy;
						for (auto arg_d: call->arg_types) {
							auto decl = new Variable_Declaration;
							decl->dt = arg_d;
							decl->has_name = false;
							dummy.args.push_back(decl);
						}
						dummy.ret = context->type_void;
						no_matching_overload(&dummy);
					}
				}
				break;
			}
			default:
				illegal_operator = parent;
				break;
		}

		if (illegal_operator) {
			std::stringstream message;
			message << "illegal operator '";
			message << illegal_operator->to_string();
			message << "' being used on a procedure (did you forget to call the procedure?)";
			context->report_error_at_indent(message.str(), illegal_operator->token->col);
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

Datatype_Information*
Expression_Call::typecheck(Parse_Context* context) {
	/*
	auto proc_t = proc->typecheck(context);
	auto proc_d_t = dynamic_cast<Procedure_Information *>(proc_t);
	if (!proc_d_t) {
		context->report_error_at_indent("attempt to call a non-procedure value",
									    token->col);
	}
	*/

	std::vector<Expression *> sorted_arguments;
	
	if (argument) {
		if (argument->is_binary_type(COMMA)) {
			// find lowest comma;
			auto lowest_comma = argument;
			while (lowest_comma->is_binary_type(COMMA)) {
				lowest_comma = static_cast<Expression_Binary *>(lowest_comma)->left;
			}
			lowest_comma = lowest_comma->parent;
			auto comma_as_bin = static_cast<Expression_Binary *>(lowest_comma);
			comma_as_bin->left->typecheck(context);
			comma_as_bin->right->typecheck(context);
			sorted_arguments.push_back(comma_as_bin->left);
			sorted_arguments.push_back(comma_as_bin->right);
			while (comma_as_bin->parent && comma_as_bin->right) {
				comma_as_bin = static_cast<Expression_Binary *>(comma_as_bin->parent);
				comma_as_bin->right->typecheck(context);
				sorted_arguments.push_back(comma_as_bin->right);
			}	
		} else {
			argument->typecheck(context);
			sorted_arguments.push_back(argument);
		}
	}

	std::vector<Datatype_Information *> evals;
	for (auto e: sorted_arguments) {
		if (e->eval->is_struct()) {
			// if it's a raw struct implicitly pass by pointer
			auto copy = e->eval->clone();
			copy->ptr_dim = 1;
			evals.push_back(copy);
		} else {
			evals.push_back(e->eval);
		}
	}
	
	arg_types = evals;	
	call_sig = Procedure_Information::make_signature(evals);	
	
	proc->typecheck(context);

	auto proc_pi = dynamic_cast<Procedure_Information *>(proc->eval);
	int expected_args = proc_pi->args.size();
	int given_args = evals.size();

	if (!proc_pi) {
		std::stringstream message;
		message << "attempt to call a non-procedure value (got type '";
		message << proc->eval->to_string();
		message << "')";
		context->report_error_at_indent(message.str(), token->col);
	}

	if (expected_args != given_args) {
		std::stringstream message;
		message << "procedure expects ";
		message << expected_args;
		message << " arguments, got ";
		message << given_args;
		message << ".\nnote, signature of procedure is: '";
		message << proc_pi->to_string();
		message << "'";
		context->report_error_at_indent(message.str(), token->col);
	}

	for (int i = 0; i < expected_args; i++) {
		if (!evals[i]->matches(*proc_pi->args[i]->dt)) {
			std::stringstream message;
			message << "type mismatch in procedure call.  argument #";
			message << i + 1;
			message << " should be of type '";
			message << proc_pi->args[i]->dt->to_string();
			message << "' (got type '";
			message << evals[i]->to_string();
			message << "')\nnote, signature of procedure is: '";
			message << proc_pi->to_string();
			message << "'";
			context->report_error_at_indent(message.str(), sorted_arguments[i]->token->col);
		}
	}

	return eval = proc_pi->ret;
}

Datatype_Information*
Expression_Array_Index::typecheck(Parse_Context* context) {
		
	auto array_type = array->typecheck(context);
	auto index_type = index->typecheck(context);

	if (array_type->arr_dim < 1) {
		std::stringstream message;
		message << "attempt to index a non-array type (got type '";
		message << array_type->to_string();
		message << "')";
		context->report_error_at_indent(message.str(), array->token->col);
	}

	if (!index_type->is_int()) {
		std::stringstream message;
		message << "array index must evaluate to an integer (got type '";
		message << index_type->to_string();
		message << "')";
		context->report_error_at_indent(message.str(), index->token->col);
	}

	auto deref_type = array_type->clone();
	deref_type->arr_dim--;
	return eval = deref_type;

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
Ast_Return::print(int indent = 0) const {
	make_indent(indent);
	std::cout << "RETURN: [\n";
	if (expression) {
		expression->print(indent + 1);
	}
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
		for (int i = fail_indent + 1; i < lex_context->raw_file[line - 1].length(); i++) {
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
		report_error_at_indent(message + ", got token '" + token->word + "'", token->col);	
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
	assert_token();
	return token->word == word;
}

bool
Parse_Context::on(int peek_index, const std::string& word) const {
	assert_token();
	if (token_index + peek_index >= lex_context->tokens->size()) {
		return false;
	}
	return (*lex_context->tokens)[token_index + peek_index].word == word;	
}

bool
Parse_Context::is_identifier() const {
	assert_token();
	return token->type == TOKEN_IDENTIFIER;
}

bool
Parse_Context::is_operator() const {
	assert_token();
	return token->type == TOKEN_OPERATOR;
}

void
Parse_Context::register_type(Datatype_Information* dt) {
	if (get_type(dt->type_name) && guard_register_type) {
		report_error("a type with the name '" + dt->type_name + "' already exists");
	}
	defined_types.push_back(dt);
}

void
Parse_Context::register_procedure(Ast_Procedure* proc) {
	auto info = proc->info;
	if (auto prev = get_procedure(info->type_name, info->get_signature())) {
		std::stringstream message;
		message << "reimplementation of procedure '";
		message << info->type_name;
		message << "' - a procedure with that name and signature already exists.\n";
		message << "note: the other procedure was declared on line #" << prev->declared_line;
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
	for (auto dt: defined_types) {
		if (dt->type_name == type_name) {
			return dt;
		}
	}
	return nullptr;
}

Variable_Declaration*
Parse_Context::get_local(const std::string& identifier) const {
	auto block_check = current_block;
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
		auto scan_up = block_check->parent;
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
Parse_Context::matches_double_colon_declaration() const {
	return is_identifier() && on(1, "::");
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
Parse_Context::matches_comma_identifier_chain() {
	if (!is_identifier()) {
		return false;
	}

	int start = token_index;
	eat();

	while (true) {
		if (!on(",")) {
			focus_token(start);
			return true;
		}
		eat(",");
		if (!is_identifier()) {
			return false;
		}
		eat();
	}
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

		// reset local count
		local_count = 0;

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
Parse_Context::handle_constant_declaration() {
	eat();
	auto err_tok = token;
	eat("::");
	mark(";");
	auto const_expr = parse_expression_and_typecheck();
	if (const_expr->type == EXPRESSION_INTEGER_LITERAL) {

	} else if (const_expr->type == EXPRESSION_FLOAT_LITERAL) {

	} else {
		std::stringstream message;
		message << "'::' can only be used to define constant expressions";
		report_error_at_indent(message.str(), err_tok->col);
	}
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
Parse_Context::handle_return() {
	if (!current_procedure) {
		report_error_at_indent("a return statement must exist inside the body of a procedure", token->col);
	}
	auto node = new Ast_Return;
	auto expected_type = current_procedure->info->ret;
	auto err_tok = token;
	eat("return");
	mark(";");
	node->expression = parse_expression_and_typecheck();
	if (expected_type->is_void() && node->expression) {
		report_error_at_indent("procedure with return type 'void' shouldn't return an expression", err_tok->col);
	}
	if (!node->expression->eval->matches(*expected_type)) {
		std::stringstream message;
		message << "type mismatch in return statement.  procedure must return value of type '";
		message << expected_type->to_string();
		message << "' (got type '";
		message << node->expression->eval->to_string();
		message << "')";
		report_error_at_indent(message.str(), err_tok->col);
	}
	eat(";");
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
		decl->tag = local_count++;
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

	int start = token_index;

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
					if (token_index > start) {
						auto prev = get_token(token_index - 1);
						if (prev->word == ")" || prev->type == TOKEN_IDENTIFIER) {
							bool abort = false;
							// if the previous token is a type, it could be a cast.... abort
							if (prev->type == TOKEN_IDENTIFIER && get_type(prev->word)) {
								abort = true;
							}
							if (!abort) {
								auto call = new Expression_Call;
								int save = marked_index;
								auto tok = token;
								eat("(");
								mark("(", ")");
								call->argument = parse_expression();
								call->desc = get_operator_descriptor("__CALL__");
								call->token = tok;
								marked_index = save;
								shunting_pops(call->desc);
								operators.push_back(call);
								break;
							}
						}
					}

					auto push = new Expression_Unary;
					push->token = token;
					push->value = OPEN_PARENTHESIS;
					operators.push_back(push);
				} else if (token->word == ")") {
					Expression* back;
					while (true) {
						if (operators.size() == 0) {
							report_error_at_indent("unexpected closing parenthesis", token->col);
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
						report_error_at_indent("expected datatype to follow cast operator '#'", token->col);
					}
					auto push = new Expression_Cast;
					push->token = token;
					push->value = parse_datatype();
					push->desc = get_operator_descriptor("__CAST__"); 
					focus_token(--token_index); // go back one token, end on datatype
					operators.push_back(push);
				} else if (token->word == "[") {
					auto push = new Expression_Array_Index;
					auto tok = token;
					int save = marked_index;
					eat("[");
					mark("[", "]");
					push->index = parse_expression();
					push->token = tok;
					push->desc = get_operator_descriptor("__INDX__");
					marked_index = save;
					shunting_pops(push->desc);
					operators.push_back(push);
				} else {
					auto desc = get_operator_descriptor(token->word);
					Expression_Operator* push;
					if (!desc) {
						std::stringstream err;
						err << "unknown operator '";
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
		auto back = operators.back();
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
				auto bin = static_cast<Expression_Binary *>(e);
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
				auto un = static_cast<Expression_Unary *>(e);
				un->operand = safe_pop();
				un->operand->parent = un;
				tree.push_back(un);
				break;
			}
			case EXPRESSION_CAST: {
				auto cast = static_cast<Expression_Cast *>(e);
				cast->operand = safe_pop();
				cast->operand->parent = cast;
				tree.push_back(cast);
				break;
			}
			case EXPRESSION_CALL: {
				auto call = static_cast<Expression_Call *>(e);
				call->proc = safe_pop();
				call->proc->parent = call;
				tree.push_back(call);	
				break;
			}
			case EXPRESSION_ARRAY_INDEX: {
				auto index = static_cast<Expression_Array_Index *>(e);
				index->array = safe_pop();
				index->array->parent = index;
				tree.push_back(index);
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
Parse_Context::fold_expression(Expression* e) {
	
	if (!e) {
		return nullptr;
	}

	Expression* replacement = nullptr;

	auto get_result_binary = [](Binary_Operator_Type t, bool* did_fold, auto left, decltype(left) right) -> decltype(left) {
		using T = decltype(left);
		T result;
		switch (t) {
			case ADDITION:
				result = left + right;
				break;
			case SUBTRACTION:
				result = left - right;
				break;
			case MULTIPLICATION:
				result = left * right;
				break;
			case DIVISION:
				result = left / right;
				break;
			case MODULUS:
				result = (int)left % (int)right;
				break;
			case BITWISE_AND:
				result = (int)left & (int)right;
				break;
			case BITWISE_OR:
				result = (int)left | (int)right;
				break;
			case SHIFT_LEFT:
				result = (int)left << (int)right;
				break;
			case SHIFT_RIGHT:
				result = (int)left >> (int)right;
				break;
			default:
				*did_fold = false;
				return result;
		}
		*did_fold = true;
		return result;
	};

	auto get_result_unary = [](Unary_Operator_Type t, bool* did_fold, auto operand) -> decltype(operand) {
		using T = decltype(operand);
		T result;
		switch (t) {
			case LOGICAL_NOT:
				result = !operand;
				break;
			case BITWISE_NOT:
				result = ~operand;
				break;
			case DEREFERENCE:
				result = *reinterpret_cast<T *>(static_cast<intptr_t>(operand));
				break;
			default:
				*did_fold = false;
				return result;
		}		
		*did_fold = true;
		return result;
	};

	switch (e->type) {
		case EXPRESSION_OPERATOR_BINARY: {
			auto bin = static_cast<Expression_Binary *>(e);
			auto left = fold_expression(bin->left);
			auto right = fold_expression(bin->right);
			
			// works for two same types	
			auto condense_binary = [&](auto left_exp, decltype(left_exp) right_exp) -> decltype(left_exp) {
				using T = typename std::remove_pointer<decltype(left_exp)>::type;
				auto left = static_cast<T *>(left_exp)->value;	
				auto right = static_cast<T *>(right_exp)->value;	
				bool did_fold = false;
				auto result = get_result_binary(bin->value, &did_fold, left, right);
				if (!did_fold) {
					return nullptr;
				}
				auto rep = new T;
				rep->value = result;
				rep->parent = e->parent;
				rep->word = std::to_string(result);
				rep->is_l_value = e->is_l_value;
				rep->side = e->side;
				return rep;
			};

			if (left->type == EXPRESSION_INTEGER_LITERAL && right->type == EXPRESSION_INTEGER_LITERAL) {
				replacement = condense_binary(static_cast<Expression_Integer_Literal *>(left),
											  static_cast<Expression_Integer_Literal *>(right));
				if (replacement) {
					replacement->eval = type_int;
				}
			} else if (left->type == EXPRESSION_FLOAT_LITERAL && right->type == EXPRESSION_FLOAT_LITERAL) {
				replacement = condense_binary(static_cast<Expression_Float_Literal *>(left),
											  static_cast<Expression_Float_Literal *>(right));
				if (replacement) {
					replacement->eval = type_float;
				}
			}
			break;
		}
		case EXPRESSION_OPERATOR_UNARY:	{
			auto un = static_cast<Expression_Unary *>(e);
			auto operand = un->operand;
			bool did_fold = false;
			if (operand->type == EXPRESSION_INTEGER_LITERAL) {
				int op = static_cast<Expression_Integer_Literal *>(operand)->value;
				int result = get_result_unary(un->value, &did_fold, op);
				if (!did_fold) {
					return e;
				}
				auto rep = new Expression_Integer_Literal;
				rep->value = result;
				rep->parent = e->parent;
				rep->word = std::to_string(result);
				rep->eval = type_float;
				rep->is_l_value = e->is_l_value;
				rep->side = e->side;
				replacement = rep;
			}
			break;
		}
		case EXPRESSION_CAST: {
			auto cast = static_cast<Expression_Cast *>(e);
			auto target = cast->value; 
			auto operand = fold_expression(cast->operand);
			if (
				(target->is_pointer() || target->is_int()) && 
				(operand->type == EXPRESSION_INTEGER_LITERAL || operand->type == EXPRESSION_FLOAT_LITERAL)
			) {
				auto rep = new Expression_Integer_Literal;
				if (operand->type == EXPRESSION_INTEGER_LITERAL) {
					int val = static_cast<Expression_Integer_Literal *>(operand)->value;
					rep->value = val;
					rep->word = std::to_string(val);
				} else {
					double val = static_cast<Expression_Float_Literal *>(operand)->value;
					rep->value = (int)val;
					rep->word = std::to_string(val);
				}
				rep->eval = type_int;
				rep->parent = e->parent;
				rep->is_l_value = e->is_l_value;
				rep->side = e->side;
				replacement = rep;
			}
			break;
		}
		default:
			break;
	}

	if (replacement) {
		if (e->parent) {
			switch (e->parent->type) {
				case EXPRESSION_OPERATOR_BINARY: {
					auto bin = static_cast<Expression_Binary *>(e->parent);
					switch (e->side) {
						case Expression::LEAF_LEFT:
							bin->left = replacement;
							break;	
						case Expression::LEAF_RIGHT:
							bin->right = replacement;
							break;	
					}
					break;
				}
				case EXPRESSION_OPERATOR_UNARY: {
					auto un = static_cast<Expression_Unary *>(e->parent);
					un->operand = replacement;
					break;
				}
				case EXPRESSION_CAST: {
					auto cast = static_cast<Expression_Cast *>(e->parent);
					cast->operand = replacement;
					break;
				}
				default:
					break;
			}
		}
		return replacement;
	}
	
	return e;
}

Expression*
Parse_Context::parse_expression_and_typecheck() {
	auto exp = parse_expression();
	if (exp) {
		exp->typecheck(this);
		exp = fold_expression(exp);
	}
	return exp;
}

Procedure_Information*
Parse_Context::parse_procedure_descriptor() {
	// expects to be on the '(' token
	auto info = new Procedure_Information;	
	
	eat("(");

	while (true) {
		Token* dt_head = token;
		bool must_be_pointer_die = false;
		if (matches_datatype()) {
			auto unnamed = new Variable_Declaration;
			unnamed->dt = parse_datatype();
			unnamed->has_name = false;
			if (unnamed->dt->is_struct()) {
				std::stringstream message;
				message << "a procedure cannot take a raw object as a parameter - it can only accept a pointer";
				report_error_at_indent(message.str(), dt_head->col);
			}
			info->args.push_back(unnamed);
		} else if (matches_variable_declaration()) {
			auto decl = parse_variable_declaration();
			if (decl->dt->is_struct()) {
				std::stringstream message;
				message << "a procedure cannot take a raw object as a parameter - it can only accept a pointer";
				report_error_at_indent(message.str(), dt_head->col);
			}
			info->args.push_back(decl);
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
	std::vector<int> arr_size;

	while (on("[")) {
		arr_dim++;
		auto err_tok = token;
		eat();
		mark("[", "]");
		auto size = parse_expression_and_typecheck();
		if (!size) {
			report_error_at_indent("expected constant array size between '[' and ']'", err_tok->col);
		}
		if (!size->eval->is_int()) {
			std::stringstream message;
			message << "array size must be a constant integer (got type '";
			message << size->eval->to_string();
			message << "')";
			report_error_at_indent(message.str(), err_tok->col);
		}
		if (size->type != EXPRESSION_INTEGER_LITERAL) {
			report_error_at_indent("array size must be a constant integer", err_tok->col); 
		}
		arr_size.push_back(static_cast<Expression_Integer_Literal *>(size)->value);
		eat("]");
	}
	
	while (on("^")) {
		ptr_dim++;
		eat();
	}	
	
	if (on("(")) {
		
		// TODO implement non-declarative parse?
		auto proc = parse_procedure_descriptor();
		proc->type_name = "__function_pointer__";
		return proc;

	} else {	
		// TODO MAKE A REAL COPY HERE!!! THIS IS CURRENTLY VERRRRYYYYY BAD!!!!
		auto templ = get_type(token->word);
		if (!templ) {
			report_error_at_indent("invalid type name '" + token->word + "'", token->col);
		}
		auto copy = templ->clone();
		copy->ptr_dim = ptr_dim;
		copy->arr_dim = arr_dim;
		copy->arr_size = std::move(arr_size);
		eat();
		return copy;
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
	auto t_int = new Integer_Information;
	t_int->type_name = "int";
	t_int->size = 8;
	type_int = t_int;
	register_type(t_int);

	auto t_float = new Float_Information;
	t_float->type_name = "float";
	t_float->size = 8;
	type_float = t_float;
	register_type(t_float);

	auto t_void = new Void_Information;
	t_void->type_name = "void";
	t_void->size = 0;
	type_void = t_void;
	register_type(t_void);

	auto t_byte = new Byte_Information;
	t_byte->type_name = "byte";
	t_byte->size = 1;
	type_byte = t_byte;
	register_type(t_byte);

	auto t_string = new Byte_Information;
	t_string->type_name = "byte";
	t_string->size = 1;
	t_string->ptr_dim = 1;
	type_string = t_string;
	register_type(t_string);

	auto t_bool = new Bool_Information;
	t_bool->type_name = "bool";
	t_bool->size = 8;
	type_bool = t_bool;
	register_type(t_bool);

	guard_register_type = false;

}

Parse_Context*
Parser::generate_tree(Lex_Context* lex_context) {
	
	auto parser = new Parse_Context;
	parser->lex_context = lex_context;
	parser->token = &lex_context->tokens->front();
	parser->token_index = 0;
	parser->init_types();
	parser->root_node = new Ast_Block;
	parser->focus = parser->root_node;
	parser->current_block = parser->root_node;
	parser->root_node->parent = nullptr;

	while (parser->token) {
		if (parser->matches_variable_declaration()) {
			parser->handle_variable_declaration();
		} else if (parser->matches_inferred_variable_declaration()) {
			parser->handle_inferred_variable_declaration();
		} else if (parser->matches_double_colon_declaration()) {
			if (parser->on(2, "struct")) {
				parser->handle_struct_declaration();
			} else if (parser->on(2, "(")) {
				parser->handle_procedure_declaration();
			} else {
				parser->handle_constant_declaration();
			}
		} else if (parser->on("if")) {
			parser->handle_if();
		} else if (parser->on("while")) {
			parser->handle_while();
		} else if (parser->on("for")) {
			parser->handle_for();
		} else if (parser->on("return")) {
			parser->handle_return();
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
