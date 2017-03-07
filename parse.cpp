#include <sstream>
#include <iostream>
#include "parse.h"

using namespace Parser;
using namespace Lexer;

// DATATYPE IMPLEMENTATION 
Datatype_Information_Base::Datatype_Information_Base(const Datatype_Information_Base& to_copy) {
	ptr_dim = to_copy.ptr_dim;
	arr_dim = to_copy.arr_dim;
	size = to_copy.size;
	type_name = to_copy.type_name;
}

Datatype_Information_Base*
Datatype_Information_Base::clone() const {
	if (const Struct_Information* a = dynamic_cast<const Struct_Information *>(this)) {
		return new Struct_Information(*a);
	}
	return nullptr; // should never be reached
}

std::string
Datatype_Information_Base::to_string() const {
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
	buf << Datatype_Information_Base::to_string();
	buf << ": (";
	size_t nargs = args.size();
	for (int i = 0; i < nargs; i++) {
		buf << args[i]->to_string();
		if (i < nargs - 1) {
			buf << ", ";
		}
	}
	buf << ") -> ";
	buf << ret->to_string();
	return buf.str();
}

Struct_Information::Struct_Information(const Struct_Information& to_copy): Datatype_Information_Base(to_copy) {
	for (const auto field: to_copy.fields) {
		fields.push_back(new Struct_Field(*field));
	}
	is_complete = to_copy.is_complete;
}

Procedure_Information::Procedure_Information(const Procedure_Information& to_copy): Datatype_Information_Base(to_copy) {
	for (const auto arg: to_copy.args) {
		args.push_back(new Variable_Declaration(*arg));
	}
	is_implemented = to_copy.is_implemented;
}

std::string 
Procedure_Information::make_signature(const std::vector<Variable_Declaration*>& call_info) {
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

Integer_Information::Integer_Information(const Integer_Information& to_copy): Datatype_Information_Base(to_copy) {

}

Void_Information::Void_Information(const Void_Information& to_copy): Datatype_Information_Base(to_copy) {

}

// VARIABLE DECLARATION IMPLEMENTATION
Variable_Declaration::Variable_Declaration(const Variable_Declaration& to_copy) {
	identifier = to_copy.identifier;
	dt = new Datatype_Information_Base(*to_copy.dt);
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

// PARSE CONTEXT IMPLEMENTATION

void
Parse_Context::report_error(const std::string& message) const {
	int line = 0;
	int col = 0;
	std::string* word = nullptr;
	if (token) {
		line = token->line;
		col = token->col;
		word = &token->word;
	}
	std::cerr << "\n-------- SPYRE PARSE ERROR -------\n\n";
	std::cerr << "line:    " << line << std::endl;
	/*
	std::cerr << "col:     " << col << std::endl;
	if (word) {
		std::cerr << "token:   '" << *word << "'" << std::endl;
	}
	*/
	std::cerr << "message: " << message << std::endl;
	std::cerr << std::endl << std::endl;
	std::exit(1);
}

void
Parse_Context::assert_token() const {
	if (!token) {
		report_error("unexpected end of file");
	}
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
Parse_Context::register_type(Datatype_Information_Base* dt) {
	if (get_type(dt->type_name)) {
		report_error("a type with the name '" + dt->type_name + "' already exists");
	}
	defined_types.push_back(dt);
}

void
Parse_Context::register_procedure(Procedure_Information* proc) {	
	if (get_procedure(proc->type_name, proc->get_signature())) {
		report_error("a procedure with that signature already exists");
	}
	defined_procedures.push_back(proc);
}

Procedure_Information*
Parse_Context::get_procedure(const std::string& name) {
	Procedure_Information* found = nullptr;
	for (Procedure_Information* proc: defined_procedures) {
		if (proc->type_name == name) {
			if (found) {
				report_error("can't decide procedure...");
			}
			found = proc;
		}
	}
	return found;
}

Procedure_Information*
Parse_Context::get_procedure(const std::string& name, const std::string& signature) {
	Procedure_Information* found = nullptr;
	for (Procedure_Information* proc: defined_procedures) {
		// ... signatures must match
		if (proc->type_name == name && proc->get_signature() == signature) {
			found = proc;
			break;
		}
	}
	return found;
}

Datatype_Information_Base*
Parse_Context::get_type(const std::string& type_name) const {
	for (Datatype_Information_Base* dt: defined_types) {
		if (dt->type_name == type_name) {
			return dt;
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
	return on("^") || on("[") || get_type(token->word);
}

bool
Parse_Context::matches_variable_declaration() const {
	return is_identifier() && on(1, ":");	
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
		Datatype_Information_Base* existing = get_type(info->type_name);
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
	Procedure_Information* info = new Procedure_Information;
	info->type_name = eat_and_get();
	eat("::");
	eat("(");
	while (true) {
		if (matches_datatype()) {
			Variable_Declaration* unnamed = new Variable_Declaration;
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
	if (on("{")) {

		// register now so the right line is reported
		register_procedure(info);

		eat("{");
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
		
		// TODO register the procedure as a local here
	}
}

Datatype_Information_Base*
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

	Datatype_Information_Base* templ;
	
	if (on("(")) {
		
		// TODO implement non-declarative parse?

	} else {	
		templ = get_type(token->word);
		if (!templ) {
			report_error("invalid type name '" + token->word + "'");
		}
		Datatype_Information_Base* ret = new Datatype_Information_Base(*templ); 
		ret->ptr_dim = ptr_dim;
		ret->arr_dim = arr_dim;
		eat();
		return ret;
	}

	return nullptr;

}

Variable_Declaration*
Parse_Context::parse_variable_declaration() {
	
	Variable_Declaration* decl = new Variable_Declaration;
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
	Integer_Information* type_int = new Integer_Information;
	type_int->type_name = "int";
	type_int->size = 8;

	Void_Information* type_void = new Void_Information;
	type_void->type_name = "void";
	type_void->size = 0;

	register_type(type_int);
	register_type(type_void);
}

Parse_Context*
Parser::generate_tree(Lex_Context* lex_context) {
	
	Parse_Context* parser = new Parse_Context;
	parser->lex_context = lex_context;
	parser->token = &lex_context->tokens->front();
	parser->token_index = 0;
	parser->init_types();
	
	while (parser->token) {
		if (parser->matches_struct_declaration()) {
			parser->handle_struct_declaration();
		} else if (parser->matches_procedure_declaration()) {
			parser->handle_procedure_declaration();
		} else {
			parser->eat();
		}
	}

	Datatype_Information_Base* dt = new Struct_Information;
	dt->clone();

	return parser;

}
