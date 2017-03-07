#ifndef PARSE_H
#define PARSE_H

#include "lex.h"

using namespace Lexer;

namespace Parser {
	
	// forward declarations
	struct Datatype_Information_Base;
	struct Struct_Information;
	
	struct Variable_Declaration {
		Variable_Declaration() {}
		Variable_Declaration(const Variable_Declaration&);
		Variable_Declaration* clone() const;
		std::string to_string() const;
		
		bool has_name = true; // only used for procedure arguments	
		std::string identifier;
		Datatype_Information_Base* dt;
	};

	struct Datatype_Information_Base {
		virtual ~Datatype_Information_Base() {};
		Datatype_Information_Base() {}
		Datatype_Information_Base(const Datatype_Information_Base&);
		Datatype_Information_Base* clone() const;
		std::string to_string() const;

		std::string type_name;
		int ptr_dim = 0;
		int arr_dim = 0;
		int size;
	};

	struct Struct_Field {
		Struct_Field() {}
		Struct_Field(const Struct_Field&);

		Variable_Declaration* decl;
		Struct_Information* parent_struct;
	};

	struct Struct_Information : public Datatype_Information_Base {
		Struct_Information(const Struct_Information&);
		Struct_Information() {}

		bool is_complete = false;
		std::vector<Struct_Field*> fields;
	};

	struct Procedure_Information : public Datatype_Information_Base {
		Procedure_Information(const Procedure_Information&);
		Procedure_Information() {}
		std::string to_string() const;
		std::string get_signature() const;
		static std::string make_signature(const std::vector<Variable_Declaration*>&);
		
		bool is_implemented = false;
		std::vector<Variable_Declaration*> args;
		Datatype_Information_Base* ret;
	};

	struct Integer_Information : public Datatype_Information_Base {
		Integer_Information(const Integer_Information&);
		Integer_Information() {}
	};

	struct Void_Information : public Datatype_Information_Base {
		Void_Information(const Void_Information&);
		Void_Information() {}
	};

	class Parse_Context {
		private:
			int token_index;
			Token* token;	
			Lex_Context* lex_context;	
			std::vector<Datatype_Information_Base*> defined_types;
			std::vector<Procedure_Information*> defined_procedures;
			
			void report_error(const std::string&) const;	
			void assert_token() const;			
			void eat(const std::string&, const std::string&);
			void eat(const std::string&);
			void eat();
			std::string eat_and_get();
			bool on(const std::string&) const;
			bool on(int, const std::string&) const;
			bool is_identifier() const;
			bool is_operator() const;
			bool matches_datatype() const;
			bool matches_struct_declaration() const;
			bool matches_variable_declaration() const;
			bool matches_procedure_declaration() const;
			void init_types();
			void handle_struct_declaration();
			void handle_procedure_declaration();
			Variable_Declaration* parse_variable_declaration();
			Struct_Field* parse_struct_field(Struct_Information*);
			Datatype_Information_Base* parse_datatype();
			void register_type(Datatype_Information_Base*);
			void register_procedure(Procedure_Information*);
			Procedure_Information* get_procedure(const std::string&);
			Procedure_Information* get_procedure(const std::string&, const std::string&);
			Datatype_Information_Base* get_type(const std::string&) const;

		public:

		friend Parse_Context* generate_tree(Lex_Context*);
	};

	Parse_Context* generate_tree(Lex_Context*);

	
};

#endif
