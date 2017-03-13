#ifndef PARSE_H
#define PARSE_H

#include <stdint.h>
#include <map>
#include "lex.h"

using namespace Lexer;

namespace Parser {
	
	// forward declarations
	struct Datatype_Information_Base;
	struct Struct_Information;

	enum Operator_Associativity {
		ASSOC_LEFT,
		ASSOC_RIGHT
	};

	enum Operator_Type {
		OP_BINARY,
		OP_UNARY
	};
	
	// default rules:
	//   1. types must match exactly
	//   2. all datatypes are acceptable	
	//   3. byte->int always allowed
	//   4. int->byte always allowed
	//   5. int->float always allowed
	//   6. float->int always allowed
	enum Typecheck_Rule {
		IGNORE_ALL_RULES,
		ALLOW_IMPLICIT_POINTER_CAST,
		ALLOW_POINTER_INT,
		DISALLOW_FLOAT,
		DISALLOW_POINTER,
		DISALLOW_NON_POINTER,
		DISALLOW_LITERAL,
		ENFORCE_INTEGER,
		ENFORCE_FLOAT,
		ENFORCE_BOOL,
		ENFORCE_DATATYPE
	};

	struct Operator_Descriptor {
		Operator_Descriptor(std::string o, int p, Operator_Associativity a, Operator_Type t, std::vector<Typecheck_Rule> r)
							: op_string(o), prec(p), assoc(a), type(t), rules(r) { }

		std::string op_string;
		int prec;
		Operator_Associativity assoc;
		Operator_Type type;
		std::vector<Typecheck_Rule> rules;
	};


	enum Binary_Operator_Type {
		ADDITION,
		SUBTRACTION,
		MULTIPLICATION,
		DIVISION,
		MODULUS,
		BITWISE_AND,
		BITWISE_OR,
		SHIFT_LEFT,
		SHIFT_RIGHT,
		BITWISE_XOR,
		LOGICAL_AND,
		LOGICAL_OR,
		ASSIGN,
		ADDITION_BY,
		SUBTRACTION_BY,
		MULTIPLICATION_BY,
		DIVISION_BY,
		MODULUS_BY,
		BITWISE_AND_BY,
		BITWISE_OR_BY,
		BITWISE_XOR_BY,
		SHIFT_LEFT_BY,
		SHIFT_RIGHT_BY,
		COMPARE,
		COMPARE_NOT,
		LESS_THAN,
		LESS_THAN_EQUAL,
		GREATER_THAN,
		GREATER_THAN_EQUAL,
		FIND_MEMBER
	};

	enum Unary_Operator_Type {
		LOGICAL_NOT,
		BITWISE_NOT,
		ADDRESS_OF,
		DEREFERENCE,
		OPEN_PARENTHESIS,
		CLOSE_PARENTHESIS
	};

	enum Expression_Type {
		EXPRESSION_OPERATOR_BINARY,
		EXPRESSION_OPERATOR_UNARY,
		EXPRESSION_INTEGER_LITERAL,
		EXPRESSION_FLOAT_LITERAL,
		EXPRESSION_STRING_LITERAL,
		EXPRESSION_IDENTIFIER,
		EXPRESSION_DATATYPE,
		EXPRESSION_CAST
	};

	struct Expression {
		enum Leaf {
			LEAF_LEFT,
			LEAF_RIGHT
		};

		Expression(Expression_Type t): type(t) { }
		virtual ~Expression() {}
		virtual std::string to_string() const = 0;
		virtual void print(int) const;
		bool is_binary_type(Binary_Operator_Type type) const;
		bool is_unary_type(Unary_Operator_Type type) const;
		
		Expression_Type type;
		std::string word;	
		Expression* parent = nullptr;	
		Leaf side; // only applicable if parent is binary operator
		int line;
		int col;
	};

	struct Expression_Operator : public Expression {

		Expression_Operator(Expression_Type t): Expression(t) { }
		virtual std::string to_string() const = 0;

		const Operator_Descriptor* desc;
	};

	struct Expression_Binary : public Expression_Operator {
		Expression_Binary(): Expression_Operator(EXPRESSION_OPERATOR_BINARY) { }
		virtual std::string to_string() const override; 
		virtual void print(int) const override;

		static Binary_Operator_Type word_to_type(const std::string&);
		static std::string type_to_word(Binary_Operator_Type);

		static std::map<std::string, Binary_Operator_Type> word_map; 

		Binary_Operator_Type value;
		Expression* left;
		Expression* right;
	};

	struct Expression_Unary : public Expression_Operator {
		Expression_Unary(): Expression_Operator(EXPRESSION_OPERATOR_UNARY) { }
		virtual std::string to_string() const override;
		virtual void print(int) const override;

		static Unary_Operator_Type word_to_type(const std::string&);
		static std::string type_to_word(Unary_Operator_Type);

		static std::map<std::string, Unary_Operator_Type> word_map; 

		Unary_Operator_Type value;
		Expression* operand;
	};

	struct Expression_Cast : public Expression_Operator {
		Expression_Cast(): Expression_Operator(EXPRESSION_CAST) { }
		virtual std::string to_string() const override;
		virtual void print(int) const override;

		Datatype_Information_Base* value;
		Expression* operand;
	};

	struct Expression_Integer_Literal : public Expression {
		Expression_Integer_Literal(): Expression(EXPRESSION_INTEGER_LITERAL) { }
		virtual std::string to_string() const override;

		int64_t value;
	};

	struct Expression_Float_Literal : public Expression {
		Expression_Float_Literal(): Expression(EXPRESSION_FLOAT_LITERAL) { }
		virtual std::string to_string() const override;

		double value;
	};

	struct Expression_String_Literal : public Expression {
		Expression_String_Literal(): Expression(EXPRESSION_STRING_LITERAL) { }
		virtual std::string to_string() const override;

		std::string value;
	};

	struct Expression_Identifier : public Expression {
		Expression_Identifier(): Expression(EXPRESSION_IDENTIFIER) { }
		virtual std::string to_string() const override;

		std::string value;
	};

	struct Expression_Datatype : public Expression {
		Expression_Datatype(): Expression(EXPRESSION_DATATYPE) { }
		virtual std::string to_string() const override;

		Datatype_Information_Base* value;
	};
	
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
		std::vector<Struct_Field *> fields;
	};

	struct Procedure_Information : public Datatype_Information_Base {
		Procedure_Information(const Procedure_Information&);
		Procedure_Information() {}
		std::string to_string() const;
		std::string get_signature() const;
		static std::string make_signature(const std::vector<Variable_Declaration *>&);
		
		bool is_implemented = false;
		std::vector<Variable_Declaration *> args;
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
			int marked_index;
			Token* token;	
			Lex_Context* lex_context;	
			std::vector<Datatype_Information_Base *> defined_types;
			std::vector<Procedure_Information *> defined_procedures;
			
			void report_error(const std::string&) const;	

			void assert_token() const;			
			void focus_token(int index);
			Token* get_token(int index);

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
			void handle_standalone_statement();

			Variable_Declaration* parse_variable_declaration();
			Struct_Field* parse_struct_field(Struct_Information*);
			Datatype_Information_Base* parse_datatype();
			Expression* parse_expression();

			void mark(const std::string&, const std::string&);
			void mark(const std::string&);

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
