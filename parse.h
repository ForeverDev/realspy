#ifndef PARSE_H
#define PARSE_H

#include <stdint.h>
#include <map>
#include "lex.h"

using namespace Lexer;

namespace Parser {
	
	// forward declarations
	struct Variable_Declaration;
	struct Datatype_Information;
	struct Struct_Information;
	struct Procedure_Information;
	struct Byte_Information;
	class  Parse_Context;

	enum Operator_Associativity {
		ASSOC_LEFT,
		ASSOC_RIGHT
	};

	enum Operator_Type {
		OP_BINARY,
		OP_UNARY
	};
	
	// default rules:
	//   1. types must match exactly unless one of the following is true
	//   2. byte->int always allowed
	//   3. int->byte always allowed
	//   4. int->float always allowed
	//   5. float->int always allowed
	enum Typecheck_Rule {
		// both unary and binary
		IGNORE_ALL_RULES,
		ALLOW_IMPLICIT_POINTER_CAST,
		ALLOW_POINTER_INT,
		DISALLOW_FLOAT,
		DISALLOW_POINTER,
		DISALLOW_NON_POINTER,
		ENFORCE_INTEGER,
		ENFORCE_FLOAT,
		ENFORCE_BOOL,
		ENFORCE_DATATYPE,

		// binary only
		ALLOW_POINTER_LEFT_SIDE_INT,
		ENFORCE_LEFT_L_VALUE,

		// unary only
		ENFORCE_L_VALUE
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
		FIND_MEMBER,
		COMMA
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
		EXPRESSION_CAST,
		EXPRESSION_CALL,
		EXPRESSION_ARRAY_INDEX
	};

	enum Ast_Node_Type {
		NODE_IF,
		NODE_WHILE,
		NODE_FOR,
		NODE_BLOCK,
		NODE_PROCEDURE_IMPLEMENTATION,
		NODE_DECLARATION,
		NODE_STATEMENT,
		NODE_RETURN
	};

	struct Expression {
		enum Leaf {
			LEAF_LEFT,
			LEAF_RIGHT
		};

		Expression(Expression_Type t): type(t) { }
		virtual ~Expression() {}
		virtual std::string to_string() const = 0;
		virtual Datatype_Information* typecheck(Parse_Context *) = 0;
		virtual void print(int) const;
		bool is_binary_type(Binary_Operator_Type type) const;
		bool is_unary_type(Unary_Operator_Type type) const;
		
		Token* token = nullptr;	
		Expression_Type type;
		Datatype_Information* eval;
		std::string word;	
		Expression* parent = nullptr;	
		Leaf side; // only applicable if parent is binary operator
		bool is_l_value = true; // if false, is_r_value == true
	};

	struct Expression_Operator : public Expression {

		Expression_Operator(Expression_Type t): Expression(t) { }
		virtual std::string to_string() const override = 0;
		virtual Datatype_Information* typecheck(Parse_Context *) override = 0;

		const Operator_Descriptor* desc;
		const Token* tok;
	};

	struct Expression_Binary : public Expression_Operator {
		Expression_Binary(): Expression_Operator(EXPRESSION_OPERATOR_BINARY) { }
		virtual std::string to_string() const override; 
		virtual Datatype_Information* typecheck(Parse_Context *) override;
		virtual void print(int) const override;
		
		bool is_assign() const;

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
		virtual Datatype_Information* typecheck(Parse_Context *) override;
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
		virtual Datatype_Information* typecheck(Parse_Context *) override;
		virtual void print(int) const override;

		Datatype_Information* value;
		Expression* operand;
	};

	struct Expression_Call : public Expression_Operator {
		Expression_Call(): Expression_Operator(EXPRESSION_CALL) {}
		virtual std::string to_string() const override;
		virtual Datatype_Information* typecheck(Parse_Context *) override;
		virtual void print(int) const override;
	
		std::vector<Datatype_Information *> arg_types;
        std::vector<Expression *> arg_expression;
		std::string call_sig;
		Expression* proc;
		Expression* argument;
		int nargs;
	};

	struct Expression_Array_Index : public Expression_Operator {
		Expression_Array_Index(): Expression_Operator(EXPRESSION_ARRAY_INDEX) {}
		virtual std::string to_string() const override;
		virtual Datatype_Information* typecheck(Parse_Context *) override;
		virtual void print(int) const override;

		Expression* array;
		Expression* index;
	};

	struct Expression_Integer_Literal : public Expression {
		Expression_Integer_Literal(): Expression(EXPRESSION_INTEGER_LITERAL) { }
		virtual std::string to_string() const override;
		virtual Datatype_Information* typecheck(Parse_Context *) override;

		int64_t value;
	};

	struct Expression_Float_Literal : public Expression {
		Expression_Float_Literal(): Expression(EXPRESSION_FLOAT_LITERAL) { }
		virtual std::string to_string() const override;
		virtual Datatype_Information* typecheck(Parse_Context *) override;

		double value;
	};

	struct Expression_String_Literal : public Expression {
		Expression_String_Literal(): Expression(EXPRESSION_STRING_LITERAL) { }
		virtual std::string to_string() const override;
		virtual Datatype_Information* typecheck(Parse_Context *) override;

		std::string value;
	};

	struct Expression_Identifier : public Expression {
		Expression_Identifier(): Expression(EXPRESSION_IDENTIFIER) { }
		virtual std::string to_string() const override;
		virtual Datatype_Information* typecheck(Parse_Context *) override;
		
		// variable is only applicable if the identifier
		// typechecks to a local... otherwise it is null
		Variable_Declaration* variable = nullptr;
		std::string value;
	};

	struct Expression_Datatype : public Expression {
		Expression_Datatype(): Expression(EXPRESSION_DATATYPE) { }
		virtual std::string to_string() const override;
		virtual Datatype_Information* typecheck(Parse_Context *) override;

		Datatype_Information* value;
	};
	
	struct Variable_Declaration {
		Variable_Declaration() {}
		Variable_Declaration(const Variable_Declaration&);
		Variable_Declaration* clone() const;
		std::string to_string() const;
		
		int tag = -1; // -1 for debugging
		bool has_name = true; // only used for procedure arguments	
		std::string identifier;
		Datatype_Information* dt = nullptr;
		Token* identifier_token = nullptr;
	};

	struct Datatype_Information {
		virtual ~Datatype_Information() {};
		Datatype_Information() {}
		virtual Datatype_Information* clone() const = 0;
		virtual std::string to_string() const;
		virtual bool matches(const Datatype_Information&) const;
		bool is_pointer() const { return ptr_dim > 0; }
		bool is_array() const { return arr_dim > 0; }
		bool is_int() const { return type_name == "int" && !is_pointer() && !is_array(); }
		bool is_float() const { return type_name == "float" && !is_pointer() && !is_array(); }
		bool is_byte() const { return type_name == "byte" && !is_pointer() && !is_array(); }
		bool is_bool() const { return type_name == "bool" && !is_pointer() && !is_array(); }
		bool is_void() const { return type_name == "void" && !is_pointer() && !is_array(); }
		bool is_struct() const { return is_a_struct && !is_pointer() && !is_array(); }
		bool matches_strict(const Datatype_Information&) const;
		void fill_fields(const Datatype_Information&);

		std::string type_name;
		int ptr_dim = 0;
		int arr_dim = 0;
		std::vector<int> arr_size;
		int size;
		bool is_a_struct = false;
	};

	struct Struct_Field {
		Struct_Field() {}

		Variable_Declaration* decl;
		Struct_Information* parent_struct;
	};

	struct Struct_Information : public Datatype_Information {
		Struct_Information() { is_a_struct = true; }
		virtual Struct_Information* clone() const override;
		Struct_Field* get_field(const std::string& id) {
			for (auto field: fields) {
				if (field->decl->identifier == id) {
					return field;
				}
			}
			return nullptr;
		}

		bool is_complete = false;
		std::vector<Struct_Field *> fields;
	};

	struct Procedure_Information : public Datatype_Information {
		Procedure_Information() {}
		virtual Procedure_Information* clone() const override;
		virtual std::string to_string() const override;
		virtual bool matches(const Datatype_Information&) const override;
		std::string get_signature() const;
		static std::string make_signature(const std::vector<Variable_Declaration *>&);
		static std::string make_signature(const std::vector<Datatype_Information *>&);
		
		bool is_implemented = false;
		std::vector<Variable_Declaration *> args;
		Datatype_Information* ret;
	};

	struct Integer_Information : public Datatype_Information {
		Integer_Information() {}
		virtual Integer_Information* clone() const override;
	};

	struct Float_Information : public Datatype_Information {
		Float_Information() {}
		virtual Float_Information* clone() const override;
	};

	struct Void_Information : public Datatype_Information {
		Void_Information() {}
		virtual Void_Information* clone() const override;
	};

	struct Bool_Information : public Datatype_Information {
		Bool_Information() {}
		virtual Bool_Information* clone() const override;
	};

	struct Byte_Information : public Datatype_Information {
		Byte_Information() {}
		virtual Byte_Information* clone() const override;
	};

	struct Ast_Node {
		Ast_Node(Ast_Node_Type t): type(t) {}
		virtual ~Ast_Node() {}
		virtual void print(int) const = 0;

		Ast_Node_Type type;
		Ast_Node* parent = nullptr;
	};

	struct Ast_Statement : public Ast_Node {
		Ast_Statement(): Ast_Node(NODE_STATEMENT) {}
		virtual void print(int) const override;

		Expression* expression;
	};

	struct Ast_Block : public Ast_Node {
		Ast_Block(): Ast_Node(NODE_BLOCK) {}
		virtual void print(int) const override;
	
		std::vector<Ast_Node *> children;
	};

	struct Ast_Procedure : public Ast_Node {
		Ast_Procedure(): Ast_Node(NODE_PROCEDURE_IMPLEMENTATION) {}
		virtual void print(int) const override;
		
		int declared_line;	
		Procedure_Information* info;
		Ast_Node* child = nullptr;	
	};

	struct Ast_If : public Ast_Node {
		Ast_If(): Ast_Node(NODE_IF) {}
		virtual void print(int) const override;

		Expression* condition;
		Ast_Node* child = nullptr;
	};

	struct Ast_While : public Ast_Node {
		Ast_While(): Ast_Node(NODE_WHILE) {}
		virtual void print(int) const override;

		Expression* condition;
		Ast_Node* child = nullptr;
	};

	struct Ast_For : public Ast_Node {
		Ast_For(): Ast_Node(NODE_FOR) {}
		virtual void print(int) const override;
		
		Expression* initializer;
		Expression* condition;
		Expression* statement;
		Ast_Node* child = nullptr;
	};

	struct Ast_Declaration : public Ast_Node {
		Ast_Declaration(): Ast_Node(NODE_DECLARATION) {}
		virtual void print(int) const override;
		
		Variable_Declaration* decl = nullptr;
	};

	struct Ast_Return : public Ast_Node {
		Ast_Return(): Ast_Node(NODE_RETURN) {}
		virtual void print(int) const override;

		Expression* expression;
	};

	class Parse_Context {
		private:
			int token_index;
			int marked_index;
			int fail_indent = -1;
			int local_count = 0;
			Token* token;	
			Lex_Context* lex_context;	
			std::vector<Datatype_Information *> defined_types;
			std::vector<Ast_Procedure *> defined_procedures;
			Integer_Information* type_int;
			Void_Information* type_void;
			Float_Information* type_float;
			Bool_Information* type_bool;
			Byte_Information* type_byte;
			Byte_Information* type_string;
			Ast_Node* focus;
			Ast_Block* current_block;
			Ast_Procedure* current_procedure = nullptr;
			bool guard_register_type = false;
			
			void report_error(const std::string&) const;	
			void report_error_at_indent(const std::string&, int);

			void assert_token() const;			
			void focus_token(int);
			Token* get_token(int);

			void eat(const std::string&, const std::string&);
			void eat(const std::string&);
			void eat();
			std::string eat_and_get();
			bool on(const std::string&) const;
			bool on(int, const std::string&) const;
			bool is_identifier() const;
			bool is_operator() const;

			void init_types();

			bool matches_datatype() const;
			bool matches_struct_declaration() const;
			bool matches_comma_identifier_chain();
			bool matches_variable_declaration() const;
			bool matches_procedure_declaration() const;
			bool matches_inferred_variable_declaration() const;
			bool matches_double_colon_declaration() const;

			void handle_struct_declaration();
			void handle_procedure_declaration();
			void handle_variable_declaration();
			void handle_constant_declaration();
			void handle_inferred_variable_declaration();
			void handle_standalone_statement();
			void handle_if();
			void handle_while();
			void handle_for();
			void handle_return();
			void handle_block();
			void handle_jump_out();
			void append_node(Ast_Node*);

			Variable_Declaration* parse_variable_declaration();
			Struct_Field* parse_struct_field(Struct_Information *);
			Datatype_Information* parse_datatype();
			Procedure_Information* parse_procedure_descriptor();
			Expression* parse_expression();
			Expression* parse_expression_and_typecheck();
			Expression* fold_expression(Expression *);

			void mark(const std::string&, const std::string&);
			void mark(const std::string&);

			void register_type(Datatype_Information *);
			void register_procedure(Ast_Procedure* );

			Ast_Procedure* get_procedure(const std::string&, const std::string&) const;
			Datatype_Information* get_type(const std::string&) const;
			Variable_Declaration* get_local(const std::string&) const;

		public:
			Ast_Block* root_node;
			std::vector<Ast_Procedure *> get_all_procedures(const std::string&) const;

		friend Parse_Context* generate_tree(Lex_Context*);
		friend class Expression;
		friend class Expression_Binary;
		friend class Expression_Unary;
		friend class Expression_Cast;
		friend class Expression_Integer_Literal;
		friend class Expression_Float_Literal;
		friend class Expression_String_Literal;
		friend class Expression_Identifier;
		friend class Expression_Datatype;
		friend class Expression_Call;
		friend class Expression_Array_Index;
	};

	Parse_Context* generate_tree(Lex_Context*);

	
};

#endif
