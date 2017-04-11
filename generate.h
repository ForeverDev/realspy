#ifndef GENERATE_H
#define GENERATE_H

#include <fstream>
#include "parse.h"

using namespace Parser;

namespace Generator {

	class Generate_Context {
		private:
			std::ofstream output;
			Parse_Context* context;
			
			Generate_Context(const std::string& fn): output(fn, std::ios::out) {} 	
			int generate_expression(Expression *);
			void generate_node(Ast_Node *);
			void generate_if(Ast_If *);
			void generate_procedure(Ast_Procedure *);
			void generate_declaration(Ast_Declaration *);
			void generate_headers();
			std::string make_variable(const Variable_Declaration *);
			std::string make_procedure_name(const Ast_Procedure *);
			std::string make_datatype(const Datatype_Information *);
			
		public:	

		friend void generate_c_file(const std::string&, Parse_Context *);
		friend class Parse_Context;
	};

	void generate_c_file(const std::string&, Parse_Context *);	
};

#endif
