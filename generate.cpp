#include <iostream>
#include <fstream>
#include "generate.h"

using namespace Parser;
using namespace Generator;

static const std::string REG_NAME = "__R__";
static const std::string SPYRE_TYPE_NAME = "__T__";
static const int		 NUM_FRAME_REGISTERS = 12;

void
Generate_Context::generate_expression(Expression* exp) {
	if (!exp) {
		return;
	}
		
}

void
Generate_Context::generate_if(Ast_If* node) {
	generate_expression(node->condition);
	generate_node(node->child);
}

void
Generate_Context::generate_procedure(Ast_Procedure* node) {
	auto proc_name = make_procedure_name(node);
	auto ret_type = node->info->ret;
	if (proc_name == "main") {
		output << "int ";
	} else {
		output << make_datatype(ret_type) << " ";
	}
	output << proc_name << "() {\n";
	output << SPYRE_TYPE_NAME << " " << REG_NAME << "[" << NUM_FRAME_REGISTERS << "];\n";	
	generate_node(node->child);
	output << "}\n";
}

void
Generate_Context::generate_headers() {
	output << "#include <stdio.h>\n";
	output << "#include <stdlib.h>\n";
	output << "#include <stdint.h>\n";
	output << "#include <string.h>\n";
	output << "typedef struct __SPYRE_TYPE_TAG__ {\n";
	output << "  int64_t i;\n";
	output << "  double f;\n";
	output << "  char c;\n";
	output << "  void* p;\n";
	output << "} " << SPYRE_TYPE_NAME << ";\n";
}

void
Generate_Context::generate_node(Ast_Node* node) {
	switch (node->type) {
		case NODE_BLOCK:
			for (auto child: static_cast<Ast_Block *>(node)->children) {
				generate_node(child);	
			}
			break;	
		case NODE_PROCEDURE_IMPLEMENTATION: 
			generate_procedure(static_cast<Ast_Procedure *>(node));
			break;
		case NODE_IF:
			generate_if(static_cast<Ast_If *>(node));
			break;
		default:
			break;
	}
}

std::string
Generate_Context::make_procedure_name(const Ast_Procedure* proc) {
	return proc->info->type_name + proc->info->get_signature();
}

std::string
Generate_Context::make_datatype(const Datatype_Information* dt) {
	if (dt->is_void()) {
		return "void";
	}
	std::string ret = SPYRE_TYPE_NAME;
	if (dt->arr_dim + dt->ptr_dim > 0) {
		ret += " ";
	}
	for (int i = 0; i < dt->arr_dim + dt->ptr_dim; i++) {
		ret += "*";
	}
	return ret;
}

void
Generator::generate_c_file(const std::string& out_name, Parse_Context* context) {
	
	Generate_Context gc;
	gc.context = context;
	gc.output = std::ofstream(out_name, std::ios::out);

	if (!gc.output.is_open()) {
		std::cerr << "couldn't open " << out_name << " for writing\n";
		std::exit(1);
	}

	gc.generate_headers();
	gc.generate_node(context->root_node);

	gc.output.close();

}
