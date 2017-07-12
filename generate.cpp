#include <iostream>
#include <fstream>
#include <functional>
#include "generate.h"

using namespace Parser;
using namespace Generator;

static const std::string REG_NAME = "__R__";
static const std::string SPYRE_TYPE_NAME = "__T__";
static const std::string VAR_NAME = "__V__";
static const std::string INT_NAME = "int64_t";
static const std::string FLOAT_NAME = "double";
static const int		 NUM_FRAME_REGISTERS = 12;

int
Generate_Context::generate_expression(Expression* exp) {
	if (!exp) {
		return -1;
	}

	bool register_marks[NUM_FRAME_REGISTERS] = {};

	auto mark_register = [&](int r) {
		register_marks[r] = true;
	};

	auto unmark_register = [&](int r) {
		std::cout << r << std::endl;
		register_marks[r] = false;
	};

	auto get_register = [&]() {
		for (int i = 0; i < NUM_FRAME_REGISTERS; i++) {
			if (!register_marks[i]) {
				mark_register(i);
				return i;
			}
		}
		return -1;
	};

	auto make_register = [&](int r) -> std::string {
		return REG_NAME + "[" + std::to_string(r) + "]";
	};
	
	auto make_register_field = [&](int r, const std::string& field) -> std::string {
		return make_register(r) + "." + field;
	};

	auto move_register = [&](int from, int to) {
		output << make_register(to) << " = " << make_register(from) << ";";	
	};

	auto make_binary = [&](int to, int r0, int r1, const std::string& field, Binary_Operator_Type t) {
		output << make_register_field(to, field);
		output << " = ";
		output << make_register_field(r0, field);
		output << " ";
		output << Expression_Binary::type_to_word(t);
		output << " ";
		output << make_register_field(r1, field);
		output << ";";
	};

	auto make_assignment = [&](int from, int to, const std::string& field, 
							   const std::string& pointer_type, Binary_Operator_Type t) {
		output << "*(";
		output << pointer_type;
		output << " *)";
		output << make_register_field(to, field);
		output << " ";
		output << Expression_Binary::type_to_word(t);
		output << " ";
		output << make_register_field(from, field);
		output << ";";
	};

	auto newline = [&]() {
		output << std::endl;
	};
	
	// this lambda is recursive, so an explicit type is needed.....
	std::function<int (Expression *)> do_generate = [&](Expression* exp) -> int {

		switch (exp->type) {
			case EXPRESSION_OPERATOR_BINARY: {
				auto bin = static_cast<Expression_Binary *>(exp);
				switch (bin->value) {
					case ADDITION: {
						int r0 = do_generate(bin->left);
						int r1 = do_generate(bin->right);
						make_binary(r0, r0, r1, "i", bin->value); 
						newline();
						unmark_register(r1);
						return r0;
					}
					case ASSIGN:
					case ADDITION_BY:
					case SUBTRACTION_BY:
					case MULTIPLICATION_BY:
					case DIVISION_BY:
					case MODULUS_BY:
					case BITWISE_AND_BY:
					case BITWISE_OR_BY:
					case BITWISE_XOR_BY:
					case SHIFT_LEFT_BY:
					case SHIFT_RIGHT_BY: {
						int r0 = do_generate(bin->left);
						int r1 = do_generate(bin->right);
						make_assignment(r1, r0, "i", INT_NAME, bin->value);
						newline();
						unmark_register(r1);
						return r0;
					}
                    default:
                        break;
				}
				break;
			}
			case EXPRESSION_INTEGER_LITERAL: {
				auto lit = static_cast<Expression_Integer_Literal *>(exp);
				int reg = get_register();
				output << make_register_field(reg, "i");
				output << " = ";
				output << std::to_string(lit->value);
				output << ";";
				newline();
				return reg;
			}
			case EXPRESSION_FLOAT_LITERAL: {
				auto lit = static_cast<Expression_Float_Literal *>(exp);
				int reg = get_register();
				output << make_register_field(reg, "f");
				output << " = ";
				output << std::to_string(lit->value);
				output << ";";
				newline();
				return reg;
			}
			case EXPRESSION_IDENTIFIER: {
				auto id = static_cast<Expression_Identifier *>(exp);
				int reg = get_register();
				bool child_of_assign = false;
				if (exp->parent && exp->parent->type == EXPRESSION_OPERATOR_BINARY) {
					auto bin_parent = static_cast<Expression_Binary *>(exp->parent);
					child_of_assign = bin_parent->is_assign();	
				}
				if (id->variable) {
					if (child_of_assign) {
						output << make_register_field(reg, "p");
						output << " = &";
						output << make_variable(id->variable);
					} else {
						output << make_register_field(reg, "i");
						output << " = ";
						output << make_variable(id->variable);
					}
					output << ";";
					newline();	
				}
                auto all = context->get_all_procedures(id->value);
                if (all.size() == 1) {
                    output << make_register_field(reg, "p");
                    output << "=";
                    output << all[0]->info->type_name << all[0]->info->get_signature() << ";" << std::endl;
                }
				return reg;
			}
            case EXPRESSION_CALL: {
                auto call = static_cast<Expression_Call *>(exp);
                int call_reg = do_generate(call->proc);
                for (auto& arg: call->arg_expression) {
                    int arg_reg = do_generate(arg);
                }
                output << make_register_field(call_reg, "p");
                output << "(";
                size_t args = call->arg_expression.size();
                for (int i = 0; i < args; i++) {
                    output << make_register_field(call_reg + i + 1, make_prefix(call->arg_expression[i]->eval));
                    if (i < args - 1) {
                        output << ", ";
                    }
                }
                output << ");\n";
                return call_reg;
            }
			default:
                return 0;
		}

        // won't be reached but silences warning
        return 0;
	};
	
	return do_generate(exp);
		
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

std::string
Generate_Context::make_variable(const Variable_Declaration* decl) {
	return VAR_NAME + std::to_string(decl->tag);	
}

std::string
Generate_Context::make_prefix(const Datatype_Information* dt) {
    if (dt->is_float()) {
        return "f";
    }
    if (dt->is_pointer()) {
        return "p";
    }
    if (dt->is_byte()) {
        return "c";
    }
    return "i";
}

void
Generate_Context::generate_declaration(Ast_Declaration* node) {
	auto dt = node->decl->dt;
	auto var_name = make_variable(node->decl);
	output << SPYRE_TYPE_NAME << " ";
	output << var_name;
	for (int i = 0; i < dt->arr_dim; i++) {
		output << "[";
		output << std::to_string(dt->arr_size[i]);
		output << "]";
	}
	output << ";\n";
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
		case NODE_STATEMENT:
			generate_expression(static_cast<Ast_Statement *>(node)->expression);	
			break;
		case NODE_DECLARATION:
			generate_declaration(static_cast<Ast_Declaration *>(node));
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
	
	Generate_Context gc(out_name);
	gc.context = context;

	if (!gc.output.is_open()) {
		std::cerr << "couldn't open " << out_name << " for writing\n";
		std::exit(1);
	}

	gc.generate_headers();
	gc.generate_node(context->root_node);

	gc.output.close();

}
