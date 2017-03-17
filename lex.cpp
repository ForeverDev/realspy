#include "lex.h"
#include <iostream>
#include <cstdlib>
#include <cctype>
#include <sstream>

using namespace Lexer;

static const std::vector<std::string> reserved_words {
	"if", "while", "do", "repeat", "until",
	"import", "return", "continue", "break",
	"define"
};

int
Lex_Context::get() {
	int at = handle.get();
	if (at == '\n') {
		col = 1;
	} else if (at == '\t') {
		col += 8;
	} else {
		col++;
	}
	return at;
}

int
Lex_Context::peek() {
	return handle.peek();
}

void
Lex_Context::report_error(const std::string& message) const {
	int line = 0;
	int col = 0;
	Token* focus = nullptr;
	if (tokens->size() > 0) {
		focus = &tokens->back();
		line = focus->line;
		col = focus->col;
	}
	std::cerr << "-------- SPYRE LEX ERROR -------\n\n";
	std::cerr << "line:    " << line << std::endl;
	std::cerr << "col:     " << col << std::endl;
	if (focus) {
		std::cerr << "token:   '" << focus->word << "'" << std::endl;
	}
	std::cerr << "message: " << message << std::endl;
	std::cerr << std::endl << std::endl;
	std::exit(1);
}

void
Lex_Context::append_token(Token& t) {
	t.line = line;
	t.col = col - t.word.length() - 1;
	if (tokens->size() > 0) {
		Token& back = tokens->back();
		switch (t.type) {
			case TOKEN_INTEGER:
			case TOKEN_FLOAT:
				if (back.type == TOKEN_INTEGER || back.type == TOKEN_FLOAT) {
					report_error("invalid number followed by another number");
				}
				break;
			case TOKEN_STRING:
				// back to back strings? concatenate them
				if (back.type == TOKEN_STRING) {
					back.word += t.word;
					return;
				}
				break;
			default:
				break;
		}
	}
	tokens->push_back(t);
}

bool
Lex_Context::matches_number() const {
	return std::isdigit(on);
}

bool
Lex_Context::matches_identifier() const {
	return std::isalpha(on) || on == '_';
}

bool
Lex_Context::matches_string() const {
	return on == '"';
}

bool
Lex_Context::matches_operator() const {
	return std::ispunct(on) && on != '_';
}

void
Lex_Context::handle_number() {
	std::ostringstream buf;
	Token t;
	bool is_float = false;
	bool is_hex = false;
	int first_char = get();
	if (first_char == '0' && peek() == 'x') {
		is_hex = true;
		get();
	} else {
		buf << static_cast<char>(first_char);
	}
	while (std::isdigit(peek())) {
		buf << static_cast<char>(get());
	}
	bool was_a_dot = peek() == '.';
	if (was_a_dot) {
		is_float = true;
		buf << '.';
		get();
	}
	if (was_a_dot && !std::isdigit(peek())) {
		report_error("expected digit to follow token '.'");
	}
	while (std::isdigit(peek())) {
		buf << static_cast<char>(get());
	}
	if (is_float && is_hex) {
		report_error("invalid number format");
	}	
	t.word = buf.str();
	if (is_float) {
		t.type = TOKEN_FLOAT;
		t.f = std::stod(t.word);
	} else {
		t.type = TOKEN_INTEGER;
		t.i = std::stoll(t.word, nullptr, is_hex ? 16 : 10);
		if (is_hex) {
			t.word += 'h';
		}
	}
	append_token(t);
}

void
Lex_Context::handle_identifier() {
	std::ostringstream buf;
	Token t;
	t.type = TOKEN_IDENTIFIER;
	while (std::isalnum(peek()) || peek() == '_') {
		buf << static_cast<char>(get());
	}
	t.word = buf.str();
	append_token(t);
}

void
Lex_Context::handle_string() {
	std::ostringstream buf;
	Token t;
	t.type = TOKEN_STRING;
	get(); // skip "
	while (peek() != EOF && peek() != '"') {
		buf << static_cast<char>(get());
	}
	if (peek() == EOF) {
		report_error("unexpected EOF when lexing string");
	}
	get();
	t.word = buf.str();
	append_token(t);
}

void
Lex_Context::handle_operator() {

	static const std::vector<std::string> long_ops {
		"+=", "-=", "*=", "/=",
		"%=", "&=", "|=", "^=",
		">>=", "<<=", ">>", "<<",
		"&&", "||", "==", "!=",
		"::", "->", ":="
	};

	auto is_long_op = [](const std::string& op) -> bool {
		size_t len = op.length();
		for (const auto& check: long_ops) {
			if (len > check.length()) {
				continue;
			}
			if (op == check.substr(0, len)) {
				return true;
			}
		}
		return false;
	};

	std::stringstream buf;
	Token t;
	t.type = TOKEN_OPERATOR;
	while (std::ispunct(peek())) {
		buf << static_cast<char>(get());
		if (!is_long_op(buf.str() + static_cast<char>(peek()))) {
			break;
		}
	}
	t.word = buf.str();

	append_token(t);
	
}

Lex_Context*
Lexer::generate_tokens(const std::string& filename) {

	Lex_Context* state = new Lex_Context(filename);
	state->tokens = new std::vector<Token>();
	if (!state->handle.is_open()) {
		std::cerr << "couldn't open '" << filename << "' for reading.\n";
		std::exit(EXIT_FAILURE);
	}	
	state->line = 1;
	state->col = 1;
	state->on = state->handle.get();
	state->next = state->handle.get();
	state->handle.clear();
	state->handle.seekg(0);

	std::stringstream line_make;
	while ((state->on = state->peek()) != EOF) {
		if (state->on == '\n') {
			state->raw_file.push_back(line_make.str());
			line_make.str(std::string());
			state->get();
			state->on = state->peek();
			if (std::isspace(state->on)) {
				while (std::isspace(state->on)) {
					if (state->on == '\n') {
						state->raw_file.push_back("");
					}
					state->on = state->get();
				}
				state->handle.unget();
			}
			state->on = state->peek();
		} else {
			line_make << static_cast<char>(state->on);
			state->get();
		}
	}
	
	state->handle.clear();
	state->handle.seekg(0);
	
	while ((state->on = state->peek()) != EOF) {
		if (std::isspace(state->on)) {
			if (state->on == '\n') {
				state->line++;
				state->get();
				state->on = state->peek();
				if (std::isspace(state->on)) {
					while (std::isspace(state->on)) {
						if (state->on == '\n') {
							state->line++;
						}
						state->on = state->get();
					}
					state->handle.unget();
				}
				state->col = 1;
			} else {
				state->get();
			}
			continue;
		} else if (state->matches_number()) {
			state->handle_number();
		} else if (state->matches_identifier()) {
			state->handle_identifier();
		} else if (state->matches_string()) {
			state->handle_string();
		} else if (state->matches_operator()) {
			state->handle_operator();
		}
	}
	
	/*
	for (auto& tok: *state->tokens) {
		std::cout << tok.word << std::endl;
	}
	*/
	
	return state;

}
