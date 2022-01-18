#include <algorithm>
#include <cassert>
#include <charconv>
#include <filesystem>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <iterator>
#include <optional>
#include <span>

#include <fmt/format.h>
#include <fmt/ostream.h>

#include "./os-exec/os-exec.hh"

using namespace fmt::literals;
using namespace std::string_view_literals;

namespace fs = std::filesystem;

fs::path compiler_name;

fs::path filename;

void usage()
{
	fmt::print(stderr, "usage: {} <source-code>\n", compiler_name.c_str());
}

void ensure(bool b, auto message)
{
	if (b) return;
	fmt::print(stderr, "{}: error: {}\n", compiler_name.c_str(), message);
	std::exit(1);
}

enum class Keyword_Kind
{
	Function,

	Enum_Last = Function
};

struct Keyword_Description {
	Keyword_Kind kind;
	std::string_view as_string;
};

constexpr Keyword_Description Keywords_Description[] = {
	{ Keyword_Kind::Function, "fun" }
};

static_assert(std::size(Keywords_Description) == 1u + (size_t)Keyword_Kind::Enum_Last,
	"Exhaustive keyword to string matching");

enum class Instrinsic_Kind
{
	Syscall
};

struct Intrinsic_Description
{
	Instrinsic_Kind kind;
	std::string_view as_string;
};


constexpr Intrinsic_Description Intrinsic_Description[] = {
	{ Instrinsic_Kind::Syscall, "syscall" }
};

static_assert(std::size(Keywords_Description) == 1u + (size_t)Keyword_Kind::Enum_Last,
	"Exhaustive keyword to string matching");

struct Token
{
	enum class Kind
	{
		Identifier,
		Open_Paren, // (
		Close_Paren, // )

		Open_Block, // {
		Close_Block, // }

		Keyword,
		Integer,
		Intrinsic,

		Comma, // ,
	};

	Kind kind;
	uint64_t ival;
	std::string_view sval;
	Keyword_Kind keyword;
	Instrinsic_Kind intrinsic;
};

std::vector<Token> lex(std::string_view source)
{
	std::vector<Token> tokens;

	while (!source.empty()) {
		while (!source.empty()) {
			if (auto after_ws = source.find_first_not_of(" \n\r\t\v\f"); after_ws != 0) {
				if (after_ws != std::string_view::npos)
					source.remove_prefix(after_ws);
				else
					source = {};
				continue;
			}

			if (source.starts_with("//") || source.starts_with("#!")) {
				if (auto eol = source.find_first_of('\n'); eol != std::string_view::npos)
					source.remove_prefix(eol);
				else
					source = {};
				continue;
			}

			break;
		}

		if (source.empty())
			break;

		auto &token = tokens.emplace_back();

		for (auto keyword : Keywords_Description) {
			if (source.starts_with(keyword.as_string)) {
				token.kind = Token::Kind::Keyword;
				token.keyword = keyword.kind;
				source.remove_prefix(keyword.as_string.size());
				goto lex_next_keyword;
			}
		}

		if (source.starts_with('#')) {
			source.remove_prefix(1);
			for (auto intr : Intrinsic_Description) {
				if (source.starts_with(intr.as_string)) {
					token.intrinsic = intr.kind;
					token.kind = Token::Kind::Intrinsic;
					source.remove_prefix(intr.as_string.size());
					goto lex_next_keyword;
				}
			}

			auto res = std::find_if_not(source.begin(), source.end(), [](auto ch) { return std::isalpha(ch); });
			source = { source.begin(), res };
			ensure(false, "Unknown intrinsic `#{}`"_format(source));
		}

		if (auto after_num = std::find_if_not(source.begin(), source.end(), [](auto ch) { return std::isdigit(ch); }); after_num != source.begin()) {
			token.kind = Token::Kind::Integer;
			auto [p, e] = std::from_chars(source.data(), source.data() + std::distance(source.begin(), after_num), token.ival);
			source.remove_prefix(p - source.data());
			goto lex_next_keyword;
		}

		if (auto id = std::find_if_not(source.begin(), source.end(),
					[first = true](auto ch) mutable {
						return std::isalpha(ch) || ch == '_' || (!std::exchange(first, false) && std::isdigit(ch)); });
				id != source.begin()
		) {
			token.kind = Token::Kind::Identifier;
			auto end = std::distance(source.begin(), id);
			token.sval = source.substr(0, end);
			source.remove_prefix(end);
			goto lex_next_keyword;
		}

		switch (source.front()) {
		case '{': source.remove_prefix(1); token.kind = Token::Kind::Open_Block;  goto lex_next_keyword;
		case '}': source.remove_prefix(1); token.kind = Token::Kind::Close_Block; goto lex_next_keyword;
		case '(': source.remove_prefix(1); token.kind = Token::Kind::Open_Paren;  goto lex_next_keyword;
		case ')': source.remove_prefix(1); token.kind = Token::Kind::Close_Paren; goto lex_next_keyword;
		case ',': source.remove_prefix(1); token.kind = Token::Kind::Comma;       goto lex_next_keyword;
		}

		ensure(false, "Unexpected sequence: {}"_format(std::quoted(source)));
lex_next_keyword:
		;
	}

	return tokens;
}

struct Ast
{
	virtual ~Ast() = default;
	virtual void dump(size_t indent) = 0;
};

struct Ast_Function final : Ast
{
	~Ast_Function() override = default;

	void dump(size_t indent) override
	{
		fmt::print("{}Function {}\n", std::string(indent, ' '), name);
		body->dump(indent + 2);
	}

	std::string_view name;
	Ast *body = nullptr;
};

struct Ast_Value final : Ast
{
	~Ast_Value() override = default;

	void dump(size_t indent) override
	{
		fmt::print("{}Value {}\n", std::string(indent, ' '), ival);
	}

	uint64_t ival;
};

struct Ast_Block final : Ast, std::vector<Ast*>
{
	~Ast_Block() override = default;

	void dump(size_t indent) override
	{
		fmt::print("{}{{\n", std::string(indent, ' '));
		for (auto ast : *this)
			ast->dump(indent+2);
		fmt::print("{}}}\n", std::string(indent, ' '));
	}
};

struct Ast_Call_Params final : Ast, std::vector<Ast*>
{
	~Ast_Call_Params() override = default;

	void dump(size_t indent) override
	{
		fmt::print("{}(\n", std::string(indent, ' '));
		for (auto ast : *this)
			ast->dump(indent+2);
		fmt::print("{})\n", std::string(indent, ' '));
	}
};

struct Ast_Intrinsic final : Ast
{
	~Ast_Intrinsic() override = default;

	Instrinsic_Kind kind;
	Ast *body = nullptr;

	void dump(size_t indent) override
	{
		fmt::print("{}", std::string(indent, ' '));
		switch (kind) {
		case Instrinsic_Kind::Syscall: fmt::print("syscall\n"); body->dump(indent+2); break;
		}
	}

	auto& params()
	{
		assert(body);
		return *(Ast_Call_Params*)body;
	}
};

struct Parse_Result
{
	Ast *ast = nullptr;
	std::span<Token> remaining{};
	std::string error{};

	static Parse_Result success(Ast *ast, std::span<Token> remaining)
	{
		return Parse_Result { ast, remaining };
	}

	static Parse_Result failure(std::span<Token> remaining, std::string error)
	{
		return Parse_Result { nullptr, remaining, std::move(error) };
	}

	operator bool() const
	{
		return ast != nullptr;
	}

	Parse_Result wrap(auto callable) &&
	{
		if (ast)
			ast = callable(ast);
		return *this;
	}
};

bool expect(std::span<Token> tokens, Token::Kind kind)
{
	return !tokens.empty() && tokens.front().kind == kind;
}

bool expect(std::span<Token> tokens, Keyword_Kind kind)
{
	return !tokens.empty() && tokens.front().kind == Token::Kind::Keyword && tokens.front().keyword == kind;
}

std::optional<Token> consume(std::span<Token> &tokens, auto ...args)
{
	if (expect(tokens, args...)) {
		auto token = tokens.front();
		tokens = tokens.subspan(1);
		return token;
	}
	return {};
}

Parse_Result parse_value(std::span<Token> tokens)
{
	auto maybe_int = consume(tokens, Token::Kind::Integer);
	if (maybe_int) {
		auto val = new Ast_Value;
		val->ival = maybe_int->ival;
		return Parse_Result::success(val, tokens);
	}

	return Parse_Result::failure(tokens, "Expected integer value");
}

Parse_Result parse_call_params(std::span<Token> tokens)
{
	if (!consume(tokens, Token::Kind::Open_Paren))
		return Parse_Result::failure(tokens, "Call expression parameters must begin with (");

	auto params = new Ast_Call_Params;

	for (;;) {
		if (auto value = parse_value(tokens); value) {
			params->push_back(value.ast);
			tokens = value.remaining;
			if (consume(tokens, Token::Kind::Comma))
				continue;
			else if (consume(tokens, Token::Kind::Close_Paren))
				break;
			else
				return Parse_Result::failure(tokens, "Expected either , or )");
		} else if (consume(tokens, Token::Kind::Close_Paren)) {
			break;
		} else {
			return value;
		}
	}

	return Parse_Result::success(params, tokens);
}

Parse_Result parse_intrinsic(std::span<Token> tokens)
{
	auto intr = consume(tokens, Token::Kind::Intrinsic);
	if (!intr)
		return Parse_Result::failure(tokens, "Intrinsic expression must begin with intrinsic token");

	switch (intr->intrinsic) {
	case Instrinsic_Kind::Syscall:
		return parse_call_params(tokens).wrap([](Ast *ast){
			auto p = new Ast_Intrinsic;
			p->body = ast;
			p->kind = Instrinsic_Kind::Syscall;
			return p;
		});

	default:
		assert(false && "Parsing this intrinsic kind unimplemented yet");
	}
}

Parse_Result parse_block(std::span<Token> tokens)
{
	if (!consume(tokens, Token::Kind::Open_Block))
		return Parse_Result::failure(tokens, "Block must begin with {");

	auto block = parse_intrinsic(tokens).wrap([](Ast *ast) {
		auto block = new Ast_Block;
		block->push_back(ast);
		return block;
	});

	if (!consume(block.remaining, Token::Kind::Close_Block))
		return Parse_Result::failure(tokens, "Block must end with }");

	return block;
}

Parse_Result parse_function(std::span<Token> tokens)
{
	if (!consume(tokens, Keyword_Kind::Function))
		return Parse_Result::failure(tokens, "Function must begin with fun keyword");

	auto maybe_identifier = consume(tokens, Token::Kind::Identifier);
	if (!maybe_identifier)
		return Parse_Result::failure(tokens, "`fun` must be followed by an indentifier");

	return parse_block(tokens).wrap([&](Ast *ast) {
		auto f = new Ast_Function;
		f->name = maybe_identifier->sval;
		f->body = ast;
		return f;
	});
}

struct Operation
{
	enum class Kind
	{
		Syscall,
		Push_Int
	};

	static Operation push_int(unsigned ival) { Operation op; op.kind = Kind::Push_Int; op.ival = ival; return op; }
	static Operation syscall(unsigned params_count) { Operation op; op.kind = Kind::Syscall; op.syscall_params_count = params_count; return op; }

	Kind kind;
	uint64_t ival;
	unsigned syscall_params_count;
};

std::vector<Operation> to_intermidiate(Ast *ast)
{
	if (ast == nullptr)
		return {};

	if (auto func = dynamic_cast<Ast_Function*>(ast)) {
		ensure(func->name == "main", "Only main function is currently supported");
		return to_intermidiate(func->body);
	}

	if (auto block = dynamic_cast<Ast_Block*>(ast)) {
		std::vector<Operation> flat;
		for (auto stmt : *block) {
			auto ops = to_intermidiate(stmt);
			std::move(ops.begin(), ops.end(), std::back_inserter(flat));
		}
		return flat;
	}

	if (auto intr = dynamic_cast<Ast_Intrinsic*>(ast)) {
		switch (intr->kind) {
		case Instrinsic_Kind::Syscall:
			auto params = to_intermidiate(intr->body);
			params.push_back(Operation::syscall(intr->params().size()-1));
			return params;
		}
	}

	if (auto params = dynamic_cast<Ast_Call_Params*>(ast)) {
		std::vector<Operation> flat;
		for (auto stmt : *params) {
			auto ops = to_intermidiate(stmt);
			std::move(ops.begin(), ops.end(), std::back_inserter(flat));
		}
		return flat;
	}

	if (auto value = dynamic_cast<Ast_Value*>(ast)) {
		return { Operation::push_int(value->ival) };
	}

	assert(false && "unimplemented to intermidiate conversion");
}

void compile_linux_x86_64(std::ostream &out, std::span<Operation> ops)
{
	fmt::print(out, "BITS 64\n");
	fmt::print(out, "segment .text\n");
	fmt::print(out, "global _start\n");
	fmt::print(out, "_start:\n");

	for (auto op : ops) {
		switch (op.kind) {
		case Operation::Kind::Push_Int:
			fmt::print(out, "  mov rax, {}\n", op.ival);
			fmt::print(out, "  push rax\n");
			break;

		case Operation::Kind::Syscall:
			{
				static char const* regs[] = { "rax", "rdi", "rsi", "rdx", "r10", "r8", "r9" };
				for (unsigned i = op.syscall_params_count; i <= op.syscall_params_count; --i)
					fmt::print(out, "  pop {}\n", regs[i]);
				fmt::print(out, "  syscall\n");
				fmt::print(out, "  push rax\n");
			}
			break;
		}
	}

	fmt::print(out, "  mov rax, 60\n");
	fmt::print(out, "  mov rdi, 0\n");
	fmt::print(out, "  syscall\n");
}

int main(int, char **argv)
{
	bool print_tokens = false;
	bool print_ast = false;

	assert(*argv);
	compiler_name = fs::path(*argv++).filename();

	if (*argv == nullptr)
		usage();

	for (; *argv; ++argv) {
		std::string_view arg{*argv};

		if (arg.starts_with('-')) {
			arg.remove_prefix(1);
			if (arg.starts_with("ast")) { print_ast = true; continue; }
			if (arg.starts_with("tokens")) { print_tokens = true; continue; }

			ensure(false, "Unrecognized command line option: {}"_format(arg));
		}

		ensure(filename.empty(), "Only one filename can be specified");
		filename = arg;
	}

	ensure(!filename.empty(), "Source file was not specified");

	std::ifstream source_file(filename);
	ensure(bool(source_file), "Couldn't open source file {}"_format(filename));

	std::string source{std::istreambuf_iterator<char>(source_file), {}};

	auto tokens = lex(source);

	if (print_tokens) {
		for (auto token : tokens) {
			fmt::print("TOK ");
			switch (token.kind) {
			case Token::Kind::Open_Block:  fmt::print("{{"); break;
			case Token::Kind::Close_Block: fmt::print("}}"); break;
			case Token::Kind::Open_Paren:  fmt::print("("); break;
			case Token::Kind::Close_Paren: fmt::print(")"); break;
			case Token::Kind::Comma:       fmt::print(","); break;

			case Token::Kind::Identifier: fmt::print("Identifier {}", std::quoted(token.sval)); break;
			case Token::Kind::Integer: fmt::print("Int {}", token.ival); break;
			case Token::Kind::Intrinsic:
				{
					for (auto intr : Intrinsic_Description) {
						if (intr.kind == token.intrinsic) {
							fmt::print("Intrinsic {}", intr.as_string);
							break;
						}
					}
				}
				break;
			case Token::Kind::Keyword:
				{
					for (auto keyword : Keywords_Description) {
						if (keyword.kind == token.keyword) {
							fmt::print("Keyword {}", keyword.as_string);
							break;
						}
					}
				}
				break;
			default:
				fmt::print("Unknown token kind\n");
			}
			fmt::print("\n");
		}
	}

	auto parse_result = parse_function({ tokens.data(), tokens.size() });

	if (!parse_result) {
		ensure(false, parse_result.error);
	}

	if (print_ast) {
		parse_result.ast->dump(0);
	}

	auto intermidiate = to_intermidiate(parse_result.ast);

	fs::path asm_path = filename;
	asm_path = asm_path.replace_extension("asm");

	fs::path object_path = filename;
	object_path = object_path.replace_extension("o");

	fs::path executable_path = filename;
	executable_path = executable_path.replace_extension();

	{
		std::ofstream asm_file(asm_path);
		compile_linux_x86_64(asm_file, intermidiate);
	}

	if (os_exec::run("nasm", "-felf64", asm_path)) return 1;
	if (os_exec::run("ld", "-o", executable_path, object_path)) return 1;
}
