#include <algorithm>
#include <cassert>
#include <charconv>
#include <filesystem>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <iterator>
#include <list>
#include <optional>
#include <span>
#include <variant>

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

std::string_view intrinsic_name(Instrinsic_Kind kind)
{
	return std::find_if(Intrinsic_Description, std::end(Intrinsic_Description), [=](auto const& x) { return x.kind == kind; })->as_string;
}

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

// AST Node that describes anything computable (binary expressions, values, function calls)
struct Expression
{
	enum class Kind
	{
		Empty, Integer, // atoms
		Sequence, // blocks
		Function_Call, // p0(p1, p2, ...)
		Intrinsic
	};

	Kind kind;
	uint64_t ival;
	Instrinsic_Kind intrinsic;
	std::list<Expression> params;

	template<typename ...T>
	Expression(Kind k, T&& ...args) : kind(k), params{std::forward<T>(args)...} {}

	Expression() : kind(Kind::Empty) {}
	Expression(uint64_t val) : kind(Kind::Integer), ival(val) {}
	Expression(Instrinsic_Kind intr) : kind(Kind::Intrinsic), intrinsic(intr) {}
	Expression(Instrinsic_Kind intr, std::list<Expression> params) : kind(Kind::Intrinsic), intrinsic(intr), params{std::move(params)} {}

	void dump(unsigned indent = 0) const
	{
		auto const dump_params = [&] { for (auto const &p : params) p.dump(indent+2); };

		fmt::print("{}", std::string(indent, ' '));
		switch (kind) {
		case Kind::Empty:         fmt::print("NOP\n");                break;
		case Kind::Integer:       fmt::print("INT {}\n", ival);       break;
		case Kind::Function_Call: fmt::print("FCALL\n");                                   dump_params(); break;
		case Kind::Intrinsic:     fmt::print("INTRINSIC {}\n", intrinsic_name(intrinsic)); dump_params(); break;
		case Kind::Sequence:      fmt::print("SEQ\n");                                     dump_params(); break;
		}
	}
};

// AST Node which represents definition of functions
struct Function
{
	std::string name;
	Expression body;
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

struct Parser
{
	std::vector<Function> all_functions;

	struct Result
	{
		Expression expr;
		std::span<Token> remaining{};
		std::string error{};

		operator bool() const { return error.empty(); }
		Result then(auto callable) && { if (*this) return success(callable(std::move(expr)), remaining); return *this; }

	};

	static Result success(Expression e, std::span<Token> remaining) { return { std::move(e), remaining }; }
	static Result failure(std::span<Token> remaining, std::string error) { return { {}, remaining, std::move(error) }; }

	Parser::Result parse_value(std::span<Token> tokens)
	{
		auto maybe_int = consume(tokens, Token::Kind::Integer);
		return maybe_int ? success(Expression(maybe_int->ival), tokens) : failure(tokens, "Expected integer value");
	}

	Result parse_call_params(std::span<Token> tokens)
	{
		if (!consume(tokens, Token::Kind::Open_Paren))
			return failure(tokens, "Call expression parameters must begin with (");

		Expression p;

		for (;;) {
			if (auto value = parse_value(tokens); value) {
				p.params.push_back(value.expr);
				tokens = value.remaining;
				if (consume(tokens, Token::Kind::Comma))
					continue;
				else if (consume(tokens, Token::Kind::Close_Paren))
					break;
				else
					return failure(tokens, "Expected either , or )");
			} else if (consume(tokens, Token::Kind::Close_Paren)) {
				break;
			} else {
				return value;
			}
		}

		return success(p, tokens);
	}

	auto parse_intrinsic(std::span<Token> tokens)
	{
		auto intr = consume(tokens, Token::Kind::Intrinsic);
		if (!intr)
			return failure(tokens, "Intrinsic expression must begin with intrinsic token");

		switch (intr->intrinsic) {
		case Instrinsic_Kind::Syscall:
			return parse_call_params(tokens).then([](Expression expr) {
				return Expression(Instrinsic_Kind::Syscall, expr.params);
			});

		default:
			assert(false && "Parsing this intrinsic kind unimplemented yet");
		}
	}

	Result parse_block(std::span<Token> tokens)
	{
		if (!consume(tokens, Token::Kind::Open_Block))
			return failure(tokens, "Block must begin with {");

		auto block = parse_intrinsic(tokens).then([](Expression expr) {
			return Expression(Expression::Kind::Sequence, expr);
		});

		if (!consume(block.remaining, Token::Kind::Close_Block))
			return failure(tokens, "Block must end with }");

		return block;
	}

	Result parse_function(std::span<Token> tokens)
	{
		if (!consume(tokens, Keyword_Kind::Function))
			return failure(tokens, "Function must begin with fun keyword");

		auto maybe_identifier = consume(tokens, Token::Kind::Identifier);
		if (!maybe_identifier)
			return failure(tokens, "`fun` must be followed by an indentifier");

		return parse_block(tokens).then([&](Expression body) {
			all_functions.push_back(Function { std::string(maybe_identifier->sval), std::move(body) });
			return Expression{};
		});
	}

	void dump()
	{
		for (auto const& function : all_functions) {
			fmt::print("FUNCTION {}\n", function.name);
			function.body.dump();
			fmt::print("END FUNCTION {}\n", function.name);
		}
	}
};

int main(int, char **argv)
{
	bool print_ast = false;

	assert(*argv);
	compiler_name = fs::path(*argv++).filename();

	if (*argv == nullptr)
		usage();

	for (; *argv; ++argv) {
		std::string_view arg{*argv};

		if (arg.starts_with('-')) {
			arg.remove_prefix(1);
			if (arg == "ast") { print_ast = true; continue; }
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

	Parser parser;
	auto parse_result = parser.parse_function({ tokens.data(), tokens.size() });

	if (!parse_result) {
		ensure(false, parse_result.error);
	}

	if (print_ast)
		parser.dump();
}
