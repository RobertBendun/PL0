#include <algorithm>
#include <cassert>
#include <charconv>
#include <concepts>
#include <filesystem>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <iterator>
#include <list>
#include <map>
#include <optional>
#include <span>
#include <variant>

#include <fmt/format.h>
#include <fmt/ostream.h>

#include "./os-exec/os-exec.hh"

// Hack for using std::source_location
#ifndef __cpp_lib_source_location
#include <experimental/source_location>
namespace std { using std::experimental::source_location; }
#else
#include <source_location>
#endif

using namespace fmt::literals;
using namespace std::string_view_literals;
using namespace std::string_literals;

namespace fs = std::filesystem;

fs::path compiler_name;
fs::path filename;
bool report_error_raise_location = false;
bool dont_produce_executable = false;

void usage()
{
	fmt::print(stderr, "usage: {} [options] <source-code>\n", compiler_name.c_str());
	fmt::print(stderr, "  where options are:\n");
	fmt::print(stderr, "    --graph-ir <dot-file> Writes to <dot-file> graph of IR\n");
	fmt::print(stderr, "    --print-tokens        Prints tokens of whole program\n");
	fmt::print(stderr, "    --print-ast           Prints AST of whole program\n");
	fmt::print(stderr, "    --print-ir            Prints generated Intermidiate representation\n");
	fmt::print(stderr, "    --where-error         Print where (in compiler) error was generated\n");
	fmt::print(stderr, "    --no-executable       Do not produce assembly and final executable\n");
	std::exit(1);
}

struct Position
{
	std::string_view file = "<unknown>";
	unsigned line = 1, column = 1, offset = 0;

	void next_line(unsigned n, unsigned diff) { line += n; column = 1; offset += diff; }

	// Conform to the output_iterator interface for ease of moving pointed position
	auto& operator*() { return *this; }
	auto& operator++() { return *this; }
	void operator=(char c) { switch (c) { case '\n': ++line; [[fallthrough]]; case '\r': column = 1; break; default: ++column; }; ++offset; }
	void operator+=(char c) { *this = c; }
};

template<> struct fmt::formatter<Position> : fmt::formatter<std::string_view>
{
	auto format(Position const& pos, auto &ctx) -> decltype(ctx.out())
	{
		return fmt::format_to(ctx.out(), "{}:{}:{}", pos.file, pos.line, pos.column);
	}
};

namespace report
{
	inline void report_location(std::source_location loc)
	{
		if (!report_error_raise_location) return;
		fmt::print(stderr, "[INFO] Error function called at: {}:{}:{}\n", loc.file_name(), loc.line(), loc.column());
	}

	void ensure(bool b, auto message, std::source_location loc = std::source_location::current())
	{
		if (b) return;
		report_location(loc);
		fmt::print(stderr, "{}: error: {}\n", compiler_name.c_str(), message);
		std::exit(1);
	}

	void ensure(bool b, Position const& pos, auto const& message, std::source_location loc = std::source_location::current())
	{
		if (b) return;
		report_location(loc);
		fmt::print(stderr, "{}: error: {}\n", pos, message);
		std::exit(1);
	}

	[[noreturn]]
	inline void fatal(std::string type, std::string why, std::source_location loc = std::source_location::current())
	{
		fmt::print(stderr, "{}:{}:{}: {}", loc.file_name(), loc.line(), loc.column(), type);
		if (!why.empty()) {
			fmt::print(stderr, ": {}\n", why);
		} else {
			fmt::print(stderr, "\n");
		}
		std::exit(1);
	}

	[[noreturn]]
	void unimplemented(std::string why = {}, std::source_location loc = std::source_location::current())
	{
		fatal("unimplemented", why, loc);
	}

	[[noreturn]]
	void unreachable(std::string why = {}, std::source_location loc = std::source_location::current())
	{
		fatal("unreachable", why, loc);
	}

	template<typename T>
	requires requires (T const& t) { { t.pos } -> std::convertible_to<Position>; }
	void ensure(bool b, T const& with_pos_field, auto const& message, std::source_location loc = std::source_location::current())
	{
		ensure(b, with_pos_field.pos, message, loc);
	}

	void error(auto const& p1,                 std::source_location loc = std::source_location::current()) { ensure(false, p1,     loc); }
	void error(auto const& p1, auto const& p2, std::source_location loc = std::source_location::current()) { ensure(false, p1, p2, loc); }
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

std::string_view keyword_name(Keyword_Kind kind)
{
	return std::find_if(Keywords_Description, std::end(Keywords_Description), [=](auto const& x) { return x.kind == kind; })->as_string;
}

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
	Position pos;
	uint64_t ival;
	std::string_view sval;
	Keyword_Kind keyword;
	Instrinsic_Kind intrinsic;

#define Member_Compare(Type, Kind, Member) \
	friend std::partial_ordering operator<=>(Token const& t, Type v) { \
		if (t.kind == Kind) return t.Member <=> v; \
		return std::partial_ordering::unordered; } \
	friend auto operator<=>(Type v, Token const& t) { return t <=> v; }

	Member_Compare(Instrinsic_Kind, Token::Kind::Intrinsic, intrinsic)
	Member_Compare(Keyword_Kind, Token::Kind::Keyword, keyword)

#undef Member_Compare
};

void dump(std::span<Token> tokens)
{
	for (auto const& token : tokens) {
		fmt::print("{}:{}:{}: ", token.pos.file, token.pos.line, token.pos.column);
		switch (token.kind) {
		case Token::Kind::Identifier:  fmt::print("ID {}\n", std::quoted(token.sval)); break;
		case Token::Kind::Integer:     fmt::print("INT {}\n", token.ival); break;

		case Token::Kind::Intrinsic:   fmt::print("INTR {}\n", intrinsic_name(token.intrinsic)); break;
		case Token::Kind::Keyword:     fmt::print("KEYW {}\n",  keyword_name(token.keyword)); break;

		case Token::Kind::Comma:       fmt::print(",\n");  break;
		case Token::Kind::Close_Paren: fmt::print(")\n");  break;
		case Token::Kind::Open_Paren:  fmt::print("(\n");  break;
		case Token::Kind::Close_Block: fmt::print("{{\n"); break;
		case Token::Kind::Open_Block:  fmt::print("}}\n"); break;
		}
	}
}

std::vector<Token> lex(std::string_view source, std::string_view path)
{
	std::vector<Token> tokens;
	Position pos;
	pos.file = path;

	while (!source.empty()) {
		while (!source.empty()) {
			if (auto after_ws = source.find_first_not_of(" \n\r\t\v\f"); after_ws != 0) {
				if (after_ws != std::string_view::npos) {
					pos = std::copy(source.begin(), source.begin() + after_ws, pos);
					source.remove_prefix(after_ws);
				} else
					source = {};
				continue;
			}

			if (source.starts_with("//") || source.starts_with("#!")) {
				if (auto eol = source.find_first_of('\n'); eol != std::string_view::npos) {
					pos.next_line(1, eol);
					source.remove_prefix(eol);
				} else {
					source = {};
				}
				continue;
			}

			break;
		}

		if (source.empty())
			break;

		auto start_source = source;
		auto &token = tokens.emplace_back();
		token.pos = pos;

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

			auto res = std::find_if_not(source.begin(), source.end(), [](auto ch) { return std::isalpha(ch) || ch == '_'; });
			source = { source.begin(), res };
			report::error(pos, fmt::format("Unknown intrinsic `#{}`", source));
		}

		// TODO Add hex (0x), octal (0o) and binary (Ob) literals.
		// Needed for more expressive definition of some Posix constants like permissions
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

		report::error(pos, fmt::format("Unexpected sequence: {}", std::quoted(source)));
lex_next_keyword:
		pos = std::copy(start_source.data(), source.data(), pos);
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
		Function_Call, // sval(p0, p1, ...)
		Intrinsic
	};

	Kind kind = Kind::Empty;
	Position pos{};
	uint64_t ival{};
	Instrinsic_Kind intrinsic{};
	std::string sval;
	std::list<Expression> params{};

	template<typename ...T>
	Expression(Kind k, T&& ...args) : kind(k), params{std::forward<T>(args)...} {}

	Expression() : kind(Kind::Empty) {}
	Expression(uint64_t val) : kind(Kind::Integer), ival(val) {}
	Expression(Instrinsic_Kind intr) : kind(Kind::Intrinsic), intrinsic(intr) {}
	Expression(Instrinsic_Kind intr, std::list<Expression> params) : kind(Kind::Intrinsic), intrinsic(intr), params{std::move(params)} {}

	decltype(auto) operator+(Position pos) && { this->pos = pos; return std::move(*this); }

	void dump(std::ostream &out, unsigned indent = 0) const
	{
		auto const dump_params = [&] { for (auto const &p : params) p.dump(out, indent+2); };

		fmt::print("{}", std::string(indent, ' '));
		switch (kind) {
		case Kind::Empty:         fmt::print(out, "NOP\n");                break;
		case Kind::Integer:       fmt::print(out, "INT {}\n", ival);       break;
		case Kind::Function_Call: fmt::print(out, "FCALL {}\n", sval);                          dump_params(); break;
		case Kind::Intrinsic:     fmt::print(out, "INTRINSIC {}\n", intrinsic_name(intrinsic)); dump_params(); break;
		case Kind::Sequence:      fmt::print(out, "SEQ\n");                                     dump_params(); break;
		}
	}
};

// AST Node which represents definition of functions
struct Function
{
	std::string name;
	Expression body;
	Position pos;
};

struct Parser
{
	std::vector<Function> all_functions{};
	std::span<Token> all_tokens{}; // Used primarly for error reporting

	Parser(std::span<Token> all_tokens, fs::path filename) : all_tokens{all_tokens}
	{
		// TODO Empty file is not allowed for getting better error messages. Alternative (and probably better) solution might be
		// remembering where lexer finished, and keep it as end of file mark. Due to current error handling structure, changing
		// this will only require changes in Parser::failure member function.
		report::ensure(!all_tokens.empty(), fmt::format("File {} is empty. Empty source files are not allowed.", filename));
	}

	struct Result
	{
		Expression expr{};
		std::span<Token> remaining{};
		std::string error{};
		Position error_pos{};
		std::source_location error_rise_location{};

		operator bool() const { return error.empty(); }
		Result then(auto callable) && { if (*this) return success(callable(std::move(expr)), remaining); return *this; }
	};

	static Result success(Expression e, std::span<Token> remaining) { return { std::move(e), remaining }; }
	static Result failure(std::span<Token> remaining, Position pos, std::string error, std::source_location loc = std::source_location::current())
	{
		return { {}, remaining, std::move(error), std::move(pos), loc };
	}

	Result failure(std::span<Token> remaining, unsigned pos, std::string error, std::string error_eof, std::source_location loc = std::source_location::current())
	{
		if (pos >= remaining.size())
			return failure(remaining, all_tokens.back().pos, std::move(error_eof), loc);
		return failure(remaining, remaining[pos].pos, std::move(error), loc);
	}

	static bool expect(std::span<Token> tokens, Keyword_Kind kind) { return !tokens.empty() && tokens.front() <=> kind == 0; }
	static bool expect(std::span<Token> tokens, Token::Kind kind)  { return !tokens.empty() && tokens.front().kind == kind; }

	static std::optional<Token> consume(std::span<Token> &tokens, auto ...args)
	{
		if (expect(tokens, args...)) return std::exchange(tokens, tokens.subspan(1)).front();
		return {};
	}

	Result parse_value(std::span<Token> tokens)
	{
		auto maybe_int = consume(tokens, Token::Kind::Integer);
		return maybe_int
			? success(Expression(maybe_int->ival) + maybe_int->pos, tokens)
			: failure(tokens, 0, "Expected integer value", "Expected integer value after this point, got end of file");
	}

	Result parse_call_params(std::span<Token> tokens)
	{
		if (!consume(tokens, Token::Kind::Open_Paren))
				return failure(tokens, 0, "Call expression parameters must begin with (", "Expected ( after this point, got an end of file");

		Expression p;
		for (;;) {
			auto value = parse_expression(tokens);
			if (value) {
				p.params.push_back(value.expr);
				tokens = value.remaining;
				if (consume(tokens, Token::Kind::Comma)) continue; // We loop, beacuse we need to parse another argument after comma
				if (consume(tokens, Token::Kind::Close_Paren)) break; // We break, beacuse ) marks end of argument list

				return failure(tokens, 0, "Expected either , or )", "Expected either , or ) after this point, got  end of file");
			}
			// If we cannot parse any more expressions, and we have close paren we know that this is and of call
			// TODO This case probably is only hit, when we have ill-formed or empty parameter list like () or (foo, bar,) <- unnesesary comma
			// Maybe we allow this, to make code like this idiomatic:
			// foo(
			//   bar,
			//   car,
			// )
			if (consume(tokens, Token::Kind::Close_Paren))
				break;
			else
				return value; // Value is in failure state here
		}
		return p.params.empty()
			? success(std::move(p), tokens)
			: success(std::move(p) + p.params.front().pos, tokens);
	}

	auto parse_intrinsic(std::span<Token> tokens)
	{
		auto intr = consume(tokens, Token::Kind::Intrinsic);
		if (!intr)
			return failure(tokens, 0, "Intrinsic expression must begin with intrinsic token", "Expected intrinsic expression after this point, got end of file");

		switch (intr->intrinsic) {
		case Instrinsic_Kind::Syscall:
			return parse_call_params(tokens).then([&](Expression expr) {
					report::ensure(expr.params.size() >= 1, intr->pos, "#syscall requires at least one parameter (syscall number)");
				return Expression(Instrinsic_Kind::Syscall, expr.params) + intr->pos;
			});

		default:
			report::unimplemented(fmt::format("Parsing of intrinsic #{}", intrinsic_name(intr->intrinsic)));
		}
	}

	Result parse_block(std::span<Token> tokens)
	{
		auto const maybe_open_block = consume(tokens, Token::Kind::Open_Block);
		if (!maybe_open_block)
			return failure(tokens, 0, "Block must begin with {", "Expected block after this point, got end of file");

		auto block = parse_expression(tokens).then([&](Expression expr) {
			return Expression(Expression::Kind::Sequence, expr) + maybe_open_block->pos;
		});

		if (!consume(block.remaining, Token::Kind::Close_Block))
			return failure(block.remaining, 0, "Block must end with }", "Expected block end after this point, got end of file");
		return block;
	}

	Result parse_function(std::span<Token> tokens)
	{
		auto maybe_function_keyword = consume(tokens, Keyword_Kind::Function);
		if (!maybe_function_keyword)
				return failure(tokens, 0, "Function must begin with fun keyword", "Expected function after this point, got end of file");

		auto maybe_identifier = consume(tokens, Token::Kind::Identifier);
		if (!maybe_identifier)
			return failure(tokens, maybe_function_keyword->pos, "`fun` must be followed by an indentifier");

		return parse_expression(tokens).then([&](Expression body) {
			all_functions.push_back(Function { std::string(maybe_identifier->sval), std::move(body), maybe_function_keyword->pos });
			return Expression{} + maybe_function_keyword->pos;
		});
	}


	Result parse_function_call(std::span<Token> tokens)
	{
		auto maybe_identifier = consume(tokens, Token::Kind::Identifier);
		if (!maybe_identifier)
			return failure(tokens, 0, "Function call must begin with an identifier", "Expected identifier after this point, got end of file");

		auto result = parse_call_params(tokens).then([&](Expression params) {
			Expression funcall;
			funcall.kind = Expression::Kind::Function_Call;
			funcall.sval = maybe_identifier->sval;
			funcall.params = params.params;
			return std::move(funcall) + maybe_identifier->pos;
		});

		report::ensure(result, result.error_pos, result.error);
		return result;
	}

	Result parse_expression(std::span<Token> tokens)
	{
		std::function<Parser::Result(Parser&, std::span<Token>)> funcs[] = {
			&Parser::parse_function_call,
			&Parser::parse_intrinsic,
			&Parser::parse_block,
			&Parser::parse_value,
			&Parser::parse_function
		};

		for (auto func : funcs) {
			if (auto result = func(*this, tokens); result)
				return result;
		}

		return failure(tokens, 0, "Expected expression", "Expected expression after this point, got end of file");
	}

	// TODO This function will not return any Expression, so it's result should not contain expression
	Result parse_program(std::span<Token> tokens)
	{
		while (!tokens.empty()) {
			auto func = parse_function(tokens);
			if (!func) {
				return func;
			}
			tokens = func.remaining;
		}
		return {};
	}

	void dump(std::ostream &out)
	{
		for (auto const& function : all_functions) {
			fmt::print(out, "FUNCTION {} {}\n", function.name, function.pos);
			function.body.dump(out);
			fmt::print(out, "END FUNCTION {}\n", function.name);
		}
	}
};

struct Statement
{
	using Reg = uint64_t;
	static constexpr auto Empty = std::numeric_limits<Reg>::max();

	enum class Kind
	{
		Nop,
		Copy,     // Copy into IR variable other variable or initialize with value
		Syscall,  // p0 = syscall(syscall_number = p1, arg1 = p2, arg2 = p3, ...)
		Call,     // name(p0, p1, ...)
	};

	Kind kind;
	uint64_t value;                    // value for copy statement
	std::vector<Reg> params{};         // IR variables referenced by statement
	Statement *next = nullptr;         // aka true branch
	Statement *false_branch = nullptr; // aka false branch
	std::string name{};

	std::string string() const
	{
		switch (kind) {
		case Statement::Kind::Nop:
			return "NOP";

		case Statement::Kind::Call:
			return fmt::format("CALL {}", std::quoted(name));

		case Statement::Kind::Copy:
			assert(params.size() == 1 || params.size() == 2);
			return params.size() == 1
				? fmt::format("COPY ${}, {}", params[0], value)
				: fmt::format("COPY ${}, ${}", params[0], params[1]);

		case Statement::Kind::Syscall:
			assert(params.size() > 2);
			auto str = fmt::format("SYSCALL RET=${} ID=${} ", params[0], params[1]);
			if (params.size() > 2) str += "ARGS=";
			for (auto i = 2u; i < params.size(); ++i)
				str += fmt::format("${} ", params[i]);
			return str;
		}

		report::unreachable();
	}
};

struct IR_Compiler
{
	std::vector<std::unique_ptr<Statement>> all_statements;
	std::map<std::string, Statement*> entry_points;

	Statement* new_statement() { return all_statements.emplace_back(std::make_unique<Statement>()).get(); }

	std::string_view find_entry_point(Statement const* stmt) const
	{
		auto match = std::find_if(entry_points.begin(), entry_points.end(), [&](auto const& kv) { return kv.second == stmt; });
		return match != entry_points.end() ? std::string_view(match->first) : std::string_view{};
	}

	struct Context
	{
		Statement **target;       // place where to put next statement
		Statement::Reg counter{}; // counter for creation of new IR variables
	};

	// Compiles pice of Expression and returns variable if result of expression is needed
	Statement::Reg compile(Expression const& expr, Context &ctx)
	{
		// Make new IR variable
		auto make = [&]               { return ctx.counter++; };
		// Put statement at ctx.target and iterate target to the last statement
		auto put  = [&](Statement *s) { for (*ctx.target = s; s; s = *ctx.target) ctx.target = &s->next; };

		switch (expr.kind) {
		case Expression::Kind::Integer:
			{
				auto stmt = new_statement();
				stmt->kind = Statement::Kind::Copy;
				stmt->params.push_back(make());
				stmt->value = expr.ival;
				put(stmt);
				return stmt->params.front();
			}
			break;

		case Expression::Kind::Sequence:
			{
				auto result = Statement::Empty;
				// Evaluate each and only keep last value
				for (auto const& param : expr.params) result = compile(param, ctx);
				return result;
			}
			break;

		case Expression::Kind::Function_Call:
			{
				// TODO passing params is not implemented yet
				assert(expr.params.empty());
				auto stmt = new_statement();
				stmt->kind = Statement::Kind::Call;
				stmt->name = expr.sval;
				put(stmt);
				return Statement::Empty;
			}
			break;

		case Expression::Kind::Intrinsic:
			{
				switch (expr.intrinsic) {
				case Instrinsic_Kind::Syscall:
					{
						auto stmt = new_statement();
						stmt->kind = Statement::Kind::Syscall;
						stmt->params.push_back(make()); // Create return variable
						for (auto const& param : expr.params) stmt->params.push_back(compile(param, ctx));
						put(stmt);
						return stmt->params.front();
					}
					break;

				default:
					report::unimplemented(fmt::format("intrinsic `{}`", intrinsic_name(expr.intrinsic)));
				}
			}
			break;

		default:
			std::cout << "Unknown expression kind:\n";
			expr.dump(std::cerr);
			std::cout << std::flush;
			std::exit(1);
		}
	}

	void compile(Parser const& parser)
	{
		for (auto const& function : parser.all_functions) {
			Context ctx { &entry_points[function.name] };
			compile(function.body, ctx);
		}
	}

	// FIXME This algorithm produce misleading output. For now IR_Compiler::dump_dot is better then
	// IR_Compiler::dump() beacuse it prints statements in creation order, not in control flow order
	void dump(std::ostream& out) const
	{
		for (auto const& stmt_ptr : all_statements) {
			auto stmt = stmt_ptr.get();
			if (auto entry = find_entry_point(stmt); !entry.empty()) {
				fmt::print(out, "{}:\n", entry);
			}
			fmt::print(out, "  {}\n", stmt->string());
		}
	}

	void dump_dot(std::ostream &out) const
	{
		fmt::print(out, "digraph Program {{\n");
		for (auto const& stmt : all_statements) {
			fmt::print(out, "Node_{} [label={}];\n", (void*)stmt.get(), std::quoted(stmt->string()));
			if (stmt->next)
				fmt::print(out, "Node_{} -> Node_{};\n", (void*)stmt.get(), (void*)stmt->next);
		}
		fmt::print(out, "}}\n");
	}
};

template<typename T, typename R>
auto reduce(Statement const* s, T value, R reducer)
	-> decltype(reducer(std::move(value), *s))
{
	if (!s) return value;
	// TODO Support false branch
	assert(s->false_branch == nullptr);
	return reduce(s->next, reducer(std::move(value), *s), reducer);
}

namespace x86_64::linux::nasm
{
	void emit_function(std::ostream& out, std::string_view function_name, Statement *root)
	{
		fmt::print(out, "{}:\n", function_name);
		fmt::print(out, "  push rbp\n");
		fmt::print(out, "  mov rbp, rsp\n");

		auto max_var_id = reduce(root, 0u, [](auto v, Statement const& s) {
			auto max = std::max_element(s.params.begin(), s.params.end());
			return max != s.params.end() && *max > v ? *max : v;
		});

		// All values are allocated as 8 byte variables on the stack
		// The max ID of variable is the number of variables that are required for
		// given function
		// TODO: Maybe even put some in registers? x86_64 has plenty
		// TODO: Algorithm that will reduce variable names to reduce space allocation per function call

		fmt::print(out, "  sub rsp, {}\n", (max_var_id+1) * 8);

		for (auto s = root; s; s = s->next) {
			// TODO This dessign only supports linear execution
			assert(s->false_branch == nullptr);
			switch (s->kind) {
			case Statement::Kind::Nop:
				break;

			case Statement::Kind::Call:
				{
					assert(s->params.empty());
					fmt::print(out, "  call {}\n", s->name);
				}
				break;

			case Statement::Kind::Copy:
				{
					assert(!s->params.empty());
					auto target = s->params[0];
					if (s->params.size() == 1) {
						fmt::print(out, "  mov qword [rbp-{}], {}\n", 8 * target, s->value);
						continue;
					} else {
						for (auto i = 0u; i < s->params.size(); ++i) {
							fmt::print(out, "  mov rax, [rbp-{}]\n", 8 * s->params[i]);
							fmt::print(out, "  mov [rbp-{}], rax\n", 8 * target);
						}
					}
				}
				break;

			case Statement::Kind::Syscall:
				{
					static char const* regs[] = { "rax", "rdi", "rsi", "rdx", "r10", "r8", "r9" };
					assert(s->params.size()-1 < std::size(regs));

					for (auto i = 1u; i < s->params.size(); ++i) {
						fmt::print(out, "  mov {}, [rbp-{}]\n", regs[i-1], s->params[i] * 8);
					}

					fmt::print(out, "  syscall\n");
					fmt::print(out, "  mov [rbp-{}], rax\n", s->params[0] * 8);
				}
				break;
			}
		}

		fmt::print(out, "  leave\n");
		fmt::print(out, "  ret\n");
	}

	void emit_assembly(std::ostream &out, IR_Compiler const& ir)
	{
		fmt::print(out, "BITS 64\n");
		fmt::print(out, "segment .text\n");
		fmt::print(out, "extern _start\n");
		fmt::print(out, "_start:\n");
		fmt::print(out, "  call main\n");
		fmt::print(out, "  mov rax, 60\n");
		fmt::print(out, "  xor rdi, rdi\n");
		fmt::print(out, "  syscall\n\n");


		for (auto const& entry_point : ir.entry_points) {
			emit_function(out, entry_point.first, entry_point.second);
			fmt::print(out, "\n");
		}
	}
}

int main(int, char **argv)
{
	bool print_tokens = false;
	bool print_ast = false;
	bool print_ir = false;
	std::string_view graph_ir;

	assert(*argv);
	compiler_name = fs::path(*argv++).filename();

	if (*argv == nullptr)
		usage();

	for (; *argv; ++argv) {
		std::string_view arg{*argv};

		if (arg.starts_with('-')) {
			arg.remove_prefix(1);
			if (arg.starts_with('-')) arg.remove_prefix(1);

			if (arg == "help")           usage();
			if (arg == "print-tokens")   { print_tokens = true; continue; }
			if (arg == "print-ast")      { print_ast = true; continue; }
			if (arg == "print-ir")       { print_ir = true;  continue; }
			if (arg == "where-error")    { report_error_raise_location = true; continue; }
			if (arg == "no-executable")  { dont_produce_executable = true; continue; }

			if (arg == "graph-ir") {
				report::ensure(*++argv, "-graph-ir expects filename to put DOT graph");
				graph_ir = *argv;
				continue;
			}
			report::error(fmt::format("Unrecognized command line option: {}", arg));
		}

		report::ensure(filename.empty(), "Only one filename can be specified");
		filename = arg;
	}

	report::ensure(!filename.empty(), "Source file was not specified");

	std::ifstream source_file(filename);
	report::ensure(bool(source_file), fmt::format("Couldn't open source file {}", filename));

	std::string source{std::istreambuf_iterator<char>(source_file), {}};

	auto tokens = lex(source, filename.c_str());

	if (print_tokens) {
		fmt::print("--- TOKENS -------------------------------\n");
		dump(tokens);
	}

	Parser parser(tokens, filename);
	auto parse_result = parser.parse_program(tokens);

	report::ensure(parse_result, parse_result.error_pos, parse_result.error, parse_result.error_rise_location);

	if (print_ast) {
		fmt::print("--- AST DUMP -----------------------------\n");
		parser.dump(std::cout);
	}

	IR_Compiler compiler;
	compiler.compile(parser);

	if (print_ir) {
		fmt::print("--- INTERMIDIATE REPRESENTATION ----------\n");
		compiler.dump(std::cout);
	}

	if (!graph_ir.empty()) {
		if (graph_ir == "-") {
			compiler.dump_dot(std::cout);
		} else {
			std::ofstream file(graph_ir.data());
			compiler.dump_dot(file);
		}
	}

	if (dont_produce_executable)
		return 0;

	fs::path asm_path = filename; {
		asm_path = asm_path.replace_extension("asm");
		std::ofstream asm_file(asm_path);
		x86_64::linux::nasm::emit_assembly(asm_file, compiler);
	}

	fs::path obj_path = filename;
	obj_path = obj_path.replace_extension(".o");

	fs::path exe_path = filename;
	exe_path = exe_path.replace_extension();

	if (os_exec::run_echo("nasm", "-felf64", asm_path)) return 1;
	if (os_exec::run_echo("ld", obj_path, "-o", exe_path)) return 1;
}
