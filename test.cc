#include <iostream>
#include <filesystem>
#include <cassert>
#include <string_view>
#include <fstream>
#include <charconv>

#include <fmt/format.h>
#include <fmt/ostream.h>

#include "./os-exec/os-exec.hh"

namespace fs = std::filesystem;

void record(fs::path source)
{
	fmt::print("[TEST] Recording {}\n", source);

	if (os_exec::run("./pl0", source)) {
		fmt::print("[TEST] Failed during compilation of {}\n", source);
		std::exit(1);
	}

	auto exec = source;
	exec = exec.replace_extension();

	auto result = os_exec::run(exec);

	auto out_file = exec;
	out_file += ".txt";

	std::ofstream out{out_file};

	if (!result) {
		fmt::print(out, ":returns 0\n");
	} else if (result.category() == os_exec::non_zero_exit_code()) {
		fmt::print(out, ":returns {}\n", result.value());
	} else {
		fmt::print(stderr, "Failed during execution: {}", result.message());
		std::exit(1);
	}
}

std::string_view ltrim(std::string_view sv)
{
	if (auto after_ws = sv.find_first_not_of(" \n\r\t\v\f"); after_ws != 0) {
		if (after_ws != std::string_view::npos)
			sv.remove_prefix(after_ws);
		else
			sv = {};
	}
	return sv;
}

bool test(fs::path source)
{
	fmt::print("[TEST] Testing {}\n", source);

	if (os_exec::run("./pl0", source)) {
		fmt::print("[TEST] Failed during compilation of {}\n", source);
		return false;
	}

	auto exec = source;
	exec = exec.replace_extension();
	auto out_file = exec;
	out_file += ".txt";


	std::ifstream out{out_file};
	std::string spec_src{std::istreambuf_iterator<char>(out), {}};
	std::string_view spec = spec_src;

	int expected_exit_code = 0;

	for (;;) {
		spec = ltrim(spec);

		if (spec.starts_with(":returns")) {
			spec.remove_prefix(std::strlen(":returns"));
			spec = ltrim(spec);
			auto [end, error] = std::from_chars(spec.data(), spec.data() + spec.size(), expected_exit_code);
			spec.remove_prefix(end - spec.data());
			continue;
		}

		break;
	}

	auto result = os_exec::run(exec);

	int exit_code = !result ? 0 : result.value();
	if (exit_code != expected_exit_code) {
		fmt::print("[TEST] Failure in test {}. Expected exit code {}, got {}\n",
			source, expected_exit_code, exit_code);
		return false;
	}

	return true;
}

template<typename SC>
concept Sub_Cmd = requires (SC sc, fs::path path)
{
	{ sc.next(path) };
	{ sc.end() };
};

void subcommand(char **argv, std::string_view name, Sub_Cmd auto sc)
{
	if (*argv != name) return;

	while (*++argv) {
		fs::path arg{*argv};
		if (fs::is_directory(arg)) {
			for (auto entry : fs::recursive_directory_iterator(arg))
				if (auto path = entry.path(); path.extension() == ".pl0")
					sc.next(path);
		} else {
			sc.next(arg);
		}
	}

	sc.end();
	std::exit(0);
}

int main(int, char **argv)
{
	if (!*++argv) {
		fmt::print(stderr, "Expected subcommand\n");
		return 1;
	}

	{
		struct
		{
			unsigned success = 0, total = 0;
			void next(fs::path arg) { ++total; success += test(arg); }
			void end() {
				assert(total != 0 && "None tests were given");
				fmt::print("Passed {} / {} ({:.2f}%)\n", success, total, 100.f * success / total);
			}
		} spec;
		subcommand(argv, "run", spec);
	}

	{
		struct { void next(fs::path arg) { record(arg); } void end() {} } spec;
		subcommand(argv, "record", spec);
	}

	fmt::print(stderr, "Unrecognized subcommand: {}\n", *argv);
	return 1;
}
