#include "pch.h"

#include <cctype>
#include <cstring>

#include "swift_writer.h"
#include "code_writers.h"
#include "common.h"

using namespace std::literals;
using namespace xlang;
using namespace xlang::meta::reader;
using namespace xlang::text;

void write_file_boilerplate(writer& w)
{
    w.write(R"^-^(
// Swift code gen beginning experimentation

import CWinRT

)^-^");
}

static void write_swift_definitions(writer& w, type_cache const& types)
{
    for (auto const& interfaceType : types.interfaces)
    {
        interfaceType.get().write_swift_definition(w);
    }
}

void write_swift_impl(std::string_view fileName, abi_configuration const& config, type_cache const& types)
{
    writer w{ config };

    write_file_boilerplate(w);

    write_swift_definitions(w, types);

    auto filename{ config.output_directory };
    filename += fileName;
    filename += ".swift";
    w.flush_to_file(filename);
}
