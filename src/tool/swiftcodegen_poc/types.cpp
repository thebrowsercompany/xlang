#include "pch.h"

#include "swift_writer.h"
#include "code_writers.h"
#include "types.h"
#include "type_banners.h"

using namespace std::literals;
using namespace xlang::meta::reader;
using namespace xlang::text;

typedef_base::typedef_base(TypeDef const& type) :
    m_type(type),
    m_clrFullName(::clr_full_name(type)),
    m_mangledName(::mangled_name<false>(type)),
    m_genericParamMangledName(::mangled_name<true>(type)),
    m_contractHistory(get_contract_history(type))
{
    for_each_attribute(type, metadata_namespace, "VersionAttribute"sv, [&](bool, CustomAttribute const& attr)
    {
        m_platformVersions.push_back(decode_platform_version(attr));
    });
}

void typedef_base::write_swift_abi_name(writer& w) const
{
    write_mangled_name_macro(w, *this);
}

void enum_type::write_swift_generic_param_abi_type(writer& w) const
{
}

void enum_type::write_swift_abi_param(writer& w) const
{
    // Enums are passed by value
    write_swift_abi_name(w);
}

void enum_type::write_swift_definition(writer& w) const
{
}

void struct_type::write_swift_generic_param_abi_type(writer& w) const
{
}

void struct_type::write_swift_abi_param(writer& w) const
{
    // Structs are passed by value
    write_swift_abi_name(w);
}

void struct_type::write_swift_definition(writer& w) const
{
}

void delegate_type::write_swift_generic_param_abi_type(writer& w) const
{
}

void delegate_type::write_swift_abi_param(writer& w) const
{
    write_swift_abi_name(w);
    w.write('*');
}

static std::string_view function_name(MethodDef const& def)
{
    // If this is an overload, use the unique name
    auto fnName = def.Name();
    if (auto overloadAttr = get_attribute(def, metadata_namespace, "OverloadAttribute"sv))
    {
        auto sig = overloadAttr.Value();
        auto const& fixedArgs = sig.FixedArgs();
        XLANG_ASSERT(fixedArgs.size() == 1);
        fnName = std::get<std::string_view>(std::get<ElemSig>(fixedArgs[0].value).value);
    }

    return fnName;
}

template <typename T>
static void write_swift_function_thunk(writer& w, T const& type, function_def const& func)
{
    w.write(R"^-^(%return try perform(as: %.self) { pThis in
%try CHECKED(pThis.pointee.lpVtbl.pointee.%(pThis)^-^",
        indent{ 2 },
        bind_mangled_name_macro(type),
        indent{ 3 },
        function_name(func.def));

    std::string_view prefix = ", "sv;
    for (auto const& param : func.params)
    {
        w.write("%%", prefix, param.name);
    }
    if (func.return_type)
    {
        w.write("%%", prefix, func.return_type->name);
    }
    
    w.write("))\n");
    w.write("%}\n", indent{ 2 });
}

template <typename T>
static void write_swift_function_declaration(writer& w, T const& type, function_def const& func)
{
    std::string_view funcName = function_name(func.def);
    w.write("%public func %(", indent{ 1 }, funcName);

    std::string_view prefix = "\n"sv;
    for (auto const& param : func.params)
    {
        // auto refMod = param.signature.ByRef() ? "*"sv : ""sv;
        // auto constMod = is_const(param.signature) ? "const "sv : ""sv;
        if (param.signature.ByRef())
        {
            DebugBreak();
        }
        else
        {
            w.write("%%_ %: %",
                prefix,
                indent{ 2 },
                param.name,
                [&](writer& w) { param.type->write_swift_abi_param(w); });
        }
        prefix = ",\n";
    }

    if (func.return_type)
    {
        w.write("%%_ %: UnsafeMutablePointer<%>?",
            prefix,
            indent{ 2 },
            func.return_type->name,
            [&](writer& w) { func.return_type->type->write_swift_abi_param(w); });
    }

    w.write(") throws {\n");
    write_swift_function_thunk(w, type, func);
    w.write("%}\n\n", indent{ 1 });
}

template <typename T>
static void write_swift_interface_definition(writer& w, T const& type)
{
    constexpr bool is_delegate = std::is_same_v<T, delegate_type>;
    constexpr bool is_interface = std::is_same_v<T, interface_type>;
    static_assert(is_delegate || is_interface);

    w.write("public class % : ", type.cpp_abi_name());

    if constexpr (is_delegate)
    {
        w.write("IUnknown");
    }
    else if constexpr (is_interface)
    {
        w.write("IInspectable");
    }

    w.write(R"^-^( {
)^-^");

    w.write(R"^-^(%override public class var IID: IID { % }

)^-^",
        indent { 1 },
        bind_iid_name(type));

    for (auto const& func : type.functions)
    {
        write_swift_function_declaration(w, type, func);
    }

    w.write(R"^-^(}

)^-^");
}

void delegate_type::write_swift_definition(writer& w) const
{
}

void interface_type::write_swift_generic_param_abi_type(writer& w) const
{
}

void interface_type::write_swift_abi_param(writer& w) const
{
    w.write("UnsafeMutablePointer<%>?", [&](writer& w) { write_swift_abi_name(w); });
}

static void write_interface_definition(writer& w, interface_type const& type, void (*func)(writer&, interface_type const&))
{
    // Generics don't get generated definitions
    if (type.is_generic())
    {
        return;
    }

    w.write(R"^-^(private var %: IID {
%IID(%)
}

)^-^", 
        bind_iid_name(type), 
        indent{ 1 },
        bind_uuid_expanded(type));

    func(w, type);
}

void interface_type::write_swift_definition(writer& w) const
{
    write_interface_definition(w, *this, &write_swift_interface_definition<interface_type>);
}

void class_type::write_swift_generic_param_logical_type(writer& w) const
{
}

void class_type::write_swift_generic_param_abi_type(writer& w) const
{
}

void class_type::write_swift_abi_param(writer& w) const
{
    if (!default_interface)
    {
        XLANG_ASSERT(false);
        xlang::throw_invalid("Class '", m_clrFullName, "' cannot be used as a function argument since it has no "
            "default interface");
    }

    default_interface->write_swift_abi_param(w);
}


void class_type::write_swift_definition(writer& w) const
{
}

void generic_inst::write_swift_generic_param_logical_type(writer& w) const
{
}

void generic_inst::write_swift_generic_param_abi_type(writer& w) const
{
}

void generic_inst::write_swift_abi_name(writer& w) const
{
    w.write(m_mangledName);
}
//
void generic_inst::write_swift_abi_param(writer& w) const
{
    w.write("%*", m_mangledName);
}

element_type const& element_type::from_type(xlang::meta::reader::ElementType type)
{
    static element_type const boolean_type{ "Boolean"sv, "bool"sv, "boolean"sv, "boolean"sv, "boolean"sv, "b1"sv };
    static element_type const char_type{ "Char16"sv, "wchar_t"sv, "wchar_t"sv, "WCHAR"sv, "wchar__zt"sv, "c2"sv };
    static element_type const u1_type{ "UInt8"sv, "::byte"sv, "::byte"sv, "BYTE"sv, "byte"sv, "u1"sv };
    static element_type const i2_type{ "Int16"sv, "short"sv, "short"sv, "INT16"sv, "short"sv, "i2"sv };
    static element_type const u2_type{ "UInt16"sv, "UINT16"sv, "UINT16"sv, "UINT16"sv, "UINT16"sv, "u2"sv };
    static element_type const i4_type{ "Int32"sv, "int"sv, "int"sv, "INT32"sv, "int"sv, "i4"sv };
    static element_type const u4_type{ "UInt32"sv, "UINT32"sv, "UINT32"sv, "UINT32"sv, "UINT32"sv, "u4"sv };
    static element_type const i8_type{ "Int64"sv, "__int64"sv, "__int64"sv, "INT64"sv, "__z__zint64"sv, "i8"sv };
    static element_type const u8_type{ "UInt64"sv, "UINT64"sv, "UINT64"sv, "UINT64"sv, "UINT64"sv, "u8"sv };
    static element_type const r4_type{ "Single"sv, "float"sv, "float"sv, "FLOAT"sv, "float"sv, "f4"sv };
    static element_type const r8_type{ "Double"sv, "double"sv, "double"sv, "DOUBLE"sv, "double"sv, "f8"sv };
    static element_type const string_type{ "String"sv, "HSTRING"sv, "HSTRING"sv, "HSTRING"sv, "HSTRING"sv, "string"sv };
    static element_type const object_type{ "Object"sv, "IInspectable*"sv, "IInspectable*"sv, "IInspectable*"sv, "IInspectable"sv, "cinterface(IInspectable)"sv };

    switch (type)
    {
    case ElementType::Boolean: return boolean_type;
    case ElementType::Char: return char_type;
    case ElementType::U1: return u1_type;
    case ElementType::I2: return i2_type;
    case ElementType::U2: return u2_type;
    case ElementType::I4: return i4_type;
    case ElementType::U4: return u4_type;
    case ElementType::I8: return i8_type;
    case ElementType::U8: return u8_type;
    case ElementType::R4: return r4_type;
    case ElementType::R8: return r8_type;
    case ElementType::String: return string_type;
    case ElementType::Object: return object_type;
    default: xlang::throw_invalid("Unrecognized ElementType: ", std::to_string(static_cast<int>(type)));
    }
}

void element_type::write_swift_generic_param_logical_type(writer& w) const
{
}

void element_type::write_swift_generic_param_abi_type(writer& w) const
{
}

void element_type::write_swift_abi_param(writer& w) const
{
    // For element types, param name == ABI name
    write_swift_abi_name(w);

    // NOTE: if m_cppName is HSTRING add ?. Any real solution should handle this
    // more elegantly for all applicable types.
    if (m_cppName == "HSTRING") {
        w.write("?");
    }
}

void element_type::write_swift_abi_name(writer& w) const
{
    w.write(m_cppName);
}

system_type const& system_type::from_name(std::string_view typeName)
{
    if (typeName == "Guid"sv)
    {
        static system_type const guid_type{ "Guid"sv, "GUID"sv, "g16"sv };
        return guid_type;
    }

    XLANG_ASSERT(false);
    xlang::throw_invalid("Unknown type '", typeName, "' in System namespace");
}

void system_type::write_swift_generic_param_logical_type(writer& w) const
{
}

void system_type::write_swift_generic_param_abi_type(writer& w) const
{
}

void system_type::write_swift_abi_name(writer& w) const
{
    w.write(m_cppName);
}

mapped_type const* mapped_type::from_typedef(xlang::meta::reader::TypeDef const& type)
{
    if (type.TypeNamespace() == foundation_namespace)
    {
        if (type.TypeName() == "HResult"sv)
        {
            static mapped_type const hresult_type{ type, "HRESULT"sv, "HRESULT"sv, "struct(Windows.Foundation.HResult;i4)"sv };
            return &hresult_type;
        }
        else if (type.TypeName() == "EventRegistrationToken"sv)
        {
            static mapped_type event_token_type{ type, "EventRegistrationToken"sv, "EventRegistrationToken"sv, "struct(Windows.Foundation.EventRegistrationToken;i8)"sv };
            return &event_token_type;
        }
        else if (type.TypeName() == "AsyncStatus"sv)
        {
            static mapped_type const async_status_type{ type, "AsyncStatus"sv, "AsyncStatus"sv, "enum(Windows.Foundation.AsyncStatus;i4)"sv };
            return &async_status_type;
        }
        else if (type.TypeName() == "IAsyncInfo"sv)
        {
            static mapped_type const async_info_type{ type, "IAsyncInfo"sv, "IAsyncInfo"sv, "{00000036-0000-0000-c000-000000000046}"sv };
            return &async_info_type;
        }
    }

    return nullptr;
}

void mapped_type::write_swift_generic_param_logical_type(writer& w) const
{
}

void mapped_type::write_swift_generic_param_abi_type(writer& w) const
{
}

void mapped_type::write_swift_abi_name(writer& w) const
{
    w.write(m_cppName);
}

void mapped_type::write_swift_abi_param(writer& w) const
{

}
