#import "Basic";
#import "Deep_Copy";
#import "Compiler";

WireType :: enum u8 {
    Varint :: 0;
    ZigZag :: 1;
    Fixed32 :: 2;
    Fixed64 :: 3;
    Size :: 4;
}

FieldDesc :: struct {
    tag : u32;
    type: Type;
    wt: WireType;
    optional: bool;
    repeated: bool;
}

append_varint :: (b: *[..]u8, $T: Type, value: T, fd: FieldDesc) {
    append_raw_varint :: (b: *[..]u8, value: u64) {
        copy := value;
        while copy > 0x7f {
            array_add(b, (0x80 + cast(u8) (copy & 0x7f)));
            copy = copy >> 7;
        }
        array_add(b, cast(u8) (copy & 0x7f));
    }

    #if T == bool {
    val := cast(u32)value;
    TP :: u32;
    } else {
    val := cast(u64) value;
    TP :: T;
    }

    if val < 0x7f && val >=0 {
        array_add(b, cast,trunc(u8) val);
    } else {
        ti := type_info(T);
        val_u64 : u64;
        bitsize := ti.runtime_size * 8;
        if fd.wt == {
            case .ZigZag;
                val_u64 = cast(u64) (val >> (bitsize - 1)) ^ (val << 1);
            case .Varint;
                if bitsize == {
                    case 32;
                        tmpval := val;
                        val_u64 = cast(u64) << (cast(*u32) (*tmpval));
                    case 64;
                        tmpval := val;
                        val_u64 = cast(u64) << (cast(*u64) (*tmpval));
                }
        }
        append_raw_varint(b, val_u64);
    }
}

append_fixed :: (b: *[..]u8, value: $T, fd: FieldDesc) {
    index := b.count;
    array_resize(b, b.count + size_of(T));
    value_bytes : *u8 = xx *value;
    memcpy(*b.data[index], value_bytes, size_of(T));
}

append_bytes ::  (b: *[..]u8, value: $T, fd: FieldDesc) {
    append_varint(b, s64, value.count, fd);
    array_add(b, ..value);
}

append_tag ::  (b: *[..]u8, fd: FieldDesc) {
    append_varint(b, u32, cast(u32) ((fd.tag << 3) | get_wirevalue(fd)), FieldDesc.{wt=.Varint});
}

insert_raw_varint :: (b: *[..]u8, value: u64, start: s64) {
    if value < 0x7f {
        array_insert_at(b, cast,trunc(u8) value, start);
    } else {
        copy := value;
        idx := start;
        while copy > 0x7f {
            array_insert_at(b, (0x80 + cast(u8) (copy & 0x7f)), idx);
            copy = copy >> 7;
            idx = idx + 1;
        }
        array_insert_at(b, cast(u8) (copy & 0x7f), idx);
    }
}

get_wirevalue :: (fd: FieldDesc) -> u8 {
    if #complete fd.wt == {
        case .Fixed64;
            return 1;
        case .Fixed32;
            return 5;
        case .Varint;
            return 0;
        case .ZigZag;
            return 0;
        case .Size;
            return 2;
    }
}

append_one :: (b: *[..]u8, v: $T, fd: FieldDesc) {
    #if T == string {
        append_bytes(b, cast([]u8) v, fd);
    } else #if T == bool {
        append_varint(b, T, v, fd);
    } else #if T == u32 || T == s32 || T == u8 || T == s8 || T == u16 || T == s16 {
        append_varint(b, T, v, fd);
    } else #if #run type_info(T).type == .ENUM {
        append_varint(b, T, v, fd);
    } else #if T == float32 || T == float64 || T == u64 || T == s64 {
        append_fixed(b, v, fd);
    } else {
        index := b.count;
        v.encode(b, v);
        new_index := b.count;
        length := new_index - index;
        insert_raw_varint(b, cast(u64) length, index);
    }
}

append_any :: (b: *[..]u8, v: $T, fd: FieldDesc) {
    #if #run type_info(T).type == .POINTER {
        if v != null {
            append_tag(b, fd);
            append_one(b, << v, fd);
        }
    } else #if #run type_info(T).type == .ARRAY {
        for v {
            append_tag(b, fd);
            append_one(b, it, fd);
        }
    } else {
        append_tag(b, fd);
        append_one(b, v, fd);
    }
}

type_str :: (t: *Type_Info) -> string {
    sb : String_Builder;
    print_type_to_builder(*sb, t);
    return builder_to_string(*sb);
}

expand_for :: (function_name: string, element_names: [] string) -> string {
    // TODO temp mem only
    sb: String_Builder;
    for i: 0..element_names.count - 1 {
        print_to_builder(*sb, "%(\"%\");\n", function_name, element_names[i]);
    }
    return builder_to_string(*sb);
}

for_each_member :: ($T: Type, format: string) -> string {
    builder: String_Builder;
    defer free_buffers(*builder);

    struct_info := cast(*Type_Info_Struct) T;
    assert(struct_info.type == Type_Info_Tag.STRUCT);

    for struct_info.members {
        if it.flags & .CONSTANT continue;
        print_to_builder(*builder, format, it.name);
    }
    return builder_to_string(*builder);
}

get_proto_field_names :: ($PT: Type) -> [] string {
    #insert #run tprint("field_names: [%] string;", (cast(*Type_Info_Struct) type_info(PT._desc_table)).members.count);
    info := cast(*Type_Info_Struct) type_info(PT._desc_table);
    for i: 0..info.members.count - 1 {
        field_names[i] = info.members[i].name;
    }
    return field_names;
}

encode_code_runtime :: (b: *[..]u8, v: $T) {
    encode_field :: ($field_name: string) #expand {
        #insert -> string {
            return tprint("append_any(b, v.%, desc_table.%);\n", field_name, field_name);
        };
    }

    desc_table := v._desc_table.{};
    #insert #run expand_for("encode_field", #run get_proto_field_names(T));
}


decode_any :: (b: []u8, $T: Type, fd: FieldDesc) -> $R, u32 
#modify {
    // get the underlying type for a pointer or array
    ti := cast(*Type_Info) T;
    if ti.type == .POINTER {
        T = get_type((cast(*Type_Info_Pointer) ti).pointer_to);
    } else if ti.type == .ARRAY {
        T = get_type((cast(*Type_Info_Array) ti).element_type);
    }
    R = T;
    return true;
}{
    #if T == string {
        tmp, read := decode_size_bytes(b, string);
        return tmp, read;
    } else #if T == bool {
        tmp, read := decode_varint(b, T);
        return tmp, read;
    } else #if T == u32 || T == s32 || T == u8 || T == s8 || T == u16 || T == s16 {
        tmp, read := decode_varint(b, T);
        return tmp, read;
    } else #if #run type_info(T).type == .ENUM {
        tmp, read := decode_varint(b, T);
        return tmp, read;
    } else #if T == float32 || T == float64 || T == u64 || T == s64 {
        tmp, read := decode_fixed(b, T);
        return tmp, read;
    } else {
        size, read := decode_varint(b, u32);
        nb := array_view(b, read, size);
        obj : T;
        tmp := obj.decode(nb);
        return tmp, read + size;
    }
}

assign_any :: (p: *$PT, $fname: string, $FT: Type, v: $T, fd: FieldDesc) {
    tmp := Deep_Copy(*v, Deep_Copy_Config.{});
    #if #run type_info(FT).type == .POINTER {
        #insert #run tprint("p.% = tmp;\n", fname);
    } else #if #run type_info(FT).type == .ARRAY {
        #insert #run tprint("array_add(*((<< p).%), << tmp);\n", fname);
    } else {
        #insert #run tprint("p.% = << tmp;\n", fname);
    }
}

decode_check :: (b: []u8, v: *$T, tag: u32, wire: u8) -> u32 {
    desc_table := v._desc_table.{};
    read_any := cast(u32) 0;
    maybe_decode_field :: ($field_name: string) #expand {
        #insert -> string {
            vt := cast(*Type_Info_Struct) type_info(T);
            vtf := cast(*Type_Info_Struct_Member)get_field(vt, field_name);
            sb : String_Builder;
            append(*sb, tprint(#string EOF
            if tag == desc_table.%1.tag {
                tmp, read := decode_any(b, type_of(v.%1), desc_table.%1);
                assign_any(v, "%1", type_of(v.%1), tmp, desc_table.%1);
                read_any = read;
            }
            EOF, field_name));
            return builder_to_string(*sb);
        };
    }

    #insert #run expand_for("maybe_decode_field", #run get_proto_field_names(T));

    return read_any;
}

decode_code_runtime :: (b: []u8, $T: Type) -> T {
    val: T;
    desc_table := val._desc_table.{};
    op := array_view(b, 0, b.count);

    while op.count > 0 {
        cursor := 0;
        tag, wire, read := decode_tag(op);
        cursor += read;

        rest := array_view(op, cursor, op.count);
        read_check := decode_check(rest, *val, tag, wire);
        cursor += read_check;

        op = array_view(op, cursor, op.count);
        cursor = 0;
    }
    return val;
}

setters_code_writer :: (v: $T, verbose := false) -> string {
    type_str :: (t: *Type_Info) -> string {
        sb : String_Builder;
        print_type_to_builder(*sb, t);
        return builder_to_string(*sb);
    }

    vti := cast(*Type_Info_Struct) type_info(T);
    dtf := cast(*Type_Info_Struct_Member)get_field(vti, "_desc_table");
    sb: String_Builder;

    if dtf != null {
        dt : v._desc_table;
        dt_ti := type_info(v._desc_table);
        for dt_ti.members {
            fd := cast(*FieldDesc) ((cast(*u8) *dt) + it.offset_in_bytes);
            vf := cast(*Type_Info_Struct_Member) get_field(vti, it.name);

            vf_underlying := vf.type; // the type of the underlying field
            if vf_underlying.type == {
                case .POINTER;
                    real_type := (cast(*Type_Info_Pointer) vf_underlying).pointer_to;
                    append(*sb, tprint(#string EOF
                        set_%1 :: (val: *%2, in: %3) {
                            val.%1 = Deep_Copy(*in, Deep_Copy_Config.{});
                        }
                    EOF, it.name, vti.name, type_str(real_type)));
                case .ARRAY;
                case;
            }
        }
    }

    tmp := builder_to_string(*sb);
    return tmp;
}


decode_varint :: (b: []u8, $T: Type) -> (T, u8) {
    #if T == bool {
        value: u32;
        TP :: u32;
    } else {
        value: T;
        TP :: T;
    }
    index := 0;
    while b[index] & 0b10000000 != 0 {
        value += (cast(TP) (b[index] & 0x7F)) << (index * 7);
        index += 1;
    }
    value += (cast(TP) (b[index] & 0x7F)) << (index * 7);
    return cast(T) value, cast(u8) index+1;
}

decode_fixed :: (b: []u8, $T: Type) -> (T, u8) {
    value: T;
    memcpy(cast(*u8)*value, *b[0], size_of(T));
    return value, cast(u8) size_of(T);
}

decode_tag :: (b: []u8) -> (u32, u8, u8)  {
    tag_wire, read := decode_varint(b, u32);
    wire := tag_wire & 0b00000111;
    tag := tag_wire >> 3;
    return cast(u32) tag, cast,trunc(u8) wire, read;

}

decode_size_bytes :: (b: []u8, $T: Type) -> (T, u32) {
    size, read := decode_varint(b, u32);
    nb := array_view(b, read, size);
    ret : [..]u8;
    array_resize(*ret, size);
    memcpy(*ret[0], *nb[0], size);
    return cast(T) ret, size+read;
}

