#include <fcntl.h>
#include <unistd.h>

#include <compiler.h>

vector_t frames;
frame_t *fp;

// ============================================================================
// Interpreter
// ============================================================================

namespace interpreter {

// enum class op_code_t {
//     band_, // bitwise and
//     bneg_, // bitwise negation
//     getfield_, // get a field value of an object objectref
//     goto_, // goes to another instruction at branchoffset
//     goto_if_false_, // if value is false (0), goes to another instruction at branchoffset
//     iadd_, // add two ints
//     iaload_, // load an int from an array
//     iastore_, // store an int into an array
//     ilt_, // less than
//     imul_, // multiply two integers
//     invoke_, // invoke instance method on object objectref and puts result on the stack
//     isub_, // subtract two integers
//     load_, // load a reference onto the stack from a local variable #index
//     ldc_, // push a constant onto the stack
//     length_, // array length
//     new_, // create new object of type identified by class reference
//     newarray_, // create new array of integers
//     putfield_, // set field to value in an object objectref
//     print_, // print integer
//     return_, // return from method
//     store_, // store value into variable
// };

struct interpreter_t {
    std::vector<compiler::class_layout_t> classes;
    std::vector<compiler::method_layout_t> methods;
    interpreter_t(std::vector<compiler::class_layout_t> classes,
                  std::vector<compiler::method_layout_t> methods)
      : classes{std::move(classes)}, methods{std::move(methods)}
    {
        vector_init(&frames, 16);
    }
    void exec(void);
    void loop(void);
    void log(const char *msg);
    void exec_band(void);
    void exec_bneg(void);
    void exec_dup(void);
    void exec_getfield(void);
    void exec_goto(void);
    void exec_goto_if_false(void);
    void exec_iadd(void);
    void exec_iaload(void);
    void exec_iastore(void);
    void exec_ilt(void);
    void exec_imul(void);
    void exec_invoke(void);
    void exec_isub(void);
    void exec_load(void);
    void exec_ldc(void);
    void exec_length(void);
    void exec_new(void);
    void exec_newarray(void);
    void exec_putfield(void);
    void exec_print(void);
    void exec_return(void);
    void exec_store(void);
};

// #define ENABLE_LOGGING

void interpreter_t::log(const char *msg)
{
#ifdef ENABLE_LOGGING
    std::cerr << msg << std::endl;
#endif
}

void interpreter_t::exec(void)
{
    log("exec");
    // fix up vtables
    for (auto &c : classes) {
        for (auto &mpair : c.vtbl) {
            auto m = std::find_if(methods.begin(), methods.end(),
                                  [&mpair](const compiler::method_layout_t &m) {
                                      return m.method_name == mpair;
                                  });
            if (m == methods.end()) {
                std::cerr << "method not found: " << mpair.second << std::endl;
                exit(1);
            }
            c.vtable.push_back(&(*m));
        }
    }
    auto frame = frame_create();
    frame->ip = frame->ip_start = reinterpret_cast<void *>(&methods[0].instructions[0]);
    assert(std::strcmp(methods[0].method_name.second.c_str(), "main") == 0);
    vector_push(&frames, frame);
    fp = frame;
    loop();
}

void interpreter_t::loop(void)
{
    while (true) {
        log("loop");
        auto ip = reinterpret_cast<bytecode::instruction_t *>(fp->ip);
        switch (ip->op_code) {
        case bytecode::op_code_t::band_: exec_band(); break;
        case bytecode::op_code_t::bneg_: exec_bneg(); break;
        case bytecode::op_code_t::getfield_: exec_getfield(); break;
        case bytecode::op_code_t::goto_: exec_goto(); break;
        case bytecode::op_code_t::goto_if_false_: exec_goto_if_false(); break;
        case bytecode::op_code_t::iadd_: exec_iadd(); break;
        case bytecode::op_code_t::iaload_: exec_iaload(); break;
        case bytecode::op_code_t::iastore_: exec_iastore(); break;
        case bytecode::op_code_t::ilt_: exec_ilt(); break;
        case bytecode::op_code_t::imul_: exec_imul(); break;
        case bytecode::op_code_t::invoke_: exec_invoke(); break;
        case bytecode::op_code_t::isub_: exec_isub(); break;
        case bytecode::op_code_t::load_: exec_load(); break;
        case bytecode::op_code_t::ldc_: exec_ldc(); break;
        case bytecode::op_code_t::length_: exec_length(); break;
        case bytecode::op_code_t::new_: exec_new(); break;
        case bytecode::op_code_t::newarray_: exec_newarray(); break;
        case bytecode::op_code_t::putfield_: exec_putfield(); break;
        case bytecode::op_code_t::print_: exec_print(); break;
        case bytecode::op_code_t::return_: exec_return(); break;
        case bytecode::op_code_t::store_: exec_store(); break;
        default: assert(false);
        }
    }
}

void interpreter_t::exec_band(void)
{
    log("exec_band");
    auto val2 = vector_pop(&fp->val_stack);
    auto val1 = vector_pop(&fp->val_stack);
    auto ival1 = ptr_to_int(val1);
    auto ival2 = ptr_to_int(val2);
    auto iresult = ival1 & ival2;
    auto result = int_to_ptr(iresult);
    vector_push(&fp->val_stack, result);
    auto ip = reinterpret_cast<bytecode::instruction_t *>(fp->ip);
    ip += 1;
    fp->ip = reinterpret_cast<void *>(ip);
}

void interpreter_t::exec_bneg(void)
{
    log("exec_bneg");
    auto val = vector_pop(&fp->val_stack);
    auto ival = ptr_to_int(val);
    ival = !ival;
    auto result = int_to_ptr(ival);
    vector_push(&fp->val_stack, result);
    auto ip = reinterpret_cast<bytecode::instruction_t *>(fp->ip);
    ip += 1;
    fp->ip = reinterpret_cast<void *>(ip);
}

void interpreter_t::exec_getfield(void)
{
    log("exec_getfield");
    auto obj = vector_pop(&fp->val_stack);
    auto hobj = ptr_to_hval(obj);
    auto ip = reinterpret_cast<bytecode::instruction_t *>(fp->ip);
    auto field_idx = ip->operand;
    auto pfield = pith_field(hobj, field_idx);
    auto field = *pfield;
    vector_push(&fp->val_stack, reinterpret_cast<void *>(field));
    ip += 1;
    fp->ip = reinterpret_cast<void *>(ip);
}

void interpreter_t::exec_goto(void)
{
    log("exec_goto");
    auto ip = reinterpret_cast<bytecode::instruction_t *>(fp->ip);
    fp->ip = reinterpret_cast<void *>(
        reinterpret_cast<bytecode::instruction_t *>(fp->ip_start) + ip->operand);
}

void interpreter_t::exec_goto_if_false(void)
{
    log("exec_goto_if_false");
    auto val = vector_pop(&fp->val_stack);
    auto ival = ptr_to_int(val);
    auto ip = reinterpret_cast<bytecode::instruction_t *>(fp->ip);
    if (ival == 0) {
        fp->ip = reinterpret_cast<void *>(
            reinterpret_cast<bytecode::instruction_t *>(fp->ip_start) + ip->operand);
    }
    else {
        ip += 1;
        fp->ip = reinterpret_cast<void *>(ip);
    }
}

void interpreter_t::exec_iadd(void)
{
    log("exec_iadd");
    auto val2 = vector_pop(&fp->val_stack);
    auto val1 = vector_pop(&fp->val_stack);
    auto ival1 = ptr_to_int(val1);
    auto ival2 = ptr_to_int(val2);
    auto iresult = ival1 + ival2;
    auto result = int_to_ptr(iresult);
    vector_push(&fp->val_stack, result);
    auto ip = reinterpret_cast<bytecode::instruction_t *>(fp->ip);
    ip += 1;
    fp->ip = reinterpret_cast<void *>(ip);
}

void interpreter_t::exec_iaload(void)
{
    log("exec_iaload");
    auto arr = vector_pop(&fp->val_stack);
    auto harr = ptr_to_hval(arr);
    assert(harr->tag & VAL_ARRAY_TAG);
    auto idx = vector_pop(&fp->val_stack);
    auto iidx = ptr_to_int(idx);
    auto elem = *pith_field_arr(harr, iidx);
    elem |= VAL_INT_TAG;
    vector_push(&fp->val_stack, reinterpret_cast<void *>(elem));
    auto ip = reinterpret_cast<bytecode::instruction_t *>(fp->ip);
    ip += 1;
    fp->ip = reinterpret_cast<void *>(ip);
}

void interpreter_t::exec_iastore(void)
{
    log("exec_iastore");
    auto arr = vector_pop(&fp->val_stack);
    auto harr = ptr_to_hval(arr);
    assert(harr->tag & VAL_ARRAY_TAG);
    auto val = vector_pop(&fp->val_stack);
    auto idx = vector_pop(&fp->val_stack);
    auto iidx = ptr_to_int(idx);
    auto pfield = pith_field_arr(harr, iidx);
    *pfield = reinterpret_cast<int64_t>(val);
    auto ip = reinterpret_cast<bytecode::instruction_t *>(fp->ip);
    ip += 1;
    fp->ip = reinterpret_cast<void *>(ip);
}

void interpreter_t::exec_ilt(void)
{
    log("exec_ilt");
    auto val2 = vector_pop(&fp->val_stack);
    auto val1 = vector_pop(&fp->val_stack);
    auto ival1 = ptr_to_int(val1);
    auto ival2 = ptr_to_int(val2);
    auto iresult = (ival1 < ival2) ? 1 : 0;
    auto result = int_to_ptr(iresult);
    vector_push(&fp->val_stack, result);
    auto ip = reinterpret_cast<bytecode::instruction_t *>(fp->ip);
    ip += 1;
    fp->ip = reinterpret_cast<void *>(ip);
}

void interpreter_t::exec_imul(void)
{
    log("exec_imul");
    auto val2 = vector_pop(&fp->val_stack);
    auto val1 = vector_pop(&fp->val_stack);
    auto ival1 = ptr_to_int(val1);
    auto ival2 = ptr_to_int(val2);
    auto iresult = ival1 * ival2;
    auto result = int_to_ptr(iresult);
    vector_push(&fp->val_stack, result);
    auto ip = reinterpret_cast<bytecode::instruction_t *>(fp->ip);
    ip += 1;
    fp->ip = reinterpret_cast<void *>(ip);
}

void interpreter_t::exec_invoke(void)
{
    log("exec_invoke");
    auto ip = reinterpret_cast<bytecode::instruction_t *>(fp->ip);
    auto method_idx = ip->operand;
    auto nargs = ip->operand2;
    auto frame = frame_create();
    std::vector<void *> args;
    for (size_t i = 1; i < nargs; ++i) {
        auto v = vector_pop(&fp->val_stack);
        args.push_back(v);
    }
    std::reverse(args.begin(), args.end());
    auto obj = vector_pop(&fp->val_stack);
    auto hobj = ptr_to_hval(obj);
    auto vtable =
        reinterpret_cast<std::vector<compiler::method_layout_t *> *>(hobj->vtable);
    auto method = (*vtable)[method_idx];
    vector_push(&frame->locals, obj);
    for (auto &arg : args) {
        vector_push(&frame->locals, arg);
    }
    for (size_t i = 0; i < method->locals.size(); ++i) {
        vector_push(&frame->locals, nullptr);
    }
    frame->ip = frame->ip_start = reinterpret_cast<void *>(&method->instructions[0]);
    vector_push(&frames, frame);
    fp = frame;
}

void interpreter_t::exec_isub(void)
{
    log("exec_isub");
    auto val2 = vector_pop(&fp->val_stack);
    auto val1 = vector_pop(&fp->val_stack);
    auto ival1 = ptr_to_int(val1);
    auto ival2 = ptr_to_int(val2);
    auto iresult = ival1 - ival2;
    auto result = int_to_ptr(iresult);
    vector_push(&fp->val_stack, result);
    auto ip = reinterpret_cast<bytecode::instruction_t *>(fp->ip);
    ip += 1;
    fp->ip = reinterpret_cast<void *>(ip);
}

void interpreter_t::exec_load(void)
{
    log("exec_load");
    auto ip = reinterpret_cast<bytecode::instruction_t *>(fp->ip);
    auto idx = ip->operand;
    auto val = vector_get(&fp->locals, idx);
    vector_push(&fp->val_stack, val);
    ip += 1;
    fp->ip = reinterpret_cast<void *>(ip);
}

void interpreter_t::exec_ldc(void)
{
    log("exec_ldc");
    auto ip = reinterpret_cast<bytecode::instruction_t *>(fp->ip);
    auto idx = ip->operand;
    auto val = reinterpret_cast<void *>((idx << 1) | VAL_INT_TAG);
    vector_push(&fp->val_stack, val);
    ip += 1;
    fp->ip = reinterpret_cast<void *>(ip);
}

void interpreter_t::exec_length(void)
{
    log("exec_length");
    auto arr = vector_pop(&fp->val_stack);
    auto harr = ptr_to_hval(arr);
    assert(harr->tag & VAL_ARRAY_TAG);
    auto ilen = harr->size;
    auto len = int_to_ptr(ilen);
    vector_push(&fp->val_stack, len);
    auto ip = reinterpret_cast<bytecode::instruction_t *>(fp->ip);
    ip += 1;
    fp->ip = reinterpret_cast<void *>(ip);
}

void interpreter_t::exec_new(void)
{
    log("exec_new");
    auto ip = reinterpret_cast<bytecode::instruction_t *>(fp->ip);
    auto &class_layout = classes.at(ip->operand);
    auto vtable = reinterpret_cast<std::vector<compiler::method_layout_t *> *>(
        &(class_layout.vtable));
    assert((reinterpret_cast<int64_t>(vtable) & 7) == 0); // 8-byte alignment
    auto nfields = class_layout.fields.size();
    auto obj = alloc_heapval(reinterpret_cast<void *>(vtable), nfields);
    vector_push(&fp->val_stack, reinterpret_cast<void *>(obj));
    ip += 1;
    fp->ip = reinterpret_cast<void *>(ip);
}

void interpreter_t::exec_newarray(void)
{
    log("exec_newarray");
    auto len = vector_pop(&fp->val_stack);
    auto ilen = ptr_to_int(len);
    auto arr = alloc_arr(ilen);
    vector_push(&fp->val_stack, reinterpret_cast<void *>(arr));
    auto ip = reinterpret_cast<bytecode::instruction_t *>(fp->ip);
    ip += 1;
    fp->ip = reinterpret_cast<void *>(ip);
}

void interpreter_t::exec_putfield(void)
{
    log("exec_putfield");
    auto ip = reinterpret_cast<bytecode::instruction_t *>(fp->ip);
    auto field_idx = ip->operand;
    auto obj = vector_pop(&fp->val_stack);
    auto hobj = ptr_to_hval(obj);
    auto val = vector_pop(&fp->val_stack);
    auto pfield = pith_field(hobj, field_idx);
    *pfield = reinterpret_cast<int64_t>(val);
    ip += 1;
    fp->ip = reinterpret_cast<void *>(ip);
}

void interpreter_t::exec_print(void)
{
    log("exec_print");
    auto val = vector_pop(&fp->val_stack);
    auto ival = ptr_to_int(val);
    std::cout << ival << "\n";
    auto ip = reinterpret_cast<bytecode::instruction_t *>(fp->ip);
    ip += 1;
    fp->ip = reinterpret_cast<void *>(ip);
}

void interpreter_t::exec_return(void)
{
    log("exec_return");
    if (frames.size == 1) {
        exit(0);
    }
    auto r = vector_pop(&fp->val_stack);
    vector_pop(&frames);
    frame_destroy(fp);
    fp = reinterpret_cast<frame_t *>(vector_top(&frames));
    vector_push(&fp->val_stack, r);
    auto ip = reinterpret_cast<bytecode::instruction_t *>(fp->ip);
    ip += 1;
    fp->ip = reinterpret_cast<void *>(ip);
}

void interpreter_t::exec_store(void)
{
    log("exec_store");
    auto ip = reinterpret_cast<bytecode::instruction_t *>(fp->ip);
    auto idx = ip->operand;
    auto val = vector_pop(&fp->val_stack);
    vector_set(&fp->locals, idx, val);
    ip += 1;
    fp->ip = reinterpret_cast<void *>(ip);
}

} // namespace interpreter

void usage(const char *progname)
{
    fprintf(stderr, "Usage: %s <input file> [--emit-bc]\n", progname);
    exit(1);
}

// #define USE_INTERPRETER

int main(int argc, char **argv)
{
    if (!(argc == 2 || argc == 3)) {
        usage(argv[0]);
    }
    if (close(0) == -1) {
        perror("close");
    }
    if (open(argv[1], O_RDONLY) == -1) {
        perror("open");
    }
    bool emit_bc = false;
    if (argc == 3) {
        if (std::strcmp(argv[2], "--emit-bc") == 0) {
            emit_bc = true;
        }
        else {
            usage(argv[0]);
        }
    }
    scanner::scanner_t scanner = scanner::create_scanner();
    parser::parser_t parser{std::move(scanner)};
    auto goal = parser.parse_goal();
    semantics::semantic_vis_accum_classes_t semantic_vis_accum_classes;
    goal->accept(&semantic_vis_accum_classes);
    semantics::semantic_vis_accum_parent_classes_t semantic_vis_accum_parent_classes{
        semantic_vis_accum_classes.program_symtbl};
    goal->accept(&semantic_vis_accum_parent_classes);
    semantics::semantic_vis_accum_fields_t semantic_vis_accum_fields{
        semantic_vis_accum_parent_classes.program_symtbl};
    goal->accept(&semantic_vis_accum_fields);
    semantics::semantic_vis_accum_local_vars_t semantic_vis_accum_local_vars{
        semantic_vis_accum_fields.program_symtbl};
    goal->accept(&semantic_vis_accum_local_vars);
    semantics::semantic_vis_type_check_t semantic_vis_type_check{
        semantic_vis_accum_local_vars.program_symtbl};
    goal->accept(&semantic_vis_type_check);
#ifdef USE_INTERPRETER
    compiler::layout_visitor_t layout_visitor;
    goal->accept(&layout_visitor);
    compiler::vt_visitor_t vt_visitor{layout_visitor.classes, layout_visitor.methods};
    goal->accept(&vt_visitor);
    compiler::bc_compiler_visitor_t bc_compiler_visitor{
        vt_visitor.classes, vt_visitor.methods, semantic_vis_type_check};
    goal->accept(&bc_compiler_visitor);
    if (emit_bc) {
        bc_compiler_visitor.print();
    }
    interpreter::interpreter_t interpreter{bc_compiler_visitor.classes,
                                           bc_compiler_visitor.methods};
    interpreter.exec();
#else
    (void)emit_bc;
    compiler::llvm_object_layout_visitor_t llvm_layout_visitor{semantic_vis_type_check};
    goal->accept(&llvm_layout_visitor);
    compiler::llvm_vt_visitor_t llvm_vt_visitor{llvm_layout_visitor.llvm_state,
                                                llvm_layout_visitor.classes,
                                                llvm_layout_visitor.type_checker};
    goal->accept(&llvm_vt_visitor);
    compiler::llvm_compiler_visitor_t llvm_compiler_visitor{
        llvm_layout_visitor.llvm_state, llvm_vt_visitor.classes, semantic_vis_type_check};
    goal->accept(&llvm_compiler_visitor);
    llvm_compiler_visitor.llvm_state.module->print(llvm::outs(), nullptr);
    // verify IR
    llvm::verifyModule(*llvm_compiler_visitor.llvm_state.module, &llvm::outs());
#endif
    return 0;
}