using import struct
using import Map
using import enum
global rl-env : (Map Symbol Value)

enum RLValue
    Bool   : bool
    Number : f64
    String : string
    Symbol : Symbol
    List   : list

spice box-value (v)
    let T = ('typeof v)

    if (T == RLValue)
        return v

    if (T < integer)
        if (T == bool)
            spice-quote
                RLValue.Bool v
        else
            spice-quote
                RLValue.Number (v as f64)
    elseif (T < real)
        spice-quote
            RLValue.Number (v as f64)
    elseif (T == string)
        spice-quote
            RLValue.String (v as string)
    elseif (T == Symbol)
        spice-quote
            RLValue.Symbol (v as Symbol)
    elseif (T == list)
        spice-quote
            RLValue.List (v as list)
    else
        error
            .. "could not box value of type " (repr T)

# spice unbox-value (v)
#     inline unbox-bool (v)
#         v as bool
#     inline unbox-sint (v)
#         v as i64 as f64
#     inline unbox-uint (v)
#         v as u64 as f64
#     inline unbox-real (v)
#         v as f64

#     if (T < integer)
#         if (T == bool)
#             spice-quote
#                 unbox-bool v
#         elseif ('signed? T)
#             spice-quote
#                 unbox-sint v
#         else
#             spice-quote
#                 unbox-uint v
#     elseif (T < real)
#         spice-quote
#             unbox-real v
#     else
#         error "unexpected type, can't unbox"

spice rl-call (self args...)
    let argc = ('argcount args...)
    let box-args = (sc_expression_new)

    let args = `(alloca-array RLValue [argc])

    for i a in (enumerate ('args args...))
        sc_expression_append box-args `((args @ i) = (box-value a))

    sc_expression_append box-args `args
    let call-expr =
        'tag
            spice-quote
                (bitcast self Closure) [argc] args
            'anchor self
    spice-quote
        box-args
        call-expr

spice bind-fn-args ()

run-stage;

typedef RLClosure : (storageof Closure)
    let __call = (box-pointer rl-call)

run-stage;

sugar _if ()
sugar _fn (args...)
    let name args body =
        sugar-match args...
        case ((args...) body...)
            _ (Symbol "#unnamed")
                \ args... body...
        case (name (args...) body...)
            _ (name as Symbol) args... body...
        default
            error "incorrect function syntax"

    inline arity-check (expected n)
        if (expected != n)
            error
                .. "expected " (tostring expected) " arguments, got " (tostring n)
    let arg-bindings =
        fold (bindings = '()) for i arg in (enumerate args)
            cons
                qq
                    [let] [arg] = (args @ [i])
                bindings
    qq
        [let] [name] =
            [bitcast]
                [fn] (argc args)
                    [arity-check] [(countof args)] argc
                    unquote-splice arg-bindings
                    [unlet] args
                    unquote-splice body
                [RLClosure]

run-stage;

let rl-primitives =
    do
        let + - / // * ** < > <= >= == !=
        let and or not
        let print
        let sugar-quote
        let true false
        let cons
        let car = sc_list_at
        let cdr = sc_list_next
        let empty? inline
        let if = _if
        let fn = _fn

        locals;

let argc argv = (launch-args)
if (argc < 3)
    error "expected source file"

fn parse-module-name (filename)
    let match? start end = ('match? "(^.+\\/?).+?(?=\\..+$)" filename)
    if match?
        slice filename start end
    else
        filename

let filename = (string (argv @ 2))

hide-traceback;
load-module (parse-module-name filename) filename
    scope =
        'bind rl-primitives list-handler-symbol list-handler
