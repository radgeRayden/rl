using import struct
using import Map
using import enum

# we use this in place of RLFunction, since it depends on RLValue.
# Later on we'll add a method so it correctly implies to the function pointer on call.
typedef _RLFunction : voidstar
    inline __repr (self)
        default-styler 'style-type "Function"

enum RLValue
    Bool   : bool
    Number : f64
    String : string
    Symbol : Symbol
    List   : list
    Nil    = none
    Function : _RLFunction

    inline type (self)
        'apply self
            (T self) -> (default-styler 'style-type (tostring T.Name))
    inline __repr (self)
        'apply self
            inline (T self)
                static-if (T.Literal == this-type.String.Literal)
                    deref self
                else
                    (repr self)

    inline __call (self args...)
        dispatch self
        case Function (f)
            f args...
        default
            hide-traceback;
            error (.. "cannot call value of type " ('type self))

typedef RLFunction :
    pointer
        raises
            function (uniqueof RLValue -1) i32 (viewof (mutable@ RLValue) 2)
            Error

inline rlvalue-unbox-as (self T)
    static-if ((typeof self) == RLValue)
        if (('literal self) == T.Literal)
            'unsafe-extract-payload self T.Type
        else
            hide-traceback;
            error
                .. "can't unbox value of type " ('type self)
                    \ " as " (default-styler 'style-type (tostring T.Name))
    else
        imply self (elementof T.Type 0)

# we use va to accomodate for receiving nothing.
spice box-value (...)
    verify-count ('argcount ...) 0 1
    let v = ('getarg ... 0)
    let T = ('typeof v)

    if (T == RLValue)
        return `(deref (dupe v))

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
    elseif ((T == Nothing) or (T == void))
        spice-quote
            RLValue.Nil (tuple)
    elseif (T == RLFunction)
        spice-quote
            RLValue.Function (bitcast v _RLFunction)
    else
        error
            .. "could not box value of type " (repr T)

fn box-Value (v)
    let T = ('typeof v)
    if (T < integer)
        if (T == bool)
            RLValue.Bool (v as bool)
        else
            RLValue.Number (v as i32 as f64)
    elseif (T < real)
        RLValue.Number (v as f32 as f64)
    elseif (T == string)
        RLValue.String (v as string)
    elseif (T == Symbol)
        RLValue.Symbol (v as Symbol)
    elseif (T == list)
        RLValue.List (v as list)
    elseif ((T == Nothing) or (T == void))
        RLValue.Nil (tuple)
    elseif (T == RLFunction)
        RLValue.Function (bitcast (v as RLFunction) _RLFunction)
    else
        error
            .. "could not box value of type " (repr T)

spice rl-call (self args...)
    let argc = ('argcount args...)
    let box-args = (sc_expression_new)

    let args = `(alloca-array RLValue [argc])

    for i a in (enumerate ('args args...))
        sc_expression_append box-args
            spice-quote
                (args @ i) = (dupe (box-value a))

    sc_expression_append box-args `args
    let call-expr =
        'tag
            spice-quote
                (storagecast self) [argc] args
            'anchor self
    spice-quote
        box-args
        call-expr

run-stage;

typedef+ RLFunction
    let __call = (box-pointer rl-call)

typedef+ _RLFunction
    inline __call (self args...)
        (bitcast self RLFunction) args...

# RL SPECIAL FORMS
# ================================================================================
sugar _let (bindings body...)
    let bindings =
        fold (result = '()) for binding in (bindings as list)
            sugar-match (binding as list)
            case (name expr)
                cons
                    qq [let] [name] = [expr]
                    result
            default
                error "invalid let syntax"
    qq
        [do]
            [embed]
                unquote-splice ('reverse bindings)
            unquote-splice body...

sugar _if (args...)
    sugar-match args...
    case (condition tclause fclause)
        qq
            [embed]
                [if] ([rlvalue-unbox-as] [condition] [RLValue.Bool])
                    [box-value]
                        [tclause]
                else
                    [box-value]
                        [fclause]
    default
        error "incorrect if syntax"

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

    let arg-bindings =
        fold (bindings = '()) for i arg in (enumerate args)
            cons
                qq
                    [let] [arg] = ([@] args [i])
                bindings

    inline arity-check (expected n)
        if (expected != n)
            error
                .. "expected " (tostring expected) " arguments, got " (tostring n)

    inline rl-conv-fn (f)
        bitcast
            static-typify f i32
                viewof (mutable@ RLValue) 2
            RLFunction

    qq
        [let] [name] =
            [rl-conv-fn]
                [fn] (argc args)
                    [arity-check] [(countof args)] argc
                    [let] [name] = ([rl-conv-fn] this-function)
                    [unlet] this-function
                    unquote-splice arg-bindings
                    [unlet] args
                    [box-value]
                        [do]
                            unquote-splice body

run-stage;

# RL FUNCTION DEFINITIONS
# ================================================================================
_fn rlprint (arg)
    print arg

_fn rlcar (lst)
    let lst =
        rlvalue-unbox-as lst RLValue.List
    let at = (sc_list_at lst)
    box-Value at

_fn rlcdr (lst)
    let lst =
        rlvalue-unbox-as lst RLValue.List
    box-value (sc_list_next lst)

_fn rlempty? (lst)
    let lst =
        rlvalue-unbox-as lst RLValue.List
    empty? lst

_fn rlcons (v lst)
    let lst =
        rlvalue-unbox-as lst RLValue.List
    'apply v
        inline (T self)
            cons self lst

inline gen-binary-op (f)
    _fn (a b)
        f (rlvalue-unbox-as a RLValue.Number) (rlvalue-unbox-as b RLValue.Number)

let rl-primitives =
    do
        let + - / * ** < > <= >= == != =
            va-map
                gen-binary-op
                _ + - / * ** < > <= >= == !=
        let and or not
        let sugar-quote
        let true false
        let car cdr cons empty? = rlcar rlcdr rlcons rlempty?
        let if = _if
        let fn = _fn
        let print = rlprint

        # must be last!
        let let = _let
       
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
using import chaining
load-module (parse-module-name filename) filename
    scope =
        --> rl-primitives
            'bind __ list-handler-symbol list-handler
