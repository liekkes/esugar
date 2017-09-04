# An Erlang Syntactic Sugar Library

## Motivation
我大概是在2014年的时候认识`Erlang`的，并且深深爱上了这个为我解决许多工作难题的"微型操作系统"。但在和别人的交流中，我也听到一些对于`Erlang`的抱怨，其中最多的竟然是关于它的语法！直到有一天我发现一个开源日志库[Lager](https://github.com/erlang-lager/lager)，里面有一个技巧就是利用编译参数`parse_transform`对源代码按照自己的意愿进行转换。于是我去查阅了`parse_transform`的相关资料，并且写下了这个项目：一方面它作为DEMO展示了原来利用`parse_transform`我们还可以做许多很酷的事情；另一方面你也可以把它当做一个`Erlang`的语法糖库拿去使用或者扩展。

## Pipe Operator
很多语言(比如`F#`和`Elixir`)都支持管道操作符，它将前一个表达式的运算结果作为参数传给后一个函数(调用)，形成一个调用链。比如我们要用`Erlang`来实现一个`md5`函数的话，传统的写法也许会是这样：
```erlang
-spec md5(Data :: iodata()) -> Digest :: string().
md5(Data) ->
    _Digest = string:to_lower(lists:flatten(lists:map(fun(E) ->
        [integer_to_list(X, 16) || X <- [E div 16, E rem 16]]
    end, binary_to_list(erlang:md5(Data))))).
```
如果你厌倦了上面这种嵌套调用，那么我们将允许如下的管道写法，它和前一种传统写法是等效的：
```erlang
-spec md5(Data :: iodata()) -> Digest :: string().
md5(Data) ->
    _Digest = pipe@(Data
        ! fun erlang:md5/1
        ! binary_to_list()
        ! lists:map(fun(E) -> [integer_to_list(X, 16)
                   || X <- [E div 16, E rem 16]] end)
        ! lists:flatten()
        ! fun string:to_lower/1).
```

## Do Block
用`Erlang`写业务逻辑的时候我让我最烦的就是"火箭式"嵌套，简单举个例子：比如我们要写一个函数，它返回两个整数相乘和相除的结果。
```erlang
muldiv(First, Second) ->
    case is_integer(First) of
        true ->
            case is_integer(Second) of
                true ->
                    Product = First * Second,
                    case Second =/= 0 of
                        true ->
                            Quotient = First div Second,
                            {ok, {Product, Quotient}};
                        false ->
                            {error, "Second must not be zero!"}
                    end;
                false ->
                    {error, "Second must be an integer!"}
            end;
        false ->
            {error, "First must be an integer!"}
    end.
```
它像不像一架向右冲刺的火箭？这并不是夸张，真正的业务逻辑里面判断分支也许比这还要多，而且一旦需求变更需要新增一个判断，这时候就更让人苦恼了。解决这个问题的一个方法大家都知道，那就是合理拆分业务逻辑，但这并不是灵丹妙药任何时候都能管用的。这里我们提供了类似`Haskell`里面的`do`语法，可以让你避免这种烦人的嵌套。
```erlang
muldiv(First, Second) ->
    do@([esugar_do_transform_error ||
        case is_integer(First) of
        true -> return(next);
        false -> fail("First must be an integer!")
        end,
        case is_integer(Second) of
        true -> return(next);
        false -> fail("Second must be an integer!")
        end,
        Product = First * Second,
        case Second =/= 0 of
        true -> return(next);
        false -> fail("Second must not be zero!")
        end,
        Quotient = First div Second,
        return({Product, Quotient})
    ]).
```
为了像上面这样使用`do`语法，你只需要实现自己的`monad`实例(这里我们实现的是`esugar_do_transform_error`)。


## Import As
我们知道不少语言都有"模块"这个概念，一个模块里面包含了大量的函数。当其它文件想要使用这些函数的时候，需要把它们导入到该文件中。`Erlang`不用导入而是使用`M:F(...)`的方式调用外部函数，当然它也支持了`-import(M, [F/A]).`这样的语法，这样就不用指定模块名而可以直接调用`F(...)`了。我们现在提供了一个更为强大的导入语法，它可以为导入的函数重新命名，这样你就可以在当前模块使用你喜欢的名字来调用外部函数了。
```erlang
-module(example).
-compile([{parse_transform, esugar_import_as_transform}]).
-import_as({lists, [{seq/2, range}, {reverse/1, rev}]}).
-export([run/0]).

run() ->
    [3, 2, 1] = rev(range(1, 3)).
```
不过我更喜欢最原始的调用方式，连`import`语法都很少使用，这样可以避免很多低级错误，所以我们实现的这个`import_as`语法，也建议慎用。


## Bind Repeatedly
在`Erlang`里面，"变量"其实是不可变的，当我们用`X = 1`把`X`绑定了到了值`1`上后，我们不可能言而无信再让`X = 2`。这个`immutable`性质的好处在任何一本介绍函数式编程的书上都必然会写，这里就不多说了。那么我们如何修改一个变量呢，`Erlang`说：“任何时候你都不可以修改，不过你可以创建一个新的变量嘛，为什么非要修改它呢？”于是`Erlang`鼓励我们写的代码是这样子的：
```erlang
norm(X, Y) ->
    T1 = 0,
    T2 = T1 + X * X,
    T3 = T2 + Y * Y,
    T4 = math:sqrt(T3),
    T4.
```
我这只是为了举例的夸张写法，你当然可以不用创建这么多新变量。不过使用了重复绑定语法以后，我们其实可以这样写：
```erlang
norm(X, Y) ->
    T = 0,
    T = T + X * X,
    T = T + Y * Y,
    T = math:sqrt(T),
    T.
```
不是说`Erlang`不让我们修改已经绑定好的变量吗，确实是这样。上面的代码在编译期实际上会转换成下面的过渡形式：
```erlang
norm(X@1, Y@1) ->
    T@1 = 0,
    T@2 = T@1 + X@1 * X@1,
    T@3 = T@2 + Y@1 * Y@1,
    T@4 = math:sqrt(T@3),
    T@4.
```
也就是说，在每一次新的匹配模式里面作为左值，即使相同的变量名，也会加上编号被转换成一个新的变量。而作为右值的变量名，则会使用当前已经存在的最大编号。那么还剩下一个问题，如果在模式里面我就想用刚刚已经绑定好的那个变量而不是重新创建一个呢，这时候你可以在变量后面加上`@`就可以了。
```erlang
norm(X, Y) ->
    T = 0,
    T = T + X * X,
    T = T + Y * Y,
    T@ = X * X + Y * Y,
    T = math:sqrt(T),
    T.
```
它和下面的写法等效：
```erlang
norm(X@1, Y@1) ->
    T@1 = 0,
    T@2 = T@1 + X@1 * X@1,
    T@3 = T@2 + Y@1 * Y@1,
    T@3 = X@1 * X@1 + Y@1 * Y@1,
    T@4 = math:sqrt(T@3),
    T@4.
```


## In The End
最后也许你会问，这些东西到底有什么用？我会告诉你，没什么卵用。没有什么是因为缺了它们而不能做的，正如一开始我所说的：你可以把它当成一个DEMO，展示了原来我们还可以在`Erlang`的语法层面做许多有趣的事情；你也可以把它当成一个库拿去使用或者扩展，不过稳定性我可不敢保证 \_(:з」∠)\_


