# The Purescript Compiler

Prospective contributors to the compiler or authors of alternative backends may be confused at first by the module structure, because it does not make a clear high-level separation between the backend's interface for arbitrary code generation targets and the default implementation.  The `purs` executable itself could be made retargetable by exposing an API for backends, rather than relying on a build tool like `spago`.

In particular, the reasons for the boundary between what occurs in the `purs` executable and what occurs in the library is not well documented; the core imperative IR is tightly coupled to JavaScript, although this is not clearly reflected in the module structure; and various functions specific to JavaScript are sprinkled throughout the code base typically identified with prefixes, infixes or suffixes of `js`, `Js`, or `JS` rather than hived off in their own module.

Additionally, the compiler makes extensive use of anonymous products and tuples, which require the reader to know the meaning of positional arguments.  This creates a barrier to entry, though becomes manageable with greater familiarity. It is helpful to start with a deep dive through the source on Hackage (which allows go-to-definition navigation) and to use an IDE that shows type information like `ghcide`.  The use of tuples allows for convenient traversals like `Traversals.sndM`.

## Pipeline

The Purescript compiler is organized as an executable that calls a library.  The executable is named `purs` and is located in the `app` subdirectory.  The library is named `purescript` and is located in the `src` directory.

## `purs`

The executable provides many different options accessible from the command line, such as compiling source files, bundling the app, generating documentation, graphing dependencies, publishing a package, and providing feedback to an IDE.

The compilation step is organized in the `Command.Compile` module.  That module's principal function is its `compile` pipeline.  The pipeline:

* Reads input files specified by command line options, typically globbing on `*.purs`.
* Loads modules via `CST.parseModulesFromFiles`.
* Infers the name of foreign modules with `Make.inferForeignModules`.  Foreign modules are required to resolve "extern" declarations (i.e., `foreign import...`).
* Creates a list of to-dos using `Make.Actions.buildMakeActions`.
* Runs the to-dos using `Make.make` via its export to the top-level namespace.
* Prints errors and warnings returned by `Make.Monad.runMake` (an `IO` action) via its export to the top-level namespace.
* Exits with a success code.

## `purescript`

Unless otherwise specified, this write-up assumes that qualified names have the prefix `Language.Purescript...`.

## Frontend

The entry point to the frontend is `CST.parseModulesFromFiles`, which returns a `PartialResult Module` or qualified, `CST.Parser.PartialResult AST.Declarations.Module`.  The operation runs in a `MonadError Errors.MultipleErrors`.

The frontend converts the source file into lexical source tokens, converts the source tokens into a concrete syntax tree (CST) representing the parsed form of the source file.  The concrete syntax tree is then converted into an abstract syntax tree (AST) for further transformations.

* Lexically analyzes the content of the file into a `CST.Monad.LexResult` via `CST.Lexer.lex`.  The result type is a synonym for `Either (CST.Monad.LexState, CST.Errors.ParserError) CST.Types.SourceToken`.
* Parses the source tokens into a `CST.Types.Module` via `CST.Parser.parseModule`.  The module data structure contains a list of imports, exports, top-level declarations, etc.  The content of the module has type `CST.Types.Declaration`.
* "Converts" the module data into an AST via `CST.Convert.convertModule`.  Declarations are converted into values of type `AST.Declaration` using `CST.Convert.convertDeclaration`.  The declarations represent expressions as `AST.Declarations.Expr`.

## Backend

### Driver

The entry point to the backend is `Make.make`. This function constructs a dependency graph or "build plan" to determine which modules need to be rebuilt and in what order, then builds them and caches the results.  The `make` function takes as input the `AST.Module`, and returns as output an `Externs.ExternsFile`.

The `make` function compiles modules by invoking `Make.rebuildModule'`.

First, the build desugars the `AST.Declarations.Module`, performs type and exhaustiveness checks, and desugars case declarations and pattern guards.  These are `AST.Declarations.Module -> AST.Declarations.Module` passes, but also construct an `Environment.Environment`.

Second, the build converts `AST.Declarations.Module` to a `CoreFn.Module.Module` by invoking `CoreFn.Desugar.moduleToCoreFn`.  The principal work is done in a `where`-bound expression `declToCoreFn`.  The build also runs a `CoreFn -> CoreFn` optimization pass by invoking `CoreFn.optimizeCoreFn`.

Third, the build converts the `CoreFn.Module.Module` into the generated code.  This is a step configurable by alternate backends.  The specific compilation steps are configured by passing `make` (and `rebuildModule'`) a `Make.Action.MakeActions Make.Monad.Make`.

More specifically, the `MakeActions` record has `codegen` and `ffiCodegen` fields that allow backend to plug the `MakeAction` with an operation that generates code from a `CoreFn.Module.Module`.  The `rebuildModule'` pipeline invokes `ffiCodegen` and then `codegen` to produce the `ExternsFile`.

Additionally, the `MakeActions` record is a higher-kinded type that takes a monadic parameter so as to abstract over the mode of I/O used in compilation.  The `Make.Monad.Make` monad provides the default parameter of `MakeActions`, and uses disk reads and writes as the mode of I/O.

## `CoreFn` Representation

The design of the backend implies a fundamental difference between `CoreFn` functional AST, which is the starting point for all code generation targets, and the `CoreImp` imperative AST, which is specific to JavaScript.  Some backends reflect this by starting from the `CoreFn` representation and converting it to a language-specific imperative AST (e.g., Kotlin).  However, others generate code directly from the JavaScript `CoreImp` representation because it is close enough to their target (e.g., Python).

The `CoreFn.Expr.Expr` AST preserves type information in the type parameter, which is populated with a `CoreFn.Ann` value.  `Ann` is a type synonym for a quadruple containing source span information, possible comments, a possible elaborated type represented by `Types.SourceType` (if the expression is a value), and possible metadata annotations of type `CoreFn.Meta.Meta` (e.g., `IsConstructor` with a slot indicating `ProductType` or `SumType`, `IsNewtype`, `IsTypeClassConstructor`).

The `CoreFn.Expr` data structure itself represents objects as lists of string key-value pairs. Alternate backends targeting typed languages usually represent records as unityped dictionaries with string keys through a naive translation.  This creates a performance penalty, though it may be mitigated if the target runtime has a tracing JIT.

The `Types.SourceType` data structure is a synonym for `Types.Type AST.SourcePos.SourceAnn`.

### JavaScript Implementation

The default backend creates its `MakeActions Make` via `Make.Actions.buildMakeActions` to generate JavaScript via the `CodeGen.JS` library modules.  The pipeline for JavaScript:

* Converts `CoreFn.Module.Module` into `CoreImp.AST` via `CodeGen.JS.moduleToJs`.
* Runs optimization passes from `CoreImp.AST -> CoreImp.AST` via `CoreImp.Optimizer.optimize` (called within `CodeGen.JS.moduleToJs`).
* Pretty-prints the result via `CodeGen.JS.Printer.prettyPrintJS`.

The mingling of JavaScript-focused implementation with generic backend functionality in the compiler's current module structure seems undesirable, because it creates the risk that a non-JavaScript backend will inadvertently call one of those functions and introduce a bad dependency.  The current mechanism for avoiding this is not localizing JavaScript-focused functions in their own modules, but including `-Js-` or `-JS` somewhere in their name, such as `prettyPrintStringJS`.

Identifiers are generated using `Names.runIdent`, which can cause generated code to have references like `$12`.  This may be acceptable depending on the validity of identifiers in the target language, but technically should be `runIdentJs`.

#### JavaScript Imperative IR (`CoreImp.AST`)

```haskell

-- | Built-in unary operators
data UnaryOperator
  = Negate
  | Not
  | BitwiseNot
  | Positive
  | New
  deriving (Show, Eq)

-- | Built-in binary operators
data BinaryOperator
  = Add
  | Subtract
  | Multiply
  | Divide
  | Modulus
  | EqualTo
  | NotEqualTo
  | LessThan
  | LessThanOrEqualTo
  | GreaterThan
  | GreaterThanOrEqualTo
  | And
  | Or
  | BitwiseAnd
  | BitwiseOr
  | BitwiseXor
  | ShiftLeft
  | ShiftRight
  | ZeroFillShiftRight
  deriving (Show, Eq)

-- | Data type for simplified JavaScript expressions
data AST
  = NumericLiteral (Maybe SourceSpan) (Either Integer Double)
  -- ^ A numeric literal
  | StringLiteral (Maybe SourceSpan) PSString
  -- ^ A string literal
  | BooleanLiteral (Maybe SourceSpan) Bool
  -- ^ A boolean literal
  | Unary (Maybe SourceSpan) UnaryOperator AST
  -- ^ A unary operator application
  | Binary (Maybe SourceSpan) BinaryOperator AST AST
  -- ^ A binary operator application
  | ArrayLiteral (Maybe SourceSpan) [AST]
  -- ^ An array literal
  | Indexer (Maybe SourceSpan) AST AST
  -- ^ An array indexer expression
  | ObjectLiteral (Maybe SourceSpan) [(PSString, AST)]
  -- ^ An object literal
  | Function (Maybe SourceSpan) (Maybe Text) [Text] AST
  -- ^ A function introduction (optional name, arguments, body)
  | App (Maybe SourceSpan) AST [AST]
  -- ^ Function application
  | Var (Maybe SourceSpan) Text
  -- ^ Variable
  | Block (Maybe SourceSpan) [AST]
  -- ^ A block of expressions in braces
  | VariableIntroduction (Maybe SourceSpan) Text (Maybe AST)
  -- ^ A variable introduction and optional initialization
  | Assignment (Maybe SourceSpan) AST AST
  -- ^ A variable assignment
  | While (Maybe SourceSpan) AST AST
  -- ^ While loop
  | For (Maybe SourceSpan) Text AST AST AST
  -- ^ For loop
  | ForIn (Maybe SourceSpan) Text AST AST
  -- ^ ForIn loop
  | IfElse (Maybe SourceSpan) AST AST (Maybe AST)
  -- ^ If-then-else statement
  | Return (Maybe SourceSpan) AST
  -- ^ Return statement
  | ReturnNoResult (Maybe SourceSpan)
  -- ^ Return statement with no return value
  | Throw (Maybe SourceSpan) AST
  -- ^ Throw statement
  | InstanceOf (Maybe SourceSpan) AST AST
  -- ^ instanceof check
  | Comment (Maybe SourceSpan) [Comment] AST
  -- ^ Commented JavaScript
  deriving (Show, Eq)

```

### Kotlin Implementation

The Kotlin backend intercepts at `CoreFn` and generates its own `CoreImp` variant, `KtCore`.  The [`CoreImp.moduleToKt` function](https://github.com/csicar/pskt/blob/kotlin/src/CodeGen/CoreImp.hs) derives data type and constructor declarations from the `CoreFn` via its `splitDeclarations` clause.  If the `Bind Ann` has the form `(NonRec _ _ (Constructor _ tyName _ _))` then it is classified as a data type declaration.  The related declarations matching `(NonRec _ _ (Constructor _ _ ctorName idents))` are classified as data constructors.

#### Kotlin Imperative IR (`KtCore`)

```haskell

data KtExpr
  = Package [ProperName Namespace]
  | Import [ProperName Namespace] KtIdent
  | Stmt [KtExpr]
  | ObjectDecl (Maybe KtIdent) [KtExpr] KtExpr
  -- ^ object: name; extends; body
  | ClassDecl [KtModifier] KtIdent [KtIdent] [KtExpr] KtExpr
  -- ^ class modifier; name; arguments; extends; body
  | If KtExpr KtExpr (Maybe KtExpr)
  | WhenExpr [WhenCase KtExpr]
  | VariableIntroduction KtIdent KtExpr
  | Binary BinOp KtExpr KtExpr
  | Property KtExpr KtExpr
  | ArrayAccess KtExpr KtExpr
  | ObjectAccess KtExpr KtExpr
  | VarRef (Qualified KtIdent)
  | Cast KtExpr KtExpr
  | Fun (Maybe KtIdent) [KtIdent] KtExpr
  | FunRef (Qualified KtIdent)
  | Lambda KtIdent KtExpr
  | Call KtExpr [KtExpr]
  | Const (Literal KtExpr)
  | Annotated KtExpr KtExpr
  deriving (Show)

data KtModifier
  = Sealed
  | Data
  deriving (Show)

data WhenCase a
  -- Conditions, return value
  = WhenCase [a] a
  | ElseCase a deriving (Show, Functor, Foldable, Traversable)

data BinOp
  = Equals
  | IsType
  | And
  | To -- for Pairs: `1 to "Hi"`
  | Add
  deriving (Show)

mapType :: KtIdent
mapType = MkKtIdent "Map<String, Any>"

```

### Dart Implementation

The Dart ecosystem reflects changing usage patterns as the language and its runtimes have evolved. Dart 1 was a different language in many ways emphasizing optional typing.  Dart 2 defaults to requiring type annotations.  Flutter is a separate workflow and repackages Dart in a very specific way with its own practices, resulting in new annotations and codegen.

Dart's build tools have also fragmented as the ecosystem has evolved.  The tools to build a program include `dart` itself, to run a program; `pub`, which runs a  `webdev`, `build_runner`, and `flutter`.

Dart's module structure and build tools have special-case behavior for the directories `lib`, `lib/src`, and `bin`.

There is no clear standard for Dart formatting style to be followed in the generated code. The standard `dartfmt` tool provides an automated formatting.  The [pedantic](https://github.com/dart-lang/pedantic#enabled-lints) includes a series of lints in addition to its own formatting rules, which represent best practices adhered to by Google, but conflict with `dartfmt`.  The Flutter SDK has its own practices.

#### Dart Module System

Dart code generators typically create a file with the suffix `.g.dart` and include it as a `part` of a main `library`.  The generated PureScript code would follow this practice with the suffix `.ps.dart`.

If a PureScript source file `Data.Thing` has no related FFI file:

* A library file `data_thing` would be generated for it.  The library file will have the statement `library data_thing;` and `part 'data_thing.ps.dart';`.
* The generated code would become the Dart source file `data_thing.ps.dart` and would contain the statement `part of data_thing;`.

If a PureScript source file `Data.Thing` has an FFI file, the FFI file itself would be the library file.

The problem with this approach is that, if the Dart module has imports, then those names may contaminate the names in the PureScript file, because they all form part of the same library.  There would be no way of knowing what imports were made and how they were qualified.  Additionally, the imports needed for the PureScript file would have to be added to the Dart FFI file.

#### Dart Imperative IR

##### Type Definitions and Data Constructors

Type definitions map to `abstract class` with no body and a prohibition on concrete implementations.

Data constructor definitions map to concrete `class` definitions extending the corresponding type constructor `abstract class`.

PureScript types and data constructors have different namespaces, while under this scheme, types and data constructors would have the same namespace.  The types therefore would need to have a different name than the data constructor.

Additionally, the type name would need to be unique within the module.  For example, if the practice was to suffix the PureScript type name with `-Type`, such as `Data` becoming `DataType`, there would be a clash if there were another data constructor that ended in `-Type` such as `DataType`.

The type generation would be useful if there is an end goal of adding Dart type annotations to values, or annotating the types with `@sealed`.

Including abstract classes is not strictly necessary unless the types will be used for some purpose.

Dart does not have a convenient mechanism to create new namespaces for this purpose.  Dart classes cannot be nested to create their own namespaces.  Namespacing is possible by creating a separate library, but this would foreclose use of `@sealed` in the `meta` package since it would prohibit declaring the data constructor in another library.

One possibility covering most cases would be to have a convention in which types are prefixed with `Abstract-`.  If a data constructor also begins with `Abstract-` and results in a collision, then it could be suffixed with `-Ctor`.  If there is still a collision, fall back to unique suffixes formed from symbols.  The specific names used in generated code would be relevant only when calling PureScript from Dart.

##### Type Class and Instance Definitions

Type class definitions map to `abstract class`es with appropriate instance methods.

Type class instance definitions map to concrete `class` definitions (with a camel cased version of the name) extending the corresponding type class's `abstract class`.

Type class method invocations call the method on the concrete `class`.

##### Records

Records could be represented as:

* `Map<String, dynamic>`
* `Map<Symbol, dynamic>`
* "Anonymous" classes implementing `abstract class`es for each property.
* Concrete `class`es mixing in concrete `class`es for each property.

##### Variables

Variables generally will be represented as `dynamic` unless full type annotations are undertaken.  It's unclear what benefits that would have for (i) performance, (ii) ease of writing Dart FFI, and (iii) interop from Dart.

Another question is whether the default code generation should mark the functions as `final`.  Probably yes, absent a circumstance in which that would produce incorrect behavior.  Mutation requires the Dart FFI.

## Notes

* Explore whether it is possible to provide compile-time errors using the existing typeclass infrastructure if a function is marked as intended to be tail-call optimized but the compiler is unable to perform the optimization.  This may have complicated interactions with currying, but the idea would be that applying a `TailRec =>` constraint requires each function to be either non-recursive or tail-recursive.