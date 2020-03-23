# Wrappers

The rapid development of a PureScript ecosystem in Dart would require an efficient way to codegen wrapper libraries.  While not straightforward in a unityped language like JavaScript, it may be possible to construct a principled system to do this for a typed language like Dart. An inspiration for this project is the codegen bindings to `react-native` [here](https://github.com/dwhitney/purescript-react-basic-native/tree/master/codegen).

The code generator will need to read a Dart library as input, and then construct a pair of output files:  The PureScript module declaring foreign imports, and the Dart FFI file calling into the original Dart library through appropriate curried functions (with stripped types).

In principle it might be possible to make direct use of the Dart library as the original FFI files.  The generated PureScript would need to devise some modifier like a trailing underscore to represent the corresponding (curried) functions.

There is concern about the Dart analyzer throwing runtime errors if it can't validate typecasts. Since the compiler backend currently omits type information from the generated code, PureScript code generally will pass in `dynamic` values.  These may not be fully instantiated by the Dart virtual machine, such as function types.  Addressing this would require significant changes to the architecture of the PureScript frontend or the Dart backend (adding type information to `CoreFn` or forking the compiler).

If built into the compiler backend, these could be generated from arbitrary packages on the fly rather than in advance and imported as PureScript modules.
