import 'package:flutter/widgets.dart' as f;

void runApp(f.Widget w) => () => f.runApp(w);

// StatelessWidget

// NOTE: Do not attempt to downcast function types explicitly with `as`,
// as the result will compile but result in runtime errors.
typedef DynamicWidgetBuildFn<T> =
  dynamic Function(T) Function(dynamic);

class StatelessWidget<T> extends f.StatelessWidget {
  final T _props;
  final DynamicWidgetBuildFn<T> _build;

  // Key -> super(key: key)
  const StatelessWidget(this._props, this._build);

  @override
  f.Widget build(f.BuildContext context) {
    return _build(context)(_props);
  }
}

stateless<T>(DynamicWidgetBuildFn<T> buildFn) => (T props) =>
  StatelessWidget(props, buildFn);

text(String s) => f.Text(s);

center(f.Widget w) => f.Center(child: w);
