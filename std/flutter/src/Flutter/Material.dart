import 'package:flutter/material.dart' as f;

f.Widget appBar(f.Widget w) => f.AppBar(title: w);

materialApp(String title) => (f.Widget home) =>
  f.MaterialApp(title: title, home: home);

scaffold(f.Widget appBar) => (f.Widget body) =>
  f.Scaffold(appBar: appBar, body: body);
