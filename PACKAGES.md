# Package Distribution

Ideally packages could be obtained in the normal way via `pub get`.  The `pub` package manager (or `flutter pub`) would pull the source and store it in `.pub-cache` and store the package name and location mapping in the `.packages` file.

E.g., `.packages` contains:

```ini
flutter:file:///home/blankhart/dev/flutter/packages/flutter/lib/
matcher:file:///home/blankhart/dev/flutter/.pub-cache/hosted/pub.dartlang.org/matcher-0.12.6/lib/
```

If this distribution method were possible, the backend would need to be fed the `.packages` file and use it to locate foreign files (and PureScript files, if those also would be pulled and stored by `pub`).

The backend would not need to copy the foreign files since it can confirm they are already accessible to the Dart build via `pub`.
