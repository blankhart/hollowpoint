library data_unit_ffi; // ideally this would not be necessary

// The main thing is to ensure other Dart ffi modules can return unit without
// explicitly depending on this module.  Unit also could be represented as
// null, though that would be inconsistent with the JavaScript definition.
const unit = {};
