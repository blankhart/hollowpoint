unsafeHas(String label) {
  return (Map<String, dynamic> rec) {
    return rec.containsKey(label);
  };
}

unsafeGet(String label) {
  return (Map<String, dynamic> rec) {
    return rec[label];
  };
}

unsafeSet(String label) {
  return (dynamic value) {
    return (Map<String, dynamic> rec) {
      final copy = Map.from(rec);
      copy[label] = value;
      return copy;
    };
  };
}

unsafeDelete(String label) {
  return (Map<String, dynamic> rec) {
    final copy = Map.from(rec);
    copy.remove(label);
    return copy;
  };
}
