exports.sampleJson1 = {
  scalarBoolean: true,
  scalarNull: null,
  scalarNumber: 42.42,
  scalarString: "scalar string\n\t--is multiline",
  collection: [
    { name: "Albert", surname: "Einstein" },
    { name: "Alonso", surname: "Church" },
  ],
  mapping: {
    addressLines: ["Andreasstr.", "10243", "8 stock"],
    1: 2,
    nested: { els: [1] },
  },
};

exports.sampleArray10 = [[[[[1]]]]];
exports.sampleHomoObject = { key: "type", key2: "value2" };

exports.db = {
  columns: [
    { name: "name", type: "String", required: true },
    { name: "age", type: "Number", required: true },
    { name: "has_kids", type: "Bool", required: false, default: false },
  ],
  rows: [
    ["Yura", 39, true],
    ["Chiki", 40, true],
    ["Vadym", 16],
    ["Vlad", 16, false],
  ],
};

exports.db_view = [
  'name="Yura", age=39, has_kids=true',
  'name"Chiki", age=40, has_kids=true',
  'name="Vadym", age=16, has_kids=false',
  'name="Vlad", age=16, has_kids=false',
];
