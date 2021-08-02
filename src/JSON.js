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
  },
};
