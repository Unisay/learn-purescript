"use strict";
var prompt = require("prompt-sync")();
exports._clearScreen = () => {
  console.clear();
  return {};
};
exports._printLine = (line) => {
  console.log(line);
  return {};
};
exports._askForInput = (question) => {
  return prompt(question);
};
