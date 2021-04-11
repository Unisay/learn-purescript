"use strict";

function say(message) {
  document.write("<p>" + message + "</p>");
}

exports.ask = (question) => {
  return () => {
    say("Question: " + question);
    var answer = prompt(question);
    say("Answer: " + answer);
    return answer;
  };
};
exports.tell = (message) => {
  return () => say(message);
};
