export function say(message) {
    document.write("<p>" + message + "</p>");
}

export const ask = (question) => () => {
    // say("Question: " + question);
    var answer = prompt(question);
    // say("Answer: " + answer);
    return answer;
};

export const tell = (message) => () => say(message);
