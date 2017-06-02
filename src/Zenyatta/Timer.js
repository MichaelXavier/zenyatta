"use strict";

exports.play = function(audio) {
  return function() {
    audio.play();
  }
};
