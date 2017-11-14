$(document).ready(function() { 

  $("span.spoiler").hide();

  $('<a class="reveal">???</a>').insertBefore('.spoiler');

  $("a.reveal").click(function(){
    $(this).parent().children("span.spoiler").fadeIn(100);
    $(this).parent().children("a.reveal").hide();
  });

  $('.terminal').addClass('hljs-hybrid');

}); 

var slideshow = remark.create({
  highlightLines: true,
  highlightLanguage: "ocaml",
  highlightStyle: "github",
  countIncrementalSlides: false,
  // sourceUrl: "prosojnice.md",
});
