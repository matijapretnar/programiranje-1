var jQuerySrc = document.createElement('script');
jQuerySrc.src = 'https://code.jquery.com/jquery-1.12.4.min.js';
jQuerySrc.type = 'text/javascript';
jQuerySrc.onload = function() {
    var $ = window.jQuery;
    $(document).ready(function() { 

      $('span.spoiler').hide();

      $('<a class="reveal">???</a>').insertBefore('.spoiler');

      $('a.reveal').click(function(){
        $(this).next().fadeIn(100);
        $(this).hide();
      });
    }); 
};
document.head.appendChild(jQuerySrc);

var remarkSrc = document.createElement('script');
remarkSrc.src = 'https://remarkjs.com/downloads/remark-latest.min.js';
remarkSrc.type = 'text/javascript';
remarkSrc.onload = function () {
  var slideshow = remark.create({
    highlightLines: true,
    highlightLanguage: 'ocaml',
    highlightStyle: 'github',
    countIncrementalSlides: false,
    ratio: '16:9'
  });          
};
document.head.appendChild(remarkSrc);

var mathJaxSrc = document.createElement('script');
mathJaxSrc.src = 'https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.1/MathJax.js?config=TeX-AMS-MML_HTMLorMML';
mathJaxSrc.type = 'text/javascript';
document.head.appendChild(mathJaxSrc);

