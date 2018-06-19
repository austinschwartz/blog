#!/usr/bin/env node
// From https://github.com/zacharydenton/zach.se
var fs = require('fs');
var path = require('path');
var katex = require('katex');
var cheerio = require('cheerio');
var highlight = require('pygments').colorizeSync;

var input = '';

function unwrap(el) {
	if (!el) {
		return false;
	}	else {
		var textnode = el.firstChild;
		var elParent = el.parentNode;
		elParent.replaceChild(textnode,el);
	}
}

function render(html, cb) {
  if (!html) {
    console.log("no html?");
    return;
  }
	var $ = cheerio.load(html);

	// Run KaTeX to generate math blocks
	$('.math').each(function() {
		var $el = $(this);
		if ($el.children().length == 0) {
			var content = $el.text();
			try {
				var math = katex.renderToString(content, {displayMode: $el.is('.display')});
			}
			catch (e) {
				console.log("broke trying to read: \"" + content + "\"");
				throw e;
			}
			$el.html(math);
		}
	});

	// Run pygments on code blocks
	var codes = $('pre > code');
	codes.each(function() {
		var $el = $(this);
		var code = $el.text();
		var pre = $el.parent();
		var lang = pre.attr('class');
		var data = highlight(code, lang, 'html');
    if (!data || data.length < 1) {
      console.log("data: " + data);
      console.log("code: " + code);
      console.log("lang: " + lang);
      console.log("highlighting broke somehow");
    } else {
      pre.replaceWith(data);
    }
  });
  cb($.html());
}

if (process.argv.length > 2) {
	// list of files as arguments
	process.argv.slice(2).forEach(function(path) {
    console.log(path);
		fs.readFile(path, function(err, html) {
      render(html, function(processed) {
        fs.writeFile(path, processed, () => {});
      });
    });
	});
} else {
  console.log("pass file paths as args");
}
